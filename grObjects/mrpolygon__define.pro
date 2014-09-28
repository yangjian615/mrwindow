; docformat = 'rst'
;
; NAME:
;   MrPolygon__Define
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   The purpose of this program is to create a polygon object object that can drawn on a
;   data plot.
;
; :Categories:
;    Graphics
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;     Change History::
;       2014/09/21  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The purpose of this method is to draw the ColorFill object to the display window.
;-
PRO MrPolygon::Draw, $
NOERASE=noerase
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        
        IF free_ptrs THEN BEGIN
            Ptr_Free, xcoords
            Ptr_Free, ycoords
            IF nz GT 0 THEN Ptr_Free, zcoords
        ENDIF
        
        IF Ptr_Valid(xpoly) THEN BEGIN
            Ptr_Free, xpoly
            Ptr_Free, ypoly
            Ptr_Free, zpoly
        ENDIF
        
        IF N_Elements(current_state) GT 0 THEN cgSetColorState, current_state
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Return if we are hiding.
    IF self.hide THEN RETURN
    
    ;Make sure 24-bit color is turned on.
    IF !d.name EQ 'PS' THEN Device, /COLOR, BITS_PER_PIXEL=8
    
    ;Use decomposed color, if possible
    cgSetColorState, 1, CURRENTSTATE=current_state
    
    ;If data coordinates were chosen, then restore the target's coordinate system.
    IF self.data EQ 1B THEN self.target -> RestoreCoords

;---------------------------------------------------------------------
; Relative Coordinates ///////////////////////////////////////////////
;---------------------------------------------------------------------
    free_ptrs = 0B
    nz        = N_Elements(*self.zcoords)

    ;Convert to relative coordinates?
    ;   - Avoid copying the data, if possible, by copying the pointer
    IF self.relative THEN BEGIN
        xcoords = Ptr_New(*xcoords * (!x.crange[1] - !x.crange[2]) + !x.crange[0])
        ycoords = Ptr_New(*ycoords * (!y.crange[1] - !y.crange[2]) + !y.crange[0])
        IF nz GT 0 $
            THEN zcoords = Ptr_New(*zcoords * (!z.crange[1] - !z.crange[2]) + !z.crange[0]) $
            ELSE zcoords = Ptr_New(/ALLOCATE_HEAP)
        free_ptrs = 1B
    ENDIF ELSE BEGIN
        xcoords = self.xcoords
        ycoords = self.ycoords
        zcoords = self.zcoords
    ENDELSE

;---------------------------------------------------------------------
; Single Polygon /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    IF N_Elements(*self.connectivity) EQ 0 THEN BEGIN    
        ;Draw the polygon
        ;   - Draw the lines and symbols on top of the filled region.
        self -> doPolyFill, xcoords, ycoords, zcoords
        self -> doPlotS,    xcoords, ycoords, zcoords
        
;---------------------------------------------------------------------
; Many Polygons //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ENDIF ELSE BEGIN
        ;Allocate heap to the vertices
        xpoly = Ptr_New(/ALLOCATE_HEAP)
        ypoly = Ptr_New(/ALLOCATE_HEAP)
        zpoly = Ptr_New(/ALLOCATE_HEAP)
    
        ;Setup
        nConnect = N_Elements(*self.connectivity)
        iConnect = 0LL
        thisPoly = 0L
        
        ;Draw each polygon
        WHILE iConnect LT nConnect DO BEGIN
            ;Get the indices of the current polygon.
            vStart  = iConnect + 1
            vStop   = iConnect + (*self.connectivity)[iConnect]
            indices = (*self.connectivity)[vStart:vStop]
            
            ;Isolate the vertices in a new pointer.
            *xpoly = (*xcoords)[indices]
            *ypoly = (*ycoords)[indices]
            IF nz GT 0 THEN *zpoly = (*zcoords)[indices]
            
            ;Draw the polygon.
            self -> doPolyFill, xpoly, ypoly, zpoly, POLYNUM=thisPoly
            self -> doPlotS,    xpoly, ypoly, zpoly, POLYNUM=thisPoly
            
            ;Move to the next polygon
            thisPoly += 1L
            iConnect = vStop + 1LL
        ENDWHILE
        
        ;Free the pointers.
        Ptr_Free, xpoly
        Ptr_Free, ypoly
        Ptr_Free, zpoly
    ENDELSE
        
;---------------------------------------------------------------------
; Clean Up ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Free pointers
    IF free_ptrs THEN BEGIN
        Ptr_Free, xcoords
        Ptr_Free, ycoords
        IF nz GT 0 THEN Ptr_Free, zcoords
    ENDIF
    
    ;Return to the original color state
    cgSetColorState, current_state
END


;+
; This method draws the axis object.
;-
PRO MrPolygon::doPolyFill, xcoords, ycoords, zcoords, $
POLYNUM=polynum
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        ;Restore color table.
        if n_elements(r) gt 0 then tvlct, r, g, b
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    nz = N_Elements(*zcoords)

    ;Return if we are not filling anything
    if self.fill_background eq 0 && self.line_fill eq 0 then return
    
;---------------------------------------------------------------------
; One or One of Many? ////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;One of many polygons
    ;   - Each polygon can have a unique fill or line fill.
    IF N_Elements(polynum) GT 0 THEN BEGIN
        ;Cyclic index number
        iColor      = polynum mod N_Elements(*self.fill_color)
        iLinestyle  = polynum mod N_Elements(*self.fill_linestyle)
    
        ;Get the properties of the current polygon
        fill_color      = cgColor((*self.fill_color)[iColor])
        fill_linestyle  = MrLinestyle((*self.fill_linestyle)[iLinestyle])
        
        ;Is orientation defined?
        IF N_Elements(*self.orientation) GT 0 THEN BEGIN
            iOrient     = polynum mod N_Elements(*self.orientation)
            orientation = (*self.orientation)[iOrient]
        ENDIF
        
    ;One polygon
    ;   - Uniform fill or line fill throughout.
    ENDIF ELSE BEGIN
        ;Properties of the polygon
        fill_color      = cgColor(*self.fill_color)
        fill_linestyle  = MrLinestyle(*self.fill_linestyle)
        orientation     = *self.orientation
    ENDELSE
    
;---------------------------------------------------------------------
; PolyFill ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Get the current color table vectors
    tvlct, r, g, b, /GET

    ;Polyfill
    IF nz EQ 0 THEN BEGIN
        PolyFill, *xcoords, *ycoords, $
                  CLIP         =  self.clip, $
                  COLOR        =       fill_color, $
                  DATA         =  self.data, $
                  DEVICE       =  self.device, $
                  IMAGE_COORD  = *self.image_coord, $
                  IMAGE_INTERP =  self.image_interp, $
                  LINE_FILL    =  self.line_fill, $
                  LINESTYLE    =       fill_linestyle, $
                  NOCLIP       =  self.noclip, $
                  NORMAL       =  self.normal, $
                  ORIENTATION  =       orientation, $
                  PATTERN      = *self.pattern, $
                  SPACING      =  self.spacing, $
                  T3D          =  self.t3d, $
                  THICK        =  self.thick, $
                  TRANSPARENT  =  self.transparent, $
                  Z            = *self.z
    ENDIF ELSE BEGIN
        PolyFill, *xcoords, *ycoords, *zcoords, $
                  CLIP         =  self.clip, $
                  COLOR        =       fill_color, $
                  DATA         =  self.data, $
                  DEVICE       =  self.device, $
                  IMAGE_COORD  = *self.image_coord, $
                  IMAGE_INTERP =  self.image_interp, $
                  LINE_FILL    =  self.line_fill, $
                  LINESTYLE    =       fill_linestyle, $
                  NOCLIP       =  self.noclip, $
                  NORMAL       =  self.normal, $
                  ORIENTATION  =       orientation, $
                  PATTERN      = *self.pattern, $
                  SPACING      =  self.spacing, $
                  T3D          =  self.t3d, $
                  THICK        =  self.thick, $
                  TRANSPARENT  =  self.transparent, $
                  Z            = *self.z
    ENDELSE
    
    ;Restore colortable
    tvlct, r, g, b
END


;+
; This method draws the axis object.
;-
PRO MrPolygon::doPlotS, xcoords, ycoords, zcoords, $
POLYNUM=polynum
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        ;Restore the color table
        IF N_Elements(r) GT 0 THEN tvlct, r, g, b
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    nz = N_Elements(*zcoords)

;---------------------------------------------------------------------
; One or One of Many? ////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;One of many polygons
    ;   - Each polygon can have a unique perimeter
    ;   - All vertices of a single polygon are the same.
    IF N_Elements(polynum) GT 0 THEN BEGIN
        ;Cyclic index number
        iColor     = polynum MOD N_Elements(*self.color)
        iLinestyle = polynum MOD N_Elements(*self.linestyle)
        iPsym      = polynum MOD N_Elements(*self.psym)
        iSymColor  = polynum MOD N_Elements(*self.symcolor)
        iSymThick  = polynum MOD N_Elements(*self.symthick)
        iSymsize   = polynum MOD N_Elements(*self.symsize)
    
        ;Get the properties of the current polygon
        color     = cgColor((*self.color)[iColor])
        linestyle = MrLinestyle((*self.linestyle)[iLinestyle])
        psym      = cgSymCat((*self.psym)[iPsym])
        symcolor  = cgColor((*self.symcolor)[iSymColor])
        symsize   = (*self.symsize)[iSymSize]
        symthick  = (*self.symthick)[iSymThick]
        
    ;One polygon
    ;   - Each line segment can have a unique color.
    ;   - Each vertex can have a unique size, color, and thickness
    ENDIF ELSE BEGIN
        ;Properties of the polygon
        color     = cgColor(*self.color)
        linestyle = MrLinestyle(*self.linestyle)
        psym      = cgSymCat(*self.psym)
        symcolor  = cgColor(*self.symcolor)
        symsize   = *self.symsize
        symthick  = *self.symthick
    ENDELSE

    ;Return if we are not drawing anything
    IF linestyle EQ 6 && psym EQ 0 THEN RETURN
;---------------------------------------------------------------------
; Perimeter Lines ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Get the current color
    tvlct, r, g, b, /GET

    ;Number of vertices
    nVertices = n_elements(*xcoords)
    
    ;Different color for each line segment?
    nColors = n_elements(color)

    ;Draw the polygon outline or symbols.
    IF linestyle NE 6 THEN BEGIN
    ;---------------------------------------------------------------------
    ; Mono-Color /////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        IF nColors LE 1 THEN BEGIN
            IF nz EQ 0 THEN BEGIN
                PlotS, *xcoords, *ycoords, $
                       CLIP      =  self.clip, $
                       COLOR     =       color, $
                       DATA      =  self.data, $
                       DEVICE    =  self.device, $
                       NORMAL    =  self.normal, $
                       LINESTYLE =       linestyle, $
                       NOCLIP    =  self.noclip, $
                       T3D       =  self.t3d, $
                       THICK     =  self.thick, $
                       Z         = *self.z
            ENDIF ELSE BEGIN
                PlotS, *xcoords, *ycoords, *zcoords, $
                       CLIP      =  self.clip, $
                       COLOR     =       color, $
                       DATA      =  self.data, $
                       DEVICE    =  self.device, $
                       NORMAL    =  self.normal, $
                       LINESTYLE =       linestyle, $
                       NOCLIP    =  self.noclip, $
                       T3D       =  self.t3d, $
                       THICK     =  self.thick, $
                       Z         = *self.z
            ENDELSE
        
    ;---------------------------------------------------------------------
    ; Multiple Colors ////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        ENDIF ELSE BEGIN
            ;Step through each vertex
            ;   - Make vertex colors cyclic.
            FOR i = 0, nVertices - 2 DO BEGIN
                IF nz EQ 0 THEN BEGIN
                    PlotS, (*xcoords)[[i,i+1]], (*ycoords)[[i,i+1]], $
                           CLIP      =  self.clip, $
                           COLOR     =       color[i mod nColors], $
                           DATA      =  self.data, $
                           DEVICE    =  self.device, $
                           NORMAL    =  self.normal, $
                           LINESTYLE =       linestyle, $
                           NOCLIP    =  self.noclip, $
                           T3D       =  self.t3d, $
                           THICK     =  self.thick, $
                           Z         = *self.z
                ENDIF ELSE BEGIN
                    PlotS, (*xcoords)[[i,i+1]], (*ycoords)[[i,i+1]], (*zcoords)[[i,i+1]], $
                           CLIP      =  self.clip, $
                           COLOR     =       color[i mod nColors], $
                           DATA      =  self.data, $
                           DEVICE    =  self.device, $
                           NORMAL    =  self.normal, $
                           LINESTYLE =       linestyle, $
                           NOCLIP    =  self.noclip, $
                           T3D       =  self.t3d, $
                           THICK     =  self.thick, $
                           Z         = *self.z
                ENDELSE
            ENDFOR
        ENDELSE
    ENDIF
    
;---------------------------------------------------------------------
; Perimeter Symbols //////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Different symbol color for each point?
    nSymSize  = N_Elements(symsize)
    nSymColor = N_Elements(symcolor)
    nSymThick = N_Elements(symthick)
    singleSym = (psym NE 0) && (nSymSize LE 1) && (nSymColor LE 1) && (nSymColor LE 1)
    
    ;Do not connect the points
    psym = Abs(psym)
    
    IF psym NE 0 THEN BEGIN
    ;---------------------------------------------------------------------
    ; Same Symbol ////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        IF singleSym THEN BEGIN
            IF nz EQ 0 THEN BEGIN
                PlotS, *xcoords, *ycoords, $
                       CLIP      =  self.clip, $
                       COLOR     =       symcolor, $
                       DATA      =  self.data, $
                       DEVICE    =  self.device, $
                       NORMAL    =  self.normal, $
                       NOCLIP    =  self.noclip, $
                       PSYM      =       psym, $
                       SYMSIZE   =       symsize, $
                       T3D       =  self.t3d, $
                       THICK     =       symthick, $
                       Z         = *self.z
            ENDIF ELSE BEGIN
                PlotS, *xcoords, *ycoords, *zcoords, $
                       CLIP      =  self.clip, $
                       COLOR     =       symcolor, $
                       DATA      =  self.data, $
                       DEVICE    =  self.device, $
                       NORMAL    =  self.normal, $
                       NOCLIP    =  self.noclip, $
                       PSYM      =       psym, $
                       SYMSIZE   =       symsize, $
                       T3D       =  self.t3d, $
                       THICK     =       symthick, $
                       Z         = *self.z
            ENDELSE
        
    ;---------------------------------------------------------------------
    ; Different Symbols //////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        ENDIF ELSE BEGIN
            FOR i = 0LL, nVertices - 1 DO BEGIN
                IF nz EQ 0 THEN BEGIN
                    PlotS, (*xcoords)[i], (*ycoords)[i], $
                           CLIP      =   self.clip, $
                           COLOR     =        symcolor[i mod nSymColor], $
                           DATA      =   self.data, $
                           DEVICE    =   self.device, $
                           NORMAL    =   self.normal, $
                           NOCLIP    =   self.noclip, $
                           PSYM      =        psym, $
                           SYMSIZE   =        symsize[i mod nSymSize], $
                           T3D       =   self.t3d, $
                           THICK     =        symthick[i mod nSymThick], $
                           Z         =  *self.z
                ENDIF ELSE BEGIN
                    PlotS, (*xcoords)[i], (*ycoords)[i], (*zcoords)[i], $
                           CLIP      =   self.clip, $
                           COLOR     =        symcolor[i mod nSymColor], $
                           DATA      =   self.data, $
                           DEVICE    =   self.device, $
                           NORMAL    =   self.normal, $
                           NOCLIP    =   self.noclip, $
                           PSYM      =        psym, $
                           SYMSIZE   =        symsize[i mod nSymSize], $
                           T3D       =   self.t3d, $
                           THICK     =        symthick[i mod nSymThick], $
                           Z         =  *self.z
                ENDELSE
            ENDFOR
        ENDELSE
    ENDIF
    
    ;Restore the color table
    tvlct, r, g, b
END


;+
;   Return the subscripts of the array elements contained inside the polygon.
;
; :Params:
;                       Y subscripts of the vertices that define the polygon.
;       SX:             in, required, type=integer
;                       The number of columns in the array surrounding the polygon.
;       SY:             in, required, type=integer
;                       The number of rows in the array surrounding the polygon.
;       RUN_LENGTH:     in, optional, type=boolean, default=0
;                       If set, return a vector of run lengths, rather than subscripts.
;                           When run-length encoded, each element with an even subscript
;                           result contains the length of the run, and the following
;                           element contains the starting index of the run.
;
; :Keywords:
;       FILLED:         out, optional, type=bytarr(`SX`\,`SY`)
;                       A named variable to receive a square byte-mask indicating the
;                           filled region of the polygon.
;
; :Returns:
;       RESULT:         1D subscripts of the array elemets contained within the polygon.
;                           If no points are contained inside the polygon, -1 is returned
;                           and an informational message is printed.
;-
function MrPolygon::PolyFillV, Sx, Sy, run_length, $
FILLED=filled
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN, -1
    ENDIF
    
    ;Default
    run_length = keyword_set(run_length)
    doFill     = arg_present(filled)
    if run_length && doFill then $
        message, 'RUN_LENGTH and FILLED are mutually exclusive'
    
    ;Call PolyFillV
    result = polyfillv(*self.xcoords, *self.ycoords, Sx, Sy, run_length)
    
    ;Return a polygon mask?
    if arg_present(filled) then begin
        filled = bytarr(Sx, Sy)
        filled[result] = 1B
    endif

    return, result
END


;+
;   Set the polygon vertex locations.
;
; :Params:
;       X:              out, required, type=numeric
;                       A vector argument providing the X coordinates of the points to be
;                           connected. The vector must contain at least three elements.
;       Y:              out, optional, type=numeric
;                       A vector argument providing the Y coordinates of the points to be
;                           connected. Y must contain at least three elements.
;       Z:              out, optional, type=number
;                       An optional vector argument providing the Z coordinates of the
;                           points to be connected. Z must contain at least three elements.
;-
PRO MrPolygon::GetData, x, y, z
    Compile_Opt StrictArr
    On_Error, 2
    
    SWITCH N_Params() OF
        3: z = *self.zcoords
        2: y = *self.ycoords
        1: x = *self.xcoords
    ENDSWITCH
END


;+
;   This method obtains the current properties of the object. 
; 
; :Keywords:
;       CLIP:           out, optional, type=fltarr(4)
;                       The coordinates of a rectangle used to clip the graphics output.
;                           Coordinates are specified in data units unless `NORMAL` or
;                           `DEVICE` is specified.
;       COLOR:          out, optional, type=string/byte/integer/long
;                       The name of the fill color. Color names are those used with cgColor. 
;                           This value can also be a long integer or an index into the
;                           current color table.
;       DATA:           out, optional, type=boolean
;                       Indicate that polygon vertices are in data coordinates. This is
;                           the default.
;       DEVICE:         out, optional, type=boolean
;                       Set to indicate the polygon vertices are in device coordinates.
;       IMAGE_COORD:    out, optional, type=2xN numeric
;                       The fill pattern array subscripts of each of the n polygon
;                           vertices. Use this keyword in conjunction with the `PATTERN`
;                           keyword to warp images over 2-D and 3-D polygons. Only
;                           available when creating output with the Z-Buffer.
;       IMAGE_INTERP:   out, optional, type=boolean
;                       Specifies the method of sampling the PATTERN array when the
;                           `IMAGE_COORD` keyword is present. The default method is to use
;                           nearest-neighbor sampling. Bilinear interpolation sampling is
;                           performed if `IMAGE_INTERP` is set. Used only when creating
;                           output with the Z-Buffer.
;       LINE_FILL:      out, optional, type=boolean
;                       Set this keyword to indicate that polygons are to be filled with
;                           parallel lines, rather than using solid or patterned filling
;                           methods. When using the line-drawing method of filling, the
;                           thickness, linestyle, orientation, and spacing of the lines
;                           may be specified with keywords.
;       LINESYTLE:      out, optiona, type=integer
;                       Line style used to draw lines when `LINE_FILL` is set.
;       NOCLIP:         out, optional, type=boolean
;                       If set, suppresses clipping of the polygons.
;       NORMAL:         out, optional, type=boolean
;                       Set to indicate the polygon vertices are in normalized coordinates.
;       ORIENTATION:    out, optional, type=float
;                       Specifies the counterclockwise angle in degrees from horizontal of
;                           the lines used to fill polygons. Forces `LINE_FILL`=1.
;       PATTERN:        out, optional, type=numeric
;                       Set this keyword to a rectangular array of pixels giving the fill
;                           pattern. If this keyword parameter is omitted, POLYFILL fills
;                           the area with a solid color. The pattern array may be of any
;                           size; if it is smaller than the filled area the pattern array
;                           is cyclically repeated. Postscript output requires
;                           Device, Language_Level=2.
;       SPACING:        out, optional, type=numeric
;                       The spacing, in centimeters, between the parallel lines used to
;                           fill polygons.
;       T3D:            out, optional, type=boolean
;                       If set, the generalized transformation matrix in !P.T will be used.
;       THICK:          out, optional, type=float
;                       Thickness of the lines when `POLY_FILL` is set.
;       TRANSPARENT:    out, optional, type=integer
;                       Specifies the minimum pixel value to draw in conjunction with the
;                           `PATTERN` and `IMAGE_COORD` keywords. Pixels less than this
;                           value are not drawn and the Z-buffer is not updated.
;       ZVALUE:         out, optional, type=float
;                       Provides the Z coordinate if a Z parameter is not present in the
;                           call. This is of use only if `T3D` is set.
;       _REF_EXTRA:     out, optional, type=any
;                       Any keyword accepted by MrGrAtom::GetProperty is also accepted
;                           via keyword inheritance.
;-
PRO MrPolygon::GetProperty, $ 
CLIP=clip, $
COLOR=color, $
CONNECTIVITY=connectivity, $
DATA=data, $
DEVICE=device, $
FILL_BACKGROUND=fill_background, $
FILL_COLOR=fill_color, $
FILL_LINESTYLE=fill_linestyle, $
IMAGE_COORD=image_coord, $
IMAGE_INTERP=image_interp, $
LINE_FILL=line_fill, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
PATTERN=pattern, $
PSYM=psym, $
RELATIVE=relative, $
SPACING=spacing, $
SYMCOLOR=symcolor, $
SYMSIZE=symsize, $
SYMTHICK=symthick, $
T3D=t3d, $
TARGET=target, $
THICK=thick, $
TRANSPARENT=transparent, $
ZVALUE=zValue, $
_REF_EXTRA=extra
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Object Properties
    IF Arg_Present(clip)            GT 0 THEN clip            =  self.clip
    IF Arg_Present(color)           GT 0 THEN color           = *self.color
    IF Arg_Present(connectivity)    GT 0 THEN connectivity    = *self.connectivity
    IF Arg_Present(data)            GT 0 THEN data            =  self.data
    IF Arg_Present(device)          GT 0 THEN device          =  self.device
    IF Arg_Present(fill_background) GT 0 THEN fill_background =  self.fill_background
    IF Arg_Present(fill_color)      GT 0 THEN fill_color      = *self.fill_color
    IF Arg_Present(fill_linestyle)  GT 0 THEN fill_linestyle  = *self.fill_linestyle
    IF Arg_Present(image_coord)     GT 0 THEN image_coord     = *self.image_coord
    IF Arg_Present(image_interp)    GT 0 THEN image_interp    =  self.image_interp
    IF Arg_Present(line_fill)       GT 0 THEN line_fill       =  self.line_fill
    IF Arg_Present(linestyle)       GT 0 THEN linestyle       = *self.linestyle
    IF Arg_Present(noclip)          GT 0 THEN noclip          =  self.noclip
    IF Arg_Present(normal)          GT 0 THEN normal          =  self.normal
    IF Arg_Present(orientation)     GT 0 THEN orientation     = *self.orientation
    IF Arg_Present(pattern)         GT 0 THEN pattern         = *self.pattern
    IF Arg_Present(psym)            GT 0 THEN psym            = *self.psym
    IF Arg_Present(relative)        GT 0 THEN relative        =  self.relative
    IF Arg_Present(spacing)         GT 0 THEN spacing         = *self.spacing
    IF Arg_Present(symcolor)        GT 0 THEN symcolor        = *self.symcolor
    IF Arg_Present(symsize)         GT 0 THEN symsize         = *self.symsize
    IF Arg_Present(symthick)        GT 0 THEN symthick        = *self.symthick
    IF Arg_Present(t3d)             GT 0 THEN t3d             =  self.t3d
    IF Arg_Present(thick)           GT 0 THEN thick           =  self.thick
    IF Arg_Present(transparent)     GT 0 THEN transparent     =  self.transparent
    IF Arg_Present(zValue)          GT 0 THEN zValue          = *self.z

    ;Target
    IF Arg_Present(target) GT 0 THEN IF Obj_Valid(self.target) GT 0 $
        THEN target = self.target $
        ELSE target = Obj_New()
    
    ;Superclass
    IF Arg_Present(extra) GT 0 THEN self -> MrGrAtom::GetProperty, _STRICT_EXTRA=extra
END


;+
;   Set the polygon vertex locations.
;
; :Params:
;       X:              in, required, type=numeric
;                       A vector argument providing the X coordinates of the points to be
;                           connected. The vector must contain at least three elements. If
;                           only one argument is specified, X must be an array of either two
;                           or three vectors (i.e., (2,*) or (3,*)). In this special case,
;                           the vector X[0,*] specifies the X values, X[1,*] specifies Y,
;                           and X[2,*] contain the Z values.
;       Y:              in, optional, type=numeric
;                       A vector argument providing the Y coordinates of the points to be
;                           connected. Y must contain at least three elements.
;       Z:              in, optional, type=number
;                       An optional vector argument providing the Z coordinates of the
;                           points to be connected. Z must contain at least three elements.
;
; :Keywords:
;       POSITION:       in, optional, type=float
;                       Set to the normal four-element normalized position array for locating 
;                           a rectangular region in a graphics window. If this keyword is used,
;                           the x and y parameters are constructed from this position.
;-
PRO MrPolygon::SetData, x, y, z, $
CONNECTIVITY=connectivity, $
POSITION=position
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Position vector
    IF N_Elements(position) GT 0 THEN BEGIN
        *self.xcoords = position[[0,0,2,2,0]]
        *self.xcoords = position[[1,1,3,3,1]]
        void          = Temporary(*self.zcoords)
        self.nparams  = 2B
        RETURN
    ENDIF
    
    ;Number of defined parameters.
    nparams = N_Elements(x)     eq 0 ? 0 $
                : N_Elements(y) eq 0 ? 1 $
                : N_Elements(z) eq 0 ? 2 $
                : 3
                
    ;Nothing given?
    IF nparams EQ 0 THEN BEGIN
        Print, 'Use Syntax: myPolygon -> SetData, x [, y [, z]].'
    
    ;X-only?
    ENDIF ELSE IF nparams EQ 1 THEN BEGIN
        ndims = Size(x, /N_DIMENSIONS)
        CASE ndims OF
            1: Message, 'If a single argument is supplied, it must be a 2D or 3D array.'
            2: BEGIN
                *self.xcoords = Reform(x[0,*])
                *self.ycoords = Reform(x[1,*])
                void          = Temporary(*self.zcoords)
                self.nparams  = 2
            ENDCASE
            3: BEGIN
                *self.xcoords = Reform(x[0,*])
                *self.ycoords = Reform(x[1,*])
                *self.zcoords = Reform(x[2,*])
                 self.nparams = 3
            ENDCASE
            ELSE: Message, 'X does not have the correct number of dimensions.'
        ENDCASE

    ;X, Y [, and Z]?
    ENDIF ELSE BEGIN
        *self.xcoords = x
        *self.ycoords = y
        IF nparams EQ 3 THEN *self.zcoords = z
        self.nparams = nparams
    ENDELSE
    
    ;Connectivity?
    IF N_Elements(connectivity) GT 0 $
        THEN *self.connectivity = connectivity $
        ELSE void = temporary(*self.connectivity)
END


;+
;   This method sets the properties of the object.
;
; :Keywords:
;       CLIP:           in, optional, type=fltarr(4)
;                       The coordinates of a rectangle used to clip the graphics output.
;                           Coordinates are specified in data units unless `NORMAL` or
;                           `DEVICE` is specified.
;       COLOR:          in, optional, type=string/byte/integer/long
;                       The name of the fill color. Color names are those used with cgColor. 
;                           This value can also be a long integer or an index into the
;                           current color table.
;       DATA:           in, optional, type=boolean
;                       Indicate that polygon vertices are in data coordinates. This is
;                           the default.
;       DEVICE:         in, optional, type=boolean
;                       Set to indicate the polygon vertices are in device coordinates.
;       IMAGE_COORD:    in, optional, type=2xN numeric
;                       The fill pattern array subscripts of each of the n polygon
;                           vertices. Use this keyword in conjunction with the `PATTERN`
;                           keyword to warp images over 2-D and 3-D polygons. Only
;                           available when creating output with the Z-Buffer.
;       IMAGE_INTERP:   in, optional, type=boolean
;                       Specifies the method of sampling the PATTERN array when the
;                           `IMAGE_COORD` keyword is present. The default method is to use
;                           nearest-neighbor sampling. Bilinear interpolation sampling is
;                           performed if `IMAGE_INTERP` is set. Used only when creating
;                           output with the Z-Buffer.
;       LINE_FILL:      in, optional, type=boolean
;                       Set this keyword to indicate that polygons are to be filled with
;                           parallel lines, rather than using solid or patterned filling
;                           methods. When using the line-drawing method of filling, the
;                           thickness, linestyle, orientation, and spacing of the lines
;                           may be specified with keywords.
;       LINESYTLE:      in, optiona, type=integer
;                       Line style used to draw lines when `LINE_FILL` is set.
;       NOCLIP:         in, optional, type=boolean
;                       If set, suppresses clipping of the polygons.
;       NORMAL:         in, optional, type=boolean
;                       Set to indicate the polygon vertices are in normalized coordinates.
;       ORIENTATION:    in, optional, type=float
;                       Specifies the counterclockwise angle in degrees from horizontal of
;                           the lines used to fill polygons. Forces `LINE_FILL`=1.
;       PATTERN:        in, optional, type=numeric
;                       Set this keyword to a rectangular array of pixels giving the fill
;                           pattern. If this keyword parameter is omitted, POLYFILL fills
;                           the area with a solid color. The pattern array may be of any
;                           size; if it is smaller than the filled area the pattern array
;                           is cyclically repeated. Postscript output requires
;                           Device, Language_Level=2.
;       SPACING:        in, optional, type=numeric
;                       The spacing, in centimeters, between the parallel lines used to
;                           fill polygons.
;       T3D:            in, optional, type=boolean
;                       If set, the generalized transformation matrix in !P.T will be used.
;       THICK:          in, optional, type=float
;                       Thickness of the lines when `POLY_FILL` is set.
;       TRANSPARENT:    in, optional, type=integer
;                       Specifies the minimum pixel value to draw in conjunction with the
;                           `PATTERN` and `IMAGE_COORD` keywords. Pixels less than this
;                           value are not drawn and the Z-buffer is not updated.
;       ZVALUE:         in, optional, type=float
;                       Provides the Z coordinate if a Z parameter is not present in the
;                           call. This is of use only if `T3D` is set.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrGrAtom::SetProperty is also accepted
;                           via keyword inheritance.
;-
PRO MrPolygon::SetProperty, $ 
CLIP=clip, $
COLOR=color, $
DATA=data, $
DEVICE=device, $
FILL_BACKGROUND=fill_background, $
FILL_COLOR=fill_color, $
FILL_LINESTYLE=fill_linestyle, $
IMAGE_COORD=image_coord, $
IMAGE_INTERP=image_interp, $
LINE_FILL=line_fill, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
PATTERN=pattern, $
POSITION=position, $
PSYM=psym, $
RELATIVE=relative, $
SPACING=spacing, $
SYMCOLOR=symcolor, $
SYMSIZE=symsize, $
SYMTHICK=symthick, $
T3D=t3d, $
TARGET=target, $
THICK=thick, $
TRANSPARENT=transparent, $
ZVALUE=zValue, $
_REF_EXTRA=extra
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Object Properties
    IF N_Elements(clip)            GT 0 THEN  self.clip            = clip
    IF N_Elements(color)           GT 0 THEN *self.color           = color
    IF N_Elements(fill_background) GT 0 THEN  self.fill_background = keyword_set(fill_background)
    IF N_Elements(fill_color)      GT 0 THEN *self.fill_color      = fill_color
    IF N_Elements(fill_linestyle)  GT 0 THEN *self.fill_linestyle  = fill_linestyle
    IF N_Elements(image_coord)     GT 0 THEN *self.image_coord     = image_coord
    IF N_Elements(image_interp)    GT 0 THEN  self.image_interp    = keyword_set(image_interp)
    IF N_Elements(linestyle)       GT 0 THEN *self.linestyle       = linestyle
    IF N_Elements(noclip)          GT 0 THEN  self.noclip          = noclip
    IF N_Elements(pattern)         GT 0 THEN *self.pattern         = pattern
    IF N_Elements(psym)            GT 0 then *self.psym            = psym
    IF N_Elements(spacing)         GT 0 THEN *self.spacing         = spacing
    IF N_Elements(symcolor)        GT 0 THEN *self.symcolor        = symcolor
    IF N_Elements(symsize)         GT 0 THEN *self.symsize         = symsize
    IF N_Elements(symthick)        GT 0 THEN *self.symthick        = symthick
    IF N_Elements(t3d)             GT 0 THEN  self.t3d             = t3d
    IF N_Elements(thick)           GT 0 THEN  self.thick           = thick
    IF N_Elements(transparent)     GT 0 THEN  self.transparent     = transparent
    IF N_Elements(zValue)          GT 0 THEN *self.z               = zValue

    ;ORIENTATION
    ;   - Automatically sets LINE_FILL=1.
    if n_elements(orientation) gt 0 then begin
        *self.orientation = orientation
        line_fill = 1
    endif
    
    ;LINE_FILL
    ;   = 1 sets FILL_BACKGROUND
    ;   = 0 must also cause ORIENTATION to be undefined.
    if n_elements(line_fill) gt 0 then begin
        self.line_fill = keyword_set(line_fill)
        if self.line_fill eq 1 then self.fill_background = 1
        if self.line_fill eq 0 then void = temporary(*self.orientation)
    endif
    
    ;POSITION
    if n_elements(position) gt 0 then begin
        *self.xcoords = position[[0,2,2,0,0]]
        *self.ycoords = position[[1,1,3,3,1]]
        void = temporary(*self.zcoords)
    endif

    IF N_Elements(target) GT 0 THEN IF Obj_Valid(target) $
        THEN self.target = target $
        ELSE self.target = Obj_New()
    
    IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra

;-----------------------------------------------------
;Data, Device, Normal, Relative \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;They depend on one another.
    ;   - In order of precedence.
    if n_elements(relative) gt 0 then begin
        self.relative = keyword_set(relative)
        
        ;Can only use relative coordinates if a target was given.
        if self.relative && obj_valid(self.target) eq 0 then begin
            message, 'TARGET not valid. Cannot set RELATIVE.', /INFORMATIONAL
            self.relative = 0
        endif
        
        ;Must set DATA.
        if self.relative then data = 1
    endif
    
    if n_elements(data) gt 0 then begin
        self.data = keyword_set(data)
        if self.data then begin
            normal = 0B
            device = 0B
        endif
    endif
    
    if n_elements(normal) gt 0 then begin
        self.normal = keyword_set(normal)
        if self.normal then begin
            device        = 0B
            self.data     = 0B
            self.relative = 0B
        endif
    endif
    
    if n_elements(device) gt 0 then begin
        self.device = keyword_set(device)
        if self.device then begin
            self.data     = 0B
            self.normal   = 0B
            self.relative = 0B
        endif
    endif
    
    if self.data + self.device + self.normal eq 0 then self.data = 1B

;-----------------------------------------------------
;Finish Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    self.window -> Draw
END


;+
; The clean-up routine for the object. Destroy pointers, etc.
;-
pro MrPolygon::cleanup
    compile_opt idl2
    
    ; catch the error.
    catch, theerror
    if theerror ne 0 then begin
        catch, /cancel
        void = cgerrormsg()
        return
    endif
    
    ;free pointers
    ptr_free, self.xcoords
    ptr_free, self.ycoords
    ptr_free, self.zcoords
    ptr_free, self.color
    ptr_free, self.connectivity
    ptr_free, self.fill_color
    ptr_free, self.fill_linestyle
    ptr_free, self.image_coord
    ptr_free, self.linestyle
    ptr_free, self.orientation
    ptr_free, self.pattern
    ptr_free, self.psym
    ptr_free, self.symcolor
    ptr_free, self.symsize
    ptr_free, self.symthick
    ptr_free, self.vert_colors
    ptr_free, self.z
end


;+
; :Params:
;       XCOORDS:        in, required, type=numeric
;                       A vector argument providing the X coordinates of the points to be
;                           connected. The vector must contain at least three elements. If
;                           only one argument is specified, X must be an array of either two
;                           or three vectors (i.e., (2,*) or (3,*)). In this special case,
;                           the vector X[0,*] specifies the X values, X[1,*] specifies Y,
;                           and X[2,*] contain the Z values.
;       YCOORDS:        in, optional, type=numeric
;                       A vector argument providing the Y coordinates of the points to be
;                           connected. Y must contain at least three elements.
;       ZCOORDS:        in, optional, type=number
;                       An optional vector argument providing the Z coordinates of the
;                           points to be connected. Z must contain at least three elements.
;
; :Keywords:
;       CLIP:           in, optional, type=fltarr(4)
;                       The coordinates of a rectangle used to clip the graphics output.
;                           Coordinates are specified in data units unless `NORMAL` or
;                           `DEVICE` is specified.
;       COLOR:          in, optional, type=string/byte/integer/long, default='rose'
;                       A color name, color triple, 24-bit color, or indexed color of the
;                           line around the polygon perimeter. Color names, 24-bit color,
;                           and color-indices can be arrays, coloring the segments between
;                           vertices different colors. If fewer colors exist than vertices,
;                           the colors are repeated cyclicly.
;       DATA:           in, optional, type=boolean, default=1
;                       Indicate that polygon vertices are in data coordinates. This is
;                           the default.
;       DEVICE:         in, optional, type=boolean, default=0
;                       Set to indicate the polygon vertices are in device coordinates.
;       FILL_BACKGROUND: in, optional, type=boolean, default=0
;                       If set, the polygon will be filled. FILL_BACKGROUND takes
;                           precedence over `LINE_FILL`.
;       FILL_COLOR:     in, optional, type=string/byte/longarr(3), default='opposite'
;                       A color name, color triple, or indexed color of the interior of
;                           the polygon. Used with `FILL_BACKGROUND` and `LINE_FILL`.
;       FILL_LINESTYLE: in, optional, type=string/integer, default='-'
;                       Linestyle used with line-filling the interior of the polygon. Used
;                           with `LINE_FILL`. See `LINESTYLE` for options.
;       IMAGE_COORD:    in, optional, type=2xN numeric
;                       The fill pattern array subscripts of each of the n polygon
;                           vertices. Use this keyword in conjunction with the `PATTERN`
;                           keyword to warp images over 2-D and 3-D polygons. Only
;                           available when creating output with the Z-Buffer.
;       IMAGE_INTERP:   in, optional, type=boolean, default=0
;                       Specifies the method of sampling the PATTERN array when the
;                           `IMAGE_COORD` keyword is present. The default method is to use
;                           nearest-neighbor sampling. Bilinear interpolation sampling is
;                           performed if `IMAGE_INTERP` is set. Used only when creating
;                           output with the Z-Buffer.
;       LINE_FILL:      in, optional, type=boolean, default=0
;                       Set this keyword to indicate that polygons are to be filled with
;                           parallel lines, rather than using solid or patterned filling
;                           methods. When using the line-drawing method of filling, the
;                           thickness, linestyle, orientation, and spacing of the lines
;                           may be specified with keywords.
;       LINESYTLE:      in, optiona, type=integer, default=0
;                       Line style used to draw the perimeter of the polygon. Options are::
;                           0, '-'      Solid
;                           1, '.'      Dotted
;                           2, '--'     Dashed
;                           3, '-.'     Dash-dot
;                           4, '-:'     Dash-dot-dot
;                           5, '--'     Long dash
;                           6, ' '      None
;       NOCLIP:         in, optional, type=boolean, default=0
;                       If set, suppresses clipping of the polygons.
;       NORMAL:         in, optional, type=boolean, default=0
;                       Set to indicate the polygon vertices are in normalized coordinates.
;       ORIENTATION:    in, optional, type=float, default=0
;                       Specifies the counterclockwise angle in degrees from horizontal of
;                           the lines used to fill polygons. Forces `LINE_FILL`=1.
;       PATTERN:        in, optional, type=numeric
;                       Set this keyword to a rectangular array of pixels giving the fill
;                           pattern. If this keyword parameter is omitted, POLYFILL fills
;                           the area with a solid color. The pattern array may be of any
;                           size; if it is smaller than the filled area the pattern array
;                           is cyclically repeated. Postscript output requires
;                           Device, Language_Level=2.
;       POSITION:       in, optional, type=float
;                       Set to the normal four-element position array for locating 
;                           a rectangular region in a graphics window. If this keyword is
;                           used, the x and y parameters are constructed from this position.
;       PSYM:           in, optional, type=integer/string, default='None'
;                       The symbol to make each polygon vertex. Any symbol recognized by
;                           cgSymCat is accepted.
;       RELATIVE:       in, optional, type=boolean, default=0
;                       Set to indicate that the polygon vertices are normalized to the
;                           dataspace of `TARGET`. Setting this keyword sets `DATA`=1.
;       SPACING:        in, optional, type=numeric, default=0.0
;                       The spacing, in centimeters, between the parallel lines used to
;                           fill polygons.
;       SYMCOLOR:       in, optional, type=float/fltarr, default=`FILL_COLOR`
;                       Color of each symbol that comprize the polygon vertices. If an
;                           array is provided, each symbol will be colored differently. If
;                           there are fewer elements than vertices in the polygon, symbol
;                           colors are repeated cyclically.
;       SYMSIZE:        in, optional, type=float/fltarr, default=1.0
;                       A scale factor for the size of each symbol. If an array is
;                           provided, each symbol will be sized differently. If there are
;                           fewer elements than vertices in the polygon, symbol sizes are
;                           repeated cyclically.
;       SYMTHICK:       in, optional, type=float/fltarr, default=`FILL_COLOR`
;                       Thickness of each symbol that comprize the polygon vertices. If an
;                           array is provided, each symbol will have a different thickness.
;                           If there are fewer elements than vertices in the polygon,
;                           symbol thicknesses are repeated cyclically.
;       TARGET:         in, optional, type=object
;                       If coordinates are given in `DATA` or `RELATIVE` units, set this
;                           to the graphic object that defines the data space. If no
;                           target is given, the first selected object is used. If not
;                           objects are selected, the highest ordered graphic is used.
;       T3D:            in, optional, type=boolean, default=0
;                       If set, the generalized transformation matrix in !P.T will be used.
;       THICK:          in, optional, type=float, default=1.0
;                       Thickness of the lines when `POLY_FILL` is set.
;       TRANSPARENT:    in, optional, type=integer
;                       Specifies the minimum pixel value to draw in conjunction with the
;                           `PATTERN` and `IMAGE_COORD` keywords. Pixels less than this
;                           value are not drawn and the Z-buffer is not updated.
;       ZVALUE:         in, optional, type=float
;                       Provides the Z coordinate if a Z parameter is not present in the
;                           call. This is of use only if `T3D` is set.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrGrAtom::SetProperty is also accepted
;                           via keyword inheritance.
;-
FUNCTION MrPolygon::init, xcoords, ycoords, zcoords, $
CLIP=clip, $
CONNECTIVITY=connectivity, $
DATA=data, $
DEVICE=device, $
NOCLIP=noclip, $
NORMAL=normal, $
POSITION=position, $
RELATIVE=relative, $
TARGET=target, $
T3D=t3d, $
THICK=thick, $
ZVALUE=zValue, $
;PlotS
COLOR=color, $
LINESTYLE=linestyle, $
PSYM=psym, $
;Polyfill
FILL_BACKGROUND=fill_background, $
FILL_COLOR=fill_color, $
FILL_LINESTYLE=fill_linestyle, $
IMAGE_COORD=image_coord, $
IMAGE_INTERP=image_interp, $
LINE_FILL=line_fill, $
ORIENTATION=orientation, $
PATTERN=pattern, $
SYMCOLOR=symcolor, $
SYMSIZE=simsize, $
SYMTHICK=simthick, $
SPACING=spacing, $
TRANSPARENT=transparent, $
_REF_EXTRA=extra
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN, 0
    ENDIF
    
;-----------------------------------------------------
;Target \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Find a single target.
    if n_elements(target) eq 0 then begin
        target = self -> _GetTarget(/ANY, COUNT=nTargets)
        if nTargets eq 0 then message, 'Insert MrPolygon failed. No targets available.'
        if nTargets gt 1 then begin
            message, 'More than one target available. Choosing first target.', /INFORMATIONAL
            target = target[0]
        endif
    endif

;---------------------------------------------------------------------
;Superclass & Window /////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Window is obtained by MrGrAtom
    if self -> MrGrAtom::INIT(TARGET=target, WINREFRESH=refresh) eq 0 then $
        message, 'Unable to initialize MrGrAtom'

;---------------------------------------------------------------------
;Defaults and Allocate Heap to Pointers //////////////////////////////
;---------------------------------------------------------------------
    
    ;Defaults
    data      = keyword_set(data)
    device    = keyword_set(device)
    normal    = keyword_set(normal)
    line_fill = keyword_set(line_fill)
    relative  = keyword_set(relative)
    fill_background = n_elements(fill_background) EQ 0 ? 1 : keyword_set(fill_background)
    if n_elements(color)          eq 0 then color          = 'black'
    if n_elements(fill_color)     eq 0 then fill_color     = 'rose'
    if n_elements(fill_linestyle) eq 0 then fill_linestyle = '-'
    if n_elements(linestyle)      eq 0 then linestyle      = '-'
    if n_elements(noclip)         eq 0 then noclip         = 1
    if n_elements(psym)           eq 0 then psym           = 'None'
    if n_elements(symcolor)       eq 0 then symcolor       = color
    if n_elements(symsize)        eq 0 then symsize        = 1.0
    if n_elements(symthick)       eq 0 then symthick       = 1.0
    if n_elements(thick)          eq 0 then thick          = 1.0
    
    ;Dependencies.
    if normal + data + device + relative eq 0 then normal = 1
    if relative then data = 1
    if normal + data + device ne 1 then message, 'NORMAL, DEVICE, and DATA are mutually exclusive.'
    if n_elements(orentation) gt 0 then line_fill = 1
    if line_fill then fill_background = 1
    
    ;Make pointers valid
    self.xcoords         = Ptr_New(/ALLOCATE_HEAP)
    self.ycoords         = Ptr_New(/ALLOCATE_HEAP)
    self.zcoords         = Ptr_New(/ALLOCATE_HEAP)
    self.color           = Ptr_New(/ALLOCATE_HEAP)
    self.connectivity    = Ptr_New(/ALLOCATE_HEAP)
    self.fill_color      = Ptr_New(/ALLOCATE_HEAP)
    self.fill_linestyle  = Ptr_New(/ALLOCATE_HEAP)
    self.image_coord     = Ptr_New(/ALLOCATE_HEAP)
    self.linestyle       = Ptr_New(/ALLOCATE_HEAP)
    self.orientation     = Ptr_New(/ALLOCATE_HEAP)
    self.pattern         = Ptr_New(/ALLOCATE_HEAP)
    self.psym            = Ptr_New(/ALLOCATE_HEAP)
    self.symcolor        = Ptr_New(/ALLOCATE_HEAP)
    self.symsize         = Ptr_New(/ALLOCATE_HEAP)
    self.symthick        = Ptr_New(/ALLOCATE_HEAP)
    self.z               = Ptr_New(/ALLOCATE_HEAP)

;---------------------------------------------------------------------
;Set Properties //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Set the polygon vertex locations
    self -> SetData, xcoords, ycoords, zcoords, $
                     POSITION=position, CONNECTIVITY=connectivity

    ;Set Properties
    ;   - Do not call the SetProperty method in case a subclass uses fewer properties.
     self.normal          = normal
     self.device          = device
     self.data            = data
     self.relative        = relative
    *self.color           = color
     self.fill_background = fill_background
    *self.fill_color      = fill_color
    *self.fill_linestyle  = fill_linestyle
     self.line_fill       = line_fill
    *self.linestyle       = linestyle
     self.noclip          = noclip
    *self.psym            = psym
    *self.symcolor        = symcolor
    *self.symsize         = symsize
    *self.symthick        = symthick
     self.target          = target
     self.thick           = thick
    if n_elements(clip)         gt 0 then  self.clip         = clip
    if n_elements(image_coord)  gt 0 then *self.image_coord  = image_coord
    if n_elements(image_interp) gt 0 then  self.image_interp = keyword_set(image_interp)
    if n_elements(orientation)  gt 0 then *self.orientation  = orientation
    if n_elements(pattern)      gt 0 then *self.pattern      = pattern
    if n_elements(spacing)      gt 0 then  self.spacing      = spacing
    if n_elements(t3d)          gt 0 then  self.t3d          = keyword_set(t3d)
    if n_elements(transparent)  gt 0 then  self.transparent  = keyword_set(transparent)
    if n_elements(zvalue)       gt 0 then *self.z            = zvalue
    
    ;Superclass
    if n_elements(extra) gt 0 then self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra
    
    
    ;Refersh the graphics window
    if refresh then self.window -> Refresh
                         
    Return, 1
END


;+
; The class definition module for the object.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO MrPolygon__define, class
    
    class = { MrPolygon, $
              inherits MrGrAtom, $
              
              xcoords:         ptr_new(), $
              ycoords:         ptr_new(), $
              zcoords:         ptr_new(), $
              nparams:         0B, $
              clip:            fltarr(4), $
              color:           ptr_new(), $     ;24-bit, index, or name
              connectivity:    ptr_new(), $
              data:            0B, $
              device:          0B, $
              fill_background: 0B, $
              fill_color:      ptr_new(), $     ;24-bit, index, or name
              fill_linestyle:  ptr_new(), $
              image_coord:     ptr_new(), $
              image_interp:    0B, $
              line_fill:       0B, $
              linestyle:       ptr_new(), $     ;of /LINE_FILL
              noclip:          0B, $
              normal:          0B, $
              orientation:     ptr_new(), $     ;Forces /LINE_FILL
              pattern:         ptr_new(), $
              psym:            ptr_new(), $
              relative:        0B, $
              spacing:         0.0, $
              symcolor:        ptr_new(), $
              symsize:         ptr_new(), $
              symthick:        ptr_new(), $
              target:          obj_new(), $
              t3d:             0B, $
              thick:           1.0, $
              transparent:     0B, $
              vert_colors:     ptr_new(), $
              z:               ptr_new() $
            }
END