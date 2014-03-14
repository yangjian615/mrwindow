; docformat = 'rst'
;
; NAME:
;   MrColorFill__Define
;
;*****************************************************************************************
;   Copyright (c) 2013, Matthew Argall                                                   ;
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
;   The purpose of this program is to create an polyfill object object that can drawn on a
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
;       2014/01/11  -   Written by Matthew Argall
;       2014/01/12  -   Added the doColorFillMulti method. Required changing some object
;                           properties to pointers. - MRA
;       2014/03/12  -   Automatically pick a TARGET if one has not been given. Superclass
;                           properties can now be set/get. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to draw the ColorFill object to the display window.
;-
PRO MrColorFill::Draw, $
NOERASE=noerase
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF

    ;Return if we are hiding.
    IF self.hide THEN RETURN
    
    ;If data coordinates were chosen, then restore the target's coordinate system.
    if self.data eq 1B then self.target -> RestoreCoords
    
    ;Number of parameters to pass.
    nparams = 0
    if n_elements(*self.xcoords) gt 0 then nparams += 1
    if n_elements(*self.ycoords) gt 0 then nparams += 1
    if n_elements(*self.zcoords) gt 0 then nparams += 1
    
    ;Are multiple colorfills being performed?
;    if nparas gt 1 and size(*self.xcoords, /N_DIMENSIONS) gt 1 then doMulti = 1 else doMulti = 0
    doMulti = 0
    
    ;Color fill
    if doMulti $
        then self -> doColorFillMulti, nparams $
        else self -> doColorFill, nparams
END


;+
; This method draws the axis object.
;-
PRO MrColorFill::doColorFill, nparams
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Fill the region.
    case nparams of
        1: cgColorFill, *self.xcoords, $
                        CLIP         =  self.clip, $
                        COLOR        = *self.color, $
                        DATA         =  self.data, $
                        DEVICE       =  self.device, $
                        IMAGE_COORD  = *self.image_coord, $
                        IMAGE_INTERP =  self.image_interp, $
                        LINE_FILL    =  self.line_fill, $
                        LINESTYLE    = *self.linestyle, $
                        NOCLIP       =  self.noclip, $
                        NORMAL       =  self.normal, $
                        ORIENTATION  = *self.orientation, $
                        PATTERN      = *self.pattern, $
                        POSITION     = *self.position, $
                        SPACING      = *self.spacing, $
                        T3D          =  self.t3d, $
                        THICK        = *self.thick, $
                        TRANSPARENT  =  self.transparent, $
                        Z            = *self.z
                        
        2: cgColorFill, *self.xcoords, *self.ycoords, $
                        CLIP         =  self.clip, $
                        COLOR        = *self.color, $
                        DATA         =  self.data, $
                        DEVICE       =  self.device, $
                        IMAGE_COORD  = *self.image_coord, $
                        IMAGE_INTERP =  self.image_interp, $
                        LINE_FILL    =  self.line_fill, $
                        LINESTYLE    = *self.linestyle, $
                        NOCLIP       =  self.noclip, $
                        NORMAL       =  self.normal, $
                        ORIENTATION  = *self.orientation, $
                        PATTERN      = *self.pattern, $
                        POSITION     = *self.position, $
                        SPACING      = *self.spacing, $
                        T3D          =  self.t3d, $
                        THICK        = *self.thick, $
                        TRANSPARENT  =  self.transparent, $
                        Z            = *self.z
                        
        3: cgColorFill, *self.xcoords, *self.ycoords, *self.zcoords, $
                        CLIP         =  self.clip, $
                        COLOR        = *self.color, $
                        DATA         =  self.data, $
                        DEVICE       =  self.device, $
                        IMAGE_COORD  = *self.image_coord, $
                        IMAGE_INTERP =  self.image_interp, $
                        LINE_FILL    =  self.line_fill, $
                        LINESTYLE    = *self.linestyle, $
                        NOCLIP       =  self.noclip, $
                        NORMAL       =  self.normal, $
                        ORIENTATION  = *self.orientation, $
                        PATTERN      = *self.pattern, $
                        POSITION     = *self.position, $
                        SPACING      = *self.spacing, $
                        T3D          =  self.t3d, $
                        THICK        = *self.thick, $
                        TRANSPARENT  =  self.transparent, $
                        Z            = *self.z
    endcase
END


;+
; This method draws the axis object.
;-
PRO MrColorFill::doColorFillMulti, nparams
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Number of ColorFills to perform.
    n = n_elements((*self.xcoords)[0,*])
    
    ;How many elements are present? Repeat them cyclicly
    nColors = n_elements(self.color)
;    nLineFill = n_elements(self.line_fill)
    nLinestyle = n_elements(self.linestyle)
    norientation = n_elements(self.orientation)
    nspacing = n_elements(self.spacing)
    nthick = n_elements(self.thick)
    
    ;Fill the region.
    case nparams of
        2: for i = 0, n - 1 do $
            cgColorFill, (*self.xcoords)[*,i], (*self.ycoords)[*,i], $
                         CLIP         =   self.clip, $
                         COLOR        = (*self.color)[i mod nColors], $
                         DATA         =   self.data, $
                         DEVICE       =   self.device, $
                         IMAGE_COORD  =  *self.image_coord, $
                         IMAGE_INTERP =   self.image_interp, $
                         LINE_FILL    =  *self.line_fill, $
                         LINESTYLE    = (*self.linestyle)[i mod nLinestyle], $
                         NOCLIP       =   self.noclip, $
                         NORMAL       =   self.normal, $
                         ORIENTATION  = (*self.orientation)[i mod nOrientation], $
                         PATTERN      =  *self.pattern, $
                         POSITION     =  *self.position, $
                         SPACING      = (*self.spacing)[i mod nSpacing], $
                         T3D          =   self.t3d, $
                         THICK        = (*self.thick)[i mod nThick], $
                         TRANSPARENT  =   self.transparent, $
                         Z            =  *self.z
                        
        3: for i = 0, n - 1 do $
            cgColorFill, (*self.xcoords)[*,i], (*self.ycoords)[*,i], (*self.zcoords)[*,i], $
                         CLIP         =   self.clip, $
                         COLOR        = (*self.color)[i mod nColors], $
                         DATA         =   self.data, $
                         DEVICE       =   self.device, $
                         IMAGE_COORD  =  *self.image_coord, $
                         IMAGE_INTERP =   self.image_interp, $
                         LINE_FILL    =  *self.line_fill, $
                         LINESTYLE    = (*self.linestyle)[i mod nLinestyle], $
                         NOCLIP       =   self.noclip, $
                         NORMAL       =   self.normal, $
                         ORIENTATION  = (*self.orientation)[i mod nOrientation], $
                         PATTERN      =  *self.pattern, $
                         POSITION     =  *self.position, $
                         SPACING      = (*self.spacing)[i mod nSpacing], $
                         T3D          =   self.t3d, $
                         THICK        = (*self.thick)[i mod nThick], $
                         TRANSPARENT  =   self.transparent, $
                         Z            =  *self.z
    endcase
END


;+
;   This method serves as a wrapper for the PolyFillV function.
;
; :Params:
;       X:              in, required, type=intarr
;                       X subscripts of the vertices that define the polygon.
;       Y:              in, required, type=intarr
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
;-
function MrColorFill::PolyFillV, x, y, Sx, Sy, run_length
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
    
    ;Call PolyFillV
    result = polyfillv(x, y, Sx, Sy, run_length)

    return, result
END


;+
;   This method obtains the current properties of the object. 
; 
; :Keywords:
;       XCOORDS:        out, required, type=numeric
;                       A vector argument providing the X coordinates of the points to be
;                           connected. The vector must contain at least three elements. If
;                           only one argument is specified, X must be an array of either two
;                           or three vectors (i.e., (2,*) or (3,*)). In this special case,
;                           the vector X[0,*] specifies the X values, X[1,*] specifies Y,
;                           and X[2,*] contain the Z values.
;       YCOORDS:        out, optional, type=numeric
;                       A vector argument providing the Y coordinates of the points to be
;                           connected. Y must contain at least three elements.
;       ZCOORDS:        out, optional, type=number
;                       An optional vector argument providing the Z coordinates of the
;                           points to be connected. Z must contain at least three elements.
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
;       POSITION:       out, optional, type=float
;                       Set to the normal four-element normalized position array for locating 
;                           a rectangular region in a graphics window. If this keyword is used,
;                           the x and y parameters are constructed from this position.
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
PRO MrColorFill::GetProperty, $ 
XCOORDS=xcoords, $
YCOORDS=ycoords, $
ZCOORDS=zcoords, $
CLIP=clip, $
COLOR=color, $
DATA=data, $
DEVICE=device, $
IMAGE_COORD=image_coord, $
IMAGE_INTERP=image_interp, $
LINE_FILL=line_fill, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
PATTERN=pattern, $
POSITION=position, $
SPACING=spacing, $
T3D=t3d, $
TARGET=target, $
THICK=thick, $
TRANSPARENT=transparent, $
Z=zValue, $
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
    IF arg_present(xcoords)      GT 0 THEN xcoords      = *self.xcoords
    IF arg_present(ycoords)      GT 0 THEN ycoords      = *self.ycoords
    IF arg_present(zcoords)      GT 0 THEN zcoords      = *self.zcoords
    IF arg_present(clip)         GT 0 THEN clip         =  self.clip
    IF arg_present(color)        GT 0 THEN color        = *self.color
    IF arg_present(data)         GT 0 THEN data         =  self.data
    IF arg_present(device)       GT 0 THEN device       =  self.device
    IF arg_present(image_coord)  GT 0 THEN image_coord  = *self.image_coord
    IF arg_present(image_interp) GT 0 THEN image_interp =  self.image_interp
    IF arg_present(line_fill)    GT 0 THEN line_fill    =  self.line_fill
    IF arg_present(linestyle)    GT 0 THEN linestyle    = *self.linestyle
    IF arg_present(noclip)       GT 0 THEN noclip       =  self.noclip
    IF arg_present(normal)       GT 0 THEN normal       =  self.normal
    IF arg_present(orientation)  GT 0 THEN orientation  = *self.orientation
    IF arg_present(pattern)      GT 0 THEN pattern      = *self.pattern
    IF arg_present(position)     GT 0 THEN position     = *self.position
    IF arg_present(spacing)      GT 0 THEN spacing      = *self.spacing
    IF arg_present(t3d)          GT 0 THEN t3d          =  self.t3d
    IF arg_present(thick)        GT 0 THEN thick        = *self.thick
    IF arg_present(transparent)  GT 0 THEN transparent  =  self.transparent
    IF arg_present(zValue)       GT 0 THEN zValue       = *self.z

    ;Target
    IF Arg_Present(target) GT 0 THEN IF Obj_Valid(self.target) GT 0 $
        THEN target = self.target $
        ELSE target = Obj_New()
    
    ;Superclass
    IF Arg_Present(extra) GT 0 THEN self -> MrGrAtom::GetProperty, _STRICT_EXTRA=extra
END


;+
;   This method sets the properties of the object.
;
; :Keywords:
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
;       POSITION:       in, optional, type=float
;                       Set to the normal four-element normalized position array for locating 
;                           a rectangular region in a graphics window. If this keyword is used,
;                           the x and y parameters are constructed from this position.
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
PRO MrColorFill::SetProperty, $ 
XCOORDS=xcoords, $
YCOORDS=ycoords, $
ZCOORDS=zcoords, $
CLIP=clip, $
COLOR=color, $
DATA=data, $
DEVICE=device, $
IMAGE_COORD=image_coord, $
IMAGE_INTERP=image_interp, $
LINE_FILL=line_fill, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
PATTERN=pattern, $
POSITION=position, $
SPACING=spacing, $
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
    IF N_Elements(xcoords)      GT 0 THEN *self.xcoords      = xcoords
    IF N_Elements(ycoords)      GT 0 THEN *self.ycoords      = ycoords
    IF N_Elements(zcoords)      GT 0 THEN *self.zcoords      = zcoords
    IF N_Elements(clip)         GT 0 THEN  self.clip         = clip
    IF N_Elements(color)        GT 0 THEN *self.color        = color
    IF N_Elements(image_coord)  GT 0 THEN *self.image_coord  = image_coord
    IF N_Elements(image_interp) GT 0 THEN  self.image_interp = image_interp
    IF N_Elements(line_fill)    GT 0 THEN  self.line_fill    = line_fill
    IF N_Elements(linestyle)    GT 0 THEN *self.linestyle    = linestyle
    IF N_Elements(noclip)       GT 0 THEN  self.noclip       = noclip
    IF N_Elements(orientation)  GT 0 THEN *self.orientation  = orientation
    IF N_Elements(pattern)      GT 0 THEN *self.pattern      = pattern
    IF N_Elements(position)     GT 0 THEN *self.position     = position
    IF N_Elements(spacing)      GT 0 THEN *self.spacing      = spacing
    IF N_Elements(t3d)          GT 0 THEN  self.t3d          = t3d
    IF N_Elements(thick)        GT 0 THEN *self.thick        = thick
    IF N_Elements(transparent)  GT 0 THEN  self.transparent  = transparent
    IF N_Elements(zValue)       GT 0 THEN *self.z            = zValue

    IF N_Elements(target) GT 0 THEN IF Obj_Valid(target) $
        THEN self.target = target $
        ELSE self.target = Obj_New()
    
    IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra

;-----------------------------------------------------
;Data, Device, Normal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;They depend on one another.
    if n_elements(data) gt 0 then begin
        data = keyword_set(data)
        if data then begin
            self.normal = 0B
            self.device = 0B
        endif
        
        self.data = data
    endif
    
    if n_elements(device) gt 0 then begin
        device = keyword_set(device)
        if device then begin
            self.data = 0B
            self.normal = 0B
        endif
        
        self.device = device
    endif
    
    if n_elements(normal) gt 0 then begin
        normal = keyword_set(normal)
        if normal then begin
            self.data = 0B
            self.device = 0B
        endif
        
        self.normal = normal
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
pro MrColorFill::cleanup
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
    ptr_free, self.image_coord
    ptr_free, self.linestyle
    ptr_free, self.orientation
    ptr_free, self.pattern
    ptr_free, self.position
    ptr_free, self.spacing
    ptr_free, self.thick
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
;                       The name of the fill color. Color names are those used with cgColor. 
;                           This value can also be a long integer or an index into the
;                           current color table.
;       DATA:           in, optional, type=boolean, default=1
;                       Indicate that polygon vertices are in data coordinates. This is
;                           the default.
;       DEVICE:         in, optional, type=boolean, default=0
;                       Set to indicate the polygon vertices are in device coordinates.
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
;                       Line style used to draw lines when `LINE_FILL` is set.
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
;                       Set to the normal four-element normalized position array for locating 
;                           a rectangular region in a graphics window. If this keyword is used,
;                           the x and y parameters are constructed from this position.
;       SPACING:        in, optional, type=numeric
;                       The spacing, in centimeters, between the parallel lines used to
;                           fill polygons.
;       TARGET:         in, optional, type=object
;                       If coordinates are given in data units, set this to the graphic
;                           object that defines the data space. If no target is given, the
;                           first selected object is used. If not objects are selected,
;                           the highest ordered graphic is used.
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
FUNCTION MrColorFill::init, xcoords, ycoords, zcoords, $
CURRENT=current, $
CLIP=clip, $
COLOR=color, $
DATA=data, $
DEVICE=device, $
IMAGE_COORD=image_coord, $
IMAGE_INTERP=image_interp, $
LINE_FILL=line_fill, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
PATTERN=pattern, $
POSITION=position, $
SPACING=spacing, $
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
        RETURN, 0
    ENDIF
    
;-----------------------------------------------------
;Target \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Find a single target.
    if n_elements(target) eq 0 then begin
        target = self -> _GetTarget(/ANY, COUNT=nTargets)
        if nTargets eq 0 then message, 'Insert MrColorFill failed. No targets available.'
        if nTargets gt 1 then begin
            message, 'More than one target available. Choosing first target.', /INFORMATIONAL
            target = target[0]
        endif
    endif

;---------------------------------------------------------------------
;Superclass & Window /////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Window is obtained by MrGrAtom
    if self -> MrGrAtom::INIT(TARGET=target) eq 0 then $
        message, 'Unable to initialize MrGrAtom'
    
    ;Refresh the window?
    self.window -> GetProperty, REFRESH=refreshIn
    if refreshIn then self.window -> Refresh, /DISABLE

;---------------------------------------------------------------------
;Defaults and Allocate Heap to Pointers //////////////////////////////
;---------------------------------------------------------------------
    
    ;Defaults
    setDefaultValue, color, 'rose'
    setDefaultValue, thick, 1.0
    
    ;Make pointers valid
    self.xcoords     = Ptr_New(/ALLOCATE_HEAP)
    self.ycoords     = Ptr_New(/ALLOCATE_HEAP)
    self.zcoords     = Ptr_New(/ALLOCATE_HEAP)
    self.color       = Ptr_New(/ALLOCATE_HEAP)
    self.image_coord = Ptr_New(/ALLOCATE_HEAP)
    self.linestyle   = Ptr_New(/ALLOCATE_HEAP)
    self.orientation = Ptr_New(/ALLOCATE_HEAP)
    self.pattern     = Ptr_New(/ALLOCATE_HEAP)
    self.position    = Ptr_New(/ALLOCATE_HEAP)
    self.spacing     = Ptr_New(/ALLOCATE_HEAP)
    self.thick       = Ptr_New(/ALLOCATE_HEAP)
    self.z           = Ptr_New(/ALLOCATE_HEAP)

;---------------------------------------------------------------------
;Set Properties //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    self -> SetProperty, XCOORDS=xcoords, $
                         YCOORDS=ycoords, $
                         ZCOORDS=zcoords, $
                         CLIP=clip, $
                         COLOR=color, $
                         DATA=data, $
                         DEVICE=device, $
                         IMAGE_COORD=image_coord, $
                         IMAGE_INTERP=image_interp, $
                         LINE_FILL=line_fill, $
                         LINESTYLE=linestyle, $
                         NOCLIP=noclip, $
                         NORMAL=normal, $
                         ORIENTATION=orientation, $
                         PATTERN=pattern, $
                         POSITION=position, $
                         SPACING=spacing, $
                         T3D=t3d, $
                         TARGET=target, $
                         THICK=thick, $
                         TRANSPARENT=transparent, $
                         ZVALUE=zValue, $
                         _EXTRA=extra
    
    ;Refersh the graphics window
    if refreshIn then self.window -> Refresh
                         
    Return, 1
END


;+
; The class definition module for the object.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO MrColorFill__define, class
    
    class = { MrColorFill, $
              inherits MrGrAtom, $
              
              xcoords: ptr_new(), $
              ycoords: ptr_new(), $
              zcoords: ptr_new(), $
              clip: fltarr(4), $
              color: ptr_new(), $           ;'', $
              data: 0B, $
              device: 0B, $
              image_coord: ptr_new(), $
              image_interp: 0B, $
              line_fill: 0B, $
              linestyle: ptr_new(), $       ;0B, $
              noclip: 0B, $
              normal: 0B, $
              orientation: ptr_new(), $
              pattern: ptr_new(), $
              position: ptr_new(), $
              spacing: ptr_new(), $
              target: obj_new(), $
              t3d: 0B, $
              thick: ptr_new(), $           ;0.0, $
              transparent: 0B, $
              z: ptr_new() $
            }
END