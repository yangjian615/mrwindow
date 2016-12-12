; docformat = 'rst'
;
; NAME:
;   MrCircle__Define
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
;   Create and draw circles.
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
;       2014/09/22  -   Written by Matthew Argall
;-
;*****************************************************************************************
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
FUNCTION MrCircle::Create_Circles, r, x_center, y_center, $
CONNECTIVITY=connectivity, $
NVERTICES=nVertices
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN, -1
    ENDIF
    
    ;Defaults
    IF N_Elements(nVertices) EQ 0 THEN nVertices = 100
    IF N_Elements(x_center)  EQ 0 THEN x_center  = 0
    IF N_Elements(y_center)  EQ 0 THEN y_center  = 0
    
    ;Create a circle with unit radius
    x = cos(2*!pi*FIndGen(nVertices)/(nVertices-1))
    y = sin(2*!pi*FIndGen(nVertices)/(nVertices-1))

    ;Create the circles
    nCircles = N_Elements(r)
    r_temp   = Rebin(1#r, nVertices, nCircles)
    x        = Rebin(x,   nVertices, nCircles) * r_temp
    y        = Rebin(y,   nVertices, nCircles) * temporary(r_temp)
    
    ;Shift the centers
    x = x + Rebin(1#x_center, nVertices, nCircles)
    y = y + Rebin(1#y_center, nVertices, nCircles)
    
    ;Determine the connectivity
    ;   - [# of vertices, Index of each vertex]
    ;   - Repeat pattern for each circle.
    connectivity = Rebin([nVertices, LIndGen(nVertices)], nVertices+1, nCircles)
    connectivity[1:nVertices,*] += Rebin(LIndgen(1,nCircles)*nVertices, nVertices, nCircles)
    connectivity = Reform(connectivity, (nVertices+1)*nCircles)
    
    ;Return the circles
    circles = Transpose([[Reform(Temporary(x), nVertices*nCircles)], $
                        [Reform(Temporary(y), nVertices*nCircles)]])
               
    return, circles
END


;+
;   Set the polygon vertex locations.
;
; :Params:
;       R:              out, required, type=numeric
;                       A vector argument providing the X coordinates of the points to be
;                           connected. The vector must contain at least three elements.
;       X_CENTER:       out, optional, type=numeric
;                       A vector argument providing the Y coordinates of the points to be
;                           connected. Y must contain at least three elements.
;       Y_CENTER:       out, optional, type=number
;                       An optional vector argument providing the Z coordinates of the
;                           points to be connected. Z must contain at least three elements.
;-
PRO MrCircle::GetData, r, x_center, y_center
    Compile_Opt StrictArr
    On_Error, 2
    
    SWITCH N_Params() OF
        3: y_center = *self._y_center
        2: x_center = *self._x_center
        1: r        = *self._radius
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
PRO MrCircle::GetProperty, $
CLIP=clip, $
COLOR=color, $
DATA=data, $
DEVICE=device, $
FILL_BACKGROUND=fill_background, $
FILL_COLOR=fill_color, $
FILL_LINESTYLE=fill_linestyle, $
LINE_FILL=line_fill, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
RELATIVE=relative, $
SPACING=spacing, $
T3D=t3d, $
TARGET=target, $
THICK=thick, $
ZVALUE=zValue, $
_REF_EXTRA=extra
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN
    ENDIF
    
    ;Object Properties
    IF Arg_Present(clip)            GT 0 THEN clip            =  self.clip
    IF Arg_Present(color)           GT 0 THEN color           = *self.color
    IF Arg_Present(data)            GT 0 THEN data            =  self.data
    IF Arg_Present(device)          GT 0 THEN device          =  self.device
    IF Arg_Present(fill_background) GT 0 THEN fill_background =  self.fill_background
    IF Arg_Present(fill_color)      GT 0 THEN fill_color      = *self.fill_color
    IF Arg_Present(fill_linestyle)  GT 0 THEN fill_linestyle  = *self.fill_linestyle
    IF Arg_Present(line_fill)       GT 0 THEN line_fill       =  self.line_fill
    IF Arg_Present(linestyle)       GT 0 THEN linestyle       = *self.linestyle
    IF Arg_Present(noclip)          GT 0 THEN noclip          =  self.noclip
    IF Arg_Present(normal)          GT 0 THEN normal          =  self.normal
    IF Arg_Present(orientation)     GT 0 THEN orientation     = *self.orientation
    IF Arg_Present(pattern)         GT 0 THEN pattern         = *self.pattern
    IF Arg_Present(relative)        GT 0 THEN relative        =  self.relative
    IF Arg_Present(spacing)         GT 0 THEN spacing         = *self.spacing
    IF Arg_Present(t3d)             GT 0 THEN t3d             =  self.t3d
    IF Arg_Present(thick)           GT 0 THEN thick           =  self.thick
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
PRO MrCircle::SetData, r, x_center, y_center, $
CONNECTIVITY=connectivity, $
NVERTICES=nVertices, $
POSITION=position
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN
    ENDIF
    
    ;Create the circles
    circles = self -> Create_Circles(r, x_center, y_center, $
                                     NVERTICES=nVertices, CONNECTIVITY=connectivity)
    
    ;Set the data
    self -> MrPolygon::SetData, circles, CONNECTIVITY=connectivity
    
    ;Set object properties
    *self._radius   = r
    *self._x_center = x_center
    *self._y_center = y_center
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
PRO MrCircle::SetProperty, $
CLIP=clip, $
COLOR=color, $
DATA=data, $
DEVICE=device, $
FILL_BACKGROUND=fill_background, $
FILL_COLOR=fill_color, $
FILL_LINESTYLE=fill_linestyle, $
LINE_FILL=line_fill, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
RELATIVE=relative, $
SPACING=spacing, $
T3D=t3d, $
TARGET=target, $
THICK=thick, $
ZVALUE=zValue, $
_REF_EXTRA=extra
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN
    ENDIF
    
    ;Object Properties
    IF N_Elements(clip)            GT 0 THEN  self.clip            = clip
    IF N_Elements(color)           GT 0 THEN *self.color           = color
    IF N_Elements(fill_background) GT 0 THEN  self.fill_background = keyword_set(fill_background)
    IF N_Elements(fill_color)      GT 0 THEN *self.fill_color      = fill_color
    IF N_Elements(fill_linestyle)  GT 0 THEN *self.fill_linestyle  = fill_linestyle
    IF N_Elements(linestyle)       GT 0 THEN *self.linestyle       = linestyle
    IF N_Elements(noclip)          GT 0 THEN  self.noclip          = noclip
    IF N_Elements(spacing)         GT 0 THEN *self.spacing         = spacing
    IF N_Elements(t3d)             GT 0 THEN  self.t3d             = t3d
    IF N_Elements(thick)           GT 0 THEN  self.thick           = thick
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
pro MrCircle::cleanup
    compile_opt idl2
    
    ; catch the error.
    catch, theerror
    if theerror ne 0 then begin
        catch, /CANCEL
        MrPrintF, 'LogErr'
        return
    endif
    
    ;free pointers
    ptr_free, self._radius
    ptr_free, self._x_center
    ptr_free, self._y_center
    
    ;Superclass
    self -> MrPolygon::Cleanup
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
FUNCTION MrCircle::init, r, x_center, y_center, $
;MrCircle
NVERTICES=nVertices, $
;MrPolygon
CLIP=clip, $
COLOR=color, $
CONNECTIVITY=connectivity, $
DATA=data, $
DEVICE=device, $
FILL_BACKGROUND=fill_background, $
FILL_COLOR=fill_color, $
FILL_LINESTYLE=fill_linestyle, $
LINE_FILL=line_fill, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
RELATIVE=relative, $
SPACING=spacing, $
T3D=t3d, $
TARGET=target, $
THICK=thick, $
ZVALUE=zValue, $
_REF_EXTRA=extra
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN, 0
    ENDIF
    
    ;Allocate heap
    self._radius   = Ptr_New(/ALLOCATE_HEAP)
    self._x_center = Ptr_New(/ALLOCATE_HEAP)
    self._y_center = Ptr_New(/ALLOCATE_HEAP)

    ;Default to not filling the background (opposite MrPolyFill)
    fill_background = keyword_set(fill_background)

    ;Initialize the object
    ;   - The [xyz]coords parameters have been repurposed
    ;   - The ::SetData method, when called, will take care of the translation.
    success = self -> MrPolygon::Init( r, x_center, y_center, $
                                       CLIP            = clip, $
                                       COLOR           = color, $
                                       DATA            = data, $
                                       DEVICE          = device, $
                                       FILL_BACKGROUND = fill_background, $
                                       FILL_COLOR      = fill_color, $
                                       FILL_LINESTYLE  = fill_linestyle, $
                                       LINE_FILL       = line_fill, $
                                       LINESTYLE       = linestyle, $
                                       NOCLIP          = noclip, $
                                       NORMAL          = normal, $
                                       ORIENTATION     = orientation, $
                                       RELATIVE        = relative, $
                                       SPACING         = spacing, $
                                       TARGET          = target, $
                                       T3D             = t3d, $
                                       THICK           = thick, $
                                       ZVALUE          = zValue $
 ;                                      IMAGE_COORD     = image_coord, $
 ;                                      IMAGE_INTERP    = image_interp, $
 ;                                      PATTERN         = pattern, $
 ;                                      POSITION        = position, $
 ;                                      PSYM            = psym, $
 ;                                      SYMCOLOR        = symcolor, $
 ;                                      SYMSIZE         = simsize, $
 ;                                      SYMTHICK        = simthick, $
 ;                                      TRANSPARENT     = transparent, $
                                     )
    IF success EQ 0 THEN RETURN, 0
    
    ;Superclass
    IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra
    
    Return, 1
END


;+
; The class definition module for the object.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO MrCircle__define, class
    
    class = { MrCircle, $
              inherits MrPolygon, $
              _radius:   ptr_new(), $
              _x_center: ptr_new(), $
              _y_center: ptr_new() $
            }
END