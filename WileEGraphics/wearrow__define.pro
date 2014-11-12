; docformat = 'rst'
;
; NAME:
;   weArrow__Define
;
; PURPOSE:
;+
;   The purpose of this program is to serve as an object for the cgArrow procedure.
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
;       05/27/2013  -   Written by Matthew Argall
;       2013/11/20  -   Inherit MrGrAtom. - MRA
;-
;*****************************************************************************************
;+
; This method draws the arrow object.
;-
PRO weArrow::Draw
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    IF self.hide THEN RETURN
    
    ;Draw the arrow.
    cgArrow, *self.x0, *self.y0, *self.x1, *self.y1, $
             COLOR = *self.color, $
             DATA = *self.data, $
             DEVICE = *self.device, $
             HSIZE = *self.hsize, $
             HTHICK = *self.hthick, $
             LINESTYLE = *self.linestyle, $
             MAP_OBJECT = self.map_object, $
             NOCLIP = *self.noclip, $
             NORMAL = *self.normal, $
             PSYM = *self.psym, $
             SOLID = *self.solid, $
             SYMCOLOR = *self.symcolor, $
             SYMSIZE = *self.symsize, $
             T3D = *self.t3d, $
             THICK = *self.thick, $
             Z = *self.z
END


;+
;   This method obtains the current properties of the object. 
; 
; :Keywords:
;    x0: out, required, type=integer/float
;         The x location of the blunt end of the arrow.
;    x1: out, required, type=integer/float
;         The x location of the sharp end of the arrow.
;    y0: out, required, type=integer/float
;         The y location of the blunt end of the arrow.
;    y1: out, required, type=integer/float
;         The y location of the sharp end of the arrow.
;     addcmd: out, optional, type=boolean
;         An alternative way to set the `Window` keyword.
;     clip: out, optional, type=fltarr(4)
;         The coordinates of a rectangle used to clip the graphics output.
;     color: out, optional, type=string/integer/long
;         An alternative way to specify the color to use in erasing the graphics window.
;         Color names are those used with cgColor. This parameter is used in
;         preference to the background_color parameter.
;     data: out, optional, type=boolean
;          Set this keyword of the arrow locations are in data coordinates.
;          Device coordinates are assumed.
;     device: out, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in device coordinates.
;     hsize: out, optional, type=float
;         The size of the the arrow head. By default 1/64th the width
;         of the device. (!D.X_SIZE / 64.)
;     hthick: out, optional, type=float
;         The thickness of the line drawing the arrowhead. 
;     linestyle: out, optional, type=integer
;         The line style of the arrow. Line style integers as in PLOT.
;     map_object: out, optional, type=object
;        If you are drawing on a map projection set up with Map_Proj_Init
;        and using projected meter space, rather than lat/lon space, then you can use this
;        keyword to provide a cgMap object that will allow you to convert the `x` and `y`
;        parameters from longitude and latitude, respectively, to projected meter space
;        before drawing.
;     noclip: out, optional, type=boolean
;        Set this keyword to suppress clipping of the plot.
;     normal: out, optional, type=boolean
;          Set this keyword of the arrow locations are in normalized coordinates.
;          Device coordinates are assumed.
;     psym: out, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46. May also be specified as a
;        symbol names. See `cgSymCat` for details.
;     solid: out, optional, type=boolean
;          Set this keyword to draw solid, filled arrows.
;     symcolor: out, optional, type=string/integer/vector
;        If this keyword is a string, the name of the symbol color. By default, same as COLOR.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;        May be a vector of the same length as X.
;     symsize: out, optional, type=float/vector
;        A scalar or vector of symbol sizes. Default is 1.0. May be a vector of the same 
;        length as X.
;     t3d: out, optional, type=boolean
;          Set this keyword to indicate that the generalized transformation matrix in
;          !P.T is to be used.
;     thick: out, optional, type=float
;         The thickness of the line drawing the shaft of the arrow. 
;     window: out, optional, type=boolean
;         Set this keyword to add the command to an cgWindow application.
;     z: out, optional, type=float
;         The Z coordinate if a Z parameter is not present in the call.
;-
PRO weArrow::GetProperty, $
X0 = x0, $
Y0 = y0, $
X1 = x1, $
Y1 = y1, $
CLIP = clip, $
COLOR = color, $
DATA = data, $
DEVICE=device, $
HSIZE = hsize, $
HTHICK = hthick, $
LINESTYLE = linestyle, $
MAP_OBJECT = map_object, $
NOCLIP = noclip, $
NORMAL = normal, $
PSYM = psym, $
SOLID = solid, $
SYMCOLOR = symcolor, $
SYMSIZE = symsize, $
T3D = t3d, $
THICK = thick, $
Z = z, $
_REF_EXTRA=ref_extra
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Get Object Properties
    IF Arg_Present(x0)        AND N_Elements(*self.x0)        NE 0 THEN x0 = *self.x0
    IF Arg_Present(x1)        AND N_Elements(*self.x1)        NE 0 THEN x1 = *self.x1
    IF Arg_Present(y0)        AND N_Elements(*self.y0)        NE 0 THEN y0 = *self.y0
    IF Arg_Present(y1)        AND N_Elements(*self.y1)        NE 0 THEN y1 = *self.y1
    IF Arg_Present(clip)      AND N_Elements(*self.clip)      NE 0 THEN clip = *self.clip
    IF Arg_Present(color)     AND N_Elements(*self.color)     NE 0 THEN color = *self.color
    IF Arg_Present(data)      AND N_Elements(*self.data)      NE 0 THEN data = *self.data
    IF Arg_Present(device)    AND N_Elements(*self.device)    NE 0 THEN device = *self.device
    IF Arg_Present(hsize)     AND N_Elements(*self.hsize)     NE 0 THEN hsize = *self.size
    IF Arg_Present(hthick)    AND N_Elements(*self.hthick)    NE 0 THEN hthick = *self.thick
    IF Arg_Present(linestyle) AND N_Elements(*self.linestyle) NE 0 THEN linestyle = *self.linestyle
    IF Arg_Present(noclip)    AND N_Elements(*self.noclip)    NE 0 THEN noclip = *self.noclip
    IF Arg_Present(normal)    AND N_Elements(*self.normal)    NE 0 THEN normal = *self.normal
    IF Arg_Present(psym)      AND N_Elements(*self.psym)      NE 0 THEN psym = *self.psym
    IF Arg_Present(solid)     AND N_Elements(*self.solid)     NE 0 THEN solid = *self.solid
    IF Arg_Present(symcolor)  AND N_Elements(*self.symcolor)  NE 0 THEN symcolor = *self.symcolor
    IF Arg_Present(symsize)   AND N_Elements(*self.symsize)   NE 0 THEN symsize = *self.symsize
    IF Arg_Present(t3d)       AND N_Elements(*self.t3d)       NE 0 THEN t3d = *self.t3d
    IF Arg_Present(thick)     AND N_Elements(*self.thick)     NE 0 THEN thick = *self.thick
    IF Arg_Present(z)         AND N_Elements(*self.z)         NE 0 THEN z = *self.z
    
    IF Arg_Present(map_obj) NE 0 THEN BEGIN
        IF Obj_Valid(self.map_object) $
            THEN map_object = self.map_object $
            ELSE map_obj = Obj_New()
    ENDIF
    
    IF N_Elements(extra) GT 0 THEN self -> GetProperty, _STRICT_EXTRA=extra
END


;+
;   This method sets the properties of the object.
;
; :Keywords:
;    x0: in, required, type=integer/float
;         The x location of the blunt end of the arrow. May be a vector. Assumes
;         device coordinates.
;    x1: in, required, type=integer/float
;         The x location of the sharp end of the arrow. May be a vector. Assumes
;         device coordinates.
;    y0: in, required, type=integer/float
;         The y location of the blunt end of the arrow. May be a vector. Assumes
;         device coordinates.
;    y1: in, required, type=integer/float
;         The y location of the sharp end of the arrow. May be a vector. Assumes
;         device coordinates.
;     addcmd: in, optional, type=boolean
;         An alternative way to set the `Window` keyword.
;     color: in, optional, type=string/integer/long
;         An alternative way to specify the color to use in erasing the graphics window.
;         Color names are those used with cgColor. This parameter is used in
;         preference to the background_color parameter.
;     data: in, optional, type=boolean
;          Set this keyword of the arrow locations are in data coordinates.
;          Device coordinates are assumed.
;     device: in, optional, type=boolean
;          Set this keyword to indicate xloc and yloc are in device coordinates.
;     draw: in, optional, type=boolean, default=0
;          Draw the arrow after setting its properties.
;     hsize: in, optional, type=float
;         The size of the the arrow head. By default 1/64th the width
;         of the device. (!D.X_SIZE / 64.)
;     hthick: in, optional, type=float
;         The thickness of the line drawing the arrowhead. 
;     linestyle: in, optional, type=integer
;         The line style of the arrow. Line style integers as in PLOT.
;     map_object: in, optional, type=object
;          If you are drawing on a map projection set up with Map_Proj_Init
;          and using projected meter space, rather than lat/lon space, then you can use this
;          keyword to provide a cgMap object that will allow you to convert the `x` and `y`
;          parameters from longitude and latitude, respectively, to projected meter space
;          before drawing.
;     noclip: in, optional, type=boolean
;          Set this keyword to suppress clipping of the plot.
;     normal: in, optional, type=boolean
;          Set this keyword of the arrow locations are in normalized coordinates.
;          Device coordinates are assumed.
;     psym: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46. May also be specified as a
;        symbol names. See `cgSymCat` for details.
;     solid: in, optional, type=boolean
;          Set this keyword to draw solid, filled arrows.
;     symcolor: in, optional, type=string/integer/vector, default=COLOR
;        If this keyword is a string, the name of the symbol color. By default, same as COLOR.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;        May be a vector of the same length as X.
;     symsize: in, optional, type=float/vector, default=1.0
;        A scalar or vector of symbol sizes. Default is 1.0. May be a vector of the same 
;        length as X.
;     t3d: in, optional, type=boolean
;          Set this keyword to indicate that the generalized transformation matrix in
;          !P.T is to be used.
;     thick: in, optional, type=float
;         The thickness of the line drawing the shaft of the arrow. 
;     z: in, optional, type=float
;         The Z coordinate if a Z parameter is not present in the call.
;-
PRO weArrow::SetProperty, $
X0 = x0, $
Y0 = y0, $
X1 = x1, $
Y1 = y1, $
CLIP = clip, $
COLOR = color, $
DATA = data, $
DEVICE = device, $
DRAW = draw, $
HSIZE = hsize, $
HTHICK = hthick, $
LINESTYLE = linestyle, $
MAP_OBJECT = map_object, $
NOCLIP = noclip, $
NORMAL = normal, $
PSYM=psym, $
SOLID = solid, $
SYMCOLOR = symcolor, $
SYMSIZE = symsize, $
T3D = t3d, $
THICK = thick, $
Z = z, $
_REF_EXTRA=extra
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    SetDefaultValue, draw, 0, /BOOLEAN
    
    ;Set object properties
    IF N_Elements(x0)        NE 0 THEN *self.x0 = x0
    IF N_Elements(x1)        NE 0 THEN *self.x1 = x1
    IF N_Elements(y0)        NE 0 THEN *self.y0 = y0
    IF N_Elements(y1)        NE 0 THEN *self.y1 = y1
    IF N_Elements(clip)      NE 0 THEN *self.clip = clip
    IF N_Elements(color)     NE 0 THEN *self.color = color
    IF N_Elements(data)      NE 0 THEN *self.data = data
    IF N_Elements(device)    NE 0 THEN *self.device = device
    IF N_Elements(hsize)     NE 0 THEN *self.hsize = hsize
    IF N_Elements(hthick)    NE 0 THEN *self.hthick = hthick
    IF N_Elements(linestyle) NE 0 THEN *self.linestyle = linestyle
    IF N_Elements(noclip)    NE 0 THEN *self.noclip = noclip
    IF N_Elements(normal)    NE 0 THEN *self.normal = normal
    IF N_Elements(psym)      NE 0 THEN *self.psym = psym
    IF N_Elements(solid)     NE 0 THEN *self.solid = solid
    IF N_Elements(symcolor)  NE 0 THEN *self.symcolor = symcolor
    IF N_Elements(symsize)   NE 0 THEN *self.symsize = symsize
    IF N_Elements(t3d)       NE 0 THEN *self.t3d = t3d
    IF N_Elements(thick)     NE 0 THEN *self.thick = thick
    IF N_Elements(z)         NE 0 THEN *self.z = z
    
    IF N_Elements(map_obj) NE 0 THEN IF Obj_Valid(map_object) THEN self.map_object = map_object
    
    IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra
    
    ;Draw?
    IF Keyword_Set(draw) THEN self -> Draw

END


;+
; The clean-up routine for the object. Destroy pointers, etc.
;-
PRO weArrow::cleanup
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Free pointers
    Ptr_Free, self.clip
    Ptr_Free, self.color
    Ptr_Free, self.data
    Ptr_Free, self.device
    Ptr_Free, self.hsize
    Ptr_Free, self.hthick
    Ptr_Free, self.linestyle
    Ptr_Free, self.noclip
    Ptr_Free, self.normal
    Ptr_Free, self.psym
    Ptr_Free, self.solid
    Ptr_Free, self.symcolor
    Ptr_Free, self.symsize
    Ptr_Free, self.t3d
    Ptr_Free, self.thick
    Ptr_Free, self.z
    
    ;Destroy the map object
    IF Obj_Valid(self.map_object) THEN Obj_Destroy, self.map_object
    
    self -> MrGrAtom::CleanUp
    
END


;+
; Provides a device-independent and color-model-independent way of drawing an arrow
; in a specified color.
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Params:
;    x0: in, required, type=integer/float
;         The x location of the blunt end of the arrow. May be a vector. Assumes
;         device coordinates.
;    x1: in, required, type=integer/float
;         The x location of the sharp end of the arrow. May be a vector. Assumes
;         device coordinates.
;    y0: in, required, type=integer/float
;         The y location of the blunt end of the arrow. May be a vector. Assumes
;         device coordinates.
;    y1: in, required, type=integer/float
;         The y location of the sharp end of the arrow. May be a vector. Assumes
;         device coordinates.
;       
; :Keywords:
;     color: in, optional, type=string/integer/long, default='white'
;         An alternative way to specify the color to use in erasing the graphics window.
;         Color names are those used with cgColor. This parameter is used in
;         preference to the background_color parameter.
;     data: in, optional, type=boolean, default=0
;          Set this keyword of the arrow locations are in data coordinates.
;          Device coordinates are assumed.
;     hsize: in, optional, type=float
;         The size of the the arrow head. By default 1/64th the width
;         of the device. (!D.X_SIZE / 64.)
;     hthick: in, optional, type=float, default=1.0
;         The thickness of the line drawing the arrowhead. 
;     linestyle: in, optional, type=integer, default=0
;         The line style of the arrow. Line style integers as in PLOT.
;     normal: in, optional, type=boolean, default=0
;          Set this keyword of the arrow locations are in normalized coordinates.
;          Device coordinates are assumed.
;     solid: in, optional, type=boolean, default=0
;          Set this keyword to draw solid, filled arrows.
;     thick: in, optional, type=float, default=1.0
;         The thickness of the line drawing the shaft of the arrow. 
;     _ref_extra: in, optional, type=structure
;         Any keyword accepted by cgPlotS is also permitted for keyword inheritance.
;-
FUNCTION weArrow::init, x0, y0, x1, y1, $
COLOR = color, $
DATA = data, $
DRAW = draw, $
HSIZE = hsize, $
HTHICK = hthick, $
LINESTYLE=linestyle, $
NORMAL = normal, $
SOLID = solid, $
THICK = thick, $
_REF_EXTRA = extra
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
        IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN, 0
    ENDIF
    
    ;Set default values
    SetDefaultValue, color, 'white'
    SetDefaultValue, data, 0, /BOOLEAN
    SetDefaultValue, hthick, 1.0
    SetDefaultValue, linestyle, 0
    SetDefaultValue, normal, 0, /BOOLEAN
    SetDefaultValue, solid, 0, /BOOLEAN
    SetDefaultValue, thick, 1.0
    SetDefaultValue, device, 1, /BOOLEAN
    
    ;Make pointers valid
    self.x0 = Ptr_New(/ALLOCATE_HEAP)
    self.y0 = Ptr_New(/ALLOCATE_HEAP)
    self.x1 = Ptr_New(/ALLOCATE_HEAP)
    self.y1 = Ptr_New(/ALLOCATE_HEAP)
    self.clip = Ptr_New(/ALLOCATE_HEAP)
    self.color = Ptr_New(/ALLOCATE_HEAP)
    self.data = Ptr_New(/ALLOCATE_HEAP)
    self.device = Ptr_New(/ALLOCATE_HEAP)
    self.hsize = Ptr_New(/ALLOCATE_HEAP)
    self.hthick = Ptr_New(/ALLOCATE_HEAP)
    self.linestyle = Ptr_New(/ALLOCATE_HEAP)
    self.map_object = Obj_New()
    self.noclip = Ptr_New(/ALLOCATE_HEAP)
    self.normal = Ptr_New(/ALLOCATE_HEAP)
    self.psym = Ptr_New(/ALLOCATE_HEAP)
    self.solid = Ptr_New(/ALLOCATE_HEAP)
    self.symcolor = Ptr_New(/ALLOCATE_HEAP)
    self.symsize = Ptr_New(/ALLOCATE_HEAP)
    self.t3d = Ptr_New(/ALLOCATE_HEAP)
    self.thick = Ptr_New(/ALLOCATE_HEAP)
    self.z = Ptr_New(/ALLOCATE_HEAP)
     
    ;Set the object properties
    self -> SetProperty, X0 = x0, $
                         Y0 = y0, $
                         X1 = x1, $
                         Y1 = y1, $
                         COLOR = color, $
                         DATA = data, $
                         DEVICE = device, $
                         HSIZE = hsize, $
                         HTHICK = hthick, $
                         LINESTYLE = linestyle, $
                         NORMAL = normal, $
                         SOLID = solid, $
                         THICK = thick, $
                         _STRICT_EXTRA = extra

    Return, 1
END


;+
; The class definition module for the object.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO weArrow__define, class

    class = { weArrow, $
              inherits MrGrAtom, $            ;For dot-referencing in IDL>=8.0
              
              x0: Ptr_New(), $
              y0: Ptr_New(), $
              x1: Ptr_New(), $
              y1: Ptr_New(), $
              clip: Ptr_New(), $
              color: Ptr_New(), $
              data: Ptr_New(), $
              device: Ptr_New(), $
              hsize: Ptr_New(), $
              hthick: Ptr_New(), $
              linestyle: Ptr_New(), $
              map_object: Obj_New(), $
              noclip: Ptr_New(), $
              normal: Ptr_New(), $
              psym: Ptr_New(), $
              solid: Ptr_New(), $
              symcolor: Ptr_New(), $
              symsize: Ptr_New(), $
              t3d: Ptr_New(), $
              thick: Ptr_New(), $
              z: Ptr_New() $
            }
END