; docformat = 'rst'
;
; NAME:
;       MrPlotS__Define
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
;   The purpose of this method is to create an object out of the cgPlot routine.
;
; :Examples:
;   Plot an Nx2 array as two line plots on a single axis.
;       x = findgen(101)/100
;       y = sin(2*!pi*x)
;       z = cos(2*!pi*x)
;       a = obj_new('MrPlotS', x, [[y],[z]], DIMENSION=2, TITLE='Sin(x) & Cos(x)', $
;                                   COLOR=['black', 'blue'], XTITLE='Time (s)', $
;                                   YTITLE='Amplitude', /DRAW)
;       obj_destroy, a
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2013
;
; :History:
;   Modification History::
;       05/18/2013  -   Written by Matthew Argall
;       06/28/2013  -   Added the DIMENSION keyword. Removed the AXES and COLORBARS
;                           properties because the former was not being used and the
;                           latter interfered with the COLOR keyword. - MRA
;       07/04/2013  -   Added the COLOR keyword so that each column or row in DIMENSION
;                           can be plotted in a different color. - MRA
;       07/31/2013  -   Added the ConvertCoord method. - MRA
;       08/09/2013  -   Inherit MrIDL_Container - MRA
;       08/10/2013  -   Added the LAYOUT keyword. - MRA
;       08/22/2013  -   Added the NOERASE keyword to Draw. Was forgetting to set the
;                           position property in SetProperty. Fixed. - MRA
;       08/23/2013  -   Added the IsInside method. - MRA
;       08/30/2013  -   Missed the SYMCOLOR keyword. Now included. - MRA
;       09/08/2013  -   Number of default colors now matches the number of dimensions
;                           being plotted. - MRA
;                           PLOT_INDEX keyword in IsAvailable now works. - MRA
;       09/27/2013  -   Use N_Elements instead of N_Params in case `Y` is present but 
;                           undefined. Position and layout properties are now handled
;                           by MrGraphicAtom. Renamed from MrPlotSObject to MrPlotS. - MRA
;       2014/03/12  -   If no target is given use the currently selected graphic. Can
;                           now set/get superclass properties. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to draw the plot in the draw window. The plot is
;   first buffered into the pixmap for smoother opteration. (By allowing motion events
;   to copy from the pixmap instead of redrawing the plot, the image does not flicker.)
;-
pro MrPlotS::Draw, $
CONTINUE = continue, $
NOERASE  = noerase
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Return if we are hiding
    if self.hide then return
    
    ;If data coordinates were chosen, then restore the target's coordinate system.
    if self.data then self.target -> RestoreCoords

    ;Now draw the plot to the pixmap
    self -> doPlotS, NOERASE=noerase, CONTINUE=continue
end


;+
;   The purpose of this method is to do the actual plotting. Basically, having this here
;   merely to saves space in the Draw method.
;-
pro MrPlotS::doPlotS, $
CONTINUE=continue, $
NOERASE=noerase
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    nparams = 0
    if n_elements(*self.xcoords) gt 0 then nparams += 1
    if n_elements(*self.ycoords) gt 0 then nparams += 1
    if n_elements(*self.zcoords) gt 0 then nparams += 1

    ;cgPlotS
    case nparams of
        1: cgPlotS, *self.xcoords, $
                    COLOR      = *self.color, $
                    MAP_OBJECT =  self.map_object, $
                    PSYM       =  self.psym, $
                    SYMCOLOR   = *self.symcolor, $
                    SYMSIZE    =  self.symsize, $
                    CONTINUE   =       continue, $
                    CLIP       = *self.clip, $
                    DATA       =  self.data, $
                    DEVICE     =  self.device, $
                    NORMAL     =  self.normal, $
                    LINESTYLE  =  self.linestyle, $
                    NOCLIP     =  self.noclip, $
                    T3D        =  self.t3d, $
                    THICK      =  self.thick, $
                    Z          =  self.zvalue
                    
        2: cgPlotS, *self.xcoords, *self.ycoords, $
                    COLOR      = *self.color, $
                    MAP_OBJECT =  self.map_object, $
                    PSYM       =  self.psym, $
                    SYMCOLOR   = *self.symcolor, $
                    SYMSIZE    =  self.symsize, $
                    CONTINUE   =       continue, $
                    CLIP       = *self.clip, $
                    DATA       =  self.data, $
                    DEVICE     =  self.device, $
                    NORMAL     =  self.normal, $
                    LINESTYLE  =  self.linestyle, $
                    NOCLIP     =  self.noclip, $
                    T3D        =  self.t3d, $
                    THICK      =  self.thick, $
                    Z          =  self.zvalue
                    
        3: cgPlotS, *self.xcoords, *self.ycoords, *self.zcoords, $
                    COLOR      = *self.color, $
                    MAP_OBJECT =  self.map_object, $
                    PSYM       =  self.psym, $
                    SYMCOLOR   = *self.symcolor, $
                    SYMSIZE    =  self.symsize, $
                    CONTINUE   =       continue, $
                    CLIP       = *self.clip, $
                    DATA       =  self.data, $
                    DEVICE     =  self.device, $
                    NORMAL     =  self.normal, $
                    LINESTYLE  =  self.linestyle, $
                    NOCLIP     =  self.noclip, $
                    T3D        =  self.t3d, $
                    THICK      =  self.thick, $
                    Z          =  self.zvalue
    endcase
end


;+
;   The purpose of this method is to retrieve object properties
;
; :Keywords:
;       XCOORDS:        out, required, type=numeric
;                       A vector or scalar argument providing the X components of the
;                           points to be connected. If only one argument is specified,
;                           X must be an array of either two or three vectors
;                           (i.e., (2,*) or (3,*)). In this special case, X[0,*] are
;                           taken as the X values, X[1,*] are taken as the `Y` values,
;                           and X[2,*] are taken as the `Z` values.
;       YCOORDS:        out, optional, type=numeric
;                       Y coordinate(s) of the points to be connected.
;       ZCOORDS:        out, optional, type=numeric
;                       Z coordinate(s) of the points to be connected.
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
;       LINESYTLE:      out, optiona, type=integer
;                       Line style used to draw lines when `LINE_FILL` is set.
;       MAP_OBJECT:     out, optional, type=object
;                       If you are drawing on a map projection set up with Map_Proj_Init
;                           and using projected meter space, rather than lat/lon space,
;                           then you can use this keyword to provide a cgMap object that
;                           will allow you to convert the `x` and `y` parameters from
;                           longitude and latitude, respectively, to projected meter space
;                           before drawing. X and Y must both be present.
;       NOCLIP:         out, optional, type=boolean
;                       If set, suppresses clipping of the polygons.
;       NORMAL:         out, optional, type=boolean
;                       Set to indicate the polygon vertices are in normalized coordinates.
;       PSYM:           out, optional, type=integer
;                       Any normal IDL PSYM values, plus any value supported by the Coyote
;                           Library routine cgSYMCAT. An integer between 0 and 46. May
;                           also be specified as a symbol names. See `cgSymCat` for details.
;       SYMCOLOR:       out, optional, type=string/integer/vector
;                       If this keyword is a string, the name of the symbol color. By
;                           default, same as COLOR. Otherwise, the keyword is assumed to
;                           be a color index into the current color table. May be a vector
;                           of the same length as X.
;       SYMSIZE:        out, optional, type=float/vector
;                       A scalar or vector of symbol sizes. Default is 1.0. May be a
;                           vector of the same length as X.
;       TARGET:         out, optional, type=object
;                       If coordinates are given in data units, set this to the graphic
;                           object that defines the data space.
;       T3D:            out, optional, type=boolean
;                       If set, the generalized transformation matrix in !P.T will be used.
;       THICK:          out, optional, type=float
;                       Thickness of the lines when `POLY_FILL` is set.
;       ZVALUE:         out, optional, type=float
;                       Provides the Z coordinate if a Z parameter is not present in the
;                           call. This is of use only if `T3D` is set.
;-
pro MrPlotS::GetProperty, $
XCOORDS=xcoords, $
YCOORDS=ycoords, $
ZCOORDS=zcoords, $
TARGET=target, $

;cgPlotS Properties
COLOR=color, $
MAP_OBJECT=map_object, $
PSYM=psym, $
SYMCOLOR=symcolor, $
SYMSIZE=symsize, $

;PlotS Properties
CLIP=clip, $
DATA=data, $
DEVICE=device, $
NORMAL=normal, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
T3D=t3d, $
THICK=thick, $
ZVALUE=zvalue

    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Data Properties
    if arg_present(xcoords) ne 0 then xcoords = *self.xcoords
    if arg_present(ycoords) ne 0 then ycoords = *self.ycoords
    if arg_present(zcoords) ne 0 then zcoords = *self.zcoords

    ;cgPlotS Properties
    if arg_present(color)     ne 0 then color     = *self.color
    if arg_present(psym)      ne 0 then psym      =  self.psym
    if arg_present(symcolor)  ne 0 then symcolor  = *self.symcolor
    if arg_present(symsize)   ne 0 then symsize   =  self.symsize
    
    ;PlotS Properties
    if arg_present(clip)      ne 0 then clip      = *self.clip
    if arg_present(data)      ne 0 then data      =  self.data
    if arg_present(device)    ne 0 then device    =  self.device
    if arg_present(normal)    ne 0 then normal    =  self.normal
    if arg_present(linestyle) ne 0 then linestyle =  self.linestyle
    if arg_present(noclip)    ne 0 then noclip    =  self.noclip
    if arg_present(t3d)       ne 0 then t3d       =  self.t3d
    if arg_present(thick)     ne 0 then thick     =  self.thick
    if arg_present(zvalue)    ne 0 then zvalue    =  self.zvalue
    
    ;Map Object
    if arg_present(map_object) ne 0 then if obj_valid(self.map_object) $
        then map_object = self.map_object $
        else map_object = obj_new()

    IF Arg_Present(target) GT 0 THEN IF Obj_Valid(self.target) GT 0 $
        THEN target = self.target $
        ELSE target = Obj_New()
end


;+
;   The purpose of this method is to set object properties. 
;
; :Keywords:
;       XCOORDS:        in, required, type=numeric
;                       A vector or scalar argument providing the X components of the
;                           points to be connected. If only one argument is specified,
;                           X must be an array of either two or three vectors
;                           (i.e., (2,*) or (3,*)). In this special case, X[0,*] are
;                           taken as the X values, X[1,*] are taken as the `Y` values,
;                           and X[2,*] are taken as the `Z` values.
;       YCOORDS:        in, optional, type=numeric
;                       Y coordinate(s) of the points to be connected.
;       ZCOORDS:        in, optional, type=numeric
;                       Z coordinate(s) of the points to be connected.
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
;       LINESYTLE:      in, optiona, type=integer
;                       Line style used to draw lines when `LINE_FILL` is set.
;       MAP_OBJECT:     in, optional, type=object
;                       If you are drawing on a map projection set up with Map_Proj_Init
;                           and using projected meter space, rather than lat/lon space,
;                           then you can use this keyword to provide a cgMap object that
;                           will allow you to convert the `x` and `y` parameters from
;                           longitude and latitude, respectively, to projected meter space
;                           before drawing. X and Y must both be present.
;       NOCLIP:         in, optional, type=boolean
;                       If set, suppresses clipping of the polygons.
;       NORMAL:         in, optional, type=boolean
;                       Set to indicate the polygon vertices are in normalized coordinates.
;       PSYM:           in, optional, type=integer
;                       Any normal IDL PSYM values, plus any value supported by the Coyote
;                           Library routine cgSYMCAT. An integer between 0 and 46. May
;                           also be specified as a symbol names. See `cgSymCat` for details.
;       SYMCOLOR:       in, optional, type=string/integer/vector
;                       If this keyword is a string, the name of the symbol color. By
;                           default, same as COLOR. Otherwise, the keyword is assumed to
;                           be a color index into the current color table. May be a vector
;                           of the same length as X.
;       SYMSIZE:        in, optional, type=float/vector
;                       A scalar or vector of symbol sizes. Default is 1.0. May be a
;                           vector of the same length as X.
;       TARGET:         in, optional, type=object
;                       If coordinates are given in data units, set this to the graphic
;                           object that defines the data space.
;       T3D:            in, optional, type=boolean
;                       If set, the generalized transformation matrix in !P.T will be used.
;       THICK:          in, optional, type=float
;                       Thickness of the lines when `POLY_FILL` is set.
;       ZVALUE:         in, optional, type=float
;                       Provides the Z coordinate if a Z parameter is not present in the
;                           call. This is of use only if `T3D` is set.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrGrAtom::SetProperty is also accepted
;                           via keyword inheritance.
;-
pro MrPlotS::SetProperty, $
XCOORDS=xcoords, $
YCOORDS=ycoords, $
ZCOORDS=zcoords, $
TARGET=target, $

;cgPlotS Properties
COLOR=color, $
MAP_OBJECT=map_object, $
PSYM=psym, $
SYMCOLOR=symcolor, $
SYMSIZE=symsize, $

;PlotS Properties
CLIP=clip, $
DATA=data, $
DEVICE=device, $
NORMAL=normal, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
T3D=t3d, $
THICK=thick, $
ZVALUE=zvalue, $
_REF_EXTRA=extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Data Properties
    if n_elements(xcoords) ne 0 then *self.xcoords = xcoords
    if n_elements(ycoords) ne 0 then *self.ycoords = ycoords
    if n_elements(zcoords) ne 0 then *self.zcoords = zcoords

    ;cgPlotS Properties
    if n_elements(color)     ne 0 then *self.color = color
    if n_elements(psym)      ne 0 then  self.psym = psym
    if n_elements(symcolor)  ne 0 then *self.symcolor = symcolor
    if n_elements(symsize)   ne 0 then  self.symsize = symsize
    
    ;PlotS Properties
    if n_elements(clip)      ne 0 then *self.clip = clip
    if n_elements(linestyle) ne 0 then  self.linestyle = linestyle
    if n_elements(noclip)    ne 0 then  self.noclip = noclip
    if n_elements(t3d)       ne 0 then  self.t3d = t3d
    if n_elements(thick)     ne 0 then  self.thick = thick
    if n_elements(zvalue)    ne 0 then  self.zvalue = zvalue

    ;Target
    IF N_Elements(target) GT 0 THEN IF Obj_Valid(target) $
        THEN self.target = target $
        ELSE self.target = Obj_New()

    ;Superclass
    IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra

;-----------------------------------------------------
;Data, Device, Normal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;They depend on one another.
    if n_elements(data) gt 0 then begin
        data = keyword_set(data)
        if data then begin
            if obj_valid(target) eq 0 then $
                message, 'TARGET invalid. Cannot use DATA coordinates.'
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
    
    if self.data + self.device + self.normal eq 0 then self.normal = 1B

;-----------------------------------------------------
;Map Object \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if n_elements(map_object) ne 0 then begin
        ;Destroy the current map object
        obj_destroy, self.map_object
        
        ;Set the new object
        if obj_valid(map_object) $
            then self.map_object = map_object $
            else self.map_object = obj_new()
    endif
    
;-----------------------------------------------------
;Finish Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    self.window -> Draw
end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrPlotS::cleanup
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;free all pointers
    ptr_free, self.xcoords
    ptr_free, self.ycoords
    ptr_free, self.zcoords
    ptr_free, self.color
    ptr_free, self.symcolor
    ptr_free, self.clip
    
    ;Destroy Map Objects
    if obj_valid(self.map_object) then obj_destroy, self.map_object
end


;+
;   The purpose of this method is to initialize the object.
;
; :Params:
;       XCOORDS:        in, required, type=numeric
;                       A vector or scalar argument providing the X components of the
;                           points to be connected. If only one argument is specified,
;                           X must be an array of either two or three vectors
;                           (i.e., (2,*) or (3,*)). In this special case, X[0,*] are
;                           taken as the X values, X[1,*] are taken as the `Y` values,
;                           and X[2,*] are taken as the `Z` values.
;       YCOORDS:        in, optional, type=numeric
;                       Y coordinate(s) of the points to be connected.
;       ZCOORDS:        in, optional, type=numeric
;                       Z coordinate(s) of the points to be connected.
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
;       LINESYTLE:      in, optiona, type=integer, default=0
;                       Line style used to draw lines when `LINE_FILL` is set.
;       MAP_OBJECT:     in, optional, type=object
;                       If you are drawing on a map projection set up with Map_Proj_Init
;                           and using projected meter space, rather than lat/lon space,
;                           then you can use this keyword to provide a cgMap object that
;                           will allow you to convert the `x` and `y` parameters from
;                           longitude and latitude, respectively, to projected meter space
;                           before drawing. X and Y must both be present.
;       NOCLIP:         in, optional, type=boolean, default=0
;                       If set, suppresses clipping of the polygons.
;       NORMAL:         in, optional, type=boolean, default=0
;                       Set to indicate the polygon vertices are in normalized coordinates.
;       PSYM:           in, optional, type=integer
;                       Any normal IDL PSYM values, plus any value supported by the Coyote
;                           Library routine cgSYMCAT. An integer between 0 and 46. May
;                           also be specified as a symbol names. See `cgSymCat` for details.
;       SYMCOLOR:       in, optional, type=string/integer/vector, default=COLOR
;                       If this keyword is a string, the name of the symbol color. By
;                           default, same as COLOR. Otherwise, the keyword is assumed to
;                           be a color index into the current color table. May be a vector
;                           of the same length as X.
;       SYMSIZE:        in, optional, type=float/vector, default=1.0
;                       A scalar or vector of symbol sizes. Default is 1.0. May be a
;                           vector of the same length as X.
;       TARGET:         in, optional, type=object
;                       If coordinates are given in data units, set this to the graphic
;                           object that defines the data space. If no target is given,
;                           the graphic will be placed in the current window. If no window
;                           is available, one will be opened.
;       T3D:            in, optional, type=boolean, default=0
;                       If set, the generalized transformation matrix in !P.T will be used.
;       THICK:          in, optional, type=float, default=1.0
;                       Thickness of the lines when `POLY_FILL` is set.
;       ZVALUE:         in, optional, type=float
;                       Provides the Z coordinate if a Z parameter is not present in the
;                           call. This is of use only if `T3D` is set.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrGrAtom::SetProperty is also accepted
;                           via keyword inheritance.
;-
function MrPlotS::init, xcoords, ycoords, zcoords, $
TARGET=target, $

;cgPlotS Properties
COLOR=color, $
MAP_OBJECT=map_object, $
PSYM=psym, $
SYMCOLOR=symcolor, $
SYMSIZE=symsize, $

;PlotS Properties
CLIP=clip, $
DATA=data, $
DEVICE=device, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
NORMAL=normal, $
T3D=t3d, $
THICK=thick, $
ZVALUE=zvalue, $
_REF_EXTRA=extra
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif

;---------------------------------------------------------------------
;Superclass & Window /////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Window is obtained by MrGrAtom
    if self -> MrGrAtom::INIT(TARGET=target, /CURRENT) eq 0 then $
        message, 'Unable to initialize MrGrAtom'
    
    ;Refresh the window?
    self.window -> GetProperty, REFRESH=refreshIn
    if refreshIn then self.window -> Refresh, /DISABLE

;---------------------------------------------------------------------
;Keywords ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    setDefaultValue, symsize, 1.0
    setDefaultValue, thick, 1.0
    setDefaultValue, data, 0, /BOOLEAN
    setDefaultValue, device, 0, /BOOLEAN
    setDefaultValue, normal, 0, /BOOLEAN
    if normal + device eq 0 then data = 1B
    
    ;Allocate heap for the variables
    self.xcoords    = ptr_new(/ALLOCATE_HEAP)
    self.ycoords    = ptr_new(/ALLOCATE_HEAP)
    self.zcoords    = ptr_new(/ALLOCATE_HEAP)
    self.color      = ptr_new(/ALLOCATE_HEAP)
    self.symcolor   = ptr_new(/ALLOCATE_HEAP)
    self.clip       = ptr_new(/ALLOCATE_HEAP)
    
    ;Objects
    self.map_object = obj_new()

;---------------------------------------------------------------------
;Set Object Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Set the object properties
    self -> setProperty, XCOORDS=xcoords, $
                         YCOORDS=ycoords, $
                         ZCOORDS=zcoords, $
                         ;cgPlotS Properties
                         COLOR=color, $
                         MAP_OBJECT=map_object, $
                         PSYM=psym, $
                         SYMCOLOR=symcolor, $
                         SYMSIZE=symsize, $
                         ;PlotS Properties
                         CLIP=clip, $
                         DATA=data, $
                         DEVICE=device, $
                         NORMAL=normal, $
                         LINESTYLE=linestyle, $
                         NOCLIP=noclip, $
                         T3D=t3d, $
                         TARGET=target, $
                         THICK=thick, $
                         ZVALUE=zvalue, $
                         _EXTRA=extra

    ;Draw?
    if n_elements(target) eq 0 $
        then self -> Refresh $
        else if refreshIn then self -> Refresh

    return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrPlotS__define, class
    compile_opt idl2
    
    class = { MrPlotS, $
              inherits MrGrAtom, $

              ;Data Properties
              xcoords: ptr_new(), $         ;x-coordinates
              ycoords: ptr_new(), $         ;y-coordinates
              zcoords: ptr_new(), $         ;z-coordinates

              ;cgPlotS Properties
              color: ptr_new(), $
              map_object: obj_new(), $
              psym: 0B, $
              symcolor: ptr_new(), $
              symsize: 0.0, $
              
              ;PlotS Properties
              clip: ptr_new(), $
              data: 0B, $
              device: 0B, $
              normal: 0B, $
              linestyle: 0B, $
              noclip: 0B, $
              t3d: 0B, $
              target: obj_new(), $
              thick: 0.0, $
              zvalue: 0B $
            }
end