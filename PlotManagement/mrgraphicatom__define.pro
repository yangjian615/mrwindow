; docformat = 'rst'
;
; NAME:
;       MrGraphicAtom__Define
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
;   A set of method inherited by all graphic objects.
;
; :Restrictions:
;   Inheriting object must have the following properties::
;       - Position
;           Used In:            IsInside, SetProperty
;           Description:        The typical 4-element position accepted by IDL's PLOT
;                               command. Though this in not an inherent requirement for
;                               certain Direct Graphics procedures, they have been built
;                               into the Mr[Graphic] classes to provide a better means of
;                               determining where they are.
;       - P_sysvar
;           Used In:            ConvertCoord
;           Description:        The !P system variable established when the graphic
;                               object's Draw method is called.
;       - X_sysvar
;           Used In:            ConvertCoord
;           Description:        The !X system variable established when the graphic
;                               object's Draw method is called.
;       - Y_sysvar
;           Used In:            ConvertCoord
;           Description:        The !Y system variable established when the graphic
;                               object's Draw method is called.
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
;       08/23/2013  -   Written by Matthew Argall
;       09/24/2013  -   Added the POSITION parameter to IsInside. - MRA
;       09/25/2013  -   Added the layout-related object properties and the Get/SetProperty
;                           and CalcPositions methods. - MRA
;-
;*****************************************************************************************
;+
;   Calculate a position based on the layout properties.
;-
function MrGraphicAtom::CalcPosition
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, fltarr(4)
    endif
    
    nLay = n_elements(layout)
    
    ;If the [nCols,nRows] <= [0,0] or [col,row] <= [0,0] then return current position.
    ;This means the layout or the location within that layout has not be defined.
    if ((*self.layout)[0] le 0 and (*self.layout)[1]      le 0) || $
       ((*self.layout)[2] le 0 and (*self.layout)[nLay-1] le 0) then return, self.position
    
    ;Make sure the layout is valid.
    if ((*self.layout)[0] le 0) xor ((*self.layout)[1] le 0) $
        then message, 'Cannot calculate position. Layout must have at least ' + $
                      'one column and one row.'
help, /tr
    ;Calculate positions
    position = MrPlotLayout(*self.layout, $
                            ASPECT  = *self.aspect, $
                            XGAP    =  self.xgap, $
                            XMARGIN =  self.xmargin, $
                            YGAP    =  self.ygap, $
                            YMARGIN =  self.ymargin)

    return, position    
end


;+
;   Destroy the object.
;-
pro MrGraphicAtom::destroy    
    obj_destroy, self
end


;+
;   Determine if the coordate made by X and Y lies within the object's position.
;
; :Params:
;       X:              in, required, type=numeric scalar
;                       X coordinate 
;       Y:              in, optional, type=numeric scalar/array
;                       Y coordinate
;       POSITION:       in, optional, type=fltarr(4), default=*self.position
;                       Determine if [`X`,`Y`] is inside this POSITION.
;
; :Keywords:
;       DATA:           in, optional, type=boolean, default=0
;                       If set, X and Y are provided in data coordinates.
;       DEVICE:         in, optional, type=boolean
;                       If set, X and Y are provided in device coordinates. This is assumed.
;       DELTA:          out, optional, type=float
;                       A named variable into which the distance between [`X`, `Y`] and
;                           the center of the object will be returned. DELTA is calculated
;                           as Delta = Sqrt((x0-x1)^2 + (y0-y1)^2).
;       NORMAL:         in, optional, type=boolean, default=0
;                       If set, X and Y are provided in normal coordinates.
;
; :Returns:
;       TF_INSIDE:      Returns true (1) if [x,y] lies within POSITION. False (0) otherwise.
;-
function MrGraphicAtom::IsInside, x, y, position, $
DATA = data, $
DELTA = delta, $
NORMAL = normal
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif

    ;If a position was not given then  use self.position, but only if it is not equal
    ;to [0,0,0,0]. In the latter case, return false.
    if n_elements(position) eq 0 then if (array_equal(self.position, fltarr(4)) eq 0) $
        then position = self.position $
        else return, 0
        
    ;Defaults
    SetDefaultValue, data, 0, /BOOLEAN
    SetDefaultValue, normal, 0, /BOOLEAN
    SetDefaultValue, device, 0, /BOOLEAN
    
    ;Check Inputs
    if data + device + normal eq 0 then device = 1
    if data + device + normal ne 1 then message, 'DATA, DEVICE, and NORMAL are mutually exclusive.'
    
    ;Convert to normal coordinates, if necessary
    if keyword_set(data) or keyword_set(device) $
        then coord = self -> ConvertCoord(x, y, DATA=data, DEVICE=device, /TO_NORMAL) $
        else coord = [x, y]
    
    ;Is the coordinate inside the plot?
    if coord[0] ge position[0] && coord[0] le position[2] && $
       coord[1] gt position[1] && coord[1] le position[3] $
        then tf_inside = 1 $
        else tf_inside = 0
        
    ;Determine how far from the center the coordinate lies
    if arg_present(delta) then begin
        center = [mean(position[[0,2]]), mean(position[[1,3]])]
        delta = sqrt((coord[0] - center[0])^2 + (coord[1] - center[1])^2)
    endif
    
    return, tf_inside
end


;+
;   Convert between data, normal, and device coordinates.
;
; :Params:
;       X:                      in, required, type=numeric scalar/array
;                               X components of the input coordinates. If only one argument
;                                   is specified, then X[0,*] represents the X-coordinates,
;                                   X[1,*] represents the Y-coordinates, and X[2,*]
;                                   represents the Z-coordinates (if present).
;       Y:                      in, optional, type=numeric scalar/array
;                               Y components of the input coordinates.
;       Z:                      in, optional, type=numeric scalar/array
;                               Z components of the input coordinates.
;
; :Keywords:
;       _REF_EXTRA:             in, optional, type=any
;                               Any keyword accepted by IDL's Convert_Coord function is
;                                   also accepted for keyword inheritance.
;-
function MrGraphicAtom::ConvertCoord, x, y, z, $
_REF_EXTRA=extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, -1
    endif
    
    ;Get the current P, X, Y system variables
    P_current = !P
    X_current = !X
    Y_current = !Y
    
    ;Load the syetem variable states as they relate to this plot
    !P = self.p_sysvar
    !X = self.x_sysvar
    !Y = self.y_sysvar
    
    ;Convert coordinates
    case n_params() of
        1: coords = convert_coord(x, _STRICT_EXTRA=extra)
        2: coords = convert_coord(x, y, _STRICT_EXTRA=extra)
        3: coords = convert_coord(x, y, z, _STRICT_EXTRA=extra)
        else: message, 'Incorrect number of parameters.'
    endcase
    
    ;Reset the system variables
    !P = P_current
    !X = X_current
    !Y = Y_current
    
    return, coords
end


;+
;   Set properties of the object.
;
; :Params:
;       ASPECT:         out, optional, type=float
;                       Aspect ratio of the plot.
;       LAYOUT:         out, optional, type=intarr(3)/intarr(4)
;                       [nCols, nRows, col, row]: The layout of the display and the
;                           graphic's location within that layout. OR [nCols,nRows,index]:
;                           Here "index" is the plot index location, where 0 indicates
;                           the upper-left plot and increases first down the right.
;       POSITION:       out, optional, type=intarr(4), default=[1,1,1,1]
;                       The normalized position of the graphic on the display.
;       XMARGIN:        out, optional, type=intarr(2)
;                       Width of the left and right margins in character units.
;       XGAP:           out, optional, type=integer
;                       Horizontal space between plots, in character units.
;       YMARGIN:        out, optional, type=intarr(2)
;                       Height of the top and bottom margins in character units.
;       YGAP:           out, optional, type=integer
;                       Vertical space between plots in character units.
;-
pro MrGraphicAtom::GetProperty, $
ASPECT=aspect, $
LAYOUT=layout, $
POSITION=position, $
XMARGIN=xmargin, $
XGAP=xgap, $
YMARGIN=ymargin, $
YGAP=ygap
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Default to updating the position
    SetDefaultValue, updatePosition, 1, /BOOLEAN
    
    ;Set GraphicAtom properties
    if arg_present(layout)   then layout   = *self.layout
    if arg_present(aspect)   then aspect   = *self.aspect
    if arg_present(position) then position =  self.position
    if arg_present(xmargin)  then xmargin  =  self.xmargin
    if arg_present(xgap)     then xgap     =  self.xgap
    if arg_present(ymargin)  then ymargin  =  self.ymargin
    if arg_present(ygap)     then ygap     =  self.ygap
end


;+
;   Set properties of the object.
;
; :Params:
;       ASPECT:         in, optional, type=float
;                       Aspect ratio of the plot.
;       LAYOUT:         in, optional, type=intarr(3)/intarr(4)
;                       [nCols, nRows, col, row]: The layout of the display and the
;                           graphic's location within that layout. OR [nCols,nRows,index]:
;                           Here "index" is the plot index location, where 0 indicates
;                           the upper-left plot and increases first down the right. If
;                           proveded, then `POSITION` will be calculated, unless
;                           `UPDATEPOSITION`=0.
;       POSITION:       in, optional, type=intarr(4), default=[1,1,1,1]
;                       The normalized position at which to place the graphic on the
;                           display. If provided, then `LAYOUT` will be reset.
;       UPDATE_LAYOUT:  in, optional, type=boolean, default=1
;                       If set, the position will be updated based on the new layout. If
;                           `POSITION` was provided, then `LAYOUT`[2:3]=0 to reflect a
;                           user-defined position.
;       XMARGIN:        in, optional, type=intarr(2)
;                       Width of the left and right margins in character units.
;       XGAP:           in, optional, type=integer
;                       Horizontal space between plots, in character units.
;       YMARGIN:        in, optional, type=intarr(2)
;                       Height of the top and bottom margins in character units.
;       YGAP:           in, optional, type=integer
;                       Vertical space between plots in character units.
;-
pro MrGraphicAtom::SetProperty, $
ASPECT=aspect, $
LAYOUT=layout, $
POSITION=position, $
UPDATE_LAYOUT=update_layout, $
XMARGIN=xmargin, $
XGAP=xgap, $
YMARGIN=ymargin, $
YGAP=ygap
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Default to updating the position
    SetDefaultValue, update_layout, 1, /BOOLEAN
    
    ;Set Properties. Make sure layout elements are always >= 0.
    if n_elements(layout)  ne 0 then *self.layout  = layout
    if n_elements(aspect)  ne 0 then *self.aspect  = aspect
    if n_elements(xmargin) ne 0 then  self.xmargin = xmargin
    if n_elements(xgap)    ne 0 then  self.xgap    = xgap
    if n_elements(ymargin) ne 0 then  self.ymargin = ymargin
    if n_elements(ygap)    ne 0 then  self.ygap    = ygap
    
    ;Update the position to fit the new layout?
    if update_layout eq 1 then begin
        new_position = self -> CalcPosition()
        if array_equal(new_position, fltarr(4)) eq 0 $
            then self.position = new_position
    endif

    ;If a position was given, then reset the layout
    if n_elements(position) ne 0 then begin
        ;Update the layout to reflect a user-defined position has been given. 
        if update_layout eq 1 then *self.layout = [(*self.layout)[0:1],0,0]
        
        ;Update the position.
        self.position = position
    endif
end


;+
;   Clean up after the object is destroy
;-
pro MrGraphicAtom::cleanup
    ptr_free, self.layout
    ptr_free, self.aspect
end


;+
;   The initialization method.
;
; :Params:
;       ASPECT:         in, optional, type=float
;                       Aspect ratio of the plot.
;       LAYOUT:         in, optional, type=intarr(3)/intarr(4), default=[1,1,1,1]
;                       [nCols, nRows, col, row] or [nCols, nRows, index]. [nCols,nRows]
;                           is the number of columns and rows in the display. [col,row]
;                           is the column and row in which to place the plot. "index" is
;                           the plot index at which to place the plot. [col,row] locations
;                           begin at [1,1], "index" 0-based plot index, with 0 being the
;                           upper-left corner, then incrementing first down, then right.
;       POSITION:       in, optional, type=intarr(4), default=[1,1,1,1]
;                       The position at which the plot is located. An array of the form
;                           [x0,y0,x1,y1], where (x0,y0) are the normalized coordinates
;                           of the lower-left corner and (x1,y1) are the coordinates of
;                           the upper-right corner fo the plot.
;       XMARGIN:        in, optional, type=intarr(2), default=[10,3]
;                       Width of the left and right margins in character units.
;       XGAP:           in, optional, type=integer, default=14
;                       Horizontal space between plots, in character units.
;       YMARGIN:        in, optional, type=intarr(2), default=[4,2]
;                       Height of the top and bottom margins in character units.
;       YGAP:           in, optional, type=integer, default=6
;                       Vertical space between plots in character units.
;-
function MrGraphicAtom::init, $
ASPECT=aspect, $
LAYOUT=layout, $
POSITION=position, $
XMARGIN=xmargin, $
XGAP=xgap, $
YMARGIN=ymargin, $
YGAP=ygap
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif

    ;Default Values
    SetDefaultValue, layout, [0,0,0,0]
    SetDefaultValue, xmargin, [10, 3]
    SetDefaultValue, xgap, 14
    SetDefaultValue, ymargin, [4,2]
    SetDefaultValue, ygap, 6
    
    ;Define a default position
    if array_equal(layout, [0,0,0,0]) and n_elements(position) eq 0 then begin
        position = [0.125, 0.125, 0.925, 0.9]
        update_layout = 0
    endif
    
    ;Define pointers
    self.aspect   = ptr_new(/ALLOCATE_HEAP)
    self.layout   = ptr_new(/ALLOCATE_HEAP)

    ;Set the object properties. Because this class is meant to be inherited, call
    ;MrGraphicAtom::SetProperty explicitly to skip the subclass.
    self -> MrGraphicAtom::SetProperty, LAYOUT=layout, $
                                        ASPECT=aspect, $
                                        POSITION=position, $
                                        XMARGIN=xmargin, $
                                        XGAP=xgap, $
                                        YMARGIN=ymargin, $
                                        YGAP=ygap
    
    return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;
; :Fields:
;       ASPECT:         Aspect ratio of the plot.
;       LAYOUT:         [nCols, nRows, col, row] or [nCols, nRows, index]. [nCols,nRows]
;                           is the number of columns and rows in the display. [col,row]
;                           is the column and row in which to place the plot. "index" is
;                           the plot index at which to place the plot. [col,row] locations
;                           begin at [1,1], "index" 0-based plot index, with 0 being the
;                           upper-left corner, then incrementing first down, then right.
;       POSITION:       The position at which the plot is located. An array of the form
;                           [x0,y0,x1,y1], where (x0,y0) are the normalized coordinates
;                           of the lower-left corner and (x1,y1) are the coordinates of
;                           the upper-right corner fo the plot.
;       XMARGIN:        Width of the left and right margins in character units.
;       XGAP:           Horizontal space between plots, in character units.
;       YMARGIN:        Height of the top and bottom margins in character units.
;       YGAP:           Vertical space between plots in character units.
;-
pro MrGraphicAtom__define, class
    compile_opt idl2
    
    define = { MrGraphicAtom, $
               aspect: ptr_new(), $
               layout: ptr_new(), $
               position: fltarr(4), $
               xmargin: [0, 0], $
               xgap: 0, $
               ymargin: [0, 0], $
               ygap: 0 $
             }
end