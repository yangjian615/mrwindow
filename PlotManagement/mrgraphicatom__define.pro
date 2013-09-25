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
;           Used In:            IsInside
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
;-
;*****************************************************************************************
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
        return, -1
    endif
    
    ;If no position was provided, return false
    if n_elements(position) eq 0 then if n_elements(*self.position) gt 0 $
        then position = *self.position $
        else return, -1
        
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
;   Clean up after the object is destroy
;-
pro MrGraphicAtom::cleanup
    ;Nothing to clean up
end


;+
;   The initialization method. Because MrGraphicAtom is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractPlot object will result
;   in an error.
;-
function MrGraphicAtom::init
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif

    ;Cause an error
    message, 'This is an abstract class and does not have an INIT method.'
    
    return, 0
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrGraphicAtom__define, class
    compile_opt idl2
    
    define = { MrGraphicAtom, $
               _GraphicAtom: 0 $         ;Not used, but cannot have empty class
             }
end