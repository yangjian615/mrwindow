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
;       2013/11/20  -   Written by Matthew Argall
;       2013/11/22  -   Added the WINDOW property, the _SetWindow, Refresh, and Close
;                           methods, and the BUFFER, CURRENT, and NOGUI keywords. - MRA
;-
;*****************************************************************************************
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
function MrGrAtom::IsInside, x, y, position, $
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
;   Turn refreshing of the graphics window on or off.
;
; :Keywords:
;       DISABLE:            in, optional, type=boolean, default=0
;                           If set, refreshing of the graphics window will be disabled.
;-
pro MrGrAtom::Refresh, $
DISABLE=disable
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Turn refresh on or off.
    if obj_valid(self.window) then self.window -> Refresh, DISABLE=disable
end


;+
;   Set the MrWindow object in which the graphic will be displayed.
;
; :Keywords:
;       BUFFER:             in, optional, type=boolean, default=0
;                           If set, graphics will be directed to a buffer and a window
;                               will not be created.
;       CURRENT:            in, optional, type=boolean, default=0
;                           If set, the graphic will be added to the current window. If
;                               not, a new window will be created.
;       NOGUI:              in, optional, type=boolean, default=0
;                           If set, a MrWindow object will be created to manage all
;                               graphics activities, but the display window will be a
;                               normal IDL direct graphics window.
;       NOWINDOW:           in, optional, type=boolean, default=0
;                           If set, no MrWindow object will be created and `THEWINDOW`
;                               will be an invalid object reference. In this case, graphics
;                               are displayed and handled in the normal Direct Graphics
;                               manner.
;-
pro MrGrAtom::_SetWindow, $
BUFFER=buffer, $
CURRENT=current, $
NOGUI=noGUI
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;No window?
    if keyword_set(noWindow) then begin
        self.window = obj_new()
        return
    endif
    
    ;If the graphic is already in a window, then remove it.
    if obj_valid(self.window) then self.window -> Remove, self, DESTROY=0
    
    ;If we are to put the graphic in the current window, then get the current window.
    if keyword_set(current) then begin
        defsysv, '!MrWindow_Array', EXISTS=exists
        
        ;If the sytem variable exists and is a valid object, use it.
        ;Otherwise, create a new window.
        if exists and n_elements(*!MrWindow_Array) gt 0 $
            then theWindow = (*!MrWindow_Array)[0] $
            else theWindow = obj_new('MrWindow', BUFFER=buffer, NOGUI=noGUI, REFRESH=0)
            
    ;If the graphic is to go in a new window, create the window
    endif else theWindow = obj_new('MrWindow', BUFFER=buffer, NOGUI=noGUI, REFRESH=0)

    self.window = theWindow
end


;+
;   The purpose of this method is to close the window in which the graphic is displayed.
;   Doing so will::
;       - Close the window.
;       - Destroy the MrWindow object reference.
;       - Destroy all objects that were contained in the window.
;       - Destroy the widget, if there was one.
;-
pro MrGrAtom::Close
    obj_destroy, self.window
end


;+
;   Destroy the object.
;-
pro MrGrAtom::Destroy
    obj_destroy, self
end


;+
;   Set properties of the object.
;
; :Params:
;       NEWWINDOW:          in, required, type=object
;                           A valid MrWindow object reference into which the graphic
;                               will be moved.
;
; :Keywords:
;       DESTROY:            in, optional, type=boolean, default=0
;                           Destroy the old MrWindow object once the graphic is removed.
;-
pro MrGrAtom::SwitchWindows, newWindow, $
DESTROY=destroy
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Make sure a valid window was given
    if obj_valid(newWindow) eq 0 || obj_class(newWindow) ne 'MRWINDOW' then $
        message, 'NEWWINDOW must be a valid MrWindow object.'
    
    ;Remove SELF from the current window
    self.window -> Remove, self, DESTROY=0
    
    ;Switch to the new window
    if keyword_set(destroy) then obj_destroy, self.window
    self.window = newWindow
    self.window -> Add, self
end


;+
;   Return the name of the object.
;-
function MrGrAtom::GetName
    return, self.name
end


;+
;   Set properties of the object.
;
; :Params:
;       HIDE:           out, optional, type=boolean
;                       If set, the graphic will not be displayed.
;       NAME:           out, optional, type=string
;                       A string specifying the name of the graphic. It can be used
;                           retrieve the graphic using bracket array notation.
;       WINDOW:         out, optional, type=object
;                       The MrWindow object in which the graphic is displayed.
;-
pro MrGrAtom::GetProperty, $
HIDE=hide, $
NAME=name, $
WINDOW=window
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Set GraphicAtom properties
    if arg_present(hide) then hide = self.hide
    if arg_present(name) then name = self.name
    if arg_present(window) then window = self.window
end


;+
;   Set properties of the object.
;
; :Keywords:
;       HIDE:           in, optional, type=boolean
;                       If set, the graphic will not be displayed.
;       NAME:           in, optional, type=string
;                       A string specifying the name of the graphic. It can be used
;                           retrieve the graphic using bracket array notation.
;-
pro MrGrAtom::SetProperty, $
HIDE=hide, $
NAME=name
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Set object properties
    if n_elements(hide) gt 0 then self.hide = keyword_set(hide)
    if n_elements(name) gt 0 then self.name = name
end


;+
;   Clean up after the object is destroy
;-
pro MrGrAtom::cleanup
    ;Nothing to clean up.
end


;+
;   The initialization method.
;
; :Keywords:
;       BUFFER:         in, optional, type=boolean, default=0
;                       If set, graphics will be directed to a buffer and a window
;                           will not be created.
;       CURRENT:        in, optional, type=boolean, default=0
;                       If set, the graphic will be added to the current window. If
;                           not, a new window will be created.
;       NOGUI:          in, optional, type=boolean, default=0
;                       If set, graphics will be displayed in a normal IDL window.
;       NOWINDOW:       in, optional, type=boolean, default=0
;                       If set, no MrWindow object will be created and `THEWINDOW`
;                           will be an invalid object reference. In this case, graphics
;                           are displayed and handled in the normal Direct Graphics
;                           manner.
;       HIDE:           in, optional, type=boolean, default=0
;                       If set, the graphic will not be displayed.
;       NAME:           in, optional, type=string, default=Obj_Class(self)
;                       A string specifying the name of the graphic. It can be used
;                           retrieve the graphic using bracket array notation.
;-
function MrGrAtom::init, $
BUFFER = buffer, $
CURRENT = current, $
HIDE = hide, $
NAME = name, $
NOGUI = noGUI, $
NOWINDOW = noWindow, $
REFRESH = refresh
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif

    ;Set default values
    if n_elements(hide) eq 0 then hide = 0 else hide = keyword_set(hide)
    if n_elements(name) eq 0 then name = Obj_Class(self)
    if keyword_set(noWindow) then message, 'NOWINDOW keyword is disabled.', /INFORMATIONAL

    ;Create a MrWindow widget in which to display the graphic?
    self -> _SetWindow, BUFFER=buffer, CURRENT=current, NOGUI=noGUI
    if n_elements(refresh) gt 0 then self.window -> Refresh, DISABLE=~keyword_set(refresh)
    
    ;Add SELF to the window
    self.window -> Add, self

    ;Set properties
    self.hide = hide
    self.name = name
    
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
;       HIDE:           Do not display the graphic?
;       NAME:           Name of the graphic.
;       WINDOW:         MrWindow object in which to display the graphic.
;-
pro MrGrAtom__define, class
    compile_opt strictarr
    
    define = { MrGrAtom, $
               inherits IDL_Object, $
               inherits MrDataCoords, $
               hide: 0B, $
               name: '', $
               window: obj_new() $
             }
end