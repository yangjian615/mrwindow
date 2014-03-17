

;+
;   The purpose of this method is to handle mouse down events.
;
; :Params:
;       WINDOW:         in, required, type=int/obj
;                       The object reference or window ID of the window in which the
;                           event occured.
;       X:              in, required, type=number
;                       The x-coordinate of the mouse cursor (in device coordinates)
;       Y:              in, required, type=number
;                       The y-coordinate of the mouse cursor (in device coordinates)
;       BUTTON:         in, required, type=int
;                       The value of the clicked button::
;                           1 - Right
;                           2 - Left
;                           4 - Center
;       KEYMODS:        in, required, type=int
;                       Value containing a bitwise mask indicating key modifiers::
;                           1 - Shift
;                           2 - Control
;                           4 - Caps Lock
;                           8 - Alt
;       CLICKS:         in, required, type=int
;                       Number of button clicks that occurred::
;                           1 - Single Click
;                           2 - Double Click
;-
pro MrDraw_Event_Handler::MouseDown, Window, X, Y, Button, KeyMods, Clicks
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Click + Drag has begun.
    self._drag = 1
end


;+
;   The purpose of this method is to handle mouse down events.
;
; :Params:
;       WINDOW:         in, required, type=int/obj
;                       The object reference or window ID of the window in which the
;                           event occured.
;       X:              in, required, type=number
;                       The x-coordinate of the mouse cursor (in device coordinates)
;       Y:              in, required, type=number
;                       The y-coordinate of the mouse cursor (in device coordinates)
;       KEYMODS:        in, required, type=int
;                       Value containing a bitwise mask indicating key modifiers::
;                           1 - Shift
;                           2 - Control
;                           4 - Caps Lock
;                           8 - Alt
;-
pro MrDraw_Event_Handler::MouseMotion, Window, X, Y, KeyMods
    ;Nothing to do
end


;+
;   The purpose of this method is to handle mouse up events.
;
; :Params:
;       WINDOW:         in, required, type=int/obj
;                       The object reference or window ID of the window in which the
;                           event occured.
;       X:              in, required, type=number
;                       The x-coordinate of the mouse cursor (in device coordinates)
;       Y:              in, required, type=number
;                       The y-coordinate of the mouse cursor (in device coordinates)
;       BUTTON:         in, required, type=int
;                       The value of the clicked button::
;                           1 - Right
;                           2 - Left
;                           4 - Center
;-
pro MrDraw_Event_Handler::MouseUp, Window, X, Y, Button
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Click + Drag has ended.
    self.drag = 0
end


;+
;   The purpose of this method is to handle mouse up events.
;
; :Params:
;       WINDOW:         in, required, type=int/obj
;                       The object reference or window ID of the window in which the
;                           event occured.
;       X:              in, required, type=number
;                       The x-coordinate of the mouse cursor (in device coordinates)
;       Y:              in, required, type=number
;                       The y-coordinate of the mouse cursor (in device coordinates)
;       DELTA:          in, required, type=int
;                       Direction and number of movements of the mouse wheel.
;       KEYMODS:        in, required, type=int
;                       Value containing a bitwise mask indicating key modifiers::
;                           1 - Shift
;                           2 - Control
;                           4 - Caps Lock
;                           8 - Alt
;-
pro MrDraw_Event_Handler::MouseWheel, Window, X, Y, Delta, KeyMods
    ;Nothing to do...
end


;+
;   The purpose of this method is to clean up object properties
;   after the object is destroyed.
;-
pro MrDraw_Event_Handler::cleanup
    ;Nothing to clean up
end


;+
;   Initialization method
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
function MrDraw_Event_Handler::Init
    ;Nothing to do
    return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrDraw_Event_Handler__Define, class
    compile_opt strictarr
    
    class = { MrDraw_Event_Handler, $
              drag: 0B $
            }
end