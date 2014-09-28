;+
;   The purpose of this method is to handle keyboard events.
;
; :Params:
;       WINDOW:         in, required, type=int/obj
;                       The object reference or window ID of the window in which the
;                           event occured.
;       ISASCII:        in, required, type=boolean
;                       A scalar byte value that indicates whether the keyboard event
;                           corresponds to an ASCII character. If ISASCII is non-zero, the
;                           `CHARACTER` argument will be set to a byte value corresponding
;                           to the character of the pressed key. If ISASCII is zero, the
;                           `KEYVALUE` argument will be set to a numeric value indicating
;                           the key that was pressed.
;       CHARACTER:      in, required, type=integer
;                       If `ISASCII`=1, then this is the byte value of the pressed
;                           character. Otherwise this argument is zero.
;       KEYVALUE:       in, required, type=integer
;                       If `ISASCII`=1, this argument is set to a value that indicates
;                           the key that was pressed. Otherwise, this argument is set to
;                           zero. Possible values are:
;                                1 - Shift
;                                2 - Control
;                                3 - CapsLock
;                                4 - Alt
;                                5 - Left
;                                6 - Right
;                                7 - Up
;                                8 - Down
;                                9 - Page Up
;                               10 - Page Down
;                               11 - Home
;                               12 - End
;       X:              in, required, type=number
;                       The x-coordinate of the mouse cursor (in device coordinates)
;       Y:              in, required, type=number
;                       The y-coordinate of the mouse cursor (in device coordinates)
;       PRESS:          in, required, type=boolean
;                       If set, indicates that the key was pressed.
;       RELEASE:        in, required, type=boolean
;                       If set, indicates that the key was released.
;       KEYMODS:        in, required, type=int
;                       Value containing a bitwise mask indicating key modifiers::
;                           1 - Shift
;                           2 - Control
;                           4 - Caps Lock
;                           8 - Alt
;
; :Returns:
;       RESULT:         Returns::
;                           1 - Event handling will continue to the function callback
;                           0 - Event handling will stop after MouseWheel completes.
;-
function MrGraphicsEventAdapter::KeyHandler, Window, IsASCII, Character, KeyValue, X, Y, Press, Release, KeyMods
    return, 1
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
;
; :Returns:
;       RESULT:         Returns::
;                           1 - Event handling will continue to the function callback
;                           0 - Event handling will stop after MouseDown completes.
;-
function MrGraphicsEventAdapter::MouseDown, Window, X, Y, Button, KeyMods, Clicks
    return, 1
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
;
; :Returns:
;       RESULT:         Returns::
;                           1 - Event handling will continue to the function callback
;                           0 - Event handling will stop after MouseMotion completes.
;-
function MrGraphicsEventAdapter::MouseMotion, Window, X, Y, KeyMods
    return, 1
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
;
; :Returns:
;       RESULT:         Returns::
;                           1 - Event handling will continue to the function callback
;                           0 - Event handling will stop after MouseUp completes.
;-
function MrGraphicsEventAdapter::MouseUp, Window, X, Y, Button
    return, 1
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
;
; :Returns:
;       RESULT:         Returns::
;                           1 - Event handling will continue to the function callback
;                           0 - Event handling will stop after MouseWheel completes.
;-
function MrGraphicsEventAdapter::MouseWheel, Window, X, Y, Delta, KeyMods
    return, 1
end


;+
;   The purpose of this method is to handle selection change events.
;
; :Params:
;       WINDOW:         in, required, type=int/obj
;                       The object reference or window ID of the window in which the
;                           event occured.
;       GRAPHIC:        in, required, type=object
;                       The graphic to be selected or unselected.
;       MODE:           in, required, type=integer
;                       Mode that was used for the current selection. Possible values are::
;                           0 - Unselect
;                           1 - Select
;                           2 - Toggle selection
;                           3 - Additive
;       WASSELECTED:    in, required, type=boolean
;                       If set, indicates that the graphic was selected prior to the event.
;
; :Returns:
;       RESULT:         Returns::
;                           1 - Event handling will continue to the function callback
;                           0 - Event handling will stop after MouseWheel completes.
;-
function MrGraphicsEventAdapter::SelectChange, Window, Graphic, Mode, WasSelected
    return, 1
end


;+
;   The purpose of this method is to clean up object properties
;   after the object is destroyed.
;-
pro MrGraphicsEventAdapter::cleanup
    ;Nothing to clean up
end


;+
;   Initialization method
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
function MrGraphicsEventAdapter::Init
    ;Nothing to do
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
;       _MRGRAPHICSEVENTADAPTER:    Objects must have at least one property.
;-
pro MrGraphicsEventAdapter__Define, class
    compile_opt strictarr
    
    class = { MrGraphicsEventAdapter, $
              _MrGraphicsEventAdatapter: 0B $
            }
end