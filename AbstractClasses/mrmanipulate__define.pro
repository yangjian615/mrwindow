; docformat = 'rst'
;
; NAME:
;       MrManipulateGraphic__Define
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
;+
;   The purpose of this method is to serve as an abstract class for translating, rotating,
;   and stretching a graphic object interactively within the display window.
;
;   SETUP::
;       Subclass must have the following properties::
;           position:           The position of the object within the display window.
;
;       Subclass must have the following methods::
;           ConvertCoords:      A method for converting between Data, Device, and Normal
;                               coordinates.
;
;       Set the draw widget UValue and Event Procedure::
;           Widget_Control, wDrawID, SET_UVALUE = {object: self, method: 'Draw_Events'}, $
;                                    SET_EVENT_PRO = 'your_event_handling_procedure_goes_here'
;
;       In the procedure specified by EVENT_PRO, use the Call_Method procedure::
;           Widget_Control, wDrawID, GET_UVALUE=event_handler
;           Call_Method, event_handler.method, event_handler.object, event
;
;   MANIPULATING::
;       - Button events must be turned on for the draw widget.
;       - LMODE and/or RMODE must be set to a valid manipulation bit (see below).
;       - Motion events are turned on and off when needed.
;           * Use On_Off_Button_Events and On_Off_Motion_Events to turn on and off
;             button and motion events, respectively. See those methods rules governing
;             'ON' and 'OFF' and the 'Draw_Events' method for the sequences of events.
;
;   ZOOM MENU::
;       1) Create a top level base containing a menu bar and a draw widget.
;       2) Pass the menu bar's widget ID to the Create_Manipulate_Menu.
;       3) Event handling for the menu is done internally by the Manipulate_Menu_Events method.
;       4) Draw widget event handling must be configured as described above.
;
;   Zoom Options::
;       Translate   -   Move a graphic around on the screen.
;       Rotate      -   Rotate a graphic about the center.
;       Stretch     -   Change the size of the graphic.
;
;   Zoom Bits::
;       0       -   No manipulation effects
;       1       -   Translate
;       2       -   Stretch
;       4       -   Rotate
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
;	Modification History::
;       08/13/2013  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Determine which "Manipulate Menu" button was pressed and tell the draw widget which
;   type of events to pay attention to.
;
; :Params:
;       POSITION:       in, optional, type=2xN numeric
;                       The [x,y]-coordinates of the points around which 4x4 pixel boxes
;                           are to be drawn.
;
; :Keywords:
;       DATA:           in, optional, tyep=boolean
;                       Indicate that `POSITION` are provided in data coordinates.
;       DEVICE:         in, optional, tyep=boolean
;                       Indicate that `POSITION` are provided in device coordinates. This
;                           is the default.
;       NORMAL:         in, optional, tyep=boolean
;                       Indicate that `POSITION` are provided in normal coordinates.
;-
function MrManipulate::ClickedInside, x, y, position, $
DATA = data, $
DEVICE = device, $
NORMAL = normal
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    if n_elements(position) eq 0 then begin
        pos = *self.boxes
        
    endif else begin
        ;Default Values
        data = keyword_set(data)
        device = keyword_set(device)
        normal = keyword_set(normal)
    
        ;Make sure only one is set.
        if data + device + normal eq 0 then device = 1
        if data + device + normal ne 1 then $
            message, 'DATA, DEVICE, and NORMAL are mutually exclusive.'
            
        if device eq 0 then begin
            pos = self -> ConvertCoords(position[[0,2]], position[[1,3]], $
                                        DATA=data, NORMAL=normal, $
                                        /TO_DEVICE)
            pos = [pos[0,0], pos[1,0], pos[1,0], pos[1,1]]
        endif else begin
            pos = position
        endelse
    endelse
        
    
    ;Loop through all positions
    nPos = n_elements(pos[0,*])
    for i = 0, nPos - 1 do begin
        ;is [x,y] fall within the position provided?
        if x ge pos[0,i] and x le pos[2,i] and $
           y gt pos[1,i] and y le pos[3,i] $
            then tf_clicked[i] = 1 $
            else tf_clicked[i] = 0
    endfor
    
    ;Return a scalar if a scalar was given.
    if nPos eq 1 $
        then return, tf_clicked[0] $
        else return, tf_clicked
end


;+
;   Determine which "Manipulate Menu" button was pressed and tell the draw widget which
;   type of events to pay attention to.
;
; :Params:
;       POINTS:         in, optional, type=2xN numeric
;                       The [x,y]-coordinates of the points around which 4x4 pixel boxes
;                           are to be drawn. Must be provided if `ROTATE`, `STRETCH`, and
;                           `TRANSLATE` are not set.
;
; :Keywords:
;       ROTATE:         in, optional, type=boolean
;                       If set, points for rotation will be selected.
;       STRETCH:        in, optional, type=boolean
;                       If set, points for stretching will be selected.
;       TRANSLATE:      in, optional, type=boolean
;                       If set, points for translation will be selected.
;
;       DATA:           in, optional, tyep=boolean
;                       Indicate that `POINTS` are provided in data coordinates.
;       DEVICE:         in, optional, tyep=boolean
;                       Indicate that `POINTS` are provided in device coordinates. This
;                           is assumed.
;       NORMAL:         in, optional, tyep=boolean
;                       Indicate that `POINTS` are provided in normal coordinates.
;-
pro MrManipulate::DrawBoxAroundPoints, points, $
TRANSLATE = translate, $
ROTATE = rotate, $
STRETCH = stretch, $

DATA = data, $
DEVICE = device, $
NORMAL = normal
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;If the points were given directly
    if n_elements(points) eq 0 then begin
        ;Default Values
        data = keyword_set(data)
        device = keyword_set(device)
        normal = keyword_set(normal)
    
        ;Make sure only one is set.
        if data + device + normal eq 0 then device = 1
        if data + device + normal ne 1 then $
            message, 'DATA, DEVICE, and NORMAL are mutually exclusive.'
            
        if device eq 0 $
            then pts = self -> ConvertCoords(points, DATA=data, NORMAL=normal, /TO_DEVICE) $
            else pts = points
    
    ;Collect points
    endif else begin
    
        ;Get the points around which to draw boxes
        pts = self -> GetPointsToBox(TRANSLATE=translate, $
                                     ROTATE=rotate, $
                                     STRETCH=stretch)
    endelse
    
    ;Draw 4x4 pixel boxes centered on each coordinate in PTS
    npts = n_elements(pts[0,*])
    boxes = intarr(2,4,npts)
    for i = 0, npts - 1 do begin
        ;Outline the boxes
        boxes[*,*,i] = [[pts[0,i]-2, pts[1,i]-2], $
                        [pts[0,i]+2, pts[1,i]-2], $
                        [pts[0,i]+2, pts[1,i]+2], $
                        [pts[0,i]-2, pts[1,i]+2]]
        
        ;Draw the boxes
        plots, pts[0,*,i], pts[1,*,i], /DEVICE, COLOR=load_color('blue')
    endfor
    
    ;Store the boxes
    if ptr_valid(self.boxes) $
        then *self.boxes = boxes $
        else self.boxes = ptr_new(boxes)
end


;+
;   Create a menu bar with various zoom options in it.
;
; :Params:
;       PARENT:             in, required, type=integer
;                           The widget ID of the parent widget for the new SaveAs Menu.
;-
pro MrManipulate::Create_Manipulate_Menu, parent, $
MENU = menu, $
ROTATE = rotate, $
STRETCH = stretch, $
TRANSLATE = translate
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Defaults
    rotate = keyword_set(rotate)
    stretch = keyword_set(stretch)
    translate = keyword_set(translate)

    if rotate + translate + stretch eq 0 then begin
        menu = 1
        rotate = 1
        stretch = 1
        translate = 1
    endif

    ;Create the Menu
    if keyword_set(menu) $
        then manipID = widget_button(parent, VALUE='Manipulate', /MENU) $
        else manipID = parent

    ;Make the zoom menu in the menu bar
    button = widget_button(manipID, VALUE='Translate', UNAME='TRANSLATE', /CHECKED_MENU, UVALUE={object: self, method: 'Manipulate_Menu_Events'})
    button = widget_button(manipID, VALUE='Stretch', UNAME='STRETCH', /CHECKED_MENU, UVALUE={object: self, method: 'Manipulate_Menu_Events'})
    button = widget_button(manipID, VALUE='Rotate', UNAME='ROTATE', /CHECKED_MENU, UVALUE={object: self, method: 'Manipulate_Menu_Events'})
    
    button = widget_button(manipID, VALUE='None', UNAME='MNone', UVALUE={object: self, method: 'Manipulate_Menu_Events'})
end


;+
;   Handle events from the draw widget.
;
;   NOTES on copyPixmap::
;       Box Zoom    -   copyPixmap at beginning of every motion event
;
;   NOTES of Draw::
;       Box Zoom    -   Draw event after the button is released (see ::Box_Zoom)
;       Pan         -   Draw at end of every motion event
;
;   NOTES on Zoom Event Sequence::
;       Box Zoom::
;           1) Button down  -- Record new min ranges
;           2) Motion       -- copyPixmap, Draw box
;           3) Button Up    -- Record new max ranges, set new range, DRAW
;       XY Zoom::
;           1) Button down  -- Record new min range
;           2) Button down  -- Record new max range, set new ranges, DRAW
;       Pan::
;           1) Button down  -- Record "pan" point
;           2) Motion       -- Pan, Set new "pan" point, Draw
;           3) Button up    -- Reset configuration
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrManipulate::Draw_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
;---------------------------------------------------------------------
;Button Press Events /////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 0 then begin
    
        ;Process the event
        case self.mMode[0] of
            1: self -> Translate, event
            2: self -> Stretch, event
            3: self -> Rotate, event
            else: ;Do nothing
        endcase
        
    endif
    
;---------------------------------------------------------------------
;Button Release Events ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 1 then begin
        case self.mMode[0] of
            1: self -> Translate, event
            2: self -> Stretch, event
            3: self -> Rotate, event
            else: ;Do nothing
        endcase
    endif

;---------------------------------------------------------------------
;Motion Events ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 2 then begin
        case self.mMode[0] of
            1: self -> Translate, event
            2: self -> Stretch, event
            3: self -> Rotate, event
            else: ;Do nothing
        endcase
        
        ;Re-Draw everything after it has been manipulated
        self -> Draw
    endif
end


;+
;   Serve as a general error handler for event handling routines.
;-
pro MrManipulate::Error_Handler, tlb
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    self -> Turn_Everything_Off, tlb
end


;+
;   Determine which "Manipulate Menu" button was pressed and tell the draw widget which
;   type of events to pay attention to.
;
; :Params:
;       POINTS:         in, required, type=2xN numeric
;                       The [x,y]-coordinates of the points around which 4x4 pixel boxes
;                           are to be drawn.
;
; :Keywords:
;       ROTATE:         in, optional, type=boolean
;                       If set, points for rotation will be selected.
;       STRETCH:        in, optional, type=boolean
;                       If set, points for stretching will be selected.
;       TRANSLATE:      in, optional, type=boolean
;                       If set, points for translation will be selected.
;-
function MrManipulate::GetPointsToBox, $
TRANSLATE = translate, $
ROTATE = rotate, $
STRETCH = stretch
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, !Null
    endif

;---------------------------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;---------------------------------------------------------------------
    
    ;Defaults
    translate = keyword_set(translate)
    rotate = keyword_set(rotate)
    stretch = keyword_set(stretch)
    
    ;Check Inputs
    if translate + rotate + stretch ne 1 then $
        message, 'One and only one of {ROTATE | STRETCH | TRANSLATE} must be set.'
    
    ;Return if TRANSLATE is set
    if translate then return, !Null

;---------------------------------------------------------------------
;Convert Position to Device Coordinates \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;---------------------------------------------------------------------
    
    ;Get the position and coordinate system
    self -> GetProperty, POSITION=position, DATA=data, DEVICE=device, NORMAL=normal
    
    ;The default for Direct Graphics is DATA
    data = keyword_set(data)
    device = keyword_set(device)
    normal = keyword_set(normal)
    if data + device + normal eq 0 then data = 1
    
    ;Convert the position to device coordinates
    if device eq 0 then begin
        pos = self -> ConvertCoords(position[[0,2]], position[[1,3]], $
                                    DATA=data, NORMAL=normal, $
                                    /TO_DEVICE)
        pos = [pos[0,0], pos[1,0], pos[1,0], pos[1,1]]
    endif else begin
        pos = position
    endelse

;---------------------------------------------------------------------
;Rotate \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;---------------------------------------------------------------------
    if rotate eq 1 then begin
        
        x = mean(pos[[0,2]])
        y = mean(pos[[1,3]])
        pointsToBox = fix([x, y])

;---------------------------------------------------------------------
;Stretch \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;---------------------------------------------------------------------
    endif else if stretch eq 1 then begin
        
        pointsToBox = [[pos[0], pos[0]], $
                       [pos[1], pos[0]], $
                       [pos[0], pos[1]], $
                       [pos[1], pos[1]]]
                   

;---------------------------------------------------------------------
;Translate \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;---------------------------------------------------------------------
    endif else if translate eq 1 then begin
        ;Do nothing
    endif
    
    return, pointsToBox
end


;+
;   Determine which "Manipulate Menu" button was pressed and tell the draw widget which
;   type of events to pay attention to.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrManipulate::Manipulate_Menu_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Reset the initial click
    self.x0 = -1
    self.y0 = -1
    
    ;Get the value of the button that cause the event
    widget_control, event.id, GET_VALUE=manip_type
    manip_type = strupcase(manip_type)
    
    ;Determine if the button is set or not, then toggle it
    isSet = widget_info(event.id, /BUTTON_SET)
    
    ;Get the button's siblings and uncheck them all
    parent = widget_info(event.id, /PARENT)
    kids = widget_info(parent, /ALL_CHILDREN)
    for i = 0, n_elements(kids) - 1 do widget_control, kids[i], SET_BUTTON=0
    
    ;Set the zoom mode, set the event handling method, and indicate which type
    ;of events to listen for.
    case manip_type of
        'NONE': self -> Turn_Everything_Off, event.tlb
        
        'TRANSLATE': self.mMode = [1,0]
        'STRETCH': begin
            self.mMode = [2,0]
            self -> DrawBoxAroundPoints, /STRETCH
        endcase
        
        'ROTATE': begin
            self.mMode = [4,0]
            self -> Box_Lever
        endcase
        else: message, 'Button "' + manip_type + '" unknown.'
    endcase
    
    ;Check the button in the menu
    widget_control, event.id, /SET_BUTTON
    
    ;Turn on button events.
    if manip_type ne 'NONE' then self -> On_Off_Button_Events, /ON
end



;+
;   Turn DRAW_BUTTON_EVENTS on or off.
;
; :Keywords:
;       ON:                     in, required, type=boolean, default=0
;                               Turn motion events on.
;       OFF:                    in, optional, type=boolean, default=0
;                               Turn motion events off.
;-
pro MrManipulate::On_Off_Button_Events, $
ON = on, $
OFF = off
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Make sure ON or OFF is indicated
    on = keyword_set(on)
    off = keyword_set(off)
    if on + off ne 1 then message, 'Either ON or OFF must be set (but not both).'
    
    ;Check to see if motion events are on
    isOn = widget_info(self.drawID, /DRAW_BUTTON_EVENTS)
    
    ;Turn on?
    if keyword_set(on) then begin
        if isOn eq 0 then widget_control, self.drawID, DRAW_BUTTON_EVENTS=1
        
    ;Turn off?
    endif else if keyword_set(off) then begin
        if isOn eq 0 then return
        
        ;Turn button events off only if no modes are on/active
        if self.mMode[0] eq 0 then widget_control, self.drawID, DRAW_BUTTON_EVENTS=0
    endif
end


;+
;   Turn DRAW_MOTION_EVENTS on or off. When motion events are turned off, all events
;   are cleared.
;
; :Keywords:
;       ON:                     in, required, type=boolean, default=0
;                               Turn motion events on.
;       OFF:                    in, optional, type=boolean, default=0
;                               Turn motion events off.
;-
pro MrManipulate::On_Off_Motion_Events, $
ON = on, $
OFF = off
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Make sure ON or OFF is indicated
    on = keyword_set(on)
    off = keyword_set(off)
    if on + off ne 1 then message, 'Either ON or OFF must be set (but not both).'
    
    ;Check to see if motion events are on
    isOn = widget_info(self.drawID, /DRAW_MOTION_EVENTS)
    
    ;Turn on?
    if keyword_set(on) then begin
        if isOn eq 0 then widget_control, self.drawID, DRAW_MOTION_EVENTS=1
    
    ;Turn off?
    endif else if keyword_set(off) then begin
        if isOn eq 0 then return
        
        ;Turn motion events off only if no modes are on/active
        if self.mMode[0] eq 0 then begin
            widget_control, self.drawID, DRAW_MOTION_EVENTS=0
            widget_control, self.drawID, /CLEAR_EVENTS
        endif
    endif
end


;+
;   Translate the object around on the display.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrManipulate::Translate, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = cgErrorMsg()
        return
    endif

    ;Return if we are not trying to Pan
    case event.type of
        0: if event.press eq 1 and self.mMode[0] eq 0 then return   ;left button down
        1: if self.mMode[0] eq 0 then return                        ;button up
        2: if self.mMode[1] eq 0 then return                        ;motion
        else: return
    endcase

    case event.type of
        0: begin    ;button down
            ;Return if not clicked
            theObj = self -> Get(self.iFocus)
            isInside = theObj -> IsInside(event.x, event.y)
            if isInside eq 0 then return
        
            ;Make modition events active
            self.mMode[1] = 1
        
            ;Store the clicked coordinates
            self.x0 = event.x
            self.y0 = event.y
        
            ;Turn on motion events
            self -> On_Off_Motion_Events, /ON
        endcase
        
        2: begin    ;motion event    
            ;How much did the mouse move?
            delta_x = event.x - self.x0
            delta_y = event.y - self.y0
            
            ;Get the object being translated and some of its properties
            theObj = self -> Get(self.iFocus)
            theObj -> GetProperty, XLOC=xloc, YLOC=yloc, DEVICE=device, DATA=data, NORMAL=normal
            
            ;Convert the clicked points into the coordinate system of the object.
            xy = theObj -> ConvertCoords([event.x, self.x0], [event.y, self.y0], /DEVICE, $
                                         TO_DATA=data, TO_DEVICE=device, TO_NORMAL=normal)
            
            ;Amount to shift
            delta_x = xy[1,0] - xy[0,0]
            delta_y = xy[1,1] - xy[0,1]
            
            ;Update the position
            theObj -> SetProperty, XLOC=xloc + delta_x, YLOC=yloc + delta_y
                        
            ;Draw the plot in the new position
            self -> Draw
        endcase
        
        1: begin    ;button up
            ;Turn off Translate
            self.mMode = [0,0]
        
            ;Turn motion events off right away (requires self.mMode = 0)
            self -> On_Off_Motion_Events, /OFF
            self.x0 = -1
            self.y0 = -1
            
            ;Free the boxes pointer
            ptr_free, self.boxes
        endcase
    endcase
end


;+
;   A method for turning off all Manipulation options and effects.
;-
pro MrManipulate::Turn_Everything_Off, tlb
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Reset event processing
    self.x0 = -1
    self.y0 = -1
    self.mMode = [0,0]
    self -> On_Off_Button_Events, /OFF
    self -> On_Off_Motion_Events, /OFF
    
    ;Draw no more boxes
    ptr_free, self.boxes
    
    ;Uncheck all menu buttons
    if n_elements(tlb) gt 0 && widget_info(tlb, /VALID_ID) then begin
        ;Get the widget IDs of the menu buttons
        transID = widget_info(tlb, FIND_BY_UNAME='TRANSLATE')
        rotID = widget_info(tlb, FIND_BY_UNAME='ROTATE')
        stchID = widget_info(tlb, FIND_BY_UNAME='STRETCH')
        
        ;Uncheck the buttons
        if widget_info(transID, /VALID_ID) then widget_control, transID, SET_BUTTON=0
        if widget_info(rotID, /VALID_ID) then widget_control, rotID, SET_BUTTON=0
        if widget_info(stchID, /VALID_ID) then widget_control, stchID, SET_BUTTON=0
    endif
end


;+
;   Clean up after the object is destroy
;-
pro MrManipulate::cleanup
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Free pointers
    ptr_free, boxes
end


;+
;   The initialization method. Because MrManipulate is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrManipulate object will result
;   in an error.
;-
function MrManipulate::init
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    message, 'This is an abstract class and must be inherited.'
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrManipulate__define, class
    compile_opt idl2
    
    class = {MrManipulate, $
             mMode: [0,0], $        ;[mode, active?].
             boxes: ptr_new() $     ;Clickable boxes to manipulate.
            }
end