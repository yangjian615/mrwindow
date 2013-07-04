; docformat = 'rst'
;
; NAME:
;       MrAbstractArrow__Define
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
;   The purpose of this method is to serve as an abstract class for making, adding, 
;   removing, and drawing weArrow objects. It is meant to be inherited and will
;   not work on its own.
;
;   SETUP::
;       Subclass must have the following properties::
;           drawID:             Widget ID of the draw window
;           pixID:              Window ID of the pixmap window
;           winID:              Window ID of the display window
;
;       Subclass must have the following methods::
;           copyPixmap:         Method for copying the pixmap (pixID) to the display (winID)
;
;       Set the draw widget UValue and Event Procedure::
;           UVALUE = {object: self, method: 'Draw_Events'}
;           EVENT_PRO = 'your_event_handling_procedure_goes_here'
;
;       In the procedure specified by EVENT_PRO, use the Call_Method procedure::
;           Widget_Control, self.drawID, GET_UVALUE=event_handler
;           Call_Method, event_handler.method, event_handler.object, event
;
;   ARROW EVENTS::
;       - Button events must be turned on for the draw widget.
;       - arrowmode[0] must be set to the proper bit (see below).
;       - Button and motion events are turned on and off as needed:
;           * Use On_Off_Button_Events and On_Off_Motion_Events to turn on and off
;             button and motion events, respectively. See those methods rules governing
;             'ON' and 'OFF' and the 'Draw_Events' method for the sequences of events.
;
;   ARROW MENU::
;       1) Create a top level base containing a menu bar and a draw widget.
;       2) Pass the menu bar's widget ID to the Create_Arrow_Menu.
;       3) Event handling for the menu is done internally by the Arrow_Menu_Events method.
;       4) Draw widget event handling must be configured as described above.
;
;   ABOUT ArrowMode::
;       Arrow mode is an integer array with three values::
;           arrowmode[0]:        The bit corresponding to which mode is set
;           arrowmode[1]:        Indicates that motion events are in use by arrowmode[0]
;           arrowmode[2]:        The index of the arrow object currently being manipulated.
;
;   MODES::
;       0:                  -   None
;       1:      Place_Arrow -   Place arrow by clicking on the draw window and dragging the mouse.
;       2:      Move_Arrow  -   Move an Arrow around the draw window.
;       4:      Adjust_Arrow-   Stretch and Rotate an Arrow
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
;       05/28/2013  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Add, remove, place, or clear weArrow objects.
;
; :Params:
;       ARROW_OBJECT:           in, optional, type=object/obj_arr
;                               The weArrow object(s) to add to the plot.
;
;   :Keywords:
;       CLEAR:                  in, optional, type=boolean, default=0
;                               If set, clear all arrow objects before adding new ones.
;                                   Otherwise, new ones are appended to the end.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the arrows to the list.
;       INDEX:                  in, optional, type=int, default=0
;                               The index at which to `REPLACE` or `REMOVE` arrow objects.
;       REMOVE:                 in, optional, type=boolean, default=0
;                               If set, remove the arrow object and `INDEX` from the list.
;       REPLACE:                in, optional, type=boolean, default=0
;                               If set, replace the object at `INDEX` instead of appending
;                                   to the end of the list.
;-
pro MrAbstractArrow::AddArrows, arrow_object, $
CLEAR = clear, $
DRAW = draw, $
INDEX = index, $
REMOVE = remove, $
REPLACE = replace
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

;---------------------------------------------------------------------
;Remove an Arrow Object //////////////////////////////////////////////
;---------------------------------------------------------------------

    if keyword_set(remove) then begin
        if n_elements(index) eq 0 then index = 0
        n_arrows = n_elements(*self.arrows)
        
        case n_arrows of
            0: return
            1: begin
                ;destroy the object and reset the pointer
                obj_destroy, (*self.arrows)[index]
                ptr_free, self.arrows
                self.arrows = ptr_new(/ALLOCATE_HEAP)
            endcase
            
            else: begin
                ;destroy the object, shift the invalid object to the end, truncate
                obj_destroy, (*self.arrows)[index]
                *self.arrows = shift(*self.arrows, -index-1)
                *self.arrows = (*self.arrows)[0:nArrows-1]
                *self.arrows = shift(*self.arrows, index)
            endelse
        endcase
        
        ;Draw?
        if keyword_set(draw) then self -> Draw
        
        return
    endif

;---------------------------------------------------------------------
;Replace Arrow Object ////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if keyword_set(replace) then begin
        if n_elements(index) eq 0 then index = 0
        obj_destroy, (*self.arrows)[index]
        (*self.arrows)[index] = arrow_object
        
        ;Draw?
        if keyword_set(draw) then self -> Draw
        
        return
    endif

;---------------------------------------------------------------------
;Clear All Arrow Objects /////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Clear all overplots first?
    if keyword_set(clear) then begin
        if ptr_valid(self.arrows) then begin
            for i = 0, n_elements(self.arrows) - 1 do obj_destroy, (*self.arrows)[i]
        endif
        ptr_free, self.arrows
    endif

;---------------------------------------------------------------------
;Add an Arrow Object /////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If nothing was given to add, then return
    if n_elements(arrow_object) eq 0 then begin
        if ptr_valid(self.arrows) eq 0 then self.arrows = ptr_new(/ALLOCATE_HEAP)
        return
    endif
    
    ;Add the object
    if ptr_valid(self.arrows) $
        then *self.arrows = [*self.arrows, arrow_object] $
        else self.arrows = ptr_new(arrow_object)
    
    ;Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   Event hanlder for moving, rotating, and stretching arrows on the display.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractArrow::Adjust_Arrow, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = error_message()
        return
    endif

    ;Make sure we want to move the text.
    case event.type of
        0: if self.arrowmode[0] ne 2 and self.arrowmode[0] ne 4 then return
        1: if self.arrowmode[0] ne 2 and self.arrowmode[0] ne 4 then return
        2: if self.arrowmode[1] eq 0 || (self.arrowmode[0] ne 2 and self.arrowmode[0] ne 4) then return
        else: return
    endcase
    
    ;Which stage of the movement are we in?
    case event.type of
        ;Button Down
        0: begin
            ;Get the positions, widths, and charsizes of all the text annotations
            nArrows = n_elements(*self.arrows)
            arrowPos = intarr(4, nArrows)
        
            ;Create a box around each text approximately where it can be found.
            arrowPos = self -> Arrow_Positions(/BOX)
            
            ;Find the text that is closest
            thisArrow = where(arrowPos[0,*] le event.x and arrowPos[2,*] ge event.x and $
                             arrowPos[1,*] le event.y and arrowPos[3,*] ge event.y, count)

            ;If no text exists at that location, then return
            if count eq 0 then return
            
            ;Store the location of the text and turn on motion events
            self.arrowmode[2] = thisArrow[0]
            self -> On_Off_Motion_Events, /ON
            self.arrowmode[1] = 1
        endcase
        
        ;Motion
        2: begin
            ;1. Must Copy Pixmap to get rid of ghost arrow
            ;       --> done in Draw_Events
            ;2. Draw the arrow again.
            ;Move?
            if self.arrowmode[0] eq 2 then begin
                ;
                ;The cursor position is the midpoint of the arrow's shaft.
                ;
                
                ;Get the arrow
                theArrow = (*self.arrows)[self.arrowmode[2]]
                theArrow -> GetProperty, X0=x0, Y0=y0, X1=x1, Y1=y1
                
                ;Arrow length.
                delta_xa = x1 - x0
                delta_ya = y1 - y0
                
                ;Location of head and tail
                x0_new = event.x - delta_xa/2
                y0_new = event.y - delta_ya/2
                x1_new = event.x + delta_xa/2
                y1_new = event.y + delta_ya/2
                
                ;Update the position
                theArrow -> SetProperty, X0=x0_new, Y0=y0_new, X1=x1_new, Y1=y1_new, /DRAW
            
            ;Stretch/Rotate?
            endif else if self.arrowmode[0] eq 4 then begin

                ;The cursor position is the location of the arrowhead
                (*self.arrows)[self.arrowmode[2]] -> SetProperty, X1=event.x, Y1=event.y, /DRAW
            endif
        endcase
        
        ;Button Up
        1: begin
            ;Turn off motion events
            if self.arrowmode[0] eq 4 $
                then self.arrowmode = [4, 0, -1] $
                else self.arrowmode = [2, 0, -1]
                
            self -> On_Off_Motion_Events, /OFF

            ;Redraw to get rid of ghost text
            self -> Draw
        endcase
    endcase
end


;+
;   Create a weAxis object to be overplotted onto the graph.
;
; :Params:
;       X0:             in, required, type=integer/float
;                       The x location of the blunt end of the arrow. May be a vector.
;                           Assumes device coordinates.
;       X1:             in, required, type=integer/float
;                       The x location of the sharp end of the arrow. May be a vector.
;                           Assumes device coordinates.
;       Y0:             in, required, type=integer/float
;                       The y location of the blunt end of the arrow. May be a vector.
;                           Assumes device coordinates.
;       Y1:             in, required, type=integer/float
;                       The y location of the sharp end of the arrow. May be a vector.
;                           Assumes device coordinates.
;
; :Keywords:
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the overplot to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weArrow__define or cgArrow is also
;                               accepted for keyword inheritance.
;-
pro MrAbstractArrow::Arrow, x0, y0, x1, y1, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Create a cgOverPlot object
    theArrow = obj_new('weArrow', x0, y0, x1, y1, _STRICT_EXTRA=extra)

    ;Add the overplot to the array of overplots
    self -> AddArrows, theArrow
    
    ;Re-Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   Determine which "Annotate Menu" button was pressed and tell the draw widget which
;   type of events to pay attention to.
;
;   Only one annotation mode can be active at a time.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractArrow::Arrow_Menu_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Get the value of the button that cause the event
    widget_control, event.id, GET_VALUE=arrow_type
    arrow_type = strupcase(arrow_type)
    
    ;Get the button's siblings and uncheck them all
    parent = widget_info(event.id, /PARENT)
    kids = widget_info(parent, /ALL_CHILDREN)
    for i = 0, n_elements(kids) - 1 do widget_control, kids[i], SET_BUTTON=0
    
    ;Toggle the checked state of the menu button
    isSet = widget_info(event.id, /BUTTON_SET)
    isSet = ~isSet
    
    case arrow_type of
        'NONE': begin
            ;Turn everything off and copy the pixmap
            self.arrowmode = [0, 0, -1]
            self -> On_Off_Button_Events, /OFF
            self -> On_Off_Motion_Events, /OFF
            self -> copyPixmap
            
            ;Get the button's siblings and uncheck them all
            parent = widget_info(event.id, /PARENT)
            kids = widget_info(parent, /ALL_CHILDREN)
            for i = 0, n_elements(kids) - 1 do widget_control, kids[i], SET_BUTTON=0
        end
    
        'ADD ARROW': begin
            ;On
            if isSet then begin
                self.arrowmode = [1, 0, -1]
                self -> On_Off_Button_Events, /ON
            
            ;Off
            endif else begin
                self.arrowmode = [0, 0, -1]
                self -> On_Off_Button_Events, /OFF
            endelse
        endcase
        
        'MOVE ARROW': begin
            ;Turn on button events
            if isSet then begin
                self.arrowmode = [2, 0, -1]
                self -> On_Off_Button_Events, /ON
                
            ;If the button was turned off
            endif else begin
                self.arrowmode = [0, 0, -1]
                self -> On_Off_Button_Events, /OFF
            endelse
        endcase
        
        'STRETCH/ROTATE ARROW': begin
            ;If the button was turned on
            if isSet then begin
                self.arrowmode = [4, 0, -1]
                self -> On_Off_Button_Events, /ON
                
            ;If the button was turned off
            endif else begin
                self.arrowmode = [0, 0, -1]
                self -> On_Off_Button_Events, /OFF
            endelse
        endcase
        
        'REMOVE ARROW': begin
            ;If the button was turned on
            self.arrowmode = [8, 0, -1]
            self -> On_Off_Button_Events, /ON
        endcase
        
        else: message, 'Button "' + arrow_type + '" unknown.'
    endcase
    
    ;Put a check mark by the button
    if arrow_type ne 'ADD ARROW' and arrow_type ne 'REMOVE ARROW' and arrow_type ne 'NONE' $
        then widget_control, event.id, SET_BUTTON=isSet
end


;+
;   This method retrieves the positions of each arrow.
;
; :Keywords:
;       BOX:                in, optional, type=boolean, default=0
;                           Sort the positions so that [x0, y0] is the lower-left corner
;                               and [x1, y1] is the upper-right corner of the smallest
;                               box encompansing the arrow.
;       SYSTEM:             out, optional, type=string
;                           The coordinate system of `TEXTPOS`. This is one of "DATA", 
;                               "DEVICE" or "NORMAL".
;
; :Returns:
;       ARROWPOS:           The position [x0, y0, x1, y1] of the arrow tail and arrowhead.
;-
function MrAbstractArrow::Arrow_Positions, $
BOX = box, $
SYSTEM = system
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, -1
    endif
    
    ;Get the positions, widths, and charsizes of all the text annotations
    nArrows = n_elements(*self.arrows)
    if nArrows eq 0 then return, -1
    
    ;Get the coordinate system?
    get_system = arg_present(system)
    if get_system then system = strarr(nArrows)
    
    ;Allocate memory
    arrowPos = intarr(4, nArrows)
    
    ;Get the location of the texts.
    for i = 0, nArrows - 1 do begin
        (*self.arrows)[i] -> GetProperty, X0=x0, X1=x1, Y0=y0, Y1=y1, $
                                          DEVICE=device, NORMAL=normal, DATA=data
        
        ;Determine which coordinate system is being used?
        if get_system then begin
            if keyword_set(normal) then system[i] = 'Normal'
            if keyword_set(device) then system[i] = 'Device'
            if keyword_set(data) then system[i] = 'Data'
            
            ;The default is data coordinates
            if system[i] eq '' then system[i] = 'Device'
        endif
        
        ;Record the arrow positions
        if keyword_set(box) $
            then arrowPos[*,i] = [x0 < x1, y0 < y1, x1 > x0, y1 > y0] $
            else arrowPos[*,i] = [x0, y0, x1, y1]
    endfor
    
    return, arrowPos
end


;+
;   Create a menu bar with various Arrow options in it.
;
; :Params:
;       PARENT:             in, required, type=integer
;                           The widget ID of the parent widget for the new SaveAs Menu.
;-
pro MrAbstractArrow::Create_Arrow_Menu, parent
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Create the text menu
    textID = widget_button(parent, VALUE='Arrow', /MENU)
    button = widget_button(textID, VALUE='Add Arrow', /CHECKED_MENU, UVALUE={object: self, method: 'Arrow_Menu_Events'})
    button = widget_button(textID, VALUE='Move Arrow', /CHECKED_MENU, UVALUE={object: self, method: 'Arrow_Menu_Events'})
    button = widget_button(textID, VALUE='Stretch/Rotate Arrow', /CHECKED_MENU, UVALUE={object: self, method: 'Arrow_Menu_Events'})
    button = widget_button(textID, VALUE='Remove Arrow', /CHECKED_MENU, UVALUE={object: self, method: 'Arrow_Menu_Events'})
    button = widget_button(textID, VALUE='None', UNAME='ArrowNone', /CHECKED_MENU, UVALUE={object: self, method: 'Arrow_Menu_Events'})

end


;+
;   The purpose of this method is to draw the arrows in the draw window.
;-
pro MrAbstractArrow::Draw
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Call each of the weArrows' draw methods.
    for i = 0, n_elements(*self.arrows) - 1 do (*self.arrows)[i] -> Draw
end


;+
;   Handle events from the draw widget
;
;   Event Sequence::
;       Place_Text
;           1) Click on plot
;           2) Enter info into GUI
;           3) Text is drawn, button events are turned off
;       Move Text
;           1) Click on text Motion events turned on
;               a) Text is removed from list
;               b) Redraw plot without text to eliminate "ghost"
;           2) Drag text to new location
;               a) Copy Pixmap
;               b) Draw text in new location
;           3) Release to fix text
;               a) Turn off motion events
;       Rotate Text
;           1) Click on text Motion events turned on
;               a) Text is removed from list
;               b) Redraw plot without text to eliminate "ghost"
;           2) Drag mouse to set new rotation angle
;               a) Copy Pixmap
;               b) Draw text in new location
;           3) Release to fix text
;               a) Turn off motion events
;-
pro MrAbstractArrow::Draw_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = error_message()
        return
    endif
    
;---------------------------------------------------------------------
;Button Press Events /////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 0 then begin
        if self.arrowmode[0] eq 1 then self -> Place_Arrow, event     ;Place arrow
        if self.arrowmode[0] eq 2 then self -> Move_Arrow, event      ;Move arrow
        if self.arrowmode[0] eq 4 then self -> Rotate_Arrow, event    ;Stretch/Rotate arrow
        if self.arrowmode[0] eq 8 then self -> Remove_Arrow, event    ;Remove arrow
    endif

;---------------------------------------------------------------------
;Motion Events ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 2 then begin
        ;Do not compete for copying the pixmap
        if ( self.arrowmode[1] eq 1 && ( self.arrowmode[0] eq 1 || $      ;Place arrow
                                         self.arrowmode[0] eq 2 || $      ;Move arrow
                                         self.arrowmode[0] eq 4 ) ) $     ;Stretch/Rotate arrow
            then self -> copyPixmap
        
        ;Arrow Mode
        if (self.arrowmode)[1] eq 1 then begin
            if self.arrowmode[0] eq 1 then self -> Place_Arrow, event     ;Place arrow
            if self.arrowmode[0] eq 2 then self -> Adjust_Arrow, event    ;Move arrow
            if self.arrowmode[0] eq 4 then self -> Adjust_Arrow, event    ;Stretch/Rotate arrow
        endif
    endif
    
;---------------------------------------------------------------------
;Button Release Events ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 1 then begin
        if self.arrowmode[1] eq 1 then begin ;Button Up turns motion events off.
            if self.arrowmode[0] eq 1 then self -> Place_Arrow, event
            if self.arrowmode[0] eq 2 then self -> Move_Arrow, event
            if self.arrowmode[0] eq 4 then self -> Adjust_Arrow, event
        endif
    endif
end


;+
;   Serve as a general error handler for event handling routines.
;-
pro MrAbstractArrow::Error_Handler
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    self.x0 = -1
    self.y0 = -1
    
    ;Turn off motion and button events. Reset the mode indicator.
    self.arrowmode = [0, 0, -1]
    self -> On_Off_Button_Events, /OFF
    self -> On_Off_Motion_Events, /OFF
    
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
pro MrAbstractArrow::On_Off_Button_Events, $
ON = on, $
OFF = off
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
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
        
        ;Turn motion events off only if nothing else needs them.
        if (self.arrowmode[0] eq 0) then widget_control, self.drawID, DRAW_BUTTON_EVENTS=0
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
pro MrAbstractArrow::On_Off_Motion_Events, $
ON = on, $
OFF = off
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
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
        
        ;Turn motion events off only if nothing else needs them.
        if (self.arrowmode[1] eq 0) then begin
            widget_control, self.drawID, DRAW_MOTION_EVENTS=0
            widget_control, self.drawID, /CLEAR_EVENTS
        endif
    endif
end


;+
;   Event hanlder for placing new text on the display.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractArrow::Place_Arrow, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = error_message()
        return
    endif
    
    ;Make sure we want to move the text.
    case event.type of
        0: if self.arrowmode[0] ne 1 then return
        1: if self.arrowmode[0] ne 1 then return
        2: if self.arrowmode[1] eq 0 || self.arrowmode[0] ne 1 then return
        else: return
    endcase
    
    ;Which stage of the movement are we in?
    case event.type of
        ;Button Down
        0: begin
            ;Store the location of the arrow's tail
            self.x0 = event.x
            self.y0 = event.y
            
            ;Activate motion events
            self.arrowmode[1] = 1
            self -> On_Off_Motion_Events, /ON
        endcase
        
        ;Motion
        2: begin
            ;Draw the arrow. The tail's location has been saved. The head is located
            ;at the current mouse location.
            cgArrow, self.x0, self.y0, event.x, event.y, COLOR='Black'
        endcase
        
        ;Button Up
        1: begin
            ;Permanently add the arrow object.
            self -> Arrow, self.x0, self.y0, event.x, event.y, COLOR='Black'

            ;Reset configuration. Only draw one arrow at a time.
            self.arrowmode = [0, 0, -1]
            self -> On_Off_Button_Events, /OFF
            self -> On_Off_Motion_Events, /OFF
            
            self.x0 = -1
            self.y0 = -1

            ;Redraw to get rid of ghost text
            self -> Draw
        endcase
    endcase
    
    
end


;+
;   Event hanlder for placing new text on the display.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractArrow::Remove_Arrow, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if ptr_valid(pText) then ptr_free, pText
        self -> Error_Handler
        void = error_message()
        return
    endif
    
    ;Only listen to button presses
    if event.type ne 0 then return
    if self.arrowmode[0] ne 8 then return   ;Remove Arrow
    
    ;Get the text positions
    arrowPos = self -> Arrow_Positions(/BOX)
            
    ;Find the text that is closest
    thisArrow = where(arrowPos[0,*] le event.x and arrowPos[2,*] ge event.x and $
                      arrowPos[1,*] le event.y and arrowPos[3,*] ge event.y, count)

    ;If no text exists at that location, then return
    if count eq 0 then return

    ;Remove the text object
    self -> AddArrows, INDEX=thisArrow, /REMOVE, /DRAW
    
    ;Turn off Button Events
    self.arrowmode = [0, 0, -1]
    self -> On_Off_Button_Events, /OFF
    
end


;+
;   This method provides a means of determining the index location at which each
;   Arrow is stored. Arrows are distinguished by their colors.
;-
PRO MrAbstractArrow::whichArrows
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ;Allow the Superclass to have a space flush with the left of the display window
    Print, '  Arrows:'
    
    ;Print the arrow indices and colors
    FOR j = 0, N_Elements(*self.arrows) - 1 DO BEGIN
        (*self.arrows)[j] -> GetProperty, X0=x0, X1=x1, Y0=y0, Y1=y1, COLOR=color, DEVICE=device
        index = String(j, FORMAT='(i3)')
        
        IF N_Elements(color) EQ 0 THEN color = ''
        IF N_Elements(device) EQ 0 THEN device = 0

        Print, FORMAT='(%"    Index: %i    Color: %s    Position: [%i, %i, %i, %i]")', $
               index, color, x0, y0, x1, y1
    ENDFOR
END


;+
;   This method cleans up after the object is destroyed.
;-
pro MrAbstractArrow::cleanup
    compile_opt idl2
    
    ;Destroy all weOverPlot objects
    if ptr_valid(self.arrows) then begin
        for i = 0, n_elements(*self.arrows)-1 do begin
            obj_destroy, (*self.arrows)[i]
        endfor
        ptr_free, self.arrows
    endif
end


;+
;   The initialization method. Because MrAbstractArrow is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractArrow object will result
;   in an error.
;-
function MrAbstractArrow::init
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif
    
    message, 'This is an abstract class and must be inherited.'
end


;+
;   The class definition statement.
;-
pro MrAbstractArrow__define
    compile_opt idl2
    
    class = {MrAbstractArrow, $
             arrows: ptr_new(), $             ;Array of weText objects
             arrowmode: [0, 0, 0]}            ;[text mode, active, index]
end