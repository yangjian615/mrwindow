; docformat = 'rst'
;
; NAME:
;       MrCursor__Define
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
;   The purpose of this method is to serve as an abstract class various cursor
;   capabilites.
;
;   SETUP::
;       Subclass must have the following properties::
;           pixID:              Window ID of the pixmap window
;           statusID:           The widget ID of a widget label in which to
;                               display the cursor's location on the screen.
;           plotObjects:        An array containing the object references of each plot
;                               drawn in the window.
;           positions:          A 4xN array containing the plot positions, in normalized
;                               coordinates, of each plot drawn in the window.
;
;       Subclass must have the following methods::
;           copyPixmap:         Method for copying the pixmap (pixID) to the display (winID)
;           Draw_Events:        Event handling method for the draw widget.
;
;       Set the draw widget UValue and Event Procedure::
;           UVALUE = {object: self, method: 'Draw_Events'}
;           EVENT_PRO = 'your_event_handling_procedure_goes_here'
;
;       In the procedure specified by EVENT_PRO, use the Call_Method procedure::
;           Widget_Control, self.drawID, GET_UVALUE=event_handler
;           Call_Method, event_handler.method, event_handler.object, event
;
;   CURSOR-ING::
;       - Button and/or Motion events must be turned on for the draw widget.
;       - CMODE must be set to any combination of valid cursor bits (see below).
;       - To turn on/off button or motion events::
;           * Use On_Off_Button_Events and On_Off_Motion_Events to turn on and off
;             button and motion events, respectively. See those methods rules governing
;             'ON' and 'OFF' and the 'Draw_Events' method for the sequences of events.
;
;   CURSOR MENU::
;       1) Create a top level base containing a menu bar and a draw widget.
;           a) For the Show_XY method to work, a widget label with ID statusID must
;               be present as well.
;       2) Pass the menu bar's widget ID to the Create_Cursor_Menu.
;       3) Event handling for the menu is done internally by the Cursor_Menu_Events method.
;       4) Draw widget event handling must be configured as described above.
;
;   Cursor Options::
;       Get Points  -   Click on the plot to have the location of the mouse printed to
;                       the command window.
;       Cross Hairs -   Vertical and horizontal lines spanning the display window will
;                       be drawn at the location of the mouse pointer as the mouse moves
;                       around the display.
;       Show [X,Y]  -   Display the x- and y-coordinates of the mouse in a status menu
;                       below the plot.
;
;   Cursor Bits::
;       0       -   None
;       1       -   Get Point
;       2       -   Cross Hairs
;       4       -   Show [X,Y]
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
;       05/17/2013  -   Written by Matthew Argall
;       06/27/2013  -   Added the Focus method.
;       07/07/2013  -   All buttons are individually implementable. - MRA
;       07/31/2013  -   A list index can now be given to the focus method. - MRA
;       08/23/2013  -   Added the Init, SetProperty and GetProperty methods. Renamed
;                           to MrCursor__Define from MrAbstractCursor__Define. Added
;                           the Turn_Everything_Off method. Keywords to Create_Cursor_Menu
;                           are now being used. Superclass is now assumed to use either
;                           IDL_Container or MrIDL_Container. Removed the Focus method
;                           because it did not fit conceptually. Moved to MrWindow__Define. - MRA
;       09/23/2013  -   Removed the IFOCUS property. - MRA
;       2013/12/19  -   Now use cgColor to load colors into the color table. - MRA
;-
;*****************************************************************************************
;+
;   Create a menu bar with various cursor options in it. If no keywords are set, then all
;   buttons will be created.
;
; :Params:
;       PARENT:             in, required, type=integer
;                           The widget ID of the parent widget.
;
; :Keywords:
;       MENU:               in, optional, type=boolean, default=0
;                           If set, all buttons will be placed under a "Cursor" submenu.
;       CROSS_HAIRS:        in, optional, type=boolean, default=0
;                           Create the "Cross Hairs" button.
;       GET_POINT:          in, optional, type=boolean, default=0
;                           Create the "Get Point" button.
;       SHOW_XY:            in, optional, type=boolean, default=0
;                           Create the "Show [X,Y]" button.
;       NONE:               in, optional, type=boolean, default=0
;                           Create the "None" button.
pro MrCursor::Create_Cursor_Menu, parent, $
MENU = menu, $
CROSS_HAIRS = cross_hairs, $
GET_POINT = get_point, $
SHOW_XY = show_xy, $
NONE = none
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Defaults
    setDefaultValue, menu, 0, /BOOLEAN
    setDefaultValue, cross_hairs, 0, /BOOLEAN
    setDefaultValue, get_point, 0, /BOOLEAN
    setDefaultValue, show_xy, 0, /BOOLEAN
    setDefaultValue, none, 0, /BOOLEAN
    
    ;If nothing was selected, create all buttons
    if cross_hairs + get_point + show_xy eq 0 then begin
        cross_haris = 1
        focus = 1
        get_point = 1
        show_xy = 1
        none = 1
    endif
    
    ;Create the Menu
    if keyword_set(menu) $
        then cursorID = widget_button(parent, VALUE='Cursor', /MENU) $
        else cursorID = parent
    
    ;Create Buttons
    if cross_hairs then button = widget_button(cursorID, VALUE='Cross Hairs', UNAME='CROSS_HAIRS', /CHECKED_MENU, UVALUE={object: self, method: 'Cursor_Menu_Events'})
    if get_point   then button = widget_button(cursorID, VALUE='Get Point',   UNAME='GET_POINT',   /CHECKED_MENU, UVALUE={object: self, method: 'Cursor_Menu_Events'})
    if show_xy     then button = widget_button(cursorID, VALUE='Show [x,y]',  UNAME='SHOW_XY',     /CHECKED_MENU, UVALUE={object: self, method: 'Cursor_Menu_Events'})
    if none        then button = widget_button(cursorID, VALUE='None',        UNAME='CURSOR_NONE',                UVALUE={object: self, method: 'Cursor_Menu_Events'})
end


;+
;   Determine which "Cursor Menu" button was pressed and tell the draw widget which
;   type of events to pay attention to.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrCursor::Cursor_Menu_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Get the value of the button that cause the event
    widget_control, event.id, GET_VALUE=cursor_type
    
    ;Toggle the checked state of the menu button
    isSet = widget_info(event.id, /BUTTON_SET)
    isSet = ~isSet
    
    case strupcase(cursor_type) of
        'NONE': self -> Turn_Everything_Off, self.tlb
        
        'GET POINT': begin
            if isSet then begin
                self.cmode += 1
                self -> On_Off_Button_Events, /ON
            endif else begin
                self.cmode -= 1
                self -> On_Off_Button_Events, /OFF
            endelse
        endcase
        
        'CROSS HAIRS': begin
            ;If the button was turned on
            if isSet then begin
                self.cmode += 2
                self -> On_Off_Motion_Events, /ON
                
            ;If the button was turned off
            endif else begin
                self.cmode -= 2
                self -> On_Off_Motion_Events, /OFF
                
                ;Copy the pixmap to remove the corss-hairs
                self -> copyPixmap
            endelse
        endcase
        
        'SHOW [X,Y]': begin
            if isSet then begin
                self.cmode += 4
                self -> On_Off_Motion_Events, /ON
            endif else begin
                self.cmode -= 4
                widget_control, self.statusID, SET_VALUE=' '
                self -> On_Off_Motion_Events, /OFF
            endelse
        endcase
        
        else: message, 'Button "' + cursor_type + '" unknown.'
    endcase
    
    ;Put a check mark by the button
    widget_control, event.id, SET_BUTTON=isSet
end


;+
;   Display a set of cross-hairs that extends from the mouse location horizontally and
;   vertically to the edge of the display window.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrCursor::Cross_Hairs, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = cgErrorMsg()
        return
    endif
    
    ;Draw from the first clicked point to the current position
    horiz_x = [0, self.xsize]
    horiz_y = [event.y, event.y]
    vert_x = [event.x, event.x]
    vert_y = [0, self.ysize]
    plots, horiz_x, horiz_y, color=cgColor('blue'), /DEVICE
    plots, vert_x, vert_y, color=cgColor('blue'), /DEVICE
end


;+
;   Handle events from the draw widget
;
;   NOTES on copyPixmap::
;       Cross Hairs -   copyPixmap at beginning of every motion event
;
;   NOTES on Cursor Event Sequence::
;       Get Point::
;           * Button down   -- Print coordinates of clicked point
;       Cross Hair::
;           * Motion        -- COPYPIXMAP, draw cross hairs
;       Show [X,Y]::
;           * Motion        -- Update status bar with coordinates of cursor
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrCursor::Draw_Events, event
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
        if ((self.cmode and 1) gt 0) then self -> Get_Point, event      ;Get Point
    endif

;---------------------------------------------------------------------
;Motion Events ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 2 then begin
        ;Do not compete for copying the pixmap: Cross Hairs
        if ((self.cmode and 2) gt 0) then self -> copyPixmap

        ;Handle motion events
        if ((self.cmode and 2) gt 0) then self -> Cross_Hairs, event
        if ((self.cmode and 4) gt 0) then self -> Show_XY, event
    endif
end


;+
;   Serve as a general error handler for event handling routines.
;-
pro MrZoom::Error_Handler
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    self -> Turn_Everything_Off, self.tlb
end


;+
;   Print the coordinates of the mouse click to the command window.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrCursor::Get_Point, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = cgErrorMsg()
        return
    endif
    
    ;Only care about button presses
    if event.type ne 0 then return

    ;Convert the clicked point to data coordinates and print the location
    coords = convert_coord(event.x, event.y, /DEVICE, /TO_DATA)
    print, FORMAT='(%"xy = [%f, %f]")', coords[0], coords[1]
end


;+
;   The purpose of this method is to get object properties.
;
; :Keywords:
;       CMODE:              out, optional, type=int
;                           The cursor mode(s) currently active.
;-
pro MrCursor::GetProperty, $
CMODE = cmode
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    if arg_present(cmode) ne 0 then cmode = self.cmode
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
pro MrCursor::On_Off_Button_Events, $
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
        
        ;Turn motion events off only if nothing else needs them. Multiple cursor
        ;modes can be active at the same time. Use AND to check for bits.
        
        ;Get Points
        if ((self.cmode and 1) eq 0) then $
            widget_control, self.drawID, DRAW_BUTTON_EVENTS=0
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
pro MrCursor::On_Off_Motion_Events, $
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
        
        ;Turn motion events off only if nothing else needs them. Multiple cursor
        ;modes can be active at the same time. Use AND to check for bits.
        
        ;Cross Hairs, Show [X,Y]
        if ((self.cmode and 2) eq 0) and ((self.cmode and 4) eq 0) then begin
            widget_control, self.drawID, DRAW_MOTION_EVENTS=0
            widget_control, self.drawID, /CLEAR_EVENTS
        endif
    endif
end


;+
;   The purpose of this method is to set object properties.
;
; :Keywords:
;       CMODE:              in, optional, type=int
;                           The cursor mode(s) to be active.
;-
pro MrCursor::SetProperty, $
CMODE = cmode
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    if n_elements(cmode) ne 0 then self.cmode = cmode
end


;+
;   Show the [x, y] coordinates of the cursor below the plot.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrCursor::Show_XY, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = cgErrorMsg()
        return
    endif

    ;Convert the cursor position from device to data coordinates
    datac = convert_coord(event.x, event.y, /DEVICE, /TO_DATA)
    
    ;Make a string variable from the coordinates of the mouse.
    status_string = string(FORMAT='(%"[%f0, %f0]")', datac[0], datac[1])

    ;Set the string as the value of the label widget
    widget_control, self.statusID, SET_VALUE=status_string
end


;+
;   A method for turning off all zoom options and effects.
;
; :Params:
;       TLB:        in, optional, type=int
;                   The widget ID of the top level base.
;-
pro MrCursor::Turn_Everything_Off, tlb
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Reset event processing
    self.cmode = 0
    self -> On_Off_Button_Events, /OFF
    self -> On_Off_Motion_Events, /OFF
    
    ;Copy the pixmap to erase the cursor.
    self -> CopyPixmap
    
    ;Make the status bar blank.
    widget_control, self.statusID, SET_VALUE=' '
    
    ;Uncheck all menu buttons
    if n_elements(tlb) gt 0 && widget_info(tlb, /VALID_ID) then begin
        ;Get the widget IDs of the menu buttons
        chID  = widget_info(tlb, FIND_BY_UNAME='CROSS_HAIRS')
        focID = widget_info(tlb, FIND_BY_UNAME='FOCUS')
        gpID  = widget_info(tlb, FIND_BY_UNAME='GET_POINT')
        sxyID = widget_info(tlb, FIND_BY_UNAME='SHOW_XY')

        ;Uncheck the buttons
        if widget_info(chID,  /VALID_ID) then widget_control, chID,  SET_BUTTON=0
        if widget_info(focID, /VALID_ID) then widget_control, focID, SET_BUTTON=0
        if widget_info(gpID,  /VALID_ID) then widget_control, gpID,  SET_BUTTON=0
        if widget_info(sxyID, /VALID_ID) then widget_control, sxyID, SET_BUTTON=0
    endif
end


;+
;   Clean up after the object is destroy
;-
pro MrCursor::cleanup
    ;Nothing to clean up.
end


;+
;   The initialization method.
;
; :Keywords:
;       CMODE:              in, optional, type=int
;                           The cursor mode(s) to be active initially.
;-
function MrCursor::init, $
CMODE = cmode
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    SetDefaultValue, cmode, 0
    self -> SetProperty, CMODE=cmode
    
    return, 1
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrCursor__define, class
    compile_opt idl2
    
    class = {MrCursor, $
             cmode: 0 $             ;Currntly active cursor mode(s).
            }
end