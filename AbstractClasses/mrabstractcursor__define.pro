; docformat = 'rst'
;
; NAME:
;       MrAbstractCursor__Define
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
;       8       -   Focus
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
;-
;*****************************************************************************************
;+
;   Create a menu bar with various zoom options in it.
;
; :Params:
;       PARENT:             in, required, type=integer
;                           The widget ID of the parent widget.
;
; :Keywords:
;       MENU:               in, optional, type=boolean, default=1
;                           If set, all buttons will be placed under a "Cursor" submenu.
;       CROSS_HAIRS:        in, optional, type=boolean, default=1
;                           Create the "Cross Hairs" button.
;       FOCUS:              in, optional, type=boolean, default=1
;                           Create the "Focus" button.
;       GET_POINT:          in, optional, type=boolean, default=1
;                           Create the "Get Point" button.
;       SHOW_XY:            in, optional, type=boolean, default=1
;                           Create the "Show [X,Y]" button.
;       NONE:               in, optional, type=boolean, default=1
;                           Create the "None" button.
;-
pro MrAbstractCursor::Create_Cursor_Menu, parent, $
MENU = menu, $
CROSS_HAIRS = cross_hairs, $
FOCUS = focus, $
GET_POINT = get_point, $
SHOW_XY = show_xy, $
NONE = none
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    setDefaultValue, menu, 1, /BOOLEAN
    setDefaultValue, cross_hairs, 1, /BOOLEAN
    setDefaultValue, focus, 1, /BOOLEAN
    setDefaultValue, get_point, 1, /BOOLEAN
    setDefaultValue, show_xy, 1, /BOOLEAN
    setDefaultValue, none, 1, /BOOLEAN
    
    ;Create the Menu
    if keyword_set(menu) $
        then cursorID = widget_button(parent, VALUE='Cursor', /MENU) $
        else cursorID = parent
    
    
    button = widget_button(cursorID, VALUE='Cross Hairs', UNAME='CROSS_HAIRS', /CHECKED_MENU, UVALUE={object: self, method: 'Cursor_Menu_Events'})
    button = widget_button(cursorID, VALUE='Focus', UNAME='FOCUS', /CHECKED_MENU, UVALUE={object: self, method: 'Cursor_Menu_Events'})
    button = widget_button(cursorID, VALUE='Get Point', UNAME='GET_POINT', /CHECKED_MENU, UVALUE={object: self, method: 'Cursor_Menu_Events'})
    button = widget_button(cursorID, VALUE='Show [x,y]', UNAME='SHOW_XY', /CHECKED_MENU, UVALUE={object: self, method: 'Cursor_Menu_Events'})
    button = widget_button(cursorID, VALUE='None', UNAME='CNONE', UVALUE={object: self, method: 'Cursor_Menu_Events'})
end


;+
;   Determine which "Cursor Menu" button was pressed and tell the draw widget which
;   type of events to pay attention to.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractCursor::Cursor_Menu_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Get the value of the button that cause the event
    widget_control, event.id, GET_VALUE=cursor_type
    
    ;Toggle the checked state of the menu button
    isSet = widget_info(event.id, /BUTTON_SET)
    isSet = ~isSet
    
    case strupcase(cursor_type) of
        'NONE': begin
            ;Turn everything off and copy the pixmap
            self.cmode = 0
            self -> On_Off_Button_Events, /OFF
            self -> On_Off_Motion_Events, /OFF
            self -> copyPixmap
            
            ;Get the button's siblings and uncheck them all
            parent = widget_info(event.id, /PARENT)
            kids = widget_info(parent, /ALL_CHILDREN)
            for i = 0, n_elements(kids) - 1 do widget_control, kids[i], SET_BUTTON=0
            
            ;Make the status bar blank
            widget_control, self.statusID, SET_VALUE=' '
        endcase
        
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
        
        'FOCUS': begin
            if isSet then begin
                self.cmode += 8
                self -> On_Off_Button_Events, /ON
            endif else begin
                self.cmode -= 8
                self -> On_Off_Button_Events, /OFF
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
pro MrAbstractCursor::Cross_Hairs, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Draw from the first clicked point to the current position
    horiz_x = [0, self.xsize]
    horiz_y = [event.y, event.y]
    vert_x = [event.x, event.x]
    vert_y = [0, self.ysize]
    plots, horiz_x, horiz_y, color=load_color('blue'), /DEVICE
    plots, vert_x, vert_y, color=load_color('blue'), /DEVICE
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
pro MrAbstractCursor::Draw_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
;---------------------------------------------------------------------
;Button Press Events /////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 0 then begin
        if ((self.cmode and 8) gt 0) then self -> Focus, event          ;Focus
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
;   Event handler for Focus events. Find the closest plot to the clicked point.
;
; :Params:
;   EVENT:              in, optional, type=structure/int
;                       The event returned by the windows manager. If not given, the
;                           plot indicated by SELF.IFOCUS will become the object of focus.
;                           If EVENT is an integer, then it is the index value of the plot
;                           on which to focus.
;-
pro MrAbstractCursor::Focus, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
        
;---------------------------------------------------------------------
;Event? //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    event_type = size(event, /TYPE)
    
    if event_type eq 8 then begin

        ;Only listen to left button presses and only if the Focus bit is set
        if event.type ne 0 || event.press ne 1 || ((self.cmode and 8) eq 0) then return
    
    ;---------------------------------------------------------------------
    ;Only One Option /////////////////////////////////////////////////////
    ;---------------------------------------------------------------------

        if self.nplots eq 1 then begin
            ifocus = 0
        
    ;---------------------------------------------------------------------
    ;Find Clostest Plot //////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endif else begin
            ;Convert clicked points to normal coordinates
            xy = convert_coord(event.x, event.y, /DEVICE, /TO_NORMAL)

            ;Get the location of the center of each plot
            plot_centers = fltarr(2, n_elements(*self.plot_positions)/4)
            plot_centers[0,*] = ( (*self.plot_positions)[2,*] - (*self.plot_positions)[0,*] ) / 2.0 + $
                                (*self.plot_positions)[0,*]
            plot_centers[1,*] = ( (*self.plot_positions)[3,*] - (*self.plot_positions)[1,*] ) / 2.0 + $
                                (*self.plot_positions)[1,*]
        
            ;Find the distance from the plot centers to the clicked point.
            distance = sqrt( (plot_centers[0,*] - xy[0])^2 + $
                             (plot_centers[1,*] - xy[1])^2 )

            ;Get the index of the plot closest to the clicked point.
            void = min(distance, ifocus)
        endelse
        
        self.ifocus = ifocus
        
;---------------------------------------------------------------------
;Plot Index Given? ///////////////////////////////////////////////////
;---------------------------------------------------------------------

    endif else if event_type ne 0 then begin
        
        self.ifocus = event
        
    endif

;---------------------------------------------------------------------
;Set Focus ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Set the system variables and synchronize the modes.
    theObj = (*self.allObjects)[self.ifocus]

    theObj -> getProperty, X_SYSVAR=x_sysvar, Y_SYSVAR=y_sysvar, P_SYSVAR=p_sysvar
    !X = x_sysvar
    !Y = y_sysvar
    !P = p_sysvar
end


;+
;   Print the coordinates of the mouse click to the command window.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractCursor::Get_Point, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Only care about button presses
    if event.type ne 0 then return

    ;Convert the clicked point to data coordinates and print the location
    coords = convert_coord(event.x, event.y, /DEVICE, /TO_DATA)
    print, FORMAT='(%"xy = [%f, %f]")', coords[0], coords[1]
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
pro MrAbstractCursor::On_Off_Button_Events, $
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
        
        ;Turn motion events off only if nothing else needs them. Multiple cursor
        ;modes can be active at the same time. Use AND to check for bits.
        
        ;Get Points, Focus
        if ((self.cmode and 1) eq 0) and ((self.cmode and 8) eq 0) then $
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
pro MrAbstractCursor::On_Off_Motion_Events, $
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
;   Show the [x, y] coordinates of the cursor below the plot.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractCursor::Show_XY, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
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
;   Clean up after the object is destroy
;-
pro MrAbstractCursor::cleanup
    ;Nothing to clean up.
end


;+
;   The initialization method. Because MrAbstractCursor is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractCursor object will result
;   in an error.
;-
function MrAbstractCursor::init
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
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrAbstractCursor__define, class
    compile_opt idl2
    
    class = {MrAbstractCursor, $
             cmode: 0, $            ;Currntly active cursor mode(s).
             ifocus: 0 $            ;Index of the plot on which to focus.
            }
end