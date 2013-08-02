; docformat = 'rst'
;
; NAME:
;       MrAbstractText__Define
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
;   removing, and drawing weText objects. It is meant to be inherited and will
;   not on its own.
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
;   TEXT EVENTS::
;       - Button events must be turned on for the draw widget.
;       - textmode[0] must be set to the proper bit (see below).
;       - Button and motion events are turned on and off as needed:
;           * Use On_Off_Button_Events and On_Off_Motion_Events to turn on and off
;             button and motion events, respectively. See those methods rules governing
;             'ON' and 'OFF' and the 'Draw_Events' method for the sequences of events.
;
;   TEXT MENU::
;       1) Create a top level base containing a menu bar and a draw widget.
;       2) Pass the menu bar's widget ID to the Create_Text_Menu.
;       3) Event handling for the menu is done internally by the Text_Menu_Events method.
;       4) Draw widget event handling must be configured as described above.
;
;   ABOUT TextMode::
;       Text mode is an integer array with three values::
;           textmode[0]:        The bit corresponding to which mode is set
;           textmode[1]:        Indicates that motion events are in use by textmode[0]
;           textmode[2]:        The index of the text object currently being manipulated.
;
;   MODES::
;       0:                  -   None
;       1:      Place_Text  -   Place text by clicking on the draw window.
;       2:      Move_Text   -   Move a Text around the draw window.
;       4:      Rotate_Text -   Change the orentation of the text.
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
;       05/15/2013  -   Written by Matthew Argall
;       05/17/2013  -   Added event handling capabilities: the Draw_Events, Place_Text
;                           Adjust_Text, On_Off_Button_Events, On_Off_Motion_Events,
;                           and Error_Handler methods. Changed ANDs to EQs to indicate
;                           that only one mode can function at a time. Added Rotate and
;                           Remove capabilities. - MRA
;       05/18/2013  -   Added a ::Create_Text_Menu and ::Text_Menu_Events. - MRA
;-
;*****************************************************************************************
;+
;   Add a weText objects.
;
; :Params:
;       TEXT_OBJECT:            in, optional, type=object/obj_arr
;                               The weText object(s) to add to the plot.
;
;   :Keywords:
;       CLEAR:                  in, optional, type=boolean, default=0
;                               If set, clear all axes objects before adding new ones.
;                                   Otherwise, new ones are appended to the end.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the axes to the list.
;       INDEX:                  in, optional, type=int, default=0
;                               The index at which to `REPLACE` or `REMOVE` axis objects.
;       REMOVE:                 in, optional, type=boolean, default=0
;                               If set, remove the axis object and `INDEX` from the list.
;       REPLACE:                in, optional, type=boolean, default=0
;                               If set, replace the object at `INDEX` instead of appending
;                                   to the end of the list.
;-
pro MrAbstractText::AddText, text_object, $
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
    
    ;Remove a text object?
    if keyword_set(remove) then begin
        if n_elements(index) eq 0 then index = 0
        n_axes = n_elements(*self.text)
        
        case n_axes of
            0: return
            1: begin
                ;destroy the object and reset the pointer
                obj_destroy, (*self.text)[index]
                ptr_free, self.text
                self.text = ptr_new(/ALLOCATE_HEAP)
            endcase
            
            else: begin
                ;destroy the object, shift the invalid object to the end, truncate
                obj_destroy, (*self.text)[index]
                *self.text = shift(*self.text, -index-1)
                *self.text = (*self.text)[0:n_axes-1]
                *self.text = shift(*self.text, index)
            endelse
        endcase
        
        ;Draw?
        if keyword_set(draw) then self -> Draw
        
        return
    endif
    
    ;Destroy and replace old object, then return
    if keyword_set(replace) then begin
        if n_elements(index) eq 0 then index = 0
        obj_destroy, (*self.text)[index]
        (*self.text)[index] = text_object
        
        ;Draw?
        if keyword_set(draw) then self -> Draw
        
        return
    endif
    
    ;Clear all overplots first?
    if keyword_set(clear) then begin
        if ptr_valid(self.text) then begin
            for i = 0, n_elements(self.text) - 1 do obj_destroy, (*self.text)[i]
        endif
        ptr_free, self.text
    endif
    
    ;If nothing was given to add, then return
    if n_elements(text_object) eq 0 then begin
        if ptr_valid(self.text) eq 0 then self.text = ptr_new(/ALLOCATE_HEAP)
        return
    endif
    
    ;Add the object
    if ptr_valid(self.text) $
        then *self.text = [*self.text, text_object] $
        else self.text = ptr_new(text_object)
    
    ;Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   Event hanlder for moving text around on the display.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractText::Adjust_Text, event
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
        0: if self.textmode[0] ne 2 and self.textmode[0] ne 4 then return
        1: if self.textmode[0] ne 2 and self.textmode[0] ne 4 then return
        2: if self.textmode[1] eq 0 || (self.textmode[0] ne 2 and self.textmode[0] ne 4) then return
        else: return
    endcase
    
    ;Which stage of the movement are we in?
    case event.type of
        ;Button Down
        0: begin
            ;Get the positions, widths, and charsizes of all the text annotations
            nTexts = n_elements(*self.text)
            textPos = intarr(4, nTexts)
        
            ;Create a box around each text approximately where it can be found.
            textPos = self -> Text_Positions(/BOX)
            
            ;Find the text that is closest
            thisText = where(textPos[0,*] le event.x and textPos[2,*] ge event.x and $
                             textPos[1,*] le event.y and textPos[3,*] ge event.y, count)

            ;If no text exists at that location, then return
            if count eq 0 then return
            
            ;Store the location of the text and turn on motion events
            self.textmode[2] = thisText[0]
            self -> On_Off_Motion_Events, /ON
            self.textmode[1] = 1
        endcase
        
        ;Motion
        2: begin
            ;1. Must Copy Pixmap to get rid of ghost text
            ;       --> done in Draw_Events
            ;2. Write the text again.
            ;Move?
            if self.textmode[0] eq 2 then begin
                ;Update the position
                (*self.text)[self.textmode[2]] -> SetProperty, XLOC=event.x, YLOC=event.y, /DRAW
            
            ;Rotate?
            endif else if self.textmode[0] eq 4 then begin
                ;Calculate the angle
                (*self.text)[self.textmode[2]] -> GetProperty, XLOC=x, YLOC=y
                xy = float([event.x - x, event.y - y])
                theta = acos( xy[0] / sqrt( xy[0]^2 + xy[1]^2 ) ) * !radeg

                ;Make sure to go all the way around                
                if xy[1] lt 0 then theta = (180.0 - theta) + 180.0

                ;Set the orientation
                (*self.text)[self.textmode[2]] -> SetProperty, ORIENTATION=theta, /DRAW
            endif
        endcase
        
        ;Button Up
        1: begin
            ;Turn off motion events
            if self.textmode[0] eq 4 $
                then self.textmode = [4, 0, -1] $
                else self.textmode = [2, 0, -1]
            self -> On_Off_Motion_Events, /OFF

            ;Redraw to get rid of ghost text
            self -> Draw
        endcase
    endcase
end


;+
;   Create a menu bar with various zoom options in it.
;
; :Params:
;       PARENT:             in, required, type=integer
;                           The widget ID of the parent widget for the new SaveAs Menu.
;-
pro MrAbstractText::Create_Text_Menu, parent
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Create the text menu
    textID = widget_button(parent, VALUE='Text', /MENU)
    button = widget_button(textID, VALUE='Add Text', /CHECKED_MENU, UVALUE={object: self, method: 'Text_Menu_Events'})
    button = widget_button(textID, VALUE='Move Text', /CHECKED_MENU, UVALUE={object: self, method: 'Text_Menu_Events'})
    button = widget_button(textID, VALUE='Rotate Text', /CHECKED_MENU, UVALUE={object: self, method: 'Text_Menu_Events'})
    button = widget_button(textID, VALUE='Remove Text', /CHECKED_MENU, UVALUE={object: self, method: 'Text_Menu_Events'})
    button = widget_button(textID, VALUE='None', UNAME='TextNone', /CHECKED_MENU, UVALUE={object: self, method: 'Text_Menu_Events'})

end


;+
;   The purpose of this method is to draw the over-plots in the draw window.
;-
pro MrAbstractText::Draw
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Call each of the OverPlot's draw methods. Since these are over plots, the
    ;device should already be configured by a previous call to Plot.
    for i = 0, n_elements(*self.text) - 1 do (*self.text)[i] -> Draw
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
pro MrAbstractText::Draw_Events, event
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
        if self.textmode[0] eq 1 then self -> Place_Text, event     ;Add text
        if self.textmode[0] eq 2 then self -> Move_Text, event      ;Move text
        if self.textmode[0] eq 4 then self -> Rotate_Text, event    ;Rotate text
        if self.textmode[0] eq 8 then self -> Remove_Text, event    ;Remove text
    endif

;---------------------------------------------------------------------
;Motion Events ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 2 then begin
        ;Do not compete for copying the pixmap
        if ( self.textmode[1] eq 1 && ( self.textmode[0] eq 2 || $      ;Move Text
                                        self.textmode[0] eq 4 ) ) $     ;Rotate Text
            then self -> copyPixmap
        
        ;Annotate Mode
        if (self.textmode)[1] eq 1 then begin
            if self.textmode[0] eq 2 then self -> Adjust_Text, event    ;Move Text
            if self.textmode[0] eq 4 then self -> Adjust_Text, event    ;Rotate Text
        endif
    endif
    
;---------------------------------------------------------------------
;Button Release Events ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 1 then begin
        if self.textmode[1] eq 1 then begin ;Button Up turns motion events off.
            if self.textmode[0] eq 2 then self -> Move_Text, event
            if self.textmode[0] eq 4 then self -> Rotate_Text, event
        endif
    endif
end


;+
;   Serve as a general error handler for event handling routines.
;-
pro MrAbstractText::Error_Handler
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Turn off motion and button events. Reset the mode indicator.
    self.textmode = [0, 0, -1]
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
pro MrAbstractText::On_Off_Button_Events, $
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
        if (self.textmode[0] eq 0) then widget_control, self.drawID, DRAW_BUTTON_EVENTS=0
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
pro MrAbstractText::On_Off_Motion_Events, $
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
        if (self.textmode[1] eq 0) then begin
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
pro MrAbstractText::Place_Text, event
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
    
    ;Spawn a GUI to get text string and color. Store in annotate menu.
    pText = text_gui(self.tlb)
    if (*pText).cancel eq 1 then return
    
    ;Add the text.
    color = cgColor((*pText).color)
    self -> Text, event.x, event.y, (*pText).text, COLOR=color, /DEVICE, /DRAW
    
    ;Free the pointer, end text placement, and turn off button events.
    ptr_free, pText
    self.textmode = [0, 0, -1]
    self -> On_Off_Button_Events, /OFF
end


;+
;   Create a weText object to be overplotted onto the graph.
;
; :Params:
;       XLOC:               in, optional, type=depends
;                           The X location of the axis. If `PLACE` is set, then this is
;                               the text to be drawn on the plot.
;       YLOC:               in, optional, type=depends
;                           The Y location of the axis.
;       TEXT:               in, optional, type=string
;                           The text to be put on the axis.
;
; :Keywords:
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the overplot to the list.
;       OUTLOC:             out, optional, type=fltarr(2)
;                           If `PLACE` is set, then this will return the location at which
;                               the text was placed in the window.
;       PLACE:              in, optional, type=boolean, default=0
;                           Indicate that you want to click on the plot in order to
;                               determine the location of the text. In this case, `XLOC`
;                               and `YLOC` are not given.
;       WIDTH:              out, optional, type=float
;                           A named variable into which the width of the text, in
;                               normalized units, will be returned.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weText__define or cgText is also
;                               accepted for keyword inheritance.
;-
pro MrAbstractText::Text, xloc, yloc, text, $
DRAW = draw, $
OUTLOC = outloc, $
PLACE = place, $
WIDTH = width, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;If PLACE is set, then no location was given. Shuffle
    if keyword_set(place) then text = temporary(xloc)

    ;Create a cgOverPlot object
    theText = obj_new('weText', xloc, yloc, text, PLACE=place, WIDTH=width, OUTLOC=outloc, $
                      _STRICT_EXTRA=extra)

    ;Add the text to the array of text objects
    self -> AddText, theText
    
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
pro MrAbstractText::Text_Menu_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Get the value of the button that cause the event
    widget_control, event.id, GET_VALUE=text_type
    text_type = strupcase(text_type)
    
    ;Get the button's siblings and uncheck them all
    parent = widget_info(event.id, /PARENT)
    kids = widget_info(parent, /ALL_CHILDREN)
    for i = 0, n_elements(kids) - 1 do widget_control, kids[i], SET_BUTTON=0
    
    ;Toggle the checked state of the menu button
    isSet = widget_info(event.id, /BUTTON_SET)
    isSet = ~isSet
    
    case text_type of
        'NONE': begin
            ;Turn everything off and copy the pixmap
            self.textmode = [0, 0, -1]
            self -> On_Off_Button_Events, /OFF
            self -> On_Off_Motion_Events, /OFF
            self -> copyPixmap
            
            ;Get the button's siblings and uncheck them all
            parent = widget_info(event.id, /PARENT)
            kids = widget_info(parent, /ALL_CHILDREN)
            for i = 0, n_elements(kids) - 1 do widget_control, kids[i], SET_BUTTON=0
        end
    
        'ADD TEXT': begin
            ;Turn on button events
            self.textmode = [1, 0, -1]
            self -> On_Off_Button_Events, /ON
        endcase
        
        'MOVE TEXT': begin
            ;Turn on button events
            if isSet then begin
                self.textmode = [2, 0, -1]
                self -> On_Off_Button_Events, /ON
                
            ;If the button was turned off
            endif else begin
                self.textmode = [0, 0, -1]
                self -> On_Off_Button_Events, /OFF
            endelse
        endcase
        
        'ROTATE TEXT': begin
            ;If the button was turned on
            if isSet then begin
                self.textmode = [4, 0, -1]
                self -> On_Off_Button_Events, /ON
                
            ;If the button was turned off
            endif else begin
                self.textmode = [0, 0, -1]
                self -> On_Off_Button_Events, /OFF
            endelse
        endcase
        
        'REMOVE TEXT': begin
            ;If the button was turned on
            self.textmode = [8, 0, -1]
            self -> On_Off_Button_Events, /ON
        endcase
        
        else: message, 'Button "' + text_type + '" unknown.'
    endcase
    
    ;Put a check mark by the button
    if text_type ne 'ADD TEXT' and text_type ne 'REMOVE TEXT' and text_type ne 'NONE' $
        then widget_control, event.id, SET_BUTTON=isSet
end


;+
;   This method retrieves the positions of each text.
;
; :Keywords:
;       BOX:                in, optional, type=boolean, default=0
;                           If set, `TEXTPOS` is the coordinates of the lower-left and
;                               upper-right corners of a box surrounding each text item.
;                               It will be a 4xN array, where N is the number of texts. If
;                               no texts are available, then -1 will be returned.
;       SYSTEM:             out, optional, type=string
;                           The coordinate system of `TEXTPOS`. This is one of "DATA", 
;                               "DEVICE" or "NORMAL".
;
; :Returns:
;       TEXTPOS:            The [X, Y] location of each text item. TEXTPOS is a 2xN array,
;                               where N is the number of text objects present.
;-
function MrAbstractText::Text_Positions, $
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
    nTexts = n_elements(*self.text)
    if nTexts eq 0 then return, -1
    
    ;Get the coordinate system?
    get_system = arg_present(system)
    if get_system then system = strarr(nTexts)
    
    ;Allocate memory
    if keyword_set(box) $
        then textPos = intarr(4, nTexts) $
        else textPos = intarr(2, nTexts)
    
    ;Get the location of the texts.
    for i = 0, nTexts - 1 do begin
        (*self.text)[i] -> getProperty, XLOC=xloc, YLOC=yloc, WIDTH=width, $
                                        CHARSIZE=charsize, DEVICE=device, NORMAL=normal, DATA=data
        
        ;Determine which coordinate system is being used?
        if get_system then begin
            if keyword_set(normal) then system[i] = 'Normal'
            if keyword_set(device) then system[i] = 'Device'
            if keyword_set(data) then system[i] = 'Data'
            
            ;The default is data coordinates
            if system[i] eq '' then system[i] = 'Data'
        endif
        
        ;Create a box of positions?
        if keyword_set(box) then begin
            if n_elements(charsize) eq 0 then charsize = 1.0
            
            textPos[0,i] = xloc - (width*!d.x_size)
            textPos[1,i] = yloc - !d.y_ch_size*charsize
            textPos[2,i] = xloc + (width*!d.x_size)
            textPos[3,i] = yloc + !d.y_ch_size*charsize
            
        ;Return the location
        endif else begin
            textPos[*,i] = [xloc, yloc]
        endelse
    endfor
    
    return, textPos
end


;+
;   Event hanlder for placing new text on the display.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractText::Remove_Text, event
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
    if self.textmode[0] ne 8 then return
    
    ;Get the text positions
    textPos = self -> Text_Positions(/BOX)
            
    ;Find the text that is closest
    thisText = where(textPos[0,*] le event.x and textPos[2,*] ge event.x and $
                     textPos[1,*] le event.y and textPos[3,*] ge event.y, count)

    ;If no text exists at that location, then return
    if count eq 0 then return

    ;Remove the text object
    self -> AddText, INDEX=thisText, /REMOVE, /DRAW
    
    ;Turn off Button Events
    self.textmode = [0, 0, -1]
    self -> On_Off_Button_Events, /OFF
    
end


;+
;   This method provides a means of determining the index location at which each
;   Text is stored. Texts are distinguished by their titles.
;-
PRO MrAbstractText::whichText
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ;Allow the Superclass to have a space flush with the left of the display window
    Print, '  Axes:'
    
    ;Print the color bar indices and titles
    FOR j = 0, N_Elements(*self.text) - 1 DO BEGIN
        (*self.text)[j] -> GetProperty, STRING=text
        if n_elements(text) eq 0 then text = '""'
        index = String(j, FORMAT='(i3)')

        Print, '    Index: ' + index + '    Text: ' + text
    ENDFOR
END


;+
;   This method cleans up after the object is destroyed.
;-
pro MrAbstractText::cleanup
    compile_opt idl2
    
    ;Destroy all weOverPlot objects
    if ptr_valid(self.text) then begin
        for i = 0, n_elements(*self.text)-1 do begin
            obj_destroy, (*self.text)[i]
        endfor
        ptr_free, self.text
    endif
end


;+
;   The initialization method. Because MrAbstractText is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractText object will result
;   in an error.
;-
function MrAbstractText::init
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
pro MrAbstractText__define
    compile_opt idl2
    
    class = {MrAbstractText, $
             text: ptr_new(), $             ;Array of weText objects
             textmode: [0, 0, 0]}           ;[text mode, active, index]
end