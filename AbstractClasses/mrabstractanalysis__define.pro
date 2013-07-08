; docformat = 'rst'
;
; NAME:
;       MrAbstractAnalysis__Define
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
;           allObjects:         An array containing the object references of each plot
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
;   ANALYZING::
;       - Button and/or Motion events must be turned on for the draw widget.
;       - CMODE must be set to any combination of valid cursor bits (see below).
;       - To turn on/off button or motion events::
;           * Use On_Off_Button_Events and On_Off_Motion_Events to turn on and off
;             button and motion events, respectively. See those methods for rules governing
;             'ON' and 'OFF' and the 'Draw_Events' method for the sequences of events.
;
;   ANALYSIS MENU::
;       1) Create a top level base containing a menu bar and a draw widget.
;           a) For the Show_XY method to work, a widget label with ID statusID must
;               be present as well.
;       2) Pass the menu bar's widget ID to the Create_Cursor_Menu.
;       3) Event handling for the menu is done internally by the Cursor_Menu_Events method.
;       4) Draw widget event handling must be configured as described above.
;
;   Cursor Options::
;       Get Data Point
;       Get Interval
;       Average
;       MVAB
;       None
;
;   Cursor Bits::
;       0   -   None
;       1   -   Get Value
;       2   -   Get Interval
;       4+2 -   Average
;       8+2 -   MVAB
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
;       07/07/2013  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Determine which "Analsyis Menu" button was pressed and tell the draw widget which
;   type of events to pay attention to.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractAnalysis::Analysis_Menu_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Get the value of the button that cause the event
    widget_control, event.id, GET_VALUE=analysis_type
    
    ;Toggle the checked state of the menu button
    isSet = widget_info(event.id, /BUTTON_SET)
    isSet = ~isSet

    ;Get the button's siblings and uncheck them all
    if isSet eq 0 then begin
        parent = widget_info(event.id, /PARENT)
        kids = widget_info(parent, /ALL_CHILDREN)
        for i = 0, n_elements(kids) - 1 do widget_control, kids[i], SET_BUTTON=0
    endif
    
    case strupcase(analysis_type) of
        'NONE': begin
            ;Turn everything off and copy the pixmap
            self.amode = [0, 0, 0]
            self -> On_Off_Button_Events, /OFF
            self -> On_Off_Motion_Events, /OFF
            return
        endcase
        
        'GET DATA VALUE': begin
            if isSet then begin
                self.amode = [1, 0, 0]
                self -> On_Off_Button_Events, /ON
            endif else begin
                self.amode = [0, 0, 0]
                self -> On_Off_Button_Events, /OFF
            endelse
        endcase
        
        'GET INTERVAL': begin
            if isSet then begin
                self.amode = [2, 0, 0]
                self -> On_Off_Button_Events, /ON
            endif else begin
                self.amode = [0, 0, 0]
                self -> On_Off_Button_Events, /OFF
            endelse
        endcase
        
        'AVERAGE': begin
            if isSet then begin
                self.amode = [4 + 2, 0, 0]
                self -> On_Off_Button_Events, /ON
            endif else begin
                self.amode = [0, 0, 0]
                self -> On_Off_Button_Events, /OFF
            endelse
        endcase
        
        'MVAB': begin
            if isSet then begin
                self.amode = [8 + 2, 0, 0]
                self -> On_Off_Button_Events, /ON
            endif else begin
                self.amode = [0, 0, 0]
                self -> On_Off_Button_Events, /OFF
            endelse
        endcase
                
        else: message, 'Button "' + analysis_type + '" unknown.'
    endcase

    ;Put a check mark by the button
    widget_control, event.id, SET_BUTTON=isSet
end



;+
;   The average value in a given interval.
;
; :Params:
;       X:                  in, required, type=numeric
;                           Coordinate on the abscissa for which the nearest data point is
;                               to be found.
;       Y:                  in, required, type=numeric
;                           Coordinate on the ordinate for which the nearest data point is
;                               to be found.
;
; :Returns:
;       XY_DATA:            The closest data values to `X` and `Y`.
;-
pro MrAbstractAnalysis::Average, xrange, yrange
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Get the data interval
    !Null = self -> Data_Range(xrange, yrange, INDEX=index)

    ;Retrieve the data and the dimension over which to take the mean
    (*self.allObjects)[self.ifocus] -> GetProperty, INDEP=indep, DEP=dep, DIMENSION=dimension
    if n_elements(dimension) eq 0 then dimension = 0
    
    x_avg = mean(indep[index[0]:index[1]])
    
    case dimension of
        0: y_avg = mean(dep[index[0]:index[1]])
        1: y_avg = mean(dep[index[0]:index[1], *])
        2: y_avg = mean(dep[*, index[0]:index[1]])
        else: message, 'Undetermined number of dimensions. Cannot take average.'
    endcase
    
    ;Print the results
    print, FORMAT='(%"Average = [%f, %f]")', x_avg, y_avg
end


;+
;   Create a menu bar with various analysis options in it.
;
; :Params:
;       PARENT:             in, required, type=integer
;                           The widget ID of the parent widget.
;
; :Keywords:
;       GET_DATA_VALUE:     in, optional, type=boolean, default=1
;                           Create the "Get Data Value" button.
;       GET_INTERVAL:       in, optional, type=boolean, default=1
;                           Create the "Get Interval" button.
;       AVERAGE:            in, optional, type=boolean, default=1
;                           Create the "Average" button.
;       MENU:               in, optional, type=boolean, default=1
;                           If set, all buttons will be placed under a "Analysis" submenu.
;       MVAB:               in, optional, type=boolean, default=1
;                           Create the "MVAB" button.
;       NONE:               in, optional, type=boolean, default=1
;                           Create the "None" button.
;-
pro MrAbstractAnalysis::Create_Analysis_Menu, parent, $
AVERAGE = average, $
GET_INTERVAL = get_interval, $
MENU = menu, $
MVAB = mvab, $
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
    setDefaultValue, get_data_value, 1, /BOOLEAN
    setDefaultValue, get_interval, 1, /BOOLEAN
    setDefaultValue, average, 1, /BOOLEAN
    setDefaultValue, mvab, 1, /BOOLEAN
    setDefaultValue, none, 1, /BOOLEAN
    
    ;Create the Menu
    if keyword_set(menu) $
        then cursorID = widget_button(parent, VALUE='Analysis', /MENU) $
        else cursorID = parent
    
    ;Create the menu
    if keyword_set(get_data_value) then button = widget_button(cursorID, VALUE='Get Data Value', UNAME='GET_DATA_VALUE', /CHECKED_MENU, UVALUE={object: self, method: 'Analysis_Menu_Events'})
    if keyword_set(get_interval) then button = widget_button(cursorID, VALUE='Get Interval', UNAME='GET_INTERVAL', /CHECKED_MENU, UVALUE={object: self, method: 'Analysis_Menu_Events'})
    if keyword_set(average) then button = widget_button(cursorID, VALUE='Average', UNAME='AVERAGE', /CHECKED_MENU, UVALUE={object: self, method: 'Analysis_Menu_Events'})
    if keyword_set(mvab) then button = widget_button(cursorID, VALUE='MVAB', UNAME='MVAB', /CHECKED_MENU, UVALUE={object: self, method: 'Analysis_Menu_Events'})
    if keyword_set(none) then button = widget_button(cursorID, VALUE='None', UNAME='ANONE', UVALUE={object: self, method: 'Analysis_Menu_Events'})
end


;+
;   Find the nearest value to the point [x, y], given in data coordinates.
;
; :Params:
;       XRANGE:             in, required, type=numeric
;                           A range of coordinate on the abscissa for which the data
;                               interval is to be returned
;       YRANGE:             in, required, type=numeric
;                           A range of coordinate on the ordinate for which the data
;                               interval is to be returned
;
; :Keywords:
;       INDEX:              out, optional, type=int
;                           Index value within the data array at which to find `XY_DATA`.
;                               it is ordered [sIndex, eIndex], where "s" and "e" denote
;                               the start and end of the data interval, respectively.
;
; :Returns:
;       XY_DATA:            The range of data corresponding to the coordinates `XRANGE`
;                               and `YRANGE`. It is ordered [x0, y0, x1, y1].
;-
function MrAbstractAnalysis::Data_Range, xrange, yrange, $
INDEX = index
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, !Null
    endif
    
    ;Retrieve the data and the dimension over which to take the mean
    (*self.allObjects)[self.ifocus] -> GetProperty, INDEP=indep, DEP=dep, DIMENSION=dimension
    if n_elements(dimension) eq 0 then dimension = 0
    
    ;Find the closest data point
    !Null = min(abs(indep - xrange[0]), sIndex)
    !Null = min(abs(indep - xrange[1]), eIndex)

    ;Store them
    x_data = indep[[sIndex, eIndex]]
    y_data = dep[[sIndex, eIndex]]
    
    ;Arrange them
    xy_data = [x_data[0], y_data[0], x_data[1], y_data[1]]
    index = [sIndex, eIndex]
    
    return, xy_data
end


;+
;   Handle events from the draw widget
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractAnalysis::Draw_Events, event
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
        if self.amode eq 1 then self -> Get_Data_Point, event ;Get Interval, Average
        if ((self.amode and 2) gt 0) then self -> Get_Interval, event   ;Get Interval, Average, MVAB
    endif

;---------------------------------------------------------------------
;Motion Events ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 2 then begin
        ;Do not compete for copying the pixmap: Get Interval
        if ((self.amode[0] and 2) gt 0) then self -> copyPixmap

        ;Handle motion events
        if self.amode[0] eq 1 then self -> Get_Data_Point, event
        if ((self.amode[0] and 2) gt 0) then self -> Get_Interval, event
    endif
end


;+
;   Determine which "Analsyis Menu" button was pressed and tell the draw widget which
;   type of events to pay attention to.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractAnalysis::Error_Handler, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    self.amode = [0,0,0]
    self -> On_Off_Button_Events, /OFF
    self -> On_Off_Motion_Events, /OFf
    self.x0 = -1
    self.y0 = -1
end



;+
;   Click and Drag the mouse to rubber band box in the display. When the mouse button
;   is released, the plot will be updated to the range encompassed by the box.
;
;   If the x- or y-coordinate does not change (i.e. the box is still a line), the zoom
;   will be 1-dimensional.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractAnalysis::Get_Interval, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = error_message()
        return
    endif

    ;Return if we are not trying to "Get Interval"
    case event.type of
        0: if event.press ne 1 || ((self.amode[0] and 2) eq 0) then return      ;left button down
        1: if ((self.amode[0] and 2) eq 0) then return                          ;button up
        2: if self.amode[1] eq 0 || ((self.amode[0] and 2) eq 0) then return    ;motion
        else: return
    endcase

    ;If the button was pressed
    case event.type of
        0: begin    ;Button down
            ;Make active for motion events
            self.amode[1] = 1
        
            ;Store the clicked coordinates
            self.x0 = event.x
            self.y0 = event.y

            ;Turn on motion events
            self -> On_Off_Motion_Events, /ON
        endcase
        
        2: begin    ;Motion event
            ;Draw a |------| to indicate the interval being taken
            x_right_bar = [self.x0, self.x0, self.x0]
            y_right_bar = [self.y0+3, self.y0, self.y0-3]
            
            x_left_bar = [event.x, event.x, event.x]
            y_left_bar = [event.y+3, event.y, event.y-3]
            
            x_stem = [self.x0, event.x]
            y_stem = [self.y0, event.y]
            
            plots, x_right_bar, y_right_bar, color=load_color('blue'), /DEVICE
            plots, x_left_bar, y_left_bar, color=load_color('blue'), /DEVICE
            plots, x_stem, y_stem, color=load_color('blue'), /DEVICE
        endcase
        
        1: begin    ;Button up
            ;Turn off Box Zoom.
            self.amode[1] = [0]
            
            ;Turn motion events off right away (requires self.zmode=0)
            self -> On_Off_Motion_Events, /OFF

            ;Get rid of the box by copying the pixmap again
            self -> copyPixmap

            ;Order the clicks as [min, max]. Convert to data
            x = [self.x0 < event.x, self.x0 > event.x]
            y = [self.y0 < event.y, self.y0 > event.y]

            ;Convert from device to data coordinates
            xy = convert_coord(x, y, /DEVICE, /TO_DATA)
            xrange = reform(xy[0,*])
            yrange = reform(xy[1,*])
            
            ;Forward results to proper analysis method
            if self.amode[0] eq 2 then self -> Interval, xrange, yrange
            if ((self.amode[0] and 4) gt 0) then self -> Average, xrange, yrange
            if ((self.amode[0] and 8) gt 0) then self -> MVAB, xrange, yrange
            
            ;reset initial click
            self.x0 = -1
            self.y0 = -1
        endcase
    endcase
end


;+
;   The average value in a given interval.
;
; :Params:
;       X:                  in, required, type=numeric
;                           Coordinate on the abscissa for which the nearest data point is
;                               to be found.
;       Y:                  in, required, type=numeric
;                           Coordinate on the ordinate for which the nearest data point is
;                               to be found.
;
; :Returns:
;       XY_DATA:            The closest data values to `X` and `Y`.
;-
pro MrAbstractAnalysis::Interval, xrange, yrange
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Get the data range.
    xy_data = self -> Data_Range(xrange, yrange)
    
    ;Print the results
    print, '            -x-       -y-'
    print, FORMAT='(%"Start = [%f, %f]")', xy_data[0:1]
    print, FORMAT='(%"End   = [%f, %f]")', xy_data[2:3]
end


;+
;   Retrieve the coordinates of the mouse click and find the nearest data point.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractAnalysis::Get_Data_Point, event
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
    
    ;Retrieve the data and the dimension over which to take the mean
    (*self.allObjects)[self.ifocus] -> GetProperty, INDEP=indep, DEP=dep, DIMENSION=dimension
    if n_elements(dimension) eq 0 then dimension = 0
    
    ;Find the closest data point
    !Null = min(abs(indep - coords[0]), index)

    ;Print the closest data point.
    print, FORMAT='(%"Data Value: [%f, %f]")', indep[index], dep[index]
end


;+
;   The average value in a given interval.
;
; :Params:
;       X:                  in, required, type=numeric
;                           Coordinate on the abscissa for which the nearest data point is
;                               to be found.
;       Y:                  in, required, type=numeric
;                           Coordinate on the ordinate for which the nearest data point is
;                               to be found.
;
; :Returns:
;       XY_DATA:            The closest data values to `X` and `Y`.
;-
pro MrAbstractAnalysis::MVAB, xrange, yrange
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Get the data interval
    xy_data = self -> Data_Range(xrange, yrange, INDEX=index)

    ;Retrieve the data and the dimension over which to take the mean
    (*self.allObjects)[self.ifocus] -> GetProperty, INDEP=indep, DEP=dep, DIMENSION=dimension
    if n_elements(dimension) eq 0 then begin
        dims = size(dep, /DIMENSIONS)
        dimension = where(dims eq 3, count) + 1
        if count eq 0 then message, 'Data must be 3xN to perform MVAB.'
    endif
    
    ;perform MVA
    case dimension of
        1: eigvecs = mva(indep[index[0]:index[1]], dep[*,index[0]:index[1]], EIGVALS=eigvals)
        2: eigvecs = mva(indep[index[0]:index[1]], transpose(dep[index[0]:index[1],*]), EIGVALS=eigvals)
        else: message, 'Data must be a 3xN or Nx3 array.'
    endcase
        
    ;print the results of MVAB to the command window
    nlm = ['N', 'M', 'L']
    print, '________________________________________________________________'
    print, 'MVAB eigenvalues and eigenvectors'

    print, 'Start time: ' + ssm_to_hms(xy_data[0])
    print, 'End time:   ' + ssm_to_hms(xy_data[2])
    print, 'l', 'x', 'y', 'z', format='(4(9x, a1))'
    for i=0, 2 do print, strtrim(string(nlm[i], eigvals[i], eigvecs[*,i], $
                                        format='(a1, 2x, f10.4, 3x, 3(f7.4, 3x))'), 1)
    
    ;print again in idl format easy copy and paste
    print, '________________________________________________________________'
    print, 'IDL Format for Copy + Paste'
    print, "tstart = '" + ssm_to_hms(xy_data[0]) + "'"
    print, "tend =   '" + ssm_to_hms(xy_data[2]) + "'"
    print, format='(a0, 3(f10.4, a0))', $
           'eigvals = [', eigvals[0], ', ', eigvals[1], ', ', eigvals[2], ']'
    print, format='(3(a0, f7.4), a0)', $
           'eigvecs = [[', eigvecs[0,0], ', ', eigvecs[1,0], ', ', eigvecs[2,0], '], $', $
           '           [', eigvecs[0,1], ', ', eigvecs[1,1], ', ', eigvecs[2,1], '], $', $
           '           [', eigvecs[0,2], ', ', eigvecs[1,2], ', ', eigvecs[2,2], ']]'
    
    ;reset the draw widget's user value
    self.tmatrix = eigvecs
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
pro MrAbstractAnalysis::On_Off_Button_Events, $
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
        
        ;Turn motion events off only if nothing else needs them. All analysis modes
        ;(thus far) depend on Get_Interval, so just check for that.
        if (self.amode[1] eq 0) then $
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
pro MrAbstractAnalysis::On_Off_Motion_Events, $
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
        
        ;Turn motion events off only if nothing else needs them. Check if the "active"
        ;flag is set.
        
        ;Cross Hairs, Show [X,Y]
        if self.amode[1] eq 0 then begin
            widget_control, self.drawID, DRAW_MOTION_EVENTS=0
            widget_control, self.drawID, /CLEAR_EVENTS
        endif
    endif
end


;+
;   Clean up after the object is destroy
;-
pro MrAbstractAnalysis::cleanup
    ;Nothing to clean up.
end


;+
;   The initialization method. Because MrAbstractAnalysis is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractAnalysis object will result
;   in an error.
;-
function MrAbstractAnalysis::init
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
pro MrAbstractAnalysis__define, class
    compile_opt idl2
    
    class = {MrAbstractAnalysis, $
             amode: intarr(3), $            ;[text mode, active, npts]
             tmatrix: fltarr(3,3) $         ;Coordinate transformation matrix
            }
end