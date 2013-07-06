; docformat = 'rst'
;
; NAME:
;       MrAbstractZoom__Define
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
;   The purpose of this method is to serve as an abstract class zooming in a draw widget.
;   A widget menu can be created via the Create_Zoom_Menu method that provides an
;   interface with the various zooming modes and methods.
;
;   The right and left mouse buttons can be configured to have different zoom functions.
;   The zoomfactor can be different for the x- and y- ranges. Only one zoom mode can be
;   active at a time.
;
;   SETUP::
;       Subclass must have the following properties::
;           ifocus:             The index within "plotObjects" of the plot-of-interest
;           pixID:              Window ID of the pixmap window
;           plotObjects:        A pointer to a plot object or array of plot objects.
;           winID:              Window ID of the display window
;           x0:                 Place holder for x-position of cursor
;           y0:                 Place holder for y-position of cursor
;
;       Subclass must have the following methods::
;           copyPixmap:         Method for copying the pixmap (pixID) to the display (winID)
;           Draw:               Method for drawing plots, images, etc.
;           Draw_Events:        Event handling method for the draw widget.
;
;       Plot objects must have the following properties::
;           imrange:            (optional) The range of the image.
;           xrange:             The x-range of the plot or image.
;           yrange:             The y-range of the plot or image.
;           init_imrange:       (optional) The initial image range (for unzooming)
;           init_xrange:        The initial x-range (for unzooming)
;           init_yrange:        The initial y-range (for unzooming)
;
;       Set the draw widget UValue and Event Procedure::
;           UVALUE = {object: self, method: 'Draw_Events'}
;           EVENT_PRO = 'your_event_handling_procedure_goes_here'
;
;       In the procedure specified by EVENT_PRO, use the Call_Method procedure::
;           Widget_Control, self.drawID, GET_UVALUE=event_handler
;           Call_Method, event_handler.method, event_handler.object, event
;
;   ZOOMING::
;       - Button events must be turned on for the draw widget.
;       - LMODE and/or RMODE must be set to a valid zoom bit (see below).
;       - Motion events are turned on and off when needed.
;           * Use On_Off_Button_Events and On_Off_Motion_Events to turn on and off
;             button and motion events, respectively. See those methods rules governing
;             'ON' and 'OFF' and the 'Draw_Events' method for the sequences of events.
;
;   ZOOM MENU::
;       1) Create a top level base containing a menu bar and a draw widget.
;       2) Pass the menu bar's widget ID to the Create_Zoom_Menu.
;       3) Event handling for the menu is done internally by the Zoom_Menu_Events method.
;       4) Draw widget event handling must be configured as described above.
;
;   Zoom Options::
;       Box Zoom    -   Click on the plot and drag the mouse to draw a rubber-band box
;                       in the display window. When the mouse button is released, the
;                       x- and y-ranges will be updated to e
;       Zoom X      -   Click once to set the minimum and a second time to set the maximum
;                       of the x-range.
;       Zoom Y      -   Click once to set the minimum and a second time to set the maximum
;                       of the y-range.
;       Pan         -   Click and drag the mouse to pan the data in any direction.
;       Wheel Zoom  -   Roll the wheel forward and backward to zoom in the x- and y-
;                       direction by the zoomfactor. The zoomfactor can be different
;                       for x and y.
;
;   Zoom Bits::
;       0       -   No zoom effects
;       1       -   X Zoom
;       2       -   Y Zoom
;       4       -   Box Zoom
;       8       -   Pan
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
;       05/26/2013  -   Added the Bind, UnBind, Consolidate_Bindings, and Apply_Bindings
;                           methods. - MRA
;       05/27/2013  -   Updated the Pan, Box_Zoom, XY_Zoom, and Wheel_Zoom methods to
;                           use the plotObjects and ifocus properties. The latter two
;                           properties are assumed to be present in the object that
;                           inherits MrAbstractZoom. Incorporated bindings into the zoom
;                           options. Renamed Wheel_Zoom to Wheel_XY_Zoom and added
;                           Wheel_Color_Zoom. - MRA
;-
;*****************************************************************************************
;+
;   Bind the axis of one object to the axis of another so that the zooms are synchronized.
;
; :Params:
;       THEOBJECT:          in, required, type=object
;                           Check to see if anything is bound to this object. If so,
;                               make all bound objects have the same range as THEOBJECT.
;                               Which ranges are updated depend on which keywords are set.
;
; :Keywords:
;       ALL:                in, optional, type=boolean, default=0
;                           Update the ranges for all axis bindings.
;       CAXIS:              in, optional, type=boolean, default=0
;                           Update the ranges for color axis bindings.
;       XAXIS:              in, optional, type=boolean, default=0
;                           Update the ranges for x-axis bindings.
;       YAXIS:              in, optional, type=boolean, default=0
;                           Update the ranges for y-axis bindings.
;       ZAXIS:              in, optional, type=boolean, default=0
;                           Update the ranges for z-axis bindings.
;       
;-
pro MrAbstractZoom::Apply_Bindings, theObject, $
ALL=all, $
CAXIS = caxis, $
XAXIS = xaxis, $
YAXIS = yaxis, $
ZAXIS = zaxis
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;If ALL is set, then bind all axes.
    if keyword_set(all) then begin
        caxis = 1
        xaxis = 1
        yaxis = 1
        zaxis = 1
    endif


;---------------------------------------------------------------------
;UPDATE X-BINDINGS ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    if obj_valid(self.bind_x) and keyword_set(xaxis) ne 0 then begin    
        ;Get the total number of bindings
        nBindings = self.bind_x -> Get_Count()
        i = 0
        count = 0

        ;Go through each binding
        while count eq 0 and i lt nBindings do begin
            ;until we find one that contains theObject
            thisBinding = self.bind_x -> Get_Item(i)
            iBound = where(thisBinding eq theObject, count)
        
            if count eq 0 then begin
                i += 1
                continue
            endif
            
            ;Get the range to be set.
            thisBinding[iBound] -> GetProperty, XRANGE=xrange
        
            ;Set the range of each plot that is bound to theObject
            for i = 0, n_elements(thisBinding) - 1 $
                do thisBinding[i] -> setProperty, XRANGE=xrange
                
        endwhile
    endif

;---------------------------------------------------------------------
;UPDATE Y-BINDINGS ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Do the same for y
    if obj_valid(self.bind_y) and keyword_set(yaxis) ne 0 then begin    
        nBindings = self.bind_y -> Get_Count()
        i = 0
        count = 0
    
        while count eq 0 and i lt nBindings do begin
            thisBinding = self.bind_y -> Get_Item(i)
            iBound = where(thisBinding eq theObject, count)
        
            if count eq 0 then begin
                i += 1
                continue
            endif
            
            thisBinding[iBound] -> GetProperty, YRANGE=yrange
            for i = 0, n_elements(thisBinding) - 1 $
                do thisBinding[i] -> setProperty, YRANGE=yrange
                
        endwhile
    endif

;---------------------------------------------------------------------
;UPDATE Y-BINDINGS ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Do the same for z
    if obj_valid(self.bind_z) and keyword_set(zaxis) ne 0 then begin    
        nBindings = self.bind_z -> Get_Count()
        i = 0
        count = 0
    
        while count eq 0 and i lt nBindings do begin
            thisBinding = self.bind_z -> Get_Item(i)
            iBound = where(thisBinding eq theObject, count)
        
            if count eq 0 then begin
                i += 1
                continue
            endif
        
            thisBinding[iBound] -> GetProperty, ZRANGE=zrange
            for i = 0, n_elements(thisBinding) - 1 $
                do thisBinding[i] -> setProperty, ZRANGE=zrange
                
        endwhile
    endif

;---------------------------------------------------------------------
;UPDATE COLOR RANGE BINDINGS /////////////////////////////////////////
;---------------------------------------------------------------------
    ;Do the same for z
    if obj_valid(self.bind_c) and keyword_set(caxis) ne 0 then begin    
        nBindings = self.bind_c -> Get_Count()
        i = 0
        count = 0
    
        while count eq 0 and i lt nBindings do begin
            thisBinding = self.bind_c -> Get_Item(i)
            iBound = where(thisBinding eq theObject, count)
        
            if count eq 0 then begin
                i += 1
                continue
            endif
        
            thisBinding[iBound] -> GetProperty, RANGE=range
            for i = 0, n_elements(thisBinding) - 1 $
                do thisBinding[i] -> setProperty, RANGE=range
                
        endwhile
    endif
end


;+
;   Bind the axis of one object to the axis of another so that the zooms are synchronized.
;
; :Params:
;       OBJECT1:            in, required, type=object
;                           An object whose axis is to be bound to that of `OBJECT2`
;       OBJECT2:            in, required, type=object
;                           An object to which `OBJECT1`'s axis is to be bound.
;
; :Keywords:
;       ALL:                in, optional, type=boolean, default=0
;                           Bind all axes.
;       CAXIS:              in, optional, type=boolean, default=0
;                           Bind the color axis.
;       XAXIS:              in, optional, type=boolean, default=0
;                           Bind the xaxis.
;       YAXIS:              in, optional, type=boolean, default=0
;                           Bind the yaxis.
;       ZAXIS:              in, optional, type=boolean, default=0
;                           Bind the zaxis.
;-
pro MrAbstractZoom::Bind, object1, object2, $
ALL=all, $
CAXIS = caxis, $
XAXIS = xaxis, $
YAXIS = yaxis, $
ZAXIS = zaxis
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;If ALL is set, then bind all axes.
    if keyword_set(all) then begin
        caxis = 1
        xaxis = 1
        yaxis = 1
        zaxis = 1
    endif

;---------------------------------------------------------------------
;X-AXIS //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(xaxis) then begin
    
        ;If the pointer is not valid, make a pointer to a pointer array that links
        ;OBJECT1 to OBJECT2.
        if obj_valid(self.bind_x) eq 0 then begin
            self.bind_x = obj_new('linkedlist', [object1, object2])
            
        ;If the pointer is valid
        endif else begin
            
            ;See how many different bindings there are.
            nBindings = self.bind_x -> Get_Count()
            count = 0
            i = 0
            
            ;See if OBJECT2 is bound to anything.
            while count eq 0 and i lt nBindings do begin

                thisBinding = self.bind_x -> Get_Value(i)
                iBound = where(thisBinding eq object2, count)
                
                ;If it is
                if count ne 0 then begin
                    ;Check if the binding already exists.
                    iIsBound = where(thisBinding eq object1, isBound)
                    
                    ;If it does not, bind OBJECT1 to the group
                    if isBound eq 0 then begin
                        thisBinding = [thisBinding, object1]
                        self.bind_x -> Replace_Item, thisBinding, i, /NO_COPY
                    endif
                endif
                i++
            endwhile
            
            ;If OBJECT2 is not yet bound to anything, then COUNT will still be 0. This
            ;time, see if OBJECT1 is bound to anything.
            i = 0
            while count eq 0 and i lt nBindings do begin
                thisBinding = self.bind_x -> Get_Value(i)
                iBound = where(thisBinding eq object1, count)
                
                ;If it is, bind OBJECT2 to the group.
                if count ne 0 then begin
                    thisBinding = [thisBinding, object2]
                    self.bind_x -> Replace_Item, thisBinding, i, /NO_COPY
                endif
                i++
            endwhile
            
            ;If neither OBJECT1 nor OBJECT2 are currently bound to anything, create
            ;a new binding.
            if count eq 0 then self.bind_x -> Add, [object1, object2]

        endelse
        
        ;Consolidate Bindings
        self -> Consolidate_Bindings, /XAXIS
    endif
            
;---------------------------------------------------------------------
;Y-AXIS //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Do the same thing for the y-axis
    if keyword_set(yaxis) then begin
    
        if obj_valid(self.bind_y) eq 0 then begin
            self.bind_y = obj_new('linkedlist', [object1, object2])
            
        endif else begin
            nBindings = self.bind_y -> get_count()
            count = 0
            i = 0
            
            while count eq 0 and i lt nBindings do begin
                thisBinding = self.bind_y -> Get_Value(i)
                iBound = where(thisBinding eq object2, count)
                
                if count ne 0 then begin
                    iIsBound = where(bindingSet eq object1, isBound)
                    if isBound eq 0 then begin
                        thisBinding = [thisBinding, object1]
                        self.bind_y -> Replace_Item, thisBinding, i, /NO_COPY
                    endif
                endif
                i++
            endwhile
            
            i = 0
            while count eq 0 and i lt nBindings do begin
                thisBinding = self.bind_y -> Get_Value(i)
                iBound = where(thisBinding eq object1, count)
                
                if count ne 0 then begin
                    thisBinding = [thisBinding, object2]
                    self.bind_y -> Replace_Item, thisBinding, i, /NO_COPY
                endif
                i++
            endwhile
            
            if count eq 0 then self.bind_y -> Add, [object1, object2]
        endelse
        
        self -> Consolidate_Bindings, /YAXIS
    endif
        
;---------------------------------------------------------------------
;Z-AXIS //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Do the same thing for the z-axis
    if keyword_set(zaxis) then begin
    
        if obj_valid(self.bind_z) eq 0 then begin
            self.bind_z = obj_new('linkedlist', [object1, object2])
            
        endif else begin
            nBindings = self.bind_z -> get_count()
            count = 0
            i = 0
            
            while count eq 0 and i lt nBindings do begin
                thisBinding = self.bind_z -> Get_Value(i)
                iBound = where(thisBinding eq object2, count)
                
                if count ne 0 then begin
                    iIsBound = where(bindingSet eq object1, isBound)
                    if isBound eq 0 then begin
                        thisBinding = [thisBinding, object1]
                        self.bind_z -> Replace_Item, thisBinding, i, /NO_COPY
                    endif
                endif
                i++
            endwhile
            
            i = 0
            while count eq 0 and i lt nBindings do begin
                thisBinding = self.bind_z -> Get_Value(i)
                iBound = where(thisBinding eq object1, count)
                
                if count ne 0 then begin
                    thisBinding = [thisBinding, object2]
                    self.bind_z -> Replace_Item, thisBinding, i, /NO_COPY
                endif
                i++
            endwhile
            
            if count eq 0 then self.bind_z -> Add, [object1, object2]
        endelse
        
        self -> Consolidate_Bindings, /ZAXIS
    endif
        
;---------------------------------------------------------------------
;COLOR AXIS //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Do the same thing for the color axis
    if keyword_set(caxis) then begin
    
        if obj_valid(self.bind_c) eq 0 then begin
            self.bind_c = obj_new('linkedlist', [object1, object2])
            
        endif else begin
            nBindings = self.bind_c -> get_count()
            count = 0
            i = 0
            
            while count eq 0 and i lt nBindings do begin
                thisBinding = self.bind_c -> Get_Value(i)
                iBound = where(thisBinding eq object2, count)
                
                if count ne 0 then begin
                    iIsBound = where(bindingSet eq object1, isBound)
                    if isBound eq 0 then begin
                        thisBinding = [thisBinding, object1]
                        self.bind_c -> Replace_Item, thisBinding, i, /NO_COPY
                    endif
                endif
                i++
            endwhile
            
            i = 0
            while count eq 0 and i lt nBindings do begin
                thisBinding = self.bind_c -> Get_Value(i)
                iBound = where(thisBinding eq object1, count)
                
                if count ne 0 then begin
                    thisBinding = [thisBinding, object2]
                    self.bind_c -> Replace_Item, thisBinding, i, /NO_COPY
                endif
                i++
            endwhile
            
            if count eq 0 then self.bind_c -> Add, [object1, object2]
        endelse
        
        self -> Consolidate_Bindings, /CAXIS
    endif
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
pro MrAbstractZoom::Box_Zoom, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = error_message()
        return
    endif

    ;Return if we are not trying to Box Zoom
    case event.type of
        0: if event.press eq 1 and self.lmode ne 4 || $         ;left button down
              event.press eq 4 and self.rmode ne 4 then return  ;right button down
        1: if self.zmode ne 4 then return                       ;button up
        2: if self.zmode ne 4 then return                       ;motion
        else: return
    endcase

    ;If the button was pressed
    case event.type of
        0: begin    ;Button down
            ;Turn on Box Zoom.
            self.zmode = 4
        
            ;Store the clicked coordinates
            self.x0 = event.x
            self.y0 = event.y

            ;Turn on motion events
            self -> On_Off_Motion_Events, /ON
        endcase
        
        2: begin    ;Motion event
            ;Draw from the first clicked point to the current position
            x = [self.x0, self.x0, event.x, event.x, self.x0]
            y = [self.y0, event.y, event.y, self.y0, self.y0]
            plots, x, y, color=load_color('blue'), /DEVICE
        endcase
        
        1: begin    ;Button up
            ;Turn off Box Zoom.
            self.zmode = 0
            
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
            
            ;Only zoom if the coordinate changed
            if self.x0 eq event.x then void = temporary(xrange)
            if self.y0 eq event.y then void = temporary(yrange)
            
            ;Only update ranges and draw if points are different
            if self.x0 ne event.x || self.y0 ne event.y then begin
                theObj = (*self.plotObjects)[self.ifocus]
                theObj -> setProperty, XRANGE=xrange, YRANGE=yrange
                self -> Apply_Bindings, theObj, /xaxis, /yaxis
                self -> Draw
            endif
            
            ;reset initial click
            self.x0 = -1
            self.y0 = -1
        endcase
    endcase
end


;+
;   If an object is found in more than one set of bindings, then consolidate the
;   different sets into one.
;
; :Keywords:
;       ALL:                in, optional, type=boolean, default=0
;                           Bind all axes.
;       CAXIS:              in, optional, type=boolean, default=0
;                           Bind the color axis.
;       XAXIS:              in, optional, type=boolean, default=0
;                           Bind the xaxis.
;       YAXIS:              in, optional, type=boolean, default=0
;                           Bind the yaxis.
;       ZAXIS:              in, optional, type=boolean, default=0
;                           Bind the zaxis.
;
; :Uses:
;   Uses the following external programs::
;       error_message.pro (Coyote Graphics)
;       MrUniq.pro
;-
pro MrAbstractZoom::Consolidate_Bindings, $
ALL=all, $
CAXIS = caxis, $
XAXIS = xaxis, $
YAXIS = yaxis, $
ZAXIS = zaxis
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;If ALL is set, then consolidate all axes.
    if keyword_set(all) then begin
        caxis = 1
        xaxis = 1
        yaxis = 1
        zaxis = 1
    endif
    
;---------------------------------------------------------------------
;Consolidate X-Bindings //////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(xaxis) and ptr_valid(self.bind_x) then begin
        nBindings = self.bind_x -> Get_Count()
            
    ;---------------------------------------------------------------------
    ;One Set of Bindings /////////////////////////////////////////////////
    ;---------------------------------------------------------------------    
        if nBindings eq 1 then begin
            ;Select only the unique bindings
            thisBinding = self.bind_x -> Get_Item(0)
            thisBinding = thisBinding[MrUniq(thisBinding, /SORT)]
            
            ;If only one object is left, destroy the binding list
            if n_elements(thisBinding) le 1 then obj_destroy, self.bind_x
    
    ;---------------------------------------------------------------------
    ;Many Sets of Bindings ///////////////////////////////////////////////
    ;---------------------------------------------------------------------  
        endif else begin
            ;Make an array of all of the objects
            allObjects = self.bind_x -> Get_Item(0)
            for i = 1, nBindings - 1 do allObjects = [allObjects, self.bind_x -> Get_Item(i)]
            
            ;Find duplicates
            void = MrUniq(allObjects, /SORT, NCOMPLEMENT=ncopies, COMPLEMENT=icopies)
    
        ;---------------------------------------------------------------------
        ;Deal With Duplicates ////////////////////////////////////////////////
        ;---------------------------------------------------------------------
            if ncopies gt 0 then begin
                objToConsolicate = allObjects[icopies[0]]
                
                ;Step through all of the bindings
                i = 0
                iKeep = -1
                while i lt nBindings do begin
                    thisBinding = self.bind_x -> Get_Item(i)
                
                    ;Find the first instance of the duplicate object
                    if iKeep eq -1 then begin
                        void = where(thisBinding eq objToConsolidate, count)
                        if count ne 0 then iKeep = i
                        i += 1
                        
                    ;Find the second instance of the duplicate object
                    endif else begin
                        void = where(thisBinding eq objToConsolidate, count)
                        
                        ;When found
                        if count ne 0 then begin
                            ;Concatenate the two rows. Do not keep the duplicate.
                            thatBinding = self.bind_x -> Get_Item(iKeep)
                            thatBinding = [thatBinding, thisBinding]
                            thatBinding = thatBinding[MrUniq(thatBinding, /SORT)]
                            self.bind_x -> Replace_Item, thatBinding, iKeep
                            
                            ;Remove the duplicate from the concatenated row
                            self.bind_x -> Delete_Item, i, DESTROY=0
                            
                            ;Decrease the total number of bindings. Do not increase "i"
                            ;because the i-th binding was removed.
                            nBindings = self.bind_x -> Get_Count()
                        endif
                    endelse
                endwhile
                
                ;Now that one copy is gone, consolidate again to remove those that remain
                self -> Consolidate_Bindings, /XAXIS
            endif        
        endelse
    endif
    
end


;+
;   Create a menu bar with various zoom options in it.
;
; :Params:
;       PARENT:             in, required, type=integer
;                           The widget ID of the parent widget for the new SaveAs Menu.
;-
pro MrAbstractZoom::Create_Zoom_Menu, parent
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Make the zoom menu in the menu bar
    zoomID = widget_button(parent, VALUE='Zoom', /MENU)
    button = widget_button(zoomID, VALUE='Zoom X', /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    button = widget_button(zoomID, VALUE='Zoom Y', /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    button = widget_button(zoomID, VALUE='Box Zoom', /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    button = widget_button(zoomID, VALUE='Pan', /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    
    ;Wheel zoom submenu
    wheelID = widget_button(zoomID, VALUE='Wheel Zoom', /MENU)
    button = widget_button(wheelID, VALUE='Wheel Zoom: XY', /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    button = widget_button(wheelID, VALUE='Wheel Zoom: Color', /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    
    ;Back to main Zoom menu
    button = widget_button(zoomID, VALUE='UnZoom', UVALUE={object: self, method: 'UnZoom'})
    button = widget_button(zoomID, VALUE='None', UNAME='ZNone', UVALUE={object: self, method: 'Zoom_Menu_Events'})
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
pro MrAbstractZoom::Draw_Events, event
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
        if self.lmode eq 1 || self.rmode eq 1 then self -> XY_Zoom, event     ;X Zoom
        if self.lmode eq 2 || self.rmode eq 2 then self -> XY_Zoom, event     ;Y Zoom
        if self.lmode eq 4 || self.rmode eq 4 then self -> Box_Zoom, event    ;Box Zoom
        if self.lmode eq 8 || self.rmode eq 8 then self -> Pan, event         ;Pan
    endif
    
;---------------------------------------------------------------------
;Button Release Events ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 1 then begin
        if self.zmode eq 4 then self -> Box_Zoom, event   ;Box Zoom
        if self.zmode eq 8 then self -> Pan, event        ;Pan
    endif

;---------------------------------------------------------------------
;Motion Events ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 2 then begin
        ;Do not compete for copying the pixmap
        if ((self.zmode and 4) gt 0) || ((self.cmode and 2) gt 0) $  ;Box Zoom, Cross Hairs
            then self -> copyPixmap

        ;Handle motion events
        if self.zmode eq 4 then self -> Box_Zoom, event     
        if self.zmode eq 8 then self -> Pan, event 
        
        ;Do not compete for drawing (Pan)
        if self.zmode eq 8 then self -> Draw                  ;Pan
    endif

;---------------------------------------------------------------------
;Wheel Events ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Zoom in the X and Y direction only. Forget about the color dimension (for now).
    if event.type eq 7 then begin
        if self.wmode eq 1 then self -> Wheel_Zoom_XY, event
        if self.wmode eq 2 then self -> Wheel_Zoom_Color, event
    endif
end


;+
;   Serve as a general error handler for event handling routines.
;-
pro MrAbstractZoom::Error_Handler
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
    self.zmode = 0

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
pro MrAbstractZoom::On_Off_Button_Events, $
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
        
        ;Turn button events off only if nothing else needs them (all zoom modes need
        ;button events).
        if self.lmode eq 0 and self.rmode eq 0 $
            then widget_control, self.drawID, DRAW_BUTTON_EVENTS=0
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
pro MrAbstractZoom::On_Off_Motion_Events, $
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
        
        ;Turn motion events off only if nothing else needs them. Only one zoom mode
        ;can be active at a time, so ZMODE must be 0 to turn off motion events.
        if self.zmode eq 0 then begin
            widget_control, self.drawID, DRAW_MOTION_EVENTS=0
            widget_control, self.drawID, /CLEAR_EVENTS
        endif
    endif
end


;+
;   Pan the plot left or right. Click and hold on the plot, then drag the mouse in any
;   direction.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractZoom::Pan, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = error_message()
        return
    endif

    ;Return if we are not trying to Pan
    case event.type of
        0: if event.press eq 1 and self.lmode ne 8 || $         ;left button down
              event.press eq 4 and self.rmode ne 8 then return  ;right button down
        1: if self.zmode ne 8 then return                       ;button up
        2: if self.zmode ne 8 then return                       ;motion
        else: return
    endcase

    case event.type of
        0: begin    ;button down
            ;Turn on Pan
            self.zmode = 8
        
            ;Store the clicked coordinates
            self.x0 = event.x
            self.y0 = event.y
        
            ;Turn on motion events
            self -> On_Off_Motion_Events, /ON
        endcase
        
        2: begin    ;motion event
            ;Convert to data coordinates.
            x = [self.x0, event.x]
            y = [self.y0, event.y]
            xy = convert_coord(x, y, /DEVICE, /TO_DATA)
            
            ;How much did the mouse move?
            delta_x = xy[0,1] - xy[0,0]
            delta_y = xy[1,1] - xy[1,0]
            
            ;Get the current x- and y-range
            theObj = (*self.plotObjects)[self.ifocus]
            theObj -> GetProperty, XRANGE=xrange, YRANGE=yrange
            
            ;A drag to the right will have delta_x < 0. Subtracting will then add the
            ;difference, panning more positive data into the window.
            xrange = xrange - delta_x
            yrange = yrange - delta_y
            
            ;Set the new ranges
            theObj -> SetProperty, XRANGE=xrange, YRANGE=yrange
            
            self.x0 = event.x
            self.y0 = event.y
        endcase
        
        1: begin    ;button up
            ;Turn off Pan
            self.zmode = 0
        
            ;Turn motion events off right away (requires self.zmode = 0)
            self -> On_Off_Motion_Events, /OFF
            self.x0 = -1
            self.y0 = -1
            
            ;Apply bindings. Need a Draw to update the bindings.
            self -> Apply_Bindings, (*self.plotObjects)[self.ifocus], /XAXIS, /YAXIS
            self -> Draw
        endcase
    endcase
    
end


;+
;   Unbind one set of axes from all others.
;
; :Params:
;       THEOBJECT:          in, optional, type=object
;                           The object whose axes are to be unbound. If not given, all
;                               sets of axes will be unbound. Which axes are unbound is
;                               determined by the keywords.
;
; :Keywords:
;       ALL:                in, optional, type=boolean, default=0
;                           Unbind all of `THEOBJECTS`'s from all other axes.
;       CAXIS:              in, optional, type=boolean, default=0
;                           Unbind all of the `THEOBJECT`'s color axis from all others
;                               sets of color axes.
;       XAXIS:              in, optional, type=boolean, default=0
;                           Unbind all of the `THEOBJECT`'s x-axis from all others sets
;                               of x-axes.
;       YAXIS:              in, optional, type=boolean, default=0
;                           Unbind all of the `THEOBJECT`'s y-axis from all others sets
;                               of x-axes.
;       ZAXIS:              in, optional, type=boolean, default=0
;                           Unbind all of the `THEOBJECT`'s z-axis from all others sets
;                               of x-axes.
;-
pro MrAbstractZoom::UnBind, theObject, $
ALL=all, $
CAXIS = caxis, $
XAXIS = xaxis, $
YAXIS = yaxis, $
ZAXIS = zaxis
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
;---------------------------------------------------------------------
;Unbind All Axes /////////////////////////////////////////////////////
;---------------------------------------------------------------------

    if keyword_set(all) then begin
        caxis = 1
        xaxis = 1
        yaxis = 1
        zaxis = 1
    endif
    
;---------------------------------------------------------------------
;Unbind From X-Axis //////////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(xaxis) and obj_valid(self.bind_x) then begin
            
    ;---------------------------------------------------------------------
    ;Destroy All X-Bindings //////////////////////////////////////////////
    ;---------------------------------------------------------------------
    
        if n_params() eq 0 then begin
            obj_destroy, self.bind_x
            
    ;---------------------------------------------------------------------
    ;Unbind theObject ////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endif else begin
            
            ;See how many different sets bindings there are.
            nBindings = self.bind_x -> Get_Count()
            
        ;---------------------------------------------------------------------
        ;One Set of Bindings /////////////////////////////////////////////////
        ;---------------------------------------------------------------------
            if nBindings eq 1 then begin
            
                ;Count how many objects are bound
                thisBinding = self.bind_x -> Get_Item(0)
                nSet = n_elements(thisBinding)
                iBound = where(thisBinding eq theObject, count)
                
                ;If only a pair is bound together
                if count ne 0 and nSet le 2 then begin
                    obj_destroy, self.bind_x
                    
                ;If multiple objects are bound together
                endif else if count ne 0 and nSet gt 2 then begin
                    ;Truncate theObject
                    thisBinding = shift(thisBinding, -thisBinding)
                    self.bind_x -> Replace_Item, thisBinding[1:*], 0, /NO_COPY
                endif
            
        ;---------------------------------------------------------------------
        ;Several Sets of Bindings ////////////////////////////////////////////
        ;---------------------------------------------------------------------
            endif else begin
                    
                count = 0
                i = 0
                ;See if theObject is bound to anything.
                while count eq 0 and i lt nBindings do begin
                    thisBinding = self.bind_x -> Get_Item(i)
                    iBound = where(thisBinding eq theObject, count)
                    
                    ;if theObject was not found, skip to the next iteration
                    if count eq 0 then begin
                        i++
                        continue
                    endif
                
                    ;If a single pair of objects are bound together
                    if n_elements(thisBinding) le 2 then begin
                        ;Remove the binding from the list
                        self.bind_x -> Remove_Item, i, DESTROY=0
                        
                    ;If a group of objects are bound together
                    endif else begin
                        ;Shift the binding to the end of the row and truncate it
                        thisBinding = shift(thisBinding, -theBinding)
                        self.bind_x -> Replace_Item, thisBinding[1:*], i, /NO_COPY
                    endelse
                
                    i++
                endwhile
            endelse
        endelse
    endif
        
;---------------------------------------------------------------------
;Unbind From Y-Axis //////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Do the same for the y-axis
    if keyword_set(yaxis) and obj_valid(self.bind_y) then begin    
        if n_params() eq 0 then begin
            if ptr_valid(self.bind_y) then obj_destroy, self.bind_y
            
        endif else begin
            nBindings = self.bind_y -> Get_Count()
            
            if nBindings eq 1 then begin
            
                thisBinding = self.bind_y -> Get_Item(0)
                nSet = n_elements(thisBinding)
                iBound = where(thisBinding eq theObject, count)
                
                if count ne 0 and nSet le 2 then begin
                    obj_destroy, self.bind_y
                    
                endif else if count ne 0 and nSet gt 2 then begin
                    thisBinding = shift(thisBinding, -thisBinding)
                    self.bind_y -> Replace_Item, thisBinding[1:*], 0, /NO_COPY
                endif
            
            endif else begin
                    
                count = 0
                i = 0
                while count eq 0 and i lt nBindings do begin
                    thisBinding = self.bind_y -> Get_Item(i)
                    iBound = where(thisBinding eq theObject, count)
                    
                    if count eq 0 then begin
                        i++
                        continue
                    endif
                
                    if n_elements(thisBinding) le 2 then begin
                        self.bind_y -> Remove_Item, i, DESTROY=0
                        
                    endif else begin
                        thisBinding = shift(thisBinding, -theBinding)
                        self.bind_y -> Replace_Item, thisBinding[1:*], i, /NO_COPY
                    endelse
                
                    i++
                endwhile
            endelse
        endelse
    endif
    
;---------------------------------------------------------------------
;Unbind From Z-Axis //////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Do the same for the z-axis
    if keyword_set(zaxis) and obj_valid(self.bind_z) then begin    
        if n_params() eq 0 then begin
            if ptr_valid(self.bind_z) then obj_destroy, self.bind_z
            
        endif else begin
            nBindings = self.bind_z -> Get_Count()
            
            if nBindings eq 1 then begin
            
                thisBinding = self.bind_z -> Get_Item(0)
                nSet = n_elements(thisBinding)
                iBound = where(thisBinding eq theObject, count)
                
                if count ne 0 and nSet le 2 then begin
                    obj_destroy, self.bind_z
                    
                endif else if count ne 0 and nSet gt 2 then begin
                    thisBinding = shift(thisBinding, -thisBinding)
                    self.bind_z -> Replace_Item, thisBinding[1:*], 0, /NO_COPY
                endif
            
            endif else begin
                    
                count = 0
                i = 0
                while count eq 0 and i lt nBindings do begin
                    thisBinding = self.bind_z -> Get_Item(i)
                    iBound = where(thisBinding eq theObject, count)
                    
                    if count eq 0 then begin
                        i++
                        continue
                    endif
                
                    if n_elements(thisBinding) le 2 then begin
                        self.bind_z -> Remove_Item, i, DESTROY=0
                        
                    endif else begin
                        thisBinding = shift(thisBinding, -theBinding)
                        self.bind_z -> Replace_Item, thisBinding[1:*], i, /NO_COPY
                    endelse
                
                    i++
                endwhile
            endelse
        endelse
    endif
    
;---------------------------------------------------------------------
;Unbind From Color Axis //////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Do the same for the color axis
    if keyword_set(caxis) and obj_valid(self.bind_c) then begin    
        if n_params() eq 0 then begin
            if ptr_valid(self.bind_c) then obj_destroy, self.bind_c
            
        endif else begin
            nBindings = self.bind_c -> Get_Count()
            
            if nBindings eq 1 then begin
            
                thisBinding = self.bind_c -> Get_Item(0)
                nSet = n_elements(thisBinding)
                iBound = where(thisBinding eq theObject, count)
                
                if count ne 0 and nSet le 2 then begin
                    obj_destroy, self.bind_c
                    
                endif else if count ne 0 and nSet gt 2 then begin
                    thisBinding = shift(thisBinding, -thisBinding)
                    self.bind_c -> Replace_Item, thisBinding[1:*], 0, /NO_COPY
                endif
            
            endif else begin
                    
                count = 0
                i = 0
                while count eq 0 and i lt nBindings do begin
                    thisBinding = self.bind_c -> Get_Item(i)
                    iBound = where(thisBinding eq theObject, count)
                    
                    if count eq 0 then begin
                        i++
                        continue
                    endif
                
                    if n_elements(thisBinding) le 2 then begin
                        self.bind_c -> Remove_Item, i, DESTROY=0
                        
                    endif else begin
                        thisBinding = shift(thisBinding, -theBinding)
                        self.bind_c -> Replace_Item, thisBinding[1:*], i, /NO_COPY
                    endelse
                
                    i++
                endwhile
            endelse
        endelse
    endif
end


;+
;   Return to initial, unzoomed x- and y-ranges
;-
pro MrAbstractZoom::UnZoom, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Set the x- and y-range
    theObj = (*self.allObjects)[self.ifocus]
    theObj -> getProperty, INIT_XRANGE=init_xrange, INIT_YRANGE=init_yrange
    theObj -> setProperty, XRANGE=init_xrange, YRANGE=init_yrange
    
    ;Update Bindings
    self -> Apply_Bindings, theObj, /XAXIS, /YAXIS
    
    ;Redraw
    self -> Draw
end


;+
;   Print the heap variable numbers of the objects that are bound together.
;-
pro MrAbstractZoom::whichBindings
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
;---------------------------------------------------------------------
;X-Bindings //////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    print, 'X-Bindings: '
    if obj_valid(self.bind_x) then begin
        nBindings = self.bind_x -> Get_Count()
        for i = 0, nBindings - 1 do begin
            thisBinding = self.bind_x -> Get_Item(i)
            ith = strtrim(i, 2)
            print, '  Binding ' + ith + ': ', thisBinding
        endfor
    endif
    
;---------------------------------------------------------------------
;Y-Bindings //////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    print, 'Y-Bindings: '
    if obj_valid(self.bind_y) then begin
        nBindings = self.bind_y -> Get_Count()
        for i = 0, nBindings - 1 do begin
            thisBinding = self.bind_y -> Get_Item(i)
            ith = strtrim(i, 2)
            print, '  Binding ' + ith + ': ', thisBinding
        endfor
    endif
    
;---------------------------------------------------------------------
;Z-Bindings //////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    print, 'Z-Bindings: '
    if obj_valid(self.bind_z) then begin
        nBindings = self.bind_z -> Get_Count()
        for i = 0, nBindings - 1 do begin
            thisBinding = self.bind_z -> Get_Item(i)
            ith = strtrim(i, 2)
            print, '  Binding ' + ith + ': ', thisBinding
        endfor
    endif
    
;---------------------------------------------------------------------
;Color Bindings //////////////////////////////////////////////////////
;---------------------------------------------------------------------

    print, 'Color Bindings: '
    if obj_valid(self.bind_c) then begin
        nBindings = self.bind_c -> Get_Count()
        for i = 0, nBindings - 1 do begin
            thisBinding = self.bind_c -> Get_Item(i)
            ith = strtrim(i, 2)
            print, '  Binding ' + ith + ': ', thisBinding
        endfor
    endif
    
end


;+
;   Roll the mouse button forward or backward to zoom in or out in the X and Y directions
;   by the zoom factor. The zoom factor for the x- and y-directions can be different.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractZoom::Wheel_Zoom_XY, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Get the current x- and y-range
    theObj = (*self.allObjects)[self.ifocus]
    theObj -> getProperty, XRANGE=xrange, YRANGE=yrange

    ;Zooming in and out is determined by the sign of events.clicks.
    ;   events.clicks < 0 if the scroll wheel is moved forward -- zoom in
    ;   events.clicks > 0 if the scroll wheel is moved backward -- zoom out
    ;Make sure xrange[0] < xrange[1]
    delta_x = abs(xrange[1] - xrange[0]) * self.zoomfactor[0] * event.clicks
    xrange[0] = xrange[0] + delta_x
    xrange[1] = xrange[1] - delta_x
    if xrange[0] gt xrange[1] then xrange[0] = xrange[1] - delta_x
    
    ;Do the same for the y-range
    delta_y = abs(yrange[1] - yrange[0]) * self.zoomfactor[1] * event.clicks
    yrange[0] = yrange[0] + delta_y
    yrange[1] = yrange[1] - delta_y
    if yrange[0] gt yrange[1] then yrange[0] = yrange[1] - delta_y
    
    ;Set the zoomed x- and y-range
    theObj -> SetProperty, XRANGE=xrange, YRANGE=yrange
    self -> Apply_Bindings, theObj, /XAXIS, /YAXIS

    ;Redraw the plot
    self -> draw
end


;+
;   Roll the mouse button forward or backward to zoom in or out in the color dimension
;   by the zoom factor. The zoom factor for the x- and y-directions can be different.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractZoom::Wheel_Zoom_Color, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Get the current x- and y-range
    theObj = (*self.allObjects)[self.ifocus]
    theObj -> getProperty, RANGE=range

    ;Zooming in and out is determined by the sign of events.clicks.
    ;   events.clicks < 0 if the scroll wheel is moved forward -- zoom in
    ;   events.clicks > 0 if the scroll wheel is moved backward -- zoom out
    ;Make sure range[0] < range[1]
    delta = abs(range[1] - range[0]) * self.zoomfactor[0] * event.clicks
    range[0] = range[0] + delta
    range[1] = range[1] - delta
    if range[0] gt range[1] then range[0] = range[1] - delta
    
    ;Set the zoomed x- and y-range
    theObj -> SetProperty, RANGE=range
    self -> Apply_Bindings, theObj, /CAXIS

    ;Redraw
    self -> draw
end


;+
;   Zoom in the X or Y direction, depending on the zoom mode. The first click sets
;   the minimum of the new range while the second click sets the minimum updates the
;   display to reflect the new range.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractZoom::XY_Zoom, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = error_message()
        return
    endif
    
    ;Only listen to button presses
    if event.type ne 0 then return
    case event.press of
        1: if ((self.lmode and 1) eq 0) and ((self.lmode and 2) eq 0) then return
        4: if ((self.rmode and 1) eq 0) and ((self.rmode and 2) eq 0) then return
        else: if ((self.zmode and 1) eq 0) && ((self.zmode and 2) eq 0) then return
    endcase

    ;Store the location of the first click without doing
    ;anything else.
    if self.x0 eq -1 and self.y0 eq -1 then begin
        case event.press of
            1: self.zmode = self.lmode
            4: self.zmode = self.rmode
        endcase
    
        self.x0 = event.x
        self.y0 = event.y
        return
    endif

    ;Order the clicks as [min, max]. Convert to data
    x = [self.x0 < event.x, self.x0 > event.x]
    y = [self.y0 < event.y, self.y0 > event.y]

    ;Convert from device to data coordinates
    xy = convert_coord(x, y, /DEVICE, /TO_DATA)
    xrange = reform(xy[0,*])
    yrange = reform(xy[1,*])
    
    ;only zoom if the coordinate changed
    case self.zmode of
        1: begin
            if self.x0 ne event.x then begin
                theObj = (*self.plotObjects)[self.ifocus]
                theObj -> SetProperty, XRANGE=xrange
                self -> Apply_Bindings, theObj, /XAXIS
            endif
        endcase
        
        2: begin
            if self.y0 ne event.y then begin
                theObj = (*self.plotObjects)[self.ifocus]
                theObj -> SetProperty, YRANGE=yrange
                self -> Apply_Bindings, theObj, /YAXIS
            endif
        endcase
        
        else: ;Do nothing
    endcase
    
    ;Reset the active zoom and initial click properties.
    self.zmode = 0
    self.x0 = -1
    self.y0 = -1
    
    ;Re-draw the plot
    self -> draw
end


;+
;   Determine which "Zoom Menu" button was pressed and tell the draw widget which type of
;   events to pay attention to.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrAbstractZoom::Zoom_Menu_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Reset the initial click
    self.x0 = -1
    self.y0 = -1
    
    ;Get the value of the button that cause the event
    widget_control, event.id, GET_VALUE=zoom_type
    zoom_type = strupcase(zoom_type)
    
    ;Determine if the button is set or not, then toggle it
    isSet = widget_info(event.id, /BUTTON_SET)

    ;
    ;Because Wheel Zoom buttons are in a submenu within the Zoom menu, the setting and
    ;unsetting of check boxes works as is. This is because the /ALL_CHILDREN keyword to
    ;Widget_Info returns only immediate children. Since wheel zoom events are independent
    ;from other zoom types, to make things more general, it may be better to separate the
    ;wheel zoom menu event handling into its own method.
    ;
    
    ;Get the button's siblings and uncheck them all
    parent = widget_info(event.id, /PARENT)
    kids = widget_info(parent, /ALL_CHILDREN)
    for i = 0, n_elements(kids) - 1 do widget_control, kids[i], SET_BUTTON=0
    
    ;Set the zoom mode, set the event handling method, and indicate which type
    ;of events to listen for.
    case zoom_type of
        'NONE': begin
            self.lmode = 0
            self -> On_Off_Button_Events, /OFF
        endcase
        
        'ZOOM X': self.lmode = 1
        'ZOOM Y': self.lmode = 2
        'BOX ZOOM': self.lmode = 4
        'PAN': self.lmode = 8
        'WHEEL ZOOM: XY': self.wmode = 1
        'WHEEL ZOOM: COLOR': self.wmode = 2
        else: message, 'Button "' + analysis_type + '" unknown.'
    endcase
    
    ;Check the button in the menu
    widget_control, event.id, /SET_BUTTON
    
    ;Turn on button events.
    if zoom_type ne 'NONE' then self -> On_Off_Button_Events, /ON
end


;+
;   Clean up after the object is destroy
;-
pro MrAbstractZoom::cleanup
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Destroy the linked lists that bind axes together
    obj_destroy, self.bind_x
    obj_destroy, self.bind_y
    obj_destroy, self.bind_z
    obj_destroy, self.bind_c
    
end


;+
;   The initialization method. Because MrAbstractZoom is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractZoom object will result
;   in an error.
;-
function MrAbstractZoom::init
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
pro MrAbstractZoom__define, class
    compile_opt idl2
    
    class = {MrAbstractZoom, $
             bind_x: obj_new(), $   ;X-axis bindings
             bind_y: obj_new(), $   ;Y-axis bindings
             bind_z: obj_new(), $   ;Z-axis bindings
             bind_c: obj_new(), $   ;Color bindings (e.g. between a color bar and an image)
             rmode: 0, $            ;Zoom mode for the right mouse button
             lmode: 0, $            ;Zoom mode for the left mouse button
             wmode: 0, $            ;Wheel mode: XY or Color axis
             zmode: 0, $            ;Currently active zoom mode.
             zoomfactor: [0.0,0.0] $;Wheel zoom factor
            }
end