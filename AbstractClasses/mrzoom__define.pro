; docformat = 'rst'
;
; NAME:
;       MrZoom__Define
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
;           Widget_Control, wDrawID, SET_UVALUE = {object: self, method: 'Draw_Events'}, $
;                                    SET_EVENT_PRO = 'your_event_handling_procedure_goes_here'
;
;       In the procedure specified by EVENT_PRO, use the Call_Method procedure::
;           Widget_Control, wDrawID, GET_UVALUE=event_handler
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
;   Button Zoom Bits::
;       0       -   No zoom effects
;       1       -   X Zoom
;       2       -   Y Zoom
;       4       -   Box Zoom
;       8       -   Pan
;
;   Wheel Zoom Bits::
;       0       -   No zoom effects
;       1       -   Zoom in the X- and Y-directions
;       2       -   Zomm in the color range (for images)
;       3       -   Flip pages (for images)
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
;                           inherits MrZoom. Incorporated bindings into the zoom
;                           options. Renamed Wheel_Zoom to Wheel_XY_Zoom and added
;                           Wheel_Color_Zoom. - MRA
;       07/10/2013  -   Added the Wheel_Zoom_Page method. - MRA
;       07/14/2013  -   Added the Bindem method. - MRA
;       08/23/2013  -   Added Init, GetProperty, and SetProperty methods. Renamed from
;                           MrAbstractZoom__Define to MrZoom__Define. Now assumes objects
;                           are stored in an IDL_Container. Menu buttons are now optional.
;                           Added the Turn_Everything_Off method. Use the ConvertCoord
;                           method of the object being zoomed to convert to data
;                           coordinates. - MRA
;       09/23/2013  -   Index SELF.IFOCUS was changed to an object reference SELF.FOCUS.
;                           Added COPY_PIX and DRAW keywords to the Draw_Events method. - MRA
;       10/05/2013  -   Bind now can accept a object arrays. Added the AddToBindingList
;                           method. Eliminate a lot of effort by first creating a new set
;                           of bindings, then by consolidating bindings. Removed the BindEm
;                           method. - MRA
;-
;*****************************************************************************************
;+
;   Bind the axis of one object to the axis of another so that the zooms are synchronized.
;
; :Params:
;       BINDINGLIST:        in, required, type=linked list
;                           A linked list in which to add objects whose properties are
;                               to be kept synchronized.
;       THEOBJECTS:         in, required, type=objarr()
;                           Add these objects to `BINDINGLIST`
;-
pro MrZoom::AddToBindingList, bindingList, theObjects
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Number of objects to bind.
    nToAdd = n_elements(theObjects)
    if nToAdd lt 2 then message, 'At least two objects must be provided.'

;---------------------------------------------------------------------
;Create a Binding List? //////////////////////////////////////////////
;---------------------------------------------------------------------
    nSets = bindingList -> Get_Count()
    
    ;Create a new node
    bindingList -> Add, theObjects

    ;If bindings already exist, then consolidate
    if nSets eq 0 then self -> Consolidate_Bindings
end


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
;-
pro MrZoom::Apply_Bindings, theObject, $
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
;       OBJECT1:            in, required, type=object/objarr()
;                           The objects that will be bound together. If a scalar is given,
;                               `OBJECT2` must contain at least 1 valid object.
;       OBJECT2:            in, optional, type=object/objarr()
;                           Objects to be bound with those in `OBJECT1`.
;
; :Keywords:
;       ALL:                in, optional, type=boolean, default=0
;                           Bind all axes.
;       APPLY:              in, optional, type=boolean, default=1
;                           If set, once the bindings are created, they will be applied so
;                               that the axis ranges match. The ranges of `OBJECT2` will
;                               be update to match those of `OBJECT1[0]`.
;       REMOVE:             in, optional, type=boolean, default=0
;                           If set, the given objects will be unbound from their current
;                               binding set.
;       CAXIS:              in, optional, type=boolean, default=0
;                           Bind the color axis.
;       XAXIS:              in, optional, type=boolean, default=0
;                           Bind the xaxis.
;       YAXIS:              in, optional, type=boolean, default=0
;                           Bind the yaxis.
;       ZAXIS:              in, optional, type=boolean, default=0
;                           Bind the zaxis.
;-
pro MrZoom::Bind, object1, object2, $
ALL=all, $
APPLY = apply, $
REMOVE = remove, $
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
    
    ;Check input parameters
    case n_params() of 
        2: bindThese = [object1, object2]
        1: bindThese = object1
        else: message, 'Use syntax: myZoomObj -> Bind, Object1[, Object2]'
    endcase
    
    ;Apply bindings?
    if n_elements(apply) eq 0 $
        then apply = 1 $
        else apply = keyword_set(apply)
    
    ;Which axes will be bound?
    caxis = keyword_set(caxis)
    xaxis = keyword_set(xaxis)
    yaxis = keyword_set(yaxis)
    zaxis = keyword_set(zaxis)
    
    ;If ALL is set, then bind all axes.
    if keyword_set(all) then begin
        caxis = 1
        xaxis = 1
        yaxis = 1
        zaxis = 1
    endif
    
    ;Make sure at least one axis is being bound.
    if (caxis + xaxis + yaxis + zaxis eq 0) then message, 'Must choose an axis to bind: /[CXYZ]AXIS'

;---------------------------------------------------------------------
;Remove Bindings? ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(remove) then begin
        if (xaxis eq 1) then self -> UnBind, self.bind_x, bindThese
        if (yaxis eq 1) then self -> UnBind, self.bind_y, bindThese
        if (zaxis eq 1) then self -> UnBind, self.bind_z, bindThese
        if (caxis eq 1) then self -> UnBind, self.bind_c, bindThese
    
;---------------------------------------------------------------------
;Create Bindings? ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        if (xaxis eq 1) then self -> AddToBindingList, self.bind_x, bindThese
        if (yaxis eq 1) then self -> AddToBindingList, self.bind_y, bindThese
        if (zaxis eq 1) then self -> AddToBindingList, self.bind_z, bindThese
        if (caxis eq 1) then self -> AddToBindingList, self.bind_c, bindThese
    endelse
        
    ;Apply Bindings?
    if apply eq 1 then self -> Apply_Bindings, bindThese[0], ALL=all, CAXIS=caxis, $
                                               XAXIS=xaxis, YAXIS=yaxis, ZAXIS=zaxis
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
pro MrZoom::Box_Zoom, event
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
            
            ;Return if nothing changed.
            if self.x0 eq event.x && self.y0 eq event.y then return
            
            ;Order the clicks as [min, max]. Convert to data
            x = [self.x0 < event.x, self.x0 > event.x]
            y = [self.y0 < event.y, self.y0 > event.y]

            ;Get the object of focus. Convert data coordinates
            xy = self.focus -> ConvertCoord(x, y, /DEVICE, /TO_DATA)
            xrange = reform(xy[0,*])
            yrange = reform(xy[1,*])
            
            ;Only zoom if the coordinate changed
            if self.x0 eq event.x then void = temporary(xrange)
            if self.y0 eq event.y then void = temporary(yrange)
            
            ;Set the new range and apply any bindings
            self -> Refresh, /DISABLE
            self.focus -> SetProperty, XRANGE=xrange, YRANGE=yrange
            self -> Apply_Bindings, self.focus, /XAXIS, /YAXIS
            self -> Refresh
            
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
pro MrZoom::Consolidate_Bindings, bindingList, $
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
    
    if obj_valid(bindingList) eq 0 then return
    nBindings = bindingList -> Get_Count()

;---------------------------------------------------------------------
;One Set of Bindings /////////////////////////////////////////////////
;---------------------------------------------------------------------    
    if nBindings eq 1 then begin
        ;Select only the unique bindings
        thisBinding = bindingList -> Get_Item(0)
        thisBinding = thisBinding[MrUniq(thisBinding, /SORT)]
        
        ;If only one object is left, remove the binding set from the list
        if n_elements(thisBinding) le 1 then bindingList -> Delete, 0, DESTROY=0

;---------------------------------------------------------------------
;Many Sets of Bindings ///////////////////////////////////////////////
;---------------------------------------------------------------------  
    endif else begin
        ;Make an array of all of the objects in all of the different sets of bindings.
        allObjects = bindingList -> Get_Item(0)
        for i = 1, nBindings - 1 do allObjects = [allObjects, bindingList -> Get_Item(i)]
        
        ;Find duplicates
        void = MrUniq(allObjects, /SORT, NCOMPLEMENT=ncopies, COMPLEMENT=icopies)

    ;---------------------------------------------------------------------
    ;Deal With Duplicates ////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if ncopies gt 0 then begin
            objToConsolicate = allObjects[icopies[0]]
            
            ;Step through all of the binding sets
            i = 0
            iKeep = -1
            while i lt nBindings do begin
                thisBinding = bindingList -> Get_Item(i)
            
                ;Find the first instance of the duplicate object
                if iKeep eq -1 then begin
                    void = where(thisBinding eq objToConsolidate, count)
                    
                    ;Store the index of the binding set in which the duplicate was found.
                    ;Go to the next bindings set.
                    if count ne 0 then iKeep = i
                    i += 1
                    
                ;Find other instances of the duplicate object
                endif else begin
                    void = where(thisBinding eq objToConsolidate, count)
                    
                    ;When found
                    if count ne 0 then begin
                        ;Concatenate the two rows. Do not keep the duplicate.
                        thatBinding = bindingList -> Get_Item(iKeep)
                        thatBinding = [thatBinding, thisBinding]
                        thatBinding = thatBinding[MrUniq(thatBinding, /SORT)]
                        self.bind_x -> Replace_Item, thatBinding, iKeep
                        
                        ;Remove the binding set containing the duplicate from list of bindings
                        bindingList -> Delete, i, DESTROY=0
                        
                        ;Decrease the total number of bindings. Do not increase "i"
                        ;because the i-th binding was removed.
                        nBindings = bindingList -> Get_Count()
                    endif
                endelse
            endwhile
            
            ;Now that one copy is gone, consolidate again to remove those that remain
            self -> Consolidate_Bindings, bindingList
        endif        
    endelse    
end


;+
;   Create a menu bar with various zoom options in it. If no buttons are selected, then
;   all keywords will be turned set.
;
; :Params:
;       PARENT:             in, required, type=boolean
;                           The widget ID of the parent widget for the new SaveAs Menu.
;
; :Keywords:
;       BOX_ZOOM:           in, optional, type=boolean, default=0
;                           Create a button for turning on and off Box Zoom.
;       MENU:               in, optional, type=boolean, default=0
;                           If set, zoom buttons will be placed in a menu.
;       NONE:               in, optional, type=boolean, default=0
;                           Create a button for turning on and off zoom effects.
;       PAN:                in, optional, type=boolean, default=0
;                           Create a button for turning on and off Panning.
;       UNZOOM:             in, optional, type=boolean, default=0
;                           Create a button to unzoom.
;       WHEELZOOM_MENU:     in, optional, type=boolean, default=0
;                           If set, and `MENU` is set, wheel zoom buttons will be 
;                               placed in a sub-menu.
;       WHEELZOOM_COLOR:    in, optional, type=boolean, default=0
;                           Create a button for turning on and off wheel color-zoom.
;       WHEELZOOM_PAGE:     in, optional, type=boolean, default=0
;                           Create a button for turning on and off wheel paging.
;       WHEELZOOM_XY:       in, optional, type=boolean, default=0
;                           Create a button for turning on and off wheel xy-zooming.
;       ZOOM_X:             in, optional, type=boolean, default=0
;                           Create a button for turning on and off zooming in X.
;       ZOOM_Y:             in, optional, type=boolean, default=0
;                           Create a button for turning on and off zooming in Y.
;-
pro MrZoom::Create_Zoom_Menu, parent, $
BOX_ZOOM = box_zoom, $
MENU = menu, $
NONE = none, $
PAN = pan, $
UNZOOM = unzoom, $
WHEELZOOM_MENU = wheelzoom_menu, $
WHEELZOOM_COLOR = wheelzoom_color, $
WHEELZOOM_PAGE = wheelzoom_page, $
WHEELZOOM_XY = wheelzoom_xy, $
ZOOM_X = zoom_x, $
ZOOM_Y = zoom_y
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;SetDefaults
    SetDefaultValue, box_zoom,        0, /BOOLEAN
    SetDefaultValue, menu,            0, /BOOLEAN
    SetDefaultValue, none,            0, /BOOLEAN
    SetDefaultValue, pan,             0, /BOOLEAN
    SetDefaultValue, unzoom,          0, /BOOLEAN
    SetDefaultValue, wheelzoom_color, 0, /BOOLEAN
    SetDefaultValue, wheelzoom_menu,  0, /BOOLEAN
    SetDefaultValue, wheelzoom_page,  0, /BOOLEAN
    SetDefaultValue, wheelzoom_xy,    0, /BOOLEAN
    SetDefaultValue, zoom_x,          0, /BOOLEAN
    SetDefaultValue, zoom_y,          0, /BOOLEAN
    
    ;If nothing was chosen, create all of the buttons.
    if (zoom_x + zoom_y + box_zoom + pan + $
        wheelzoom_xy + wheelzoom_color + wheelzoom_page) eq 0 then begin
        
        box_zoom = 1
        menu = 1
        none = 1
        pan = 1
        unzoom = 1
        wheelzoom_menu = 1
        wheelzoom_xy = 1
        wheelzoom_color = 1
        wheelzoom_page = 1
        zoom_x = 1
        zoom_y = 1
    endif
    
    ;Create a menu button?
    if keyword_set(menu) $
        then zoomID = widget_button(parent, VALUE='Zoom', /MENU) $
        else zoomID = parent

    ;Make the zoom buttons
    if zoom_x   then button = widget_button(zoomID, VALUE='Zoom X',   UNAME='ZOOM_X',   /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    if zoom_y   then button = widget_button(zoomID, VALUE='Zoom Y',   UNAME='ZOOM_Y',   /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    if box_zoom then button = widget_button(zoomID, VALUE='Box Zoom', UNAME='BOX_ZOOM', /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    if pan      then button = widget_button(zoomID, VALUE='Pan',      UNAME='ZOOM_X',   /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    
    ;Create a wheel-zoom submenu?
    if keyword_set(wheelzoom_menu) $
        then wheelID = widget_button(zoomID, VALUE='Wheel Zoom', /MENU) $
        else wheelID = zoomID

    ;Create the wheel zoom buttons
    if wheelzoom_xy    then button = widget_button(wheelID, VALUE='Wheel Zoom: XY',    UNAME='WHEELZOOM_XY',    /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    if wheelzoom_color then button = widget_button(wheelID, VALUE='Wheel Zoom: Color', UNAME='WHEELZOOM_COLOR', /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    if wheelzoom_page  then button = widget_button(wheelID, VALUE='Wheel Zoom: Page',  UNAME='WHEELZOOM_PAGE',  /CHECKED_MENU, UVALUE={object: self, method: 'Zoom_Menu_Events'})
    
    ;Back to main Zoom menu
    if unzoom then button = widget_button(zoomID, VALUE='UnZoom', UNAME='UNZOOM',    UVALUE={object: self, method: 'UnZoom'})
    if none   then button = widget_button(zoomID, VALUE='None',   UNAME='ZOOM_NONE', UVALUE={object: self, method: 'Zoom_Menu_Events'})
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
;
; :Keywords:
;       COPY_PIX:           in, optional, type=boolean, default=1
;                           Some zoom events require the pixmap to be copied before an
;                               action is performed (e.g. to erase the previous box when
;                               box-zooming). Set `COPY_PIX`=0 to not copy the pixmap.
;       DRAW:               in, optional, type=boolean, default=0
;                           Some zoom events required the graphics to be redrawn after
;                               an action is performed (e.g. after a pan event). Set
;                               `DRAW`=0 to prevent the graphics from being re-drawn.
;-
pro MrZoom::Draw_Events, event, $
COPY_PIX = copy_pix, $
DRAW = draw
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    SetDefaultValue, copy_pix, 1, /BOOLEAN
    SetDefaultValue, draw, 1, /BOOLEAN

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
        if (copy_pix eq 1) && (self.zmode eq 4) then self -> copyPixmap  ;Box Zoom

        ;Handle motion events
        if self.zmode eq 4 then self -> Box_Zoom, event     
        if self.zmode eq 8 then self -> Pan, event 
        
        ;Do not compete for drawing (Pan)
        if (draw eq 1) && self.zmode eq 8 then self -> Draw            ;Pan
    endif

;---------------------------------------------------------------------
;Wheel Events ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 7 then begin
        if self.wmode eq 1 then self -> Wheel_Zoom_XY, event
        if self.wmode eq 2 then self -> Wheel_Zoom_Color, event
        if self.wmode eq 4 then self -> Wheel_Zoom_Page, event
    endif
end


;+
;   The purpose of this method is to get object properties.
;
; :Keywords:
;       LMODE:              out, optional, type=int
;                           The zoom mode associated with the left mouse button.
;       RMODE:              out, optional, type=int
;                           The zoom mode associated with the right mouse button.
;       WMODE:              out, optiona, type=int
;                           The zoom mode associated with the mouse wheel.
;       ZOOMFACTOR:         out, optional, type=float/fltarr(2)
;                           The zoom factor with which the mouse wheel will cause the
;                               x- and y-ranges to be adjusted. If only one value is given,
;                               it will be applied to both axes.
;-
pro MrZoom::GetProperty, $
LMODE = lmode, $
RMODE = rmode, $
WMODE = wmode, $
ZOOMFACTOR = zoomfactor
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Get Properties
    if arg_present(lmode) ne 0 then lmode = self.lmode
    if arg_present(rmode) ne 0 then rmode = self.rmode
    if arg_present(wmode) ne 0 then wmode = self.wmode
    if arg_present(zoomfactor) ne 0 then zoomfactor = self.zoomfactor
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
        void = error_message()
        return
    endif

    self -> Turn_Zoom_Off, self.tlb
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
pro MrZoom::On_Off_Button_Events, $
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
pro MrZoom::On_Off_Motion_Events, $
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
;   The purpose of this method is to set object properties.
;
; :Keywords:
;       LMODE:              in, optional, type=int
;                           The zoom mode associated with the left mouse button.
;       RMODE:              in, optional, type=int
;                           The zoom mode associated with the right mouse button.
;       WMODE:              in, optiona, type=int
;                           The zoom mode associated with the mouse wheel.
;       ZOOMFACTOR:         in, optional, type=float/fltarr(2)
;                           The zoom factor with which the mouse wheel will cause the
;                               x- and y-ranges to be adjusted. If only one value is given,
;                               it will be applied to both axes.
;-
pro MrZoom::SetProperty, $
LMODE = lmode, $
RMODE = rmode, $
WMODE = wmode, $
ZOOMFACTOR = zoomfactor
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Make sure only one zoom bit is set
    if n_elements(lmode) ne 0 then $
        if total(fix(binary(lmode))) le 1 $
            then self.lmode = lmode $
            else message, 'LMODE must have only one bit set'

    ;Make sure only one zoom bit is set            
    if n_elements(rmode) ne 0 then $
        if total(fix(binary(rmode))) le 1 $
            then self.rmode = rmode $
            else message, 'RMODE must have only one bit set'

    ;Make sure only one zoom bit is set            
    if n_elements(wmode) ne 0 then $
        if total(fix(binary(wmode))) le 1 $
            then self.wmode = wmode $
            else message, 'WMODE must have only one bit set'

    ;ZoomFactor
    nZF = n_elements(zoomfactor)
    case nZF of
        0: ;Do nothing
        1: self.zoomfactor = [zoomfactor, zoomfactor]
        2: self.zoomfactor = zoomfactor
        else: message, 'Incorrect number of elements: ZOOMFACTOR.'
    endcase
end


;+
;   A method for turning off all zoom options and effects.
;
; :Params:
;       TLB:        in, optional, type=int
;                   The widget ID of the top level base.
;-
pro MrZoom::Turn_Zoom_Off, tlb
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Reset event processing
    self.x0 = -1
    self.y0 = -1
    self.rmode = 0
    self.lmode = 0
    self.wmode = 0
    self.zmode = 0
    self -> On_Off_Button_Events, /OFF
    self -> On_Off_Motion_Events, /OFF
    
    ;Uncheck all menu buttons
    if n_elements(tlb) gt 0 && widget_info(tlb, /VALID_ID) then begin
        ;Get the widget IDs of the menu buttons
        boxID   = widget_info(tlb, FIND_BY_UNAME='BOX_ZOOM')
        panID   = widget_info(tlb, FIND_BY_UNAME='PAN')
        colorID = widget_info(tlb, FIND_BY_UNAME='WHEELZOOM_COLOR')
        pageID  = widget_info(tlb, FIND_BY_UNAME='WHEELZOOM_PAGE')
        wxyID   = widget_info(tlb, FIND_BY_UNAME='WHEELZOOM_XY')
        zxID    = widget_info(tlb, FIND_BY_UNAME='ZOOM_X')
        zyID    = widget_info(tlb, FIND_BY_UNAME='ZOOM_Y')
        
        ;Uncheck the buttons
        if widget_info(boxID,   /VALID_ID) then widget_control, boxID,   SET_BUTTON=0
        if widget_info(panID,   /VALID_ID) then widget_control, panID,   SET_BUTTON=0
        if widget_info(colorID, /VALID_ID) then widget_control, colorID, SET_BUTTON=0
        if widget_info(pageID,  /VALID_ID) then widget_control, pageID,  SET_BUTTON=0
        if widget_info(wxyID,   /VALID_ID) then widget_control, wxyID,   SET_BUTTON=0
        if widget_info(zxID,    /VALID_ID) then widget_control, zxID,    SET_BUTTON=0
        if widget_info(zyID,    /VALID_ID) then widget_control, zyID,    SET_BUTTON=0
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
pro MrZoom::Pan, event
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
            
            ;Hide the plot being panned. Setting the property will casue the
            ;graphic window to be refreshed with the FOCUS graphic hidden.
            self.focus -> SetProperty, HIDE=1   ;;;;;;;;;;;;;
            
            ;Disable refreshing then turn hide off
            self -> Refresh, /DISABLE
            self.focus -> SetProperty, HIDE=0
        
            ;Turn on motion events
            self -> On_Off_Motion_Events, /ON
        endcase
        
        2: begin    ;motion event
            ;Get the object of focus
            self.focus -> GetProperty, XRANGE=xrange, YRANGE=yrange

            ;Convert to data coordinates.
            x = [self.x0, event.x]
            y = [self.y0, event.y]
            xy = self.focus -> ConvertCoord(x, y, /DEVICE, /TO_DATA)
            
            ;How much did the mouse move?
            delta_x = xy[0,1] - xy[0,0]
            delta_y = xy[1,1] - xy[1,0]
            
            ;A drag to the right will have delta_x < 0. Subtracting will then add the
            ;difference, panning more positive data into the window.
            xrange = xrange - delta_x
            yrange = yrange - delta_y

            ;Set the new ranges
;            self.focus -> SetProperty, XRANGE=xrange, YRANGE=yrange
            self.focus -> SetProperty, HIDE=0, XRANGE=xrange, YRANGE=yrange
            self -> CopyPixmap
            self.focus -> Draw, /NOERASE
            
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
            self -> Apply_Bindings, self.focus, /XAXIS, /YAXIS, /CAXIS
            self -> Refresh
        endcase
    endcase
end


;+
;   Unbind one set of objects from all others. This method is private. Call the Bind
;   method with the /REMOVE keyword set.
;
; :Params:
;       BINDINGLIST:        in, required, type=linked list
;                           A linked list of various sets of object bindings from which
;                               `THEOBJECTS` will be removed.
;       THEOBJECTS:         in, required, type=objarr()
;                           The objects whose axes are to be unbound.
;-
pro MrZoom::UnBind, bindingList, theObjects
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    if obj_valid(bindingList) eq 0 then return
            
    ;See how many different sets of bindings there are.
    nBindings = bindingList -> Get_Count()
    
    ;Step through each binding set
    for i = 0, nBindings - 1 do begin
        thisBinding = bindingList -> Get_Item(i)
        void = ismember(theObjects, thisBinding, NONMEMBER_INDS=iKeep, N_NONMEMBERS=nKeep)
        
        ;If only one object will be left in this set of bindings, remove the entire set.
        ;Otherwise, trim out the unwanted bindings.
        if nKeep le 1 $
            then bindingList -> Delete, i $
            else bindingList -> Replace, thisBinding[iKeep], i
    endfor
end


;+
;   Return to initial, unzoomed x- and y-ranges
;-
pro MrZoom::UnZoom, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Set the x- and y-range
    self.focus -> getProperty, INIT_XRANGE=init_xrange, INIT_YRANGE=init_yrange
    self.focus -> setProperty, XRANGE=init_xrange, YRANGE=init_yrange
    
    ;Update Bindings
    self -> Apply_Bindings, self.focus, /XAXIS, /YAXIS
    
    ;Redraw
    self -> Draw
end


;+
;   Print the heap variable numbers of the objects that are bound together.
;-
pro MrZoom::whichBindings
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
pro MrZoom::Wheel_Zoom_XY, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = error_message()
        return
    endif

    ;Get the current x- and y-range
    self.focus -> getProperty, XRANGE=xrange, YRANGE=yrange

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
    self -> Refresh, /DISABLE
    self.focus -> SetProperty, XRANGE=xrange, YRANGE=yrange
    self -> Apply_Bindings, self.focus, /XAXIS, /YAXIS
    self -> Refresh
end


;+
;   Roll the mouse button forward or backward to zoom in or out in the color dimension
;   by the zoom factor. The zoom factor for the x- and y-directions can be different.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrZoom::Wheel_Zoom_Color, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = error_message()
        return
    endif

    ;Get the current x- and y-range
    self.focus -> getProperty, RANGE=range

    ;Zooming in and out is determined by the sign of events.clicks.
    ;   events.clicks < 0 if the scroll wheel is moved forward -- zoom in
    ;   events.clicks > 0 if the scroll wheel is moved backward -- zoom out
    ;Make sure range[0] < range[1]
    delta = abs(range[1] - range[0]) * self.zoomfactor[0] * event.clicks
    range[0] = range[0] + delta
    range[1] = range[1] - delta
    if range[0] gt range[1] then range[0] = range[1] - delta
    
    ;Set the zoomed x- and y-range
    self.focus -> SetProperty, RANGE=range
    self -> Apply_Bindings, self.focus, /CAXIS

    ;Redraw
    self -> draw
end


;+
;   Roll the mouse button forward or backward "Page" through the image data. This is
;   possible only if the image has more than 2 dimensions.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrZoom::Wheel_Zoom_Page, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = error_message()
        return
    endif

    ;Get the current x- and y-range
    self.focus -> GetProperty, IMAGE=image, IDISPLAY=iDisplay

    ;Make sure the image has more than 2 dimensions
    ndims = n_elements(image)
    if ndims lt 3 then return
    
    ;How many pages are there?
    nPages = n_elements(image[0,0,*])
    
    ;Change pages. Stop at beginning and at the end
    iDisplay += event.clicks
    if iDisplay lt 0 then iDisplay = 0
    if iDisplay ge nPages then iDisplay = nPages - 1
    
    ;Set the page being displayed
    self.focus -> SetProperty, IDISPLAY=iDisplay
    self -> Apply_Bindings, self.focus, /CAXIS

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
pro MrZoom::XY_Zoom, event
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
    xy = self.focus -> ConvertCoord(x, y, /DEVICE, /TO_DATA)
    xrange = reform(xy[0,*])
    yrange = reform(xy[1,*])
    
    ;only zoom if the coordinate changed
    self -> Refresh, /DISABLE
    case self.zmode of
        1: begin
            if self.x0 ne event.x then begin
                self.focus -> SetProperty, XRANGE=xrange
                self -> Apply_Bindings, self.focus, /XAXIS
            endif
        endcase
        
        2: begin
            if self.y0 ne event.y then begin
                self.focus -> SetProperty, YRANGE=yrange
                self -> Apply_Bindings, self.focus, /YAXIS
            endif
        endcase
        
        else: ;Do nothing
    endcase
    
    ;Reset the active zoom and initial click properties.
    self.zmode = 0
    self.x0 = -1
    self.y0 = -1
    
    ;Re-draw the plot
    self -> Refresh
end


;+
;   Determine which "Zoom Menu" button was pressed and tell the draw widget which type of
;   events to pay attention to.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrZoom::Zoom_Menu_Events, event
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
    
    ;Determine if the button is set or not
    isSet = widget_info(event.id, /BUTTON_SET)
    
    ;Temporarily turn off zooming
    if strpos(zoom_type, 'WHEEL') ge 0 then self.wmode=0 else self.lmode=0

    ;If the button is set, we need to toggle it off and return
    if isSet then begin
        widget_control, event.id, SET_BUTTON=0
        return
    endif

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
        'NONE': self -> Turn_Zoom_Off, self.tlb
        'ZOOM X': self.lmode = 1
        'ZOOM Y': self.lmode = 2
        'BOX ZOOM': self.lmode = 4
        'PAN': self.lmode = 8
        'WHEEL ZOOM: XY': self.wmode = 1
        'WHEEL ZOOM: COLOR': self.wmode = 2
        'WHEEL ZOOM: PAGE': self.wmode = 4
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
pro MrZoom::cleanup
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
;   The initialization method.
;
; :Keywords:
;       LMODE:              in, optional, type=int
;                           The zoom mode associated with the left mouse button.
;       RMODE:              in, optional, type=int
;                           The zoom mode associated with the right mouse button.
;       WMODE:              in, optiona, type=int
;                           The zoom mode associated with the mouse wheel.
;       ZOOMFACTOR:         in, optional, type=float/fltarr(2)
;                           The zoom factor with which the mouse wheel will cause the
;                               x- and y-ranges to be adjusted. If only one value is given,
;                               it will be applied to both axes.
;-
function MrZoom::init, $
LMODE = lmode, $
RMODE = rmode, $
WMODE = wmode, $
ZOOMFACTOR = zoomfactor
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif
    
    ;Defaults
    SetDefaultValue, lmode, 0
    SetDefaultValue, rmode, 0
    SetDefaultValue, wmode, 0
    SetDefaultValue, zmode, 0
    SetDefaultValue, zoomfactor, [0.05, 0.05]
    
    ;Linked lists
    self.bind_x = obj_new('linkedlist')
    self.bind_y = obj_new('linkedlist')
    self.bind_z = obj_new('linkedlist')
    self.bind_c = obj_new('linkedlist')
    
    ;Set the property
    Self -> SetProperty, LMODE = lmode, $
                         RMODE = rmode, $
                         WMODE = wmode, $
                         ZOOMFACTOR = zoomfactor
                         
    return, 1
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrZoom__define, class
    compile_opt idl2
    
    class = {MrZoom, $
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