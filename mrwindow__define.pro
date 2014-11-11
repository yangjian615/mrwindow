; docformat = 'rst'
;
; NAME:
;       MrWindow__Define
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
;   The purpose of this method is to create a zoomable, resizeable, annotateable plot
;   window that organizes plots within an autmatically adjusting 2D plotting grid. Any
;   type of plot object can be added via the AddPlots method, so long as they conform to
;   the requirements below. Plots added with the Plot method are created with wePlot__define,
;   an object based off of cgPlot.
;
;   Required Methods::
;       GetProperty     -   A method for retrieving object properties
;       SetProperty     -   A method for setting object properties
;
;   Required Properties::
;       p_syzvar        -   The !P system variable configuration for the plot.
;       x_sysvar        -   the !X system variable configuration for the plot
;       y_sysvar        -   the !Y system variable configuration for the plot
;       xrange          -   the [min, max] x-ranges displayed
;       yrange          -   the [min, max] y-ranges displayed
;       zrange          -   the [min, max] z-ranges displayed
;
;   For details concerning zooming, cursor, annotation, and save options, see::
;       MrAbstractArrow__define.pro
;       MrCursor__define.pro
;       MrSaveAs__define.pro
;       MrAbstractText__define.pro
;       MrZoom__define.pro
;
;   For details concerning the automatically updating 2D plotting grid, see::
;       MrPlotLayout__define.pro
;
; :Examples:
;   What follows is one large example exhibiting the utility of MrWindow
;
;   ;Create a MrWindow object::
;       mywin = MrWindow()
;
;   ;Add three line plots: Cos(x), Sin(x), x^2. Specify the LAYOUT. Note how they
;   ;are adjusted to make room::
;       x = findgen(100)/99.0
;       y = sin(2*!pi*x)
;       z = cos(2*!pi*x)
;       w = x^2
;       Plot1 = mywin -> Plot(x, y, TITLE='Sin(x)', XTITLE='Time', YTITLE='Amplitude')
;       Plot2 = mywin -> Plot(x, z, TITLE='Cos(x)', XTITLE='Time', YTITLE='Amplitude', LAYOUT=[1,2,1,1])
;       Plot3 = mywin -> Plot(x, w, TITLE='x^2', XTITLE='Time', YTITLE='Amplitude', LAYOUT=[1,2,1,2])
;
;   ;Add an image to the layout.
;       image = dist(256)
;       imx = findgen(256)
;       imy = findgen(256)
;       Image1 = mywin -> Image(image, imx, imy, TITLE='Dist(256)', XTITLE='X-Title', YTITLE='Y-Title', /SCALE, /AXES, CTINDEX=13)
;       mywin -> SetProperty, XMARGIN=[10,15], XSIZE=550, YSIZE=675, /DRAW
;
;   ;Give the image a colorbar
;       mywin -> whichobjects
;       mywin -> GetProperty, 3, RANGE=range
;       c1 = mywin -> Colorbar(CBLOCATION='RIGHT', CTINDEX=13, GRAPHIC=Image1, RANGE=range, /RIGHT, TITLE='CB Title')
;
;   ;Finally, add a contour plot with colorbar. Reporduce the "Filled Contour Plot"
;   ;example in the `Coyote Graphics Graphics Plot Gallery 
;   ;<http://www.idlcoyote.com/gallery/>`::
;       data = cgDemoData(26)
;       minValue = Floor(Min(data))
;       maxValue = Ceil(Max(data))
;       nLevels = 10
;       xtitle = 'X Axis'
;       ytitle = 'Y Axis'
;       cbTitle = 'Data Value'
;       cgLoadCT, 33, NColors=nlevels, Bottom=1, CLIP=[30,255]
;       contourLevels = cgConLevels(data, NLevels=nLevels, MinValue=minValue)
;
;       cont = mywin -> Contour(data, /FILL, LEVELS=contourLevels, $
;                               C_COLORS=bindgen(nlevels)+1B, /OUTLINE, POSITION=position, $
;                               XTITLE=xtitle, YTITLE=ytitle, DRAW=0)
;               
;       conCB = mywin -> Colorbar(NColors=nlevels, Bottom=1, Graphic=cont, CBLOCATION='RIGHT', $
;                                 Range=[MinValue, MaxValue], /RIGHT, Divisions=nlevels, /Discrete, $
;                                 Title=cbTitle, TLocation='Right')
;
;   ;Remove the line plots. Fill holes in layout. Trim off empty rows and columns.
;       mywin -> Remove, POSITION=[0,1,2], /FILLHOLES, /TRIMLAYOUT, /DRAW
;
;   ;Destroy the window
;       mywin -> Destroy
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;	Modification History::
;       04/22/2013  -   Written by Matthew Argall
;       04/23/2013  -   Added ::On_Off_Button_Events, ::On_Off_Motion_Events, 
;                           ::Draw_Events, ::Analysis_Menu_Events, and ::Zoom_Menu_Events.
;                           Removed ::Menu_Events. Left and right zoom modes are
;                           configurable, several events can take place at the same time,
;                           though it is getting a little complicated. Zoom events are
;                           negative if they are active (i.e. in the process of zooming).
;                           This way if both the left and right mouse button have a zoom
;                           mode that requires motion events, competing copy and draw
;                           cases can be avoided. - MRA
;       04/24/2013  -   Added the "zmode" property to say which zoom mode is currently
;                           active. The ::Draw_Events method was getting overly cluttered,
;                           and since only one zoom mode can be active at a time, this
;                           works out well. This takes the place of [r,l]mode < 0.
;                       Added ::Overplot and ::AddOverPlot methods. - MRA
;       04/25/2013  -   Extracted all window parts from MrLinePlot__Define for easier
;                           inheritance. Renamed ::Analysis_Menu_Events to
;                           ::Cursor_Menu_Events, changed the "Analysis" menu button to
;                           "Cursor" and added a 'none' option to the cursor menu. Added
;                           AMODE, RMODE, LMODE, and SAVEDIR to the list of input keywords.
;                           Added ::Show_XY. - MRA
;       05/11/2013  -   Added Coyote Graphics saving capabilities: CreatePostScriptFile,
;                           Output, SaveAsRaster, cgSaveAs_Events. - MRA
;       05/16/2013  -   Added the Annotate menu, ::Move_Text, and ::Place_Text. - MRA
;       05/17/2013  -   Moved ::Move_Text and ::Place_Text into MrAbstractText class.
;                           Changed ANDs to EQs in the Draw_Events method for annotation
;                           events to indicate that only one text mode can function at a
;                           time. - MRA
;       05/18/2013  -   Moved the SaveAs, Zoom, and Cursor menu and methods to their own
;                           respective abstract classes.
;       05/28/2013  -   Inherit MrAbstractArrow to incorporate adding, moving, rotating,
;                           stretching, and removing arrows. Nested Text and Arrow menus
;                           under the "Annotate" menu in the menu bar. - MRA
;       06/16/2013  -   Can now get and set properties for objects contained within the
;                           MrWindow window via the GetProperty and SetProperty
;                           methods. Added the whichObjects method. - MRA
;       06/19/2013  -   Value_Locate was used to search for object matches via MrIsMember,
;                           which was throwing errors. When setting properties of the plot
;                           layout, SetProperty now calls the SetPositions method instead
;                           of MrPlotLayout::SetProperty. This is so that the plot
;                           positions are adjusted to the new layout. - MRA
;       06/20/2013  -   Pixmap was not being copied for cross-hairs. Needed to check for
;                           bit 2, not EQ 2, as multiple cursor events can take place
;                           simultaneously. Fixed. - MRA
;       07/04/2013  -   Forgot to pass keyword to the BUILD method so windows were not
;                           being made the proper size. Fixed. - MRA
;       07/05/2013  -   Added the CREATEGUI keyword. - MRA
;       07/09/2013  -   Implemented the MrAbstractAnalysis class. A re-Focus was needed
;                           after plotting is done in the Draw method. - MRA
;       07/10/2013  -   3D spectrograms can now be plotted from CDF files. - MRA
;       07/20/2013  -   The height of the menu and status bars are now taken into account
;                           when resizing the widget base. This allows the plot layout to
;                           be determined more accurately in the sense that the margins
;                           are now the proper size. This entailed adding the menuID
;                           property to the class definition. Renamed the Resize_Events to
;                           TLB_Resize_Events, added the ResizeDrawWidget method. - MRA
;       07/21/2013  -   Added the WMODE and DISPLAY keywords. Previously, when BUILD=0 or
;                           and the MrSaveAs::Output method was called, an error
;                           REALIZE=0 would occur because self.winID would be an invalid
;                           window ID. Furthermore, to do a screen capture, something had
;                           to have first been drawn in a window. This could not happen
;                           without first realizing the window... Recalculate the plot
;                           positions if DRAW is set in the SetProperty method. Before,
;                           the plot bled off the window. - MRA.
;       08/23/2013  -   Focus is now always done. Removed from Cursor menu. Never turn
;                           off button events so that FOCUS always works. Moved the Focus
;                           method from MrCursor to here. Added example that works. - MRA
;       08/24/2013  -   Focus was saving the index of a subarray of objects, not an
;                           index into the container. Fixed.  Inherit CDF_Plot instead
;                           of MrAbstractCDF. - MRA
;       08/30/2013  -   Bindings are now applied when [XYC]Range are set via the
;                           SetProperty method. - MRA
;       09/15/2013  -   Focus simply returns if there are no objects in the container. - MRA
;       09/23/2013  -   Cleanup checks if windows are still open before deleting them.
;                           Added the IsZoomable method as a quick way of determining if
;                           the current object is zoomable. IFOCUS changed from an object's
;                           container index to the object's reference. Added the FOCUS
;                           property. Zoom events are forwarded to MrZoom::Draw_Events - MRA
;       2013/11/22  -   Renamed DISPLAY to BUFFER, removed the BUILD option, changed
;                           REALIZE to DRAW, renamed CREATEGUI to NOGUI. Input options
;                           have been simplified sigificantly. Added NAME keyword. - MRA
;       2014/01/10  -   Prevent the graphics from drawing twice upon realization by updating
;                           the _realized property in ::Notify_Realize and by putting a
;                           Return after calling ::Realize in ::Draw. Two draws were
;                           occurring because ::Notify_Realize calls ::Draw. - MRA
;       2014/01/22  -   ::TLB_Resize_Events renamed to ::TLB_Events. TLB_Kill_Request_Events
;                           now destroys the window when the native OS (x)=close button
;                           is pressed. "File | Close" now destroys the widget as well.
;                           "File | Destroy" was removed. Removed unused keywords. - MRA
;       2014/02/12  -   Resize created a window when graphics was buffered. Fixed. - MRA
;       2014/03/09  -   Added the GetSelect, _SetSelect, IsSelected, HitTest, and Focus2
;                           methods. Added the _Selection property. - MRA
;       2014/03/10  -   Added the Show and SetCurrent methods. - MRA
;       2014/03/12  -   Removed the old Focus method and renamed Focus2 to Focus. Also
;                           removed the old GetSelect and SetSelect methods. - MRA
;       2014/03/13  -   Disinherit the MrSaveAs class, but keep it around as an object
;                           property. Added the GetRefresh method for convenience. - MRA
;       2014/03/26  -   Set the current display when the graphics window is made current. - MRA
;       2014/04/01  -   Check if CDF cancel buttons were pressed to prevent errors. - MRA
;       2014/04/11  -   Added the SetSave method. - MRA
;       2014/05/12  -   Save method now knows when and how to turn Refresh on. - MRA
;       2014/05/15  -   Set the grid layout after the window has been created to ensure
;                           graphics are positioned properly. - MRA
;       2014/05/16  -   The Focus method selects only the fore-most graphics object. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to provide output when the PRINT procedure is used.
;
; :Private:
;-
function MrWindow::_OverloadPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    ;Get the class name and the heap identifier of the object
    objClass = obj_class(self)
    heapNum = obj_valid(self, /GET_HEAP_IDENTIFIER)
    objStr = string(FORMAT='(%"%s  <%i>")', objClass, heapNum)
    
    ;Get descriptions
    self -> whichDataObjects, dataText
    self -> whichAnnotateObjects, annText
    infoStr = [[dataText], [annText]]

    ;Combine the results
    outText = [[objStr], [infoStr]]

    return, outText
end


;+
;   Determine if objects
;-
pro MrWindow::_SetSelect, object, $
ADD=add, $
ALL=all, $
CLEAR=clear, $
TOGGLE=toggle, $
UNSELECT=unselect
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = cgErrorMsg()
        return
    endif
    
    ;Add the object to the selection?
    if keyword_set(add) then begin
        self._selection -> Add, object
        return
    endif
    
    ;Add all objects to the selection?
    if keyword_set(all) then begin
        allObjs = self -> Get(/ALL)
        self._selection -> Remove, /ALL
        self._selection -> Add, allObjs
        return
    endif
    
    ;Clear all selections?
    if keyword_set(clear) then begin
        self._selection -> Remove, /ALL
        return
    endif
    
    ;Toggle the object?
    if keyword_set(toggle) then begin
        tf_contained = self._selection -> IsContained(object)
        if tf_contained eq 1 $
            then self._selection -> Remove, object $
            else self._selection -> Add, object
        return
    endif
    
    ;Unselect the object?
    if keyword_set(unselect) then begin
        self._selection -> Remove, object
        return
    endif
    
    ;Clear the selection and add the object?
    self._selection -> Remove, /ALL
    self._selection -> Add, object
end


;+
;   Build the GUI. This is separate from the realization method so that custom GUI
;   configurations can be used. Also, so that the GUI can be build in the background
;   without being realized right away.
;
; :Private:
;
; :Keywords:
;       XSIZE:          in, optional, type=boolean, default=600
;                       Width of the draw widget.
;       YSIZE:          in, optional, type=boolean, default=340
;                       Height of the draw widget.
;-
pro MrWindow::BuildGUI, $
XSIZE = xsize, $
YSIZE = ysize
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Default window size
    if n_elements(xsize) eq 0 then if self.xsize eq 0 then xsize = 600 else xsize = self.xsize
    if n_elements(ysize) eq 0 then if self.ysize eq 0 then ysize = 600 else ysize = self.ysize
    self.xsize = xsize
    self.ysize = ysize
    
    ;Make a top-level base with a menu bar
    self.tlb = widget_base(TITLE=self.title, /COLUMN, /TLB_SIZE_EVENTS, $
                           UVALUE={object: self, method: 'TLB_Events'}, $
                           MBAR=menuID, XOFFSET=100, YOFFSET=0, UNAME='tlb', $
                           /TLB_KILL_REQUEST_EVENTS)
    self.menuID = menuID

    ;Make the file menu.
    fileID = widget_button(menuID, VALUE='File', /MENU, EVENT_PRO=butto)
    button = widget_button(fileID, VALUE='Open CDF', UVALUE={object: self, method: 'File_Menu_Events'})
    button = widget_button(fileID, VALUE='Close', UVALUE={object: self, method: 'Destroy'}, /SEPARATOR)

    ;Create a "Save As" menu for the menu bar.
    self._SaveAs -> SetProperty, GROUP_LEADER=self.tlb
    self._SaveAs -> Create_SaveAs_Menu, menuID, /MENU

    ;Make the edit menu in the menu bar
    editID = widget_button(menuID, VALUE='Edit', /MENU)
    button = widget_button(editID, VALUE='Layout',            UVALUE={object: self, method: 'AdjustLayout_Property'})
    button = widget_button(editID, VALUE='Move',              UVALUE={object: self, method: 'AdjustLayout_Move'})
    button = widget_button(editID, VALUE='Remove',            UVALUE={object: self, method: 'AdjustLayout_Remove'})
;    button = widget_button(editID, VALUE='Plot', SENSITIVE=0, UVALUE={object: self, method: 'AdjustLayout'})
;    button = widget_button(editID, VALUE='Copy', SENSITIVE=0, UVALUE={object: self, method: 'AdjustLayout_Copy'})

    ;Create the Zoom Menu
    self -> Create_Zoom_Menu, menuID

    ;Create the Cursor Menu. Do not make a button for FOCUS
    self -> Create_Cursor_Menu, menuID, /MENU, /NONE, /CROSS_HAIRS, /GET_POINT, /SHOW_XY
    
    ;Create the Analysis Menu
    self -> Create_Analysis_Menu, menuID

    ;Create manipulation menu
;    self -> Create_Manipulate_Menu, menuID

    ;Create the Arrow and Text Menus
;    annotateID = widget_button(menuID, VALUE='Annotate', /MENU)
;    self -> Create_Arrow_Menu, annotateID
;    self -> Create_Text_Menu, annotateID

    ;Make the draw widget. We still need to get the Window ID in which the plot
    ;is drawn, so get that when the window is realized (see note within ::init).
    if !version.os_family eq 'unix' then retain = 2 else retain = 1

    self.drawID = widget_draw(self.tlb, XSIZE=xsize, YSIZE=ysize, RETAIN=retain, $
                              /BUTTON_EVENTS, /WHEEL_EVENTS, $
                              UVALUE={object: self, method: 'Draw_Events'}, $
                              NOTIFY_REALIZE='MrWindow_Notify_Realize', $
                              EVENT_PRO='MrWindow_Events')

    ;print the data coordinates of the mouse below the draw widget
    self.statusID = widget_label(self.tlb, VALUE=' ', /DYNAMIC_RESIZE, UVALUE={object: self, method: 'Status'})
end


;+
;   Copy the pixmap to the display window.
;
; :Private:
;-
pro MrWindow::copyPixmap
    compile_opt strictarr
    
    ;copy the pixmap to the draw window
    wset, self.winID
    device, COPY=[0, 0, self.xsize, self.ysize, 0, 0, self.pixID]
end


;+
;   The purpose of this method is to draw the plot in the draw window. The plot is
;   first buffered into the pixmap for smoother opteration (by allowing motion events
;   to copy from the pixmap instead of redrawing the plot, the image does not flicker).
;-
pro MrWindow::Draw, $
ERASE=erase
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Return if we are not refreshing.
    if self._refresh eq 0B then return
    
    ;Realize the widget if need be. This will cause Notify_Realize to call the
    ;Draw method. Thus, after realizing, we can exit.
    if self.buffer eq 0 and self._realized eq 0 then begin
        self -> Realize
        return
    endif
    
    ;Set the current window to the pixmap window and erase it
    if (!d.flags and 256) ne 0 then begin
        wset, self.pixID
        erase
    endif
    
    ;Get all objects
    allObj = self -> Get(/ALL, COUNT=nObj)
    
    ;Create an empty plot if none exists.
    cgErase, 'white'

    ;Draw all of the objects.
    for i = 0, nObj - 1 do begin
        if i eq 0 then noerase = 0 else noerase = 1
        allObj[i] -> Draw, NOERASE=noerase
    endfor
    
    ;Reset the focus.
    if self.nplots gt 0 then self -> Focus
    
    ;Copy plot from the pixmap
    if (!d.flags and 256) ne 0 then begin
        wset, self.winID
        device, COPY=[0, 0, self.xsize, self.ysize, 0, 0, self.pixID]
    endif
end


;+
;   Handle events from the draw widget
;
;   NOTES on copyPixmap::
;       Box Zoom    -   copyPixmap at beginning of every motion event
;       Cross Hairs -   copyPixmap at beginning of every motion event
;
;   NOTES of Draw::
;       Box Zoom    -   Draw event after the button is released (see ::Box_Zoom)
;       Pan         -   Draw at end of every motion event
;
;   NOTES on Zoom Event Sequence::
;       Box Zoom::
;           1) Button down  -- Record new min ranges
;           2) Motion       -- COPYPIXMAP, Draw box
;           3) Button Up    -- Record new max ranges, set new range, DRAW
;       XY Zoom::
;           1) Button down  -- Record new min range
;           2) Button down  -- Record new max range, set new ranges, DRAW
;       Pan::
;           1) Button down  -- Record "pan" point
;           2) Motion       -- Pan, Set new "pan" point, Draw
;           3) Button up    -- Reset configuration
;
;   NOTES on Cursor Event Sequence::
;       Get Point::
;           * Button down   -- Print coordinates of clicked point
;       Cross Hair::
;           * Motion        -- COPYPIXMAP, draw cross hairs
;       Show [X,Y]::
;           * Motion        -- Update status bar with coordinates of cursor
;
;   NOTES on Text Event Sequence::
;       Add Text::
;           * Button down   -- DRAW
;       Move Text::
;           1) Button down  -- Select text
;           2) Motion       -- COPYPIXMAP, draw text
;           3) Button Up    -- Fix text, turn off motion events, DRAW
;       Rotate Text::
;           1) Button down  -- Select text
;           2) Motion       -- COPYPIXMAP, draw text
;           3) Button Up    -- Fix text, turn off motion events, DRAW
;
; :Private:
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::Draw_Events, event
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;If a button press happened, we need to focus first. If anything else happened,
    ;we need to know if the object of focus is zoomable right away.
    if event.type ne 0 then tf_zoomable = self -> isZoomable()

;---------------------------------------------------------------------
;Button Press Events /////////////////////////////////////////////////
;---------------------------------------------------------------------

    if event.type eq 0 then begin
        self -> Focus, event      ; Always Focus
        tf_zoomable = self -> isZoomable()
        
        ;Cursor Events
        if ((self.cmode and 1) gt 0) then self -> Get_Point, event  ;Get Point
        
        ;Analysis Events
        if ((self.amode[0] and 1) gt 0) then self -> Get_Data_Point, event ;Get Data Value
        if ((self.amode[0] and 2) gt 0) then self -> Get_Interval, event   ;Get Interval, Average
        
        ;Arrow Events
        if self.arrowmode[0] eq 1 then self -> Place_Arrow, event     ;Place arrow
        if self.arrowmode[0] eq 2 then self -> Adjust_Arrow, event    ;Move arrow
        if self.arrowmode[0] eq 4 then self -> Adjust_Arrow, event    ;Stretch/Rotate arrow
        if self.arrowmode[0] eq 8 then self -> Remove_Arrow, event    ;Remove arrow
        
        ;Text Events
        if self.textmode[0] eq 1 then self -> Place_Text, event     ;Add text
        if self.textmode[0] eq 2 then self -> Adjust_Text, event    ;Move text
        if self.textmode[0] eq 4 then self -> Adjust_Text, event    ;Rotate text
        if self.textmode[0] eq 8 then self -> Remove_Text, event    ;Remove text

        ;Zoom Events
        if tf_zoomable eq 1 then self -> MrZoom::Draw_Events, event
    endif
    
;---------------------------------------------------------------------
;Button Release Events ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 1 then begin
        ;Analysis Mode -- must be active
        if self.amode[1] eq 1 then begin
            if ((self.amode[0] and 2) gt 0) then self -> Get_Interval, event   ;Get Interval, Average        
        endif
        
        ;Arrow Events -- must be active
        if self.arrowmode[1] eq 1 then begin ;Button Up turns motion events off.
            if self.arrowmode[0] eq 1 then self -> Place_Arrow, event   ;Create Arrow
            if self.arrowmode[0] eq 2 then self -> Adjust_Arrow, event  ;Move Arrow
            if self.arrowmode[0] eq 4 then self -> Adjust_Arrow, event  ;Stretch/Rotate Arrow
        endif

        ;Text Events -- must be active
        if self.textmode[1] eq 1 then begin
            if self.textmode[0] eq 2 then self -> Adjust_Text, event    ;Move Text
            if self.textmode[0] eq 4 then self -> Adjust_Text, event    ;Rotate Text
        endif
        
        ;Zoom Events
        if tf_zoomable eq 1 then self -> MrZoom::Draw_Events, event
    endif

;---------------------------------------------------------------------
;Motion Events ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 2 then begin
        ;Do not compete for copying the pixmap
        if self.zmode eq 4 || ((self.cmode and 2) gt 0) || $             ;Box Zoom, Cross Hairs
        
            ;Text events must be active AND set
           ( self.textmode[1] eq 1 && ( self.textmode[0] eq 2 || $      ;Move Text
                                        self.textmode[0] eq 4 ) ) || $  ;Rotate Text
           
           ;Arrow events must be active AND set                             
           ( self.arrowmode[1] eq 1 && ( self.arrowmode[0] eq 1 || $      ;Place arrow
                                         self.arrowmode[0] eq 2 || $      ;Move arrow
                                         self.arrowmode[0] eq 4 ) ) || $  ;Stretch/Rotate arrow
                                         
           ( self.amode[1] eq 1 && ( ((self.amode[0] and 2) gt 0) ) ) $   ;Get Interval, Average
            then self -> copyPixmap
            
        ;Analysis Mode -- must be active
        if self.amode[1] eq 1 then begin
            if ((self.amode[0] and 2) gt 0) then self -> Get_Interval, event   ;Get Interval, Average        
        endif

        ;Zoom Events
        if tf_zoomable eq 1 then self -> MrZoom::Draw_Events, event, COPY_PIX=0, DRAW=0
        
        ;Cursor Events
        if ((self.cmode and 2) gt 0) then self -> Cross_Hairs, event
        if ((self.cmode and 4) gt 0) then self -> Show_XY, event
        
        ;Text Events
        if self.textmode[1] eq 1 then begin
            if self.textmode[0] eq 2 then self -> Adjust_Text, event    ;Move Text
            if self.textmode[0] eq 4 then self -> Adjust_Text, event    ;Rotate Text
        endif
        
        ;Arrow Events
        if (self.arrowmode)[1] eq 1 then begin
            if self.arrowmode[0] eq 1 then self -> Place_Arrow, event     ;Place arrow
            if self.arrowmode[0] eq 2 then self -> Adjust_Arrow, event    ;Move arrow
            if self.arrowmode[0] eq 4 then self -> Adjust_Arrow, event    ;Stretch/Rotate arrow
        endif

        ;Do not compete for drawing (Pan)
;        if self.zmode eq 8 then self -> Draw                            ;Pan
    endif

;---------------------------------------------------------------------
;Wheel Events ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 7 then if (tf_zoomable eq 1) then self -> Wheel_Zoom, event
end


;+
;   Destroy the object and, if it exists, the widget.
;
; :Private:
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::Destroy, event
    compile_opt strictarr
    
    ;Destroy the object and the widget, if it still exists
    obj_destroy, self
end


;+
;   Destroy the object and, if it exists, the widget.
;
; :Private:
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::Error_Handler, event
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    self -> MrAbstractAnalysis::Error_Handler
    self -> Turn_Everything_Off
end


;+
;   Handle events triggered by the File Menu.
;
; :Private:
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::File_Menu_Events, event
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if obj_valid(cdf_obj) then obj_destroy, cdf_obj
        if (obj_valid(thePlot)     eq 1) && (self -> IsContained(thePlot)     eq 0) then obj_destroy, thePlot
        if (obj_valid(theColorbar) eq 1) && (self -> IsContained(theColorbar) eq 0) then obj_destroy, theColorbar
        void = cgErrorMsg()
        return
    endif
    
    ;Get the name of the button that was pressed
    widget_control, event.id, GET_VALUE=button_name

;---------------------------------------------------------------------
;Which Menu Button? //////////////////////////////////////////////////
;---------------------------------------------------------------------
    case strupcase(button_name) of

    ;---------------------------------------------------------------------
    ;OPEN CDF ////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        'OPEN CDF': begin
            ;Set this window as the current window
            self -> SetCurrent
            
            ;Create the plot from the data
            cdf_obj = obj_new('CDF_PLOT')
            if obj_valid(cdf_obj) eq 0 then return
            
            thePlot = cdf_obj -> Plot(GROUP_LEADER=self.tlb, $
                                      DISPLAY_TYPE=display_type, $
                                      /CURRENT)
            if obj_valid(thePlot) eq 0 then return
            
            ;Add Spectrograms
            if display_type eq '3D_SPECTROGRAM' || display_type eq 'SPECTROGRAM' then begin
                ;Make sure there is enough room for a colorbar.
                if self.oxmargin[1] lt 15 then self -> SetProperty, OXMARGIN=[self.xmargin[0],15]
            endif
            
            ;Draw and destroy the object
            self -> Draw
            obj_destroy, cdf_obj
        endcase
        
        else: message, 'Button "' + button_name + '" not recognized.'
    endcase
end


;+
;   Determine if a graphics object was clicked. If no keyboard modifiers are pressed,
;   the fore-most object will be selected. If keyboard modifiers are pressed, the fore-
;   most object will be selected.
;
; :Private:
;
; :Params:
;       EVENT:          in, optional, type=structure/int
;                       The event returned by the windows manager. If not given, the
;                           currently selected graphic will become the object of focus.
;                           If EVENT is an integer, then it is the index value of the
;                           object within the container on which to focus.
;-
pro MrWindow::Focus, event
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = cgErrorMsg()
        return
    endif

;---------------------------------------------------------------------
;Event? //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    event_type = size(event, /TNAME)
    if event_type eq 'STRUCT' then begin

        ;Only listen to button presses.
        if event.type ne 0 then return
    
    ;---------------------------------------------------------------------
    ;Find the Objects that Were Clicked //////////////////////////////////
    ;---------------------------------------------------------------------
        ;Get all of the objects that are selectable
        newSelect = self -> HitTest(event.x, event.y, COUNT=nHits, $
                                    ISA=['MRPLOT', 'MRIMAGE', 'MRCOLORBAR', 'MRCONTOUR'])
        if nHits eq 0 then return

        ;Were modifiers pressed?
        ;   If not, select the top-most object.
        ;   If said object is overplotted, choose its target instead.
        if total(event.modifiers) gt 0 then begin
            newSelect = newSelect[nHits-1]
            if newSelect -> GetOverplot(TARGET=target) then newSelect = target
        endif else begin
            newSelect = newSelect[nHits-1]
            if newSelect -> GetOverplot(TARGET=target) then newSelect = target
        endelse

        ;Remove all selected items
        self._selection -> Remove, /ALL
        
        ;Add the selected items
        self._selection -> Add, newSelect
        
;---------------------------------------------------------------------
;Container Index Given? //////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else if event_type ne 'UNDEFINED' then begin
        ;Remove other selections
        self._selection -> Remove, /ALL
        
        ;Get the new selection
        newSelect = self -> Get(POSITION=index)
        self._selection, Add, newSelect
        
    endif else begin
        newSelect = self -> GetSelect()
    endelse

;---------------------------------------------------------------------
;Set Focus ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(newSelect) eq 1 && obj_valid(newSelect) $
        then newSelect -> RestoreCoords
end


;+
;   A simple way of obtaining the window's name.
;
;   :Returns:
;       NAME:           The name of the window
;-
function MrWindow::GetName
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    return, self.name
end


;+
;   The purpose of this method is to retrieve object properties from the MrWindow
;   instance itself or any of the objects contained within it.
;
; :Params:
;       INDEX:              in, optional, type=int
;                           If given, the index within the container of the object whose
;                               properties are to be retrieved.INDEX can be obtained via
;                               the whichObjects method.
;
; :Keywords:
;       NAME:               out, optional, type=string
;                           Name to be given to the window. Windows can be accessed by
;                               name via the GetMrWindows function.
;       REFRESH:            out, optional, type=boolean
;                           If set, the display will be refreshed after the window is
;                               created. This will cause the widget to be realized and
;                               the contents to be drawn.
;       SAVEAS:             out, optional, type=object
;                           MrSaveAs object reference with postscript and ImageMagick
;                               settings.
;       SAVEDIR:            out, optional, type=string
;                           The directory in which plots will be saved.
;       THEOBJ:             out, optional, type=boolean, default=0
;                           The object reference of the object referred to by `INDEX`.
;       XSIZE:              out, optional, type=long, default=512
;                           The width of the draw window in pixels
;       YSIZE:              out, optional, type=long, default=512
;                           The height of the draw window in pixels
;       _REF_EXTRA:         out, optional, type=any
;                           If `INDEX` is present, then any keyword accepted by the
;                               indicated object's GetProperty method. If not present,
;                               then any keyword accepted by the MrZoom, 
;                               MrCursor, or MrPlotLayout classes.
;                           
;-
pro MrWindow::GetProperty, index, $
NAME = name, $
REFRESH = refresh, $
SAVEDIR = save_dir, $
SAVEAS = saveas, $
THEOBJ = theObj, $
XSIZE = xsize, $
YSIZE = ysize, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

;---------------------------------------------------------------------
;INHERITED OBJECTS ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get properties of a particular object?
    if n_params() eq 1 then begin
        theObj = self -> Get(POSITION=index)
        theObj -> GetProperty, _EXTRA=extra
        return
    endif

;---------------------------------------------------------------------
;WINDOW PROPERTIES ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get Properties
    if arg_present(cmode)   then cmode   = self.cmode
    if arg_present(name)    then name    = self.name
    if arg_present(refresh) then refresh = self._refresh
    if arg_present(savedir) then savedir = self.savedir
    if arg_present(saveas)  then saveas  = self._SaveAs
    if arg_present(xsize)   then xsize   = self.xsize
    if arg_present(ysize)   then ysize   = self.ysize

    nExtra = n_elements(extra)
    
;---------------------------------------------------------------------
;CURSOR PROPERTIES ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    if nExtra gt 0 then begin
        void = MrIsMember(['CMODE'], extra, $
                        iCursor, COUNT=nMatches, COMPLEMENT=iExtra, $
                        NCOMPLEMENT=nExtra, /FOLD_CASE)
        
        if nMatches gt 0 then self -> MrCursor::GetProperty, _STRICT_EXTRA=extra[iCursor]
        if nExtra gt 0 then extra = extra[iExtra] else void = temporary(extra)
    endif
    
;---------------------------------------------------------------------
;ZOOM PROPERTIES /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if nExtra gt 0 then begin
        void = MrIsMember(['RMODE', 'LMODE', 'WMODE', 'ZOOMFACTOR'], extra, $
                        iZoom, COUNT=nMatches, COMPLEMENT=iExtra, $
                        NCOMPLEMENT=nExtra, /FOLD_CASE)
        
        if nMatches gt 0 then self -> MrZoom::GetProperty, _STRICT_EXTRA=extra[iZoom]
        if nExtra gt 0 then extra = extra[iExtra] else void = temporary(extra)
    endif
        
;---------------------------------------------------------------------
;LAYOUT PROPERTIES ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    if nExtra gt 0 then begin
        self -> MrGrLayout::GetProperty, _STRICT_EXTRA=extra
    endif
end


;+
;   A simple way of obtaining the window's refresh state.
;
;   :Returns:
;       REFRESH:            The refresh state of the window.
;-
function MrWindow::GetRefresh
    return, self._refresh
end


;+
;
;-
function MrWindow::GetSelect, $
COUNT=count
    return, self._selection -> Get(/ALL, COUNT=count)
end


;+
;   Determine if objects
;-
function MrWindow::HitTest, x, y, $
DIMENSIONS=dimensions, $
ISA=isa, $
COUNT=count
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = cgErrorMsg()
        return, obj_new()
    endif
    
    ;Get all of the objects
    allObjs = self -> Get(/ALL, ISA=isa, COUNT=nObjs)

    ;Figure out which ones were it
    hitObjs = objarr(nObjs)
    count = 0
    for i = 0, nObjs - 1 do begin
        tf_hit = allObjs[i] -> HitTest(x, y, DIMENSIONS=dimensions)
        if tf_hit eq 1 then begin
            hitObjs[count] = allObjs[i]
            count += 1
        endif
    endfor
    
    ;Did we hit something?
    if count eq 0 then return, obj_new()
    
    ;Trim the results and return
    hitObjs = hitObjs[0:count-1]
    return, hitObjs
end


;+
;   Determine if objects
;-
function MrWindow::IsSelected, object
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Error_Handler
        void = cgErrorMsg()
        return, obj_new()
    endif
    
    ;Check of the given object is selected
    tf_selected = self._selection -> IsContained(object)
    
    return, tf_selected
end


;+
;   Determines if the currently selected object is zoomable.
;
; :Private:
;
;   :Returns:
;       TF_ZOOMABLE:            Returns true (1) if the currently selected graphic
;                                   is able to be zoomed, false (0) if not.
;-
function MrWindow::IsZoomable
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif

    ;Get the object and determine if it is zoomable or not.
    oGraphic = self -> GetSelect(COUNT=count)
    if count ne 1 then return, 0
    
    oType = typename(oGraphic)
    tf_zoomable = MrIsMember([(*self.gTypes).data, (*self.gTypes).colorbar], oType)

    return, tf_zoomable
end


;+
;   Because the draw widget can be added to an external GUI, we must know when that GUI
;   is realized in order to obtain the widget ID of the draw widget.
;
; :Private:
;-
pro MrWindow::Notify_Realize, id
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;If a temporary pixmap window was being used, delete it.
    if WindowAvailable(self.winID) then wDelete, self.winID
    
    ;Get the window ID of the draw widget
    widget_control, self.drawID, GET_VALUE=winID
    self.winID = winID
    
    ;Read image data from this window.
    self._SaveAs -> SetProperty, WINID=id
    
    ;Indicate that the widget has been realized. This must come before calling Draw
    self._realized = 1B

    ;Draw the plot now that the window is realized.
    self -> Draw
end


;+
;   Turn DRAW_BUTTON_EVENTS on or off.
;
; :Private:
;
; :Keywords:
;       ON:                     in, required, type=boolean, default=0
;                               Turn motion events on.
;       OFF:                    in, optional, type=boolean, default=0
;                               Turn motion events off.
;-
pro MrWindow::On_Off_Button_Events, $
ON = on, $
OFF = off
    compile_opt strictarr
    
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
        ;
        ; Never turn off button events. FOCUS is always on.
        ;
    endif
end


;+
;   Turn DRAW_MOTION_EVENTS on or off. When motion events are turned off, all events
;   are cleared.
;
; :Private:
;
; :Keywords:
;       ON:                     in, required, type=boolean, default=0
;                               Turn motion events on.
;       OFF:                    in, optional, type=boolean, default=0
;                               Turn motion events off.
;-
pro MrWindow::On_Off_Motion_Events, $
ON = on, $
OFF = off
    compile_opt strictarr
    
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
        
        ;Turn motion events off only if nothing else needs them.
        ;CMODE can be in multiple modes, so check bits.
        ;ARROWMODE, TEXTMODE, and ZMODE can be in only one mode at a time.
        if (self.zmode eq 0) or (self.arrowmode[0] eq 0) and $              ;Zoom, Arrow
           (self.textmode[0] eq 0) and $                                    ;Text
           ((self.cmode and 2) eq 0) and ((self.cmode and 4) eq 0) and $    ;Cross Hairs, Show [X,Y]
           (self.amode[1] eq 0) $
        then begin
            widget_control, self.drawID, DRAW_MOTION_EVENTS=0
            widget_control, self.drawID, /CLEAR_EVENTS
        endif
    endif
end


;+
;   Destroy the widget without destroying the object.
;
; :Private:
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::quit, event
    compile_opt strictarr
    
    ;Destroy the widget
    widget_control, event.top, /DESTROY
    self._realized = 0B
end


;+
;   Realize the GUI. This method can be called as long as the object still exists (i.e.
;   the GUI can be closed then opened again).
;
; :Private:
;
; :Keywords:
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by the BUILD method is also excepted for
;                           keyword inheritance.
;-
pro MrWindow::Realize, $
_REF_EXTRA = extra
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Build the GUI
    if widget_info(self.tlb, /VALID_ID) eq 0 then self -> buildGUI, _STRICT_EXTRA=extra
        
    ;Realize the widget and start event handling
    widget_control, self.tlb, /REALIZE
    xmanager, 'MrWindow', self.tlb, /NO_BLOCK, EVENT_HANDLER='MrWindow_Events', $
              CLEANUP='MrWindow_CleanUp'
end


;+
;   The purpose of this method it enable or diable refreshing of the display.
;-
pro MrWindow::Refresh, $
DISABLE=disable
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Enable or disable.
    if n_elements(disable) eq 0 $
        then self._refresh = 1B $
        else self._refresh = ~keyword_set(disable)
    
    ;Re-draw the window contents.
    if self._refresh then self -> Draw
end

  
;+
;   This method resizes the draw window and redraws the plot to the adjusted size.
;
; :Private:
;
; :Params:
;       XSIZE:              in, required, type=long
;                           The width of the draw window in pixels
;       YSIZE:              in, required, type=long
;                           The height of the draw window in pixels
;-
pro MrWindow::ResizeDrawWidget, xsize, ysize
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Make sure the dimensions of the new window are >= 0. Ambiguous error results otherwise:
    ;"integer parameter out of range for operation"
    if xsize le 0 or ysize le 0 then message, 'XSIZE and YSIZE must be > 0.'

;---------------------------------------------------------------------
;Check Windows ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Check to see if a window is available to be resized. This may not be true if the
    ;widget has not been realized yet.
    drawIsValid = widget_info(self.drawID, /VALID_ID)
    winIsAvailable = WindowAvailable(self.winID)
    
    ;Make sure the widget has been built first
    if drawIsValid eq 0 && winIsAvailable eq 0 $
        then message, 'No window exists yet. Cannot resize. (Realize the widget first).'

;---------------------------------------------------------------------
;Pixmap //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Delete the pixmap, then create a new pixmap window at the new size
    wdelete, self.pixID
    self.pixID = MrGetWindow(XSIZE=xsize, YSIZE=ysize, /FREE, /PIXMAP)

;---------------------------------------------------------------------
;Draw Widget /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Resize the draw widget
    if drawIsValid then begin
        widget_control, self.drawID, DRAW_XSIZE=xsize, DRAW_YSIZE=ysize

;---------------------------------------------------------------------
;IDL Window/Pixmap ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Resize normal windows.
    endif else if winIsAvailable then begin
        wDelete, self.winID
        
        if self.buffer then pixmap = 1 else pixmap = 0
        self.winID = MrGetWindow(TITLE='MrWindow', XSIZE=xsize, YSIZE=ysize, /FREE, /PIXMAP)
    endif
    
    ;Update the object properties
    self.xsize = xsize
    self.ysize = ysize

    ;Recalculate and apply positions in resized window.
    refresh_in = self._refresh
    self -> Refresh, /DISABLE
    self -> CalcPositions
    self -> ApplyPositions
    self -> Refresh, DISABLE=~refresh_in
end


;+
;   Save the display to a file.
;
; :Params:
;       FILENAME:           in, optional, type=string, default='MrWindow.ps'
;                           Name of the file to which graphics output will be saved. The
;                               type of image file created is determined by the extension
;                               given. Options include "BMP", "EPS", "GIF", "JPEG", "JPG",
;                               "PDF", "PNG", "PS", "TIF", and "TIFF".
;-
pro MrWindow::Save, filename
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self._refresh = thisRefresh
        void = cgErrorMsg()
        return
    endif
    
    ;Get the refresh state
    thisRefresh = self._refresh
    
    ;Get the file extension
    void = cgRootName(filename, EXTENSION=extension)
    extension = strupcase(extension)
    
    ;Turn refresh on
    ;   - Raster files without ImageMagick require a snapshot. Must draw first.
    ;   - All other files will be drawn later.
    self._SaveAs -> GetProperty, IM_RASTER=im_raster
    if (im_raster eq 0) && (extension ne 'PS' && extension ne 'EPS') $
        then self -> Refresh $
        else self._refresh = 1

    ;Save the plot
    self._SaveAs -> Save, filename
    
    ;Return to the original refresh state.
    self._refresh = thisRefresh
end


;+
;   Set this window as the current window.
;-
pro MrWindow::SetCurrent
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Check if the window is listed
    tf_contained = !MR_WINDOWS -> IsContained(self, POSITION=source)
    
    ;Make it first
    if tf_contained $
        then !MR_WINDOWS -> Move, source, 0 $
        else message, 'Window is not listed. Cannot make current.'
    
    ;Set the display window
    wset, self.winID
end


;+
;   A method for configuring postscript and ImageMagick output. See MrSaveAs::SetProperties
;   for details.
;
; :Keywords:
;       _REF_EXTRA:         out, optional, type=any
;                           Any keyword accepted by MrSaveAs::SetProperty is accepted via
;                               keyword inheritance.
;-
pro MrWindow::SetFont, fontName, $
HARDWARE=hardware, $
HERSHEY=hershey, $
TT_FONT=tt_font, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Defaults
    hardware = keyword_set(hardware)
    hershey  = keyword_set(hershey)
    tt_font  = keyword_set(tt_font)
    
    ;If no font was selected, choose the current font
    if (hardware + hershey + tt_font) eq 0 then begin
        case !p.font of
            -1: hershey  = 1
             0: hardware = 1
             1: tt_font  = 1
             else: ;No other options
        endcase
    endif
    
    ;Dependencies
    if hardware + hershey + tt_font ne 1 then $
        message, 'One and only one font type may be chosen: HARDWARE | HERSHEY | TT_FONT.'

    ;Store the font
    case 1 of
        hershey:  self.font = -1
        hardware: self.font =  0
        tt_font:  self.font =  1
    endcase
;---------------------------------------------------------------------
;Hershey Fonts ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if hershey then begin
        if n_elements(fontName) eq 0 then fontName = 'Simplex Roman'
        
        ;Available Hershey fonts
        fonts = ['', '', 'SIMPLEX ROMAN', 'SIMPLEX GREEK', 'DUPLEX ROMAN', 'COMPLEX ROMAN', $
                 'COMPLEX ROMAN', 'COMPLEX ITALIC', 'MATH AND SPECIAL', '', 'GOTHIC ENGLISH', $
                 'SIMPLEX SCRIPT', 'COMPLEX SCRIPT', 'GOTHIC ITALIAN', 'GOTHIC GERMAN', $
                 'CYRILLIC', 'TRIPLEX ROMAN', 'TRIPLEX ITALIAN', '', 'MISCELLANEOUS']
                 
        ;Get the font number
        if size(fontName, /TYPE) eq 'STRING' then begin
            fontIndex = where(fonts eq strupcase(fontName), count)
            if count ne 1 then message, 'No font names match "' + fontName + '".'
            fontIndex = fontIndex[0]
        endif else begin
            fontIndex = fontName
        endelse
    
        ;Set the font by writing something to an invisible pixmap window
        case !d.name of
            'WIN': begin
                theWin = MrGetWindow(/FREE, /PIXMAP)
                xyouts, 0, 0, '!' + string(fontIndex, FORMAT='i0') + 'Change Font 123'
                wdelete, theWin
            endcase
            
            'X': begin
                theWin = MrGetWindow(/FREE, /PIXMAP)
                xyouts, 0, 0, '!' + string(fontIndex, FORMAT='i0') + 'Change Font 123'
                wdelete, theWin
            endcase
            
            'PS': begin
                ;Temporarily swtich out of PS mode
                if strupcase(!version.os_family) eq 'WINDOWS' $
                    then set_plot, 'WIN' $
                    else set_plot, 'x'
                
                ;Set the font
                theWin = MrGetWindow(/FREE, /PIXMAP)
                xyouts, 0, 0, '!' + string(fontIndex, FORMAT='i0') + 'Change Font 123'
                wdelete, theWin
                
                ;Return to postscript
                set_plot, 'PS'
            endcase
            
            else: ;Do nothing
        endcase
;---------------------------------------------------------------------
; True-Type Fonts ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else if tt_font then begin
        cgTT_Font, fontName
    
;---------------------------------------------------------------------
; Device or Hardware Fonts ///////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        
    endelse
    
    ;Store the font name
    self.fontName = fontName
    ;Set SaveAs properties
    if n_elements(extra) gt 0 then self._SaveAs -> SetProperty, _EXTRA=extra
end


;+
;   The purpose of this method is to set object properties from the MrWindow
;   instance itself or any of the objects contained within it.
;
; :Params:
;       INDEX:              in, optional, type=int
;                           If given, the index value of the object for which to obtain
;                               properties. INDEX can be obtained via the whichObjects
;                               method.
;
; :Keywords:
;       AMODE:              in, optional, type=int
;                           The analysis mode(s) to be enabled.
;       POSITION:           in, optional, type=fltarr(4)
;                           If `INDEX` is also provided, then this is the new position of
;                               the object. POSITION can be a 2-element [col,row] location
;                               or a standard 4-element position in normal coordinates.
;       RANGE:              in, optional, type=fltarr(2)
;                           The color range of an image, colorbar, etc.
;       SAVEDIR:            in, optional, type=string
;                           The directory in which plots will be saved.
;       XRANGE:             in, optional, type=fltarr(2)
;                           Range of the independent variable axis. Used with `INDEX`.
;       XSIZE:              in, optional, type=long, default=512
;                           The width of the draw window in pixels
;       YRANGE:             in, optional, type=fltarr(2)
;                           Range of the dependent variable axis. Used with `INDEX`.
;       YSIZE:              in, optional, type=long, default=512
;                           The height of the draw window in pixels
;
;       _REF_EXTRA:         out, optional, type=any
;                           If `INDEX` is present, then any keyword accepted by the
;                               indicated object's GetProperty method. If not present,
;                               then any keyword accepted by MrZoom,
;                               MrCursor, or MrPlotLayout.
;-
pro MrWindow::SetProperty, index, $
AMODE = amode, $
NAME = name, $
POSITION = position, $
RANGE = range, $
SAVEDIR = savedir, $
XSIZE = xsize, $
XRANGE = xrange, $
YRANGE = yrange, $
YSIZE = ysize, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

;---------------------------------------------------------------------
;CONTAINER OBJECTS ///////////////////////////////////////////////////
;---------------------------------------------------------------------

    if n_elements(index) ne 0 then begin
        ;Get object to be changed
        theObj = self -> Get(POSITION=index)
                
        ;Apply axis bindings if ranges were set. Set RANGE, if it was provided
        if n_elements(xrange) ne 0 then begin
            theObj -> SetProperty, XRANGE=xrange
            self -> Apply_Bindings, theOBj, /XAXIS
        endif
        
        if n_elements(yrange) ne 0 then begin
            theObj -> SetProperty, YRANGE=yrange
            self -> Apply_Bindings, theObj, /YAXIS
        endif
        
        if n_elements( range) ne 0 then begin
            theObj -> SetProperty, RANGE=range
            self -> Apply_Bindings, theObj, /CAXIS
        endif
        
        ;Set the object's position
        if n_elements(position) gt 0 then begin
            theObj -> GetProperty, LAYOUT=layout, POSITION=position
            
            ;Check if the object is within the auto-updating layout
            case n_elements(layout) of
                0: old_position = position
                3: old_position = self -> ConvertLocation(layout[2], /LIST_INDEX, /TO_COLROW)
                4: old_position = layout[2:3]
            endcase
            
            ;Set the position
            self -> SetPosition, old_position, position
        endif
        
        ;Set additional properties
        if n_elements(extra) ne 0 then theObj -> SetProperty, _EXTRA=extra

        if keyword_set(draw) then self -> Draw
        return
    endif

;---------------------------------------------------------------------
;WINDOW PROPERTIES ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Set Properties
    if n_elements(amode)   ne 0 then self.amode = amode
    if n_elements(name)    ne 0 then self.name = name
    if n_elements(xsize)   ne 0 then self -> ResizeDrawWidget, xsize, self.ysize
    if n_elements(ysize)   ne 0 then self -> ResizeDrawWidget, self.xsize, ysize
    if n_elements(savedir) ne 0 then self.savedir = savedir
        
    nExtra = n_elements(extra)

;---------------------------------------------------------------------
;CURSOR PROPERTIES ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    if nExtra gt 0 then begin
        void = MrIsMember(['CMODE'], extra, $
                         iCursor, COUNT=nMatches, COMPLEMENT=iExtra, $
                         NCOMPLEMENT=nExtra, /FOLD_CASE)
        
        if nMatches gt 0 then self -> MrCursor::SetProperty, _STRICT_EXTRA=extra[iCursor]
        if nExtra gt 0 then extra = extra[iExtra] else void = temporary(extra)
    endif
    
;---------------------------------------------------------------------
;ZOOM PROPERTIES /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if nExtra gt 0 then begin
        void = MrIsMember(['RMODE', 'LMODE', 'WMODE', 'ZOOMFACTOR'], extra, $
                        iZoom, COUNT=nMatches, COMPLEMENT=iExtra, $
                        NCOMPLEMENT=nExtra, /FOLD_CASE)
        
        if nMatches gt 0 then self -> MrZoom::SetProperty, _STRICT_EXTRA=extra[iZoom]
        if nExtra gt 0 then extra = extra[iExtra] else void = temporary(extra)
    endif
        
;---------------------------------------------------------------------
;LAYOUT PROPERTIES ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    if nExtra gt 0 then begin
        ;Recalculate the plot positions when layout changes.
        self -> MrGrLayout::SetProperty, _STRICT_EXTRA=extra
        self -> ApplyPositions
    endif

    ;Draw?
    self -> draw
end


;+
;   Expose or hide the graphics window
;
; :Params:
;       DOSHOW:         in, optional, type=boolean, default=1
;                       Set to 0 to hide the window or 1 to expose it.
;
; :Keywords:
;       ICONIFY:        in, optional, type=boolean
;                       Set to 1 to iconify the window. Set to 0 to de-iconify the window.
;-
pro MrWindow::Show, doShow, $
ICONIFY=iconify
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;To show, or not to show?
    show = n_elements(show) eq 0 ? 1 : keyword_set(show)
    
    ;Show the window
    wshow, self.winID, doShow, ICONIC=iconify
end


;+
;   The purpose of this method is to add a MrWindow object to the !MrWindow_Array system
;   variable. If the system variable does not exist, it will be created.
;
; :Private:
;-
pro MrWindow::SysVAdd, object
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Was a MrWindow object given?
    if obj_class(object) ne 'MRWINDOW' then $
        message, 'Only MrWindow objects can be added to !MR_WINDOWS.'

    ;Does the system variable exist?
    if self -> SysVExists() eq 0 $
        then defsysv, '!MR_WINDOWS', obj_new('MrWindow_Container')
    
    ;Add the window to the beginning of the container
    !MR_WINDOWS -> Add, self, POSITION=0
end


;+
;   The purpose of this method is to check if the !MrWindow_Array system variable exists.
;
; :Private:
;
; :Returns:
;       EXISTS:             True (1) if !MrWindow_Array exists. False (0) if not.
;-
function MrWindow::SysVExists
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Check if it exists.
    defsysv, '!MR_WINDOWS', EXISTS=exists
    return, exists
end


;+
;   The purpose of this method is to remove a MrWindow object from the !MrWindow_Array
;   system variable.
;
; :Private:
;-
pro MrWindow::SysVRemove, object
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Make sure MrWindow object was given
    if obj_class(object) ne 'MRWINDOW' then $
        message, 'Only MrWindow objects can be removed from !MR_WINDOWS.'
        
    ;Check if the system variable exists
    if self -> SysVExists() eq 0 then return
    
    ;Remove the window
    !MR_WINDOWS -> Remove, self
end


;+
;   This method resizes the top level base, then updates the contents of the draw widget
;   to be proportioned appropriately.
;
; :Private:
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::TLB_Events, event
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    sname = tag_names(event, /STRUCTURE_NAME)
    
    case sname of
        'WIDGET_KILL_REQUEST': self -> Destroy
        
        'WIDGET_BASE': begin
            ;Get the geometry of the menu and status bars.
            gMenu = widget_info(self.menuID, /GEOMETRY)
            gStatus = widget_info(self.statusID, /GEOMETRY)
    
            ;Subtract the height of the menu and status bars from the size of the top level base
            xNew = event.x
            yNew = event.y - gMenu.ysize - 2*gMenu.margin - gStatus.ysize - gStatus.margin
    
            ;Delete the pixmap, then create a new pixmap window at the new size
            wdelete, self.pixID
            self.pixID = MrGetWindow(XSIZE=xNew, YSIZE=yNew, /FREE, /PIXMAP)
    
            ;Set the new size of the draw widget
            widget_control, self.drawID, DRAW_XSIZE=xNew, DRAW_YSIZE=yNew
            self.xsize = xNew
            self.ysize = yNew
    
            ;Recalculate the normalized positions based on the new window size.
            ;Draw the plot to the new size
            refresh_in = self._refresh
            self -> Refresh, /DISABLE
            self -> CalcPositions
            self -> ApplyPositions
            self -> Refresh, DISABLE=~refresh_in
        endcase
        
        else: Message, 'TLB_Event not recognized: ' + sname
    endcase
end


;+
;   A method for temporarily turning off everything that would otherwise cause a
;   draw widget event.
;
; :Private:
;
; :Params:
;       TLB:        in, optional, type=int
;                   The widget ID of the top level base.
;-
pro MrWindow::Turn_Everything_Off, tlb
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Get the widget IDs of the "None" buttons
    AnnNoneID = widget_info(self.tlb, FIND_BY_UNAME='AnnNone')
    ANoneID = widget_info(self.tlb, FIND_BY_UNAME='ANone')
    
    ;Create an event and send it
    mockEvent = {id: ANoneID, top: self.tlb, handler: ANoneID}
    widget_control, SEND_EVENT=mockEvent
    
    ;Create an event and send it
    mockEvent = {id: AnnNoneID, top: self.tlb, handler: AnnNoneID}
    widget_control, SEND_EVENT=mockEvent
    
    ;CURSOR
    self -> MrCursor::Turn_Everything_Off, tlb
    
    ;ZOOM
    self -> MrZoom::Turn_Zoom_Off, tlb
end


;+
;   Hanle wheel zoom event.
;
; :Private:
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::Wheel_Zoom, event
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Refresh, DISABLE=~refreshIn
        void = cgErrorMsg()
        return
    endif
    
    ;Only listen to wheel events.
    if event.type ne 7 || self.wmode eq 0 then return
    
    ;Momentarily turn off refresh
    refreshIn = self -> GetRefresh()
    self -> Refresh, /DISABLE
    
    ;Figure out which type of object has the focus.
    oGraphic = self -> GetSelect()
    oType = self -> WhatAmI(oGraphic)

    ;Choose the zoom type
    case oType of
        'PLOT': if self.wmode eq 1 then self -> MrZoom::Wheel_Zoom_XY, event
        
        'IMAGE': begin
            if self.wmode eq 1 then self -> MrZoom::Wheel_Zoom_XY, event
            if self.wmode eq 2 then self -> MrZoom::Wheel_Zoom_Color, event
            if self.wmode eq 4 then self -> MrZoom::Wheel_Zoom_Page, event
        endcase
        
        'COLORBAR': if self.wmode eq 2 then self -> MrZoom::Wheel_Zoom_Color, event
        
        else: ;do nothing
    endcase
    
    ;Turn refresh back on
    self -> Refresh, DISABLE=~refreshIn
end


;+
;   Print which data objects are present and the index at which they are stored.
;
; :Private:
;-
pro MrWindow::whichDataObjects, outText
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
        
;---------------------------------------------------------------------
;Data Objects ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get all of the data objects
    dataObj = self -> Get(/ALL, ISA=(*self.gTypes).data, COUNT=nData)
    
    ;Description plus header
    outText = strarr(nData + 1)
    
    ;Get the container indices of each object.
    if nData gt 0 $
        then index = self -> GetIndex(dataObj) $
        else outText = ['No Data object in the container']
        
    
    ;The string length of the longest type
    typeLen = string(max(strlen((*self.gTypes).data)), FORMAT='(i0)')
    
    ;Step through each object
    for i = 0, nData - 1 do begin
        ;Print a header.
        if i eq 0 then outText[0] = string('--Index--', '--Type--', '-Location-', '--Name--', $
                                           FORMAT='(a9, 4x, a' + typeLen + ', 4x, a10, 4x, a8)')
                              
        ;Get the object's position and layout
        dataObj[i] -> GetLayout, LAYOUT=layout
        colrow = self -> ConvertLocation(layout[2], /PINDEX, /TO_COLROW)

        ;Print the type-name, location, and position
        sIndex    = string(index[i], FORMAT='(i2)')
        sLocation = string(colrow, FORMAT='(%"[%3i,%3i]")')
        sName = dataObj[i] -> GetName()

        outText[i+1] = string(FORMAT='(4x, a2, 7x, a' + typeLen + ', 5x, a0, 5x, a0)', $
                              sIndex, obj_class(dataObj[i]), sLocation, sName)
    endfor
    
    ;Output the text
    case n_params() of
        0: print, transpose(outText)
        1: outText = transpose(temporary(outText))
        else: message, 'Incorrect number of parameters.'
    endcase
end


;+
;   Print which annotate objects are present and the index at which they are stored.
;
; :Private:
;-
pro MrWindow::whichAnnotateObjects, text
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
     
;---------------------------------------------------------------------
;Annotation Objects //////////////////////////////////////////////////
;---------------------------------------------------------------------
    annObj = self -> Get(/ALL, ISA=(*self.gTypes).annotate, COUNT=nAnn)

    ;Desctription plus header
    outText = strarr(nAnn + 1)
    
    ;Get the container indices of each object.
    if nAnn gt 0 $
        then index = self -> GetIndex(annObj) $
        else outText = ['No annotate objects in the container']

    ;The string length of the longest type
    typeLen = string(max(strlen((*self.gTypes).annotate)), FORMAT='(i0)')
    
    ;Step through each annotate object
    for i = 0, nAnn - 1 do begin
        ;Print a header.
        if i eq 0 then outText[0] = string('--Index--', '--Type--', '--Name--', $
                                           FORMAT='(a9, 4x, a' + typeLen + ', 4x, a12)')
    
        ;Print the type-name, location, and position
        sIndex = string(index[i], FORMAT='(i2)')
        sName  = annObj[i] -> GetName()

        outText[i+1] = string(FORMAT='(4x, a2, 7x, a' + typeLen + ', 5x, a0)', $
                              sIndex, obj_class(annObj[i]), sName)
    
    endfor
    
    ;Output the text
    case n_params() of
        0: print, transpose(outText)
        1: text = transpose(temporary(outText))
        else: message, 'Incorrect number of parameters.'
    endcase
end


;+
;   Print which objects are present and the index at which they are stored.
;-
pro MrWindow::whichObjects
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
        
    ;Get information about the data and annotate objects
    self -> whichDataObjects, dataText
    self -> whichAnnotateObjects, annText
    
    outText = [['DATA OBJECTS:'], [dataText], [''], ['ANNOTATE OBJECTS:'], [annText]]
    print, outText
end


;+
;   The purpose of this method is to foward events to the proper event handler.
;
; :Private:
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow_Events, event
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Get the user value and call the appropriate event handling method.
    widget_control, event.id, GET_UVALUE=event_handler
    
    ;Handle Scrool Wheel events differently
    call_method, event_handler.method, event_handler.object, event
end


;+
;   The "Notify Realize" event handler is separate from the "Event Pro" event handler.
;   Thus, this program forwards the Notify Realize event to its event handling method.
;
; :Private:
;-
pro MrWindow_Notify_Realize, id
    compile_opt strictarr
    
    ;Call the Notify_Realize method
    widget_control, id, GET_UVALUE=uvalue
    
    ;Call the Notify_Realize method
    call_method, 'Notify_Realize', uvalue.object, id
    
end


;+
;   Clean up after the widget is destroyed.
;
; :Private:
;-
pro MrWindow_Cleanup, tlb
    ;Do nothing. Cleanup is done when the object is destroyed.
end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrWindow::cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Cleanup inherited properties.
    self -> MrAbstractAnalysis::Cleanup
;    self -> MrAbstractArrow::Cleanup
    self -> MrCursor::Cleanup
;    self -> MrAbstractText::Cleanup
;    self -> MrSaveAs::Cleanup
    self -> MrZoom::Cleanup
    self -> MrPlotManager::Cleanup
    
    ;Destroy objects
    obj_destroy, self._saveas
    
    ;Delete the windows.
    if windowavailable(self.winID) then wDelete, self.winID
    if windowavailable(self.pixID) then wDelete, self.pixID
    
    ;If a widget is still active, destroy it
    if widget_info(self.tlb, /VALID_ID) then widget_control, self.tlb, /DESTROY
    
    ;Remove self from the !MrWindow_Array system variable
    self -> SysVRemove, self
end


;+
;   The purpose of this method is to create line plot in a zoomable, resizeable window
;   that contains several analysis options (with more to be added). Only certain features
;   are available at any one time, but all can be selected from the menu bar.
;
; :Params:
;       PARENT:             in, optional, type=int
;                           The widget ID of a parent widget in which to place the draw
;                               window. If not provided, the draw window will be placed
;                               in a MrWindow widget unless `NOGUI`=1.
;
; :Keywords:
;       ARROWS:             in, optional, type=object/objarr
;                           MrArrow object(s) to be added to the diplay window.
;       BUFFER:             in, optional, type=boolean, default=0
;                           If set, graphics will be directed to an invisible pixmap window
;                               instead of generating a widget window.
;       DRAW:               in, optional, type=boolean, default=1
;                           If set, the Draw method will be called. Widgets will be
;                               realized at this time.
;       NAME:               in, optional, type=string, default='MrWindow'
;                           Name to be given to the window. Windows can be accessed by
;                               name via the GetMrWindows function.
;       NOGUI:              in, optional, type=boolean, default=0
;                           If set, graphics will be created in a normal IDL window, unless
;                               `PARENT` is provided.
;       PLOTOBJECTS:        in, optional, type=object/objarr
;                           MrPlot object(s) to be added to the display.
;       REFRESH:            in, optional, type=boolean, default=0
;                           If set, the display will be refreshed after the window is
;                               created. This will cause the widget to be realized and
;                               the contents to be drawn.
;       SAVEDIR:            in, optional, type=string, default=current
;                           The directory in which to save files. When the display is
;                               saved via the ::saveImage method, if no filename is given,
;                               a dialog will open to this directory.
;       TEXT:               in, optional, type=object
;                           A weText object or an array of weText objects to be added
;                               to the draw window.
;       XSIZE:              in, optional, type=long, default=512
;                           The width of the draw window in pixels
;       YSIZE:              in, optional, type=long, default=512
;                           The height of the draw window in pixels
;       WINDOW_TITLE:       in, optional, type=string, default='MrWindow'
;                           Title to be placed graphic window's title bar.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by the superclasses is also accepted for
;                               keyword inheritance.
;
; :Uses:
;   Uses the following external programs::
;       binary.pro (Coyote Graphics)
;       error_message.pro (Coyote Graphics)
;
; :History:
;   Modification History::
;       05/05/2013  -   Added the BUILD and REALIZE keywords. - MRA
;-
function MrWindow::init, parent, $
;MrWindow Keywords
BUFFER = buffer, $
DRAW = draw, $
NAME = name, $
NOGUI = noGUI, $
REFRESH = refresh, $
;SAVEDIR = savedir, $
XSIZE = xsize, $
YSIZE = ysize, $
WINDOW_TITLE=window_title, $
_REF_EXTRA = extra
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Superclasses
    if self -> MrPlotManager::Init()   eq 0 then return, 0
    if self -> MrZoom::Init()          eq 0 then return, 0
    if self -> MrCursor::Init(CMODE=8) eq 0 then return, 0

;---------------------------------------------------------------------
;Keywords ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Default window size
    setDefaultValue, amode, 0, /BOOLEAN
    setDefaultValue, buffer, 0, /BOOLEAN
    setDefaultValue, refresh, 1, /BOOLEAN
    setDefaultValue, name, 'MrWindow'
    setDefaultValue, window_title, 'MrWindow'
    setDefaultValue, noGUI, 0, /BOOLEAN
;    setDefaultValue, savedir, current
    setDefaultvalue, xsize, 600
    setDefaultValue, ysize, 340

    ;Set properties
    self.amode = amode
    self.buffer = buffer
    self.name = name
;    self.savedir = savedir
    self.title = window_title
    self.xsize = xsize
    self.ysize = ysize
    self._selection = obj_new('MrIDL_Container')
    self._saveas    = obj_new('MrSaveAs', void, self)
    
    ;Wait until here set REFRESH so that nothing is drawn or realized
    self._refresh = refresh

;---------------------------------------------------------------------
;Display Window //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Buffer the output?
    if buffer then begin
        self.pixID = MrGetWindow(XSIZE=xsize, YSIZE=ysize, /PIXMAP, /FREE)
        self.winID = MrGetWindow(XSIZE=xsize, YSIZE=ysize, /PIXMAP, /FREE)

	;MrWindow or IDL Window?
	endif else if n_elements(parent) eq 0 then begin

    ;---------------------------------------------------------------------
    ;IDL Window //////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------

	    ;Is a GUI being built or realized? If not...
	    if keyword_set(noGUI) then begin
	    
	        ;Create a normal window. Make it a pixmap if DISPLAY=0
	        if display eq 1 then pixmap = 0 else pixmap = 1
            self.pixID = MrGetWindow(XSIZE=xsize, YSIZE=ysize, /FREE, /PIXMAP)
            self.winID = MrGetWindow(TITLE=window_title, XSIZE=xsize, YSIZE=ysize, $
                                     /FREE, PIXMAP=pixmap)

    ;---------------------------------------------------------------------
    ;MrWindow GUI ////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------

        ;If so, build the GUI
        endif else begin	
	
            ;Create a pixmap window. This has to be done before the GUI is realized
            ;because the NOTIFY_REALIZE method calls the DRAW method, which reequires a
            ;pixmap window.
            self.pixID = MrGetWindow(XSIZE=xsize, YSIZE=ysize, /PIXMAP, /FREE)
        
            ;Refresh? -- If we are not refreshing, we must create a temporary window.
            if refresh eq 0 $
                then self.winID = MrGetWindow(TITLE=window_title, XSIZE=xsize, YSIZE=ysize, $
                                              /PIXMAP, /FREE)
	    endelse

    ;---------------------------------------------------------------------
    ;External GUI ////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        ;
        ;If a parent was provided, just add the draw widget. We have to know when the 
        ;widget is realized so that we can get the Window ID in which to draw the plots.
        ;Realization needs its own event handler because the UVALUE is for other events
        ;
	endif else begin
	    self.tlb = parent
        
        ;create a pixmap window
        self.pixID = MrGetWindow(XSIZE=xsize, YSIZE=ysize, /PIXMAP, /FREE)
	    
	    ;Make the draw widget
        self.drawID = widget_draw(self.tlb, XSIZE=xsize, YSIZE=ysize, $
                                  NOTIFY_REALIZE='MrWindow_Notify_Realize', $
                                  EVENT_PRO='MrWindow_Events', $
                                  UVALUE={object: self, method: 'Draw_Events'})
	endelse

    ;Set properties after the window has been created.
    self -> SetProperty, _EXTRA=extra

    ;Add to the container of open windows
    self -> SysVAdd, self
    
    ;Draw
    self -> Draw

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
;       TLB:            Widget ID of the top level base
;       DISPLAY:        Display the graphics?
;       DRAWID:         Widget ID of the draw widget
;       FOCUS:          Object of focus
;       PIXID:          Window ID of the pixmap
;       MENUID:         Widget ID of the menu bar
;       _REALIZED:      Indicates that the widget has been realized
;       _REFRESH:       Indicates that the graphics window should be refreshed
;       STATUSID:       Widget ID of the status bar
;       WINID:          Window ID of the draw window
;       X0:             First clicked x-coordinate
;       Y0:             First clicked y-coordinate
;       XSIZE:          x-size of the draw window
;       YSIZE:          y-size of the draw window
;-
pro MrWindow__define, class
    compile_opt strictarr
    
    class = { MrWindow, $
              ;In order of importance.
              inherits MrPlotManager, $         ;Manage plot layout
              inherits MrZoom, $                ;Zoom events and menu
              inherits MrCursor, $              ;Cursor events and menu
              inherits MrManipulate, $          ;Manipulation events and menu
              inherits MrAbstractText, $        ;Text events and menu
              inherits MrAbstractArrow, $       ;Arrow events and menu
              inherits MrAbstractAnalysis, $    ;Analysis events and menu
              
              tlb: 0, $                         ;Widget ID of the top level base
              buffer: 0, $                      ;Buffer the graphics?
              drawID: 0, $                      ;Widget ID of the draw widget
              _selection: obj_new(), $          ;Container of selected objects
              focus: obj_new(), $               ;Object of focus
              font: 0, $
              fontName: '', $
              pixID: 0, $                       ;Window ID of the pixmap
              menuID: 0, $                      ;Widget ID of the menu bar
              name: '', $                       ;Name of the window
              _realized: 0B, $                  ;Has the widget been realized?
              _refresh: 0B, $                   ;Refresh the window?
              _SaveAs: obj_new(), $             ;Object for saving image output.
              statusID: 0, $                    ;Widget ID of the status bar
              title: '', $                      ;Title placed on the title bar
              winID: 0, $                       ;Window ID of the draw window
              x0: 0, $                          ;First clicked x-coordinate, for zooming
              y0: 0, $                          ;First clicked y-coordinate, for zooming
              xsize: 0, $                       ;x-size of the draw window
              ysize: 0 $                        ;y-size of the draw window
            }
end