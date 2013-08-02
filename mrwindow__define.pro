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
;       MrAbstractCursor__define.pro
;       MrAbstractSaveAs__define.pro
;       MrAbstractText__define.pro
;       MrAbstractZoom__define.pro
;
;   For details concerning the automatically updating 2D plotting grid, see::
;       MrPlotLayout__define.pro
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
;       06/19/2013  -   Value_Locate was used to search for object matches via isMember,
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
;                           and the MrAbstractSaveAs::Output method was called, an error
;                           REALIZE=0 would occur because self.winID would be an invalid
;                           window ID. Furthermore, to do a screen capture, something had
;                           to have first been drawn in a window. This could not happen
;                           without first realizing the window... Recalculate the plot
;                           positions if DRAW is set in the SetProperty method. Before,
;                           the plot bled off the window. - MRA.
;-
;*****************************************************************************************
;+
;   Build the GUI. This is separate from the realization method so that custom GUI
;   configurations can be used. Also, so that the GUI can be build in the background
;   without being realized right away.
;
; :Keywords:
;       XSIZE:          in, optional, type=boolean, default=600
;                       If `BUILD` is set, then this is the width of the draw widget.
;       YSIZE:          in, optional, type=boolean, default=340
;                       If `BUILD` is set, then this is the height of the draw widget.
;-
pro MrWindow::buildGUI, $
XSIZE = xsize, $
YSIZE = ysize
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Default window size
    if n_elements(xsize) eq 0 then if self.xsize eq 0 then xsize = 600 else xsize = self.xsize
    if n_elements(ysize) eq 0 then if self.ysize eq 0 then ysize = 600 else ysize = self.ysize
    self.xsize = xsize
    self.ysize = ysize
    
    ;Make a top-level base with a menu bar
    self.tlb = widget_base(title='MrWindow', /COLUMN, /TLB_SIZE_EVENTS, $
                           UVALUE={object: self, method: 'TLB_Resize_Events'}, $
                           MBAR=menuID, XOFFSET=100, YOFFSET=0, UNAME='tlb')
    self.menuID = menuID

    ;Make the file menu.
    fileID = widget_button(menuID, VALUE='File', /MENU, EVENT_PRO=butto)
    button = widget_button(fileID, VALUE='Open CDF', UVALUE={object: self, method: 'File_Menu_Events'})
    button = widget_button(fileID, VALUE='Destroy', UVALUE={object: self, method: 'destroy'}, /SEPARATOR)
    button = widget_button(fileID, VALUE='Close', UVALUE={object: self, method: 'quit'}, /SEPARATOR)

    ;Create a "Save As" menu for the menu bar.
    self -> Create_SaveAs_Menu, menuID

    ;Make the edit menu in the menu bar
    editID = widget_button(menuID, VALUE='Edit', SENSITIVE=0, /MENU)
    button = widget_button(editID, VALUE='Layout', UVALUE={object: self, method: 'AdjustLayout'})
    button = widget_button(editID, VALUE='Plot', UVALUE={object: self, method: 'AdjustLayout'})
    button = widget_button(editID, VALUE='Move', UVALUE={object: self, method: 'AdjustLayout'})
    button = widget_button(editID, VALUE='Copy', UVALUE={object: self, method: 'AdjustLayout'})
    button = widget_button(editID, VALUE='Remove', UVALUE={object: self, method: 'Draw'})

    ;Create the Zoom Menu
    self -> Create_Zoom_Menu, menuID

    ;Create the Cursor Menu
    self -> Create_Cursor_Menu, menuID
    
    ;Create the Analysis Menu
    self -> Create_Analysis_Menu, menuID

    ;Create the Arrow and Text Menus
    annotateID = widget_button(menuID, VALUE='Annotate', /MENU)
    self -> Create_Arrow_Menu, annotateID
    self -> Create_Text_Menu, annotateID

    ;Make the draw widget. We still need to get the Window ID in which the plot
    ;is drawn, so get that when the window is realized (see note within ::init).
    self.drawID = widget_draw(self.tlb, XSIZE=xsize, YSIZE=ysize, $
                              /BUTTON_EVENTS, /WHEEL_EVENTS, $
                              UVALUE={object: self, method: 'Draw_Events'}, $
                              NOTIFY_REALIZE='MrWindow_Notify_Realize', $
                              EVENT_PRO='MrWindow_Events')

    ;print the data coordinates of the mouse below the draw widget
    self.statusID = widget_label(self.tlb, VALUE=' ', /DYNAMIC_RESIZE, UVALUE={object: self, method: 'Status'})
end


;+
;   Copy the pixmap to the display window.
;-
pro MrWindow::copyPixmap
    compile_opt idl2
    
    ;copy the pixmap to the draw window
    wset, self.winID
    device, COPY=[0, 0, self.xsize, self.ysize, 0, 0, self.pixID]
end


;+
;   The purpose of this method is to draw the plot in the draw window. The plot is
;   first buffered into the pixmap for smoother opteration (by allowing motion events
;   to copy from the pixmap instead of redrawing the plot, the image does not flicker).
;-
pro MrWindow::Draw
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Set the current window to the pixmap window and erase it
    if (!d.flags and 256) ne 0 then begin
        wset, self.pixID
        erase
    endif
    
    nPlots = n_elements(*self.allObjects)
    
    ;Create an empty plot if none exists
    if nPlots eq 0 then cgErase

    ;Plot and Image both erase the plotting area. Make sure only the first "draw" does.
    if n_elements(*self.plotObjects) eq 0 then noerase=0 else noerase=1

    ;Call all of the superclass draw methods.
    self -> MrAbstractPlot::Draw
    self -> MrAbstractImage::Draw, NOERASE=noerase
    self -> MrAbstractColorbar::Draw
    self -> MrAbstractArrow::Draw
    self -> MrAbstractText::Draw
    
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
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::Draw_Events, event
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
        ;Cursor Events
        if ((self.cmode and 8) gt 0) then self -> Focus, event      ;Focus
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
        if self.lmode eq 1 || self.rmode eq 1 then self -> XY_Zoom, event     ;X Zoom
        if self.lmode eq 2 || self.rmode eq 2 then self -> XY_Zoom, event     ;Y Zoom
        if self.lmode eq 4 || self.rmode eq 4 then self -> Box_Zoom, event    ;Box Zoom
        if self.lmode eq 8 || self.rmode eq 8 then self -> Pan, event         ;Pan
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
        if self.zmode eq 4 then self -> Box_Zoom, event   ;Box Zoom
        if self.zmode eq 8 then self -> Pan, event        ;Pan
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
        if self.zmode eq 4 then self -> Box_Zoom, event     
        if self.zmode eq 8 then self -> Pan, event 
        
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
        if self.zmode eq 8 then self -> Draw                            ;Pan
    endif

;---------------------------------------------------------------------
;Wheel Events ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if event.type eq 7 then self -> Wheel_Zoom, event
end


;+
;   Destroy the object and, if it exists, the widget.
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::destroy, event
    compile_opt idl2
    
    ;Destroy the object and the widget, if it still exists
    obj_destroy, self
end


;+
;   Destroy the object and, if it exists, the widget.
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::Error_Handler, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    self -> MrAbstractAnalysis::Error_Handler
    self -> Turn_Everything_Off
end



;+
;   Handle events triggered by the File Menu.
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::File_Menu_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
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
            ;Create the plot from the data
            thePlot = self -> Plot_CDF(GROUP_LEADER=self.tlb, $
                                       DISPLAY_TYPE=display_type, $
                                       OCOLORBAR=theColorbar)
            if obj_valid(thePlot) eq 0 then return
            
            ;Add it to the proper lists.
            case strupcase(display_type) of
                'TIME_SERIES': self -> AddPlots, thePlot, /DRAW
                '3D_SPECTROGRAM': begin
                    ;Add the image and get its position
                    self -> AddImages, thePlot, /DRAW
                    thePlot -> GetProperty, POSITION=position
                    
                    ;Make sure there is enough room for a colorbar.
                    if self.xmargin[1] lt 15 then self -> SetProperty, XMARGIN=[self.xmargin[0],15]

                    ;Add the colorbar and bind it to the image.
                    self -> AddColorbars, theColorbar, position, LOCATION='RIGHT'
                    self -> Bind, thePlot, theColorbar, /CAXIS
                    self -> Draw
                endcase
                else: message, 'Display type "' + display_type + '" not recognized.'
            endcase
        endcase
        
        else: message, 'Button "' + button_name + '" not recognized.'
    endcase
end


;+
;   The purpose of this method is to retrieve object properties from the MrWindow
;   instance itself or any of the objects contained within it.
;
; :Params:
;       INDEX:              in, optional, type=int
;                           If given, the index value of the object for which to obtain
;                               properties. INDEX can be obtained via the whichObjects
;                               method and the object type is set with the `ARROW`, 
;                               `COLORBAR`, `IMAGE`, `PLOT`, and `TEXT` keywords.
;
; :Keywords:
;
;       AMODE:              out, optional, type=int
;                           The analysis mode(s) presently enabled.
;       LMODE:              out, optional, type=int
;                           The zoom mode associated with the left mouse button.
;       OREF:               out, optional, type=boolean, default=0
;                           The object reference of the object referred to by `INDEX`.
;       RMODE:              out, optional, type=int
;                           The zoom mode associated with the right mouse button.
;       SAVEDIR:            out, optional, type=string
;                           The directory in which plots will be saved.
;       XSIZE:              out, optional, type=long, default=512
;                           The width of the draw window in pixels
;       YSIZE:              out, optional, type=long, default=512
;                           The height of the draw window in pixels
;       ZOOMFACTOR:         out, optional, type=float/fltarr(2)
;                           The zoom factor with which the mouse wheel will cause the
;                               x- and y-ranges to be adjusted. If only one value is given,
;                               it will be applied to both axes.
;
;       ARROW:              in, optional, type=boolean, default=0
;                           If set, then `INDEX` will be taken to refer to an arrow
;                               object. Use with the `INDEX` keyword.
;       COLORBAR:           in, optional, type=boolean, default=0
;                           If set, then `INDEX` will be taken to refer to a colorbar
;                               object. Use with the `INDEX` keyword.
;       IMAGE:              in, optional, type=boolean, default=0
;                           If set, then `INDEX` will be taken to refer to an image
;                               object. Use with the `INDEX` keyword.
;       PLOT:               in, optional, type=boolean, default=0
;                           If set, then `INDEX` will be taken to refer to a plot
;                               object. Use with the `INDEX` keyword.
;       TEXT:               in, optional, type=boolean, default=0
;                           If set, then `INDEX` will be taken to refer to an text
;                               object. Use with the `INDEX` keyword.
;
;       _REF_EXTRA:         out, optional, type=any
;                           If `INDEX` is present, then any keyword accepted by the
;                               indicated object's GetProperty method. If not present,
;                               then any keyword accepted by MrPlotLayout::GetProperty.
;                           
;-
pro MrWindow::GetProperty, index, $
CMODE = cmode, $
LMODE = lmode, $
RMODE = rmode, $
SAVEDIR = save_dir, $
XSIZE = xsize, $
YSIZE = ysize, $
ZOOMFACTOR = zoomfactor, $

ARROW = arrow, $
COLORBAR = colorbar, $
IMAGE = image, $
OREF = oref, $
PLOT = plot, $
TEXT = text, $

_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

;---------------------------------------------------------------------
;INHERITED OBJECTS ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get properties of a particular object?
    if n_params() eq 1 then begin
        arrow = keyword_set(arrow)
        colorbar = keyword_set(colorbar)
        image = keyword_set(image)
        plot = keyword_set(plot)
        text = keyword_set(text)
        
        ;Make sure only one keyword is set.
        if (image + plot + colorbar + arrow + text) ne 1 then $
            message, 'Can "Get" properties from exactly one type of object at a time.'
    
        ;ARROWS
        if arrow then begin
            if arg_present(oref) then oref = (*self.arrowObjects)[index]
            if n_elements(extra) gt 0 then (*self.arrowObjects)[index] -> GetProperty, _EXTRA=extra
        
        ;COLORBARS
        endif else if colorbar then begin
            if arg_present(oref) then oref = (*self.colorbars)[index]
            if n_elements(extra) gt 0 then (*self.colorbars)[index] -> GetProperty, _EXTRA=extra
        
        ;IMAGES
        endif else if image then begin
            if arg_present(oref) then oref = (*self.imageObjects)[index]
            if n_elements(extra) gt 0 then (*self.imageObjects)[index] -> GetProperty, _EXTRA=extra
        
        ;PLOTS
        endif else if plot then begin
            if arg_present(oref) then oref = (*self.plotObjects)[index]
            if n_elements(extra) gt 0 then (*self.plotObjects)[index] -> GetProperty, _EXTRA=extra
        
        ;TEXT
        endif else if text then begin
            if arg_present(oref) then oref = (*self.text)[index]
            if n_elements(extra) gt 0 then (*self.text)[index] -> GetProperty, _EXTRA=extra
            
        endif
    
        return
    endif

;---------------------------------------------------------------------
;WINDOW PROPERTIES ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get Properties
    if arg_present(cmode)      and n_elements(self.cmode)      ne 0 then cmode = self.cmode
    if arg_present(lmode)      and n_elements(self.lmode)      ne 0 then lmode = self.lmode
    if arg_present(rmode)      and n_elements(self.rmode)      ne 0 then rmode = self.rmode
    if arg_present(savedir)    and n_elements(self.savedir)    ne 0 then savedir = self.savedir
    if arg_present(xsize)      and n_elements(self.xsize)      ne 0 then xsize = self.xsize
    if arg_present(ysize)      and n_elements(self.ysize)      ne 0 then ysize = self.ysize
    if arg_present(zoomfactor) and n_elements(self.zoomfactor) ne 0 then zoomfactor = self.zoomfactor

;---------------------------------------------------------------------
;SUPERCLASS PROPERTIES ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(extra) gt 0 then begin

    ;---------------------------------------------------------------------
    ;SAVEAS PROPERTIES ///////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        void = isMember(['ADJUSTSIZE', 'IM_DENSITY', 'IM_OPTIONS', 'IM_RASTER', 'IM_RESIZE', $
                         'IM_TRANSPARENT', 'IM_WIDTH', 'PDF_UNIX_CONVERT_CMD', 'PDF_PATH', $
                         'PS_CHARSIZE', 'PS_DECOMPOSED', 'PS_DELETE', 'PS_ENCAPSULATED', $
                         'PS_FONT', 'PS_METRIC', 'PS_QUIET', 'PS_SCALE_FACTOR', 'PS_TT_FONT'], $
                         extra, iSaveAs, N_MATCHES=nmatches, NONMEMBER_INDS=iExtra, /FOLD_CASE)
        
        if nmatches gt 0 then self -> MrAbstractSaveAs::GetProperty, _STRICT_EXTRA=extra[iSaveAs]
        
    ;---------------------------------------------------------------------
    ;LAYOUT PROPERTIES ///////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        self -> MrPlotLayout::GetProperty, _STRICT_EXTRA=extra[iExtra]
    endif
    
end


;+
;   Because the draw widget can be added to an external GUI, we must know when that GUI
;   is realized in order to obtain the widget ID of the draw widget.
;-
pro MrWindow::Notify_Realize
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;If a temporary pixmap window was being used, delete it.
    if WindowAvailable(self.winID) then wDelete, self.winID
    
    ;Get the window ID of the draw widget
    widget_control, self.drawID, GET_VALUE=winID
    self.winID = winID
    
    ;Draw the plot now that the window is realized.
    self -> Draw
    
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
pro MrWindow::On_Off_Button_Events, $
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
        
        ;Turn button events off only if nothing else needs them. Cursor modes are the
        ;only type of modes that can have multiple bits set at the same time.
        if (self.zmode eq 0) and (self.arrowmode[0] eq 0) and $        ;Zoom, Arrow
           (self.textmode)[0] eq 0 and ((self.cmode and 1) eq 0) and $ ;Text, Get Points
           (self.amode)[0] eq 0 $                                      ;Analysis
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
pro MrWindow::On_Off_Motion_Events, $
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
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::quit, event
    compile_opt idl2
    
    ;Destroy the widget
    widget_control, event.top, /DESTROY
end


;+
;   Realize the GUI. This method can be called as long as the object still exists (i.e.
;   the GUI can be closed then opened again).
;
; :Keywords:
;       BUILD:          in, optional, type=Boolean, default=1
;                       Build the GUI before realizing it.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by the BUILD method is also excepted for
;                           keyword inheritance.
;-
pro MrWindow::realizeGUI, $
BUILD = build, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Default to building the GUI
    setDefaultValue, build, 1, /BOOLEAN
    
    ;Switch display modes, if necessary.
    if self.display eq 0 then begin
        self.display = 1
        wDelete, self.winID
    endif
    
    ;Build the GUI
    if keyword_set(build) then self -> buildGUI, _STRICT_EXTRA=extra
    
    ;Make sure a GUI has been built.
    if self.tlb eq 0 then message, 'GUI must be built first.'
        
    ;Realize the widget and start event handling
    widget_control, self.tlb, /REALIZE
    xmanager, 'MrWindow', self.tlb, /NO_BLOCK, EVENT_HANDLER='MrWindow_Events', $
              CLEANUP='MrWindow_CleanUp'
end

  
;+
;   This method resizes the draw window and redraws the plot to the adjusted size.
;
; :Params:
;       XSIZE:              in, required, type=long
;                           The width of the draw window in pixels
;       YSIZE:              in, required, type=long
;                           The height of the draw window in pixels
;
; :Keywords:
;       DRAW:               in, optional, type=boolean, default=0
;-
pro MrWindow::ResizeDrawWidget, xsize, ysize, $
DRAW = draw
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

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
        
        if self.display then pixmap = 0 else pixmap = 1
        self.winID = MrGetWindow(TITLE='MrWindow', XSIZE=xsize, YSIZE=ysize, /FREE, PIXMAP=pixmap)
    endif
    
    ;Update the object properties
    self.xsize = xsize
    self.ysize = ysize

    ;Recalculate the normalized positions based on the new window size.
    self -> MrPlotLayout::SetProperty
    
    ;Draw the plot to the new size
    if keyword_set(draw) then self -> Draw
end


;+
;   The purpose of this method is to set object properties from the MrWindow
;   instance itself or any of the objects contained within it.
;
; :Params:
;       INDEX:              in, optional, type=int
;                           If given, the index value of the object for which to obtain
;                               properties. INDEX can be obtained via the whichObjects
;                               method and the object type is set with the `ARROW`, 
;                               `COLORBAR`, `IMAGE`, `PLOT`, and `TEXT` keywords.
;
; :Keywords:
;       AMODE:              in, optional, type=int
;                           The analysis mode(s) presently enabled.
;       LMODE:              in, optional, type=int
;                           The zoom mode associated with the left mouse button.
;       RMODE:              in, optional, type=int
;                           The zoom mode associated with the right mouse button.
;       SAVEDIR:            in, optional, type=string
;                           The directory in which plots will be saved.
;       XSIZE:              in, optional, type=long, default=512
;                           The width of the draw window in pixels
;       YSIZE:              in, optional, type=long, default=512
;                           The height of the draw window in pixels
;       ZOOMFACTOR:         in, optional, type=float/fltarr(2)
;                           The zoom factor with which the mouse wheel will cause the
;                               x- and y-ranges to be adjusted. If only one value is given,
;                               it will be applied to both axes.
;
;       ARROW:              in, optional, type=boolean, default=0
;                           If set, then `INDEX` will be taken to refer to an arrow
;                               object. Use with the `INDEX` keyword.
;       CB:                 in, optional, type=boolean, default=0
;                           If set, then `INDEX` will be taken to refer to a colorbar
;                               object. Use with the `INDEX` keyword.
;       IMAGE:              in, optional, type=boolean, default=0
;                           If set, then `INDEX` will be taken to refer to an image
;                               object. Use with the `INDEX` keyword.
;       PLOT:               in, optional, type=boolean, default=0
;                           If set, then `INDEX` will be taken to refer to a plot
;                               object. Use with the `INDEX` keyword.
;       TEXT:               in, optional, type=boolean, default=0
;                           If set, then `INDEX` will be taken to refer to an text
;                               object. Use with the `INDEX` keyword.
;
;       _REF_EXTRA:         out, optional, type=any
;                           If `INDEX` is present, then any keyword accepted by the
;                               indicated object's GetProperty method. If not present,
;                               then any keyword accepted by MrPlotLayout::GetProperty.
;-
pro MrWindow::SetProperty, index, $
AMODE = amode, $
DRAW = draw, $
LMODE = lmode, $
RMODE = rmode, $
SAVEDIR = savedir, $
XSIZE = xsize, $
YSIZE = ysize, $
ZOOMFACTOR = zoomfactor, $

ARROW = arrow, $
CB = cb, $
IMAGE = image, $
PLOT = plot, $
TEXT = text, $

_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

;---------------------------------------------------------------------
;INHERITED OBJECTS ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get properties of a particular object?
    if n_params() eq 1 then begin
        arrow = keyword_set(arrow)
        cb = keyword_set(cb)
        image = keyword_set(image)
        plot = keyword_set(plot)
        text = keyword_set(text)
        
        ;Make sure only one keyword is set.
        if (image + plot + cb + arrow + text) ne 1 then $
            message, 'Can "Set" properties from exactly one type of object at a time.'
    
        ;ARROWS
        if arrow then begin
            (*self.arrowObjects)[index] -> SetProperty, _EXTRA=extra
        
        ;COLORBARS
        endif else if cb then begin
            (*self.colorbars)[index] -> SetProperty, _EXTRA=extra
        
        ;IMAGES
        endif else if image then begin
            (*self.imageObjects)[index] -> SetProperty, _EXTRA=extra
        
        ;PLOTS
        endif else if plot then begin
            (*self.plotObjects)[index] -> SetProperty, _EXTRA=extra
        
        ;TEXT
        endif else if text then begin
            (*self.textObjects)[index] -> SetProperty, _EXTRA=extra
        endif
        
        ;Draw?
        if keyword_set(draw) then self -> Draw
        
        return
    endif

;---------------------------------------------------------------------
;WINDOW PROPERTIES ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Set Properties
    if n_elements(amode)      ne 0 then self.amode = amode
    if n_elements(xsize)      ne 0 then self -> ResizeDrawWidget, xsize, self.ysize
    if n_elements(ysize)      ne 0 then self -> ResizeDrawWidget, self.xsize, ysize
    if n_elements(savedir)    ne 0 then self.savedir = savedir
    if n_elements(zoomfactor) ne 0 then self.zoomfactor = zoomfactor
    
    ;Make sure only one zoom bit is set
    if n_elements(lmode) ne 0 then $
        if total(fix(binary(lmode))) eq 1 $
            then self.lmode = lmode $
            else message, 'LMODE must have only one bit set'

    ;Make sure only one zoom bit is set            
    if n_elements(rmode) ne 0 then $
        if total(fix(binary(rmode))) eq 1 $
            then self.rmode = rmode $
            else message, 'RMODE must have only one bit set'
    
    n_extra = n_elements(extra)

;---------------------------------------------------------------------
;SUPERCLASS PROPERTIES ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_extra gt 0 then begin

    ;---------------------------------------------------------------------
    ;SAVEAS PROPERTIES ///////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        void = isMember(['ADJUSTSIZE', 'IM_DENSITY', 'IM_OPTIONS', 'IM_RASTER', 'IM_RESIZE', $
                         'IM_TRANSPARENT', 'IM_WIDTH', 'PDF_UNIX_CONVERT_CMD', 'PDF_PATH', $
                         'PS_CHARSIZE', 'PS_DECOMPOSED', 'PS_DELETE', 'PS_ENCAPSULATED', $
                         'PS_FONT', 'PS_METRIC', 'PS_QUIET', 'PS_SCALE_FACTOR', 'PS_TT_FONT'], $
                         extra, iSaveAs, N_MATCHES=nmatches, NONMEMBER_INDS=iExtra, /FOLD_CASE)
        
        if nmatches gt 0 then self -> MrAbstractSaveAs::SetProperty, _STRICT_EXTRA=extra[iSaveAs]
        
    ;---------------------------------------------------------------------
    ;LAYOUT PROPERTIES ///////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if n_elements(iExtra) ne 0 then self -> MrPlotLayout::SetProperty, _STRICT_EXTRA=extra[iExtra]
    endif

    ;Recalculate the plot positions in case the layout has changed.
    if keyword_set(draw) then self -> draw
end

  
;+
;   This method resizes the top level base, then updates the contents of the draw widget
;   to be proportioned appropriately.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::TLB_Resize_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
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
    self -> MrPlotLayout::SetProperty
    self -> Draw
end


;+
;   A method for temporarily turning off everything that would otherwise cause a
;   draw widget event.
;-
pro MrWindow::Turn_Everything_Off
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Get the widget IDs of the "None" buttons
    AnnNoneID = widget_info(self.tlb, FIND_BY_UNAME='AnnNone')
    ANoneID = widget_info(self.tlb, FIND_BY_UNAME='ANone')
    CNoneID = widget_info(self.tlb, FIND_BY_UNAME='CNone')
    ZNoneID = widget_info(self.tlb, FIND_BY_UNAME='ZNone')
    
    ;Create an event and send it
    mockEvent = {id: ANoneID, top: self.tlb, handler: ANoneID}
    widget_control, SEND_EVENT=mockEvent
    
    ;Create an event and send it
    mockEvent = {id: AnnNoneID, top: self.tlb, handler: AnnNoneID}
    widget_control, SEND_EVENT=mockEvent
    
    ;Create an event and send it
    mockEvent = {id: CNoneID, top: self.tlb, handler: CNoneID}
    widget_control, SEND_EVENT=mockEvent
    
    ;Create an event and send it
    mockEvent = {id: ZNoneID, top: self.tlb, handler: ZNoneID}
    widget_control, SEND_EVENT=mockEvent
end


;+
;   Hanle wheel zoom event.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow::Wheel_Zoom, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Only listen to wheel events.
    if event.type ne 7 || self.wmode eq 0 then return
    
    ;Figure out which type of object has the focus.
    theObject = (*self.allObjects)[self.ifocus]
    oType = self -> WhatAmI(theObject)

    ;Choose the zoom type
    case oType of
        'PLOT': self -> MrAbstractZoom::Wheel_Zoom_XY, event
        
        'IMAGE': begin
            if self.wmode eq 1 then self -> MrAbstractZoom::Wheel_Zoom_XY, event
            if self.wmode eq 2 then self -> MrAbstractZoom::Wheel_Zoom_Color, event
            if self.wmode eq 4 then self -> MrAbstractZoom::Wheel_Zoom_Page, event
        endcase
        
        else: ;do nothing
    endcase

end


;+
;   Print which objects are present and the index at which they are stored.
;-
pro MrWindow::whichObjects
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Display all of the objects and their index numbers.
    self -> whichPlots
    self -> whichImages
    self -> whichColorbars
    self -> whichText
    self -> whichArrows

end


;+
;   The purpose of this method is to foward events to the proper event handler.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrWindow_Events, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
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
;-
pro MrWindow_Notify_Realize, id
    compile_opt idl2
    
    ;Call the Notify_Realize method
    widget_control, id, GET_UVALUE=uvalue
    call_method, 'Notify_Realize', uvalue.object
    
end


;+
;   Clean up after the widget is destroyed.
;-
pro MrWindow_Cleanup, tlb
    ;Do nothing. Cleanup is done when the object is destroyed.
end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrWindow::cleanup
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Cleanup inherited properties.
    self -> MrAbstractAnalysis::Cleanup
    self -> MrAbstractArrow::Cleanup
    self -> MrAbstractCursor::Cleanup
    self -> MrAbstractText::Cleanup
    self -> MrAbstractSaveAs::Cleanup
    self -> MrAbstractZoom::Cleanup
    self -> MrPlotManager::Cleanup
    
    ;Delete the windows.
    wDelete, self.winID
    wDelete, self.pixID
    
    ;If a widget is still active, destroy it
    if widget_info(self.tlb, /VALID_ID) then widget_control, self.tlb, /DESTROY
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
;                               in a MrWindow widget unless `CREATEGUI`=0.
;
; :Keywords:
;       BUILD:              in, optional, type=Boolean, default=1
;                           Build the GUI. This gives the option to construct the GUI
;                               without having to realize it yet.
;       CMODE:              in, optional, type=int, default=0
;                           The default cursor mode in which to start
;       CREATEGUI:          in, optional, type=boolean, default=1
;                           If set, graphics will be drawn in either a MrWindow widget
;                               or in the widget specified by `PARENT`. If not set, graphics
;                               will be diplayed in a normal IDL graphics window. Setting
;                               this keyword to 0 also sets `REALIZE`=0 and `BUILD`=0
;       DISPLAY:            in, optional, type=boolean, default=1
;                           If set, graphics produced by the Draw method will be created
;                               in a visible display window. If not set (i.e. =0),
;                               graphics will be output to an invisible pixmap window. The
;                               latter will aslo automatically set CreateGUI=0. This is
;                               useful, say, if you want to save an image without having
;                               it be displayed first.
;       LMODE:              in, optional, type=int, default=4 (Box Zoom)
;                           The zoom mode associated with the left mouse button.
;       REALIZE:            in, optional, type=boolean, default=1
;                           If set and `PARENT` is not given, then the MrWindow widget will
;                               be realized imediately after it is built. If not set, the
;                               GUI will be built but not realized. Realization causes a
;                               call to the Draw method via "Notify_Realize". Setting this
;                               also sets `BUILD`=1.
;       RMODE:              in, optional, type=int, default=8 (Pan)
;                           The zoom mode associated with the right mouse button.
;       SAVEDIR:            in, optional, type=string, default=current
;                           The directory in which to save files. When the display is
;                               saved via the ::saveImage method, if no filename is given,
;                               a dialog will open to this directory.
;       TEXT:               in, optional, type=object
;                           A weText object or an array of weText objects to be added
;                               to the draw window.
;       WMODE:              in, optional, type=int, default=8 (Pan)
;                           The zoom mode associated with the mouse wheel.
;       XSIZE:              in, optional, type=long, default=512
;                           The width of the draw window in pixels
;       YSIZE:              in, optional, type=long, default=512
;                           The height of the draw window in pixels
;       ZOOMFACTOR:         in, optional, type=float/fltarr(2), default=[0.05\, 0.05]
;                           The zoom factor with which the mouse wheel will cause the
;                               x- and y-ranges to be adjusted. If only one value is given,
;                               it will be applied to both axes.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrPlotLayout__Define and 
;                               MrAbstractSaveAs__Define is also accepted for keyword
;                               inheritance.
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
ARROWS = arrows, $
CMODE = cmode, $
CREATEGUI = createGUI, $
DISPLAY = display, $
BUILD = build, $
LMODE = lmode, $
PLOTOBJECTS = plotObjects, $
REALIZE = realize, $
RMODE = rmode, $
SAVEDIR = savedir, $
TEXT = text, $
WMODE = wmode, $
XSIZE = xsize, $
YSIZE = ysize, $
ZOOMFACTOR = zoomfactor, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif

    if self -> MrPlotManager::init(_EXTRA=extra) eq 0 then return, 0
    if self -> MrAbstractSaveAs::init() eq 0 then return, 0

;---------------------------------------------------------------------
;Keywords ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    cd, CURRENT=current
    
    ;Default window size
    setDefaultValue, wmode, 0               ;None
    setDefaultValue, cmode, 0               ;None
    setDefaultValue, build, 1, /BOOLEAN
    setDefaultValue, lmode, 0               ;None
    setDefaultValue, rmode, 0               ;None
    setDefaultValue, realize, 1, /BOOLEAN
    setDefaultValue, savedir, current
    setDefaultvalue, xsize, 600
    setDefaultValue, ysize, 340
    setDefaultValue, createGUI, 1, /BOOLEAN
    setDefaultValue, display, 1, /BOOLEAN
    
    ;If nothing is to be displayed, do not create the GUI.
    if display eq 0 then createGUI = 0
    
    ;If not creating a GUI, then do not build or realize.
    if createGUI eq 0 then begin
        realize = 0
        build = 0
    endif
    
    ;Zoom Factor
    case n_elements(zoomfactor) of
        0: zoomfactor = [0.05, 0.05]
        1: zoomfactor = [zoomfactor, zoomfactor]
        2: zoomfactor = zoomfactor
        else: message, 'ZOOMFACTOR: Incorrect number of elements.'
    endcase
    
    ;Ensure only one zoom bit is set for the left mouse button
    if total(fix(binary(lmode))) gt 1 then begin
        message, 'LMODE must be a single bit. Defaulting to 0 (None)', /INFORMATIONAL
        lmode = 0
    endif
    
    ;Ensure only one zoom bit is set for the right mouse button
    if total(fix(binary(rmode))) gt 1 then begin
        message, 'LMODE must be a single bit. Defaulting to 0 (None)', /INFORMATIONAL
        rmode = 0
    endif
    
    ;Set object properties.
    self.display = display
    self.cmode = cmode
    self.lmode = lmode
    self.rmode = rmode
    self.wmode = wmode
    self.savedir = savedir
    self.xsize = xsize
    self.ysize = ysize
    self.x0 = -1
    self.y0 = -1
    self.zoomfactor = zoomfactor
    
    ;Serves as an INIT method.
    self -> AddArrows, arrows
    self -> AddColorbars, colorbars
    self -> AddPlots, plotObjects
    self -> AddImages, imageObjects
    self -> AddText, text

;---------------------------------------------------------------------
;Display Window //////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Was a parent widget provided? If not...
	if n_elements(parent) eq 0 then begin

    ;---------------------------------------------------------------------
    ;IDL Window //////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
	    
	    ;Is a GUI being built or realized? If not...
	    if keyword_set(createGUI) eq 0 then begin
	    
	        ;Create a normal window. Make it a pixmap if DISPLAY=0
	        if display eq 1 then pixmap = 0 else pixmap = 1
            self.pixID = MrGetWindow(XSIZE=xsize, YSIZE=ysize, /FREE, /PIXMAP)
            self.winID = MrGetWindow(TITLE='MrWindow', XSIZE=xsize, YSIZE=ysize, $
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
        
            ;Realize the widget?
            if keyword_set(realize) then begin
                self -> realizeGUI, /BUILD, XSIZE=xsize, YSIZE=ysize
                
            ;Build the widget?
            ;Without realizing the widget, no window will be created. Thus, the Draw method
            ;will have nothing to draw to and the SaveAs options will have nothing to read
            ;from. As a fix, create a temporary pixmap window (it will be deleted when the
            ;widget is realized and the Notify_Realize method is called).
            endif else if keyword_set(build) then begin
                self.winID = MrGetWindow(XSIZE=xsize, YSIZE=ysize, /PIXMAP, /FREE)
                self -> buildGUI, XSIZE=xsize, YSIZE=ysize
                
            ;Neither Build nor Realize. See previous comment.
            endif else begin
                self.winID = MrGetWindow(XSIZE=xsize, YSIZE=ysize, /PIXMAP, /FREE)
            endelse
	    
	    endelse

    ;---------------------------------------------------------------------
    ;External GUI ////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------

    ;If a parent was provided, just add the draw widget. We have to know when the 
    ;widget is realized so that we can get the Window ID in which to draw the plots.
    ;Realization needs its own event handler because the UVALUE is for other events
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

    return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrWindow__define, class
    compile_opt idl2
    
    class = { MrWindow, $
              inherits MrAbstractAnalysis, $    ;Analysis events and menu
              inherits MrAbstractArrow, $       ;Arrow events and menu
              inherits MrAbstractCDF, $         ;Plotting data from CDF files
              inherits MrAbstractCursor, $      ;Cursor events and menu
              inherits MrAbstractText, $        ;Text events and menu
              inherits MrAbstractSaveAs, $      ;SaveAs menu
              inherits MrAbstractZoom, $        ;Zoom events and menu
              inherits MrPlotManager, $         ;Manage plot layout
              
              tlb: 0, $                         ;Widget ID of the top level base
              display: 0, $                     ;Display the graphics?
              drawID: 0, $                      ;Widget ID of the draw widget
              pixID: 0, $                       ;Window ID of the pixmap
              menuID: 0, $                      ;Widget ID of the menu bar
              statusID: 0, $                    ;Widget ID of the status bar
              winID: 0, $                       ;Window ID of the draw window
              x0: 0, $                          ;First clicked x-coordinate, for zooming
              y0: 0, $                          ;First clicked y-coordinate, for zooming
              xsize: 0, $                       ;x-size of the draw window
              ysize: 0 $                        ;y-size of the draw window
            }
end