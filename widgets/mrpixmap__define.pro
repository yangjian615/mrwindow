; docformat = 'rst'
;
; NAME:
;       MrPixmap__Define
;
;*****************************************************************************************
;   Copyright (c) 2014, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
; PURPOSE
;+
;   A resizable pixmap object. This object is a simplified version MrDrawWidget, having
;   only the ability to be resized, copied, and mapped. By mapping and unmapping the
;   draw widget, it becomes visible and hidden to the user. This ability is useful to
;   check if what you expect to be happening in the pixmap is indeed what is really
;   happening.
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
;       2014/06/17  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   This method draws the contents of the draw widget object's container in the draw
;   widget window. It does this by calling the DRAW methods of any objects found in its
;   container object.
;
; :Keywords:
;       BACKGROUND_COLOR:   in, optional, type=string, default='White'
;                           Set this keyword to the name of the background color. Used
;                               only when erasing window contents. On 8-bit displays, this
;                               will load a color in !P.BACKGROUND.
;       NOERASE:            in, optional, type=boolean, default=1
;                           Set this keyword to prevent the window from being erasee
;                               before drawing contents.
;       HOURGLASS:          in, optional, type=boolean, default=0
;                           If set, the cursor will change to an hourglass during the
;                               draw operation.
;       TARGET_WINDOW:      in, optional, type=integer
;                           Normally the draw widget draws into its own window. But,
;                               sometimes you want the draw widget to draw somewhere else.
;                               Setting this keyword to another DRAWWIDGET or PIXMAPWIDGET
;                               object reference allows graphics to be drawn there.
;       TARGETS:            in, optional, type=object/objarr
;                           Typically, calling the DRAW method of a DrawWidget will call
;                               the DRAW method of any objects in its container. However, if
;                               the TARGETS keyword is set to an object reference (or array of
;                               object references), only these objects will be drawn. This
;                               would allow you, for example, to re-draw only a single image
;                               object in a window with several image objects.
;       _EXTRA:             in, optional, type=any
;                           Any extra keywords appropriate for superclass DRAW methods.
;-
PRO MrPixmap::Draw, $
 BACKGROUND_COLOR=background_color, $
 NOERASE=noerase, $
 HOURGLASS=hourglass, $
 TARGET_WINDOW=target_window, $
 TARGETS=targets, $
_EXTRA=extraKeywords
   compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Do not draw anything?
    if self._refresh eq 0 then return

    ;Make sure the window is valid
    if (WindowAvailable(self._winID) eq 0) then $
        message, 'Draw widget does not have a valid window id. Try realizing it.'

    ;Defaults
    noerase   = keyword_set(noerase)
    hourglass = keyword_set(hourglass)
    _noerase  = n_elements(noerase)          eq 0 ? self._noerase    : noerase
    bg_color  = n_elements(background_color) eq 0 ? self._background : background_color

    ;Enable the hourglass mouse cursor, if needed.
    if hourglass then widget_control, /HOURGLASS

    ;Does the device support windows?
    ;   - Draw to the pixmap.
    if (!d.flags and 256) gt 0 then begin
        self -> SetCurrent
        if noerase eq 0B then self -> Erase, bg_color
    endif

    ;Draw all of the objects in the container.
    allObj = self -> Get(/ALL, COUNT=nObj)
    for i = 0, nObj - 1 do allObj[i] -> Draw, /NOERASE
end


;+
;   The purpose of this method is to erase the draw window.
;-
pro MrPixmap::Erase, color
   compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Default background color
    if n_elements(color) eq 0 then color = self._background
    
    ;Erase the window.
    if (!d.flags and 256) ne 0 then wset, self._winID
    cgErase, self._background
end


;+
;   Get class properties
;
; :Keywords;
;       VISIBLE:        out, optional, type=boolean
;                       If set, the pixmap is mapped and should be visible.
;                           This is equivalent to getting the MAP keyword of the TLB.
;       _REF_EXTRA:     out, optional, type=any
;                       Any keyword appropriate for the MrDrawWidget::SetProperty method.
;-
PRO MrPixmap::GetProperty, $
VISIBLE=visible, $
_REF_EXTRA=extra
   compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Object and widget properties
    if arg_present(visible) then visible = widget_info(self._id, /MAP)
    
    ;Window sizes
    if n_elements(extra) gt 0 then self -> MrDrawWidget::GetProperty, _EXTRA=extra
end


;+
;   Set class properties
;
; :Keywords:
;       VISIBLE:            in, optional, type=boolean
;                           If set, the pixmap will be mapped so that it is visible.
;                               This is equivalent to setting the MAP keyword in the TLB.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword appropriate for MrWidgetDraw::SetProperty.
;-
PRO MrPixmap::SetProperty, $
VISIBLE=visible, $
_REF_EXTRA=extra
   compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Superclass
    if n_elements(extra) gt 0 then self -> MrDrawWidget::SetProperty, _EXTRA=extra

    ;Set properties
    if n_elements(visible) gt 0 then widget_control, self._tlbID, MAP=keyword_set(visible)
END


;+
;   This is the MrDrawWidget object class initialization method
;
; :Params:
;       PARENT:         in, optional, type=integer/object
;                       Either the widget ID of a parent widget or MrTopLevelBase object
;                           reference. If not provided, the draw widget will be placed
;                           in a resizeable top level base widget.
;
; :Keywords:
;       APP_SCROLL:     in, optional, type=boolean
;                       A memory-saving way of scrolling draw widgets.
;                           See WIDGET_DRAW documentation.
;       ASPECT:         in, optional, type=float
;                       The aspect ratio (`XSIZE`:`YSIZE`) of the draw widget to be made.
;                           An aspect ratio of 1.0 produces a square window.
;   BACKGROUND_COLOR:   in, optional, type=string, default='White'
;                       The name of the initial color for the draw widget. Used when
;                           realized and if the draw widget is set up to erase before
;                           display (i.e., NOERASE=0).
;       BUTTON_EVENTS:  in, optional, type=boolean, default=0
;                       If set, button events will be enabled.
;       CONTEXT_EVENTS: in, optional, type=boolean, default=0
;                       If set, context events will be enabled.
;       DRAG_NOTIFY:    in, optional, type=string/structure
;                       If a string, the name of a procedure to be called when items are
;                           dragged over the window. If a structure, it defines the object
;                           and its procedure method that will handle the event.
;       DRAW_HANDLER:   in, optional, type=string/structure
;                       If a string is provided, it is the name of a procedure
;                           to be called when events are generated. If a structure
;                           is provided, its only fields must be "object" and "method",
;                           describing the procedure method to be used as an event
;                           handler::
;                               {object: oRef, $
;                                method: 'callback_method'}
;                           For function event handling, set the `FUNC_HANDLERS`
;                           keyword. If set, all of the *_HANDLER keywords will be
;                           ignored.
;       DROP_EVENTS:    in, optional, type=boolean, default=0
;                       If set, drop events will be enabled.
;       DROP_HANDLER:   in, optional, type=string/structure
;                       Either the name of the procedure or a structure defining an object
;                           method to be called when drop events are generated.
;       EVENT_HANDLER:  in, optional, type=object
;                       The object used when events are generated. It object must be
;                           a subclass of `MrGraphicsEventAdapter`.
;       EXPOSE_EVENTS:  in, optional, type=boolean, default=0
;                       If set, expose events will be enabled.
;       EXPOSE_HANDLER: in, optional, type=string/structure
;                       Either the name of the procedure or a structure defining an object
;                           method to be called when expose events are generated.
;       FUNC_HANDLERS:  in, optional, type=boolean, default=0
;                       If set, function callbacks will be used, not procedures. This
;                           switches `EVENT_HANDLER` from EVENT_PRO to EVENT_FUNC. All
;                           events handled by `EVENT_HANDLER`, then, must have functions
;                           for event handlers, not procdures.
;       FRAME:          in, optional, type=integer, default=0
;                       Create a frame this many pixels wide around the widget.
;       GROUP_LEADER:   in, optional, type=integer
;                       The ID of a widget that serves as the group leader. If the group
;                           leader is destroyed, all widgets in the group are destroyed
;                           as well.
; IGNORE_ACCELERATORS:  in, optional, type=boolean/string/strarr, default=0
;                       Set this keyword to specify what WIDGET_BUTTON accelerators are
;                           to be ignored when this draw widget has keyboard focus.
;                           Setting IGNORE_ACCELERATORS allows a defined list of
;                           accelerators to be processed by the draw widget instead
;                           of by the conflicting accelerated button. Valid values are::
;                             1             -- All accelerators should be ignored.
;                             string/strarr -- Any value that is legal for the
;                                              ACCELERATOR keyword for BUTTONWIDGET
;   KEYBOARD_EVENTS:    in, optional, type=boolean, default=0
;                       If set, keyboard events will be enabled.
;   KEYBOARD_HANDLER:   in, optional, type=string, default=''
;                       The name of a function to be called when keyboard events are
;                           generated. See `Widget_Window <http://exelisvis.com/docs/WIDGET_WINDOW.html>`
;                           for details. Also, the `DRAW_HANDLER` and `EVENT_HANDLER`
;                           keywords provide a simpler means of handling events.
;       MOTION_EVENTS:  in, optional, type=boolean, default=0
;                       If set, motion events will be enabled.
;   MOUSE_UP_HANDLER:   in, optional, type=string, default=''
;                       The name of a function to be called when button up events are
;                           generated. See `Widget_Window <http://exelisvis.com/docs/WIDGET_WINDOW.html>`
;                           for details. Also, the `DRAW_HANDLER` and `EVENT_HANDLER`
;                           keywords provide a simpler means of handling events.
;   MOUSE_DOWN_HANDLER: in, optional, type=string, default=''
;                       The name of a function to be called when button down events are
;                           generated. See `Widget_Window <http://exelisvis.com/docs/WIDGET_WINDOW.html>`
;                           for details. Also, the `DRAW_HANDLER` and `EVENT_HANDLER`
;                           keywords provide a simpler means of handling events.
; MOUSE_MOTION_HANDLER: in, optional, type=string, default=''
;                       The name of a function to be called when the mouse motion events are
;                           generated. See `Widget_Window <http://exelisvis.com/docs/WIDGET_WINDOW.html>`
;                           for details. Also, the `DRAW_HANDLER` and `EVENT_HANDLER`
;                           keywords provide a simpler means of handling events.
; MOUSE_WHEEL_HANDLER:  in, optional, type=string, default=''
;                       The name of a function to be called when mouse wheel events are
;                           generated. See `Widget_Window <http://exelisvis.com/docs/WIDGET_WINDOW.html>`
;                           for details. Also, the `DRAW_HANDLER` and `EVENT_HANDLER`
;                           keywords provide a simpler means of handling events.
;       NOERASE:        in, optional, type=boolean, default=0
;                       If set, the draw widget will not be erased before drawing.
;       NOTIFY_REALISE: in, optional, type=string/structure
;                       Either the name of the procedure or a structure defining an object
;                           method to be called when the draw widget is realized.
;       RENDERER:       in, optional, type=integer, defualt=0
;                       Which graphics renderer to use. Options are::
;                           0 - Platform native OpenGL
;                           1 - IDL's software implementation
;       REFRESH:        In, optional, type=boolean, default=1
;                       Set to zero to prohibit the draw widget from refreshing.
;       RESOURCE_NAME:  in, optional, type=string
;                       X Window's resource name applied to the widget.
;       RETAIN:         in, optional, type=integer, default="{windows: 1, unix: 2}"
;                       Set this keyword to determine how backing store is handled.
;       SCR_XSIZE:      in, optional, type=integer
;                       Set the screen X size of the base to this many pixels. (Use discouraged.)
;       SCR_YSIZE:      in, optional, type=integer
;                       Set the screen Y size of the base to this many pixels. (Use discouraged.)
;       SCROLL:         in, optional, type=boolean, default=0
;                       Set this keyword to add scroll bars to the draw widget.
;       TOOLTIP:        in, optional, type=string, default=''
;                       A string displayed when the cursor hovers over the draw widget.
;       UNITS:          in, optional, type=integer, default=0
;                       The units for measurments. Choices are::
;                           0 - Pixels
;                           1 - Inches
;                           2 - Centimeters
;   VIEWPORT_EVENTS:    in, optional, type=boolean, default=0
;                       If set, viewport scroll events will be enabled.
;   VIEWPORT_HANLER:    in, optional, type=string/structure
;                       Either the name of the procedure or a structure defining an object
;                           method to be called when viewport scroll events are generated.
;       WHEEL_EVENTS:   in, optional, type=boolean, default=0
;                       If set, wheel events will be enabled.
;       WINDOW_TITLE:   in, optional, type=string, default='MrDrawWidget'
;                       Name to be placed on the window's title bar. Ignored if `PARENT`
;                           is given.
;       X_SCROLL_SIZE:  in, optional, type=integer
;                       The X size (pixels) of the scrollable window area.
;       XOFFSET:        in, optional, type=integer
;                       The horizontal space (pixels) from upper left corner of the display.
;       XSIZE:          in, optional, type=integer, default=300
;                       The X size of the widget. (300 pixels by default.)
;       Y_SCROLL_SIZE:  in, optional, type=integer
;                       The Y size (pixels) of the scrollable window area
;       YOFFSET:        in, optional, type=integer
;                       The vertical space (pixels) from upper left corner of the display.
;       YSIZE:          in, optional, type=integer, default=300
;                       The Y size of the widget.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword appropriate for the MrWidgetAtom INIT methods.
;-
function MrPixmap::init, parent,   $
;MrDrawWidget Keywords
 BACKGROUND=background, $
 NOERASE=noerase, $
 REFRESH=refresh, $
 WINDOW_TITLE=window_title, $
;Widget_Draw Keywords
 APP_SCROLL=app_scroll, $
 ASPECT=aspect, $
 GROUP_LEADER=group_leader, $
 RENDERER=renderer, $
 RESOURCE_NAME=resource_name, $
 RETAIN=retain, $
 SCR_XSIZE=scr_xsize, $
 SCR_YSIZE=scr_ysize, $
 SCROLL=scroll, $
 UNITS=units, $
 X_SCROLL_SIZE=x_scroll_size, $
 XOFFSET=xoffset, $
 XSIZE=xsize, $
 Y_SCROLL_SIZE=y_scroll_size, $
 YOFFSET=yoffset, $
 YSIZE=ysize
   compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;
    ; IMPORTANT:
    ;   Do not call the MrWidgitDraw::Init method because it will
    ;   create a MrPixmap object of its own, causing an infinite
    ;   loop and segmentation fault.
    ;

;---------------------------------------------------------------------
;Defaults ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    noerase = keyword_set(noerase)
    refresh = keyword_set(refresh)
    if n_elements(background_color) eq 0 then background_color = 'White'
    if n_elements(retain)           eq 0 then retain           = (!version.os_family eq 'windows') ? 1 : 2
    if n_elements(xsize)            eq 0 then xsize            = 640
    if n_elements(ysize)            eq 0 then ysize            = 512
    
    ;Aspect ratio
    if n_elements(aspect) gt 0 then begin
        if aspect ge 1 $
            then yrange = round(xrange / aspect) $
            else xrange = round(yrange * aspect)
    endif

;---------------------------------------------------------------------
; Parent /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Are we creating a top-level base here?
    tf_tlb_created = 0B

    ;Create a MrTopLevelBase object to use as the parent
    if n_elements(parent) eq 0 then begin
        self           -> BuildWindow, WINDOW_TITLE=window_title
        self._oTLB     -> GetProperty, ID=parentID
        self._oTLB     -> SetProperty, MAP=0
        tf_tlb_created  = 1B

    ;Use an existing MrTopLevelBase object as the parent.
    endif else if size(parent, /TNAME) eq 'OBJREF' then begin
        tf_MrTLB = cgObj_IsA(parent, 'MrTopLevelBase')
        if tf_MrTLB eq 0 then message, 'Only "MrTopLevelBase" objects may be given as a parent object.'
        self._oTLB = parent
        self._oTLB -> GetProperty, ID=parentID
        self._oTLB -> SetProperty, MAP=0
    
    ;Use a normal widget base as the parent.
    endif else begin
        parentID = parent
        widget_control, parent, MAP=0
    endelse
    
    ;Set the top-level base widget ID
    self._tlbID = parentID
    
;---------------------------------------------------------------------
; Create the Draw Widget /////////////////////////////////////////////
;---------------------------------------------------------------------
    self._id = widget_draw( parentID,   $
                            APP_SCROLL          = app_scroll, $
                            GROUP_LEADER        = group_leader, $
                            RENDERER            = renderer, $
                            RESOURCE_NAME       = resource_name, $
                            RETAIN              = retain, $
                            SCR_XSIZE           = scr_xsize, $
                            SCR_YSIZE           = scr_ysize, $
                            SCROLL              = scroll, $
                            UNITS               = units, $
                            X_SCROLL_SIZE       = x_scroll_size, $
                            XOFFSET             = xoffset, $
                            XSIZE               = xsize, $
                            Y_SCROLL_SIZE       = y_scroll_size, $
                            YOFFSET             = yoffset, $
                            YSIZE               = ysize $
                          )

;---------------------------------------------------------------------
; Set Object Properties //////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Properties
    self._background = background_color
    self._noerase    = noerase
    self._refresh    = refresh

    ;Widget Atom
    success = self -> MrWidgetAtom::INIT(_STRICT_EXTRA=extra)
    if success eq 0 then message, 'MrWidgetAtom could not be initialized.'

;---------------------------------------------------------------------
; PixMap and SaveAs Objects //////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;This is the pixmap object
    self._oPixmap = obj_new()
    
    ;SaveAs
    self._oSaveAs = obj_new('MrSaveAs')
    
    ;Allocate heap
    self._drag_notify = ptr_new(/ALLOCATE_HEAP)

;---------------------------------------------------------------------
; Return Step ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;If we created a new tlb, realize it now and register it with XManager.
    if tf_tlb_created then begin
        self._oTLB -> XManager
        self._refresh = 1B
    endif

    self -> Draw
    return, 1
end



;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrPixmap__define, class

   class = { MrPixmap, $
             inherits MrDrawWidget $
           }
end