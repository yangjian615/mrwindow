; docformat = 'rst'
;
; NAME:
;       Mr_Window__Define
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
;   The purpose of this class is to provide an object wrapper for the Draw_Widget
;   function. It is modelled off of IDL's Draw_Window function.
;
;   EVENT PROCESSING:
;       Events are first sent to MrGraphicsEventAdapter (or a subclass of it). If that
;       returns 1 (one -- the default), then even processing proceeds to the user-defined
;       callback function. If that also returns 1 (one), then the defualt callback routine
;       is called.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2014/03/17  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The purpose of this method is to create a resizable base in which to place the
;   draw widget.
;
; :Private:
;
; :Keywords:
;       XSIZE:              in, optional, type=boolean, default=600
;                           Width of the draw widget.
;       YSIZE:              in, optional, type=boolean, default=340
;                           Height of the draw widget.
;       WINDOW_TITLE:       in, optional, type=string, default='Mr_Window'
;                           Name to be placed on the window's title bar.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword appropriate for MrDrawWidget::Init()
;-
pro Mr_Window::_BuildWindow, $
WINDOW_TITLE=window_title, $
XSIZE = xsize, $
YSIZE = ysize, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
;---------------------------------------------------------------------
; Top-Level Base /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    oTLB = obj_new('MrTopLevelBase', $
                   /TLB_KILL_REQUEST_EVENTS, $
                   /TLB_SIZE_EVENTS, $
                   COLUMN                   = 1, $
                   EVENT_OBJ                = self, $
                   EVENT_PRO                = '', $
                   ID                       = tlbID, $
                   TITLE                    = window_title, $
                   TLB_SIZE_HANDLER         = 'TLB_RESIZE', $
                   TLB_KILL_REQUEST_HANDLER = 'TLB_KILL_REQUEST', $
                   UNAME                    = 'tlb', $
                   XOFFSET                  = 100, $
                   YOFFSET                  = 0)
    oTLB -> GetProperty, ID=tlbID
    self._oTLB  = oTLB
    self._tlbID = tlbID

;---------------------------------------------------------------------
; Draw Widget ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    success = self -> MrDrawWidget::Init(oTLB, $
                                         EVENT_HANDLER = self, $
                                         EVENT_PRO     = 'Event_Handler', $
                                         REFRESH       = 0, $
                                         RETAIN        = retain, $
                                         UNAME         = 'DRAW_WIDGET', $
                                         XSIZE         = xsize, $
                                         YSIZE         = ysize, $
                                         _STRICT_EXTRA = extra)
    if success eq 0 then message, 'Unable to initialize MrDrawWidget.'

;---------------------------------------------------------------------
; Button Row /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    wbase = obj_new('MrWidgetBase', self._tlbID,   ID=wBaseBottomID, COLUMN=2, UNAME='BOTTOM_BASE')
    wbase = obj_new('MrWidgetBase', wBaseBottomID, ID=wBaseButtonID, ROW=1, UNAME='BUTTON_BASE')
    wbase = obj_new('MrWidgetBase', wBaseBottomID, ID=wBaseStatusID, ROW=1, UNAME='STATUS_BASE', $
                    /BASE_ALIGN_RIGHT, /ALIGN_RIGHT, FRAME=3)

    ;SAVEAS
    ;   - Make sure the widget dies with the top-level base.
    self._oSaveAs -> SetProperty, GROUP_LEADER=self._tlbID
    
    ;Create a bitmap image
    fname = '/Users/argall/Downloads/32_save.png'
    bitmap = transpose(read_png(fname), [1,2,0])
    self._oSaveAs -> Create_SaveAs_Menu, wBaseButtonID, /MENU, /BITMAP, VALUE=bitmap

    ;STATUS
    ;   - Print the data coordinates of the mouse below the draw widget
    self._statusID = widget_label(wBaseStatusID, VALUE='Value', $
                                  /DYNAMIC_RESIZE, $
                                  UVALUE={object: self, method: 'Event_Handler'})
end


;+
;   The purpose of this method is to provide an array-like means of accessing graphics
;   objects within the container. Two options are avaible: the object index within the
;   container or the name of the graphic object.
;
; :Private:
;
; :Params:
;       ISRANGE:            in, required, type=intarr
;                           A vector of 1's and 0's indicating if the corresponding
;                               subscript parameters `SUBSCRIPT1` are index ranges or
;                               index values.
;       SUBSCRIPT1:         in, required, type=intarr/strarr
;                           Index subscript of the graphics object to be returned, or the
;                               class names of the objects to return.
;-
function Mr_Window::_OverloadBracketsRightSide, isRange, subscript1
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Search for objects by name. Return the first match.
    if size(subscript1, /TNAME) eq 'STRING' then begin
        if MrIsA(subscript1, /SCALAR) eq 0 then message, 'String subscripts must be scalars.'
        upSub1 = strupcase(subscript1)

        ;Get all of the objects
        allObjs = self -> Get(/ALL, COUNT=nObj)
        i = 0
        success = 0
        
        ;Search for the 
        while success eq 0 and i lt nObj do begin
            if strupcase(allObjs[i] -> GetName()) eq upSub1 then begin
                success = 1
                result = allObjs[i]
            endif
            i++
        endwhile
        
        if success eq 0 then result = obj_new()
        
    ;Call the superclass's method
    endif else result = self -> MrIDL_Container::_OverloadBracketsRightSide(isRange, subscript1)

    return, result
end


;+
;   The purpose of this method is to provide output when the PRINT procedure is used.
;
; :Private:
;-
function Mr_Window::_OverloadPrint
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
    self -> WhichObjects, printText

    ;Combine the results
    outText = [objStr, printText]
    return, transpose(outText)
end


;+
;   Change selection state of graphics objects.
;
; :Params:
;       OBJECT:         in, optional, type=objref
;                       The MrGraphics object for which the selection is to be changed.
;
; :Keywords:
;       ADD:            in, optional, type=boolean, default=0
;                       If set, `OBJECT` will be added to the current selection.
;       ALL:            in, optional, type=boolean, default=0
;                       If set, all graphics in the window will selected.
;       CLEAR:          in, optional, type=boolean, default=0
;                       If set, selected items will be deselected.
;       TOGGLE:         in, optional, type=boolean, default=0
;                       If set, then if `OBJECT ` is selected it will be unselected and
;                           vice versa.
;       UNSELECT:       in, optional, type=boolean, default=0
;                       If set, `OBJECT` will be unselected.
;-
pro Mr_Window::_SetSelect, object, $
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
;   The purpose of this method is to add a Mr_Window object to the !MrWindows system
;   variable. If the system variable does not exist, it will be created.
;
; :Private:
;-
pro Mr_Window::_SysVAdd, object
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Was a MrWindow object given?
    if obj_class(object) ne 'MR_WINDOW' then $
        message, 'Only MrWindow objects can be added to !MR_WINDOWS.'

    ;Does the system variable exist?
    if self -> _SysVExists() eq 0 $
        then defsysv, '!MR_WINDOWS', obj_new('MrWindow_Container')
    
    ;Add the window to the beginning of the container
    !MR_WINDOWS -> Add, self, POSITION=0
end


;+
;   The purpose of this method is to check if the !MrWindows system variable exists.
;
; :Private:
;
; :Returns:
;       EXISTS:             True (1) if !MrWindows exists. False (0) if not.
;-
function Mr_Window::_SysVExists
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
;   The purpose of this method is to remove a MrWindow object from the !MrWindows
;   system variable.
;
; :Private:
;-
pro Mr_Window::_SysVRemove, object
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Make sure MrWindow object was given
    if obj_class(object) ne 'MR_WINDOW' then $
        message, 'Only Mr_Window objects can be removed from !MR_WINDOWS.'
        
    ;Check if the system variable exists
    if self -> _SysVExists() eq 0 then return
    
    ;Remove the window
    !MR_WINDOWS -> Remove, self
end


;+
;   Add a MrLayout object to the MrLayoutManager so that it can be managed.
;
; :Private:
;
; :Params:
;       CHILD_LAYOUT:       in, required, type=object
;                           A MrLayout object that the layout manager can keep track of.
;-
pro Mr_Window::AddToLayout, child_layout
   compile_opt strictarr
   on_error, 2
   
   ;Add the child layout object to the container.
   self._oLayoutMGR -> Add, child_layout
end


;+
;   Close the window and destroy everything in it.
;-
pro Mr_Window::Close
   compile_opt strictarr
   on_error, 2
   
   ;Destroy self to close window and clean up
   obj_destroy, self
end


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
pro Mr_Window::Draw, $
 BACKGROUND_COLOR=bg_color, $
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
    if (WindowAvailable(self -> GetWinID()) eq 0) then $
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
        self._oPixmap -> SetCurrent
        if noerase eq 0B then self -> Erase, bg_color
    endif

    ;Draw all of the objects in the container.
    allObj = self -> Get(/ALL, COUNT=nObj)
    for i = 0, nObj - 1 do allObj[i] -> Draw, /NOERASE
    
    ;Copy from the pixmap to the draw window
    if (!d.flags and 256) gt 0 then begin
        self -> SetCurrent
        self -> Copy_Pixmap
    endif
end


;+
;   The purpose of this method is to erase the draw window.
;
; :Private:
;-
pro Mr_Window::Event_Handler, event
   compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Print the type of event
    sName = size(event, /SNAME)
    case sName of
        'WIDGET_KILL_REQUEST': self -> TLB_Kill_Request, event
        'WIDGET_BASE':         self -> TLB_Resize, event
        else: print, 'Event from "' + sName + '" passed through.'
    endcase
end


;+
;   A simple way of obtaining the window's name.
;
;   :Returns:
;       NAME:           The name of the window
;-
function Mr_Window::GetName
    return, self.name
end


;+
;   Get class properties
;
; :Keywords;
;       NAME:           out, optional, type=string
;                       Name of the graphic.
;       _REF_EXTRA:     out, optional, type=any
;                       Any keyword appropriate MrWidgetDraw and MrLayoutManager.
;-
PRO Mr_Window::GetProperty, $
LAYOUTMGR=layoutMGR, $
NAME = name, $
_REF_EXTRA = extra
    
    ;Properties
    if arg_present(layoutMGR) then layoutMGR = self._oLayoutMGR
    if arg_present(name)      then name = self.name
    
    ;EXTRA
    if n_elements(extra) gt 0 then begin
        ;Keywords accepted by the layout manager
        layout_keys = ['Aspect', 'Col_Width', 'CharSize', 'IXMargin', 'IYMargin', $
                       'Layout', 'Margin',    'OXMargin', 'OYMargin', 'Row_Height', $
                       'XGap',   'YGap']
    
        ;Separate LayoutManager from DrawWidget keywords.
        void = MrIsMember(layout_keys, extra, iLayout, $
                          COUNT       = nLayout, $
                          COMPLEMENT  = iDraw, $
                          NCOMPLEMENT = nDraw)
    
        ;MrWidgetDraw
        if nDraw   gt 0 then self -> MrDrawWidget::GetProperty, _EXTRA=extra[iDraw]
        if nLayout gt 0 then self.oLayoutMGR -> GetProperty, _STRICT_EXTRA=extra[iLayout]
    endif
end


;+
;   Get object references of all celected objects.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Name variable to receive the number of object references returned.
;
; :Returns:
;       SELECTION:      All currently selected graphics objects.
;-
function Mr_Window::GetSelect, $
COUNT=count
    return, self._selection -> Get(/ALL, COUNT=count)
end


;+
;   Determine if a point is inside of a graphics object.
;
; :Params:
;       X:              in, required, type=float
;                       x-coordinate of a point in the graphics window.
;       Y:              in, required, type=float
;                       y-coordinate of a point in the graphics window.
;
; :Keywords:
;       DIMENSIONS:     in, optional, type=intarr(2)
;                       The [X,Y] dimensions of a box centered on `X` and `Y` to use
;                           for the hit test. Device coordinates.
;       ISA:            in, optional, type=string/strarr
;                       Name or names of object classes to be tested.
;       COUNT:          out, optional, type=integer
;                       Number of objects into which the point (`X`,`Y`) falls.
;
; :Returns:
;       HITOBJS:        Objects inside of which which the point (`X`,`Y`) resides.
;-
function Mr_Window::HitTest, x, y, $
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
;   Determine if objects are selected.
;
; :Params:
;       OBJECT:         in, required, type=objref/objarr
;                       Determine if these objects are selected.
;
; :Returns:
;       TF_SELECTED:    Returns true (1) of `OBJECT` is selected and false (0) otherwise.
;-
function Mr_Window::IsSelected, object
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
    return, self._selection -> IsContained(object)
end


;+
;   Set this window as the current window.
;-
pro Mr_Window::SetCurrent
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
    self -> MrDrawWidget::SetCurrent
end


;+
;   The purpose of this method is to provide a means of changing graphics objects
;   globally, like setting the ![PXYZ] system variables.
;-
pro Mr_Window::SetGlobal, $
ISA=isa, $
CHARSIZE = charsize, $
CHARTHICK = charthick, $
FONT = font, $
RANGE = range, $
TICKLEN = ticklen, $
TITLE = title, $
THICK = thick, $
XCHARSIZE = xcharsize, $
XGRIDSTYLE = xgridstyle, $
XLOG = xlog, $
XMINOR = xminor, $
XRANGE = xrange, $
XSTYLE = xstyle, $
XTHICK = xthick, $
XTICKFORMAT = xtickformat, $
XTICKLAYOUT = xticklayout, $
XTICKLEN = xticklen, $
XTICKNAME = xtickname, $
XTICKS = xticks, $
XTICKUNITS = xtickunits, $
XTICKV = xtickv, $
XTITLE = xtitle, $
YCHARSIZE = ycharsize, $
YGRIDSTYLE = ygridstyle, $
YLOG = ylog, $
YMINOR = yminor, $
YRANGE = yrange, $
YSTYLE = ystyle, $
YTHICK = ythick, $
YTICKFORMAT = ytickformat, $
YTICKLAYOUT = yticklayout, $
YTICKLEN = yticklen, $
YTICKNAME = ytickname, $
YTICKS = yticks, $
YTICKUNITS = ytickunits, $
YTICKV = ytickv, $
YTITLE = ytitle, $
ZCHARSIZE = zcharsize, $
ZGRIDSTYLE = zgridstyle, $
ZLOG = zlog, $
ZMINOR = zminor, $
ZRANGE = zrange, $
ZSTYLE = zstyle, $
ZTHICK = zthick, $
ZTICKFORMAT = ztickformat, $
ZTICKLAYOUT = zticklayout, $
ZTICKLEN = zticklen, $
ZTICKNAME = ztickname, $
ZTICKS = zticks, $
ZTICKUNITS = ztickunits, $
ZTICKV = ztickv, $
ZTITLE = ztitle
    compile_opt idl2

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Refresh, DISABLE=~refresh_in
        void = cgErrorMsg()
        return
    endif

    ;Disable refresh
    self -> GetProperty, REFRESH=refresh_in
    self -> Refresh, /DISABLE
    
    ;If we are changing CHARSIZE, set the character size of the window, too
    if n_elements(charsize) gt 0 then self -> SetProperty, CHARSIZE=charsize

    ;Get all of the objects
    allObjs = self -> Get(/ALL, ISA=isa, COUNT=nObjs)
    if nObjs eq 0 then return
    
    ;Step through each object and set the pertinent values
    for i = 0, nObjs - 1 do begin
        oClass = obj_class(allObjs[i])
    
;---------------------------------------------------------------------
;Data Objects ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
        if IsMember((*self.gTypes).data, oClass) then begin
            allObjs[i] -> SetProperty, CHARSIZE    = charsize, $
                                       CHARTHICK   = charthick, $
                                       FONT        = font, $
                                       THICK       = thick, $
                                       TICKLEN     = ticklen, $
                                       TITLE       = title, $
                                       XCHARSIZE   = xcharsize, $
                                       XGRIDSTYLE  = xgridstyle, $
                                       XLOG        = xlog, $
                                       XMINOR      = xminor, $
                                       XRANGE      = xrange, $
                                       XSTYLE      = xstyle, $
                                       XTHICK      = xthick, $
                                       XTICKFORMAT = xtickformat, $
                                       XTICKLAYOUT = xticklayout, $
                                       XTICKLEN    = xticklen, $
                                       XTICKNAME   = xtickname, $
                                       XTICKS      = xticks, $
                                       XTICKUNITS  = xtickunits, $
                                       XTICKV      = xtickv, $
                                       XTITLE      = xtitle, $
                                       YCHARSIZE   = ycharsize, $
                                       YGRIDSTYLE  = ygridstyle, $
                                       YLOG        = ylog, $
                                       YMINOR      = yminor, $
                                       YRANGE      = yrange, $
                                       YSTYLE      = ystyle, $
                                       YTHICK      = ythick, $
                                       YTICKFORMAT = ytickformat, $
                                       YTICKLAYOUT = yticklayout, $
                                       YTICKLEN    = yticklen, $
                                       YTICKNAME   = ytickname, $
                                       YTICKS      = yticks, $
                                       YTICKUNITS  = ytickunits, $
                                       YTICKV      = ytickv, $
                                       YTITLE      = ytitle, $
                                       ZCHARSIZE   = zcharsize, $
                                       ZGRIDSTYLE  = zgridstyle, $
;                                       ZLOG        = zlog, $
                                       ZMINOR      = zminor, $
                                       ZRANGE      = zrange, $
                                       ZSTYLE      = zstyle, $
                                       ZTHICK      = zthick, $
                                       ZTICKFORMAT = ztickformat, $
                                       ZTICKLAYOUT = zticklayout, $
                                       ZTICKLEN    = zticklen, $
                                       ZTICKNAME   = ztickname, $
                                       ZTICKS      = zticks, $
                                       ZTICKUNITS  = ztickunits, $
                                       ZTICKV      = ztickv, $
                                       ZTITLE      = ztitle
        endif

;---------------------------------------------------------------------
;Axis Objects ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
        if oClass eq 'MRAXIS' then begin
            allObjs[i] -> GetProperty, DIRECTION=direction
            
            case direction of
                'X': begin
                    allObjs[i] -> SetProperty, AXIS_RANGE   = xrange, $
                                               CHARSIZE     = charsize, $
                                               CHARTHICK    = charthick, $
                                               COLOR        = color, $
                                               FONT         = font, $
                                               GRIDSTYLE    = xgridstyle, $
                                               LOG          = xlog, $
                                               MINOR        = xminor, $
                                               SUBTITLE     = xsubtitle, $
                                               STYLE        = xsytle, $
                                               THICK        = xthick, $
                                               TICKFORMAT   = xtickformat, $
                                               TICKINTERVAL = xtickinterval, $
                                               TICKLAYOUT   = xticklayout, $
                                               TICKLEN      = xticklen, $
                                               TICKNAME     = xtickname, $
                                               TICKS        = xticks, $
                                               TICKUNITS    = xtickunits, $
                                               TICKVALUES   = xtickvalues, $
                                               TITLE        = xtitle, $
                                               XCHARSIZE    = xcharsize
                endcase
                
                'Y': begin
                    allObjs[i] -> SetProperty, AXIS_RANGE   = yrange, $
                                               CHARSIZE     = charsize, $
                                               CHARTHICK    = charthick, $
                                               COLOR        = color, $
                                               FONT         = font, $
                                               GRIDSTYLE    = ygridstyle, $
                                               LOG          = ylog, $
                                               MINOR        = yminor, $
                                               SUBTITLE     = ysubtitle, $
                                               STYLE        = ysytle, $
                                               THICK        = ythick, $
                                               TICKFORMAT   = ytickformat, $
                                               TICKINTERVAL = ytickinterval, $
                                               TICKLAYOUT   = yticklayout, $
                                               TICKLEN      = yticklen, $
                                               TICKNAME     = ytickname, $
                                               TICKS        = yticks, $
                                               TICKUNITS    = ytickunits, $
                                               TICKVALUES   = ytickvalues, $
                                               TITLE        = ytitle, $
                                               XCHARSIZE    = ycharsize
                
                endcase
                
                'Z': begin
                    allObjs[i] -> SetProperty, AXIS_RANGE   = zrange, $
                                               CHARSIZE     = charsize, $
                                               CHARTHICK    = charthick, $
                                               COLOR        = color, $
                                               FONT         = font, $
                                               GRIDSTYLE    = zgridstyle, $
                                               LOG          = zlog, $
                                               MINOR        = zminor, $
                                               SUBTITLE     = zsubtitle, $
                                               STYLE        = zsytle, $
                                               THICK        = zthick, $
                                               TICKFORMAT   = ztickformat, $
                                               TICKINTERVAL = ztickinterval, $
                                               TICKLAYOUT   = zticklayout, $
                                               TICKLEN      = zticklen, $
                                               TICKNAME     = ztickname, $
                                               TICKS        = zticks, $
                                               TICKUNITS    = ztickunits, $
                                               TICKVALUES   = ztickvalues, $
                                               TITLE        = ztitle, $
                                               XCHARSIZE    = zcharsize
                endcase
            endcase

;---------------------------------------------------------------------
; Images /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
        endif else if oClass eq 'MRIMAGE' then begin
            allObjs[i] -> SetProperty, RANGE=range
            
        endif else if oClass eq 'WECOLORBAR' then begin
            allObjs[i] -> SetProperty, RANGE=range, CHARSIZE=charsize, $
                                       CHARTHICK=charthick, TCHARSIZE=charsize
            
;---------------------------------------------------------------------
;Other Annotation Objects ////////////////////////////////////////////
;---------------------------------------------------------------------
        endif else if IsMember(['MRTEXT', 'WELEGENDITEM'], oClass) then begin
            allObjs[i] -> SetProperty, CHARSIZE = charsize, $
                                       CHARTHICK = charthick
        endif
    endfor
        
    ;Reset the refresh state.
    self -> Refresh, DISABLE=~refresh_in
end


;+
;   Set class properties
;
; :Keywords:
;       NAME:           in, optional, type=string
;                       Name of the graphics object.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword appropriate for MrLayoutManager and MrDrawWidget.
;-
PRO Mr_Window::SetProperty, $
NAME = name, $
_REF_EXTRA = extra
   compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Object Properties
    if n_elements(name) gt 0 then self.name = name
    
    ;EXTRA
    if n_elements(extra) gt 0 then begin
        ;Keywords accepted by the layout manager
        layout_keys = ['Aspect', 'Col_Width', 'CharSize', 'IXMargin', 'IYMargin', $
                       'Layout', 'Margin',    'OXMargin', 'OYMargin', 'Row_Height', $
                       'XGap',   'YGap']
    
        ;Separate LayoutManager from DrawWidget keywords.
        void = MrIsMember(layout_keys, extra, iLayout, $
                          COUNT       = nLayout, $
                          COMPLEMENT  = iDraw, $
                          NCOMPLEMENT = nDraw)
    
        ;MrWidgetDraw
        if nDraw   gt 0 then self -> MrDrawWidget::SetProperty, _EXTRA=extra[iDraw]
        if nLayout gt 0 then self._oLayoutMGR -> SetProperty, _STRICT_EXTRA=extra[iLayout]
    endif
end


;+
;   Event handling method for TLB_Size_Events. This method is only called if PARENT was
;   not provided when the draw widget was made.
;
; :Private:
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;-
pro Mr_Window::TLB_Resize, event
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Get Geometry of outer-most bases
    wID         = self._oTLb -> Find_By_uName('BOTTOM_BASE')
    botGEO      = widget_info(wID, /GEOMETRY)
    self._oTLB -> GetProperty, GEOMETRY=tlbGEO
    
    ;Calculate size of draw widget.
    ;   - Subtract padding from tlb and bottom base.
    ;   - Subtract height of bottom base.
    xnew = event.x - 2*tlbGEO.xpad
    ynew = event.y - 2*tlbGEO.ypad - 2*botGEO.ypad - botGEO.ysize

    ;Set the new size of the draw widget and the pixmap.
    self._oPixmap -> Resize, xNew, yNew
    self          -> Resize, xNew, yNew
    
    ;Draw occurs in MrDrawWidget::Resize
end


;+
;   Handle TLB_Kill_Request Events.
;
; :Private:
;
; :Params:
;       EVENT:              in, optional, type=structure
;                           An event structure returned by the windows manager.
;-
pro Mr_Window::TLB_Kill_Request, event
    on_error, 2
    obj_destroy, self
end


;+
;   Print which data objects are present and the index at which they are stored.
;
; :Private:
;-
pro Mr_Window::WhichDataObjects, outText
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
pro Mr_Window::WhichObjects, outText, $
DATA=data, $
ANNOTATE=annotate
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    data     = keyword_set(data)
    annotate = keyword_set(annotate)
    all      = data + annotate eq 1 ? 0 : 1
    
;---------------------------------------------------------------------
;Annotation Objects //////////////////////////////////////////////////
;---------------------------------------------------------------------
    allObj = self -> Get(/ALL, COUNT=nObj)
    if nObj eq 0 then begin
        if n_params() eq 1 $
            then outText = 'No objects in the container.' $
            else print, 'No objects in the container.'
        return
    endif

    ;Description plus header
    dataText = strarr(nObj + 1)
    annText  = strarr(nObj + 1)
    
    ;Get the container indices of each object.
    index = self -> GetIndex(allObj) 

    ;The string length of the longest type
    classLen = string(max(strlen(MrObj_Class(allObj))) > 8, FORMAT='(i0)')
    header   = string('--Order--', '--Type--', '--Name--', $
                      FORMAT='(a9, 4x, a' + classLen + ', 4x, a12)')
    dataText[0] = header
    annText[0]  = header
    
    ;Step through each annotate object
    nData     = 0L
    nAnnotate = 0L
    for i = 0L, nObj - 1 do begin
        ;Only data/annotate?
        isData = obj_isa(allObj[i], 'MRGRDATAATOM')
        if all eq 0 then begin
            if data     then if ~isData then continue
            if annotate then if  isDate then continue
        endif
        
        ;Print the type-name, location, and position
        sIndex = string(index[i], FORMAT='(i2)')
        sName  = allObj[i] -> GetName()

        ;Form the output string
        tempText = string(FORMAT='(4x, a2, 7x, a' + classLen + ', 5x, a0)', $
                          sIndex, obj_class(allObj[i]), sName)
        
        if isData then begin
            dataText[nData+1] = tempText
            nData += 1
        endif else begin
            annText[nAnnotate+1] = tempText
            nAnnotate += 1
        endelse
    endfor
    
    ;Were data/annotate objects found?
    if nData     eq 0 then dataText[1] = '  No Data objects'
    if nAnnotate eq 0 then annText[1]  = '  No Annotate objects'
    
    ;Form the output
    case 1 of
        all:      outText = [dataText[0:nData > 1], annText[0:nAnnotate > 1]]
        data:     outText = dataText[0:nData > 1]
        annotate: outText = annText[0:nAnnotate > 1]
    endcase
    
    ;Output the text
    if n_params() ne 1 then print, transpose(outText)
end


;+
;   This is the Mr_Window object class destructor method.
;
; :Private:
;-
pro Mr_Window::cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Free event handlers (but do not destroy event handling objects)
    if obj_valid(self._selection)  then obj_destroy, self._selection
    if obj_valid(self._oLayoutMGR) then obj_destroy, self._oLayoutMGR

    ;Destroy the widget, if it still exists
    if widget_info(self._tlbID, /VALID_ID) then widget_control, self._tlbID, /DESTROY
    
    ;Clean up the superclasses
    self -> MrDrawWidget::Cleanup
end


;+
;   This is the Mr_Window object class initialization method
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
;       WINDOW_TITLE:   in, optional, type=string, default='Mr_Window'
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
;                       Any keyword appropriate for MrDrawWidget::Init.
;-
function Mr_Window::init, parent, $
ASPECT     = aspect, $
BUFFER     = buffer, $
COL_WIDTH  = col_width, $
CHARSIZE   = charsize, $
IXMARGIN   = ixmargin, $
IYMARGIN   = iymargin, $
LAYOUT     = layout, $
MARGIN     = margin, $
NAME       = name, $
OXMARGIN   = oxmargin, $
OYMARGIN   = oymargin, $
REFRESH    = refresh, $
ROW_HEIGHT = row_height, $
WINDOW_TITLE = window_title, $
XGAP       = xgap, $
XSIZE      = xsize, $
YGAP       = ygap, $
YSIZE      = ysize, $
_REF_EXTRA = extra
   compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Default window title and size
    refresh = n_elements(refresh) eq 0 ? 1 : keyword_set(refresh)
    if n_elements(window_title) eq 0 then window_title = 'Mr_Window'
    if n_elements(xsize)        eq 0 then xsize        = 600
    if n_elements(ysize)        eq 0 then ysize        = 600

    ;Build the window first so that Layout can get the dimensions of the window.
    self._selection = obj_new('MrIDL_Container')
    self -> _BuildWindow, WINDOW_TITLE = window_title, $
                          XSIZE        = xsize, $
                          YSIZE        = ysize, $
                          _EXTRA       = extra
    if obj_valid(self._oTLB) eq 0 then return, 0
    
    ;Initialize the layout manager
    self._oLayoutMGR = obj_new('MrLayoutManager', $
                               ASPECT     = aspect, $
                               COL_WIDTH  = col_width, $
                               CHARSIZE   = charsize, $
                               IXMARGIN   = ixmargin, $
                               IYMARGIN   = iymargin, $
                               LAYOUT     = layout, $
                               MARGIN     = margin, $
                               OXMARGIN   = oxmargin, $
                               OYMARGIN   = oymargin, $
                               ROW_HEIGHT = row_height, $
                               XGAP       = xgap, $
                               YGAP       = ygap)
    if obj_valid(self._oLayoutMGR) eq 0 then return, 0

    ;Add to the container of open windows
    self -> _SysVAdd, self
    
    ;Realize and manage the widget
    self._refresh = refresh
    self._oTLB -> XManager
    if self._refresh then self -> Draw

    return, 1
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro Mr_Window__define, class

   class = { Mr_Window, $
             inherits MrDrawWidget, $
                      ;--> MrWidgetAtom
                      ;    --> IDL_Object
                      ;    MrIDL_Container
                      ;    MrGraphicEventsAdapter
             _oLayoutMGR: obj_new(), $
             _selection:  obj_new(), $
             _statusID:   0L, $
             name:        '' $
           }
end