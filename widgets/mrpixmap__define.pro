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
;   Get class properties
;
; :Keywords;
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
;   BACKGROUND_COLOR:   in, optional, type=string
;                       The name of the initial color for the draw widget. Used when
;                           realized and if the draw widget is set up to erase before
;                           display (i.e., NOERASE=0).
;       NOERASE:        in, optional, type=boolean
;                       If set, the draw widget will not be erased before drawing.
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
;   This is the MrPixmap object class initialization method
;
; :Params:
;       PARENT:         in, optional, type=integer/object
;                       Either the widget ID of a parent widget or MrTopLevelBase object
;                           reference. If not provided, the draw widget will be placed
;                           in a resizeable top level base widget.
;
; :Keywords:
;       ASPECT:         in, optional, type=float
;                       The aspect ratio (`XSIZE`:`YSIZE`) of the draw widget to be made.
;                           An aspect ratio of 1.0 produces a square window.
;   BACKGROUND_COLOR:   in, optional, type=string, default='White'
;                       The name of the initial color for the draw widget. Used when
;                           realized and if the draw widget is set up to erase before
;                           display (i.e., NOERASE=0).
;       GROUP_LEADER:   in, optional, type=integer
;                       The ID of a widget that serves as the group leader. If the group
;                           leader is destroyed, all widgets in the group are destroyed
;                           as well.
;       NOERASE:        in, optional, type=boolean, default=0
;                       If set, the draw widget will not be erased before drawing.
;       REFRESH:        In, optional, type=boolean, default=1
;                       Set to zero to prohibit the draw widget from refreshing.
;       RETAIN:         in, optional, type=integer, default="{windows: 1, unix: 2}"
;                       Set this keyword to determine how backing store is handled.
;       SCR_XSIZE:      in, optional, type=integer
;                       Set the screen X size of the base to this many pixels. (Use discouraged.)
;       SCR_YSIZE:      in, optional, type=integer
;                       Set the screen Y size of the base to this many pixels. (Use discouraged.)
;       SCROLL:         in, optional, type=boolean, default=0
;                       Set this keyword to add scroll bars to the draw widget.
;       UNITS:          in, optional, type=integer, default=0
;                       The units for measurments. Choices are::
;                           0 - Pixels
;                           1 - Inches
;                           2 - Centimeters
;       VISIBLE:        in, optional, type=boolean, default=0
;                       Set to 0 to make the pixmap invisible. Set to 1 (one) to make it
;                           visible again.
;       X_SCROLL_SIZE:  in, optional, type=integer
;                       The X size (pixels) of the scrollable window area.
;       XSIZE:          in, optional, type=integer, default=300
;                       The X size of the widget. (300 pixels by default.)
;       Y_SCROLL_SIZE:  in, optional, type=integer
;                       The Y size (pixels) of the scrollable window area
;       YSIZE:          in, optional, type=integer, default=300
;                       The Y size of the widget.
;-
function MrPixmap::init, parent, $
ASPECT=aspect, $
BACKGROUND=background, $
GROUP_LEADER=group_leader, $
NOERASE=noerase, $
REFRESH=refresh, $
RETAIN=retain, $
SCR_XSIZE=scr_xsize, $
SCR_YSIZE=scr_ysize, $
UNITS=units, $
VISIBLE=visible, $
X_SCROLL_SIZE=x_scroll_size, $
XSIZE=xsize, $
Y_SCROLL_SIZE=y_scroll_size, $
YSIZE=ysize
   compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Defaults
    mapIt = keyword_set(visible)
    
    ;Create the parent
    ;   - Make the parent invisible, not just the draw widget.
    ;   - Ensure new base does not flash on the screen before becoming invisible
    ;   - Event management is not needed.
    if n_elements(parent) eq 0 then begin
        parent =  obj_new('MrTopLevelBase', /TLB_SIZE_EVENTS, $
                                            MAP              = 0, $
                                            GROUP_LEADER     = group_leader, $
                                            TITLE            = 'MrPixmap Window')
        
    ;Unmap the parent
    ;   - Widget or object base?
    endif else begin
        if cgObj_IsA(parent, 'MrTopLevelBase') $
            then parent -> SetProperty, MAP=0 $
            else widget_control, parent, MAP=0
    endelse
    
    ;Initialize the draw widget
    success = self -> MrDrawWidget::Init(parent, $
                                         ASPECT=aspect, $
                                         BACKGROUND=background, $
                                         GROUP_LEADER=group_leader, $
                                         NOERASE=noerase, $
                                         REFRESH=refresh, $
                                         RETAIN=retain, $
                                         SCR_XSIZE=scr_xsize, $
                                         SCR_YSIZE=scr_ysize, $
                                         UNITS=units, $
                                         X_SCROLL_SIZE=x_scroll_size, $
                                         XSIZE=xsize, $
                                         Y_SCROLL_SIZE=y_scroll_size, $
                                         YSIZE=ysize)
    if success eq 0 then return, 0
    
    ;Realize the widget
    if obj_valid(self._oTLB) then self._oTLB -> Realize
    
    ;Make visible?
    if mapIt then widget_control, self._tlb, /MAP
    
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