; docformat = 'rst'
;
; NAME:
;       MrGrDataAtom__Define
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
; PURPOSE:
;+
;   This class is subclass of the MrGrAtom class designed to include properties and
;   methods exclusive to data graphics objects. Annotation graphics objects are included
;   in the MrGrAtom class. Data graphics include: MrPlot, MrImage, MrContour, and MrVector.
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMsg.pro
;       MrGraphicsKeywords__define.pro
;       MrLayout__define.pro
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
;   Modification History::
;       2014/03/26  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The purpose of this method is to print information about the object's properties
;   when the PRINT procedure is used.
;-
function MrGrDataAtom::_OverloadPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, "''"
    endif
    
    undefined = '<undefined>'
    undefObj = '<NullObject>'
    default = '<IDL_Default>'
    joinStr = '   '
    
    ;First, get the results from the superclasses
    atomKeys = self -> MrGrAtom::_OverloadPrint()
    grKeys = self -> MrGraphicsKeywords::_OverloadPrint()
    layKeys = self.layout -> _OverloadPrint()

    ;Class Properties
    overplot  = string('OverPlot',  '=', self.overplot,  FORMAT='(a-26, a-2, i1)')
    xlog      = string('Xlog',      '=', self.xlog,      FORMAT='(a-26, a-2, i1)')
    ylog      = string('YLog',      '=', self.ylog,      FORMAT='(a-26, a-2, i1)')
    max_value = string('Max_Value', '=', FORMAT='(a-26, a-2)')
    min_value = string('Min_Value', '=', FORMAT='(a-26, a-2)')
    target    = string('Target',    '=', FORMAT='(a-26, a-2)')
    
    ;Pointers
    if n_elements(*self.max_value) eq 0 then max_value += default else max_value += string(*self.max_value, FORMAT='(f0)')
    if n_elements(*self.min_value) eq 0 then min_value += default else min_value += string(*self.min_value, FORMAT='(f0)')
    if n_elements(*self.target) eq 0 then target += undefObj else target += strjoin(MrObj_Class(*self.target), joinStr)
    
    ;Put MrGrDataAtom properties together
    selfStr = obj_class(self) + '  <' + strtrim(obj_valid(self, /GET_HEAP_IDENTIFIER), 2) + '>'
    plotKeys = [ [overplot], $
                 [xlog], $
                 [ylog], $
                 [max_value], $
                 [min_value], $
                 [target] $
               ]

    ;Group everything in alphabetical order
    result = [[atomKeys], [grKeys], [layKeys], [plotKeys]]
    result = result[sort(result)]
    
    return, result
end


;+
;   The purpose of this method is to print information about the object's properties
;   when implied print is used.
;-
function MrGrDataAtom::_OverloadImpliedPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, "''"
    endif
    
    result = self -> _OverloadPrint()
    
    return, result
end


;+
;   The purpose of this method is to determine if overplotting is being done.
;
; :Params:
;       TARGET:             out, optional, type=object
;                           If `TF_OVERPLOT`=1, then the overplot target will be returned.
;
; :Returns:
;       TF_OVERPLOT:        True (1) if overplotting, false (0) if not.
;-
function MrGrDataAtom::GetOverplot, $
TARGET=target
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0B
    endif
    
    ;Get the overplot state and the target.
    tf_overplot = self.overplot
    if tf_overplot then if arg_present(target) then target = self.target
    
    return, tf_overplot
end


;+
;   The purpose of this method is to retrieve object properties
;
; :Keywords:
;       _REF_EXTRA:         out, optional, type=any
;                           Any keyword accepted by the MrLayout__Define class is
;                               also accepted via keyword inheritance.
;-
pro MrGrDataAtom::GetLayout, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Layout Properties
    self.layout -> GetProperty, _STRICT_EXTRA=extra
end


;+
;   The purpose of this method is to retrieve object properties
;
; :Keywords:
;       CHARSIZE:           out, optional, type=float
;                           Scale factor for IDL's default character size.
;       HIDE:               out, optional, type=boolean
;                           If set, the graphic will not be displayed.
;       MAX_VALUE:          out, optional, type=float
;                           The maximum value plotted. Any values larger than this are
;                               treated as missing.
;       MIN_VALUE:          out, optional, type=float
;                           The minimum value plotted. Any values smaller than this are
;                               treated as missing.
;       NAME:               out, optional, type=string
;                           A string specifying the name of the graphic. It can be used
;                           retrieve the graphic using bracket array notation.
;       XLOG:               out, optional, type=boolean
;                           Indicates that a log scale is used on the x-axis
;       YLOG:               out, optional, type=boolean
;                           Indicates that a log scale is used on the y-axis
;       WINDOW:             out, optional, type=object
;                           The MrWindow object in which the graphic is displayed.
;       _REF_EXTRA:         out, optional, type=any
;                           Keyword accepted by the superclasses are also accepted for
;                               keyword inheritance.
;-
pro MrGrDataAtom::GetProperty, $
;MrGrDataAtom Properties
CHARSIZE = charsize, $
HIDE = hide, $
LAYOUT = layout, $
NAME = name, $
POSITION = position, $
OVERPLOT = overplot, $
WINDOW = window, $
;Graphics Properties
MAX_VALUE = max_value, $
MIN_VALUE = min_value, $
XLOG = xlog, $
YLOG = ylog, $
;MrGraphicsKeywords Properties
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;MrGrDataAtom Properties
    if arg_present(overplot) then overplot    =  self.overplot
    if arg_present(position) then position    =  self.layout -> GetPosition()
    if arg_present(layout)   then layout      =  self.layout -> GetLayout()
    if arg_present(charsize) then self.layout -> GetProperty, CHARSIZE=charsize
    
    ;Graphics Properties
    if arg_present(max_value) and n_elements(*self.max_value) ne 0 then max_value = *self.max_value
    if arg_present(min_value) and n_elements(*self.min_value) ne 0 then min_value = *self.min_value
    if arg_present(xlog)      gt 0 then xlog = self.xlog
    if arg_present(ylog)      gt 0 then ylog = self.ylog

    ;MrGrAtom
    if arg_present(hide) gt 0 || arg_present(name) || arg_present(window) $
        then self -> MrGrAtom::GetProperty, HIDE=hide, NAME=name, WINDOW=window
    
    ;MrGraphicsKeywords
    if n_elements(extra) gt 0 then self -> MrGraphicsKeywords::GetProperty, _STRICT_EXTRA=extra
end


;+
;   The purpose of this method is to change the plot from a normal plot to an overplot
;   and vice versa.
;
; :Params:
;       TARGET:             in, optional, type=objref
;                           An MrGraphicObject onto which this Plot will be overplotted.
;                               If not present, the currently selected graphic will be
;                               used.
;
; :Keywords:
;       DISABLE:            in, optional, type=boolean, default=0
;                           Convert an overplot to a plot. If set, then `TARGET` may be
;                               a 4-element vector of the form [x0, y0, x1, y1] that
;                               specifies the lower-right and upper-left corners of the
;                               plot. If `TARGET` is not provided, the plot will be placed
;                               at the next available layout location.
;-
pro MrGrDataAtom::Overplot, target, $
DISABLE=disable
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        if n_elements(init_refresh) gt 0 then self.window -> Refresh, DISABLE=~init_refresh
        return
    endif
    
    ;Disable refreshing
    self.window -> GetProperty, REFRESH=init_refresh
    self.window -> Refresh, /DISABLE
    
    ;Disable overplotting
    if keyword_set(disable) then begin
        self.window -> Make_Location, location
        self.window -> SetPosition, self.layout[2], location
        self.overplot = 0B

    ;Enable overplotting        
    endif else begin
        ;If no TARGET was specified, get the currently selected graphic
        if obj_valid(target) eq 0 then target = self.window -> GetSelect()

        ;Ensure we can overplot on top of the target graphic
        oplottable = ['MrPlot', 'MrImage', 'MrContour', 'MrVector']
        if min(IsMember(oplottable, MrObj_Class(target), /FOLD_CASE)) eq 0 || $
           min(obj_valid(target)) eq 0 $
        then message, 'TARGET must be valid and of class ' + strjoin(oplottable, ' ')

        ;Get a position
        target[0] -> GetProperty, POSITION=position

        ;Remove SELF from layout.
        self.layout -> GetProperty, LAYOUT=layout
        self.window -> SetPosition, layout[2], position
        self.overplot = 1B
        self.target = target
        
        ;Fill and trim holes
        if layout[2] gt 0 then self.window -> TrimLayout
    endelse
    
    ;Re-enable refreshing
    self.window -> Refresh, DISABLE=~init_refresh
end


;+
;   The purpose of this method is to set the layout of a plot while maintaining the
;   automatically updating grid.
;
; :Params:
;       LAYOUT:             in, required, type=intarr(3)/intarr(4)
;                           A vector of the form [nCols, nRos, index] or
;                               [nCols, nRows, col, row], indicating the number of columns
;                               and rows in the overall layout (nCols, nRows), the index
;                               where the plot is to be placed ("index", starting with 1
;                               and increasing left->right then top->bottom). Alternatively,
;                               "col" and "row" are the column and row in which the plot
;                               is to be placed. Ignored if `POSITION` is present.
;
; :Keywords:
;       POSITION:           in, optional, type=fltarr(4)
;                           A vector of the form [x0, y0, x1, y1] specifying the lower-left
;                               and upper-right corners of the plot, in normal coordinates.
;                               If this keyword is given, `LAYOUT` is ignored. This keyword
;                               should not be used. Instead use the SetProperty method
;                               (or dot-referencing in IDL 8.0+). This is only meant
;                               for use by MrGrDataAtomManager__Define for synchronizing layout
;                               properties with the window.
;       UPDATE_LAYOUT:      in, optional, type=boolean, default=1
;                           Indicate that the layout is to be updated. All graphics within
;                               the graphics window will be adjusted. This keyword is
;                               used by MrGrDataAtomManager__Define when applying the layout
;                               grid to each plot. It is not meant to be used elsewhere.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrLayout::SetProperty is also accepted
;                               for keyword inheritance.
;-
pro MrGrDataAtom::SetLayout, layout, $
POSITION=position, $
UPDATE_LAYOUT=update_layout, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        if n_elements(init_refresh) gt 0 $
            then self.window -> Refresh, DISABLE=~init_refresh
        return
    endif
    
    ;Default to updating the layout
    update_layout = n_elements(update_layout) eq 0 ? 1 : keyword_set(update_layout) 
    
    ;Turn refresh off.
    self.window -> GetProperty, REFRESH=init_refresh
    self.window -> Refresh, /DISABLE
    
    ;If we are updating the layout, let the window take care of things.
    if update_layout then begin
        ;Get the current layout
        curLayout = self.layout -> GetLayout()

        ;Update the layout
        if n_elements(position) gt 0 $
            then self.window -> SetPosition, curLayout[2], position $
            else self.window -> SetPosition, curLayout[2], layout
        
        ;Adjust other aspects of the layout.
        if n_elements(extra) gt 0 then self.window -> SetProperty, _EXTRA=extra
    
    ;If we are not updating the layout...
    endif else begin
        self.layout -> SetProperty, LAYOUT=layout, POSITION=position, UPDATE_LAYOUT=0, $
                                   _STRICT_EXTRA=extra
    endelse
    
    ;Reset the refresh state.
    self.window -> Refresh, DISABLE=~init_refresh
end


;+
;   The purpose of this method is to set object properties. 
;
; :Keywords:
;       CHARSIZE:           in, optional, type=float
;                           Scale factor for IDL's default character size.
;       MAX_VALUE:          in, optional, type=float
;                           The maximum value plotted. Any values larger than this are
;                               treated as missing.
;       MIN_VALUE:          in, optional, type=float
;                           The minimum value plotted. Any values smaller than this are
;                               treated as missing.
;       POSITION:           in, optional, type=fltarr(4)
;                           A vector of the form [x0, y0, x1, y1] specifying the location
;                               of the lower-left and upper-right corners of the graphic,
;                               in normalized coordinates.
;       XLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the x-axis
;       YLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the y-axis
;       _REF_EXTRA:         in, optional, type=any
;                           Keyword accepted by the MrGrAtom and MrGraphicsKeywords are
;                               also accepted for keyword inheritance.
;-
pro MrGrDataAtom::SetProperty, $
;MrGrDataAtom Properties
CHARSIZE = charsize, $
HIDE = hide, $
LAYOUT = layout, $
NAME = name, $
POSITION = position, $
;Graphics Properties
MAX_VALUE = max_value, $
MIN_VALUE = min_value, $
XLOG = xlog, $
YLOG = ylog, $
;MrGraphicsKeywords Properties
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Graphics Properties
    if n_elements(max_value) ne 0 then *self.max_value = max_value
    if n_elements(min_value) ne 0 then *self.min_value = min_value
    if n_elements(xlog)      ne 0 then  self.xlog      = keyword_set(xlog)
    if n_elements(ylog)      ne 0 then  self.ylog      = keyword_set(ylog)

    if n_elements(position) gt 0 then self -> SetLayout, POSITION=position
    if n_elements(charsize) gt 0 then self -> SetLayout, CHARSIZE=charsize, UPDATE_LAYOUT=0

;---------------------------------------------------------------------
;Superclass Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if (n_elements(name) gt 0) || (n_elements(hide) gt 0) $
        then self -> MrGrAtom::SetProperty, HIDE=hide, NAME=name

    ;MrGraphicsKeywords Properties
    if n_elements(extra) gt 0 $
        then self -> MrGraphicsKeywords::SetProperty, _STRICT_EXTRA=extra
end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrGrDataAtom::cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;free all pointers
    ptr_free, self.max_value
    ptr_free, self.min_value
    
    ;Cleanup the superclass.
    self -> MrGraphicsKeywords::CLEANUP
    self -> MrGrAtom::CLEANUP
end


;+
;   Set properties for data objects.
;
; :Keywords:
;       CURRENT:            in, optional, type=boolean, default=0
;                           If set, the graphic will be added to the current window. If
;                               not, a new window will be created.
;       HIDE:               in, optional, type=boolean, default=0
;                           If set, the graphic will not be displayed.
;       LAYOUT:             in, optional, type=intarr(3)
;                           A vector of the form [nCols, nRows, pIndex], where the first
;                               two elements are the number of columns and rows of plots
;                               whithin the plotting grid, and pIndex is the plot index,
;                               starting with 1 (one) in the upper-left corner of the
;                               grid and increasing first left-to-right then top-to-bottom.
;                               If LAYOUT and `POSITION` are not provided, the graphic
;                               will be placed at the next avaible pIndex location.
;       MAX_VALUE:          in, optional, type=float
;                           The maximum value plotted. Any values larger than this are
;                               treated as missing.
;       MIN_VALUE:          in, optional, type=float
;                           The minimum value plotted. Any values smaller than this are
;                               treated as missing.
;       NAME:               in, optional, type=string, default=Obj_Class(self)
;                           A string specifying the name of the graphic. It can be used
;                               retrieve the graphic using bracket array notation.
;       OVERPLOT:           in, optional, type=boolean/object
;                           Set equal to 1 or to a graphic object refrence. If set to 1,
;                               the graphic will be overploted onto an existing graphic
;                               in the current window. If a graphic is selected, it will
;                               be the target. If no graphics are selected, the highest
;                               ordered graphic will be the target. If no window is open,
;                               a new window will be created. If set to a graphic's object
;                               refrece, that graphic is used as the target of the overplot.
;       POSITION:           in, optional, type=fltarr(4)
;                           A vector of the form [x0, y0, x1, y1], where (x0,y0) specify
;                               the location of the lower-left corner and (x1,y1) specify
;                               the location of the upper-right corner of the graphic.
;       REFRESH:            out, optional, type=boolean
;                           Returns true (1) if the graphic should be refreshed as the
;                               last step in the initialization process or false (0) if
;                               not. Depends on `CURRENT` and `OVERPLOT`.
;       WINDOW_TITLE:       in, optional, type=string, default='MrWindow'
;                           If a new window is created, the title to be placed on the
;                               window's title bar.
;       XLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the x-axis
;       YLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the y-axis
;       _REF_EXTRA:         in, optional, type=any
;                           Keywords accepted by MrGraphicsKeywords are also
;                               accepted for keyword inheritcance.
;-
function MrGrDataAtom::init, x, y, $
;MrGrDataAtom Keywords
CURRENT = current, $
HIDE = hide, $
LAYOUT = layout, $
NAME = name, $
OVERPLOT = target, $
POSITION = position, $
REFRESH = refresh, $
WINDOW_TITLE = window_title, $

;Graphics Keywords
MAX_VALUE = max_value, $
MIN_VALUE = min_value, $
XLOG = xlog, $
YLOG = ylog, $
_REF_EXTRA = extra
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif


;---------------------------------------------------------------------
;Superclass Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------

    ;MrGraphicsKeywords
    if self -> MrGraphicsKeywords::INIT(_STRICT_EXTRA=extra) eq 0 then $
        message, 'Unable to initialize MrGraphicsKeywords.'

;---------------------------------------------------------------------
;Defaults and Heap Values ////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Defaults
    self.xlog    = keyword_set(xlog)
    self.ylog    = keyword_set(ylog)
    
    ;Allocate Heap
    self.min_value = ptr_new(/ALLOCATE_HEAP)
    self.max_value = ptr_new(/ALLOCATE_HEAP)
    
    ;Objects
    self.target = obj_new()

;---------------------------------------------------------------------
;Window and Layout ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;If a target was given, get its position
    ;   - This will prevent MrGrAtom from adding the graphic to the automatically
    ;     updating grid-layout
    if obj_valid(target) then target -> GetProperty, POSITION=position
  
    ;Layout -- Must be done before initializing MrGrAtom
    self.layout = obj_new('MrLayout', LAYOUT=layout, POSITION=position)
    
    ;Was the /OVERPLOT keyword set, instead of giving a target
    if MrIsA(target, /SCALAR, 'INT') $
        then if keyword_set(target) then target = self -> _GetTarget()

    ;Superclass.
    if self -> MrGrAtom::INIT(CURRENT=current, NAME=name, HIDE=hide, TARGET=target, $
                              WINREFRESH=winRefresh, WINDOW_TITLE=window_title) eq 0 $
       then message, 'Unable to initialize MrGrAtom'

;---------------------------------------------------------------------
;Overplot & Refresh //////////////////////////////////////////////////
;--------------------------------------------------------------------- 
    ;Overplot?
    if n_elements(target) gt 0 then self -> Overplot, target

    ;Refresh the graphics?
    refresh = 0B
    if keyword_set(current) eq 0 && n_elements(target) eq 0 $
        then refresh = 1B $
        else if winRefresh then refresh = 1B

    return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrGrDataAtom__define, class
    compile_opt strictarr
    
    class = { MrGrDataAtom, $
              inherits MrGrAtom, $
              inherits MrGraphicsKeywords, $
             
              ;Graphics Properties
              layout:    obj_new(), $
              max_value: ptr_new(), $
              min_value: ptr_new(), $
              overplot:  0B, $
              target:    obj_new(), $
              xlog:      0B, $
              ylog:      0B  $
            }
end