; docformat = 'rst'
;
; NAME:
;       MrLayout__Define
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
; PURPOSE:
;+
;   An object for setting plotting layout properties and graphics positions.
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
;   Modification History::
;       2013/11/20  -   Written by Matthew Argall
;       2013/11/22  -   Added the O[XY]MARGIN, [XY]_REGION, and [XY]_WINDOW properties. - MRA
;       2013/11/24  -   Forgot to add O[XY]MARGIN keywords to Set/GetProperty. Fixed. - MRA
;       2013/11/28  -   LAYOUT property is strictly a 3-element array. - MRA
;       2014/01/10  -   Forgot to set the POSITION property within INIT. Fixed. - MRA
;       2014/01/24  -   Added the _OverloadPrint method. Changed object properties to
;                           floats so that fractions of a character size can be specified. - MRA
;       2014/02/12  -   [XY]MARGIN renamed to O[XY]MARGIN. [IO][XY]MARGIN now have the
;                           same meaning as their direct graphics keywords. - MRA
;       2014/03/10  -   Added the GetPosition and GetLayout methods. - MRA
;       2014/04/16  -   XGAP and YGAP are now pointers, reflecting the fact that they
;                           can be scalars or vectors. - MRA
;-
;*****************************************************************************************
;+
;   Calculate a position based on the layout properties.
;-
function MrLayout::_OverloadPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, fltarr(4)
    endif
    
    undefined = '<undefined>'
    
    ;Column Width
    if n_elements(*self.col_width) eq 0 $
        then col_width = undefined $
        else col_width = '[' + strjoin(string(*self.col_width, FORMAT='(f6.4)'), ', ') + ']'
        
    ;Row Height
    if n_elements(*self.row_height) eq 0 $
        then row_height = undefined $
        else row_height = '[' + strjoin(string(*self.row_height, FORMAT='(f6.4)'), ', ') + ']'
    
    ;Aspect Ratio
    if n_elements(*self.aspect) eq 0 $
        then aspect = undefined $
        else aspect = string(*self.aspect, FORMAT='(f0)')

    ;XGap
    case n_elements(*self.xgap) of
        0:    xgap = undefined
        1:    xgap = string(*self.xgap, FORMAT='(f0.4)')
        else: xgap = '[' + strjoin(string(*self.xgap, FORMAT='(f0.4)'), ', ') + ']'
    endcase
    
    ;YGap
    case n_elements(*self.ygap) of
        0:    ygap = undefined
        1:    ygap = string(*self.ygap, FORMAT='(f0.4)')
        else: ygap = '[' + strjoin(string(*self.ygap, FORMAT='(f0.4)'), ', ') + ']'
    endcase
    
    ;Create the string
    aspect     = string('Aspect',    '=',       aspect,     FORMAT='(a-26, a-2, a0)')
    charsize   = string('CharSize',  '=',  self.charsize,   FORMAT='(a-26, a-2, f0)')
    col_width  = string('Col_Width', '=',       col_width,  FORMAT='(a-26, a-2, a0)')
    ixmargin   = string('IXMargin',  '=',  self.ixmargin,   FORMAT='(a-26, a-2, 2(f0, 2x))')
    iymargin   = string('IYMargin',  '=',  self.iymargin,   FORMAT='(a-26, a-2, 2(f0, 2x))')
    layout     = string('Layout',    '=',  self.layout,     FORMAT='(a-26, a-2, 3(i0, 2x))')
    oxmargin   = string('OXMargin',  '=',  self.oxmargin,   FORMAT='(a-26, a-2, 2(f0, 2x))')
    oymargin   = string('OYMargin',  '=',  self.oymargin,   FORMAT='(a-26, a-2, 2(f0, 2x))')
    position   = string('Position',  '=',  self.position,   FORMAT='(a-26, a-2, 4(f0, 2x))')
    xmargin    = string('IXMargin',  '=',  self.ixmargin,   FORMAT='(a-26, a-2, 2(f0, 2x))')
    row_height = string('Col_Width', '=',       row_height, FORMAT='(a-26, a-2, a0)')
    xgap       = string('XGap',      '=',       xgap,       FORMAT='(a-26, a-2, a0)')
    x_region   = string('X_Region',  '=',  self.x_region,   FORMAT='(a-26, a-2, 2(f0, 2x))')
    x_window   = string('X_Window',  '=',  self.x_window,   FORMAT='(a-26, a-2, 2(f0, 2x))')
    ygap       = string('YGap',      '=',       ygap,       FORMAT='(a-26, a-2, a0)')
    y_region   = string('Y_Region',  '=',  self.y_region,   FORMAT='(a-26, a-2, 2(f0, 2x))')
    y_window   = string('Y_Window',  '=',  self.y_window,   FORMAT='(a-26, a-2, 2(f0, 2x))')
    
    ;Combine all of the string into an array
    result = [ aspect, $
               charsize, $
               col_width, $
               ixmargin, $
               iymargin, $
               layout, $
               oxmargin, $
               oymargin, $
               position, $
               row_height, $
               xmargin, $
               xgap, $
               x_region, $
               x_window, $
               ygap, $
               y_region, $
               y_window $
             ]
    
    ;Return a column vector so that everything is printed on its own line.
    return, '  ' + transpose(result)
end



;+
;   Calculate a position based on the layout properties.
;-
function MrLayout::CalcPosition
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, fltarr(4)
    endif
    
    nLay = n_elements(layout)
    
    ;If the [nCols,nRows] = [0,0] or [col,row] = [0,0] then return current position.
    ;This means the layout or the location within that layout has not be defined.
    if (self.layout[0] eq 0 and self.layout[1]      eq 0) || $
       (self.layout[2] eq 0 and self.layout[nLay-1] eq 0) then return, self.position
    
    ;Make sure the layout is valid.
    if (self.layout[0] lt 0) or (self.layout[1] lt 0) then begin
        message, 'Layout must have at least one column and one row. ' + $
                 'Returning current position.', /INFORMATIONAL
        return, self.position
    endif

    ;Calculate positions
    position = MrLayout(self.layout, $
                        ASPECT     = *self.aspect, $
                        CHARSIZE   =  self.charsize, $
                        COL_WIDTH  = *self.col_width, $
                        OXMARGIN   =  self.oxmargin, $
                        OYMARGIN   =  self.oymargin, $
                        P_REGION   =       p_region, $
                        P_WINDOW   =       p_window, $
                        ROW_HEIGHT = *self.row_height, $
                        XGAP       = *self.xgap, $
                        XMARGIN    =  self.xmargin, $
                        YGAP       = *self.ygap, $
                        YMARGIN    =  self.ymargin)
    
    ;Save the window and region
    self.x_region = p_region[[0,2]]
    self.x_window = P_window[[0,2]]
    self.y_region = p_region[[1,3]]
    self.y_window = p_window[[1,3]]

    return, position    
end


;+
;   Get the position.
;
; :Returns:
;       POSITION:       out, optional, type=intarr(4)
;                       The normalized position of the graphic on the display.
;-
function MrLayout::GetPosition
    return, self.position
end


;+
;   Get the layout.
;
; :Returns:
;       LAYOUT:         [nCols, nRows, index]. [nCols,nRows] is the number of columns and
;                           rows in the display. "index" is the plot index at which to
;                           place the plot, beginning with 1,1], 1 in the upper-left
;                           corner and incrementing first right then down.
;-
function MrLayout::GetLayout
    return, self.layout
end


;+
;   Set properties of the object.
;
; :Keywords:
;       ASPECT:         out, optional, type=float
;                       Aspect ratio of the plot.
;       COL_WIDTH:      in, optional, type=fltarr
;                       Width of each column normalized to the plotting window (area
;                           excluding O[XY]MARGIN.
;       LAYOUT:         out, optional, type=intarr(3)/intarr(4)
;                       [nCols, nRows, index]. [nCols,nRows] is the number of columns and
;                           rows in the display. "index" is the plot index at which to
;                           place the plot, beginning with 1,1], 1 in the upper-left
;                           corner and incrementing first right then down.
;       MARGIN:         out, optional, type=int/intarr(4)
;                       Size of the [left, bottom, right, top] margins, in character
;                           units. If a scalar is provided, all margins will be equal.
;                           This keyword takes precedence over `XMARGIN` and `YMARGIN`.
;       OXMARGIN:       out, optional, type=intarr
;                       Size of the left and right outer margins, in units of !D.X_CH_Size.
;                           Acts as a matte around the X_REGION.
;       OYMARGIN:       out, optional, type=intarr
;                       Size of the bottom and top outer margins, in units of !D.Y_CH_Size.
;                           Acts as a matte around the Y_REGION.
;       POSITION:       out, optional, type=intarr(4)
;                       The normalized position of the graphic on the display.
;       ROW_HEIGHT:     in, optional, type=fltarr
;                       Height of each row, normalized to the width of the plotting region
;                           (area excluding OYMARGIN).
;       XMARGIN:        out, optional, type=intarr(2)
;                       Width of the left and right margins in character units.
;       XGAP:           out, optional, type=integer
;                       Horizontal space between plots, in character units.
;       X_REGION:       out, optional, type=fltarr(2)
;                       Left and right coordinates of the region containing the plot
;       X_WINDOW:       out, optional, type=fltarr(2)
;                       Left and right coordinates of the window outlined by the axes
;       YMARGIN:        out, optional, type=intarr(2)
;                       Height of the top and bottom margins in character units.
;       YGAP:           out, optional, type=integer
;                       Vertical space between plots in character units.
;       Y_REGION:       out, optional, type=fltarr(2)
;                       Bottom and top coordinates of the region containing the plot
;       Y_WINDOW:       out, optional, type=fltarr(2)
;                       Bottom and top coordinates of the window outlined by the axes
;-
pro MrLayout::GetProperty, $
ASPECT=aspect, $
CHARSIZE=charsize, $
LAYOUT=layout, $
MARGIN=margin, $
OXMARGIN=oxmargin, $
OYMARGIN=oymargin, $
POSITION=position, $
XMARGIN=xmargin, $
XGAP=xgap, $
X_REGION=x_region, $
X_WINDOW=x_window, $
YMARGIN=ymargin, $
YGAP=ygap, $
Y_REGION=y_region, $
Y_WINDOW=y_window
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Set GraphicAtom properties
    if arg_present(aspect)     then aspect     = *self.aspect
    if arg_present(charsize)   then charsize   =  self.charsize
    if arg_present(col_width)  then col_width  = *self.col_width
    if arg_present(layout)     then layout     =  self.layout
    if arg_present(oxmargin)   then oxmargin   =  self.oxmargin
    if arg_present(oymargin)   then oymargin   =  self.oymargin
    if arg_present(position)   then position   =  self.position
    if arg_present(row_height) then row_height = *self.row_height
    if arg_present(xmargin)    then xmargin    =  self.xmargin
    if arg_present(xgap)       then xgap       = *self.xgap
    if arg_present(x_region)   then x_region   =  self.x_region
    if arg_present(x_window)   then x_window   =  self.x_window
    if arg_present(ymargin)    then ymargin    =  self.ymargin
    if arg_present(ygap)       then ygap       = *self.ygap
    if arg_present(y_region)   then y_region   =  self.y_region
    if arg_present(y_window)   then y_window   =  self.y_window
    if arg_present(margin)     then margin     = [self.oxmargin[0], self.oymargin[0], $
                                                  self.oxmargin[1], self.oymargin[1]]
end


;+
;   Set properties of the object.
;
; :Keywords:
;       ASPECT:         in, optional, type=float
;                       Aspect ratio of the plot.
;       CHARSIZE:       in, optional, type=float
;                       Fraction of IDL's default character size. Used to determine size
;                           of `XMARGIN`, `YMARGIN`, `XGAP` and `YGAP`.
;       COL_WIDTH:      in, optional, type=fltarr
;                       Width of each column normalized to the plotting window (area
;                           excluding O[XY]MARGIN.
;       LAYOUT:         in, optional, type=intarr(3)/intarr(4)
;                       [nCols, nRows, index]. [nCols,nRows] is the number of columns and
;                           rows in the display. "index" is the plot index at which to
;                           place the plot, beginning with 1,1], 1 in the upper-left
;                           corner and incrementing first right then down. If
;                           proveded, then `POSITION` will be calculated, unless
;                           `UPDATEPOSITION`=0.
;       MARGIN:         in, optional, type=int/intarr(4)
;                       Size of the [left, bottom, right, top] margins, in character
;                           units. If a scalar is provided, all margins will be equal.
;                           This keyword takes precedence over `XMARGIN` and `YMARGIN`.
;       OXMARGIN:       in, optional, type=intarr
;                       Size of the left and right outer margins, in units of !D.X_CH_Size.
;                           Acts as a matte around the X_REGION.
;       OYMARGIN:       in, optional, type=intarr
;                       Size of the bottom and top outer margins, in units of !D.Y_CH_Size.
;                           Acts as a matte around the Y_REGION.
;       POSITION:       in, optional, type=intarr(4)
;                       The normalized position at which to place the graphic on the
;                           display. If provided, then `LAYOUT` will be reset.
;       ROW_HEIGHT:     in, optional, type=fltarr
;                       Height of each row, normalized to the width of the plotting region
;                           (area excluding OYMARGIN).
;       UPDATE_LAYOUT:  in, optional, type=boolean, default=1
;                       If set, the position will be updated based on the new layout. If
;                           `POSITION` was provided, then `LAYOUT`[2:3]=0 to reflect a
;                           user-defined position.
;       XMARGIN:        in, optional, type=intarr(2)
;                       Width of the left and right margins in character units.
;       XGAP:           in, optional, type=integer
;                       Horizontal space between plots, in character units.
;       YMARGIN:        in, optional, type=intarr(2)
;                       Height of the top and bottom margins in character units.
;       YGAP:           in, optional, type=integer
;                       Vertical space between plots in character units.
;-
pro MrLayout::SetProperty, $
ASPECT=aspect, $
CHARSIZE=charsize, $
COL_WIDTH=col_width, $
IXMARGIN=ixmargin, $
IYMARGIN=iymargin, $
LAYOUT=layout, $
MARGIN=margin, $
OXMARGIN=oxmargin, $
OYMARGIN=oymargin, $
POSITION=position, $
ROW_HEIGHT=row_height, $
UPDATE_LAYOUT=update_layout, $
XGAP=xgap, $
YGAP=ygap
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Default to updating the position
    if n_elements(update_layout) eq 0 $
        then update_layout=1 $
        else update_layout = keyword_set(update_layout)
    
    ;Margin -- Must be checked before [XY]MARGIN
    case n_elements(margin) of
        0: ;Do nothing
        1: begin
            oxmargin = [margin, margin]
            oymargin = [margin, margin]
        endcase
        4: begin
            oxmargin = margin[[0,2]]
            oymargin = margin[[1,3]]
        endcase
        else: message, 'MARGIN must be a scalar or 4-element vector.'
    endcase
    
    ;Set Properties. Make sure layout elements are always >= 0.
    if n_elements(aspect)     ne 0 then *self.aspect     = aspect
    if n_elements(charsize)   gt 0 then  self.charsize   = charsize
    if n_elements(col_width)  gt 0 then *self.col_width  = col_width
    if n_elements(iymargin)   ne 0 then  self.iymargin   = iymargin
    if n_elements(ixmargin)   ne 0 then  self.ixmargin   = ixmargin
    if n_elements(layout)     ne 0 then  self.layout     = layout
    if n_elements(oxmargin)   gt 0 then  self.oxmargin   = oxmargin
    if n_elements(oymargin)   gt 0 then  self.oymargin   = oymargin
    if n_elements(row_height) gt 0 then *self.row_height = row_height
    if n_elements(xgap)       ne 0 then *self.xgap       = xgap
    if n_elements(ygap)       ne 0 then *self.ygap       = ygap
    
    ;Update the position to fit the new layout?
    if update_layout eq 1 then begin
        new_position = self -> CalcPosition()
        if array_equal(new_position, fltarr(4)) eq 0 $
            then self.position = new_position
    endif

    ;If a position was given, then reset the layout
    if n_elements(position) ne 0 then begin
        ;Update the layout to reflect a user-defined position has been given. 
        if update_layout eq 1 then *self.layout = [(*self.layout)[0:1],0]
        
        ;Update the position.
        self.position = position
    endif
end


;+
;   Clean up after the object is destroy
;-
pro MrLayout::cleanup
    ptr_free, self.aspect
    ptr_free, self.col_width
    ptr_free, self.row_height
    ptr_free, self.xgap
    ptr_free, self.ygap
end


;+
;   The initialization method.
;
; :Keywords:
;       ASPECT:         in, optional, type=float
;                       Aspect ratio of the plot.
;       CHARSIZE:       in, optional, type=float, default=1.5
;                       Fraction of IDL's default character size. Used to determine size
;                           of `XMARGIN`, `YMARGIN`, `XGAP` and `YGAP`.
;       COL_WIDTH:      in, optional, type=fltarr
;                       Width of each column normalized to the plotting window (area
;                           excluding O[XY]MARGIN. The default is to make equal-width
;                           columns.
;       LAYOUT:         in, optional, type=intarr(3)/intarr(4), default="[1,1,1,1]"
;                       [nCols, nRows, index]. [nCols,nRows] is the number of columns and
;                           rows in the display. "index" is the plot index at which to
;                           place the plot, beginning with 1,1], 1 in the upper-left
;                           corner and incrementing first right then down.
;       MARGIN:         in, optional, type=int/intarr(4), default="[10,4,3,2]"
;                       Size of the [left, bottom, right, top] margins, in character
;                           units. If a scalar is provided, all margins will be equal.
;                           This keyword takes precedence over `XMARGIN` and `YMARGIN`.
;       OXMARGIN:       in, optional, type=intarr, default="[0,0]"
;                       Size of the left and right outer margins, in units of !D.X_CH_Size.
;                           Acts as a matte around the [XY]_REGION.
;       OYMARGIN:       in, optional, type=intarr, default="[0,0]"
;                       Size of the bottom and top outer margins, in units of !D.Y_CH_Size.
;                           Acts as a matte around the [XY]_REGION.
;       POSITION:       in, optional, type=intarr(4), default="[1,1,1,1]"
;                       The position at which the plot is located. An array of the form
;                           [x0,y0,x1,y1], where (x0,y0) are the normalized coordinates
;                           of the lower-left corner and (x1,y1) are the coordinates of
;                           the upper-right corner fo the plot.
;       ROW_HEIGHT:     in, optional, type=fltarr
;                       Height of each row, normalized to the width of the plotting region
;                           (area excluding OXMARGIN). The default is to create equal-
;                           height rows.
;       XMARGIN:        in, optional, type=intarr(2), default="[10,3]"
;                       Width of the left and right margins in character units.
;       XGAP:           in, optional, type=integer, default=14
;                       Horizontal space between plots, in character units.
;       YMARGIN:        in, optional, type=intarr(2), default="[4,2]"
;                       Height of the top and bottom margins in character units.
;       YGAP:           in, optional, type=integer, default=6
;                       Vertical space between plots in character units.
;-
function MrLayout::init, $
ASPECT=aspect, $
CHARSIZE=charsize, $
COL_HEIGHT=col_height, $
IXMARGIN=ixmargin, $
IYMARGIN=iymargin, $
LAYOUT=layout, $
MARGIN=margin, $
OXMARGIN=oxmargin, $
OYMARGIN=oymargin, $
POSITION=position, $
ROW_WIDTH=row_width, $
XGAP=xgap, $
YGAP=ygap
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, 0
    endif

    ;Default Margin
    case n_elements(margin) of
        0: ;Do nothing
        1: begin
            oxmargin = [margin, margin]
            oymargin = [margin, margin]
        endcase
        4: begin
            oxmargin = margin[[0,2]]
            oymargin = margin[[1,3]]
        endcase
        else: message, 'MARGIN must be a scalar or 4-element vector.'
    endcase
    
    ;Default pointers
    self.aspect     = ptr_new(/ALLOCATE_HEAP)
    self.col_width  = ptr_new(/ALLOCATE_HEAP)
    self.row_height = ptr_new(/ALLOCATE_HEAP)
    self.xgap       = ptr_new(/ALLOCATE_HEAP)
    self.ygap       = ptr_new(/ALLOCATE_HEAP)
    
    ;Defualt Values
    if n_elements(charsize) eq 0 then charsize = 1.5
    if n_elements(iymargin) eq 0 then iymargin = [0,0]
    if n_elements(ixmargin) eq 0 then ixmargin = [0,0]
    if n_elements(layout)   eq 0 then layout   = [0,0,0]
    if n_elements(oxmargin) eq 0 then oxmargin = [10, 3]
    if n_elements(oymargin) eq 0 then oymargin = [4,2]
    if n_elements(xgap)     eq 0 then xgap     = 14
    if n_elements(ygap)     eq 0 then ygap     = 6

    ;Set Properties
    self.charsize = charsize
    self.ixmargin = ixmargin
    self.iymargin = iymargin
    self.layout   = layout
    self.oxmargin = oxmargin
    self.oymargin = oymargin
    *self.xgap    = xgap
    *self.ygap    = ygap
    if n_elements(aspect)     gt 0 then *self.aspect     = aspect
    if n_elements(col_width)  gt 0 then *self.col_width  = col_width
    if n_elements(row_height) gt 0 then *self.row_height = row_height
    
    ;Default position
    if array_equal(self.layout, [0,0,0]) then begin
        if n_elements(position) eq 0 $
            then self.position = [0.125, 0.125, 0.925, 0.9] $
            else self.position = position
    endif

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
;       ASPECT:         Aspect ratio of the plot.
;       CHARSIZE:       Fraction of IDL's default character size. Used to determine size
;                           of `XMARGIN`, `YMARGIN`, `XGAP` and `YGAP`.
;       COL_WIDTH:      Normalized width of each column.
;       LAYOUT:         [nCols, nRows, index]. [nCols,nRows] is the number of columns and
;                           rows in the display. "index" is the plot index at which to
;                           place the plot, beginning with 1,1], 1 in the upper-left
;                           corner and incrementing first right then down.
;       OXMARGIN:       Size of the left and right outer margins, in units of !D.X_CH_Size.
;                           Acts as a matte around the [XY]_REGION.
;       OYMARGIN:       Size of the bottom and top outer margins, in units of !D.Y_CH_Size.
;                           Acts as a matte around the [XY]_REGION.
;       POSITION:       The position at which the plot is located. An array of the form
;                           [x0,y0,x1,y1], where (x0,y0) are the normalized coordinates
;                           of the lower-left corner and (x1,y1) are the coordinates of
;                           the upper-right corner of the plot.
;       ROW_HEIGHT:     Normalized height of each row.
;       XMARGIN:        Width of the left and right margins in character units.
;       XGAP:           Horizontal space between plots, in character units.
;       X_REGION:       Left and right coordinates of the region containing the plot
;       X_WINDOW:       Left and right coordinates of the window outlined by the axes
;       YMARGIN:        Height of the top and bottom margins in character units.
;       YGAP:           Vertical space between plots in character units.
;       Y_REGION:       Bottom and top coordinates of the region containing the plot
;       Y_WINDOW:       Bottom and top coordinates of the window outlined by the axes
;-
pro MrLayout__define, class
    compile_opt strictarr
    
    define = { MrLayout, $
               aspect:     ptr_new(), $
               charsize:   0.0, $
               col_width:  ptr_new(), $
               ixmargin:   [0.0, 0.0], $
               iymargin:   [0.0, 0.0], $
               layout:     intarr(3), $
               oxmargin:   [0.0,0.0], $
               oymargin:   [0.0,0.0], $
               position:   fltarr(4), $
               row_height: ptr_new(), $
               xgap:       ptr_new(), $
               x_region:   [0.0, 0.0], $
               x_window:   [0.0, 0.0], $
               ygap:       ptr_new(), $
               y_region:   [0.0, 0.0], $
               y_window:   [0.0, 0.0] $
             }
end