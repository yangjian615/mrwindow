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
;-
;*****************************************************************************************
;+
;   Calculate a position based on the layout properties.
;-
function MrLayout::CalcPosition
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
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
    position = MrPlotLayout(self.layout, $
                            ASPECT   = *self.aspect, $
                            CHARSIZE =  self.charsize, $
                            OXMARGIN =  self.oxmargin, $
                            OYMARGIN =  self.oymargin, $
                            P_REGION =       p_region, $
                            P_WINDOW =       p_window, $
                            XGAP     =  self.xgap, $
                            XMARGIN  =  self.xmargin, $
                            YGAP     =  self.ygap, $
                            YMARGIN  =  self.ymargin)
    
    ;Save the window and region
    self.x_region = p_region[[0,2]]
    self.x_window = P_window[[0,2]]
    self.y_region = p_region[[1,3]]
    self.y_window = p_window[[1,3]]

    return, position    
end


;+
;   Set properties of the object.
;
; :Params:
;       ASPECT:         out, optional, type=float
;                       Aspect ratio of the plot.
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
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Set GraphicAtom properties
    if arg_present(aspect)   then aspect   = *self.aspect
    if arg_present(charsize) then charsize =  self.charsize
    if arg_present(layout)   then layout   =  self.layout
    if arg_present(oxmargin) then oxmargin =  self.oxmargin
    if arg_present(oymargin) then oymargin =  self.oymargin
    if arg_present(position) then position =  self.position
    if arg_present(xmargin)  then xmargin  =  self.xmargin
    if arg_present(xgap)     then xgap     =  self.xgap
    if arg_present(x_region) then x_region =  self.x_region
    if arg_present(x_window) then x_window =  self.x_window
    if arg_present(ymargin)  then ymargin  =  self.ymargin
    if arg_present(ygap)     then ygap     =  self.ygap
    if arg_present(y_region) then y_region =  self.y_region
    if arg_present(y_window) then y_window =  self.y_window
    if arg_present(margin)   then margin   = [self.xmargin[0], self.ymargin[0], $
                                              self.xmargin[1], self.ymargin[1]]
end


;+
;   Set properties of the object.
;
; :Params:
;       ASPECT:         in, optional, type=float
;                       Aspect ratio of the plot.
;       CHARSIZE:       in, optional, type=float
;                       Fraction of IDL's default character size. Used to determine size
;                           of `XMARGIN`, `YMARGIN`, `XGAP` and `YGAP`.
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
LAYOUT=layout, $
MARGIN=margin, $
OXMARGIN=oxmargin, $
OYMARGIN=oymargin, $
POSITION=position, $
UPDATE_LAYOUT=update_layout, $
XMARGIN=xmargin, $
XGAP=xgap, $
YMARGIN=ymargin, $
YGAP=ygap
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
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
            xmargin = [margin, margin]
            ymargin = [margin, margin]
        endcase
        4: begin
            xmargin = margin[[0,2]]
            ymargin = margin[[1,3]]
        endcase
        else: message, 'MARGIN must be a scalar or 4-element vector.'
    endcase
    
    ;Set Properties. Make sure layout elements are always >= 0.
    if n_elements(aspect)   ne 0 then *self.aspect   = aspect
    if n_elements(charsize) gt 0 then  self.charsize = charsize
    if n_elements(layout)   ne 0 then  self.layout   = layout
    if n_elements(oxmargin) gt 0 then  self.oxmargin = oxmargin
    if n_elements(oymargin) gt 0 then  self.oymargin = oymargin
    if n_elements(xmargin)  ne 0 then  self.xmargin  = xmargin
    if n_elements(xgap)     ne 0 then  self.xgap     = xgap
    if n_elements(ymargin)  ne 0 then  self.ymargin  = ymargin
    if n_elements(ygap)     ne 0 then  self.ygap     = ygap
    
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
end


;+
;   The initialization method.
;
; :Params:
;       ASPECT:         in, optional, type=float
;                       Aspect ratio of the plot.
;       CHARSIZE:       in, optional, type=float, default=1.5
;                       Fraction of IDL's default character size. Used to determine size
;                           of `XMARGIN`, `YMARGIN`, `XGAP` and `YGAP`.
;       LAYOUT:         in, optional, type=intarr(3)/intarr(4), default=[1,1,1,1]
;                       [nCols, nRows, index]. [nCols,nRows] is the number of columns and
;                           rows in the display. "index" is the plot index at which to
;                           place the plot, beginning with 1,1], 1 in the upper-left
;                           corner and incrementing first right then down.
;       MARGIN:         in, optional, type=int/intarr(4), default=[10,4,3,2]
;                       Size of the [left, bottom, right, top] margins, in character
;                           units. If a scalar is provided, all margins will be equal.
;                           This keyword takes precedence over `XMARGIN` and `YMARGIN`.
;       OXMARGIN:       in, optional, type=intarr, default=[0,0]
;                       Size of the left and right outer margins, in units of !D.X_CH_Size.
;                           Acts as a matte around the [XY]_REGION.
;       OYMARGIN:       in, optional, type=intarr, default=[0,0]
;                       Size of the bottom and top outer margins, in units of !D.Y_CH_Size.
;                           Acts as a matte around the [XY]_REGION.
;       POSITION:       in, optional, type=intarr(4), default=[1,1,1,1]
;                       The position at which the plot is located. An array of the form
;                           [x0,y0,x1,y1], where (x0,y0) are the normalized coordinates
;                           of the lower-left corner and (x1,y1) are the coordinates of
;                           the upper-right corner fo the plot.
;       XMARGIN:        in, optional, type=intarr(2), default=[10,3]
;                       Width of the left and right margins in character units.
;       XGAP:           in, optional, type=integer, default=14
;                       Horizontal space between plots, in character units.
;       YMARGIN:        in, optional, type=intarr(2), default=[4,2]
;                       Height of the top and bottom margins in character units.
;       YGAP:           in, optional, type=integer, default=6
;                       Vertical space between plots in character units.
;-
function MrLayout::init, $
ASPECT=aspect, $
CHARSIZE=charsize, $
LAYOUT=layout, $
MARGIN=margin, $
OXMARGIN=oxmargin, $
OYMARGIN=oymargin, $
POSITION=position, $
XMARGIN=xmargin, $
XGAP=xgap, $
YMARGIN=ymargin, $
YGAP=ygap
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif

    ;Default Margin
    case n_elements(margin) of
        0: ;Do nothing
        1: begin
            xmargin = [margin, margin]
            ymargin = [margin, margin]
        endcase
        4: begin
            xmargin = margin[[0,2]]
            ymargin = margin[[1,3]]
        endcase
        else: message, 'MARGIN must be a scalar or 4-element vector.'
    endcase
    
    ;Defualt Values
    if n_elements(charsize) gt 0 then self.charsize = charsize else self.charsize = 1.5
    if n_elements(oxmargin) gt 0 then self.oxmargin = oxmargin else self.oxmargin = [0, 0]
    if n_elements(oymargin) gt 0 then self.oymargin = oymargin else self.oymargin = [0, 0]
    if n_elements(xmargin)  gt 0 then self.xmargin  = xmargin  else self.xmargin  = [10, 3]
    if n_elements(xgap)     gt 0 then self.xgap     = xgap     else self.xgap     = 14
    if n_elements(ymargin)  gt 0 then self.ymargin  = ymargin  else self.ymargin  = [4,2]
    if n_elements(ygap)     gt 0 then self.ygap     = ygap     else self.ygap     = 6
    
    ;Default pointers
    self.aspect = ptr_new(/ALLOCATE_HEAP)
    if n_elements(layout) gt 0 then  self.layout = layout else self.layout = [0,0,0]
    if n_elements(aspect) gt 0 then *self.aspect = aspect
    
    ;Default position
    if array_equal(self.layout, [0,0,0]) then begin
        if n_elements(position) eq 0 then begin
            position = [0.125, 0.125, 0.925, 0.9]
            update_layout = 0
        endif
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
               aspect: ptr_new(), $
               charsize: 0.0, $
               layout: intarr(3), $
               oxmargin: [0,0], $
               oymargin: [0,0], $
               position: fltarr(4), $
               xmargin: [0, 0], $
               xgap: 0, $
               x_region: [0.0, 0.0], $
               x_window: [0.0, 0.0], $
               ymargin: [0, 0], $
               ygap: 0, $
               y_region: [0.0, 0.0], $
               y_window: [0.0, 0.0] $
             }
end