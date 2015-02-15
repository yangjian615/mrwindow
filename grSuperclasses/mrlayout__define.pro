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
;       2014/02/01  -   Added the ConvertIndex, Exists, and GetPosition methods. Renamed
;                           ::CalcPositions to ::SetGrid. Added the GRID property. - MRA
;       2014/02/02  -   Changed INDEX parameters to PINDEX to better convey that a plot
;                           index is required. Simplified ::ConvertIndex. The Init method
;                           now calls ::SetProperty. Checking the layout is more robust. - MRA
;       0214/02/04  -   Added the ShiftColumn, ShiftRow, and UpdateIndex methods. Added
;                           the LOCATION keyword. - MRA
;       2015/01/22  -   Added the AddRow, AddColumn, DeleteRow, and DeleteColumn methods.
;                           Added the EXPAND keyword to ShiftRow and ShiftColumn methods. - MRA
;       2015/01/23  -   Added the FEED keyword to ShiftRow and ShiftColumn. - MRA
;-
;*****************************************************************************************
;+
;   Determines if the layout object is being managed.
;
; :Returns:
;       TF_MANAGED:     Returns true (1) if the layout is being managed and false (0) otherwise.
;-
function MrLayout::IsManaged
    return, self.isManaged
end


;+
;   Set properties of the object.
;
; :Keywords:
;       MANAGED:        in, optional, type=boolean
;                       If set, the layout will be managed by the layout manager.
;-
pro MrLayout::GetProperty, $
MANAGED=managed, $
_REF_EXTRA=extra
    compile_opt strictarr
    on_error, 2
    
    ;Get Properties
    if arg_present(managed) then managed = self.managed
    if n_elements(extra) gt 0 then self -> MrLayoutAtom::GetProperty, _STRICT_EXTRA=extra
end


;+
;   Turn layout management on or off.
;
; :Keywords:
;       DISABLE:        in, optional, type=boolean
;                       If set, layout management will be disabled. The default is to
;                           enable layout managment.
;-
pro MrLayout::Manage, $
DISABLE=disable
    compile_opt strictarr
    on_error, 2
    
    ;Turn layout management on or off.
    self.manage = keyword_set(disable)
end


;+
;   Set properties of the object.
;
; :Keywords:
;       MANAGE:         in, optional, type=boolean
;                       If set, the layout will be managed by the layout manager.
;-
pro MrLayout::SetProperty, $
MANAGE=manage, $
_REF_EXTRA=extra
    compile_opt strictarr
    on_error, 2
    
    ;Set properties
    if n_elements(manage) gt 0 then self.isManaged = keyword_set(manage)
    if n_elements(extra)  gt 0 then self -> MrLayoutAtom::SetProperty, _STRICT_EXTRA=extra
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
;       LAYOUT:         in, optional, type=intarr(2)/intarr(3), default="[0,0,0]"
;                       An array of the form [nCols, nRows] or [nCols, nRows, index].
;                           [nCols,nRows] is the number of columns and rows in the display.
;                           "index" is the plot index at which to place the plot,
;                           beginning with 1,1], 1 in the upper-left corner and
;                           incrementing first right then down. If 2-elements, the current
;                           index will be kept.
;       LOCATION:       in, optional, type=int/intarr(2)
;                       Another way of specifying the index in `LAYOUT`. Can also be the
;                           [column, row] in which the graphic is to be placed.
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
COL_WIDTH=col_width, $
CHARSIZE=charsize, $
IXMARGIN=ixmargin, $
IYMARGIN=iymargin, $
LAYOUT=layout, $
LOCATION=location, $
MANAGE=manage, $
MARGIN=margin, $
OXMARGIN=oxmargin, $
OYMARGIN=oymargin, $
POSITION=position, $
ROW_HEIGHT=row_height, $
SQUARE=square, $
WDIMS=wDims, $
XGAP=xgap, $
YGAP=ygap
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif

    ;Should the layout be managed?
    if n_elements(position) eq 0 && n_elements(layout) lt 3 then managed = 1B
    
    ;Set Properties
    self -> SetProperty, ASPECT     = aspect, $
                         COL_WIDTH  = col_width, $
                         CHARSIZE   = charsize, $
                         IXMARGIN   = ixmargin, $
                         IYMARGIN   = iymargin, $
                         LAYOUT     = layout, $
                         LOCATION   = location, $
                         MANAGE     = manage, $
                         MARGIN     = margin, $
                         OXMARGIN   = oxmargin, $
                         OYMARGIN   = oymargin, $
                         POSITION   = position, $
                         ROW_HEIGHT = row_height, $
                         WDIMS      = wDims, $
                         XGAP       = xgap, $
                         YGAP       = ygap
    
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
;       ISMANAGED:      Is the layout object being magaged by the layout manager?
;-
pro MrLayout__define, class
    compile_opt strictarr
    
    define = { MrLayout, $
               inherits MrLayoutAtom, $
               isManaged: 0B $
             }
end