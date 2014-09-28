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
;                            the LOCATION keyword. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to provide output when the object is an argument to
;   the PRINT procedure.
;
; :Private:
;-
function MrLayout::_OverloadPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, fltarr(4)
    endif
    
    ;Useful strings
    undefined = '<undefined>'
    
    ;Create the string
    charsize = string('CharSize', '=', self.charsize, FORMAT='(a-26, a-2, f0)')
    layout   = string('Layout',   '=', self.layout,   FORMAT='(a-26, a-2, 3(i0, 2x))')
    oxmargin = string('OXMargin', '=', self.oxmargin, FORMAT='(a-26, a-2, 2(f0, 2x))')
    oymargin = string('OYMargin', '=', self.oymargin, FORMAT='(a-26, a-2, 2(f0, 2x))')
    position = string('Position', '=', self.position, FORMAT='(a-26, a-2, 4(f0, 2x))')
    xmargin  = string('XMargin',  '=', self.xmargin,  FORMAT='(a-26, a-2, 2(f0, 2x))')
    xgap     = string('XGap',     '=', self.xgap,     FORMAT='(a-26, a-2, f0)')
    x_region = string('X_Region', '=', self.x_region, FORMAT='(a-26, a-2, 4(f0, 2x))')
    x_window = string('X_Window', '=', self.x_window, FORMAT='(a-26, a-2, 4(f0, 2x))')
    ymargin  = string('YMargin',  '=', self.ymargin,  FORMAT='(a-26, a-2, 2(f0, 2x))')
    ygap     = string('YGap',     '=', self.ygap,     FORMAT='(a-26, a-2, f0)')
    y_region = string('Y_Region', '=', self.y_region, FORMAT='(a-26, a-2, 4(f0, 2x))')
    y_window = string('Y_Window', '=', self.y_window, FORMAT='(a-26, a-2, 4(f0, 2x))')
    
    ;Pointers may or may not have a value
    aspect = string('Aspect', '=', FORMAT='(a-26, a-2)')
    if n_elements(*self.aspect) eq 0 then aspect += undefined else aspect += string(*self.aspect, FORMAT='(f0)')
    
    ;Combine all of the string into an array
    result = [ aspect, $
               charsize, $
               layout, $
               oxmargin, $
               oymargin, $
               position, $
               xmargin, $
               xgap, $
               x_region, $
               x_window, $
               ymargin, $
               ygap, $
               y_region, $
               y_window $
             ]
    
    ;Return a column vector so that everything is printed on its own line.
    return, transpose(result)
end


;+
;   The purpose of this method is to convert from a [col, row] plot location to a
;   plot-index location, and vice versa.
;
; :Examples:
;       ;Convert from a plot index to a [col,row] location::
;           colrow = theObj -> ConvertIndex(pIndex)
;           colrow = theObj -> ConvertIndex(pIndex, /PINDEX, /TO_COLROW)
;
;       ;Convert from a [col,row] location to a pIndex::
;           pIndex = theObj -> ConvertIndex(colrow, /COLROW)
;           pIndex = theObj -> ConvertIndex(colrow, /COLROW, /TO_PINDEX)
;
; :Params:
;       LOCATION:       in, required, type=int/intarr
;                       The plot index, starting with 1, plot to be converted.
;       LAYOUT:         in, optional, type=intarr(2), default=current layout
;                       The number of columns and rows in the layout: [ncols, nrows].
;
; :Keywords:
;       EXISTS:         out, optional, type=boolean
;                       Returns 1 if `LOCATION` exists within `LAYOUT` and 0 otherwise.
;                           If EXISTS is 0 and no named variable is present, an error 
;                           will occur.
;       COLROW:         in, optional, type=boolean, default=0
;                       Indicate that `LOCATION` is a [col,row] location.
;       AINDEX:         in, optional, private, type=boolean, default=0
;                       Indicate that `LOCATION` is actually an array-index.
;       PINDEX:         in, optional, type=boolean
;                       Indicate that `LOCATION` is a plot-index. This is assumed.
;       TO_COLROW:      in, optional, type=boolean
;                       Indicate that `LOCATION` is to be converted to a [col, row] location.
;                           If none of the TO_* keywords are set, the default is
;                           TO_COLROW = `PINDEX` EQ 1.
;       TO_AINDEX:      in, optional, private, type=boolean, default=0
;                       Indicate that `LOCATION` is to be converted to an array-index.
;       TO_PINDEX:      in, optional, type=boolean, default=0
;                       Indicate that `LOCATION` is to be converted to a plot-index.
;                           If none of the TO_* keywords are set, the default is
;                           TO_PINDEX = `COLROW` EQ 1.
;
; :Returns:
;       RESULT:         The result of the convertion.
;-
function MrLayout::ConvertIndex, location, layout, $
EXISTS=exists, $
COLROW=colrow, $
AINDEX=aIndex, $
PINDEX=pIndex, $
TO_AINDEX=to_aIndex, $
TO_PINDEX=to_pIndex, $
TO_COLROW=to_colrow
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, -1
    endif

;---------------------------------------------------------------------
;Defaults ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Assume existence
    exists = 1

    ;Default to a [col, row] location.
    if n_elements(layout) eq 0 then layout = self.GrLayout
    aIndex = keyword_set(aIndex)
    pIndex = keyword_set(pIndex)
    colrow = keyword_set(colrow)
    to_aIndex = keyword_set(to_aIndex)
    to_pIndex = keyword_set(to_pIndex)
    to_colrow = keyword_set(to_colrow)

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Put restrictions.
    if aIndex + pIndex + colrow eq 0 then pIndex = 1
    if aIndex + pIndex + colrow ne 1 then $
        message, 'AINDEX, PINDEX, and COLROW are mutually exclusive.'
    
    ;Default to converting to a [col,row] location.
    if to_pIndex + to_aIndex + to_colrow eq 0 then begin
        to_ColRow = pIndex
        to_pIndex = colrow
    endif
    if to_pIndex + to_aIndex + to_colrow ne 1 then $
        message, 'One and only one of TO_PINDEX, TO_AINDEX, and TO_COLROW are allowed.'
    
    ;Make sure the location exists
    exists = self -> Exists(location, layout, COLROW=colrow, AINDEX=aIndex)
    if max(exists) eq 0 then if arg_present(exists) then begin
        exists = 0
        return, -1
    endif else message, 'Location does not exist within layout. Cannot convert.'

;---------------------------------------------------------------------
;Convert Layout Location /////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;PLOT INDEX
    if pIndex then begin
        case 1 of
            to_aIndex: result = index - 1
            to_ColRow: result = array_indices(layout, index-1, /DIMENSIONS) + 1
            to_pIndex: result = index
        endcase
    
    ;ARRAY INDEX
    endif else if aIndex then begin
        case 1 of
            to_aIndex: result = index
            to_ColRow: result = array_indices(layout, index, /DIMENSIONS) + 1
            to_pIndex: result = index + 1
        endcase
        
    ;[COL, ROW]
    endif else if colrow then begin
        case 1 of
            to_aIndex: result = layout[0]*(index[1,*]-1) + index[0,*] - 1
            to_ColRow: result = index
            to_pIndex: result = layout[0]*(index[1,*]-1) + index[0,*]
        endcase
    endif
    
    return, result
end


;+
;   The purpose of this method is to convert from a plot-index location to a [col,row]
;   location and vice versa.
;
; :Params:
;       PINDEX:         in, required, type=int/intarr
;                       The index of the plot, starting with 1, whose [col,row] location
;                           is to be determined.
;       LAYOUT:         in, optional, type=intarr(2), default=current layout
;                       The number of columns and rows in the layout: [ncols, nrows].
;
; :Keywords:
;       AINDEX:         in, optional, private, type=boolean, default=0
;                       If set, `PINDEX` is take to be array-index value.
;       COLROW:         in, optional, type=boolean, default=0
;                       If set, `INDEX` is take to be a [col,row] location.
;
; :Returns:
;       TF_EXIST:       Returns 1 for each `PINDEX` that exists within `LAYOUT` and 0 for
;                           those that do not.
;-
function MrLayout::Exists, pIndex, layout, $
AINDEX=aIndex, $
COLROW=ColRow
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif

    ;Defaults
    if n_elements(layout) eq 0 then layout = self.layout[0:1]
    ColRow = keyword_set(ColRow)
    aIndex = keyword_set(aIndex)
    if ColRow + aIndex gt 1 then message, 'COLROW and AINDEX are mutually exclusive.'
    
    ;Check if the location exists within the layout
    case 1 of
        aIndex: tf_exist = (pIndex le layout[0]*layout[1] - 1) && (pIndex ge 0)
        ColRow: tf_exist = (pIndex[0,*] lt layout[0]) && (pIndex[1,*] lt layout[1])
        else:   tf_exist = pIndex lt layout[0]*layout[1]
    endcase 
    
    return, tf_exist
end


;+
;   The purpose of this method is to shift a given amount of rows.
;
; :Params:
;       NCOLS:          in, optional, type=int, default=1
;                       Number of rows to shift the graphic. If NCOLS is positive
;                           (negative), the graphic will be shifted right (left).
;
; :Keywords:
;       WRAP:           in, optional, type=boolean, default=1
;                       If set, the graphic will be rapped around from bottom to top
;                           if `NROWS` pushes the graphic out of the layout. If set equal
;                           to zero, the same situation will cause an error.
;-
pro MrLayout::ShiftColumn, nCols, $
WRAP=wrap
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;If no layout position is present, return
    if self.layout[3] eq 0 then return
    
    ;Defaults
    if n_elements(wrap)  eq 0 then wrap  = 1 else wrap = keyword_set(wrap)
    if n_elements(nCols) eq 0 then nCols = 1
    
    ;Convert the plot index to a [col,row] location
    colRow = self -> ConvertIndex(self.layout[3], /PINDEX, /TO_COLROW)
    
    ;Calculate the new row
    newCol = colRow[0] + nCols
    
    ;Make sure the graphic is still in the layout
    if newCol gt self.layout[0] || newCol le 0 then begin
        if wrap eq 0 then message, 'NCOLS is too large. Cannot shift.'
        
        ;Row 0 wraps around to the bottom row
        if newCol eq 0 then begin
            newCol = self.layout[0]
            
        ;Other rows wrap to the remainder of the quotient.
        endif else begin
            newCol = abs(newCol mod self.layout[0])
            if newCol eq 0 then newCol = self.layout[0]
        endelse
    endif
        
    ;Convert back to a plot index
    pIndex = self -> ConvertIndex([newCol, colRow[1]], /COLROW, /PINDEX)
    
    ;Update the layout and position
    self -> SetProperty, LAYOUT=pIndex
end


;+
;   The purpose of this method is to shift up a given amount of rows.
;
; :Params:
;       NROWS:          in, optional, type=int, default=1
;                       Number of rows to shift the plot up. If NROWS is positive
;                           (negative), the graphic will be shifted down (up).
;
; :Keywords:
;       WRAP:           in, optional, type=boolean, default=1
;                       If set, the graphic will be rapped around from bottom to top
;                           if `NROWS` pushes the graphic out of the layout. If set equal
;                           to zero, the same situation will cause an error.
;-
pro MrLayout::ShiftRow, nRows, $
WRAP=wrap
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;If no layout position is present, return
    if self.layout[3] eq 0 then return
    
    ;Defaults
    if n_elements(wrap)  eq 0 then wrap  = 1 else wrap = keyword_set(wrap)
    if n_elements(nRows) eq 0 then nRows = 1
    
    ;Convert the plot index to a [col,row] location
    colRow = self -> ConvertIndex(self.layout[3], /PINDEX, /TO_COLROW)
    
    ;Calculate the new row
    newRow = colRow[1] + nRows
    
    ;Make sure the graphic is still in the layout
    if newRow gt self.layout[1] || newRow le 0 then begin
        if wrap eq 0 then message, 'NROWS is too large. Cannot shift.'
        
        ;Row 0 wraps around to the bottom row
        if newRow eq 0 then begin
            newRow = self.layout[1]
            
        ;Other rows wrap to the remainder of the quotient.
        endif else begin
            newRow = abs(newRow mod self.layout[1])
            if newRow eq 0 then newRow = self.layout[1]
        endelse
    endif
        
    ;Convert back to a plot index
    pIndex = self -> ConvertIndex([colrow[0], newRow], /COLROW, /PINDEX)
    
    ;Update the layout and position
    self -> SetProperty, LAYOUT=pIndex
end


;+
;   Calculate a position based on the layout properties.
;
; :Private:
;-
pro MrLayout::SetGrid
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    nLay = n_elements(layout)
    
    ;If the [nCols,nRows] = [0,0] then return -- No layout has been defined.
    if (self.layout[0] eq 0) and (self.layout[1] eq 0) then return
    
    ;Make sure the layout is valid.
    if (self.layout[0] lt 0) or (self.layout[1] lt 0) then begin
        message, 'Layout must have at least one column and one row.', /INFORMATIONAL
        return
    endif

    ;Calculate positions
    grid = MrLayout(self.layout[0:1], $
                    ASPECT     = *self.aspect, $
                    CHARSIZE   =  self.charsize, $
                    COL_WIDTH  = *self.col_width, $
                    IXMARGIN   =  self.ixmargin, $
                    IYMARGIN   =  self.iymargin, $
                    OXMARGIN   =  self.oxmargin, $
                    OYMARGIN   =  self.oymargin, $
                    P_REGION   =       p_region, $
                    ROW_HEIGHT = *self.row_height, $
                    WDIMS      =  self.wdims, $
                    XGAP       =  self.xgap, $
                    XMARGIN    =  self.xmargin, $
                    YGAP       =  self.ygap, $
                    YMARGIN    =  self.ymargin)
    
    ;Save the window and region
    self.x_region = p_region[[0,2]]
    self.y_region = p_region[[1,3]]

    ;Save the grid
    *self.grid = grid
end


;+
;   Get a position from the layout grid.
;
; :Params:
;       PINDEX:         in, required, type=int/intarr, default=current index
;                       The index of the plot whose position is to be determined. It must
;                           exist within the current layout. If PINDEX is a scalar equal
;                           to zero (0), the current position is returned.
;
; :Keywords:
;       COLROW:         in, optional, type=boolean, default=0
;                       If set, `INDEX` is take to be a [col,row] location. 2xN arrays
;                           are accepted.
;
; :Returns:
;       POSITION:       Position within the layout corresponding to `PINDEX`. If `PINDEX`
;                           is a scalar, the output is a 4-element array. Otherise a 4xN
;                           array is returned, where N is the number of locations given.
;-
function MrLayout::GetPosition, pIndex, $
COLROW=colrow
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, self.position
    endif

    ;Default
    colrow = keyword_set(colrow)

    ;Get the current position
    if n_elements(pIndex) eq 0 then begin
        position = self.position
    
    ;If PINDEX is zero, get the current position.
    endif else if MrIsA(pIndex, /SCALAR) && pIndex eq 0 then begin
        position = self.position
    
    ;Get the desired position
    endif else begin
        ;Check if the index exists
        tf_exist = self -> Exists(pIndex, COLROW=colrow)
        if min(tf_exist) eq 0 then $
            message, 'PINDEX does not exist within the current layout.'
        
        ;Convert to an array index
        if colrow eq 0 $
            then aIndex = self -> ConvertIndex(pIndex, /PINDEX, /TO_AINDEX) $
            else aIndex = self -> ConvertIndex(pIndex, /COLROW, /TO_AINDEX)
        
        ;Get the position
        position = (*self.grid)[*,aIndex]
    endelse

    return, position
end


;+
;   Set properties of the object.
;
; :Keywords:
;       ASPECT:         out, optional, type=float
;                       Aspect ratio of the plot.
;       LAYOUT:         out, optional, type=intarr(3)
;                       [nCols, nRows, index]. [nCols,nRows] is the number of columns and
;                           rows in the display. "index" is the plot index at which to
;                           place the plot, beginning with 1,1], 1 in the upper-left
;                           corner and incrementing first right then down.
;       LOCATION:       in, optional, type=int/intarr(2)
;                       The [col,row] location where the graphic is located. If you want
;                           the index corresponding to the grid position, use `LAYOUT`.
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
COL_WIDTH=col_width, $
CHARSIZE=charsize, $
IXMARGIN=ixmargin, $
IYMARGIN=iymargin, $
LAYOUT=layout, $
LOCATION=location, $
MARGIN=margin, $
OXMARGIN=oxmargin, $
OYMARGIN=oymargin, $
POSITION=position, $
ROW_HEIGHT=row_height, $
WDIMS=wDims, $
XGAP=xgap, $
YGAP=ygap
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Get Properties
    if arg_present(aspect)     then aspect     = *self.aspect
    if arg_present(charsize)   then charsize   =  self.charsize
    if arg_present(col_width)  then col_width  = *self.col_width
    if arg_present(ixmargin)   then ixmargin   =  self.ixmargin
    if arg_present(iymargin)   then iymargin   =  self.iymargin
    if arg_present(layout)     then layout     =  self.layout
    if arg_present(oxmargin)   then oxmargin   =  self.oxmargin
    if arg_present(oymargin)   then oymargin   =  self.oymargin
    if arg_present(position)   then position   =  self.position
    if arg_present(row_height) then row_height = *self.row_height
    if arg_present(wdims)      then wdims      =  self.wdims
    if arg_present(xmargin)    then xmargin    =  self.xmargin
    if arg_present(xgap)       then xgap       =  self.xgap
    if arg_present(x_region)   then x_region   =  self.x_region
    if arg_present(x_window)   then x_window   =  self.x_window
    if arg_present(ymargin)    then ymargin    =  self.ymargin
    if arg_present(ygap)       then ygap       =  self.ygap
    if arg_present(y_region)   then y_region   =  self.y_region
    if arg_present(y_window)   then y_window   =  self.y_window
    if arg_present(margin)     then margin     = [self.xmargin[0], self.ymargin[0], $
                                                  self.xmargin[1], self.ymargin[1]]
    
    if arg_present(location) then begin
        location = self -> ConvertIndex(self.pindex[2], /PINDEX, /TO_COLROW, EXISTS=exists)
        if exists eq 0 then location = [0,0]
    endif
end


;+
;   The purpose of this method is to convert plot index values from one layout to another.
;   Only [col, row] locations are invariant to changes in layout.
;
; :Keywords:
;       PINDEX:         in, required, type=int/intarr
;                       Plot index values that are to be converted from their locations
;                           in `OLD_LAYOUT` to their new locations in `LAYOUT`.
;       LAYOUT:         in, required, type=intarr(2)
;                       An array of the form [nCols, nRows] indicating the number of
;                           columns and rows of the new layout in which to locate
;                           `PINDEX`.
;       OLD_LAYOUT:     in, optional, type=intarr(2), default=current layout
;                       The layout in which `PINDEX` currently exists.
;
; :Keywords:
;       AINDEX:         in, optional, type=boolean, default=0
;                       If set, `INDEX` is actually array-index values.
;       SUCCESS:        out, optional, type=boolean
;                       A named variable that will equal 1 (one) if the conversion was
;                           successful and 0 (zero) if not. If no variable is provided,
;                           an error will occur.
;
; :Returns:
;       NEWINDEX:       Index locations of `PINDEX` within the new `LAYOUT`. If `PINDEX`
;                           is the scalar 0, then 0 is returned. In the event `SUCCESS`=0,
;                           an invalid index will be returned (-1).
;-
function MrLayout::UpdateIndex, pIndex, layout, old_layout, $
AINDEX=aindex, $
SUCCESS=success
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, -1
    endif

    ;The null index?
    if MrIsA(pIndex, /SCALAR) && pIndex eq 0 then return, 0

    ;Defaults
    aindex = keyword_set(aindex)
    pindex = ~aindex
    if n_elements(old_layout) eq 0 then old_layout = self.layout[0:1]
    
    ;Convert to [col,row] locations
    ColRow = self -> ConvertIndex(index, old_layout, PINDEX=pindex, AINDEX=aindex, $
                                  /TO_COLROW, EXISTS=succcess)
                                  
    ;Did it work?
    if success eq 0 then if arg_present(success) eq 0 then $
        message, 'PINDEX lies outside of LAYOUT. Cannot update.'
    
    ;Conver to index locations in the new layout
    newPIndex = self -> ConverIndex(ColRow, layout, /COLROW, TO_PINDEX=pindex, TO_AINDEX=aindex)
    
    return, newPIndex
end


pro MrLayout::SetLayout, layout
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    nCols = layout[0] - self.layout[0]
    nRows = layout[1] - self.layout[1]
    
    if nCols gt 0 $
        then self -> DeleteCol, abs(nCols) $
        else self -> AddCol, nCols
        
    if nRows gt 0 $
        then self -> DeleteRow, abs(nRows) $
        else self -> AddRow, nRows
        
    self -> SetGrid
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
;       LAYOUT:         in, optional, type=intarr(2)/intarr(3)
;                       An array of the form [nCols, nRows] or [nCols, nRows, index].
;                           [nCols,nRows] is the number of columns and rows in the display.
;                           "index" is the plot index at which to place the plot,
;                           beginning with 1,1], 1 in the upper-left corner and
;                           incrementing first right then down. If 2-elements, the current
;                           index will be kept. If `LOCATION` is present, this keyword is
;                           ignored.
;       LOCATION:       in, optional, type=int/intarr(2)
;                       Another way of specifying the index in `LAYOUT`. Can also be the
;                           [column, row] in which the graphic is to be placed. If given,
;                           `LAYOUT` is ignored and the current layout is used.
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
COL_WIDTH=col_width, $
CHARSIZE=charsize, $
IXMARGIN=ixmargin, $
IYMARGIN=iymargin, $
LAYOUT=layout, $
LOCATION=location, $
MARGIN=margin, $
OXMARGIN=oxmargin, $
OYMARGIN=oymargin, $
POSITION=position, $
ROW_HEIGHT=row_height, $
WDIMS=wDims, $
XGAP=xgap, $
YGAP=ygap
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
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
    if n_elements(aspect)   gt 0 then *self.aspect   = aspect
    if n_elements(charsize) gt 0 then  self.charsize = charsize
    if n_elements(ixmargin) gt 0 then  self.ixmargin = ixmargin
    if n_elements(iymargin) gt 0 then  self.iymargin = iymargin
    if n_elements(oxmargin) gt 0 then  self.oxmargin = oxmargin
    if n_elements(oymargin) gt 0 then  self.oymargin = oymargin
    if n_elements(wdims)    gt 0 then  self.wdims    = wdims
    if n_elements(xmargin)  gt 0 then  self.xmargin  = xmargin
    if n_elements(xgap)     gt 0 then  self.xgap     = xgap
    if n_elements(ymargin)  gt 0 then  self.ymargin  = ymargin
    if n_elements(ygap)     gt 0 then  self.ygap     = ygap
    
    nColW = n_elements(col_width)
    if nColW gt 0 then if nColW eq begin
        if nColW eq layout[0]-1 $
            then *self.col_width = col_width $
            else message, 'COL_WIDTH: Incorrect number of elements', /INFORMATIONAL
    endif
    
    nRowH = n_elements(row_height)
    if nRowH gt 0 then if nRowH eq begin
        if nRowH eq layout[1]-1 $
            then *self.row_height = row_height $
            else message, 'ROW_HEIGHT: Incorrect number of elements', /INFORMATIONAL
    endif
        
;---------------------------------------------------------------------
;Location ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(location) gt 0 then begin
        oLocation = location
        
        ;Convert to a plot-index and make sure it exists within the layout.
        if n_elements(oLocation) eq 2 $
            then oLocation = self -> ConvertIndex(oLocation, /COLROW, /TO_PINDEX, SUCCESS=success) $
            else success = self -> Exists(oLocation)

        ;Save the location. Make sure LAYOUT is not updated after.
        if success eq 0 $
            then message, 'LOCATION does not fit inside the current layout. Ignoring', /INFORMATIONAL $
            else self.layout[2] = oLocation
    endif
        
;---------------------------------------------------------------------
;Update the Grid /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    self -> SetGrid
        
;---------------------------------------------------------------------
;Position ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;
    ;If a position was given, set the layout index equal to zero so
    ;that when the grid is updated the position will not be.
    ;
    ;If a position was not given, get the position indicated by the
    ;current index in case the grid has changed. If index=0, then
    ;the current position will be retrieved (i.e. the position will
    ;not change).
    ;    
    if n_elements(position) ne 0 then begin
        self.layout = [self.layout[0:1], 0]
        self.position = position
    endif else self.position = GetPosition(self.layout[3])
end


;+
;   Clean up after the object is destroy
;-
pro MrLayout::cleanup
    ptr_free, self.aspect
    ptr_free, self.grid
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
    
    ;We must have the dimensions of the window
    if n_elements(wDims) eq 0 then begin
        if (!d.window eq -1) then begin
            if ((!d.flags and 256) gt 0) then begin
                iWin = MrGetWindow(/FREE, /PIXMAP)
                wDims = [!d.x_vsize, !d.y_vsize]
                wDelete, iWin
            endif else begin
                message, 'Windows not supported and window not available. Provide WDIMS.'
            endelse
        endif else begin
            wDims = [!d.x_vsize, !d.y_vsize]
        endelse
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
    
    ;Layout
    case n_elements(layout) of
        0: layout = [0,0,0]
        2: layout = [layout, 0]
        else: message, 'LAYOUT must be a vector of the form [nCols, nRows] or [nCols, nRows, index].'
    endcase
    
    ;Location
    case n_elements(location) of
        0: ;Do nothing
        1: layout[2] = location
        2: layout[2] = self -> ConvertIndex(location, layout[0:1], /COLROW, /TO_PINDEX)
        else: message, 'LOCATION must be a scalar grid index or a [col,row] location.'
    endcase
    
    ;Defualt Values
    if keyword_set(square)    eq 1 then aspect     = 1.0
    if n_elements(charsize)   eq 0 then charsize   = 1.5
    if n_elements(col_width)  eq 0 then col_width  = replicate(wDims[0] / layout[0], layout[0])
    if n_elements(layout)     eq 0 then layout     = [0,0,0]
    if n_elements(oxmargin)   eq 0 then oxmargin   = [0, 0]
    if n_elements(oymargin)   eq 0 then oymargin   = [0, 0]
    if n_elements(position)   eq 0 then position   = fltarr(4)
    if n_elements(row_height) eq 0 then row_height = replicate(wDims[1] / layout[1], layout[1])
    if n_elements(xmargin)    eq 0 then xmargin    = [10, 3]
    if n_elements(xgap)       eq 0 then xgap       = 14
    if n_elements(ymargin)    eq 0 then ymargin    = [4,2]
    if n_elements(ygap)       eq 0 then ygap       = 6
        
    
    ;Allocate Heap
    self.aspect     = ptr_new(/ALLOCATE_HEAP)
    self.col_height = ptr_new(/ALLOCATE_HEAP)
    self.row_width  = ptr_new(/ALLOCATE_HEAP)
    
    ;Set Properties
    self -> SetProperty, ASPECT=aspect, $
                         COL_WIDTH=col_width, $
                         CHARSIZE=charsize, $
                         IXMARGIN=ixmargin, $
                         IYMARGIN=iymargin, $
                         LAYOUT=layout, $
                         LOCATION=location, $
                         MARGIN=margin, $
                         OXMARGIN=oxmargin, $
                         OYMARGIN=oymargin, $
                         POSITION=position, $
                         ROW_HEIGHT=row_height, $
                         SQUARE=square, $
                         WDIMS=wDims, $
                         XGAP=xgap, $
                         YGAP=ygap
    
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
;       GRID:           All normalized positions contained in LAYOUT.
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
               aspect:    ptr_new(), $
               charsize:  0.0, $
               col_width: ptr_new(), $
               grid:      ptr_new(), $
               layout:    intarr(3), $
               ixmargin:  [0.0,0.0], $
               iymargin:  [0.0,0.0], $
               oxmargin:  [0.0,0.0], $
               oymargin:  [0.0,0.0], $
               position:  fltarr(4), $
               row_width: ptr_new(), $
               wDims:     lonarr(2), $
               xmargin:   [0.0, 0.0], $
               xgap:      0.0, $
               x_region:  [0.0, 0.0], $
               x_window:  [0.0, 0.0], $
               ymargin:   [0.0, 0.0], $
               ygap:      0.0, $
               y_region:  [0.0, 0.0], $
               y_window:  [0.0, 0.0] $
             }
end