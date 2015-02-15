; docformat = 'rst'
;
; NAME:
;       MrLayoutAtom__Define
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
;   The purpose of this method is to provide output when the object is an argument to
;   the PRINT procedure.
;
; :Private:
;-
function MrLayoutAtom::_OverloadPrint
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
    result = [ [aspect], $
               [charsize], $
               [layout], $
               [oxmargin], $
               [oymargin], $
               [position], $
               [xmargin], $
               [xgap], $
               [x_region], $
               [x_window], $
               [ymargin], $
               [ygap], $
               [y_region], $
               [y_window] $
             ]
    
    ;Return a column vector so that everything is printed on its own line.
    return, result
end


;+
;   Add columns to the layout while keeping the plot in the same location.
;
; :Params:
;       NCOLS:          in, optional, type=int, default=1
;                       Number of columns to add. A negative value will remove columns.
;       COL:            in, optional, type=int, default=last column
;                       The column before/after which more columns are to be added.
;
; :Keywords:
;       BEFORE:         in, optional, type=boolean, default=0
;                       If set, `NCOLS` will be added to the left (before) of `COL`. The
;                           default is to add to the right (after).
;-
pro MrLayoutAtom::AddColumn, nCols, col, $
BEFORE=before
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Default to adding a single column to the right of the layout
    before = keyword_set(before)
    if n_elements(col)   eq 0 then col   = self.layout[0]
    if n_elements(nCols) eq 0 then nCols = 1
    
    ;Adding nothing or removing rows?
    if nCols eq 0 then return
    if nCols lt 0 then begin
        self -> DeleteColumn, nCols, col, BEFORE=before
        return
    endif
    
    ;Determine new layout
    newLayout     = self.layout
    newLayout[0] += nCols
    
    ;Does the graphic have to move?
    colrow = self -> ConvertLocation(self.layout[2], /PINDEX, /TO_COLROW)
    if before then begin
        if col le colrow[0] then colrow[0] += nCols
    endif else begin
        if col lt colrow[0] then colrow[0] += nCols
    endelse
    newPIndex = self -> ConvertLocation(colrow, newLayout, /COLROW, /TO_PINDEX)
    
    ;Update the layout
    self.layout = [newLayout, newPIndex]
    self -> SetGrid
end


;+
;   Add rows to the layout while keeping the plot in the same location.
;
; :Params:
;       NROWS:          in, optional, type=int, default=1
;                       Number of rows to add. A negative value will remove rows.
;       ROW:            in, optional, type=int, default=last row
;                       The row above/below which more rows are to be added.
;
; :Keywords:
;       BEFORE:         in, optional, type=boolean, default=0
;                       If set, `NROWS` will be added above (before) `ROW`. The default
;                           is to add below (after).
;-
pro MrLayoutAtom::AddRow, nRows, row, $
BEFORE=before
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Default to adding a single row to the bottom of the layout
    before = keyword_set(before)
    if n_elements(row)   eq 0 then row = self.layout[1]
    if n_elements(nRows) eq 0 then nRows = 1
    
    ;Adding nothing or removing rows?
    if nRows eq 0 then return
    if nRows lt 0 then begin
        self -> DeleteRow, abs(nRows), row, BEFORE=before
        return
    endif
    
    ;Determine new layout and pIndex
    newLayout     = self.layout
    newLayout[1] += nRows
    
    ;Does the graphic have to move?
    colrow = self -> ConvertLocation(self.layout[2], /PINDEX, /TO_COLROW)
    if before then begin
        if row le colrow[1] then colrow[1] += nRows
    endif else begin
        if row lt colrow[1] then colrow[1] += nRows
    endelse
    newPIndex = self -> ConvertLocation(colrow, newLayout, /COLROW, /TO_PINDEX)
    
    ;Update the layout
    self.layout = [newLayout, newPIndex]
    self -> SetGrid
end


;+
;   Add MrLayout objects to be managed.
;
; :Params:
;       OLAYOUT:        in, required, type=object
;                       A MrLayout object.
;-
pro MrLayoutAtom::ComputeGrid
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    xsize     = !d.x_size
    ysize     = !d.y_size
	xcharsize = ceil(double(!d.x_ch_size)*self.charsize)
	ycharsize = ceil(double(!d.y_ch_size)*self.charsize)

;-----------------------------------------------------------------------------------------
; Calculate Margins and Areas \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------------------------------------------
	;Convert from character units to pixels.
	ixmargin = self.ixmargin * xcharsize
    iymargin = self.iymargin * ycharsize
	oxmargin = self.oxmargin * xcharsize
	oymargin = self.oymargin * ycharsize
	xgap     = self.xgap     * xcharsize
	ygap     = self.ygap     * ycharsize

	;Calculate the area of the region in which plots will be drawn.
	p_region = [oxmargin, oymargin[0], xsize - oxmargin[1], ysize - oymargin[1]]

    ;Calculate the plot dimensions
    plot_width  = (p_region[2] - p_region[0] - total(xspace)) * *self.col_width 
    plot_height = (p_region[3] - p_region[1] - total(yspace)) * *self.row_height

    ;Offset between upper left corner of p_region and lower right corner of
    ;the plot area of each plot.
    xoffset = total(plot_width  + xgap, /CUMULATIVE)
    yoffset = total(plot_height + ygap, /CUMULATIVE)

	;Calculate the areas in which the plots will be created.
	p_areas = fltarr(4, nCols, nRows)
	for ii = 0, nCols-1 do begin
		for jj = 0, nRows-1 do begin
		    p_areas[2,ii,jj] = p_region[0] + xoffset[ii]
		    p_areas[1,ii,jj] = p_region[3] - yoffset[jj]
		    p_areas[0,ii,jj] = p_areas[2,ii,jj] - plot_width[ii]
		    p_areas[3,ii,jj] = p_areas[1,ii,jj] + plot_height[jj]
		endfor
	endfor 

;-----------------------------------------------------------------------------------------
; Calculate Positions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------------------------------------------
	;Subtract the inner margin to create the plot position
	positions        = fltarr(4, nCols, nRows)
	positions[0,*,*] = p_areas[0,*,*] + ixmargin[0]
	positions[2,*,*] = p_areas[2,*,*] - ixmargin[1]
	positions[1,*,*] = p_areas[1,*,*] + iymargin[1]
	positions[3,*,*] = p_areas[3,*,*] - iymargin[1]
	
	;Reform into a 4xnCols*nRows array
	positions = reform(positions, 4, nCols*nRows)
	p_areas   = reform(p_areas,   4, nCols*nRows)

;-----------------------------------------------------------------------------------------
; Set the Aspect Ratio \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------------------------------------------
	if n_elements(aspect) gt 0 then begin	    
	    ;Loop through all of the plots
	    for i = 0, nCols*nRows-1 do begin
	        if aspect[i] eq 0 then continue
	        
	        pWidth  = positions[2,i] - positions[0,i]
	        pHeight = positions[3,i] - positions[1,i]
	        
	        ;Make sure the scaled dimension becomes smaller
	        newPWidth  = pWidth
	        newPHeight = newPWidth * aspect[i]
	        if newPHeight gt pHeight then begin
	            newPHeight = pHeight
	            newPWidth = newPHeight / aspect[i]
	        endif
	        
	        ;Center the new position within its old position
	        positions[0,i] = positions[0,i] + (pWidth  - newPWidth)  / 2
	        positions[1,i] = positions[1,i] + (pHeight - newPHeight) / 2
	        positions[2,i] = positions[0,i] + newPWidth
	        positions[3,i] = positions[1,i] + newPHeight
	    endfor
	endif

;-----------------------------------------------------------------------------------------
; Normal or Device Coordinates? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------------------------------------------
	if keyword_set(normal) then begin
		positions[[0,2],*] = positions[[0,2],*] / xsize
		positions[[1,3],*] = positions[[1,3],*] / ysize
		
		p_region[[0,2]] = p_region[[0,2]] / xsize
		p_region[[1,3]] = p_region[[1,3]] / ysize
		
		p_areas[[0,2],*] = p_areas[[0,2],*] / xsize
		p_areas[[1,3],*] = p_areas[[1,3],*] / ysize

	;Create integer values for device coordinates.
	endif else begin
	    positions[[0,1],*] = fix(floor(positions[[0,1],*]))
	    positions[[2,3],*] = fix(ceil(positions[[2,3],*]))
	    
	    p_region[[0,1]] = fix(floor(p_region[[0,1]]))
	    p_region[[2,3]] = fix(ceil(p_region[[2,3]]))
		
		p_areas[[0,1],*] = fix(floor(p_areas[[0,1]]))
		p_areas[[2,3],*] = fix(ceil(p_areas[[2,3]]))
	endelse

;-----------------------------------------------------------------------------------------
; Save the Positions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------------------------------------------
	self.x_region = p_region[[0,2]]
	self.y_region = p_region[[1,3]]
	self.x_window = p_areas[[0,2]]
	self.y_window = p_areas[[1,3]]
    *self.grid    = positions
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
function MrLayoutAtom::ConvertLocation, location, layout, $
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
    if n_elements(layout) eq 0 then layout = self.layout
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
    
    ;Convert from [col, row] to pIndex (or vice versa).
    if to_pIndex + to_aIndex + to_colrow eq 0 then begin
        to_ColRow = pIndex
        to_pIndex = colrow
    endif
    if to_pIndex + to_aIndex + to_colrow ne 1 then $
        message, 'Keywords TO_PINDEX, TO_AINDEX, and TO_COLROW are mutually exclusive.'
    
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
            to_aIndex: result = location - 1
            to_ColRow: result = array_indices(layout, location-1, /DIMENSIONS) + 1
            to_pIndex: result = location
        endcase
    
    ;ARRAY INDEX
    endif else if aIndex then begin
        case 1 of
            to_aIndex: result = location
            to_ColRow: result = array_indices(layout, location, /DIMENSIONS) + 1
            to_pIndex: result = location + 1
        endcase
        
    ;[COL, ROW]
    endif else if colrow then begin
        case 1 of
            to_aIndex: result = layout[0]*(location[1,*]-1) + location[0,*] - 1
            to_ColRow: result = location
            to_pIndex: result = layout[0]*(location[1,*]-1) + location[0,*]
        endcase
    endif
    
    return, result
end


;+
;   Remove columns from the layout while keeping the plot in the same location.
;
; :Params:
;       NCOLS:          in, optional, type=int, default=1
;                       Number of columns to add.
;       COL:            in, optional, type=int, default=last column
;                       The column to be deleted. If `NCOLS` > 1, then this is the first
;                           of `NCOLS` adjacent columns that will be deleted.
; :Keywords:
;       BEFORE:         in, optional, type=boolean, default=0
;                       If set, then `COL` is the last of `NCOLS` adjacent columns to be deleted.
;-
pro MrLayoutAtom::DeleteColumn, nCols, col, $
BEFORE=before
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Default to deleting a single column from the right of the layout.
    before = keyword_set(before)
    if n_elements(col)   eq 0 then col   = self.layout[0]
    if n_elements(nCols) eq 0 then nCols = 1
    
    ;Removing nothing?
    if nCols eq 0 then return
    
    ;Make sure nCols is positive.
    _nCols = abs(nCols)
    if _nCols ge self.layout[0] $
        then message, string(FORMAT='(%"Layout has only %i columns. Cannot delete %i columns.")', self.layout[0], _nCols)
    
    ;Determine new layout and pIndex
    newLayout     = self.layout
    newLayout[0] -= _nCols
    
    ;There must be at least 1 column in the layout
    if newLayout[0] le 0 then begin
        newLayout = 1
        _nCols    = self.layout[0] - 1
    endif
    
    ;First and last columns begin deleted
    if before then begin
        first_col = col - _nCols + 1
        last_col  = col
    endif else begin
        first_col = col
        last_col  = col + _nCols - 1
    endelse
    
    ;Is the graphic being deleted?
    colrow = self -> ConvertLocation(self.layout[2], /PINDEX, /TO_COLROW)
    if (colrow[0] ge first_col) && (colrow[0] le last_col) $
        then message, 'Column ' + strtrim(colrow[0], 2) + ' is not empty. Cannot delete.'
    
    ;Does the graphic need to be moved?
    if colrow[0] gt last_col then colrow[0] -= _nCols
    
    ;Get the new pIndex
    newPIndex = self -> ConvertLocation(colrow, newLayout, /COLROW, /TO_PINDEX)
    
    ;Update the layout
    self.layout = [newLayout, newPIndex]
    self -> SetGrid
end


;+
;   Remove rows from the layout while keeping the plot in the same location.
;
; :Params:
;       NROWS:          in, optional, type=int, default=1
;                       Number of rows to delete.
;       ROW:            in, optional, type=int, default=bottom row
;                       The row to be deleted. If `NROWS` > 1, then this is the first
;                           of `NROWS` adjacent rows that will be deleted.
; :Keywords:
;       BEFORE:         in, optional, type=boolean, default=0
;                       If set, then `ROW` is the last of `NROWS` adjacent rows to be deleted.
;-
pro MrLayoutAtom::DeleteRow, nRows, row
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Default to adding a single row
    before = keyword_set(before)
    if n_elements(row)   eq 0 then row   = self.layout[1]
    if n_elements(nRows) eq 0 then nRows = 1
    
    ;Removing nothing?
    if nRows eq 0 then return
    
    ;Make sure nRows is positive.
    _nRows = nRows lt 0 ? abs(nRows) : nRows
    if _nRows ge self.layout[1] $
        then message, string(FORMAT='(%"Layout has only %i rows. Cannot delete %i.")', self.layout[1], _nRows)
    
    ;Determine new layout and pIndex
    newLayout     = self.layout
    newLayout[1] -= _nRows
    
    ;First and last row to be deleted
    if before then begin
        first_row = row - _nRows + 1
        last_row  = row
    endif else begin
        first_row = row
        last_row  = row + _nRows - 1
    endelse
    
    ;Is the graphic being deleted?
    colrow = self -> ConvertLocation(self.layout[2], /PINDEX, /TO_COLROW)
    if (colrow[1] ge first_col) && (colrow[1] le last_col) $
        then message, 'Row ' + strtrim(colrow[1], 2) + ' is not empty. Cannot delete.'
        
    ;Does the graphic need to be moved?
    if colrow[1] gt last_row then colrow[1] -= _nRows
    
    ;Get the new pIndex
    newPIndex = self -> ConvertLocation(colrow, newLayout, /COLROW, /TO_PINDEX)
    
    ;Update the layout
    self.layout = [newLayout, newPIndex]
    self -> SetGrid
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
;       EXPAND:         in, optional, type=boolean, default=0
;                       If set, and there are not enough columns to shift by `NCOLS`,
;                           then the layout will be expanded to accommodate the
;                           request; however, it can only be expanded to the right.
;                           Shifting too far to the left will cause an error, unless
;                           `WRAP` or `FEED` is set.
;       FEED:           in, optional, type=boolean, default=0
;                       If set, and there are not enough columns to shift by `NCOLS`,
;                           then the graphic will be pushed down a row and be wrapped
;                           around, like a typewriter's line feed.
;       WRAP:           in, optional, type=boolean, default=1
;                       If set, the graphic will be rapped around from bottom to top
;                           if `NROWS` pushes the graphic out of the layout. If set equal
;                           to zero, the same situation will cause an error.
;-
pro MrLayoutAtom::ShiftColumn, nCols, $
EXPAND=expnd, $
FEED=feed, $
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
    wrap  = keyword_set(wrap)
    expnd = keyword_set(expnd)
    feed  = keyword_set(feed)
    if expnd + feed + wrap gt 0 then message, 'EXPAND, FEED, and WRAP are mutually exclusive.'
    if n_elements(nCols) eq 0 then nCols = 1
    
    ;Convert the plot index to a [col,row] location
    colRow = self -> ConvertIndex(self.layout[3], /PINDEX, /TO_COLROW)
    
    ;Calculate the new row
    newCol = colRow[0] + nCols
    
    ;Not enough places?
    if newCol gt self.layout[0] || newCol le 0 then begin
        ;Wrap around?
        if wrap then begin
            ;Column 0 wraps to the right-most column
            newCol = abs(newCol mod self.layout[0])
            if newCol eq 0 then newCol = self.layout[0]
        
        ;Expand the layout?
        endif else if expnd then begin
            if nCols lt 0 then message, 'Cannot expand layout to the left.'
            nAdd = newCol - colRow[0]
            self -> AddColumn, nAdd
        
        ;Feed into new column?
        endif else if feed then begin
            ;Shift over one column
            self -> ShiftRow, 1
            
            ;Wrap around
            ;   - New row is the remainder of NEWROW / LAYOUT[1]
            ;   - Row 0 wraps around to the bottom row
            newCol = abs(newCol mod self.layout[0])
            if newCol eq 0 then newCol = self.layout[0]
            
        ;Cannot fit graphic
        endif else begin
            message, 'NCOLS is too large. Cannot shift.'
        endelse
    endif
        
    ;Convert back to a plot index
    pIndex = self -> ConvertIndex([newCol, colRow[1]], /COLROW, /PINDEX)
    
    ;Update the layout and position
    self -> SetProperty, LOCATION=pIndex
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
;       EXPAND:         in, optional, type=boolean, default=0
;                       If set, and there are not enough rows to shift by `NROWS`,
;                           then the layout will be expanded to accommodate the
;                           request; however, it can only be expanded down.
;                           Shifting too far up will cause an error, unless `WRAP` is set.
;       FEED:           in, optional, type=boolean, default=0
;                       If set, and there are not enough rows to shift by `NROWS`,
;                           then the graphic will be pushed over a column and be wrapped
;                           around, like a typewriter's line feed.
;       WRAP:           in, optional, type=boolean, default=0
;                       If set, the graphic will be rapped around from bottom to top
;                           if `NROWS` pushes the graphic out of the layout. If not set,
;                           the same situation will cause an error.
;-
pro MrLayoutAtom::ShiftRow, nRows, $
EXPAND=expnd, $
FEED=feed, $
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
    expnd = keyword_set(expnd)
    feed  = keyword_set(feed)
    wrap  = keyword_set(wrap)
    if expnd + feed + wrap gt 0 then message, 'EXPAND, FEED, and WRAP are mutually exclusive.'
    if n_elements(nRows) eq 0 then nRows = 1
    
    ;Convert the plot index to a [col,row] location
    colRow = self -> ConvertIndex(self.layout[3], /PINDEX, /TO_COLROW)
    
    ;Calculate the new row
    newRow = colRow[1] + nRows
    
    ;Not enough places?
    if newRow gt self.layout[1] || newRow le 0 then begin
        ;Wrap around?
        if wrap then begin
            ;New row is the remainder of NEWROW / LAYOUT[1]
            ;   - Row 0 wraps around to the bottom row
            newRow = abs(newRow mod self.layout[1])
            if newRow eq 0 then newRow = self.layout[1]
        
        ;Expand the layout?
        endif else if expnd then begin
            if nRow lt 0 then message, 'Cannot expand layout upward.'
            nAdd = newRow - colRow[1]
            self -> AddRow, nAdd
        
        ;Feed into new column?
        endif else if feed then begin
            ;Shift over one column
            self -> ShiftCol, 1
            
            ;Wrap around
            ;   - New row is the remainder of NEWROW / LAYOUT[1]
            ;   - Row 0 wraps around to the bottom row
            newRow = abs(newRow mod self.layout[1])
            if newRow eq 0 then newRow = self.layout[1]
        
        ;Cannot fit graphic
        endif else begin
            message, 'NROWS is too large. Cannot shift.'
        endelse
    endif
        
    ;Convert back to a plot index
    pIndex = self -> ConvertIndex([colrow[0], newRow], /COLROW, /PINDEX)
    
    ;Update the layout and position
    self -> SetProperty, LOCATION=pIndex
end


;+
;   Calculate a position based on the layout properties.
;
; :Private:
;-
pro MrLayoutAtom::SetGrid
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
    if (self.layout[0] eq 0) && (self.layout[1] eq 0) then return
    
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
                    YGAP       =  self.ygap)
    
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
;       LOCATION:       in, required, type=integer/intarr(N)/intarr(2,N)
;                       Either the plot index or the [col, row] location of the plot
;                           for which the position is to be returned.
;
; :Keywords:
;       COLROW:         in, optional, type=boolean, default=0
;                       If set, `INDEX` is take to be a [col,row] location. 2xN arrays
;                           are accepted.
;
; :Returns:
;       POSITION:       Position within the current layout to `PINDEX`. If `PINDEX`
;                           is a scalar, the output is a 4-element array. Otherise a 4xN
;                           array is returned, where N is the number of locations given.
;-
function MrLayoutAtom::GetPosition, location, $
COLROW=colrow
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
    endif
    
    ;Return the current position?
    if n_elements(location) eq 0 then begin
        if self.layout[2] eq 0 then return, self.position
        
        ;Use the layout position.
        colrow   = 0
        location = self.layout[2]
    endif
    
    ;Default    
    colrow = keyword_set(colrow)

    ;Get the array index associated with the location given.
    aIndex = self -> ConvertLocation(location, COLROW=colrow, PINDEX=~colrow, /TO_AINDEX)

    ;Get the position
    position = (*self.grid)[*,aIndex]

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
pro MrLayoutAtom::GetProperty, $
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
;   Resize the layout.
;
; :Params:
;       LAYOUT:         in, required, type=intarr(2)
;                       The number of [columns, rows] in the new layout.
;-
pro MrLayoutAtom::SetLayout, layout
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;By how many columns and rows is the layout changing?
    nCols = layout[0] - self.layout[0]
    nRows = layout[1] - self.layout[1]
    
    ;Columns
    case 1 of
        nCols eq 0: ;Do nothing
        nCols gt 0: self -> AddColumn, nCols
        nCols lt 0: self -> DeleteColumn, nCols
        else: message, 'LAYOUT must contain an integer number of columns.'
    endcase
    
    ;Rows
    case 1 of
        nRows eq 0: ;Do nothing
        nRows gt 0: self -> AddRow, nRows
        nRows lt 0: self -> DeleteRow, nRows
        else: message, 'LAYOUT must contain an integer number of rows.'
    endcase
    
    ;Compute the grid
    self -> ComputeGrid
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
;       IXMARGIN:       in, optional, type=intarr(2)
;                       Width of the left and right inner margins in character units.
;       IYMARGIN:       in, optional, type=intarr(2)
;                       Height of the top and bottom inner margins in character units.
;       LAYOUT:         in, optional, type=intarr(2)/intarr(3)
;                       An array of the form [nCols, nRows] or [nCols, nRows, index].
;                           [nCols,nRows] is the number of columns and rows in the display.
;                           "index" is the plot index at which to place the plot,
;                           beginning with 1 in the upper-left corner and
;                           incrementing first right then down. If 2-elements, the current
;                           index will be kept.
;       LOCATION:       in, optional, type=int/intarr(2)
;                       Another way of specifying the index in `LAYOUT`. Can also be the
;                           [column, row] in which the graphic is to be placed. If given,
;                           `LAYOUT` is ignored and the current layout is used.
;       MARGIN:         in, optional, type=int/intarr(4)
;                       Size of the [left, bottom, right, top] margins, in character
;                           units. If a scalar is provided, all margins will be equal.
;                           This keyword takes precedence over `OXMARGIN` and `OYMARGIN`.
;       OXMARGIN:       in, optional, type=intarr
;                       Size of the left and right outer margins, in units of !D.X_CH_Size.
;                           Acts as a matte around the X_REGION.
;       OYMARGIN:       in, optional, type=intarr
;                       Size of the bottom and top outer margins, in units of !D.Y_CH_Size.
;                           Acts as a matte around the Y_REGION.
;       XGAP:           in, optional, type=integer
;                       Horizontal space between plots, in character units.
;       YGAP:           in, optional, type=integer
;                       Vertical space between plots in character units.
;-
pro MrLayoutAtom::SetProperty, $
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
    
    ;Column width
    nColW = n_elements(col_width)
    if nColW gt 0 then begin
        if nColW eq 1 || nColW eq layout[0]-1 $
            then *self.col_width = col_width $
            else message, 'COL_WIDTH: Incorrect number of elements', /INFORMATIONAL
    endif
    
    ;Row height
    nRowH = n_elements(row_height)
    if nRowH gt 0 then begin
        if nRowH eq 1 || nRowH eq layout[1]-1 $
            then *self.row_height = row_height $
            else message, 'ROW_HEIGHT: Incorrect number of elements', /INFORMATIONAL
    endif
        
;---------------------------------------------------------------------
; Layout, Location, Position /////////////////////////////////////////
;---------------------------------------------------------------------
    ;Layout
    nLay = n_elements(layout)
    if nLay gt 0 then begin
        case nLay of
            2: self.layout[0:1] = layout
            3: self.layout      = layout
            else: message, 'Incorrect number of elements: Layout.'
        endcase
    endif

    ;Location
    if n_elements(location) gt 0 then begin
        oLocation = location
        
        ;Convert [col,row] to pIndex
        ;   - Make sure it fits within the layout.
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
; Set the New Position ///////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Position takes precedence over LAYOUT and LOCATION
    if n_elements(position) gt 0 then begin
        self.position  = position
        self.layout[2] = 0
    endif else begin
        self.position = self -> GetPosition()
    endelse
end


;+
;
;-
pro MrLayoutAtom::ViewGrid
    compile_opt idl2
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMSG()
        return
    endif

    ;Create the window
    win = MrWindow(ASPECT     = *self.aspect, $
                   CHARSIZE   =  self.charsize, $
                   COL_WIDTH  = *self.col_width, $
                   IXMARGIN   =  self.ixmargin, $
                   IYMARGIN   =  self.iymargin, $
                   LAYOUT     =  self.layout, $
                   OXMARGIN   =  self.oxmargin, $
                   OYMARGIN   =  self.oymargin, $
                   ROW_HEIGHT = *self.row_height, $
                   XGAP       = *self.xgap, $
                   XSIZE      =    !d.x_size, $
                   YGAP       = *self.ygap, $
                   YSIZE      =    !d.y_size)

    ;Show the outer margins
    !Null = MrPlotS( self.p_region[[0,2,2,0,0]], self.p_region[[1,1,3,3,1]], $
                     /NORMAL, COLOR='Blue' )

    ;I[XY]MARGIN -- Bounded in red by P_AREAS. The space between the red box and the axes.
    ;[XY]GAPS    -- Space between P_AREAS (red boxes) demonstrates the gaps.
    for ii = 0, nCols*nRows-1 do begin
        ;Draw the inner margins
        !Null = MrPlotS( self.p_region[[0,2,2,0,0],ii], self.p_region[[1,1,3,3,1],ii], $
                         /NORMAL, COLOR=cgColor('red') )
        
        ;Draw the plots
        !Null = MrPlot( !x.range, !y.range, POSITION=pos[*,ii], /CURRENT, /NODATA, /NORMAL, $
                        TITLE='Title', XTITLE='X-Title', YTITLE='Y-Title', CHARSIZE=charsize)
    endfor
end


;+
;   Clean up after the object is destroy
;-
pro MrLayoutAtom::cleanup
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
function MrLayoutAtom::init, $
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
        ;Are all windows closed?
        if (!d.window eq -1) then begin
            ;Does the device support windows?
            if ((!d.flags and 256) gt 0) then begin
                ;Create an invisible window and get its dimensions.
                window, /FREE, /PIXMAP
                wDims = [!d.x_vsize, !d.y_vsize]
                wDelete, !d.window
            endif else begin
                message, 'Windows not supported and window not available. Provide WDIMS.'
            endelse
        ;Use dimensions of current window
        endif else begin
            wDims = [!d.x_vsize, !d.y_vsize]
        endelse
    endif

    ;Default Margin
    ;   - Takes precedence over O[XY]MARGIN
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
    
    ;Layout
    case n_elements(layout) of
        0: layout = [1,1,0]
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
    if n_elements(col_width)  eq 0 then col_width  = layout[0] eq 1 ? 1.0 : replicate(1.0 / layout[0], layout[0]-1)
    if n_elements(ixmargin)   eq 0 then ixmargin   = [0, 0]
    if n_elements(iymargin)   eq 0 then iymargin   = [0, 0]
    if n_elements(oxmargin)   eq 0 then oxmargin   = [10, 3]
    if n_elements(oymargin)   eq 0 then oymargin   = [ 4, 2]
    if n_elements(position)   eq 0 then location   = 1
    if n_elements(row_height) eq 0 then row_height = layout[1] eq 1 ? 1.0 : replicate(1.0 / layout[1], layout[1]-1)
    if n_elements(xgap)       eq 0 then xgap       = 14
    if n_elements(ygap)       eq 0 then ygap       = 6
    
    ;Allocate Heap
    self.aspect     = ptr_new(/ALLOCATE_HEAP)
    self.col_width  = ptr_new(/ALLOCATE_HEAP)
    self.grid       = ptr_new(/ALLOCATE_HEAP)
    self.row_height = ptr_new(/ALLOCATE_HEAP)

    ;Set Properties
    self -> SetProperty, ASPECT     = aspect, $
                         COL_WIDTH  = col_width, $
                         CHARSIZE   = charsize, $
                         IXMARGIN   = ixmargin, $
                         IYMARGIN   = iymargin, $
                         LAYOUT     = layout, $
                         LOCATION   = location, $
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
pro MrLayoutAtom__define, class
    compile_opt strictarr
    
    define = { MrLayoutAtom, $
               aspect:     ptr_new(), $
               charsize:   0.0, $
               col_width:  ptr_new(), $
               grid:       ptr_new(), $
               layout:     intarr(3), $
               ixmargin:   [0.0,0.0], $
               iymargin:   [0.0,0.0], $
               oxmargin:   [0.0,0.0], $
               oymargin:   [0.0,0.0], $
               position:   [0.0,0.0,0.0,0.0], $
               row_height: ptr_new(), $
               wDims:      lonarr(2), $
               xmargin:    [0.0, 0.0], $
               xgap:       0.0, $
               x_region:   [0.0, 0.0], $
               x_window:   [0.0, 0.0], $
               ymargin:    [0.0, 0.0], $
               ygap:       0.0, $
               y_region:   [0.0, 0.0], $
               y_window:   [0.0, 0.0] $
             }
end