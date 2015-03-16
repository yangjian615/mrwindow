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
pro MrLayout::AddColumn, nCols, col, $
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
	newLayout     = self.layout[0:1]
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
	self -> ComputeGrid
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
pro MrLayout::AddRow, nRows, row, $
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
	newLayout     = self.layout[0:1]
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
	self -> ComputeGrid
end


;+
;   Set properties of the object.
;-
pro MrLayout::ComputeGrid
	compile_opt strictarr
	on_error, 2

	;Do not set if we are being managed
	if self.isManaged then return

	;Set the grid
	self -> MrLayoutAtom::ComputeGrid
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
pro MrLayout::DeleteColumn, nCols, col, $
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
	newLayout     = self.layout[0:1]
	newLayout[0] -= _nCols

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
	self -> ComputeGrid
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
pro MrLayout::DeleteRow, nRows, row, $
BEFORE=before
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
	newLayout     = self.layout[0:1]
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
	if (colrow[1] ge first_row) && (colrow[1] le last_row) $
		then message, 'Row ' + strtrim(colrow[1], 2) + ' is not empty. Cannot delete.'
	
	;Does the graphic need to be moved?
	if colrow[1] gt last_row then colrow[1] -= _nRows

	;Get the new pIndex
	newPIndex = self -> ConvertLocation(colrow, newLayout, /COLROW, /TO_PINDEX)

	;Update the layout
	self.layout = [newLayout, newPIndex]
	self -> ComputeGrid
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
	if arg_present(managed) then managed = self.isManaged
	if n_elements(extra) gt 0 then self -> MrLayoutAtom::GetProperty, _STRICT_EXTRA=extra
end


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
    self.isManaged = ~keyword_set(disable)
end


;+
;   Set the grid. When the layout is being managed, ::ComputeGrid does not
;   execute. To keep the grid positions up-to-date (so that ::GetPosition
;   can obtain the current position), it must be set by the layout manager.
;   This method should not be called by anything other than the layout
;   manager.
;
; :Private:
;
; :Params:
;       GRID:           in, required, type=Nx4 fltarr
;                       An array containing plot positions for each plot in the layout.
;-
pro MrLayout::SetGrid, grid
    compile_opt strictarr
    on_error, 2
    
    ;Set the grid
    *self.grid = grid
end


;+
;   Set properties of the object.
;
; :Keywords:
;       MANAGE:         in, optional, type=boolean
;                       If set, the layout will be managed by the layout manager.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by the MrLayoutAtom::Init method is
;                           also accepted via keyword inheritance.
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
;   The purpose of this method is to shift a given amount of rows.
;
; :Params:
;       NCOLS:          in, optional, type=int, default=1
;                       Number of columns to shift the graphic. If NCOLS is positive
;                           (negative), the graphic will be shifted right (left).
;
; :Keywords:
;       EXPAND:         in, optional, type=boolean, default=0
;                       If set, graphics that are shifted beyond the layout will cause
;                           additional columns to be added to the layout. If `FEED` is
;                           also set, graphics will be fed through to the last row, then
;                           additional rows will be added to accommodate the shift.
;       FEED:           in, optional, type=boolean, default=0
;                       If set, graphics that are shifted past the last (first) column
;                           will be fed into the first (last) column one row below (above)
;                           its current location (like a carriage return + line feed). If
;                           also pushed beyond the first or last row, an error will occur
;                           unless `WRAP` or `EXPAND` is set.
;       NEWLAYOUT:      out, optional, type=intarr(3)
;                       The new layout after shifting has occurred: [nCols, nRows, pIndex].
;       TEST:           in, optional, type=boolean, default=0
;                       If set, a dry run will be performed and nothing will be shifted.
;                           See the `STATUS` keyword for results.
;       STATUS:         out, optional, type=integer
;                       Final status of the shift. Values include::
;                           0  -  Successful shift.
;                           1  -  Incorrect inputs given.
;                           2  -  Not enough columns to perform shift.
;                           3  -  WRAP or EXPAND required for FEED to work.
;                           4  -  Error WRAPping or EXPANDing rows with FEED set.
;       WRAP:           in, optional, type=boolean, default=1
;                       If set, graphics that are shifted past the last (first) column
;                           will wrap to the first (last) column. If `FEED` is also set,
;                           the feed will occur first. If a graphic is shifted past the
;                           last (first) row, it will be wrapped to the first (last)
;                           row.
;-
pro MrLayout::ShiftColumn, nCols, $
EXPAND=expnd, $
FEED=feed, $
NEWLAYOUT=newLayout, $
STATUS=status, $
TEST=test, $
WRAP=wrap
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif
	
	;No errors
	status = 0

;---------------------------------------------------------------------
; Defaults ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	expnd = keyword_set(expnd)
	feed  = keyword_set(feed)
	test  = keyword_set(test)
	wrap  = keyword_set(wrap)
	send_message = ~arg_present(status)
	if n_elements(nCols) eq 0 then nCols = 1
	
	if expnd + wrap gt 1 then begin
		status = 1
		msg    = 'EXPAND and WRAP are mutually exclusive.'
		if send_message then message, msg
	endif

;---------------------------------------------------------------------
; New Location ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Convert the plot index to a [col,row] location
	colRow = self -> ConvertLocation(self.layout[2], /PINDEX, /TO_COLROW)

	;New column and layout
	newLayout = self.layout[0:1]
	tempCol   = colRow[0] + nCols

;---------------------------------------------------------------------
; Does Not Fit ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	nAdd = 0
	if tempCol gt self.layout[0] || tempCol le 0 then begin
		;Number of shifts to get to the first/last column
		n_to_lastcol = (nCols gt 0) ? (self.layout[0] - colRow[0]) : (colRow[0] - 1)
		
		;Once we are at the end of the row, we can determine how many
		;complete rows we will have to skip.
		n_shift_rows = ceil( float(abs(nCols) - n_to_lastcol) / self.layout[0] )
		
		;The number of columns remaining will determine the destination.
		;   - N_SHIFT_ROWS includes the destination row, so subtract 1.
		;   - If NCOLS > 0, add from beginning of layout and subtract 1 (1 - 1 = 0)
		;   - If NCOLS < 0, subtract (add negative) from layout and add 1.
		finalCol = (nCols gt 0) ? (nCols - n_to_lastcol - (n_shift_rows - 1) * self.layout[0]) : $
		                          (nCols + n_to_lastcol + (n_shift_rows - 1) * self.layout[0]) + self.layout[0] + 1

	;---------------------------------------------------------------------
	; Feed ///////////////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		if feed then begin
			;Is a wrap or expansion required?
			;   - Yes if the number of rows to shift is larger than the number of
			;     rows above or below us.
			;   - WRAP and EXPAND mean something different when FEED is set.
			tf_wrexp = (nCols gt 0) ? n_shift_rows gt self.layout[1] - colRow[1] : $
			                          n_shift_rows ge colRow[1]
			
			;Shift is possible
			;   - WRAP and EXPAND are not necessary.
			;   - Or they are and one of the keywords is set.
			if (tf_wrexp eq 0) || wrap || expnd then begin
				;Shift to the correct row.
				;   - How we shift to a new row will be handled properly with
				;     the WRAP and EXPAND keywords.
				self -> ShiftRow, n_shift_rows, $
				                  EXPAND    = expnd, $
				                  NEWLAYOUT = newLayout, $
				                  STATUS    = rstat, $
				                  TEST      = test, $
				                  WRAP      = wrap

				;Did an error occur?
				if status ne 0 then begin
					status = 4
					if send_message then message, 'Error shifting rows.'
				endif
				
				;Update the graphic's location
				;   - Take the final column calculated above
				;   - Find the row within the new layout
				tempColRow = self -> ConvertLocation(newLayout[2], newLayout[0:1], /PINDEX, /TO_COLROW)
				newCol = finalCol
				newRow = tempColRow[1]
			
			;Shift is not possible
			endif else begin
				status = 3
				msg    = 'Error shifting columns: Not enough rows for FEED. See WRAP and EXPAND.'
				if send_message then message, msg
			endelse
			
	;---------------------------------------------------------------------
	; Wrap ///////////////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		;Keep the graphic in the same row and wrap it from end to end.
		endif else if wrap then begin
			;FINALCOL was calculated assuming we were wrapping.
			;   - The row will stay the same.
			newCol = finalCol
			newRow = colrow[1]

	;---------------------------------------------------------------------
	; Expand /////////////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		;Keep the graphic in the same row and add columns until there is room.
		endif else if expnd then begin
			;Add the columns
			;   - Add BEFORE: tempCol = 1 - nAdd                nCols < 0
			;   - Add AFTER:  tempCol = layout[0] + nAdd        nCols > 0
			if nCols gt 0 then begin
				nAdd   = tempCol - self.layout[0]
				newCol = self.layout[0] + nAdd
			endif else begin
				nAdd   = abs(tempCol) + 1
				newCol = 0
			endelse
			
			;Same row
			newRow = colRow[1]
		
	;---------------------------------------------------------------------
	; Not Possible ///////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		endif else begin
			status = 2
			msg    = 'NCOLS is too large. Cannot shift.'
			if send_message then message, msg
		endelse

;---------------------------------------------------------------------
; Fits ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else begin
		newCol = tempCol
		newRow = colrow[1]
	endelse

;---------------------------------------------------------------------
; Move to New Location ///////////////////////////////////////////////
;---------------------------------------------------------------------
	;Was this just a test?
	if test then begin
		newLayout[0] += nAdd
		newLayout[2]  = self -> ConvertLocation([newCol, newRow], newLayout, /COLROW, /TO_PINDEX)
	
	;Real deal
	endif else begin
		;Add the proper number of columns
		;   - Row shift has already occurred.
		if nAdd gt 0 then self -> AddColumn, nAdd, BEFORE=(nCols lt 0)
		
		;Update the position
		self -> SetProperty, LOCATION=[newCol, newRow]
		newLayout = self.layout
	endelse
end


;+
;   The purpose of this method is to shift a given amount of rows.
;
; :Params:
;       NROWS:          in, optional, type=int, default=1
;                       Number of rows to shift the graphic. If NROWS is positive
;                           (negative), the graphic will be shifted down (up).
;
; :Keywords:
;       EXPAND:         in, optional, type=boolean, default=0
;                       If set, graphics that are shifted beyond the layout will cause
;                           additional rows to be added to the layout. If `FEED` is
;                           also set, graphics will be fed through to the last column, then
;                           additional columns will be added to accommodate the shift.
;       FEED:           in, optional, type=boolean, default=0
;                       If set, graphics that are shifted past the bottom (top) row
;                           will be fed into the column to the right (left) of its current
;                           location, then wrapped to the top (bottom) (lke a carriage
;                           return + line feed). If also pushed beyond the first or last
;                           column, an error will occur unless `WRAP` or `EXPAND` is set.
;       NEWLAYOUT:      out, optional, type=intarr(3)
;                       The new layout after shifting has occurred: [nCols, nRows, pIndex].
;       TEST:           in, optional, type=boolean, default=0
;                       If set, a dry run will be performed and nothing will be shifted.
;                           See the `NEWLAYOUT` and `STATUS` keyword for results.
;       STATUS:         out, optional, type=integer
;                       Final status of the shift. Values include::
;                           0  -  Successful shift.
;                           1  -  Incorrect inputs given.
;                           2  -  Not enough columns to perform shift.
;                           3  -  WRAP or EXPAND required for FEED to work.
;                           4  -  Error WRAPping or EXPANDing rows with FEED set.
;       WRAP:           in, optional, type=boolean, default=1
;                       If set, graphics that are shifted past the bottom (top) row
;                           will wrap to the top (bottom) row. If `FEED` is also set,
;                           the feed will occur first. If a graphic is shifted past the
;                           last (first) column, it will be wrapped to the first (last)
;                           column.
;-
pro MrLayout::ShiftRow, nRows, $
EXPAND=expnd, $
FEED=feed, $
NEWLAYOUT=newLayout, $
STATUS=status, $
TEST=test, $
WRAP=wrap
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif
	
	;No errors
	status = 0

;---------------------------------------------------------------------
; Defaults ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	expnd = keyword_set(expnd)
	feed  = keyword_set(feed)
	test  = keyword_set(test)
	wrap  = keyword_set(wrap)
	send_message = ~arg_present(status)
	if n_elements(nRows) eq 0 then nRows = 1
	
	if expnd + wrap gt 1 then begin
		status = 1
		msg    = 'EXPAND and WRAP are mutually exclusive.'
		if send_message then message, msg
	endif

;---------------------------------------------------------------------
; New Location ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Convert the plot index to a [col,row] location
	colRow = self -> ConvertLocation(self.layout[2], /PINDEX, /TO_COLROW)

	;New column and layout
	newLayout = self.layout[0:1]
	tempRow   = colRow[1] + nRows

;---------------------------------------------------------------------
; Does Not Fit ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	nAdd = 0
	if tempRow gt self.layout[1] || tempRow le 0 then begin
		;Number of shifts to get to the bottom/top row
		n_to_lastrow = (nRows gt 0) ? (self.layout[1] - colRow[1]) : (colRow[1] - 1)
		
		;Once we are at the end of the row, we can determine how many
		;complete rows we will have to skip.
		n_shift_cols = ceil( float(abs(nRows) - n_to_lastrow) / self.layout[1] )
		
		;The number of rows remaining will determine the destination.
		;   - N_SHIFT_COLS includes the destination row, so subtract 1.
		;   - If NROWS > 0, add from beginning of layout and subtract 1 (1 - 1 = 0)
		;   - If NROWS < 0, subtract (add negative) from layout and add 1.
		finalRow = (nRows gt 0) ? (nRows - n_to_lastrow - (n_shift_cols - 1) * self.layout[1]) : $
		                          (nRows + n_to_lastrow + (n_shift_cols - 1) * self.layout[1]) + self.layout[1] + 1

	;---------------------------------------------------------------------
	; Feed ///////////////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		if feed then begin
			;Is a wrap or expansion required?
			;   - Yes if the number of rows to shift is larger than the number of
			;     columns to the left or right of us.
			;   - WRAP and EXPAND mean something different when FEED is set.
			tf_wrexp = (nRows gt 0) ? n_shift_cols gt self.layout[0] - colRow[0] : $
			                          n_shift_cols ge colRow[0]
			
			;Shift is possible
			;   - WRAP and EXPAND are not necessary.
			;   - Or they are and one of the keywords is set.
			if (tf_wrexp eq 0) || wrap || expnd then begin
				;Shift to the correct row.
				;   - How we shift to a new row will be handled properly with
				;     the WRAP and EXPAND keywords.
				self -> ShiftColumn, n_shift_cols, $
				                     EXPAND    = expnd, $
				                     NEWLAYOUT = newLayout, $
				                     STATUS    = rstat, $
				                     TEST      = test, $
				                     WRAP      = wrap
				
				;Did an error occur?
				if status ne 0 then begin
					status = 4
					if send_message then message, 'Error shifting columns.'
				endif
				
				;Update the graphic's location
				tempColRow = self -> ConvertLocation(newLayout[2], newLayout[0:1], /PINDEX, /TO_COLROW)
				newCol = tempColRow[0]
				newRow = finalRow
			
			;Shift is not possible
			endif else begin
				status = 3
				msg    = 'Error shifting rows: Not enough columns for FEED. See WRAP and EXPAND.'
				if send_message then message, msg
			endelse
			
	;---------------------------------------------------------------------
	; Wrap ///////////////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		;Keep the graphic in the same column and wrap it from end to end.
		endif else if wrap then begin
			;FINALROW was calculated assuming we were wrapping.
			;   - The column will stay the same.
			newCol = colrow[0]
			newRow = finalRow
		
	;---------------------------------------------------------------------
	; Expand /////////////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		;Keep the graphic in the same column and add rows until there is room.
		endif else if expnd then begin
			;Add the rows
			;   - Add BEFORE: tempRow = 1 - nAdd                nRows < 0
			;   - Add AFTER:  tempRow = layout[0] + nAdd        nRows > 0
			if nRows gt 0 then begin
				nAdd   = tempRow - self.layout[1]
				newRow = self.layout[1] + nAdd
			endif else begin
				nAdd   = abs(tempRow) + 1
				newRow = 0
			endelse
			
			;Same column
			newCol = colrow[0]
		
	;---------------------------------------------------------------------
	; Not Possible ///////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		endif else begin
			status = 2
			msg    = 'NCOLS is too large. Cannot shift.'
			if send_message then message, msg
		endelse

;---------------------------------------------------------------------
; Fits ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else begin
		newCol = colrow[0]
		newRow = tempRow
	endelse

;---------------------------------------------------------------------
; Move to New Location ///////////////////////////////////////////////
;---------------------------------------------------------------------
	;Was this just a test?
	if test then begin
		newLayout[1] += nAdd
		newLayout[2]  = self -> ConvertLocation([newCol, newRow], newLayout, /COLROW, /PINDEX)
	
	;Real deal
	endif else begin
		;Add the proper number of columns
		;   - Row shift has already occurred.
		if nAdd gt 0 then self -> AddRow, nAdd, BEFORE=(nRows lt 0)
		
		;Update the position
		self -> SetProperty, LOCATION=[newCol, newRow]
		newLayout = self.layout
	endelse
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