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
;-
;*****************************************************************************************
;+
;   Allow square-bracket array indexing from the right side of an operator.
;
; :Params:
;       ISRANGE:        in, required, type=intarr
;                       A vector that has one element for each Subscript argument
;                           supplied by the user; each element contains a zero if the
;                           corresponding input argument was a scalar index value or
;                           array of indices, or a one if the corresponding input
;                           argument was a subscript range.
;       I1:             in, required, type=integer/intarr(3)
;                       Index subscripts. Either a scalar, an index array, or a 
;                           subscript range in the form [start, stop, step_size]. Taken
;                           by itself, I1 represents the pIndex locations of the child
;                           layout objects to be returned. A scalar value of 0 will return
;                           the MrLayoutManager "self" object.
;       I2:             in, optional, type=integer/intarr(3)
;                       Index subscripts. If provided, [`I1`, I2] make up [col, row]
;                           locations, with `I1` being the colums and I2 being the rows.
;
; :Returns:
;       RESULT:         in, required, type=numeric array
;                       A subset of all layout objects in the object container.
;-
function MrLayoutManager::_OverloadBracketsRightSide, isRange, i1, i2
    compile_opt strictarr
    on_error, 2

    ;Number of subscripts given
    nSubscripts = n_elements(isRange)

    ;Return self if 0 is the only subscript given.
    if nSubscripts eq 1 && n_elements(i1) eq 1 && i1 eq 0 then return, self

;-----------------------------------------------------
; [col, row] Locations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if nSubscripts eq 2 then begin
        ;Columns
        if isRange[0] eq 1 then begin
            nCols = i1[1] - i1[0]
            cols  = lindgen(nCols) * i1[2]
        endif else begin
            cols  = i1
        endelse
        
        ;Rows
        if isRange[1] eq 1 then begin
            nRows = i2[1] - i2[0]
            rows  = lindgen(nRows) * i2[2]
        endif else begin
            rows  = i2
        endelse
        
        ;Convert to pIndices
        pIndex = self -> ConvertIndex(transpose([[col], [row]]), /COLROW, /TO_PINDEX)
    
;-----------------------------------------------------
; Plot Index Locations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else begin
        if isRange[0] then begin
            nPIndex = i1[1] - i1[0]
            pIndex  = lindgen(nPIndex) * i1[2]
        endif else begin
            pIndex = i1
        endelse
    endelse
    
;-----------------------------------------------------
; Get the Layout Objects \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Allocate memory
    oChildren = self -> Get(/ALL, COUNT=nChildren)
    oLayouts  = objarr(nChildren)
    count     = 0L
    
    ;Step through all children and search for matches
    for i = 0L, nChildren - 1 do begin
        theChild = oChildren[i]
        
        ;Skip if not begin managed
        if theChild -> IsManaged() eq 0 then continue
        
        ;Skip if not a match
        theChild -> GetProperty, LAYOUT=layout
        if max(layout[2] eq pIndex) eq 0 then continue
        
        ;Keep it
        oLayouts[count] = theChild
        count += 1
    endfor
    
    ;Truncate.
    if count eq 1 $
        then oLayouts = oLayouts[0] $
        else oLayouts = oLayouts[0:count-1]

    return, oLayouts
end


;+
;   The purpose of this method is to provide output when the object is an argument to
;   the PRINT procedure.
;
; :Private:
;-
function MrLayoutManager::_OverloadPrint
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
;   Add MrLayout objects to be managed.
;
; :Params:
;       OLAYOUT:        in, required, type=object
;                       A MrLayout object.
;-
pro MrLayoutManager::Add, oLayout
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	;Default to adding a single row
	if MrObj_Class(oLayout) ne 'MRLAYOUT' then $
		message, 'OLAYOUT must be a MRLAYOUT class.'

	;Check if a location has been defined.
	oLayout -> GetProperty, LAYOUT=layout, POSITION=position, MANAGED=managed

	;If not, then manage the layout
	if managed eq 0 && (layout[2] eq 0 && total(position) eq 0) then begin
		managed = 1
		oLayout -> SetProperty, MANAGE=manage
	endif

;-------------------------------------------------------
; Not Being Managed ////////////////////////////////////
;-------------------------------------------------------
	if managed eq 0 then begin
		;Add to the container
		self -> MrIDL_Container::Add, oLayout
		return
	endif

;-------------------------------------------------------
; User-Defined Position ////////////////////////////////
;-------------------------------------------------------
	if total(position) gt 0 && layout[2] eq 0 then begin
		;Add to the container
		self -> MrIDL_Container::Add, oLayout
		return

;-------------------------------------------------------
; User-Defined Location ////////////////////////////////
;-------------------------------------------------------
	endif else if layout[2] ne 0 then begin
		;Does it fit within the current layout?
		tf_exist = self -> Exists(layout[2])
		if tf_exist eq 0 then begin
			newLayout     = self.layout
			newLayout[0] >= layout[0]
			newLayout[1] >= layout[1]
			if ~array_equal(newLayout, self.layout) then self -> SetProperty, LAYOUT=newLayout
		endif

		;Check if something is there already
		!Null = self -> FindByPIndex(layout[2], COUNT=nClash)
	
		;If the spot is taken
		if nClash gt 0 then begin
			;Find all empty cells greater then the given pIndex
			pEmpty = self -> FindEmptyCell(COUNT=nEmpty)
			iEmpty = where(pEmpty gt layout[2], nEmpty)
		
			;If there are no empty cells, add a row
			;   - The next empty location will be [first column, last row].
			;   - Otherwise, pick the first empty location
			if nEmpty eq 0 then begin
				self -> AddRow, 1
				pEmpty = self -> ConvertLocation([1, self.layout[1]], /COLROW, /TO_PINDEX)
			endif else begin
				pEmpty = pEmpty[iEmpty[0]]
			endelse
		
			;Step through all children
			allChildren = self -> Get(/ALL, COUNT=nChildren)
			for i = 0, nChildren-1 do begin
				;Skip children that are not mananged
				if allChildren[i] -> IsManaged() eq 0 then continue
				
				;Pick out the managed layouts
				if ~isManaged then continue
			
				;If the child is between the desired and empty locations, shift it down.
				allChildren[i] -> GetProperty, LAYOUT=childLayout
				if (childLayout[2] ge layout[2]) && (childLayout[2] lt pEmpty) $
					then allChildren[i] -> ShiftColumn, 1, /FEED
			endfor
		endif

		;Final location
		pIndex = layout[2]
;-------------------------------------------------------
; No Location Given ////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Find an empty location
		pEmpty = self -> FindEmptyCell(COUNT=nEmpty)

		;Need to make room?
		if nEmpty eq 0 then begin
			self -> AddRow, 1
			pEmpty = self -> ConvertLocation([1, self.layout[1]], /COLROW, /TO_PINDEX)
		endif
	
		;Select the first empty location
		pIndex = pEmpty[0]
	endelse

	;Synchronize
	oLayout -> SetProperty, LOCATION=pIndex
	self    -> SyncLayout, oLayout

	;Add to the container
	self -> MrIDL_Container::Add, oLayout
end


;+
;   Add columns to the layout while keeping graphics in the same location.
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
pro MrLayoutManager::AddColumn, nCols, col, $
BEFORE=before
	compile_opt strictarr
	on_error, 2

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

	;Step through all children
	oChildren = self -> Get(/ALL, COUNT=nChildren)
	for i = 0, nChildren - 1 do begin
		;Is the child being managed?
		if oChildren[i] -> IsManaged() eq 0 then continue
	
		;Add the column
		oChildren[i] -> AddColumn, nCols, col, BEFORE=before
	endfor

	;Update the layout
	;   - This will compute the new grid and apply it to all managed children.
	self -> SetProperty, LAYOUT=newLayout[0:1]
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
pro MrLayoutManager::AddRow, nRows, row, $
BEFORE=before
	compile_opt strictarr
	on_error, 2

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

	;Determine new layout
	newLayout     = self.layout[0:1]
	newLayout[1] += nRows

	;Step through all children
	oChildren = self -> Get(/ALL, COUNT=nChildren)
	for i = 0, nChildren - 1 do begin
		;Is the child being managed?
		if oChildren[i] -> IsManaged() eq 0 then continue
	
		;Add the column
		oChildren[i] -> AddRow, nRows, row, BEFORE=before
	endfor

	;Update the layout
	;   - This will compute the new grid and apply it to all managed children.
	self -> SetProperty, LAYOUT=newLayout[0:1]
end


;+
;   Remove columns from the layout while keeping graphics in their same locations.
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
pro MrLayoutManager::DeleteColumn, nCols, col, $
BEFORE=before
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	;Default to removing the last column
	before = keyword_set(before)
	_nCols = n_elements(nCols) gt 0 ? abs(nCols) : 1
	if n_elements(col) eq 0 then col = self.layout[0]

	;Determine the new layout
	newLayout     = self.layout[0:1]
	newLayout[0] -= _nCols

	;At least one column must remain
	if _nCols ge self.layout[0] $
		then message, string(FORMAT='(%"Layout has only %i columns. Cannot delete %i columns.")', self.layout[0], _nCols)

	;First and last columns
	if before then begin
		first_col = col - _nCols + 1
		last_col  = col
	endif else begin
		first_col = col
		last_col  = col + _nCols - 1
	endelse

	;Make sure the columns are empty
	void = self -> FindEmptyCells(MASK=mask)
	if total(~mask[first_col-1:last_col-1, *]) gt 0 $
		then message, 'Columns are not empty. Cannot delete.'

	;Remove columns
	oChildren = self -> Get(/ALL, COUNT=nChildren)
	for i = 0, nChildren - 1 do begin
		;Is the child being managed?
		if oChildren[i] -> IsManaged() eq 0 then continue
	
		;Remove the columns
		oChildren[i] -> DeleteColumn, nCols, col, BEFORE=before
	endfor

	;Update the layout
	;   - This will compute the new grid and apply it to all managed children.
	self -> SetProperty, LAYOUT=newLayout[0:1]
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
pro MrLayoutManager::DeleteRow, nRows, row, $
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
	_nRows = n_elements(nRows) gt 0 ? abs(nRows) : 1
	if n_elements(row) eq 0 then row = self.layout[1]

	;Determine new layout
	newLayout     = self.layout
	newLayout[1] -= _nRows

	;At least one row must remain
	if _nRows ge self.layout[1] $
		then message, string(FORMAT='(%"Layout has only %i rows. Cannot delete %i rows.")', self.layout[1], _nRows)

	;First and last rows
	if before then begin
		first_row = row - _nRows + 1
		last_row  = row
	endif else begin
		first_row = row
		last_row  = row + _nRows - 1
	endelse

	;Make sure the columns are empty
	void = self -> FindEmptyCells(MASK=mask)
	if total(~mask[*, first_row-1:last_row-1]) gt 0 $
		then message, 'Rows are not empty. Cannot delete.'

	;Remove rows
	oChildren = self -> Get(/ALL, COUNT=nChildren)
	for i = 0, nChildren - 1 do begin
		;Is the child being managed?
		if oChildren[i] -> IsManaged() eq 0 then continue
	
		;Remove the columns
		oChildren[i] -> DeleteRow, nRows, row, BEFORE=before
	endfor

	;Update the layout
	;   - This will compute the new grid and apply it to all managed children.
	self -> SetProperty, LAYOUT=newLayout[0:1]
end


;+
;   Find a child layout by its [col,row] location.
;
; :Params:
;       COLROW:         in, required, type=2xN intarr
;                       The [column, row] in which the desired graphic is located.
;
; :Returns:
;       OBJECT:         The graphics located at `COLROW`.
;-
function MrLayoutManager::FindByColRow, colrow, $
COUNT=count
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        count = 0
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Convert COLROW to a plot index
    pIndex = self._oLayout -> ConvertLocation(ColRow, /COLROW, /TO_PINDEX)

    ;Call FindByPIndex
    object = self -> FindByPIndex(pIndex, COUNT=count)

    return, object
end


;+
;   Find a child layout by its plot index location.
;
; :Params:
;       PINDEX:         in, required, type=integer
;                       The plot index, starting with 1 and increasing left to right then
;                           top to bottom, in which the desired graphic is located.
;
; :Returns:
;       OBJECT:         The graphics located at `PINDEX`.
;-
function MrLayoutManager::FindByPIndex, pIndex, $
COUNT=count
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        count = 0
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Get all of the objects in the container
    allChildren = self -> Get(/ALL, COUNT=count)
    if count eq 0 then return, obj_new()
    
    ;Get all of the plot indices
    piChild = lonarr(count)
    for i = 0L, count - 1 do begin
        if allChildren[i] -> IsManaged() eq 0 then continue
        allChildren[i] -> GetProperty, LAYOUT=layChild
        piChild[i] = layChild[2]
    endfor

    ;Find a match
    tf_member = MrIsMember(pIndex, piChild, iKeep, COUNT=count)
    if count eq 0 then return, obj_new()
    
    ;Return the matching objects
    return, allChildren[iKeep]
end


;+
;   Find empty locations within the layout.
;
; :Private:
;
; :Keywords:
;       MASK:       out, optional, type=bytarr
;                   An array the same size as the layout. Elements are 1 if the location
;                       is taken and 0 if it is empty.
;       COLROW:     in, optional, type=boolean, default=0
;                   If set, [col,row] locations are returned instead of plot indices.
;       COUNT:      out, optional, type=integer
;                   The number of empty locations found.
;
; :Returns:
;       PEMPTY:     Plot indices of each empty location. If `COUNT`=0, then -1 is returned.
;-
function MrLayoutManager::FindEmptyCells, $
MASK=mask, $
COLROW=colrow, $
COUNT=nEmpty
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, -1
    endif
    
    ;Defaults
    count   = 0
    colrow  = keyword_set(colrow)
    
    ;Get all of the layout objects
    allChildren = self -> Get(/ALL, COUNT=nLayouts)
    
    ;Create a mask of layout positions. Assume all positions are empty.
    mask = make_array(self.layout[0]*self.layout[1], VALUE=1B, /BYTE)
    
    ;Step through each one
    for i = 0, nLayouts - 1 do begin
        thisChild = allChildren[i]
        
        ;Skip it if it is not being managed.
        if thisChild -> IsManaged() eq 0 then continue
        thisChild -> GetProperty, LAYOUT=chLayout
        
        ;Fill in the mask -- we only know if the spot is taken.
        aIndex       = self -> ConvertLocation(chLayout[2], /PINDEX, /TO_AINDEX)
        mask[aIndex] = 0B
    endfor
    
    ;Determine the plot indices
    aEmpty = where(mask eq 1B, nEmpty)
    if nEmpty eq 0 $
        then pEmpty = -1 $
        else pEmpty = self -> ConvertLocation(aEmpty, /AINDEX, TO_COLROW=colrow, TO_PINDEX=~colrow)
    
    ;Return the empty positions
    mask = reform(mask, self.layout[0:1])
    return, pEmpty
end


;+
;   Set properties of the object.
;
; :Keywords:
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by the MrLayoutAtom::SetProperty method is
;                           also accepted via keyword inheritance.
;-
pro MrLayoutManager::SetProperty, $
_REF_EXTRA=extra
	compile_opt strictarr
	on_error, 2

	;Set properties
	if n_elements(extra) gt 0 then begin
		self -> MrLayoutAtom::SetProperty, _STRICT_EXTRA=extra
		self -> SyncLayout, /ALL
	endif
end


;+
;   Synchronize the child layout properties with those of the parent.
;
; :Private:
;
; :Params:
;       CHILD_LAYOUT:   in, required, type=object
;                       A MrLayout child object.
;-
pro MrLayoutManager::SyncLayout, oChild, $
ALL=all
	compile_opt strictarr
	on_error, 2

	;Sync all children?
	if keyword_set(all) then begin
		oChildren = self -> Get(/ALL, COUNT=nChildren)
		for i = 0, nChildren - 1 do self -> SyncLayout, oChildren[i]
		return
	endif

	;Is the child being managed?
	if oChild -> IsManaged() eq 0 then return
	
	;Get the child's current layout
	oChild -> GetProperty, LAYOUT=layout, LOCATION=colrow

	;Set the grid
	oChild -> SetGrid, *self.grid

	;Set properties of the child layout object.
	oChild -> SetProperty, ASPECT     = *self.aspect, $
	                       CHARSIZE   =  self.charsize, $
	                       COL_WIDTH  = *self.col_width, $
	                       IXMARGIN   =  self.ixmargin, $
	                       IYMARGIN   =  self.iymargin, $
	                       LAYOUT     =  self.layout[0:1], $
	                       OXMARGIN   =  self.oxmargin, $
	                       OYMARGIN   =  self.oymargin, $
	                       ROW_HEIGHT = *self.row_height, $
	                       WDIMS      =  self.wdims, $
	                       XGAP       =  self.xgap, $
	                       YGAP       =  self.ygap
	
	;If the layout has changed, keep the child in the same location
	;  - [col, row] locations are not layout-dependent, but plot indices are.
	if layout[0] ne self.layout[0] || self.layout[1] ne self.layout[1] $
		then oChild -> SetProperty, LOCATION=colrow
end


;+
;   The purpose of this method is to shift a given amount of rows.
;
; :Params:
;       LOCATION:       in, optional, type=int/intarr(2)/objref, default=1
;                       The plot index (scalar) or [col, row] location of the graphic
;                           to be shifted to a new column. Alternatively, a child layout
;                           object can be provided. This is useful if multiple children
;                           reside in the same location.
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
;       PUSH:           in, optional, type=boolean, default=0
;                       If set, graphics in adjacent columns will be pushed ahead until
;                           an empty location is found.
;       WRAP:           in, optional, type=boolean, default=1
;                       If set, graphics that are shifted past the last (first) column
;                           will wrap to the first (last) column. If `FEED` is also set,
;                           the feed will occur first. If a graphic is shifted past the
;                           last (first) row, it will be wrapped to the first (last)
;                           row.
;-
pro MrLayoutManager::ShiftColumn, location, nCols, $
EXPAND=expnd, $
FEED=feed, $
PUSH=push, $
WRAP=wrap
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	;Make sure LOCATION is defined
	if n_elements(location) eq 0 $
		then message, 'Usage: LayoutMGR -> ShiftColumn, location [, nCols].'
	nLocs = n_elements(location)

	;Defaults
	expnd = keyword_set(expnd)
	feed  = keyword_set(feed)
	push  = keyword_set(push)
	wrap  = keyword_set(wrap)
	if nLocs gt 2 then message, 'Only one LOCATION can be provided.'
	
	;Incompatible keywords
	;   - Cannot shift rows and NOT shift rows
	if expnd + wrap gt 1 then message, 'EXPAND and WRAP are mutually exclusive.'
	
	;Get the child layout object to be shifted.
	if MrIsA(location, 'MrLayout') then begin
		theChild = location
		count    = 1
		if self -> IsContained(theChild) eq 0 then message, 'Child object is not being managed by me. Cannot shift.'
	endif else begin
		;ColRow or PIndex?
		if n_elements(location) eq 2 $
			then theChild = self -> FindByColRow(location, COUNT=count) $
			else theChild = self -> FindByPIndex(location, COUNT=count)
		if count eq 0 then message, 'Nothing exists at location [' + strjoin(strtrim(location, 2), ',') + '].'
	endelse
	
	;Is the object being managed?
	if theChild -> IsManaged() eq 0 then message, 'Child object is not being managed. Cannot shift.'

	;Get the child's location within the layout
;	theChild -> GetProperty, LAYOUT=tempLay, LOCATION=colrow
;	pIndex = (temporary(tempLay))[2]
;	aIndex = self -> ConvertLocation(pIndex, /PINDEX, /TO_AINDEX)

	;Determine the final location (might not be within the current layout).
;	newColRow = [colrow[0] + nCols, colrow[1]]

	;Determine where the empty cells are
;	void = self -> FindEmptyCells(MASK=mask)

;---------------------------------------------------------------------
; Push ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;PUSH?
	;   - WRAP:  All items in same row wrap. Locations are conserved.
	;   - EXPND: A new column will be added to accomodate pushing out of the layout.
	;   - FEED:  All items will be fed through until an empty location is reached. If no
	;            empty location esists, an error will be thrown unless:
	;               EXPND: A new column will be added.
	;               WRAP:  All items are wrapped to the other end of the layout (conserves locations)
	;
	; Child layout objects are capable of expanding the layout, when necessary, but
	; for those changes to be visible by other children in the layout, the manager must
	; decide when the number of rows or columns changes. This only occurs if EXPND is set.
	; In all other cases,
	if push eq 0 $
		then theChild -> ShiftColumn, nCols, EXPAND=expnd, FEED=feed, WRAP=wrap, NEWLAYOUT=newLayout
	
	if newLayout[0] ne self.layout[0] || newLayout[1] ne self.layout[1] $
		then self -> SetProperty, LAYOUT=newLayout[0:1]
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
;                       If set, and there are not enough columns to shift by `NROWS`,
;                           then the layout will be expanded to accommodate the
;                           request; however, it can only be expanded down.
;                           Shifting too far up will cause an error, unless `WRAP` is set.
;       WRAP:           in, optional, type=boolean, default=0
;                       If set, the graphic will be rapped around from bottom to top
;                           if `NROWS` pushes the graphic out of the layout. If not set,
;                           the same situation will cause an error.
;-
pro MrLayoutManager::ShiftRow, location, nRows, $
EXPAND=expnd, $
FEED=feed, $
PUSH=push, $
WRAP=wrap
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

	;Make sure LOCATION is defined
	if n_elements(location) eq 0 $
		then message, 'Usage: LayoutMGR -> ShiftRow, location [, nRows].'
	nLocs = n_elements(location)

	;Defaults
	expnd = keyword_set(expnd)
	feed  = keyword_set(feed)
	push  = keyword_set(push)
	wrap  = keyword_set(wrap)
	if nLocs gt 2 then message, 'Only one LOCATION can be provided.'
	
	;Incompatible keywords
	;   - Cannot shift rows and NOT shift rows
	if expnd + wrap gt 1 then message, 'EXPAND and WRAP are mutually exclusive.'
	
	;Get the child layout object to be shifted.
	if MrIsA(location, 'MrLayout') then begin
		theChild = location
		count    = 1
		if self -> IsContained(theChild) eq 0 then message, 'Child object is not being managed by me. Cannot shift.'
	endif else begin
		;ColRow or PIndex?
		if n_elements(location) eq 2 $
			then theChild = self -> FindByColRow(location, COUNT=count) $
			else theChild = self -> FindByPIndex(location, COUNT=count)
		if count eq 0 then message, 'Nothing exists at location [' + strjoin(strtrim(location, 2), ',') + '].'
	endelse
	
	;Is the object being managed?
	if theChild -> IsManaged() eq 0 then message, 'Child object is not being managed. Cannot shift.'

;---------------------------------------------------------------------
; Push ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;PUSH?
	;   - WRAP:  All items in same row wrap. Locations are conserved.
	;   - EXPND: A new column will be added to accomodate pushing out of the layout.
	;   - FEED:  All items will be fed through until an empty location is reached. If no
	;            empty location esists, an error will be thrown unless:
	;               EXPND: A new column will be added.
	;               WRAP:  All items are wrapped to the other end of the layout (conserves locations)
	;
	; Child layout objects are capable of expanding the layout, when necessary, but
	; for those changes to be visible by other children in the layout, the manager must
	; decide when the number of rows or columns changes. This only occurs if EXPND is set.
	; In all other cases,
	if push eq 0 $
		then theChild -> ShiftRow, nRows, EXPAND=expnd, FEED=feed, WRAP=wrap, NEWLAYOUT=newLayout $
		else message, 'The PUSH keyword is not implemented yet.'
	
	if newLayout[0] ne self.layout[0] || newLayout[1] ne self.layout[1] $
		then self -> SetProperty, LAYOUT=newLayout[0:1]
end


;+
;   Draw the outer and inner margins (the plot window and region) as well as the
;   position of the graphic.
;
; :Keywords:
;       ALL:                in, optional, type=boolean, default=0
;                           If set, all positions will be displayed. The default is
;                               to display only positions occupied by child layout objects.
;       CURRENT:            in, optional, type=boolean, default=0
;                           If set, the margins and position will be drawn into the
;                               current MrGraphics window.
;       DELETE:             in, optional, type=boolean, default=0
;                           If set, all graphics are deleted from the window before
;                               displaying the grid. Useful for successive calls to
;                               ::ViewGrid.
;-
pro MrLayoutManager::ViewGrid, $
CURRENT=current, $
DELETE=delete
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMSG()
		return
	endif
	
	;Defaults
	all     = keyword_set(all)
	current = keyword_set(current)
	delete  = keyword_set(delete)

	;Create or get the window
	if current then begin
		win = GetMrWindows(/CURRENT)
	endif else begin
		win = Mr_Window(ASPECT     = *self.aspect, $
		                CHARSIZE   =  self.charsize, $
		                COL_WIDTH  = *self.col_width, $
		                IXMARGIN   =  self.ixmargin, $
		                IYMARGIN   =  self.iymargin, $
		                LAYOUT     =  self.layout, $
		                OXMARGIN   =  self.oxmargin, $
		                OYMARGIN   =  self.oymargin, $
		                ROW_HEIGHT = *self.row_height, $
		                XGAP       =  self.xgap, $
		                XSIZE      =    !d.x_size, $
		                YGAP       =  self.ygap, $
		                YSIZE      =    !d.y_size)
	endelse
	win -> Refresh, /DISABLE
	
	;Delete all current objects?
	if delete then win -> Remove, /ALL

	;Show the outer margins
	!Null = MrPlotS( self.x_window[[0,1,1,0,0]], self.y_window[[0,0,1,1,0]], $
	                 /NORMAL, COLOR='Black' )

	;Step through each cell in the grid
	nCells = self.layout[0] * self.layout[1]
	for i = 0, nCells - 1 do begin
		pos = (*self.grid)[*,i]

		;Draw the positions
		!Null = MrPlotS( pos[[0,2,2,0,0]], pos[[1,1,3,3,1]], $
		                 /NORMAL, COLOR='Red' )

		;Draw the plot indices
		!Null = MrText(pos[0] + (pos[2] - pos[0])/2.0, $
		               pos[1] + (pos[3] - pos[1])/2.0, $
		               strtrim(i+1, 2), $
		               ALIGNMENT = 0.5, $
		               CHARSIZE  = 4.0, $
		               /NORMAL)
	endfor
	
	;Now draw the occupied positions
	allChildren = self -> Get(/ALL, COUNT=nChildren)
	for i = 0, nChildren - 1 do begin
		;Get the child and is position
		theChild  = allChildren[i]
		if theChild -> IsManaged() eq 0 then continue
		
		;Draw the child's inner margins and positions
		theChild -> ViewGrid, /IMARGIN, /POSITION, /CURRENT
	endfor
	
	;Refresh the window
	win -> Refresh
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrLayoutManager__define, class
	compile_opt strictarr

	define = { MrLayoutManager, $
	           inherits MrLayoutAtom, $
	           inherits MrIDL_Container $
	         }
end