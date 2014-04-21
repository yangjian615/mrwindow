; docformat = 'rst'
;
; NAME:
;       MrGrLayout__Define
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
;+
;   The purpose of this class is to keep track of plot positios and the over-all layout
;   of the plotting grid. All interaction with this class should be via the GetPositions
;   and SetPositions methods.
;
;   Plot positions can be of two types: fixed and auto-updating. If a plot is "auto-
;   updating", then it is assigned a location within a 2D plotting grid and is
;   automatically moved and adjusted as more plots are added. If a plot is "fixed", then
;   its position will not change as more plots are added. Plots with fixed position have
;   a location of [-1,N], where N is a number indicating the order in which it was added.
;
; :Examples:
;   EXAMPLE 1: The Basics::
;       ;Make plots at [2,1] and [3,3]. Fill holes, remove extra cols and rows.
;           MyObj = obj_new('MrGrLayout')
;           MyObj -> AddToLayout, [2,1]
;           MyObj -> AddToLayout, [3,3]
;           MyObj -> WhichLayout
;           MyObj -> FillHoles, /TRIMLAYOUT
;           MyObj -> WhichLayout
;           Obj_Destroy, MyObj
;
;   EXAMPLE 2: Complete::
;       ;Create the object and add the next available position to the layout
;           MyObj = obj_new('MrGrLayout')
;           MyObj -> AddToLayout
;
;       ;Add a specific location to the layout
;           location = [1,2]
;           MyObj -> AddToLayout, location
;
;       ;Add a specific position to the layout
;           position = [0.25, 0.25, 0.75, 0.75]
;           MyObj -> AddToLayout, !Null, position
;
;       ;Get information about the plots that have been added
;           MyObj -> whichLayout
;
;       ;Remove the fixed position from the layout
;           MyObj -> RemoveFromLayout, [-1, 1]
;
;       ;Add a column to the layout
;           MyObj -> ExpandLayout, 1, 0
;
;       ;Remove a layout position
;           MyObj -> RemoveFromLayout, [1,1]
;
;       ;Change aspects of the plot layout::
;           MyObj -> SetProperty, XMARGIN=[10,4], YMARGIN=[4,2], XGAP=3, YGAP=0
;
;       ;Remove location [1,2]. Replace it with location [2,2]. Return new position.
;           MyObj -> SetPosition, [1,2], [2,2], OUTPOSITION=position
;
;       ;Move location [2,2] from the layout into a fixed position, making [2,2] avaialble.
;           MyObj -> SetPosition, [2,2], /TOFIXED, OUTPOSITION=outPos, OUTLOCATION=outLoc
;
;       ;Move a fixed position into the layout at location [1,1]
;           MyObj -> SetPosition, [-1,1], [1,1], OUTPOSITION=outPos
;
;       ;Move location [1,1] to a specific, fixed location, making [1,1] available
;           SetPosition = [0.25, 0.25, 0.75, 0.75]
;           MyObj -> SetPosition, [1,1], SetPosition, OUTLOCATION=outLoc
;
;       ;Change the layout from [nCols,nRos]=[1,2] to [2,1]
;           MyObj -> SetProperty, LAYOUT=[2,1]
;
;       ;Destroy the object
;           Obj_Destroy, MyObj
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
;	Modification History::
;       04/26/2013  -   Written by Matthew Argall
;       05/05/2013  -   Added the CALCULATE keyword so that the positions can be updated
;                           with the layout. - MRA
;       05/17/2013  -   Extracted layout management methods from MrPlot__define and put
;                           them here: the FillHoles, FindLayoutHoles, GetListIndexAvailability,
;                           Make_Location, PlotExists, setPositions, shiftPlots, and 
;                           renumber methods. - MRA
;       05/18/2013  -   Major rewrite of the SetPositions method. - MRA
;       05/20/2013  -   Added keywords REMOVE and CLEAR to the SetPositions method. Lots
;                           of bug fixes. - MRA
;       05/27/2013  -   The /ADD keyword now functions properly in the SetPositions
;                           method. Locations and positions be specified with /ADD now,
;                           as well. - MRA
;       06/19/2013  -   Since interaction with this class should be through the
;                           SetPositions method, not the SetProperty method, within the
;                           former an explicit interal call is made to 
;                           MrGrLayout::SetProperty so that subclasses do not get into
;                           in infinite loop in their SetProperty method when they call
;                           SetPositions. Also, the number of plots is updated as
;                           positions and locations are added/removed. Lastly, locations
;                           and positions are added to the plot_locations and PLOT_POSITIONS
;                           properties before the layout is updated, in the SetPositions
;                           method. As such, a call to the FillHoles method was not
;                           working. Now, the layout is not adjusted when /ADD is set. - MRA
;       07/07/2013  -   Separated the SetPositions method into the Add, Remove, Replace,
;                           and Clear methods. Added the whichLayout method. - MRA
;       07/09/2013  -   Reduced the default YGAP size to 6. - MRA
;       08/19/2013  -   Renamed AddPositions to AddToLayout, ApplyPositions to UpdateLayout, 
;                           CalcPositions to CalcLayoutPositions, ClearPositions to
;                           ClearLayout. - MRA
;       08/20/2013  -   Changed Plot_Locations to posIsTaken and Plot_Positions to 
;                           Fixed_Positions in the class defninition. Removed the
;                           UpdateLayout method. Renamed ReNumber to RePosition. Added
;                           the AddRows and AddCols methods. Make sure positions are
;                           re-positioned in SetProperty if the layout changes. - MRA
;       08/21/2013  -   Renamed GetMaxLocation to GetListIndexAvailability and repurposed
;                           it to return the plot indices and number of un/available
;                           positions. RePosition can now accept individual indices. Added
;                           the ConvertLocation method. Print a grid of 1's and 0's
;                           indicating if a spot is available or unavailable. Added
;                           POSITION keyword to Make_Location. Renamed RemovePositions to
;                           RemoveFromLayout. Combined AddCols and AddRows into the
;                           ExpandLayout method.
;       08/22/2013  -   Plot location are now being converted properly. ReplacePositions
;                           is now working properly. Added FindFixedLocation method. - MRA
;       08/23/2013  -   Added MakeFixedLocation and removed GetPositions. - MRA
;       08/24/2013  -   Added FillHoles and TrimLayout methods. - MRA
;       08/28/2013  -   Moved the functionality of GetListIndexAvailability into
;                           IsAvailable. Removed the former. ReplacePositions has been
;                           re-written and renamed to SetPosition. - MRA
;       09/24/2013  -   Calling SetProperty with the LAYOUT keyword now works. Check if
;                           the new layout is large enough to fit all of the plots. The
;       2013/11/17  -   Added the CHARSIZE property. - MRA
;       2013/11/24  -   Inherit MrLayout__Define. Renamed from MrPlotLayout__Define to
;                           MrGrLayout__Define. Changed LAYOUT property to GRLAYOUT. - MRA
;       2013/11/29  -   Array and plot indices are now row-order. MakeFixedLocation and
;                           Make_Location both return layouts as [ncol, nrow, index] so
;                           that they can be directly passed to MrLayout. Plot indices
;                           are now the primary means of identifying plots within the
;                           current layout. IsAvailable accepts values outside of the
;                           current layout so still uses invariant [col,row] locations. - MRA
;       2014/02/07  -   Make_Location was returning the wrong plot index in cases where
;                           an extra row was added to the layout. Fixed. - MRA
;       2014/04/03  -   Return scalars when converting locations, if possible. - MRA
;       2014/04/04  -   Trim layout was finding the max [col,row] incorrectly. Fixed. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to add new positions and locations to the plotting
;   grid. This simulates adding an actual plot to the display.
;
; :Params:
;       LAYOUT:         in, out, optional, type=int/intarr(3)
;                       A scalar plot "index" or a vector of the form [ncols, nrows, index],
;                           where [ncols, nrows] is the number of columns and rows in the
;                           plotting grid. "Index" is the plot index, starting with 1 and
;                           increasing first right then down, of the location to be
;                           created. If [ncols,nrows] cannot accommodate all of the plots
;                           within the layout, it will be increased until it can. The final
;                           layout will be returned in `OLAYOUT`. If a scalar is provided,
;                           [ncols,nrows] is taken to be the current grid layout. If
;                           undefined, a LAYOUT will be returned.
;       POSITION:       in, out, optional, type=fltarr(4)
;                       The position of the plot indicated by `LOCATION`. 
;                           The lower-left and upper-right corners of a plot in normal
;                           coordinates. If not provided, then one will be generated
;                           and returned.
;
; :Keywords:
;       OLAYOUT:        out, optional, type=intarr(3)
;                       If `LAYOUT` was provided and the grid layout needed to change in
;                           order to accommodate it, then this will contain the new
;                           layout: [ncols, nrows, index].
;       UPDATE_LAYOUT:  in, optional, type=boolean, default=1
;                       Set to 0 to simulate adding a position to they layout. In this
;                           case, an available location and position will be found, but
;                           the actual layout will remain unchanged. The default is to
;                           (find and) assimilate `LOCATION` and `POSITION` into the
;                           layout.
;-
pro MrGrLayout::AddToLayout, layout, position, $
OLAYOUT=oLayout, $
UPDATE_LAYOUT=update_layout
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    init_grid = self.GrLayout
    if n_elements(update_layout) eq 0 $
        then update_layout = 1 $
        else update_layout = keyword_set(update_layout)
        
    case n_elements(layout) of
        0: ;Do nothing
        1: oLayout = [self.GrLayout, layout]
        3: oLayout = layout
        else: message, 'LAYOUT: incorrect number of elements.'
    endcase
        
    if n_elements(oLayout) gt 0 && oLayout[2] lt 0 then $
        message, 'To add a fixed position, provide POSITION. LAYOUT will be returned.'
        
;---------------------------------------------------------------------
;Get a Location? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If a position was provided, get a fixed location
    if n_elements(layout) eq 0 and n_elements(position) gt 0 then begin
        ;Create the location
        layout = self -> MakeFixedLocation()
        if update_layout eq 0 then return
        
        ;Add to the layout
        self.nFixed += 1
        *self.fixed_positions = [[*self.fixed_positions], [position]]

;---------------------------------------------------------------------
;Get a Position? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If an auto-updating location was provided
    endif else begin
        
        ;Make room for the location given.
        self -> Make_Location, oLayout, $
                               POSITION=position, $
                               UPDATE_LAYOUT=update_layout
        if n_elements(layout) eq 0 then layout = oLayout
        if update_layout eq 0 then return
        
        ;Add them to the layout
        aIndex = self -> ConvertLocation(oLayout[2], /PINDEX, /TO_AINDEX)
        (*self.posIsTaken)[aIndex] = 1
        
    endelse
    
    ;Increase the plot count
    self.nPlots += 1
end


;+
;   The purpose of this method is to calculate the plot positions.
;
;   Over-ride the super-class method to return all positions within the layout and
;   store them as an object property.
;-
pro MrGrLayout::CalcPositions
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Return if nothing has been added to the layout.
    if array_equal(self.GrLayout, [0,0]) then return
    
    ;Make sure a valid layout is present
    if self.GrLayout[0] eq 0 xor self.GrLayout[1] eq 0 $
        then message, 'Cannot calculate position. Layout must have at least ' + $
                      'one column and one row.'

    ;Calculate positions
    *self.layout_positions = MrLayout(self.GrLayout, $
                                      ASPECT    = *self.aspect, $
                                      CHARSIZE  =  self.charsize, $
                                      OXMARGIN  =  self.oxmargin, $
                                      OYMARGIN  =  self.oymargin, $
                                      P_REGION  =       p_region, $
                                      XGAP      = *self.xgap, $
                                      IXMARGIN  =  self.ixmargin, $
                                      YGAP      = *self.ygap, $
                                      IYMARGIN  =  self.iymargin)
    
    ;Save the window and region
    self.x_region = p_region[[0,2]]
    self.y_region = p_region[[1,3]]
end


;+
;   The purpose of this method is to clear the lists of positions and locations.
;
; :Keywords:
;       FIXED:          in, optional, type=boolean
;                       If set, only the fixed positions will be cleared.
;       LAYOUT:         in, optional, type=boolean
;                       If set, only the layout positions will be cleared.
;-
pro MrGrLayout::ClearLayout, $
FIXED=fixed, $
LAYOUT=layout
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Defaults
    fixed = keyword_set(fixed)
    layout = keyword_set(layout)
    if fixed + layout eq 0 then begin
        fixed = 1
        layout = 1
    endif
    
    ;Clear fixed positions.
    if fixed eq 1 then begin
        ptr_free, self.fixed_positions
        self.fixed_positions = ptr_new(/ALLOCATE_HEAP)
        self.nPlots -= self.nFixed
        self.nFixed = 0
    endif
    
    ;Clear layout positions
    if layout eq 1 then begin
        ptr_free, self.layout_positions
        ptr_free, self.posIsTaken
        
        self.layout_positions = ptr_new(/ALLOCATE_HEAP)
        self.posIsTaken       = ptr_new(/ALLOCATE_HEAP)
        
        nLayout = self.nPlots - self.nFixed
        self.nPlots -= self.nFixed
        self.GrLayout = [0,0]
    endif
end


;+
;   The purpose of this method is to convert from a [col, row] plot location to a
;   plot-index location, and vice versa.
;
; :Params:
;       LOCATION:       in, required, type=intarr(2)
;                       The [col, row] location of a plot to be converted.
;                           If LOCATION[0] < 0, then it is the location of a fixed
;                           position. In this case, the list index will be returned.
;       LAYOUT:         in, optional, type=intarr(2), default=self.GrLayout
;                       The number of columns and rows in the layout: [ncols, nrows].
;
; :Keywords:
;       EXISTS:         out, optional, type=boolean
;                       Returns 1 if `LOCATION` exists within `LAYOUT` and 0 otherwise.
;                           If EXISTS is 0 and no named variable is present, an error 
;                           will occur.
;       COLROW:         in, optional, type=boolean
;                       Indicate that `LOCATION` is a [col,row] location. This is assumed.
;       AINDEX:         in, optional, type=boolean, default=0
;                       Indicate that `LOCATION` is actually an array-index.
;       PINDEX:         in, optional, type=boolean, default=0
;                       Indicate that `LOCATION` is actually a plot-index.
;       TO_COLROW:      in, optional, type=boolean
;                       Indicate that `LOCATION` is to be converted to a [col, row] location.
;       TO_AINDEX:      in, optional, type=boolean
;                       Indicate that `LOCATION` is to be converted to an array-index.
;                           This is assumed.
;       TO_PINDEX:      in, optional, type=boolean, default=0
;                       Indicate that `LOCATION` is to be converted to a plot-index.
;       
;
; :Returns:
;       RESULT:         The result of the convertion.
;-
function MrGrLayout::ConvertLocation, location, layout, $
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
        return, MrNull(-1)
    endif

;---------------------------------------------------------------------
;Defaults ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
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
    if aIndex + pIndex + colrow eq 0 then colrow = 1
    if aIndex + pIndex + colrow ne 1 then $
        message, 'AINDEX, PINDEX, and COLROW cannot be specified together.'
    
    ;Default to converting to a list index.
    if to_pIndex + to_aIndex + to_colrow eq 0 then to_aIndex = 1
    if to_pIndex + to_aIndex + to_colrow ne 1 then $
        message, 'One and only one of TO_PINDEX, TO_AINDEX, and TO_COLROW are allowed.'
    
    ;Make sure the location exists
    exists = self -> PlotExists(location, layout, PINDEX=pIndex, AINDEX=aIndex)
    if max(exists) eq 0 then if arg_present(exists) $
        then return, MrNull(-1) $
        else message, 'Location does not exist within layout. Cannot convert.'

    ;How many locations were given?
    nLocations = n_elements(location)
    if colrow then nLocations = nLocations / 2
;---------------------------------------------------------------------
;Convert Fixed Location //////////////////////////////////////////////
;---------------------------------------------------------------------
    if location[0] lt 0 then begin
        nLoc = n_elements(location)
        
        ;TO PLOT INDEX
        if to_pIndex then begin
            case 1 of
                aIndex: result = -location - 1
                pIndex: result = location
                colrow: result = -location[1,*]
            endcase
    
        ;TO [COL, ROW]
        endif else if to_colrow then begin
            case 1 of
                aIndex: result = transpose([replicate(-1, nLoc), [location+1]])
                pIndex: result = transpose([replicate(-1, nLoc), [-location]])
                colrow: result = location
            endcase
          
        ;TO ARRAY INDEX
        endif else if to_aIndex then begin
            case 1 of
                aIndex: result = location
                pIndex: result = -location - 1
                colrow: result = -location[1,*] + 1
            endcase
        endif
        
        return, result
    endif

;---------------------------------------------------------------------
;Convert Layout Location /////////////////////////////////////////////
;---------------------------------------------------------------------

    ;TO PLOT INDEX
    if to_pIndex then begin
        case 1 of
            aIndex: result = location + 1
            pIndex: result = location
            colrow: result = layout[0]*(location[1,*]-1) + location[0,*]
        endcase
    
    ;TO [COL, ROW]
    endif else if to_colrow then begin
        case 1 of
            aIndex: result = array_indices(layout, location,   /DIMENSIONS) + 1
            pIndex: result = array_indices(layout, location-1, /DIMENSIONS) + 1
            colrow: result = location
        endcase
          
    ;TO ARRAY INDEX
    endif else if to_aIndex then begin
        case 1 of
            aIndex: result = location
            pIndex: result = location - 1
            colrow: result = layout[0]*(location[1,*]-1) + location[0,*] - 1
        endcase
    endif

    ;Return a scalar? -- Only for AINDEX and PINDEX
    if nLocations eq 1 then if to_colrow eq 0 then result = result[0]
    return, result
end


;+
;   The purpose of this method is to add rows to the layout.
;
; :Params:
;       NCOLS:          in, optional, type=integer, default=1
;                       Add this number of rows to the layout.
;       NROWS:          in, optional, type=integer, default=1
;                       Add this number of rows to the layout.
;-
pro MrGrLayout::ExpandLayout, nCols, nRows
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    if n_elements(nCols) eq 2 then nRows = nCols[1]

    ;Default to adding one row
    if n_elements(nCols) eq 0 then nCols = 1
    if n_elements(nRows) eq 0 then nRows = 1
    if nRows eq 0 && nCols[0] eq 0 then return

    ;Resposition all of the existing plots        
    self -> SetProperty, LAYOUT=self.GrLayout+[nCols[0], nRows]
end


;+
;   Adjust the layout of the plots by moving plots to fill in gaps and then
;   removing any empty columns or rows.
;
; :Keywords:
;       TRIMLAYOUT:             in, optional, type=boolean, default=0
;                               If set, empty rows and columns will be removed from the
;                                   layout.
;-
pro MrGrLayout::FillHoles, $
TRIMLAYOUT=trimLayout
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Get a vector of availability in plot-index order
    tf_available = self -> IsAvailable(IFREE=iFree, NFREE=nFRee, ITAKEN=iTaken, NTAKEN=nTaken)
    if tf_available eq !Null then return
    if nFree eq 0 then return
    
    ;Make an array indicating which locations are taken (1) and free (0). Make the last
    ;NFREE elements free.
    nTotal = self.GrLayout[0]*self.GrLayout[1]
    iFilledTaken = bytarr(nTotal) + 1B
    iFilledTaken[nTotal-nFree:nTotal-1] = 0B

    ;Update the array that indicates which plots are taken
    *self.posIsTaken = iFilledTaken
    
    ;Get rid of excess columns and rows?
    if keyword_set(trimLayout) then self -> TrimLayout
end


;+
;   The purpose of this method is to find the [col,row] location of a plot given its
;   position. For plots in fixed positions, col=-1.
;
; :Params:
;       POSITION:       in, required, type=flarr(4)
;                       A position for which the location is to be found.
;
; :Keywords:
;       EPSILON:        in, optional, type=float, default=1e-5
;                       The error by which two positions are equal. On the first pass,
;                           and exact match is saught. If not found, if two positions
;                           are within EPSILON of one another, the matching `LOCATION`
;                           will be returned.
;       FIXED:          in, optional, type=boolean
;                       If set, only fixed positions will be searched. If set to 0, only
;                           layout positions will be searched. If undefined, both fixed
;                           and layout positions will be searched.
;       NFOUND:         out, optional, type=integer
;                       A named variable into which the number of found locations will
;                           be returned.
;
; :Returns:
;       LOCATION:       The [col, row] location of `POSITION`. If no match is found,
;                           [0,0] is returned.
;-
function MrGrLayout::FindLocation, position, $
EPSILON = epsilon, $
FIXED = fixed, $
NFOUND = nFound
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, [0,0]
    endif

    if n_elements(epsilon) eq 0 then epsilon = 1e-5
    
    ;If fixed is not defined, search through both fixed and layout positions
    if n_elements(fixed) eq 0 then fixed = 2 else fixed = keyword_set(fixed)

    ;Keep track of the number of matches and the matched locations. NFOUND also
    ;serves as an index into LOCATION for new matches.
    nFound = 0
    location = intarr(2,self.nPlots)

;---------------------------------------------------------------------
;Search Fixed Positions? /////////////////////////////////////////////
;---------------------------------------------------------------------
    if fixed ne 0 then begin
        ;Step through all of the fixed positions
        for i = 0, self.nFixed - 1 do begin
        
            ;Look for matches
            if array_equal(position, (*self.fixed_positions)[*,i]) then begin
                location[*,nFound] = [-1, i+1]
                nFound += 1
            endif
        endfor
    endif
    
;---------------------------------------------------------------------
;Search Layout Positions? ////////////////////////////////////////////
;---------------------------------------------------------------------
    if fixed ne 1 then begin
        ;Step through all of the layout positions
        for i = 0, self.GrLayout[0]*self.GrLayout[1] - 1 do begin
        
            ;If the layout position is not taken, skip it
            if (*self.posIsTaken)[i] eq 0 then continue

            ;Look for a match.
            if array_equal(position, (*self.layout_positions)[*,i]) then begin
                location[*,nFound] = self -> ConvertLocation(i, /AINDEX, /TO_COLROW)
                nFound += 1
            endif
        endfor
    endif

;---------------------------------------------------------------------
;Close Enough ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if nFound eq 0 then begin
        ;Fixed Positions
        if fixed ne 0 and self.nFixed gt 0 then begin
            ;Calculate the difference between the stored positions and the given position.
            ;Rebin the position NFIXED times to save a FOR loop.
            delta_fixed = (*self.fixed_positions) - rebin(position, 4, self.nFixed)
        
            ;Find the almost-equal case.
            index = where(abs(mean(delta_fixed, DIMENSION=1)) le epsilon, nMatch)
            nFound += nMatch
        
            ;Return the location of the match. Fixed positions take the form [-1, index].
            ;There is one per match.
            if nMatch gt 0 then $
                location[*,nFound:nFound+nMatch-1] = [replicate(-1, 1, nMatch), [index+1]]
        endif
        
        ;Layout positions
        ;
        ;   NOT IMPLEMENTED YET
        ;
        nLayout = self.nPlots - self.nFixed
        if fixed ne 1 and nLayout gt 0 then begin
            delta_layout = (*self.layout_positions) - rebin(position, 4, self.nPlots-self.nFixed)
            void = self -> IsAvailable(ITAKEN=iTaken, NTAKEN=nTaken)
        endif
    endif
    
    return, location[*,nFound-1]
end


;+
;   The purpose of this method is to obtain the position of a plot given its
;   plot index location.
;
; :Params:
;       PINDEX:             in, required, type=integer
;                           The plot index of the plot whose position is to be returned.
;                               Fixed locations have a negative plot index.
;
; :Returns:
;       POSITION:           The position of the plot at `PINDEX`.
;-
function MrGrLayout::GetPosition, pIndex
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, !Null
    endif
    
    ;Ensure a single plot index was given.
    if n_elements(pIndex) ne 1 then message, 'PINDEX must be a scalar.'
    
    ;Make sure it exists.
    exists = self -> PlotExists(pIndex, /PINDEX)
    if exists eq 0 then message, 'LOCATION does not exist. Cannot get its position.'
    
    ;Convert to an array index
    aIndex = self -> ConvertLocation(pIndex, /PINDEX, /TO_AINDEX)
    
    ;Get the position
    if pIndex[0] lt 0 $
        then position = (*self.fixed_positions)[*, aIndex] $
        else position = (*self.layout_positions)[*, aIndex]
    
    return, position
end


;+
;   The purpose of this method is to retreive object properties.
;
; :Keywords:
;       LAYOUT:         out, required, type=intarr(2)
;                       A 2 element vector specifying the number of plots in the vertical
;                           and horizontal directions, [ncols, nrows].
;       LOCATION:       out, optional, type=fltarr(4)
;                       The position of the plot specified by `THISLOCATION`
;       PINDEX:         in, optional, type=int
;                       The plot index within the grid (starting with 1) of the plot whose
;                           position is to be returned.
;       _REF_EXTRA:     out, optional, type=any
;                       Any keyword accepted by MrLayout::GetProperty is also accepted
;                           for keyword inheritance.
;-
pro MrGrLayout::GetProperty, $
 LAYOUT = layout, $
 LOCATION = location, $
 PINDEX = pIndex, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Superclass
    if n_elements(extra) gt 0 then self -> MrLayout::GetProperty, _STRICT_EXTRA=extra
    
    ;Get Properties
    if arg_present(layout)   then layout   = self.GrLayout
    if arg_present(nplots)   then nplots   = self.nplots
    if arg_present(location) then $
        if n_elements(pIndex) gt 0 and n_elements(*self.layout_positions) ne 0 $
            then location = (*self.layout_positions)[*, pIndex]
end


;+
;   The purpose of this method is to provide a means of generating new plot locations
;   within the 2D plotting grid.
;
;   A [col,row] location is required in case it lies outside of the current layout.
;   [col,row] locations are independent of the grid layout.
;
; :Params:
;       COLROW:             in, optional, type=long
;                           The [col,row] location of the position that is being checked.
;                               If COLROW[0] < 0 then the fixed positions will be checked
;                               for availability. If not given, availability for all plots
;                               will be returned.
;
; :Keywords:
;       IFREE:              out, optional, type=intarr
;                           The array-index of each available layout position. Returned only
;                               if `PINDEX_IN` is not provided.
;       INSIDE:             in, optional, type=boolean, default=0
;                           Look only inside the plot layout. Normally, if `PINDEX_IN` lies
;                               outside the plot layout, `TF_AVAILABLE` will return True.
;                               If `INSIDE` is set, `TF_AVAILABLE` will return false.
;       ITAKEN:             out, optional, type=intarr
;                           The array-index of each unavailable layout position. Returned only
;                               if `LOCATION` is not provided.
;       NFREE:              out, optional, type=intarr
;                           The number of available layout positions. Returned only if
;                               `LOCATION` is not provided.
;       NTAKEN:             out, optional, type=intarr
;                           The number of unavailable layout positions. Returned only if
;                               `LOCATION` is not provided.
;       PINDEX:             in, optional, type=boolean, default=0
;                           If set and `LOCATION` is not provided, `ITAKEN` and `IFREE`
;                               will be the plot-indices of the unavailable and available
;                               positions, respectively.
;
; :Returns:
;       TF_AVAILABLE:       1     if the position indicated by `LOCATION` is avaialble,
;                           0     if not.
;                           !Null if no layout has been established.
;-
function MrGrLayout::IsAvailable, colrow, $
IFREE = iFree, $
INSIDE = inside, $
ITAKEN = iTaken, $
NFREE = nFree, $
NTAKEN = nTaken, $
PINDEX = pIndex
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, MrNull(-1)
    endif
    
    ;Defaults
    inside = keyword_set(inside)
    pIndex = keyword_set(pIndex)
       
    ;Number of possible plots in the layout.
    nLayout = self.GrLayout[0]*self.GrLayout[1]
    
;---------------------------------------------------------------------
;Availability of All Positions? //////////////////////////////////////
;---------------------------------------------------------------------
    if n_params() eq 0 then begin
        ;Has a layout been defined yet?
        if nLayout le 0 then begin
            nFree = 0
            nTaken = 0
            return, 0
        endif
    
        ;Check if the grid location is available.
        tf_available = ~*self.posIsTaken
        
        ;Find the indices of the available and unavailable grid locations 
        iFree = where(tf_available eq 1, nFree, COMPLEMENT=iTaken, NCOMPLEMENT=nTaken, /NULL)

        ;Convert from array-index order to plot-index order.
        if pIndex then begin
            ;Convert array indices to plot indices.
            if nFree  gt 0 then iFree  = self -> ConvertLocation(iFree,  /AINDEX, /TO_PINDEX)
            if nTaken gt 0 then iTaken = self -> ConvertLocation(iTaken, /AINDEX, /TO_PINDEX)
        endif
        
        return, tf_available
    endif

;---------------------------------------------------------------------
;Specific Positions? /////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Fixed locations.
    if colrow[0] lt 0 then begin
        ;Fixed locations are adjacent and numbered from 1 to nFixed
        tf_available = colrow[1] gt self.nFixed
    
    ;Layout locations.
    endif else begin

        ;If COLROW is outside of the layout, then the location is available
        ;(unless INSIDE is set).
        if self -> PlotExists(colrow) eq 0 then $
            if inside eq 1 then return, 0 else return, 1
    
        ;Check if the position has been taken already
        aIndex = self -> ConvertLocation(colrow, /COLROW, /TO_AINDEX)
        tf_available = ~(*self.posIsTaken)[aIndex]
    endelse
    
    return, tf_available
end


;+
;   The purpose of this program is get the next available location for a fixed position.
;
; :Returns:
;       LOCATION:           out, required, type=intarr(3)
;                           A vector of the form [ncols, nrows, index], where [ncols, nrows]
;                               is the number of columns and rows in the plotting grid.
;                               Index is the next available fixed plot index location.
;-
function MrGrLayout::MakeFixedLocation
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, MrNull(-1)
    endif
    
    ;Get a new location
    location = [self.GrLayout, -self.nFixed-1]
    
    ;Output the location
    return, location
end


;+
;       The purpose of this program is to determine where a plot should be displayed
;       on the plotting grid::
;           - If a plot already exists in the given location, then all plots are
;               shifted out of the way to make room.
;           - If a location was not specified, the lowest available plot location will
;               be filled.
;           - An extra row will be added to make room, if necessary.
;
; :Params:
;       LAYOUT:             in, out, optional, type=lonarr(3)
;                           A vector of the form [ncols, nrows, index], where [ncols, nrows]
;                               is the number of columns and rows in the plotting grid.
;                               Index is the plot index, starting with 1 and increasing
;                               first right then down, of the location to be created. If
;                               not provided, a LAYOUT will be returned.
;
; :Keywords:
;       UPDATE_LAYOUT:      in, optional, type=boolean, default=0
;                           If set, the plot layout will be adjusted to accomodate the
;                               location and position being retrieved. Plot positions
;                               will be recalculated to fit the new layout.
;-
pro MrGrLayout::Make_Location, layout, $
UPDATE_LAYOUT = update_layout, $
POSITION = position
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

;---------------------------------------------------------------------
;Check Initial Condition /////////////////////////////////////////////
;---------------------------------------------------------------------
    n_loc = n_elements(layout)
    update_layout = keyword_set(update_layout)

    ;SELF.GRLAYOUT is initialized to [0,0], which is an invalid plot location. If this
    ;is still the case, and LOCATION was not given, then make sure location is defined.
    if array_equal(self.GrLayout, [0,0]) && n_loc eq 0 then begin
        layout = [1,1,1]
        n_loc = 3
    endif

;---------------------------------------------------------------------
;Location Provided ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_loc eq 3 then begin    
        ;Convert to a [col,row] location
        colrow = self -> ConvertLocation(layout[2], layout[0:1], /PINDEX, /TO_COLROW)
        
        ;Check if it is available
        tf_free = self -> IsAvailable(colrow)

    ;---------------------------------------------------------------------
    ;Location is Taken? //////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        ;
        ;If the location is already available, then the only thing we have to check
        ;is the grid layout. If the location is unavailable (TF_FREE=0), then we
        ;also have to shift plots out of the way.
        ;
        if tf_free eq 0 then begin
            ;Shift plots out of the way?
            if keyword_set(update_layout) then begin
                self -> ShiftPlots, layout[2]
                
                ;Get the plot index in case the layout changed.
                pIndex = self -> ConvertLocation(colrow, /COLROW, /TO_PINDEX)
                layout = [self.GrLayout, pIndex]
                
            ;If not...
            endif else begin
                ;Get the index of all available locations.
                void = self -> IsAvailable(IFREE=pFree, NFREE=nFree, /PINDEX)
                
                ;Plots have to be shifted out of the way. If there are no free
                ;locations beyond the desired location, we must add a row. We must then
                ;find the plot index in the new layout.
                if nFree eq 0 || (max(pFree gt layout[2]) eq 0) then begin
                    gridLayout = self.GrLayout + [0,1]
                    pIndex = self -> ConvertLocation(colrow, gridLayout, /COLROW, /TO_PINDEX)
                    layout = [gridLayout, pIndex]
                endif
            endelse
        endif

    ;---------------------------------------------------------------------
    ;Change the Grid Layout? /////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if array_equal(self.GrLayout, layout[0:1]) eq 0 then begin
            ;Get the index of all taken locations
            void = self -> IsAvailable(ITAKEN=iTaken, NTAKEN=nTaken)
            
            ;If there are some
            if nTaken gt 0 then begin
                ;Find the largest column and row containing a plot
                crTaken = self -> ConvertLocation(iTaken, layout[0:1], /AINDEX, /TO_COLROW)
                crTaken = [max(colrow[0,*]), max(colrow[1,*])]
                
                ;Find a layout that includes all of the plots yet is closest to the
                ;desired location.
                gridLayout = crTaken > layout[0:1]
                
            ;If all locations are available, use the desired grid layout
            endif else gridLayout = layout[0:1]
            
            ;Get the plot index in the potentially new layout
            pIndex = self -> ConvertLocation(colrow, gridLayout, /COLROW, /TO_PINDEX)
            layout = [gridLayout, pIndex]
            
            ;Update the grid layout?
            if update_layout then self -> SetProperty, LAYOUT=gridLayout
        endif
    
;---------------------------------------------------------------------
;Location Needed /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else if n_loc eq 0 then begin
        ;Get availability 
        void = self -> IsAvailable(IFREE=pFree, NFREE=nFree, ITAKEN=pIndex, NTAKEN=nTaken, /PINDEX)
        
    ;---------------------------------------------------------------------
    ;A Location is Available? ////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if nFree gt 0 then begin
            ;Take the lowest available plot location
            layout = [self.GrLayout, pFree[0]]
            
    ;---------------------------------------------------------------------
    ;No Locations Available? /////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endif else begin
            ;Add another row
            ;   Update the layout?
            if keyword_set(update_layout) then begin
                self -> ExpandLayout, 0, 1
                pIndex = self -> ConvertLocation([1,self.GrLayout[1]], /COLROW, /TO_PINDEX)
                layout = [self.GrLayout, pIndex]
            endif else begin
                layout = [self.GrLayout + [0,1], 0]
                pIndex = self -> ConvertLocation([1,layout[1]], layout[0:1], /COLROW, /TO_PINDEX)
                layout[2] = pIndex
            endelse
        endelse
        
    ;otherwise, throw an error message
    endif else message, 'LOCATION: incorrect number of elements.'

;---------------------------------------------------------------------
;Return Associated Position? /////////////////////////////////////////
;---------------------------------------------------------------------
    if arg_present(position) then begin
        ;Was the layout updated?
        if keyword_set(update_layout) then begin
            aIndex = self -> ConvertLocation(layout[2], layout[0:1], /PINDEX, /TO_AINDEX)
            position = (*self.layout_positions)[*, aIndex]
        endif else begin
            position = MrLayout(layout, ASPECT=*self.aspect, CHARSIZE=self.charsize, $
                                OXMARGIN=self.oxmargin, IXMARGIN=self.ixmargin, XGAP=*self.xgap, $
                                OYMARGIN=self.oymargin, IYMARGIN=self.iymargin, YGAP=*self.ygap)
        endelse
    endif
end


;+
;   Determine whether a location will fit inside the given layout.
;
; :Params:
;       LOCATION:       in, required, type=intarr(2\,*)
;                       the 1 based plot location [col, row] at which to check 
;                           for existence.
;       LAYOUT:         in, optional, type=intarr(2), default=current layout
;                       The layout used to check if `LOCATION` exists.
;
; :Keywords:
;       PINDEX:         in, optional, type=boolean, default=0
;                       If set, `LOCATION` is a vector of 1D plot locations. In this case,
;                           the upper left plot is index 1, and the index number increases
;                           first downward, then across.
;
; :Returns:
;       EXISTS:         Tells whether the plot exists (1) or not (0).
;-
function MrGrLayout::PlotExists, location, layout, $
AINDEX = aIndex, $
PINDEX = pIndex
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, MrNull(0)
    endif
    
    ;Default to current layout
    if n_elements(layout) eq 0 then layout = self.GrLayout
    pIndex = keyword_set(pIndex)
    aIndex = keyword_set(aIndex)

    ;Layout Positions
    if location[0] ge 0 then begin
        ;The position does not exist if it is outside the layout
        case 1 of
            aIndex: exists = location le layout[0]*layout[1] - 1
            pIndex: exists = location le layout[0]*layout[1]
            else:   exists = (location[0,*] le layout[0]) and (location[1,*] le layout[1])
        endcase
    
    ;Fixed Positions
    endif else begin
        case 1 of
            aIndex: exists = -location le self.nFixed-1
            pIndex: exists = -location le self.nFixed
            else:   exists = location[1,*] le self.nFixed
        endcase
    endelse
    
    return, exists
end


;+
;   The purpose of this method is to remove locations and positions from the 2D plotting
;   grid.
;
; :Params:
;       PINDEX:         in, required, type=intarr
;                       Plot index of the plot to remove.
;
; :Keywords:
;       FILLHOLES:      in, optional, type=boolean, default=0
;                       Sometimes removing a plot from the 2D auto-updating grid can leave
;                           holes in the layout. Set this keyword to 1 (one) to adjust
;                           plot locations so that those holes are filled.
;-
pro MrGrLayout::RemoveFromLayout, pIndex, $
FILLHOLES = fillHoles
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    if n_elements(pIndex) ne 1 then message, 'PINDEX must be a scalar plot index.'
    
    ;Defaults
    fillHoles = keyword_set(fillHoles)
    nRemove = n_elements(pIndex)
    
    ;Check if a position exists at the given location.
    colrow = self -> ConvertLocation(pIndex, /PINDEX, /TO_COLROW)
    tf_free = self -> IsAvailable(colrow)
    if tf_free eq 1 then return

;---------------------------------------------------------------------
;Fixed Positions /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if pIndex lt 0 then begin
        aIndexAll = indgen(self.nFixed)
        aIndex = self -> ConvertLocation(pIndex, /PINDEX, /TO_AINDEX)
        void = isMember(aIndex, aIndexAll, NONMEMBER_INDS=iKeep, N_NONMEMBERS=nKeep)

        if nKeep eq 0 then begin
            self -> ClearLayout, /FIXED
        endif else begin
            self.nFixed -= nRemove
            self.nPlots -= nRemove
            *self.fixed_positions = (*self.fixed_positions)[*,iKeep]
        endelse

;---------------------------------------------------------------------
;Layout Positions ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        aIndexAll = indgen(self.nPlots - self.nFixed)
        
        aIndex = self -> ConvertLocation(pIndex, /PINDEX, /TO_AINDEX)
        void = isMember(aIndex, aIndexAll, NONMEMBER_INDS=iKeep, N_NONMEMBERS=nKeep)
        
        if nKeep eq 0 then begin
            self -> ClearLayout, /LAYOUT
        endif else begin
            self.nPlots -= nRemove
            (*self.posIsTaken)[aIndex] = 0
        endelse
    endelse
    
    ;Fill holes in the layout?
    if fillHoles then self -> FillHoles
end


;+
;   The purpose of this method is to change/replace the position and/or location of a
;   plot that already exists within the 2D plotting grid.
;
; :Params:
;       OLDPINDEX:          in, required, type={1 | 4}-element vector
;                           The plot-index or 4-element position of the plot
;                               whose position is to be changed.
;       NEWLAYOUT:          in, required, type={1 | 3 | 4}-element vector
;                           The plot-index, layout, or 4-element position to where the
;                               plot indicated by `OLD_POSITION` is to be moved. If a
;                               4-element position is provided, the plot will be placed
;                               at a fixed position, outside of the automatically-updating
;                               layout. If a plot-index is provided, it must lie within
;                               the current layout. If a [col, row] location is provided,
;                               the layout will be expanded, if need be, to accommodate
;                               the new position. If "col" is negative, then the new
;                               position will be fixed, outside of the layout.
;
; :Keywords:
;       OUTPOSITION:        out, optional, type=fltarr(4)
;                           The resulting position. If `NEW_POSITION` is a 4-element
;                               position, then `OUTPOSITION` will equal `NEW_POSITION`.
;       OUTLOCATION:        out, optional, type=intarr(2)
;                           The resulting [col, row] location. If `NEW_POSITION` is a 2-
;                               element [col, row] location and "col" is positive, then
;                               `OUTLOCATION` and `NEW_POSITION` will be equal. Under the
;                               same conditions, but "col" being negative, the two may not
;                               be equal.
;       TOFIXED:            in, optional, type=boolean, default=0
;                           If set, the plot indicated by `OLD_POSITION` will be removed
;                               from the auto-updating layout and placed into a fixed
;                               location. Its actual position will not change.
;-
pro MrGrLayout::SetPosition, oldPIndex, newLayout, $
OUTPOSITION = outPosition, $
OUTLOCATION = outLocation, $
TOFIXED = toFixed
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    toFixed = keyword_set(toFixed)

;---------------------------------------------------------------------
;Were Locations or Positions Given? //////////////////////////////////
;---------------------------------------------------------------------

    ;Where is the plot being moved?
    case n_elements(oldPIndex) of
        1: oldPI = oldPIndex
        4: oldPos = old_position
        else: message, 'Incorrect number of elements: OLDPINDEX.'
    endcase

    ;Where is it being moved to?
    case n_elements(newLayout) of
        ;If not being moved to a fixed position, NEWLAYOUT must be defined.
        0: if keyword_set(toFixed) eq 0 then message, 'Incorrect number of elements: NEWLAYOUT.'
    
        ;If a plot-index was given: It must exist within the current layout in order to
        ;know exactly where it is going.
        1: begin
            tf_exist = self -> PlotExists(newLayout, /PINDEX)
            if tf_exist eq 0 $
                then message, 'NEWLAYOUT lies outside the current layout. Provide [ncols, nrows, index] instead.' $
                else newLay = [self.GrLayout, newLayout]
        endcase
        
        ;[ncols, nrows, index] location: If a fixed-location, ensure it is the next available one.
        3: if newLayout[2] gt 0 $
            then newLay = newLayout $
            else newLay = self -> MakeFixedLocation()
        
        ;A position was given.
        4: newPos = newLayout
        else: message, 'Incorrect number of elements: NEWLAYOUT.'
    endcase

;---------------------------------------------------------------------
;Convert OLD_POSITION to a [Col, Row] Location ///////////////////////
;---------------------------------------------------------------------
    
    ;Make sure a plot exists at the old position.
    if n_elements(oldPI) gt 0 then begin
        oldColRow = self -> ConvertLocation(oldPI, /PINDEX, /TO_COLROW)
        isFree = self -> IsAvailable(oldColRow, /INSIDE)
        if isFree eq 1 then message, 'No plot exists at OLDPINDEX. Cannot set its position.'
    endif
        
    ;Turn a position into a [col, row] location. Make sure a plot can be found.
    if n_elements(oldPos) gt 0 then begin
        oldColRow = self -> FindLocation(oldPos, NFOUND=nFound)
        if nFound eq 0 then message, 'No plot exists at OLD_POSITION. Cannot set its position.'
        if nFound gt 1 then message, 'More than one plot was found at OLD_POSITION. Provide a [col, row] location instead.'
    endif
    
    ;Move a plot from an auto-updating position into a fixed position
    if keyword_set(toFixed) then begin
        if oldPI lt 0 then message, 'OLDPINDEX is already a fixed position.'
        oldAIndex = self -> ConvertLocation(oldPI, /PINDEX, /TO_AINDEX)
        void = temporary(newLay)
        newPos = (*self.layout_positions)[*, oldAIndex]
    endif
;---------------------------------------------------------------------
;Set a [Col, Row] Location ///////////////////////////////////////////
;---------------------------------------------------------------------

    ;Remove the old position, add the new one.
    self -> RemoveFromLayout, oldPI
    self -> AddToLayout, newLay, newPos
    
    ;Return the new position and layout location.
    outPosition = newPos
    outLocation = newLay
end


;+
;   If the number of rows and columns change, the index of each plot will refer to a
;   different plot position and a different [col, row] location. This method ensures
;   that a position's [col, row] location is invariant under changes to the layout.
;
; :Private:
;
; :Params:
;
;       OLD_LAYOUT:         in, required, type=lonarr(2)
;                           The old 1 based plotting layout [column, row]
;       NEW_LAYOUT:         in, required, type=lonarr(2)
;                           The new 1 based plotting layout [column, row] into which the
;                               currently stored plot locations will be fitted
;
; :Returns:
;       NEWISTAKEN:         An array indicating which positions are taken within the new
;                               layout scheme.
;-
function MrGrLayout::RePosition, old_layout, new_layout, pIndex
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, !Null
    endif

    ;If the old layout is empty, check if PINDEX fits in the new layout.
    if array_equal(old_layout, [0,0]) then begin
        ;If no plot index was given, then there *should* be nothing to convert.
        if n_elements(plot_index) eq 0 then $
            if n_elements(*self.posIsTaken) eq 0 $
                then return, MrNull(-1) $
                else message, 'OLD_LAYOUT did not match posIsTaken. Report bug.'
    
        if self -> PlotExists(pIndex, new_layout) $
            then return, pIndex $
            else message, 'PINDEX is outside of NEW_LAYOUT. Cannot Re-Position.'
    endif
    
    ;If the new layout is empty, 
    if new_layout[0] eq 0 || new_layout[1] eq 0 $
        then message, 'NEW_LAYOUT is invalid. Must have at least 1 column and 1 row.'
    
    ;Convert a plot index
    if n_elements(pIndex) gt 0 then begin
        old_colrow     = self -> ConvertLocation(pIndex, old_layout, /PINDEX, /TO_COLROW)
        new_plot_index = self -> ConvertLocation(old_colrow, new_layout, /TO_PINDEX)
        return, new_plot_index
    endif
    
    ;Find the indices of the filled positions in the old layout.
    iOldTaken = where(*self.posIsTaken eq 1, nOldTaken)
    
    ;Make a new array for the new layout.
    nNew = new_layout[0]*new_layout[1]
    newIsTaken = bytarr(nNew)
    
    ;If no positions were taken in the old layout, then none are taken in the new one.
    if nOldTaken eq 0 then return, newIsTaken

    ;Convert the indices of the taken positions to [col,row] locations. [col,row]
    ;locations are invariant under change of layout.
    oldTaken_colrow = self -> ConvertLocation(iOldTaken, old_layout, /AINDEX, /TO_COLROW)
    iNewTaken = self -> ConvertLocation(oldTaken_colrow, new_layout, /COLROW, /TO_AINDEX)

    ;Record which positions are taken in the new layout.
    newIsTaken[iNewTaken] = 1
    
    return, newIsTaken
end


;+
;   Use this method to set the value of object properties.
;
; :Keywords:
;       LAYOUT:         in, optional, type=intarr(2)
;                       A 2 element vector specifying the number of plots in the vertical
;                           and horizontal directions, [ncols, nrows].
;       UPDATE_LAYOUT:  in, optional, type=boolean, default=0
;                       This keyword is ingored.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrLayout::SetProperty is also accepted
;                           for keyword inheritance.
;-
pro MrGrLayout::SetProperty, $
LAYOUT = layout, $
UPDATE_LAYOUT = update_layout, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    if n_elements(update_layout) eq 0 $
        then update_layout = 1 $
        else update_layout = keyword_set(update_layout)

    ;Set Properties
    if n_elements(extra) gt 0 then $
        self -> MrLayout::SetProperty, UPDATE_LAYOUT=0, _STRICT_EXTRA=extra

    ;Make sure plot locations are invariant to changes in the layout
    if n_elements(layout) gt 0 then begin
    
        ;Is the new layout empty?
        if array_equal(layout, [0,0]) then begin
            self -> ClearLayout, /LAYOUT
        
        ;If not...
        endif else begin
    
            ;Make sure the new layout will be valid.
            if layout[0] eq 0 xor layout[1] eq 0 then $
                message, 'LAYOUT must have at least 1 column and 1 row.'

            ;Make sure the new layout is big enough.
            void = self -> IsAvailable(ITAKEN=pTaken, NTAKE=nTaken, /PINDEX)
            if nTaken gt 0 then begin
                colrow = self -> ConvertLocation(pTaken, /PINDEX, /TO_COLROW)
                colrow = [max(colrow[0,*]), max(colrow[1,*])]
                
                if (layout[0] lt colrow[0]) || (layout[1] lt colrow[1]) then $
                    message, 'LAYOUT is not big enough to contain the current set of plots. ' + $
                             'Try theObj -> FillHoles, /TRIMLAYOUT to.'
            endif

            ;Resposition all of the existing plots        
            *self.posIsTaken = self -> RePosition(self.GrLayout, layout)
            if MrIsNull(*self.posIsTaken, 'POINTER') then *self.posIsTaken = bytarr(layout[0]*layout[1])

            ;Change the layout
            self.GrLayout = layout
        endelse
    endif
    
    ;Calculate the positions of the plots within the new layout
    if update_layout then self -> CalcPositions
end


;+
;   Shift all plots located at and after LOCATION up one index value.
;
; :Private:
;
; :Params:
;
;       PINDEX:             in, required, type=lonarr(2)
;                           The plot-index location at which to begin shifting plots.
;-
pro MrGrLayout::ShiftPlots, pIndex
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;get the [Col, Row] location of where to start shifting plots and convert it to a
    ;1-based, 1D plot number.
    aIndex = self -> ConvertLocation(pIndex, /PINDEX, /TO_AINDEX)
    
    ;Find the positions that are not yet taken
    iNotTaken = where((*self.posIsTaken)[aIndex:*] eq 0, nNotTaken)

    ;If all positions are taken, add another row to the layout
    if nNotTaken eq 0 then begin
        self -> ExpandLayout, 0, 1
        aIndex = self -> ConvertLocation(pIndex, /PINDEX, /TO_AINDEX)
        iNotTaken = min(where((*self.posIsTaken)[aIndex:*] eq 0))+aIndex
        
    ;Otherwise pick the first available location that is after the given location.
    endif else begin
        iNotTaken = iNotTaken[0]+aIndex
    endelse
    
    ;Make the first available position unavailable, then make the chosen position
    ;available. All positions in between remain unavailable.
    (*self.posIsTaken)[iNotTaken] = 1
    (*self.posIsTaken)[aIndex] = 0
end


;+
;   Remove unused columns and rows.
;-
pro MrGrLayout::TrimLayout
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Get plot availablility.
    tf_free = self -> IsAvailable(/PINDEX, ITAKEN=pTaken, NTAKE=nTaken, NFREE=nFree)
    if nFree  eq 0 then return
    if nTaken eq 0 then return
    
    ;Find the maximum taken column and row
    ColRow = self -> ConvertLocation(pTaken, /PINDEX, /TO_COLROW)
    maxCol = max(ColRow[0,*])
    maxRow = max(ColRow[1,*])

    ;Trim the layout and recalculate the layout positions.
    if maxCol lt self.GrLayout[0] || maxRow lt self.GrLayout[1] $
        then self -> SetProperty, LAYOUT=[maxCol, maxRow]
end


;+
;   The purpose of this method is to describe the layout as well as the locations and
;   positions of the plots currently stored in it.
;-
pro MrGrLayout::whichLayout
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

;---------------------------------------------------------------------
;LAYOUT //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Print information about the overall layout.
    print, ''
    print, '-------LAYOUT-------'
    print, FORMAT='(%"  Layout:    [%i, %i]")', self.GrLayout
    print, FORMAT='(%"  nPlots:    %i")', self.nPlots
    print, FORMAT='(%"  nFixed:    %i")', self.nFixed
    if n_elements(*self.aspect) ne 0 then print, FORMAT='(%"  Aspect:     %f")', *self.aspect
    print, FORMAT='(%"  OXMARGIN:  [%i, %i]")', self.oxmargin
    print, FORMAT='(%"  OYMARGIN:  [%i, %i]")', self.oymargin
    print, FORMAT='(%"  IXMargin:  [%i, %i]")', self.ixmargin
    print, FORMAT='(%"  IYMargin:  [%i, %i]")', self.iymargin
    print, FORMAT='(%"  XGap:      %i")', *self.xgap
    print, FORMAT='(%"  YGap:      %i")', *self.ygap

;---------------------------------------------------------------------
;LAYOUT POSITIONS & LOCATIONS ////////////////////////////////////////
;---------------------------------------------------------------------
    void = self -> IsAvailable(ITAKEN=iTaken, NTAKEN=nTaken)
    if nTaken gt 0 then begin
        pIndex = self -> ConvertLocation(iTaken, /AINDEX, /TO_PINDEX)
        colrow = self -> ConvertLocation(iTaken, /AINDEX, /TO_COLROW)
    endif

    if nTaken gt 0 then begin
        print, ''
        print, '--PINDEX--  --LOCATIONS--             --POSITIONS--'
        for i = 0, nTaken - 1 do begin
            print, FORMAT='(%"     %i         [ %i, %i]      [%6.4f, %6.4f, %6.4f, %6.4f]")', $
                   pIndex[i], colrow[*,i], (*self.layout_positions)[*, iTaken[i]]
        endfor
    endif

;---------------------------------------------------------------------
;FIXED POSITIONS & LOCATIONS /////////////////////////////////////////
;---------------------------------------------------------------------
    if self.nFixed gt 0 then begin
        if nTaken eq 0 then print, '--PINDEX--  --LOCATIONS--             --POSITIONS--'
    
        for i = 0, self.nFixed - 1 do begin
            print, FORMAT='(%"    %2i         [%2i,%2i]      [%6.4f, %6.4f, %6.4f, %6.4f]")', $
                   -i-1, [-1,i+1], (*self.fixed_positions)[*,i]
        endfor
    endif

;---------------------------------------------------------------------
;GRID OF AVAILABILITY ////////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(*self.posIsTaken) gt 0 then begin
        tf_free = ~reform(*self.posIsTaken, self.GrLayout[0], self.GrLayout[1])

        print, ''
        print, '--LAYOUT AVAILABILITY--'
        for i = 0, self.GrLayout[1]-1 do begin
            print, FORMAT='(a2, ' + strtrim(self.GrLayout[0],2) + '(i1, 1x), a1)', $
                   '| ', tf_free[*,i], '|'
        endfor
    endif
    
    print, ''
end


;+
;   Clean up after the object is destroyed.
;-
pro MrGrLayout::cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ptr_free, self.fixed_positions
    ptr_free, self.layout_positions
    ptr_free, self.posIsTaken
    
    self -> MrLayout::CleanUp
end


;+
;   This method initializes the MrGrLayout object.
;
; :Keywords:
;       LAYOUT:         in, optional, type=intarr(2), default=[0\,0]
;                       A 2 element vector specifying the number of plots in the vertical
;                           and horizontal directions, [ncols, nrows].
;       UPDATE_LAYOUT:  in, optional, type=boolean, default=0
;                       If set, recalculate the plot positions after updating the layout.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrLayout::Init is also accepted
;                           for keyword inheritance.
;-
function MrGrLayout::init, $
UPDATE_LAYOUT = update_layout, $
LAYOUT = layout, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif

    if self -> MrLayout::Init(_STRICT_EXTRA=extra) eq 0 then $
        message, 'Unable to initialize MrLayout.'

    ;Set default values
    if n_elements(layout) eq 0 then layout = [0,0]
    self.fixed_positions = ptr_new(/ALLOCATE_HEAP)
    self.layout_positions = ptr_new(/ALLOCATE_HEAP)
    self.posIsTaken = ptr_new(/ALLOCATE_HEAP)
        
    
    ;Set layout an indicate that no positions have been taken.
    self.GrLayout = layout
    if layout[0]*layout[1] gt 0 then begin
        (*self.posIsTaken) = bytarr(layout[0]*layout[1])
        self -> CalcPositions
    endif
    
    if n_elements(aspect) eq 0 $
        then self.aspect = ptr_new(/ALLOCATE_HEAP) $
        else self.aspect = ptr_new(aspect)
    
    ;Calculate the positions
    if keyword_set(update_layout) and array_equal(self.GrLayout, [0,0]) eq 0 $
        then self -> CalcPositions
    
    return, 1                     
end


;+
;   The class definition
;-
pro MrGrLayout__define
    compile_opt strictarr
    
    class = {MrGrLayout, $
             inherits MrLayout, $
             fixed_positions: ptr_new(), $  ;Fixed, non-layout-related positions.
             GrLayout: [0,0], $             ;Layout of the plot area [ncols, nrows].
             layout_positions: ptr_new(), $ ;Positions outlined by the 2D plot-layout grid.
             nplots: 0, $                   ;Number of plots displayed.
             nfixed: 0, $                   ;Number of fixed positions.
             posIsTaken: ptr_new() $        ;Is the Layout_Position filled?
            }
end