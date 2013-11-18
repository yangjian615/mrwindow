; docformat = 'rst'
;
; NAME:
;       MrPlotLayout__Define
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
;           MyObj = obj_new('MrPlotLayout')
;           MyObj -> AddToLayout, [2,1]
;           MyObj -> AddToLayout, [3,3]
;           MyObj -> WhichLayout
;           MyObj -> FillHoles, /TRIMLAYOUT
;           MyObj -> WhichLayout
;           Obj_Destroy, MyObj
;
;   EXAMPLE 2: Complete::
;       ;Create the object and add the next available position to the layout
;           MyObj = obj_new('MrPlotLayout')
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
;                           MrPlotLayout::SetProperty so that subclasses do not get into
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
;                           
;-
;*****************************************************************************************
;+
;   The purpose of this method is to add new positions and locations to the plotting
;   grid. This simulates adding an actual plot to the display.
;
; :Params:
;       LOCATION:       in, out, optional, type=intarr(2\,*)
;                       Plot location [col, row] of the plot position to remove. Location
;                           [1,1] indicates the plot in the top-left corner. If not
;                           provided, one will be generated and returned.
;       POSITION:       in, out, optional, type=fltarr(4)
;                       The position of the plot indicated by `LOCATION`. 
;                           The lower-left and upper-right corners of a plot in normal
;                           coordinates. If not provided, then one will be generated
;                           and returned.
;
; :Keywords:
;       LAYOUT:         out, optional, type=boolean
;                       The layout needed to accomodate `LOCATION` and `POSITION`. If
;                           `UPDATE_LAYOUT`=1, this will be the same as "self.layout"
;       UPDATE_LAYOUT:  in, optional, type=boolean, default=1
;                       Set to 0 to simulate adding a position to they layout. In this
;                           case, an available location and position will be found, but
;                           the actual layout will remain unchanged. The default is to
;                           (find and) assimilate `LOCATION` and `POSITION` into the
;                           layout.
;-
pro MrPlotLayout::AddToLayout, location, position, $
LAYOUT=layout, $
UPDATE_LAYOUT=update_layout
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    SetDefaultValue, update_layout, 1, /BOOLEAN
    if n_elements(location) gt 0 && location[0] lt 0 then $
        message, 'To add a fixed position, provide POSITION. LOCATION will be returned.'
        
;---------------------------------------------------------------------
;Get a Location? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If no location was provided
    if n_elements(location) eq 0 and n_elements(position) gt 0 then begin
        ;Create the location
        location = self -> MakeFixedLocation()
        layout = self.layout
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
        self -> Make_Location, location, $
                               POSITION=position, $
                               LAYOUT=layout, $
                               UPDATE_LAYOUT=update_layout
        if update_layout eq 0 then return
        
        ;Add them to the layout
        iList = self -> ConvertLocation(location)
        (*self.posIsTaken)[iList] = 1
        
    endelse
    
    ;Increase the plot count
    self.nplots += 1
end


;+
;   The purpose of this method is to calculate the plot positions.
;-
pro MrPlotLayout::CalcLayoutPositions
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Return if nothing has been added to the layout.
    if array_equal(self.layout, [0,0]) then return
    
    if self.layout[0] eq 0 xor self.layout[1] eq 0 $
        then message, 'Cannot calculate position. Layout must have at least ' + $
                      'one column and one row.'

    ;Calculate positions
    *self.layout_positions = MrPlotLayout(self.layout, $
                                          ASPECT = *self.aspect, $
                                          CHARSIZE = self.charsize, $
                                          XGAP = self.xgap, $
                                          XMARGIN = self.xmargin, $
                                          YGAP = self.ygap, $
                                          YMARGIN = self.ymargin)

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
pro MrPlotLayout::ClearLayout, $
FIXED=fixed, $
LAYOUT=layout
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
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
        self.layout = [0,0]
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
;       LAYOUT:         in, optional, type=intarr(2), default=self.layout
;                       The number of columns and rows in the layout: [ncols, nrows].
;
; :Keywords:
;       EXISTS:         out, optional, type=boolean
;                       Returns 1 if `LOCATION` exists within `LAYOUT` and 0 otherwise.
;                           If EXISTS is 0 and no named variable is present, an error 
;                           will occur.
;       LIST_INDEX      in, optional, type=boolean, default=0
;                       Indicate that `LOCATION` is actually a list-index.
;       PLOT_INDEX:     in, optional, type=boolean, default=0
;                       Indicate that `LOCATION` is actually a plot-index.
;       TO_COLROW:      in, optional, type=boolean
;                       Indicate that `LOCATION` is to be converted to a [col, row] location.
;       TO_LIST_INDEX:  in, optional, type=boolean, default=0
;                       Indicate that `LOCATION` is to be converted to a list-index.
;                           This is the default.
;       TO_PLOT_INDEX:  in, optional, type=boolean, default=0
;                       Indicate that `LOCATION` is to be converted to a plot-index.
;       
;
; :Returns:
;       RESULT:         The result of the convertion.
;-
function MrPlotLayout::ConvertLocation, location, layout, $
EXISTS=exists, $
LIST_INDEX=list_index, $
PLOT_INDEX=plot_index, $
TO_LIST_INDEX=to_list_index, $
TO_PLOT_INDEX=to_plot_index, $
TO_COLROW=to_colrow
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, -1
    endif

;---------------------------------------------------------------------
;Fixed Position //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Fixed locations
    if location[0] lt 0 then begin
        result = location[1]-1
        return, result
    endif

;---------------------------------------------------------------------
;Defaults ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Default to a [col, row] location.
    SetDefaultValue, layout, self.layout
    plot_index = keyword_set(plot_index)
    list_index = keyword_set(list_index)
    to_plot_index = keyword_set(to_plot_index)
    to_list_index = keyword_set(to_list_index)
    to_colrow = keyword_set(to_colrow)

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Put restrictions.
    if plot_index + list_index eq 2 then $
        message, 'PLOT_INDEX and LIST_INDEX cannot be specified together.'
    
    ;Default to converting to a list index.
    if to_plot_index + to_list_index + to_colrow eq 0 then to_list_index = 1
    if to_plot_index + to_list_index + to_colrow ne 1 then $
        message, 'One and only one of TO_PLOT_INDEX, TO_LIST_INDEX, and TO_COLROW are allowed.'
    
    ;Make sure the location exists
    exists = self -> PlotExists(location, layout, PLOT_INDEX=plot_index, LIST_INDEX=list_index)
    if max(exists) eq 0 then if arg_present(exists) $
        then return, !Null $
        else message, 'Location does not exist within layout. Cannot convert.'

;---------------------------------------------------------------------
;Convert /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;
    ;Note that:
    ;   - PLOT_INDEX is row-major while LIST_INDEX is column-major.
    ;   - COLROW and PLOT_INDEX are 1-based while LIST_INDEX is 0-based.
    ;   - TWOD_TO_ONED_INDEX works with 0-based indices.
    ;

    ;TO PLOT INDEX
    if keyword_set(to_plot_index) then begin
        case 1 of
            keyword_set(list_index): begin
                colrow = twoD_to_oneD_index(location, layout, /oneD_to_twoD, /COL_MAJOR) ;row-major location -> [col,row]
                result = twoD_to_oneD_index(colrow, layout)+1                            ;[col,row] -> col-major index
            endcase
            keyword_set(plot_index): result = location
            else                   : result = twoD_to_oneD_index(location-1, layout)+1
        endcase
    
    ;TO [COL, ROW]
    endif else if keyword_set(to_colrow) then begin
        case 1 of
            keyword_set(plot_index): result = twoD_to_oneD_index(location-1, layout, /oneD_to_twoD)+1
            keyword_set(list_index): result = twoD_to_oneD_index(location,   layout, /oneD_to_twoD, /COL_MAJOR)+1
            else                   : result = location
        endcase
          
    ;TO LIST INDEX
    endif else if keyword_set(to_list_index) then begin
        case 1 of
            keyword_set(plot_index): begin
                colrow = twoD_to_oneD_index(location-1, layout, /oneD_to_twoD)
                result = twoD_to_oneD_index(colrow, layout, /COL_MAJOR)
            endcase
            
            keyword_set(list_index): result = location
            else:                    result = twoD_to_oneD_index(location-1, layout, /COL_MAJOR)
        endcase
    endif
    
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
pro MrPlotLayout::ExpandLayout, nCols, nRows
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    if n_elements(nCols) eq 2 then nRows = nCols[1]

    ;Default to adding one row
    SetDefaultValue, nCols, 1
    SetDefaultValue, nRows, 1
    if nRows eq 0 && nCols[0] eq 0 then return

    ;Resposition all of the existing plots        
    self -> SetProperty, LAYOUT=self.layout+[nCols[0], nRows]
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
pro MrPlotLayout::FillHoles, $
TRIMLAYOUT=trimLayout
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Get a vector of availability in plot-index order
    tf_available = self -> IsAvailable(/PLOT_INDEX)
    if tf_available eq !Null then return
    
    pFree = where(tf_available eq 1, nFree, COMPLEMENT=pTaken, NCOMPLEMENT=nTaken)
    if nFree eq 0 then return
    
    pFilledTaken = indgen(nTaken) + 1
    iFilledTaken = self -> ConvertLocation(pFilledTaken, /PLOT_INDEX, /TO_LIST_INDEX)

    ;Rearrange the positions so that all of the taken positions are at the end.
    (*self.posIsTaken)[*] = 0
    (*self.posIsTaken)[iFilledTaken] = 1
    
    ;Get rid of excess columns and rows?
    if keyword_set(trimLayout) then self -> TrimLayout
end


;+
;   The purpose of this method is to find the [col,row] location of a plot given its
;   position. For plots in fixed positions, col=-1.
;
; :Keywords:
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
;
; :Returns:
;       LOCATION:       The location of `POSITION`. If no match is found, [0,0] is returned.
;-
function MrPlotLayout::FindLocation, position, $
EPSILON = epsilon, $
FIXED = fixed, $
NFOUND = nFound
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, [0,0]
    endif

    SetDefaultValue, epsilon, 1e-5
    if n_elements(fixed) eq 0 then fixed = 2 else fixed = keyword_set(fixed)

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
        nLayout = self.layout[0]*self.layout[1]
        pLayout = reform(*self.layout_positions, 4, nLayout)
        
        ;Step through all of the layout positions
        for i = 0, self.layout[0]*self.layout[1] - 1 do begin
        
            ;If the layout position is not taken, skip it
            if (*self.posIsTaken)[i] eq 0 then continue

            ;Look for a match.
            if array_equal(position, pLayout[*,i]) then begin
                location[*,nFound] = self -> ConvertLocation(i, /LIST_INDEX, /TO_COLROW)
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
            ;Calculate the difference between the stored positions and the given positions
            delta_fixed  = (*self.fixed_positions)  - rebin(position, 4, self.nFixed)
        
            ;Find the almost-equal case.
            index = where(abs(mean(delta_fixed, DIMENSION=1)) le epsilon, nMatch)
            nFound += nMatch
        
            ;Return the location of the match
            if nMatch gt 0 then $
                location[*,nFound:nFound+nMatch-1] = [replicate(-1, 1, nMatch), [index+1]]
        endif
        
        ;Layout positions
        nLayout = self.nPlots - self.nFixed
        if fixed ne 1 and nLayout gt 0 then begin
            delta_layout = (*self.layout_positions) - rebin(position, 4, self.nPlots-self.nFixed)
            void = self -> IsAvailable(ITAKEN=iTaken, NTAKEN=nTaken)
        endif
    endif
    
    return, location[*,nFound-1]
end


;+
;   The purpose of this method is to change/replace the position and/or location of a
;   plot that already exists within the 2D plotting grid.
;
; :Params:
;       LOCATION:           in, required, type=integer/intarr(2)
;                           Either the 2-element [col, row] or, if the location is within
;                               the auto-updating layout, the scalar plot-index location
;                               of the plot whose position is to be returned.
;
; :Keywords:
;       POSITION:           The position of the plot at `LOCATION`.
;-
function MrPlotLayout::GetPosition, location
    compile_opt idl2

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, !Null
    endif

    ;Get the [col, row] location.
    case n_elements(location) of
        1: colrow = self -> ConvertLocation(location, /PLOT_INDEX, /TO_COLROW)
        2: colrow = location
        else: message, 'LOCATION: Incorrect number of elements.'
    endcase
    
    ;Make sure it exists.
    exists = self -> PlotExists(colrow)
    if exists eq 0 then message, 'LOCATION does not exist. Cannot get its position.'
    
    ;Get the position
    if location[0] lt 0 $
        then position = (*self.fixed_positions)[*,location[1]-1] $
        else position = (*self.layout_positions)[*, colrow[0]-1, colrow[1]-1]
    
    return, position
end


;+
;   The purpose of this method is to retreive object properties.
;
; :Keywords:
;       ASPECT:         out, optional, type=float
;                       The aspect ratio (plot height/plot width) of each plot. For square
;                           plots, ASPECT=1.0, for plots that are twice as wide as the are
;                           long, ASPECT=0.5.
;       CHARSIZE:       out, optional, type=float, default=1.5
;                       Fraction of IDL's default character size.
;       LAYOUT:         out, required, type=intarr(2)
;                       A 2 element vector specifying the number of plots in the vertical
;                           and horizontal directions, [ncols, nrows].
;       LOCATION:       out, optional, type=fltarr(4)
;                       The position of the plot specified by `THISLOCATION`
;       THISLOCATION:   in, optional, type=intarr(2)
;                       A vector specifying the [col, row] of the plot position to be
;                           returned. The position is returned in the variable `LOCATION`,
;                           with [1,1] indicating the top, left plot.
;       XMARGIN:        out, optional, type=fltarr(2), default="[10, 3]"
;                       The x-margins in character widths. [left margin, right margin]
;                           The margins specify the distance between the edge of the plot 
;                           axes and the edge of the plotting window.
;       YMARGIN:        out, optional, type=fltarr(2), default="[4, 2]"
;                       The y-margins in character heights. [bottom, top]
;                           The margins specify the distance between the edge of the plot 
;                           axes and the edge of the plotting window
;       XGAP:           out, optional, type=float, default=14
;                       The horizontal gap between plots in character widths (!D.y_ch_size)
;       YGAP:           out, optional, type=float, default=7
;                       The vertical gap between plots in character heights (!D.y_ch_size)
;-
pro MrPlotLayout::GetProperty, $
ASPECT = aspect, $
CHARSIZE = charsize, $
LAYOUT = layout, $
LOCATION = location, $
NPLOTS = nplots, $
THISLOCATION = thisLocation, $
XMARGIN = xmargin, $
XGAP = xgap, $
YMARGIN = ymargin, $
YGAP = ygap
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Get Properties
    if arg_present(aspect)   and n_elements(*self.aspect) ne 0 then aspect = *self.aspect
    if arg_present(charsize) then charsize = self.charsize
    if arg_present(layout)   then layout   = self.layout
    if arg_present(nplots)   then nplots   = self.nplots
    if arg_present(xgap)     then xgap     = self.xgap
    if arg_present(xmargin)  then xmargin  = self.xmargin
    if arg_present(ygap)     then ygap     = self.ygap
    if arg_present(ymargin)  then ymargin  = self.ymargin 
    if arg_present(location) then $
        if n_elements(thisLocation) ne 0 and n_elements(*self.layout_positions) ne 0 $
            then location = (*self.layout_positions)[*, location[0]-1, location[1]-1]
end


;+
;   The purpose of this method is to provide a means of generating new plot locations
;   within the 2D plotting grid.
;
; :Params:
;       LOCATION:           in, optional, type=long
;                           The [col, row] location of the position that is being checked.
;                               If LOCATION[0] < 0 then the Fixed positions will be
;                               checked for availability. If not given, availability for
;                               all plots will be returned in array-index order.
;
; :Keywords:
;       IFREE:              out, optional, type=intarr
;                           The array-index of each available layout position. Returned only
;                               if `LOCATION` is not provided.
;       INSIDE:             in, optional, type=boolean, default=0
;                           Look only inside the plot layout. Normally, if `LOCATION` lies
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
;       PLOT_INDEX:         in, optional, type=boolean, default=0
;                           If set and `LOCATION` is not provided, return availability
;                               of all positions in plot-index order. Note that plot-index
;                               order is 0-based while an actual plot index is 1-based.
;                               Take care.... `IFREE` and `ITAKEN` will be converted to
;                               plot-index values.
;
; :Returns:
;       TF_AVAILABLE:       1     if the position indicated by `LOCATION` is avaialble,
;                           0     if not.
;                           !Null if no layout has been established.
;-
function MrPlotLayout::IsAvailable, location, $
IFREE = iFree, $
INSIDE = inside, $
ITAKEN = iTaken, $
NFREE = nFree, $
NTAKEN = nTaken, $
PLOT_INDEX = plot_index
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, !Null
    endif
    
    ;Defaults
    inside = keyword_set(inside)
    plot_index = keyword_set(plot_index)
    
;---------------------------------------------------------------------
;Availability of All Positions? //////////////////////////////////////
;---------------------------------------------------------------------
    if n_params() eq 0 then begin
        ;Has a layout been defined yet?
        if n_elements(*self.posIsTaken) eq 0 then begin
            nFree = 0
            nTaken = 0
            return, !Null
        endif
        
        ;Find the indices of each available/taken plot?
        if arg_present(iFree)  || arg_present(nFree)  || $
           arg_present(iTaken) || arg_present(nTaken) then findIndex = 1 else findIndex = 0
    
        ;Check if the list index is available.
        tf_available = ~*self.posIsTaken
        iFree = where(tf_available eq 1, nFree, COMPLEMENT=iTaken, NCOMPLEMENT=nTaken, /NULL)

        ;Convert from array-index order to plot-index order.
        nLayout = n_elements(*self.posIsTaken)
        if plot_index eq 1 && nLayout gt 0 then begin
            ;Convert array indices to plot indices.
            if nFree  gt 0 then iFree  = self -> ConvertLocation(iFree,  /LIST_INDEX, /TO_PLOT_INDEX)
            if nTaken gt 0 then iTaken = self -> ConvertLocation(iTaken, /LIST_INDEX, /TO_PLOT_INDEX)
            
            ;Convert TF_AVAILBLE to plot-index availability
            tf_available = intarr(nLayout) + 1
            if nFree gt 0 then tf_available[iFree-1] = 0
        endif
        
        return, tf_available
    endif

;---------------------------------------------------------------------
;Specific Positions? /////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Fixed locations.
    if location[0] lt 0 then begin
        tf_available = location[1] gt self.nFixed
    
    ;Layout locations.
    endif else begin

        ;If the layout is empty, then the location is avaialble
        ;(unless INSIDE is set).
        if array_equal(self.layout, [0,0]) then $
            if inside eq 1 then return, 0 else return, 1
    
        ;If LOCATION is outside of the layout, then the location is available
        ;(unless INSIDE is set).
        if self -> PlotExists(location) eq 0 then $
            if inside eq 1 then return, 0 else return, 1
    
        ;Check if the position has been taken already
        iList = self -> ConvertLocation(location)
        tf_available = ~(*self.posIsTaken)[iList]
    endelse
    
    return, tf_available
end


;+
;   The purpose of this program is get the next available location for a fixed position.
;
; :Returns:
;       LOCATION:           The next available location for a fixed position.
;-
function MrPlotLayout::MakeFixedLocation
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, !Null
    endif
    
    ;Get a new location
    location = [-1, self.nFixed+1]
    
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
;       LOCATION:           in, out, optional, type=lonarr(2)
;                           If provided and defined, then this is a named variable
;                               containing the [Col, Row] location of a plot. If the plot
;                               does not fit within the current plot layout, the layout
;                               will be expanded to contain the plot. If provided and 
;                               undefined, then this is a named variable into which the
;                               lowest available [Col, Row] plot location will be returned.
;
; :Keywords:
;       UPDATE_LAYOUT:      in, optional, type=boolean, default=0
;                           If set, the plot layout will be adjusted to accomodate the
;                               location and position being retrieved. Plot positions
;                               will be recalculated to fit the new layout.
;       LAYOUT:             out, optional, type=intarr(2)
;                           The layout needed to fit the `LOCATION`. Returned only if
;                               `UPDATE_LAYOUT`=0
;
; :Uses:
;   Uses the following external programs::
;       SetDefaultValue.pro (Coyote Graphics)
;-
pro MrPlotLayout::Make_Location, location, $
UPDATE_LAYOUT = update_layout, $
LAYOUT = layout, $
POSITION = position
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

;---------------------------------------------------------------------
;Check Initial Condition /////////////////////////////////////////////
;---------------------------------------------------------------------
    n_loc = n_elements(location)

    setDefaultValue, update_layout, 0, /BOOLEAN

    ;SELF.LAYOUT is initialized to [0,0], which is an invalid plot location. If this
    ;is still the case, and LOCATION was not given, then make sure location is defined.
    if array_equal(self.layout, [0,0]) && n_loc eq 0 then begin
        location = [1,1]
        n_loc = 2
    endif

;---------------------------------------------------------------------
;Location Provided ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_loc eq 2 then begin

        ;Check if there is a plot in this location
        tf_free = self -> IsAvailable(location)

    ;---------------------------------------------------------------------
    ;Already Exists? /////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if tf_free eq 0 then begin
            ;Shift plots out of the way?
            if keyword_set(update_layout) then begin
                self -> ShiftPlots, location
                
            ;If not...
            endif else begin
                ;Get the index of the desired location and all available locations.
                pIndex = self -> ConvertLocation(location, /TO_PLOT_INDEX)
                void = self -> IsAvailable(IFREE=pFree, NFREE=nFree, /PLOT_INDEX)
                
                ;If there are no available locations after the desired one, add a row
                ;so that current positions can be shifted out of the way to make room
                ;for the new one.
                if nFree eq 0 || (max(pFree gt pIndex) eq 0) $
                    then layout = self.layout + [0,1] $
                    else layout = self.layout
            endelse

    ;---------------------------------------------------------------------
    ;Does Not Exist? /////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endif else begin
            ;Are more columns needed?
            if location[0] gt self.layout[0] $
                then nCols = location[0] - self.layout[0] $
                else nCols = 0
            
            ;Are more rows needed?
            if location[1] gt self.layout[1] $
                then nRows = location[1] - self.layout[1] $
                else nRows = 0

            ;Update the layout?
            if keyword_set(update_layout) $
                then self -> ExpandLayout, nCols, nRows $
                else layout = self.layout + [nCols, nRows]
        endelse
    
;---------------------------------------------------------------------
;Location Needed /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else if n_loc eq 0 then begin
        ;Get availability 
        void = self -> IsAvailable(ITAKEN=pIndex, NTAKEN=nTaken, /PLOT_INDEX)
        
        ;
        ;We want to return a position that is after the last unavailable position.
        ;In this way, new positions are ways taken from the end of the line.
        ;

    ;---------------------------------------------------------------------
    ;Everything Available? ///////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if nTaken eq 0 then begin
            ;Take the first one.
            location = [1,1]
            layout = self.layout
            
    ;---------------------------------------------------------------------
    ;Something Already Taken? ////////////////////////////////////////////
    ;---------------------------------------------------------------------
        endif else begin
            ;Take the highest available plot location
            thisPI = pIndex[-1] + 1

        ;---------------------------------------------------------------------
        ;Outside Current Layout? /////////////////////////////////////////////
        ;---------------------------------------------------------------------
            if thisPI gt self.layout[0]*self.layout[1]-1 then begin
            
                ;Update the layout?
                if keyword_set(update_layout) $
                    then self -> ExpandLayout, 0, 1 $
                    else layout = self.layout + [0,1]
                
                ;Find the location in the new layout.
                location = [1, self.layout[1]]
            
        ;---------------------------------------------------------------------
        ;Inside Current Layout? //////////////////////////////////////////////
        ;---------------------------------------------------------------------
            endif else begin
                location = self -> ConvertLocation(thisPI, /PLOT_INDEX, /TO_COLROW)
                layout = self.layout
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
            position = (*self.layout_positions)[*, location[0]-1, location[1]-1]
        endif else begin
            position = MrPlotLayout(layout, location, $
                                    ASPECT=*self.aspect, CHARSIZE=self.charsize, $
                                    XMARGIN=self.xmargin, XGAP=self.xgap, $
                                    YMARGIN=self.ymargin, YGAP=self.ygap)
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
;
; :Keywords:
;       LIST_INDEX:     in, optional, type=boolean, default=0
;                       If set, `LOCATION` is a vector of list index locations. The list
;                           index is the index at which the plot's object reference is
;                           stored. See the "Plots_Present" method.
;       PLOT_INDEX:     in, optional, type=boolean, default=0
;                       If set, `LOCATION` is a vector of 1D plot locations. In this case,
;                           the upper left plot is index 1, and the index number increases
;                           first downward, then across.
;
; :Returns:
;       EXISTS:         Tells whether the plot exists (1) or not (0).
;-
function MrPlotLayout::PlotExists, location, layout, $
LIST_INDEX = list_index, $
PLOT_INDEX = plot_index
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif
    
    ;Default to current layout
    SetDefaultValue, layout, self.layout

    ;Layout Positions
    if location[0] ge 0 then begin
        ;The position does not exist if it is outside the layout
        case 1 of
            keyword_set(list_index): exists = location le layout[0]*layout[1]-1
            keyword_set(plot_index): exists = location le layout[0]*layout[1]
            else                   : exists = (location[0,*] le layout[0]) and $
                                              (location[1,*] le layout[1])
        endcase
    
    ;Fixed Positions
    endif else begin
        if location[1] gt self.nFixed || location[1] le 0 $
            then exists = 0 $
            else exists = 1
    endelse
    
    return, exists
end


;+
;   The purpose of this method is to remove locations and positions from the 2D plotting
;   grid.
;
; :Params:
;       LOCATION:       in, required, type=intarr(2\,*)
;                       Plot location [col, row] of the plot position to remove. Location
;                           [1,1] indicates the plot in the top-left corner.
;
; :Keywords:
;       FILLHOLES:      in, optional, type=boolean, default=0
;                       Sometimes removing a plot from the 2D auto-updating grid can leave
;                           holes in the layout. Set this keyword to 1 (one) to adjust
;                           plot locations so that those holes are filled.
;-
pro MrPlotLayout::RemoveFromLayout, location, $
FILLHOLES = fillHoles
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Defaults
    fillHoles = keyword_set(fillHoles)
    nRemove = n_elements(location)/2
    
    ;Check if a position exists at the given location.
    tf_free = self -> IsAvailable(location)
    if tf_free eq 1 then return

;---------------------------------------------------------------------
;Fixed Positions /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if location[0] lt 0 then begin
        iListAll = indgen(self.nFixed)
        iList = self -> ConvertLocation(location)
        void = isMember(iList, iListAll, NONMEMBER_INDS=iKeep, N_NONMEMBERS=nKeep)

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
        iListAll = indgen(self.nPlots - self.nFixed)
        
        iList = self -> ConvertLocation(location)
        void = isMember(iList, iListAll, NONMEMBER_INDS=iKeep, N_NONMEMBERS=nKeep)
        
        if nKeep eq 0 then begin
            self -> ClearLayout, /LAYOUT
        endif else begin
            self.nPlots -= nRemove
            (*self.posIsTaken)[iList] = 0
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
;       OLD_POSITION:       in, required, type={1 | 2 | 4}-element vector
;                           The plot-index, [col, row], or 4-element position of the plot
;                               whose position is to be changed.
;       NEW_POSITION:       in, required, type={1 | 2 | 4}-element vector
;                           The plot-index, [col, row], or 4-element position to where the
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
pro MrPlotLayout::SetPosition, old_position, new_position, $
OUTPOSITION = outPosition, $
OUTLOCATION = outLocation, $
TOFIXED = toFixed
    compile_opt idl2

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    toFixed = keyword_set(toFixed)

;---------------------------------------------------------------------
;Were Locations or Positions Given? //////////////////////////////////
;---------------------------------------------------------------------

    ;Where is the plot being moved?
    case n_elements(old_position) of
        1: oldColRow = ConvertLocation(old_position, /PLOT_INDEX, /TO_COLROW)
        2: oldColRow = old_position
        4: oldPos = old_position
        else: message, 'Incorrect number of elements: OLD_POSITION.'
    endcase

    ;Where is it being moved to?
    case n_elements(new_position) of
        0: if keyword_set(toFixed) eq 0 then message, 'Incorrect number of elements: NEW_POSITION.'
    
        ;If a plot-index was given: It must exist within the current layout in order to
        ;know exactly where it is going.
        1: begin
            tf_exist = self -> PlotExists(new_position, /PLOT_INDEX)
            if tf_exist eq 0 $
                then message, 'NEW_POSITION lies outside the current layout. Provide a [col, row] location instead.' $
                else newColRow = self -> ConvertLocation(new_position, /PLOT_INDEX, /TO_COLROW)
        endcase
        
        ;A [col, row] location: If a fixed-location, ensure it is the next available one.
        2: begin
            if new_position[0] gt 0 $
                then newColRow = new_position $
                else newColRow = self -> MakeFixedLocation()
        endcase
        
        ;A position was given.
        4: newPos = new_position
        else: message, 'Incorrect number of elements: NEW_POSITION.'
    endcase

;---------------------------------------------------------------------
;Convert OLD_POSITION to a [Col, Row] Location ///////////////////////
;---------------------------------------------------------------------
    
    ;Make sure a plot exists at the old position.
    if n_elements(oldColRow) gt 0 then begin
        isFree = self -> IsAvailable(oldColRow, /INSIDE)
        if isFree eq 1 then message, 'No plot exists at OLD_POSITION. Cannot set its position.'
    endif
        
    ;Turn a position into a [col, row] location. Make sure a plot can be found.
    if n_elements(oldPos) gt 0 then begin
        oldColRow = self -> FindLocation(oldPos, NFOUND=nFound)
        if nFound eq 0 then message, 'No plot exists at OLD_POSITION. Cannot set its position.'
        if nFound gt 1 then message, 'More than one plot was found at OLD_POSITION. Provide a [col, row] location instead.'
    endif
    
    ;Move a plot from an auto-updating position into a fixed position
    if keyword_set(toFixed) then begin
        if oldColRow[0] lt 0 then message, 'OLD_POSITION is already a fixed position.'
        void = temporary(newColRow)
        newPos = (*self.layout_positions)[*, oldColRow[0]-1, oldColRow[1]-1]
    endif
;---------------------------------------------------------------------
;Set a [Col, Row] Location ///////////////////////////////////////////
;---------------------------------------------------------------------

    ;Remove the old position, add the new one.
    self -> RemoveFromLayout, oldColRow
    self -> AddToLayout, newColRow, newPos
    
    ;Return the new position and [col, row] location.
    outPosition = newPos
    outLocation = newColRow
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
function MrPlotLayout::RePosition, old_layout, new_layout, plot_index
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, !Null
    endif

    ;If the old layout is empty, check if PLOT_INDEX fits in the new layout.
    if array_equal(old_layout, [0,0]) then begin
        ;If no plot index was given, then there *should* be nothing to convert.
        if n_elements(plot_index) eq 0 then $
            if n_elements(*self.posIsTaken) eq 0 $
                then return, !Null $
                else message, 'OLD_LAYOUT did not match posIsTaken. Report bug.'
    
        if self -> PlotExists(plot_index, new_layout) $
            then return, plot_index $
            else message, 'PLOT_INDEX is outside of NEW_LAYOUT. Cannot Re-Position.'
    endif
    
    ;If the new layout is empty, 
    if new_layout[0] eq 0 || new_layout[1] eq 0 $
        then message, 'NEW_LAYOUT is invalid. Must have at least 1 column and 1 row.'
    
    ;Convert a plot index
    if n_elements(plot_index) gt 0 then begin
        old_colrow     = self -> ConvertLocation(plot_index, old_layout, /PLOT_INDEX, /TO_COLROW)
        new_plot_index = self -> ConvertLocation(old_colrow, new_layout, /TO_PLOT_INDEX)
        return, new_plot_index
    endif
    
    ;Find the indices of the filled positions in the old layout.
    iOldTaken = where(*self.posIsTaken eq 1, nOldTaken)
    
    ;Make a new array for the new layout.
    nNew = new_layout[0]*new_layout[1]
    newIsTaken = bytarr(nNew)
    
    ;If no positions were taken in the old layout, then none are taken in the new one.
    if nOldTaken eq 0 then return, newIsTaken

    ;Convert the indices of the taken positions to plot index locations. Plot index
    ;locations are invariant under change of layout.
    oldTaken_colrow = self -> ConvertLocation(iOldTaken, old_layout, /LIST_INDEX, /TO_PLOT_INDEX)
    iNewTaken = self -> ConvertLocation(oldTaken_colrow, new_layout, /PLOT_INDEX, /TO_LIST_INDEX)

    ;Record which positions are taken in the new layout.
    newIsTaken[iNewTaken] = 1
    
    return, newIsTaken
end


;+
;   Use this method to set the value of object properties.
;
; :Keywords:
;       ASPECT:         in, optional, type=float
;                       The aspect ratio (plot height/plot width) of each plot. For square
;                           plots, ASPECT=1.0, for plots that are twice as wide as the are
;                           long, ASPECT=0.5.
;       CHARSIZE:       in, optional, type=float, default=1.5
;                       Fraction of IDL's default character size.
;       LAYOUT:         in, required, type=intarr(2)
;                       A 2 element vector specifying the number of plots in the vertical
;                           and horizontal directions, [ncols, nrows].
;       XGAP:           in, optional, type=float, default=14
;                       The horizontal gap between plots in character widths (!D.y_ch_size)
;       XMARGIN:        in, optional, type=fltarr(2), default="[10, 3]"
;                       The x-margins in character widths. [left margin, right margin]
;                           The margins specify the distance between the edge of the plot 
;                           axes and the edge of the plotting window.
;       YGAP:           in, optional, type=float, default=7
;                       The vertical gap between plots in character heights (!D.y_ch_size)
;       YMARGIN:        in, optional, type=fltarr(2), default="[4, 2]"
;                       The y-margins in character heights. [bottom, top]
;                           The margins specify the distance between the edge of the plot 
;                           axes and the edge of the plotting window
;-
pro MrPlotLayout::SetProperty, $
ASPECT = aspect, $
CHARSIZE = charsize, $
DRAW = draw, $
LAYOUT = layout, $
XMARGIN = xmargin, $
XGAP = xgap, $
YMARGIN = ymargin, $
YGAP = ygap
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Set Properties
    if n_elements(aspect)   gt 0 then *self.aspect = aspect
    if n_elements(charsize) gt 0 then  self.charsize = charsize
    if n_elements(xgap)     gt 0 then  self.xgap = xgap
    if n_elements(xmargin)  gt 0 then  self.xmargin = xmargin
    if n_elements(ygap)     gt 0 then  self.ygap = ygap
    if n_elements(ymargin)  gt 0 then  self.ymargin = ymargin

    ;Make sure plot locations are invariant to changes in the layout
    if n_elements(layout) ne 0 then begin
    
        ;Is the new layout empty?
        if array_equal(layout, [0,0]) then begin
            self -> ClearLayout, /LAYOUT
        
        ;If not...
        endif else begin
    
            ;Make sure the new layout will be valid.
            if layout[0] eq 0 xor layout[1] eq 0 then $
                message, 'LAYOUT must have at least 1 column and 1 row.'

            ;Make sure the new layout is big enough. Get the largest plot index and
            ;compare it to the layout given.
            void = self -> IsAvailable(ITAKEN=pTaken, NTAKE=nTaken, /PLOT_INDEX)
            if nTaken gt 0 then begin
                void = max(pTaken, imax)
                if layout[0]*layout[1] lt pTaken[imax] then $
                    message, 'LAYOUT is not big enough to contain the current set of plots. ' + $
                             'Try theObj -> FillHoles, /TRIMLAYOUT, /DRAW first.'
            endif

            ;Resposition all of the existing plots        
            *self.posIsTaken = self -> RePosition(self.layout, layout)
            if *self.posIsTaken eq !Null then *self.posIsTaken = bytarr(layout[0]*layout[1])

            ;Change the layout
            self.layout = layout
        endelse
    endif
    
    ;Calculate the positions of the plots within the new layout
    self -> CalcLayoutPositions
end


;+
;   Shift all plots located at and after LOCATION up one index value.
;
; :Private:
;
; :Params:
;
;       LOCATION:           in, required, type=lonarr(2)
;                           The 1-based plot location [col, row] at which to begin 
;                               shifting plots.
;-
pro MrPlotLayout::ShiftPlots, location
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;get the [Col, Row] location of where to start shifting plots and convert it to a
    ;1-based, 1D plot number.
    iList = self -> ConvertLocation(location, /TO_LIST_INDEX)
    
    ;Find the positions that are not yet taken
    iNotTaken = where((*self.posIsTaken)[iList:*] eq 0, nNotTaken)

    ;If all positions are taken, add another row to the layout
    if nNotTaken eq 0 then begin
        self -> ExpandLayout, 0, 1
        iList = self -> ConvertLocation(location, /TO_LIST_INDEX)
        iNotTaken = min(where((*self.posIsTaken)[iList:*] eq 0))+iList
    endif else begin
        iNotTaken = iNotTaken[0]
    endelse
    
    ;Make the first available position unavailable, then make the chosen position
    ;available. All positions in between remain unavailable.
    (*self.posIsTaken)[iNotTaken] = 1
    (*self.posIsTaken)[iList] = 0
end


;+
;   Remove unused columns and rows.
;-
pro MrPlotLayout::TrimLayout
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Get plot availablility.
    tf_free = self -> IsAvailable(/PLOT_INDEX, ITAKEN=pTaken, NTAKE=nTaken, NFREE=nFree)
    if nFree  eq 0 then return
    if nTaken eq 0 then return
    
    ;Find the maximum taken column and row
    ColRow = self -> ConvertLocation(pTaken, /PLOT_INDEX, /TO_COLROW)
    maxCol = max(ColRow[0,pTaken])
    maxRow = max(ColRow[1,pTaken])

    ;Trim the layout and recalculate the layout positions.
    if maxCol lt self.layout[0] || maxRow lt self.layout[1] $
        then self -> SetProperty, LAYOUT=[maxCol, maxRow]
end


;+
;   The purpose of this method is to describe the layout as well as the locations and
;   positions of the plots currently stored in it.
;-
pro MrPlotLayout::whichLayout
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

;---------------------------------------------------------------------
;LAYOUT //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Print information about the overall layout.
    print, ''
    print, '-------LAYOUT-------'
    print, FORMAT='(%"  Layout:    [%i, %i]")', self.layout
    print, FORMAT='(%"  nPlots:    %i")', self.nPlots
    print, FORMAT='(%"  nFixed:    %i")', self.nFixed
    if n_elements(*self.aspect) ne 0 then print, FORMAT='(%"  Aspect:     %f")', *self.aspect
    print, FORMAT='(%"  XMargin:   [%i, %i]")', self.xmargin
    print, FORMAT='(%"  YMargin:   [%i, %i]")', self.ymargin
    print, FORMAT='(%"  XGap:      %i")', self.xgap
    print, FORMAT='(%"  YGap:      %i")', self.ygap

;---------------------------------------------------------------------
;LAYOUT POSITIONS & LOCATIONS ////////////////////////////////////////
;---------------------------------------------------------------------
    void = self -> IsAvailable(ITAKEN=iTaken, NTAKEN=nTaken)
    if nTaken gt 0 then colrow = self -> ConvertLocation(iTaken, /LIST_INDEX, /TO_COLROW)

    if nTaken gt 0 then begin
        print, ''
        print, '--LOCATIONS--               --POSITIONS--'
        for i = 0, nTaken - 1 do begin
            print, FORMAT='(%"   [ %i, %i]          [%6.4f, %6.4f, %6.4f, %6.4f]")', $
                   colrow[*,i], (*self.layout_positions)[*, colrow[0,i]-1, colrow[1,i]-1]
        endfor
    endif

;---------------------------------------------------------------------
;FIXED POSITIONS & LOCATIONS /////////////////////////////////////////
;---------------------------------------------------------------------
    if self.nFixed gt 0 then begin
        if nTaken eq 0 then print, '--LOCATIONS--               --POSITIONS--'
    
        for i = 0, self.nFixed - 1 do begin
            print, FORMAT='(%"   [%i, %i]          [%6.4f, %6.4f, %6.4f, %6.4f]")', $
                   [-1,i+1], (*self.fixed_positions)[*,i]
        endfor
    endif

;---------------------------------------------------------------------
;GRID OF AVAILABILITY ////////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(*self.posIsTaken) gt 0 then begin
        tf_free = ~reform(*self.posIsTaken, self.layout[0], self.layout[1])

        print, ''
        print, '--LAYOUT AVAILABILITY--'
        for i = 0, self.layout[1]-1 do begin
            print, FORMAT='(a2, ' + strtrim(self.layout[0],2) + '(i1, 1x), a1)', $
                   '| ', tf_free[*,i], '|'
        endfor
    endif
    
    print, ''
end


;+
;   Clean up after the object is destroyed.
;-
pro MrPlotLayout::cleanup
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ptr_free, self.aspect
    ptr_free, self.fixed_positions
    ptr_free, self.layout_positions
    ptr_free, self.posIsTaken
end


;+
;   This method initializes the MrPlotLayout object.
;
; :Keywords:
;       ASPECT:         in, optional, type=float
;                       The aspect ratio (plot height/plot width) of each plot. For square
;                           plots, ASPECT=1.0, for plots that are twice as wide as the are
;                           long, ASPECT=0.5.
;       CALCULATE:      in, optional, type=boolean, default=0
;                       If set, recalculate the plot positions after updating the layout.
;       CHARSIZE:       in, optional, type=float, default=1.5
;                       Fraction of IDL's default character size. Used to determine size
;                           of `XMARGIN`, `YMARGIN`, `XGAP` and `YGAP`.
;       LAYOUT:         in, required, type=intarr(2)
;                       A 2 element vector specifying the number of plots in the vertical
;                           and horizontal directions, [ncols, nrows].
;       XMARGIN:        in, optional, type=fltarr(2), default="[10, 3]"
;                       The x-margins in character widths. [left margin, right margin]
;                           The margins specify the distance between the edge of the plot 
;                           axes and the edge of the plotting window.
;       YMARGIN:        in, optional, type=fltarr(2), default="[4, 2]"
;                       The y-margins in character heights. [bottom, top]
;                           The margins specify the distance between the edge of the plot 
;                           axes and the edge of the plotting window
;       XGAP:           in, optional, type=float, default=14
;                       The horizontal gap between plots in character widths (!D.y_ch_size)
;       YGAP:           in, optional, type=float, default=6
;                       The vertical gap between plots in character heights (!D.y_ch_size)
;-
function MrPlotLayout::init, $
ASPECT = aspect, $
CALCULATE = calculate, $
LAYOUT = layout, $
XMARGIN = xmargin, $
XGAP = xgap, $
YMARGIN = ymargin, $
YGAP = ygap
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif

    ;Set default values
    setDefaultValue, calculate, 1, /BOOLEAN
    setDefaultValue, charsize, 1.5
    setDefaultValue, layout, [0, 0]
    setDefaultValue, xmargin, [10, 3]
    setDefaultValue, ymargin, [4, 2]
    setDefaultValue, xgap, 14
    setDefaultValue, ygap, 6
    
    ;Set object Properties
    self.charsize = charsize
    self.layout = layout
    self.xmargin = xmargin
    self.ymargin = ymargin
    self.xgap = xgap
    self.ygap = ygap
    self.fixed_positions = ptr_new(/ALLOCATE_HEAP)
    self.layout_positions = ptr_new(/ALLOCATE_HEAP)
    self.posIsTaken = ptr_new(/ALLOCATE_HEAP)
    
    if n_elements(aspect) eq 0 $
        then self.aspect = ptr_new(/ALLOCATE_HEAP) $
        else self.aspect = ptr_new(aspect)
    
    ;Calculate the positions
    if keyword_set(calculate) and array_equal(self.layout, [0,0]) eq 0 $
        then self -> CalcLayoutPositions
    
    return, 1                     
end


;+
;   The class definition
;-
pro MrPlotLayout__define
    compile_opt idl2
    
    class = {mrplotlayout, $
             aspect: ptr_new(), $           ;Aspect ratio of the plots.
             charsize: 0.0, $               ;Character size
             fixed_positions: ptr_new(), $  ;Fixed, non-layout-related positions.
             layout: [0,0], $               ;Layout of the plot area [ncols, nrows].
             layout_positions: ptr_new(), $ ;Positions outlined by the 2D plot-layout grid.
             nplots: 0, $                   ;Number of plots displayed.
             nfixed: 0, $                   ;Number of fixed positions.
             posIsTaken: ptr_new(), $       ;Is the Layout_Position filled?
             xgap: 0, $                     ;Size of the gap between plots in the x-direction.
             xmargin: [0,0], $              ;Size of the [left, right] margins.
             ygap: 0, $                     ;Size of the gap between plots in the y-direction.
             ymargin: [0,0]}                ;Size of the [top, bottom] margins.
end