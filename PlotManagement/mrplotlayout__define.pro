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
;       Get an empty location and its position::
;           MyObj -> GetPositions, location, POSITION=position
;
;       Get an unused "fixed" plot location::
;           MyObj -> GetPositions, location, /FIXED
;
;       Get a "fixed" location for a given position::
;           position = [0.1, 0.1, 0.85, 0.85]
;           MyObj -> SetPositions, location, POSITION=position, /FIXED
;
;       Get an unused location and its position. Add them to the lists::
;           MyObj -> GetPositions, location, POSITION=position
;           MyObj -> SetPositions, location, POSITION=position, /ADD
;
;       Gen an unused location and its position. Add them to the lists::
;           MyObj -> SetPositions, location, POSITION=position, /ADD
;
;       Change the location of an "Auto-Updating" plot. Retrieve its position::
;           MyObj -> SetPositions, [1,1], SETLOCATION=[2,1], POSITION=position
;
;       "Fix" an "Auto-Updating" plot::
;           MyObj -> SetPositions, [1,1], SETLOCATION=[-1,-1]
;
;       "Fix" an "Auto-Updating" plot. Change its position::
;           old_location = [1,2]
;           new_position = [0.1, 0.1, 0.85, 0.85]
;           MyObj -> SetPositions, old_location, SETPOSITION=new_position
;
;       Change aspects of the plot layout::
;           MyObj -> SetPositions, XMARGIN=[10,4], YMARGIN=[4,2], XGAP=3, YGAP=0
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
;                           them here: the FillHoles, FindLayoutHoles, GetPlotIndexAvailability,
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
;       08/21/2013  -   Renamed GetMaxLocation to GetPlotIndexAvailability and repurposed
;                           it to return the plot indices and number of un/available
;                           positions. RePosition can now accept individual indices. Added
;                           the ConvertLocation method. Print a grid of 1's and 0's
;                           indicating if a spot is available or unavailable. Added
;                           POSITION keyword to Make_Location. Renamed RemovePositions to
;                           RemoveFromLayout. Combined AddCols and AddRows into the
;                           ExpandLayout method.
;                           
;-
;*****************************************************************************************
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
;       LIST_INDEX:     in, optional, type=boolean, default=0
;                       If set, `LOCATION` is a vector of list index locations. The list
;                           index is the index at which the plot's object reference is
;                           stored. See the "Plots_Present" method.
;       PLOT_INDEX:     in, optional, type=boolean, default=0
;                       If set, `LOCATION` is a vector of 1D plot locations. In this case,
;                           the upper left plot is index 1, and the index number increases
;                           first downward, then across.
;       TO_COLROW:      in, optional, type=boolean, default=0
;                       If set, then `LOCATION` will be converted to a [Col, Row]
;                           location.
;       TO_LIST_INDEX:  in, optional, type=boolean, default=0
;                       If set, then `LOCATION` will be converted to its corresponding
;                           list index number.
;       TO_PLOT_INDEX:  in, optional, type=boolean, default=0
;                       If set, then `LOCATION` will be converted to its corresponcing
;                           plot index number.
;-
pro MrPlotLayout::AddToLayout, location, position
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    fixed = 0
;---------------------------------------------------------------------
;Get a Location? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If no location was provided
    if n_elements(location) eq 0 then begin
        ;Determine if we need a fixed position
        if n_elements(position) gt 0 then fixed = 1

        ;Get a position and location
        self -> GetPositions, location, LAYOUT=layout, POSITION=position, $
                                        FIXED=fixed, /UPDATE_LAYOUT

;---------------------------------------------------------------------
;Get a Position? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
        
    ;If an auto-updating location was provided
    endif else if location[0] gt 0 then begin
        
        ;Make room for the location given. The layout will be updated and returned
        ;so that positions can be calculated properly.
        self -> Make_Location, location, LAYOUT=layout, POSITION=position, /UPDATE_LAYOUT
    endif

;---------------------------------------------------------------------
;Add Position and Location ///////////////////////////////////////////
;---------------------------------------------------------------------

    ;Added the fixed position
    if fixed then begin
        self.nFixed += 1
        *self.fixed_positions = [[*self.fixed_positions], [position]]
        
    ;Indicate that a layout position has been taken
    endif else begin
        iList = self -> ConvertLocation(location, /TO_LIST_INDEX)
print, 'location = ', location
print, 'ilist = ', ilist
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
    
    ;Return if the layout is not yet valid (Must have at least 1 column and 1 row).
    if self.layout[0] eq 0 || self.layout[1] eq 0 $
        then message, FORMAT='(%"Cannot calculate position. Layout [%i, %i] at least' + $
                                'one column and one row.")', self.layout

    ;Calculate positions
    *self.layout_positions = MrPlotLayout(self.layout, $
                                          ASPECT = *self.aspect, $
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
        self.nPlots -= nLayout
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
;Fixed Position //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Fixed locations
    if location[0] lt 0 then begin
        result = location[1]-1
        return, result
    endif

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
    ;Not the PLOT_INDEX is row-major while LIST_INDEX is column-major
    ;

    ;TO PLOT INDEX
    if keyword_set(to_plot_index) then begin
        case 1 of
            keyword_set(list_index): begin
                colrow = twoD_to_oneD_index(location, layout, /oneD_to_twoD, /COL_MAJOR) ;row-major location -> [col,row]
                result = twoD_to_oneD_index(colrow, layout)                              ;[col,row] -> col-major index
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
                colrow = twoD_to_oneD_index(location, layout, /oneD_to_twoD)
                result = twoD_to_oneD_index(colrow, layout, /COL_MAJOR)
            endcase
            keyword_set(list_index): result = location
            else                   : result = twoD_to_oneD_index(location-1, layout, /COL_MAJOR)
        endcase
    endif
    
    return, result
end


;+
;   The purpose of this method is to determine which positions are available and
;   which are unavailable.
;
; :Keywords:
;       FIXED:          in, optional, type=Boolean, default=0
;                       If set, fixed positions will be searched instead of layout
;                           positions. In this case, `NUNAVAILABLE` is the the only
;                           value that will be returned.
;       PITAKEN:        out, optional, type=intarr
;                       Plot indices of layout positions that are currently occupied.
;       NTAKEN:         out, optional, type=intarr
;                       Number of layout positions that are currently occupied.
;       NFREE:          out, optional, type=intarr
;                       Number of layout positions that are currently unoccupied.
;
; :Returns:
;       PITAKEN:        Plot indices of layout positions that are currently unoccupied.
;-
function MrPlotLayout::GetPlotIndexAvailability, $
FIXED = fixed, $
PITAKEN = piTaken, $
NFREE = nFree, $
NTAKEN = nTaken
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, !Null
    endif

;---------------------------------------------------------------------
;Fixed Positions /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Return highest used plot number
    if keyword_set(fixed) then begin
        nTaken = self.nFixed
        return, !Null
    endif

;---------------------------------------------------------------------
;Layout Positions ////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Special case if the layout is empty
    if array_equal(self.layout, [0,0]) then begin
        nFree = 0
        piFree = !Null
        nTaken = 0
        piTaken = !Null
        return, nFree
    endif
    
    ;Which are un/available?
    piFree = where(*self.posIsTaken eq 0, nFree, /NULL, $
                   COMPLEMENT=piTaken, NCOMPLEMENT=nTaken)
    
    ;Convert from list index to plot index.
    if nFree  gt 0 then piFree += 1
    if nTaken gt 0 then piTaken += 1
  
    return, piFree
end


;+
;   The purpose of this method is to provide a means of generating new plot locations
;   within the 2D plotting grid.
;
; :Params:
;       LOCATION:           in, out, optional, type=long
;                           The [col, row] location of the plot. If defined, `POSITION`
;                               will contain the position of a plot placed at `LOCATION`.
;                               If not defined, then it is a named variable into which
;                               the newly generated location will be returned. The
;                               location will either be an auto-updating location or a 
;                               fixed location, as determined by the `FIXED` keyword.
;
; :Keywords:
;       FIXED:              in, optional, type=boolean, default=0
;                           If set and `POSITION` is provided, `LOCATION` will be updated
;                               to contain the location of the provided position. If not
;                               found, `LOCATION` will be filled with the next available
;                               fixed position location.
;       LAYOUT:             out, optional, type=intarr(2)
;                           A two-element vector [col, row] indicating the number of
;                               columns and rows in the plot layout. Will be different
;                               from the current layout if more space was needed to
;                               create the new location.
;       POSITION:           out, optional, type=fltarr(4)
;                           The position of the plot indicated by `LOCATION`. 
;                           The lower-left and upper-right corners of a plot in normal
;                               coordinates. If `FIXED` is set, then this is the `POSITION`
;                               of the fixed plot for which a location is to be generated.
;       UPDATE_LAYOUT:      in, optional, type=boolean, default=0
;                           If set, the plot layout will be adjusted to accomodate the
;                               location and position being retrieved. Plot positions
;                               will be recalculated to fit the new layout. This applies
;                               only when `FIXED`=0.
;                               
;       _REF_EXTRA:         in, optional, type=Structure
;                           Any keyword accepted by MrPlotLayout.pro
;
; :Uses:
;   Uses the following external programs::
;       MrPlotLayout.pro
;-
pro MrPlotLayout::GetPositions, location, $
FIXED = fixed, $
LAYOUT = layout, $
POSITION = position, $
UPDATE_LAYOUT = update_layout, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    npos = n_elements(position)
    update_layout = keyword_set(update_layout)

;---------------------------------------------------------------------
;Get a Fixed Location and its Position ///////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(fixed) then begin

        ;Search for an existing location                    
        if npos gt 0 then for i = 0, self.nFixed - 1 do begin
            if array_equal((*self.fixed_positions)[*,i], position) $
                then location = [-1, i+1]
        endfor
        
        ;If no existing position matches, then get a new location
        if n_elements(location) eq 0 then begin
            void = self -> GetPlotIndexAvailability(/FIXED, NTAKEN=nTaken)
            location = [-1, nTaken+1]
        endif

;---------------------------------------------------------------------
;Get an Auto-Updating Location and its Position //////////////////////
;---------------------------------------------------------------------
    endif else begin
        ;If LOCATION was given, find a layout to fit it. If not, get a
        ;location at the end of the line. Add a row if necessary.
        self -> Make_Location, location, $
                               LAYOUT=layout, $
                               POSITION=position, $
                               UPDATE_LAYOUT=update_layout
    endelse
end


;+
;   The purpose of this method is to retreive object properties.
;
; :Keywords:
;       ASPECT:         out, optional, type=float
;                       The aspect ratio (plot height/plot width) of each plot. For square
;                           plots, ASPECT=1.0, for plots that are twice as wide as the are
;                           long, ASPECT=0.5.
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
    if arg_present(layout)   then layout  = self.layout
    if arg_present(nplots)   then nplots  = self.nplots
    if arg_present(xgap)     then xgap    = self.xgap
    if arg_present(xmargin)  then xmargin = self.xmargin
    if arg_present(ygap)     then ygap    = self.ygap
    if arg_present(ymargin)  then ymargin = self.ymargin 
    if arg_present(location) then $
        if n_elements(thisLocation) ne 0 and n_elements(*self.layout_positions) ne 0 $
            then location = (*self.layout_positions)[*, location[0]-1, location[1]-1]
end


;+
;   The purpose of this method is to provide a means of generating new plot locations
;   within the 2D plotting grid.
;
; :Params:
;       LOCATION:           in, out, optional, type=long
;                           The [col, row] location of the position that is being checked.
;                               If LOCATION[0] < 0 then the Fixed positions will be
;                               checked for availability.
;
; :Returns:
;       TF_AVAILABLE:       1 if the position indicated by `LOCATION` is avaialble,
;                           0 if not.
;-
function MrPlotLayout::IsAvailable, location
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif
    
    ;Fixed locations.
    if location[0] lt 0 then begin
        tf_available = location[1] gt self.nFixed
    
    ;Layout locations.
    endif else begin
    
        ;If the layout is empty, then the location is avaialble
        if array_equal(self.layout, [0,0]) then return, 1
    
        ;If LOCATION is outside of the layout, then the location is aviable
        if self -> PlotExists(location) eq 0 then return, 1
    
        ;Check if the position has been taken already
        iList = self -> ConvertLocation(location, /TO_LIST_INDEX)
        tf_available = ~(*self.posIsTaken)[iList]
    endelse
    
    return, tf_available
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
                iList = self -> ConvertLocation(location, /TO_LIST_INDEX)
                piAvailable = self -> GetPlotIndexAvailablility(NAVAILABLE=nAvailable)
                
                ;If there are no available locations after the desired one, add a row
                ;so that current positions can be shifted out of the way to make room
                ;for the new one.
                if nAvailable eq 0 || (max(iAvailable gt iList) eq 0) $
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
        piFree = self -> GetPlotIndexAvailability(NFREE=nFree, $
                                                  PITAKEN=piTaken, $
                                                  NTAKEN=nTaken)
        
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
            ;Take the highest available location
            piLoc = piTaken[-1] + 1
            
        ;---------------------------------------------------------------------
        ;Outside Current Layout? /////////////////////////////////////////////
        ;---------------------------------------------------------------------
            if piLoc gt self.layout[0]*self.layout[1] then begin
            
                ;Update the layout?
                if keyword_set(update_layout) $
                    then self -> ExpandLayout, 0, 1 $
                    else layout = self.layout + [0,1]
                
                ;Find the location in the new layout.
                piLoc = RePosition(self.layout, self.layout-[0,1], piLoc)
                location = ConvertLocation(piLoc, /PLOT_INDEX, /TO_COLROW)
            
        ;---------------------------------------------------------------------
        ;Inside Current Layout? //////////////////////////////////////////////
        ;---------------------------------------------------------------------
            endif else begin
                location = ConvertLocation(piLoc, /PLOT_INDEX, /TO_COLROW)
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
    nRemove = n_elements(location)
    
    ;Check if a position exists at the given location.
    tf_free = self -> IsAvailable(location)
    if tf_free eq 1 then return

;---------------------------------------------------------------------
;Fixed Positions /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if location[0] lt 0 then begin
        ipAll = indgen(self.nFixed)+1
        iKeep = where(ipAll ne location, nKeep)

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
        ipAll = indgen(self.nPlots - self.nFixed)+1
        iKeep = where(ipAll ne plot_index, nKeep)
        
        if nKeep eq 0 then begin
            self -> ClearLayout, /LAYOUT
        endif else begin
            self.nPlots -= nRemove
            (*self.posIsTaken)[plot_index-1] = 0
        endelse
    endelse
    
    ;Fill holes in the layout?
    if fillHoles then self -> FillHoles
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

    ;Convert the indices of the taken positions to [col, row] locations.
    oldTaken_colrow = self -> ConvertLocation(iOldTaken, old_layout, /LIST_INDEX, /TO_COLROW)
    iNewTaken = self -> ConvertLocation(oldTaken_colrow, new_layout)

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
    if n_elements(aspect)  ne 0 then *self.aspect = aspect
    if n_elements(xgap)    ne 0 then self.xgap = xgap
    if n_elements(xmargin) ne 0 then self.xmargin = xmargin
    if n_elements(ygap)    ne 0 then self.ygap = ygap
    if n_elements(ymargin) ne 0 then self.ymargin = ymargin
    
    ;Make sure plot locations are invariant to changes in the layout
    if n_elements(layout) ne 0 then begin
    
        ;Make sure the new layout will be valid.
        if layout[0] eq 0 || layout[1] eq 0 then $
            message, 'LAYOUT must have at least 1 column and 1 row.'
        
        ;Resposition all of the existing plots        
        *self.posIsTaken = self -> RePosition(self.layout, layout)
        if *self.posIsTaken eq !Null then *self.posIsTaken = bytarr(layout[0]*layout[1])

        ;Change the layout
        self.layout = layout
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
        iList = self -> ConvertLocation(location, /TO_INDEX)
        iNotTaken = min(where((*self.posIsTaken)[iList:*] eq 0))
    endif else begin
        iNotTaken = iNotTaken[0]
    endelse
    
    ;Make the first available position unavailable, then make the chosen position
    ;available. All positions in between remain unavailable.
    (*self.posIsTaken)[iNotTaken] = 1
    (*self.posIsTaken)[iList] = 0
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
    print, FORMAT='(%"  XMargins:  [%i, %i]")', self.xmargin
    print, FORMAT='(%"  YMargins:  [%i, %i]")', self.ymargin
    print, FORMAT='(%"  XGaps:     %i")', self.xgap
    print, FORMAT='(%"  YGaps:     %i")', self.ygap

;---------------------------------------------------------------------
;LAYOUT POSITIONS & LOCATIONS ////////////////////////////////////////
;---------------------------------------------------------------------
    void = self -> GetPlotIndexAvailability(PITAKEN=piTaken, NTAKEN=nTaken)
    if nTaken gt 0 then colrow = self -> ConvertLocation(piTaken, /PLOT_INDEX, /TO_COLROW)

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
        isTaken = reform(*self.posIsTaken, self.layout[0], self.layout[1])
        print, ''
        print, '--LAYOUT AVAILABILITY--'
        for i = 0, self.layout[1]-1 do begin
            print, FORMAT='(a2, ' + strtrim(self.layout[0],2) + '(i1, 1x), a1)', $
                   '| ', ~isTaken[*,i], '|'
        endfor
    endif
end


;+
;   Adjust the layout of the plots by moving plots to fill in gaps and then
;   removing any empty columns or rows.
;
; :Private:
;-
pro MrPlotLayout::FillHoles
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;If there are no plots, then there is nothing to adjust
    nplots = n_elements(*self.plot_positions)/4
    if nplots eq 0 then return
    
    ;find where holes are in the plot layout
    holes = self -> FindLayoutHoles(NHOLES=nholes, PLOT_NUMBERS=plot_numbers)
    
    plot_numbers_new = plot_numbers
    
    ;while holes remain...
    while nholes gt 0 do begin
        ;only adjust the plots that are after the hole
        ;determine how many locations to shift the plots to fill the first hole, then fill it
        to_adjust = where(plot_numbers_new gt holes[0], n_to_adjust)

        if n_to_adjust ne 0 then shift_size = plot_numbers_new[to_adjust[0]] - holes[0] $
                            else shift_size = 0
        plot_numbers_new[to_adjust[0]:*] -= shift_size

        ;reduce the number of holes and get rid of the filled hole location
        nholes -= 1
        if nholes gt 0 then holes = holes[1:*]
    endwhile

    ;fit the plots within the current layout scheme at their new locations
    ;must go from a 1-based PLOT_NUMBER to a 0-based index, then back
    exists = self -> PlotExists(plot_numbers_new, colrow, /PLOT_INDEX, /TO_COLROW)
    *self.plot_locations = colrow

    ;if any columns or rows have become empty (i.e. filled with holes) then trim them
    ;set the new layout (1-based). Update the positions of all plots.
    void = self -> GetPlotIndexAvailability(MAXCOL=maxCol, MAXROW=maxRow)
    self -> SetProperty, LAYOUT=[maxCol, maxRow]
end


;+
;       Find the 1-based plot numbers (ordered top -> bottom, left -> right) of any
;       hole in the plotting grid
;
; :Private:
;
; :Keywords:
;
;       NHOLES:             out, optional, type=int
;                           the number of holes found
;       PLOT_NUMBERS:       out, optional, type=intarr()
;                           the plot numbers for all of the plots
;
; :Returns:
;
;       LAYOUTHOLES     [array] the 1-based plot grid number where no plots exist
;-
function MrPlotLayout::FindLayoutHoles, $
NHOLES = nholes, $
PLOT_NUMBERS = plot_numbers
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        nholes = 0
        return, -1
    endif

    ;If there are no plots, then there are no holes.
    nplots = n_elements(*self.plot_positions)/4
    if nplots eq 0 then begin
        nholes = 0
        return, -1
    endif

    ;Total number of possible plots and their plot numbers.
    nTot = self.layout[0] * self.layout[1]
    all_plot_numbers = indgen(nTot) + 1
    
    ;Find the holes
    theseIndices = where((*self.plot_locations)[0,*] ne -1, count)
    if count ne 0 then begin
        exists = self -> PlotExists((*self.plot_locations)[*,theseIndices], plot_numbers, /TO_PLOT_INDEX)
        void = ismember(plot_numbers, all_plot_numbers, NONMEMBER_INDS=layoutHoles, N_NONMEMBERS=nholes)
    endif

    return, layoutHoles
end


;+
;   The purpose of this method is to change/replace the position and/or location of a
;   plot that already exists within the 2D plotting grid.
;
; :Params:
;       LOCATION:           in, optional, type=long
;                           The [col, row] location of the plot to replace.
;
; :Keywords:
;       POSITION:           out, optional, type=fltarr(4)
;                           The lower-left and upper-right corners of a plot in normal
;                               coordinates. If `SETLOCATION` is in use, then this is the
;                               new position of the plot indicated by `LOCATION`.
;       SETLOCATION:        in, optional, type=lonarr(2)
;                           Use this kewyord to set the [col, row] location of the plot
;                               indicated by `LOCATION`::
;                                   `LOCATION`[0] > 0, `SETLOCATION`[0] < 0 
;                                       An auto-updating plot will become fixed at
;                                       its present position.
;                                   `LOCATION`[0] < 0, `SETLOCATION`[0] > 0 
;                                       A fixed plot will be put into the auto-updating
;                                       grid at the location indicated.
;                                   `LOCATION`[0] > 0, `SETLOCATION`[0] > 0 
;                                       An auto-updating plot will be moved within the 
;                                       grid to the location indicated.
;                                   `LOCATION`[0] < 0, `SETLOCATION`[0] < 0 
;                                       Ignored. A fixed plot is fixed no matter what.
;       SETPOSITION:        in, optional, type=fltarr(4)
;                           A four-element vector in the form [x0, y0, x1, y1] specifying
;                               the location of the lower right [x0, y0] and upper-left
;                               [x1, y1] corners of a plot. If set, then the plot given
;                               by `LOCATION` will have its position changed to SETPOSITION.
;                               If `LOCATION` is that of an auto-updating plot, it will
;                               become fixed. If a location has been changed, it will be
;                               returned in `SETLOCATION`.
;
; :Uses:
;   Uses the following external programs::
;       MrPlotLayout.pro
;-
pro MrPlotLayout::ReplacePositions, location, $
POSITION = position, $
SETLOCATION = setLocation, $
SETPOSITION = setPosition
    compile_opt idl2

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

;---------------------------------------------------------------------
;If LOCATION is Present //////////////////////////////////////////////
;---------------------------------------------------------------------
    
    exists = self -> PlotExists(location)
    if exists eq 0 then message, 'LOCATION does not exist.'
            
;---------------------------------------------------------------------
;Set a [Col, Row] Location ///////////////////////////////////////////
;---------------------------------------------------------------------

    if n_elements(SetLocation) ne 0 then begin
        ;Fixed -> Fixed
        if location[0] lt 0 and SetLocation[0] lt 0 then begin
            ;Ignored
            
        ;Auto-Update -> Fixed
        endif else if location[0] gt 0 and SetLocation[0] lt 0 then begin
            loc = self -> GetPlotIndexAvailability(/FIXED)
            location = [-1, loc-1]
            position = (*self.plot_positions)[*,index]
        
        
        ;Auto-Update -> Auto-Update || Fixed -> Auto-Update
        endif else begin
            ;Find a layout that will accommodate the new location
            ;(the layout will be updated later)
            self -> Make_Location, setLocation, LAYOUT=layout, UPDATE_LAYOUT=0
        
            ;Calculate the new position.
            position = MrPlotLayout(layout, setLocation, $
                                    XMARGIN=self.xmargin, XGAP=self.xgap, $
                                    YMARGIN=self.ymargin, YGAP=self.ygap, $
                                    _STRICT_EXTRA=extra)
        endelse
        
        ;Set the new location and position.
        (*self.plot_locations)[*,index] = setLocation
        (*self.positions)[index] = position
        
;---------------------------------------------------------------------
;Set a Position //////////////////////////////////////////////////////
;---------------------------------------------------------------------

    endif else if n_elements(SetPosition) ne 0 then begin
    
        ;Auto-Updating -> Fixed
        if (*self.plot_locations)[0,index] gt 0 then begin
            loc = self -> GetPlotIndexAvailability(/FIXED)
            SetLocation = [-1, loc-1]
        
        ;Fixed -> Fixed
        endif else SetLocation = location
        
        ;Update the location and position
        (*self.plot_locations)[*,index] = SetLocation
        (*self.plot_positions)[*,index] = SetPosition
        
    endif
    
    ;If the layout was changed, we need to update the other positions as well
    if n_elements(layout) ne 0 then self -> SetProperty, LAYOUT=layout
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
    setDefaultValue, layout, [0, 0]
    setDefaultValue, xmargin, [10, 3]
    setDefaultValue, ymargin, [4, 2]
    setDefaultValue, xgap, 14
    setDefaultValue, ygap, 6
    
    ;Set object Properties
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
    if keyword_set(calculate) then self -> CalcLayoutPositions
    
    return, 1                     
end


;+
;   The class definition
;-
pro MrPlotLayout__define
    compile_opt idl2
    
    class = {mrplotlayout, $
             aspect: ptr_new(), $           ;Aspect ratio of the plots.
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