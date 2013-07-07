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
;                           them here: the adjustLayout, findLayoutHoles, getMaxLocation,
;                           Make_Location, plotExists, setPositions, shiftPlots, and 
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
;                           method. As such, a call to the adjustLayout method was not
;                           working. Now, the layout is not adjusted when /ADD is set. - MRA
;       07/07/2013  -   Separated the SetPositions method into the Add, Remove, Replace,
;                           and Clear methods. Added the whichLayout method. - MRA
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
pro MrPlotLayout::AddPositions, location, position
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

;---------------------------------------------------------------------
;Get a Location? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If no location was provided
    if n_elements(location) eq 0 then begin
        ;Determine if we need a fixed or auto-updating position
        if n_elements(position) eq 0 $
            then fixed = 0 $
            else fixed = 1

        ;Get a position and location (layout will be set later).
        self -> GetPositions, location, LAYOUT=layout, POSITION=position, FIXED=fixed

;---------------------------------------------------------------------
;Get a Position? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
        
    ;If an auto-updating location was provided
    endif else if location[0] gt 0 then begin
        
        ;Make room for the location given. The layout will be updated and returned
        ;so that positions can be calculated properly.
        self -> Make_Location, location, LAYOUT=layout
        
        ;If no position was provided, get the position associated with LOCATION
        if n_elements(position) eq 0 then $
            position = (*self.layout_positions)[*, location[0]-1, location[1]-1]
            
        ;Do not attempt to fill holes, i.e. preserve layout due to added location.
        adjust_layout = 0
    endif

;---------------------------------------------------------------------
;Add Position and Location ///////////////////////////////////////////
;---------------------------------------------------------------------

    ;Add a new location.
    if n_elements(*self.plot_locations) eq 0 $
        then *self.plot_locations = location $
        else *self.plot_locations = [[*self.plot_locations], [location]]
        
    ;Add a new position.
    if n_elements(*self.plot_positions) eq 0 $
        then *self.plot_positions = position $
        else *self.plot_positions = [[*self.plot_positions], [position]]
    
    ;Update the positions if the layout has been adjusted.
    if n_elements(layout) ne 0 then self -> SetProperty, LAYOUT=layout
    
    ;Increase the plot count
    self.nplots += 1
end


;+
;   The purpose of this method is to recalculate the plot positions. This is necessary
;   when some aspect of the plot layout changes. Any changes to the layout should have
;   already been applied via the Set, Add, or Remove mehtods.
;-
pro MrPlotLayout::ApplyPositions
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

;---------------------------------------------------------------------
;Apply New Positions /////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;If there are no plots left, then there is nothing to reposition
    nPlots = n_elements(*self.plot_locations)/2
    if nPlots eq 0 then return
    
    ;Find the Auto-Adjusting plots.
    thesePlots = where((*self.plot_locations)[0,*] ne -1, nAuto)

    ;Reposition each Auto-Updating plot
    for i = 0, nAuto - 1 do begin
        thisPlot = thesePlots[i]
        thisLoc = (*self.plot_locations)[*,thisPlot]
        
        ;Reposition the plots
        (*self.plot_positions)[*,i] = (*self.layout_positions)[*, thisLoc[0]-1, thisLoc[1]-1]
    endfor
end



;+
;   Adjust the layout of the plots by moving plots to fill in gaps and then
;   removing any empty columns or rows.
;
; :Private:
;-
pro MrPlotLayout::adjustLayout
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
    holes = self -> findLayoutHoles(NHOLES=nholes, PLOT_NUMBERS=plot_numbers)
    
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
    exists = self -> plotExists(plot_numbers_new, colrow, /PLOT_INDEX, /TO_COLROW)
    *self.plot_locations = colrow

    ;if any columns or rows have become empty (i.e. filled with holes) then trim them
    ;set the new layout (1-based). Update the positions of all plots.
    void = self -> getMaxLocation(MAXCOL=maxCol, MAXROW=maxRow)
    self -> SetProperty, LAYOUT=[maxCol, maxRow]
end


;+
;   The purpose of this method is to calculate the plot positions.
;-
pro MrPlotLayout::CalcPositions
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Return if no layout has been provided.
    if array_equal(self.layout, [0,0]) then return

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
;-
pro MrPlotLayout::ClearPositions
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Reset the position and location pointers.
    ptr_free, self.plot_locations, self.plot_positions
    self.plot_locations = ptr_new(/ALLOCATE_HEAP)
    self.plot_positions = ptr_new(/ALLOCATE_HEAP)
    
    ;Reset the layout.
    self.nplots = 0
    self.layout = [0,0]
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
function MrPlotLayout::findLayoutHoles, $
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
        exists = self -> plotExists((*self.plot_locations)[*,theseIndices], plot_numbers, /TO_PLOT_INDEX)
        void = ismember(plot_numbers, all_plot_numbers, NONMEMBER_INDS=layoutHoles, N_NONMEMBERS=nholes)
    endif

    return, layoutHoles
end


;+
;   The purpose of this method is to provide a means of generating new plot locations
;   within the 2D plotting grid.
;
; :Params:
;       LOCATION:           in, out, optional, type=long
;                           The [col, row] location of the plot. If defined, `POSITION`
;                               will contain the position of a plot if placed at `LOCATION`.
;                               If not defined, then it is a named variable into which
;                               the newly generated location will be returned. The
;                               location will either be an auto-updating location or a 
;                               fixed location, as determined by the `FIXED` keyword.
;
; :Keywords:
;       FIXED:              in, optional, type=boolean, default=0
;                           Return the location and position of a "fixed" plot instead
;                               of an "auto-updating" location.
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
    nplots = n_elements(*self.plot_positions)/4

;---------------------------------------------------------------------
;Get a Fixed Location and its Position ///////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(fixed) then begin

        ;Search for an existing location                    
        if npos gt 0 then for i = 0, nplots - 1 do begin
            if array_equal((*self.plot_positions)[*,i], position) $
                then location = (*self.plot_locations)[*,i]
        endfor
        
        ;If no existing position matches, then get a new location
        if n_elements(location) eq 0 then begin
            loc = self -> getMaxLocation(/NEGATIVE)
            location = [-1, loc-1]
        endif

;---------------------------------------------------------------------
;Get a Auto-Updating Location and its Position ///////////////////////
;---------------------------------------------------------------------
    endif else begin
        ;Get the lowest available position and location.
        self -> Make_Location, location, LAYOUT=layout, UPDATE_LAYOUT=0

        ;Calculate the plot position.
        position = MrPlotLayout(layout, location, $
                                XMARGIN=self.xmargin, XGAP=self.xgap, $
                                YMARGIN=self.ymargin, YGAP=self.ygap, $
                                _STRICT_EXTRA=extra)
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
    if arg_present(aspect) and n_elements(*self.aspect) ne 0 then aspect = *self.aspect
    if arg_present(layout)   then layout = self.layout
    if arg_present(xgap)     then xgap = self.xgap
    if arg_present(xmargin)  then xmargin = self.xmargin
    if arg_present(ygap)     then ygap = self.ygap
    if arg_present(ymargin)  then ymargin = self.ymargin 
    if arg_present(location) then $
        if n_elements(thisLocation) ne 0 and n_elements(*self.layout_positions) ne 0 $
            then location = (*self.layout_positions)[*, location[0]-1, location[1]-1]
end


;+
;   The purpose of this method is to find the plot number of the last plot, 
;   ordered top -> bottom, left -> right.
;
; :Keywords:
;
;       MAXCOL:         out, optional, type=int
;                       the 1 based column number where the last plot is found
;       MAXROW:         out, optional, type=int
;                       the 1 based row number where the last plot is found
;       NEGATIVE:       in, optional, type=Boolean, default=0
;                       Search through the plots with user-supplied positions. Return
;                           the last location of such plots.
;
; :Returns:
;
;       NUMBER:         the 1-based plot number of the last plot,
;                       ordered top -> bottom, left -> right (see twoD_to_oneD_index.pro)
;-
function MrPlotLayout::getMaxLocation, $
MAXCOL = maxcol, $
MAXROW = maxrow, $
NEGATIVE = negative
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, -1
    endif
    
    nplots = n_elements(*self.plot_positions)/4
    
    ;Search through the user-supplied positions, which are stored at negative locations.
    ;Return the most negative row. (These plots are given a location of [-1, -i], where
    ;"i" is the i-th plot with a user-supplied position.)
    if keyword_set(negative) then begin
        number = 0
        for i = 0, nplots - 1 do begin
            loc = (*self.plot_locations)[*,i]
            if loc[1] lt number then number = loc[1]
        endfor
            
        return, number
    endif
    
    ;keep track of the highest plot number reached
    max_pnum = 0
    maxcol = 0
    maxrow = 0

    ;step through each plot lcocation...
    for i = 0, nplots - 1 do begin
        ;get the biggest plot number
        colrow = (*self.plot_locations)[*,i]
        exists = self -> plotExists(colrow, plot_number, /TO_PLOT_INDEX)
        if plot_number gt max_pnum then max_pnum = plot_number
        if colrow[0] gt maxcol then maxcol = colrow[0]
        if colrow[1] gt maxrow then maxrow = colrow[1]
    endfor
  
    return, max_pnum
end


;+
;       The purpose of this program is to determine where a plot should be displayed
;       on the plotting grid::
;           - If a plot already exists in the given location, then all plots are
;               shifted out of the way to make room.
;           - If a location was not specified, the lowest available plot location will
;               be filled.
;           - An extra row will be added to make room, if necessary.
;           - The plot layout will be automatically adjusted to fit the location provided
;               by adding more rows and columns when necessary.
;
; :Params:
;       LOCATION:           in, out, optional, type=long/lonarr(2)
;                           If provided and defined, then this is a named variable
;                               containing the [Col, Row] location of a plot. If the plot
;                               does not fit within the current plot layout, the layout
;                               will be expanded to contain the plot. If provided and 
;                               undefined, then this is a named variable into which the
;                               lowest available [Col, Row] plot location will be returned.
;
; :Keywords:
;       UPDATE_LAYOUT:      in, optional, type=boolean, default=1
;                           Adjust the layout and recalculate plot positions after
;                               determining the location.
;       LAYOUT:             out, optional, type=intarr(2)
;                           The layout needed to fit the `LOCATION`. Provided as
;                               a means of returning the new layout if `UPDATE_LAYOUT`=0
;
; :Uses:
;   Uses the following external programs::
;       SetDefaultValue.pro (Coyote Graphics)
;-
pro MrPlotLayout::Make_Location, location, $
UPDATE_LAYOUT = update_layout, $
LAYOUT = layout
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
    layout_init = self.layout
    setDefaultvalue, update_layout, 1, /BOOLEAN
    
    ;SELF.LAYOUT is initialized to [0,0], which is an invalid plot location. If this
    ;is still the case, and LOCATION was not given, then make sure location is defined.
    if array_equal(self.layout, [0,0]) && n_loc eq 0 then begin
        location = [1,1]
        n_loc = 2
    endif

;---------------------------------------------------------------------
;Adjust Plotting Layout and Find a Place to Put the Plot /////////////
;---------------------------------------------------------------------
    ;
    ; Not that even if ADJUST_LAYOUT = 0, we still need to change
    ; SELF.LAYOUT so that PLOT_EXISTS can convert between plot
    ; index numbers and [col, row] locations.
    ;
    
    ;if LOCATION is in [col, row] format, then make sure it fits within the plotting grid.
    ;Adjust the plot layout to accomodate the location if necessary.
    if n_loc eq 2 then begin
        if location[0] gt self.layout[0] then self.layout[0] = location[0]
        if location[1] gt self.layout[1] then self.layout[1] = location[1]
        
        ;Check if there is a plot in this location. If there is, shift the plots.
        exists = self -> plotExists(location)
        if exists and keyword_set(update_layout) then self -> shiftPlots, location
    
    ;if no location was supplied, find the next available location
    endif else if n_loc eq 0 then begin
        ;find the highest unused plot number, then increase it by 1 to get an empty spot
        plot_num = self -> getMaxLocation() + 1
        
        ;find how many plot spaces there are.
        nlocs = self.layout[0] * self.layout[1]
        
        ;if the highest unused plot number is greater than the spaces available, then
        ;add an additional row to the plot and turn the plot number into its
        ;[col, row] location.
        if plot_num gt nlocs then self.layout += [0,1]
        exists = self -> plotExists(plot_num, location, /PLOT_INDEX, /TO_COLROW)
        
    ;otherwise, throw an error message
    endif else message, 'if given, Location must be a 2 element vector [col, row]'
    
    layout = self.layout
    
    ;Calculate the new positions if requested.
    if keyword_set(update_layout) $
        then self -> SetProperty, LAYOUT=layout $
        else self.layout = layout_init
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
;       ADJUST_LAYOUT:  in, optional, type=boolean, default=0
;                       Sometimes removing a plot from the 2D auto-updating grid can leave
;                           holes in the layout. Set this keyword to 1 (one) to adjust
;                           plot locations so that those holes are filled.
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
;
; :Returns:
;       PLOT_EXISTS:    Tells whether the plot exists (1) or not (0).
;
; :Uses:
;   Uses the following external programs::
;       ismember.pro
;-
pro MrPlotLayout::RemovePositions, location, $
ADJUST_LAYOUT = adjust_layout, $
LIST_INDEX = list_index, $
PLOT_INDEX = plot_index, $
TO_COLROW = to_colrow, $
TO_LIST_INDEX = to_list_index, $
TO_PLOT_INDEX = to_plot_index
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Defaults
    adjust_layout = keyword_set(adjust_layout)
    list_index = keyword_set(list_index)
    plot_index = keyword_set(plot_index)
    to_colrow = keyword_set(colrow)
    to_list_index = keyword_set(to_list_index)
    to_plot_index = keyword_set(to_plot_index)
    
    ;Convert LOCATION to a list index
    exists = self -> plotExists(location, index, $
                                LIST_INDEX=list_index, PLOT_INDEX=plot_index, $
                                /TO_LIST_INDEX)
    if exists eq 0 then return
    
    ;Convert plot locations of all plots to list index locations
    void = self -> plotExists(*self.plot_locations, list_index, $
                              LIST_INDEX=list_index, PLOT_INDEX=plot_index, $
                              /TO_LIST_INDEX)

    ;Find which plots to keep
    void = ismember(list_index, index, NONMEMBER_INDS=ikeep)
    
    ;Get rid of the indices to be removed.
    if n_elements(ikeep) eq 0 then begin
        self -> ClearPositions
    endif else begin
        *self.plot_locations = (*self.plot_locations)[*,ikeep]
        *self.plot_positions = (*self.plot_positions)[*,ikeep]
    endelse
    
    ;Fill holes in the layout?
    if adjust_layout then self -> adjustLayout
    
    ;Decrease the plot count
    self.nplots -= 1

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
;       LIST_INDEX:         in, optional, type=boolean, default=0
;                           If set, `LOCATION` is a vector of list index locations. The list
;                               index is the index at which the plot's object reference is
;                               stored. See the "Plots_Present" method.
;       PLOT_INDEX:         in, optional, type=boolean, default=0
;                           If set, `LOCATION` is a vector of 1D plot locations. In this case,
;                               the upper left plot is index 1, and the index number increases
;                               first downward, then across.
;       POSITION:           out, optional, type=fltarr(4)
;                           The lower-left and upper-right corners of a plot in normal
;                               coordinates. If `SETLOCATION` is in use, then this is the
;                               new position of the plot indicated by `LOCATION`.
;       SETLOCATION:        in, optional, type=lonarr(2)
;                           Use this kewyord to set the [col, row] location of the plot
;                               indicated by `LOCATION`::
;                                   `LOCATION`[0] > 0, `SETLOCATION[0] < 0 
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
LIST_INDEX = list_index, $
PLOT_INDEX = plot_index, $
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
    
    exists = self -> plotExists(location, index, $
                                PLOT_INDEX=plot_index, LIST_INDEX=list_index, $
                                /TO_LIST_INDEX)
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
            loc = self -> getMaxLocation(/NEGATIVE)
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
            loc = self -> getMaxLocation(/NEGATIVE)
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
;   Determine whether or not a plot exists at the specified location.
;
;   NOTE::
;       There is only one plot at each [col, row] plot location.
;
; :Params:
;       LOCATION:       in, required, type=intarr(2\,*)
;                       the 1 based plot location [col, row] at which to check 
;                           for existence.
;       LOCATION_OUT:   out, optional, type=intarr
;                       If `TO_COLROW`, `TO_LIST_INDEX`, or 'TO_PLOT_INDEX` are set, then
;                           this is `LOCATION` converted to the indicated location-type.
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
;
; :Returns:
;       PLOT_EXISTS:    Tells whether the plot exists (1) or not (0).
;
; :Uses:
;   Uses the following external programs::
;       twoD_to_oneD_index.pro
;-
function MrPlotLayout::plotExists, location, location_out, $
LIST_INDEX = list_index, $
PLOT_INDEX = plot_index, $
TO_COLROW = to_colrow, $
TO_LIST_INDEX = to_list_index, $
TO_PLOT_INDEX = to_plot_index
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif
    
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;If no plots are present, then LOCATION does not exist
    if n_elements(*self.plot_locations) eq 0 then return, 0
    nplots = (*self.plot_locations)[0,*]

    ;Check keywords
    plot_index = keyword_set(plot_index)
    list_index = keyword_set(list_index)
    to_colrow = keyword_set(to_colrow)
    to_list_index = keyword_set(to_list_index)
    to_plot_index = keyword_set(to_plot_index)
    
    if plot_index + list_index gt 1 then $
        message, 'PLOT_INDEX and LIST_INDEX are mutually exclusive.'
    
    if to_colrow + to_list_index + to_plot_index gt 1 then $
        message, 'TO_COLROW, TO_LIST_INDEX, and TO_PLOT_INDEX are mutually exclustive.'
    
    ;Number of elements
    if keyword_set(plot_index) || keyword_set(list_index) $
        then nlocations = n_elements(location) $
        else nlocations = n_elements(location[0,*])
    
    ;Allocate memory
    plot_exists = intarr(nlocations)
    
    if keyword_set(to_colrow) $
        then location_out = intarr(2, nlocations) $
        else if keyword_set(to_list_index) or keyword_set(to_plot_index) $
            then location_out = intarr(nlocations)
    
;---------------------------------------------------------------------
;A Plot Index Location was Provided //////////////////////////////////
;---------------------------------------------------------------------
    
    ;If a plot index was given
    if keyword_set(plot_index) then begin
        ;If a plot index was given, convert it to a [col, row] location
        loc_colrow = twoD_to_oneD_index(location-1, self.layout, /ONED_TO_TWOD) + 1
        
        ;Check if the plots exist
        for i = 0, nlocations - 1 do begin
            thisPlot = where((*self.plot_locations)[0,*] eq loc_colrow[0,i] and $
                             (*self.plot_locations)[1,*] eq loc_colrow[1,i], nmatches)
            
            if nmatches eq 0 $
                then plot_exists[i] = 0 $
                else plot_exists[i] = 1
            
            ;Convert to list index?
            if to_list_index then location_out[i] = thisPlot
        endfor
        
        ;Convert to [Col, Row]?
        if to_colrow then location_out = loc_colrow
        if to_plot_index then location_out = location
    
;---------------------------------------------------------------------
;A List Index Location was Provided //////////////////////////////////
;---------------------------------------------------------------------
    endif else if keyword_set(list_index) then begin
        ;Plots are stored contiguously. List_Index must be < NPlots.
        plot_exists = location lt nplots
        
        ;Convert?
        if keyword_set(to_list_index) then location_out = location
        if keyword_set(to_colrow) then location_out = (*self.plot_locations)[*,location]
        if keyword_set(to_plot_index) then begin
            loc_colrow = (*self.plot_locations)[*,location]
            location_out = twoD_to_oneD_index(loc_colrow-1, self.layout) + 1
        endif
        
;---------------------------------------------------------------------
;A [Col, Row] Location was Provided //////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        ;Check if the plots exist
        for i = 0, nlocations - 1 do begin
            thisPlot = where((*self.plot_locations)[0,*] eq location[0,i] and $
                             (*self.plot_locations)[1,*] eq location[1,i], nmatches)
            
            if nmatches eq 0 $
                then plot_exists[i] = 0 $
                else plot_exists[i] = 1
                
            ;Convert to list index?
            if keyword_set(to_list_index) then location_out[i] = thisPlot
        endfor

        ;Convert to [Col, Row]?
        if keyword_set(to_colrow) then location_out = location
        if keyword_set(to_plot_index) then location_out = twoD_to_oneD_index(location-1, self.layout) + 1
    endelse

    ;If a scalar was given, return a scalar    
    if nlocations eq 1 then plot_exists = plot_exists[0]
    
    return, plot_exists
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
    if n_elements(layout)  ne 0 then self.layout = layout
    if n_elements(xgap)    ne 0 then self.xgap = xgap
    if n_elements(xmargin) ne 0 then self.xmargin = xmargin
    if n_elements(ygap)    ne 0 then self.ygap = ygap
    if n_elements(ymargin) ne 0 then self.ymargin = ymargin
    
    ;Calculate the positions of the plots within the new layout, then apply them
    ;to the plots that exist within said layout.
    self -> CalcPositions
    self -> ApplyPositions
end


;+
;   Shift all plots located at and after LOCATION up one index value.
;
; :Private:
;
; :Params:
;
;       LOCATION:           in, required, type=lonarr(2)
;                           The 1 based plot location [col, row]  at which to begin 
;                               shifting plots.
;
; :Keywords:
;       LIST_INDEX:         in, optional, type=Boolean, default=0
;                           Indicate that `LOCATION` is the index within the
;                               internal data lists at which to begin shifting plots. All
;                               plots with a plot grid index number greater than or equal
;                               to this plot will be moved.
;-
pro MrPlotLayout::shiftPlots, location, $
LIST_INDEX = list_index
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
    is_colrow = ~keyword_set(list_index)
    exists = self -> plotExists(location, index, LIST_INDEX=list_index, /TO_LIST_INDEX)
    colrow = (*self.plot_locations)[*,index]
    
    exists = self -> plotExists(colrow, plot_index, /TO_PLOT_INDEX)

    ;Find the 1D plot number of each existing plot. Allocate an array with -1's and skip
    ;all plots with user-supplied positions, leaving the plotIndex of said plots -1.
    plotIndices = lonarr(n_elements(*self.plot_locations)/2) - 1
    theseIndices = where((*self.plot_locations)[0,*] ne -1, count)
    if count ne 0 then begin
        exists = self -> plotExists((*self.plot_locations)[*,theseIndices], pindex, /TO_PLOT_INDEX)
        plotIndices[theseIndices] = pindex
    endif
    
    ;find all of the plots that need to be moved
    moveThese = where(plotIndices ge plot_index, count)
    if count eq 0 then return
    
    ;add one to their index number, essentially moving them to the next plot location over
    plotIndices[moveThese] += 1
    
    ;if there are more plots than there are positions available, add a row to the plot layout
    nLocations = self.layout[0] * self.layout[1]
    if max(plotIndices) gt nLocations then self.layout += [0, 1]
        
    ;recalculate all of the plot locations
    ;convert the 0-based plot indices to 1-based plot locations [col, row]
    theseIndices = where(plotIndices ne -1, count)
    if count ne 0 then begin
        exists = self -> plotExists(plotIndices[theseIndices], colrow_out, /PLOT_INDEX, /TO_COLROW)
        (*self.plot_locations)[*,theseIndices] = colrow_out
    endif
end


;+
;   The purpose of this method is to describe the layout as well as the locations and
;   positions of the plots currently stored in it.
;-
pro MrPlotLayout::whichLayout, old_layout, new_layout
INDEX=index
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Print information about the overall layout.
    print, '-------LAYOUT-------'
    print, FORMAT='(%"  Layout:     [%i, %i]")', self.layout
    print, FORMAT='(%"  # of Plots: %i")', self.nplots
    if n_elements(*self.aspect) ne 0 then print, FORMAT='(%"  Aspect:     %f")', *self.aspect
    print, FORMAT='(%"  XMargins:   [%i, %i]")', self.xmargin
    print, FORMAT='(%"  YMargins:   [%i, %i]")', self.ymargin
    print, FORMAT='(%"  XGaps:      %i")', self.xgap
    print, FORMAT='(%"  YGaps:      %i")', self.ygap
    
    ;Print the locations and positions of each plot
    print, '--LOCATIONS--               --POSITIONS--'
    for i = 0, self.nplots - 1 do begin
        print, FORMAT='(%"   [%i, %i]          [%6.4f, %6.4f, %6.4f, %6.4f]")', $
               (*self.plot_locations)[*,i], (*self.plot_positions)[*,i]
    endfor
end


;+
;       Renumber the plots in case of a change to the layout (i.e. the number of rows and
;       columns is changed).
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
;-
pro MrPlotLayout::renumber, old_layout, new_layout
INDEX=index
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;for each plot...
    for i = 0, self.nplots-1 do begin
        ;convert the plot location to an index number. Plot locations are 1-based. They
        ;must be converted to 0-based index values.
        old_plot_number = twoD_to_oneD_index((*self.plot_locations)[*,i] - 1, old_layout)
        
        ;find the plot's location in the new layout. Plot numbers are 0-based. They
        ;must be converted to 1-based location values.
        (*self.plot_locations)[*,i] = twoD_to_oneD_index(old_plot_number, new_layout, /oneD_to_twoD) + 1
    endfor
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
    ptr_free, self.layout_positions
    ptr_free, self.plot_locations
    ptr_free, self.plot_positions
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
;       YGAP:           in, optional, type=float, default=7
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
    setDefaultValue, ygap, 7
    
    ;Set object Properties
    self.layout = layout
    self.xmargin = xmargin
    self.ymargin = ymargin
    self.xgap = xgap
    self.ygap = ygap
    self.layout_positions = ptr_new(/ALLOCATE_HEAP)
    self.plot_locations = ptr_new(/ALLOCATE_HEAP)
    self.plot_positions = ptr_new(/ALLOCATE_HEAP)
    
    if n_elements(aspect) eq 0 $
        then self.aspect = ptr_new(/ALLOCATE_HEAP) $
        else self.aspect = ptr_new(aspect)
    
    ;Calculate the positions
    if keyword_set(calculate) then self -> CalcPositions
    
    return, 1                     
end


;+
;   The class definition
;-
pro MrPlotLayout__define
    compile_opt idl2
    
    class = {mrplotlayout, $
             aspect: ptr_new(), $           ;Aspect ratio of the plots.
             layout: [0,0], $               ;Layout of the plot area [ncols, nrows].
             nplots: 0, $                   ;Number of plots displayed.
             layout_positions: ptr_new(), $ ;Positions outlined by the 2D plot-layout grid.
             plot_locations: ptr_new(), $   ;[col, row] location of each plot.
             plot_positions: ptr_new(), $   ;Positions of each plot.
             xgap: 0, $                     ;Size of the gap between plots in the x-direction.
             xmargin: [0,0], $              ;Size of the [left, right] margins.
             ygap: 0, $                     ;Size of the gap between plots in the y-direction.
             ymargin: [0,0]}                ;Size of the [top, bottom] margins.
end