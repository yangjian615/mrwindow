; docformat = 'rst'
;
; NAME:
;       MrPlotWindow__Define
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
;   The purpose of this method is to manage the creation, addition, and removal of plots.
;   Plots, if not given a position, will be fit into a 2D plotting grid that automatically
;   determines plot locations when additional plots are added or removed.
;
;   If a plot object 
;
; :Examples
;   Example 1::
;       ;Create a sine and cosine wave.
;       x = findgen(100)/99.0
;       y = sin(2*!pi*x)
;       z = cos(2*!pi*x)
;
;       ;Create the object and add the next available position to the layout
;       MyObj = obj_new('MrWindow')
;       p1 = MyObj -> Plot(x, y, TITLE='Sin(x)', XTITLE='Time', YTITLE='Amplitude', LAYOUT=[2,2,1,2])
;       p2 = MyObj -> Plot(x, z, TITLE='Cos(x)', XTITLE='Time', YTITLE='Amplitude', LAYOUT=[3,3,3,3])
;
;       ;Move Sin(x) from location [1,2] to [2,2]. Return new position.
;       MyObj -> SetPosition, [1,2], [2,2], OUTPOSITION=position, /DRAW
;
;       ;Move location [2,2] from the layout into a fixed position, making [2,2] avaialble.
;       MyObj -> SetPosition, [2,2], /TOFIXED, OUTPOSITION=outPos, OUTLOCATION=outLoc, /DRAW
;
;       ;Move a fixed position into the layout at location [1,1]
;       MyObj -> SetPosition, [-1,1], [1,1], OUTPOSITION=outPos, /DRAW
;
;       ;Move location [1,1] to a specific, fixed location, making [1,1] available
;       SetPosition = [0.25, 0.25, 0.75, 0.75]
;       MyObj -> SetPosition, [1,1], SetPosition, OUTLOCATION=outLoc, /DRAW
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
;       Matthew Argall 2013, All rights reserved
;
; :History:
;	Modification History::
;       05/20/2013  -   Written by Matthew Argall
;       05/27/2013  -   The Plot method was not interfacing with the AddPlots and
;                           SetPositions properly. Fixed. - MRA
;       05/30/2013  -   Moved the Clear, Remove, and Replace from AddPlots into their
;                           own methods. Added the DESTROY keyword to all of them. Added
;                           the allObjects property to the class so that line plots,
;                           images, contours, etc. could all be added into a master list
;                           that matches that of MrPlotLayout.Plot_Positions. This means
;                           that objects have to be added/removed/replaced in allObjects
;                           as well as in the superclass properties MrAbstractPlot.plotObjects,
;                           MrAbstractImage.imageObjects, etc. The reason for keeping
;                           each of these lists is to be able to distinguish a plot
;                           object from an image object from a ... while still allowing
;                           any object class to be added.
;       05/31/2013  -   Added the Add method to generalize the object-adding process.
;                           Add will add any object to the master list. - MRA
;       06/19/2013  -   Fixed typos and several minor bugs. - MRA
;       07/03/2013  -   Positions were not determined properly when external plots are
;                           added. Fixed. - MRA
;       07/03/2013  -   Positions generated by the Add method are not applied to the
;                           appropriate object correctly. - MRA
;       07/09/2013  -   Positions or locations can be passed to calcColorBarPosition. - MRA
;       08/16/2013  -   Inherit MrIDL_Container and MrCreateGraphic. Removed the Plot
;                           and Image methods. No longer inherit MrAbstract* methods.
;                           The Add method now forwards actions to other methods instead
;                           of being a secondary method. Removed the allObjects property. 
;                           Removed the Add* methods for individual graphic types. - MRA
;       08/22/2013  -   Add, ShiftPlots, ApplyPositions, WhatAmI, and Config methods are
;                           working properly after remodelling the MrPlotLayout class.
;                           Added the Plot, Image, and Contour methods. The Remove
;                           method now works. - MRA
;       08/24/2013  -   Added the FillHoles and TrimLayout methods. Inherit CDF_Plot.- MRA
;       08/27/2013  -   Added the Get method. - MRA
;       08/30/2013  -   Added QUIET keyword to the Add method. - MRA
;                                   
;-
;*****************************************************************************************
;+
;   Add an object to the container and layout. It provides additional functionality to
;   the MrIDL_Container::Add method.
;       - Provide a default position for plots
;       - Plots are automatically stacked vertically without having to specify a position
;       - If a LAYOUT is specified and a collision occurs, other plots will be moved
;           out of the way
;
; :Params:
;       THEOBJECTS:            in, optional, type=object/obj_arr(N)
;                               The plot object(s) to add to the display.
;
;   :Keywords:
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding.
;       POSITION:               in, optional, type=int/intarr, default=[last]
;                               The index location(s) into the container at which to add
;                                   the object(s). The default is to put the objects at
;                                   the end of the container.
;       PLOT_POSITION:          out, optional, type=fltarr(4\,N)
;                               The standard 4-element plot position for each object in
;                                   `THEOBJECTS`. If an object is not a "data" object,
;                                   then the position will be [0,0,0,0].
;       PLOT_LOCATION:          out, optional, type=intarr(2\,N)
;                               The [col, row] location of each object in `THEOBJECTS`
;                                   where each plot will be placed in the 2D plotting grid.
;                                   If the object is not a "data" object, then its location
;                                   will be [0,0]
;       QUITE:                  in, optional, type=boolean, default=0
;                               If set, no message will be printed if an object cannot be
;                                   added to the continer.
;-
pro MrPlotManager::Add, theObjects, $
DRAW = draw, $
POSITION = index, $
PLOT_LOCATION = outPocation, $
PLOT_POSITION = outPosition, $
QUIET = quiet
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;How many objects were given?
    nObj = n_elements(theObjects)
    nIndex = n_elements(index)
    layout_init = self.layout
    
;---------------------------------------------------------------------
;Add to Master List //////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If nothing was given to add, then return
    if nObj eq 0 then return
        
    ;Allocate memory for the positions and locations.
    outLocation = intarr(2, nObj)
    outPosition = fltarr(4, nObj)
    
    ;Make sure a location and position is defined for each plot
    for i = 0, nObj - 1 do begin
        skip = 0
        if nIndex gt 0 then thisIndex = index[i]
        ImA = self -> WhatAmI(theObjects[i])

    ;---------------------------------------------------------------------
    ;Unknown Objects /////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if ImA eq '' then begin
            if keyword_set(quiet) eq 0 then $
                print, FORMAT='(%"MrPlotManager::Add: Object \"%s\" at index %i is not recognized.' + $
                                  'Cannot add.")', typename(theObjects[i]), i
            continue
        endif
            
    ;---------------------------------------------------------------------
    ;"Annotation" Objects ////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        isData = isMember((*self.gTypes).ImAData, ImA) 
        if isData eq 0 then begin
            self -> MrIDL_Container::Add, theObjects[i], POSITION=thisIndex
            continue
        endif
        
    ;---------------------------------------------------------------------
    ;"Data" Objects //////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        
        ;Get the position and layout of the object.
        theObjects[i] -> GetProperty, POSITION=pos, LAYOUT=layout
        
        ;Record the initial position for comparison.
        if n_elements(pos) eq 0 $
            then pos_init = !Null $
            else pos_init = pos
        
        ;Get the "location" part of the layout. Check for default Coyote Graphics position.
        case n_elements(layout) of
            0: if n_elements(pos) gt 0 && array_equal(pos, [0.125, 0.125, 0.925, 0.9]) $
                    then void = temporary(pos)
            3: loc = layout[2]
            4: loc = layout[2:3]
            else: begin
                print, FOMRAT='(%"Cannot add object %s at index %i". Improper layout)', $
                       typename(theObjects[i]), i
                print, 'Layout = ', layout
                skip = 1
            endelse
        endcase
        if skip then continue

        ;Set the location and position of the new plot.
        ;   If LOC is undefined, it will be returned either as an auto-updating location
        ;       (if POS is undefined) or as a fixed location (if POS is defined). If LOC
        ;       is defined and different plot already occupies that location, existing
        ;       plots will be shifted out of the way to make room.
        ;
        ;   If POS is undefined, then Auto-Updating position and location will be returned.
        ;       If defined and LOC is undefined, 
        self -> AddToLayout, loc, pos
        
        ;Set the location and position as object properties

        if loc[0] gt 0 then theObjects[i] -> SetProperty, LAYOUT=[self.layout[0:1], loc]
        theObjects[i] -> SetProperty, POSITION=pos
        
        ;Store the location and position if they were created.
        outLocation[*,i] = temporary(loc)
        outPosition[*,i] = temporary(pos)

        ;Add the object to the container.
        self -> MrIDL_Container::Add, theObjects[i], POSITION=thisIndex
    endfor
    
;---------------------------------------------------------------------
;Apply New Positions /////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;If the layout has changed, we need to apply the new positions to all plots.
    if array_equal(layout_init, self.layout) eq 0 $
        then self -> ApplyPositions
        
    ;Draw?
    if keyword_set(draw) then self -> Draw
end


;+
;   The purpose of this method is to apply the positions of a new layout to all plots
;   within the layout.
;-
pro MrPlotManager::ApplyPositions
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
        
;---------------------------------------------------------------------
;Reposition Plots ////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Get all of the data objects
    dataObjs = self -> Get(/ALL, ISA=(*self.gTypes).data, COUNT=nData)
    
    ;Step through each object
    for i = 0, nData - 1 do begin
    
        ;Check to see that it has a LAYOUT specified
        dataObjs[i] -> GetProperty, LAYOUT=layout
        nLayout = n_elements(layout)
        if nLayout eq 0 then continue
        
        ;Get the [col,row] location of the plot
        case nLayout of
            3: thisColRow = self -> ConvertLocation(layout[2], /PLOT_INDEX, /TO_COLROW)
            4: thisColRow = layout[2:3]
            else: message, 'Layout has incorrect format.'
        endcase

        ;Update the positions of each plot.
        position = (*self.layout_positions)[*, thisColRow[0]-1, thisColRow[1]-1]
        dataObjs[i] -> SetProperty, POSITION=position
    endfor
end


;+
;   Fill holes in the layout by moving each plot to the lowest available plot index.
;   Preserve the order of the plots.
;
; :Keywords:
;       DRAW:           in, optional, type=boolean, default=0
;                       If set, the draw method will be called after filling holes.
;-
pro MrPlotManager::FillHoles, $
DRAW=draw
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
        
;---------------------------------------------------------------------
;Find the Holes //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Which are available?
    tf_available = self -> IsAvailable(/PLOT_INDEX)
    if tf_available eq !Null then return
    
    ;Where are the holes? pHoles is in plot-index order, but is 0-based.
    ;We need 1-based to have plot-indices.
    pHoles = where(tf_available eq 1, nHoles) + 1
    if nHoles eq 0 then return
    
;---------------------------------------------------------------------
;Fill Holes //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get all of the relevant objects
    theseObj = self -> Get(/ALL, ISA=(*self.gTypes).data, COUNT=nObj)
    
    ;Start shifting them
    for i = 0, nObj - 1 do begin
        theseObj[i] -> GetProperty, LAYOUT=layout
        nLayout = n_elements(layout)
        if nLayout eq 0 then continue
        
        case nLayout of
            3: thisPIndex = layout[2]
            4: thisPIndex = self -> ConvertLocation(layout[2:3], self.layout, /TO_PLOT_INDEX)
            else: message, 'Layout has incorrect format. Cannot be shifted.'
        endcase

        ;If the plot is before the hole, then skip it.
        if thisPIndex le pHoles[0] then continue

        ;If we make it to here, fill a hole.
        newLocation = self -> ConvertLocation(pHoles[0], /PLOT_INDEX, /TO_COLROW)
        newPosition = (*self.layout_positions)[*, newLocation[0]-1, newLocation[1]-1]
        theseObj[i] -> SetProperty, LAYOUT=[self.layout, newLocation], POSITION=newPosition
        
        ;Now there is a hole in the old location
        pHoles[0] = thisPIndex
        pHoles = pHoles[sort(pHoles)]
    endfor
    
;---------------------------------------------------------------------
;Superclass //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Fill layout holes
    self -> MrPlotLayout::FillHoles
    
    if keyword_set(draw) then self -> Draw
end


;+
;   This method gets objects from the container. It provides additional functionality
;   to the MrIDL_Container::Get method::
;       - Filter objects based on their location in the 2D plotting grid.
;
; :Params:
;       THEOBJECTS:            in, optional, type=object/obj_arr(N)
;                               The plot object(s) to add to the display.
;
;   :Keywords:
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding.
;       POSITION:               in, optional, type=int/intarr, default=[last]
;                               The index location(s) into the container at which to add
;                                   the object(s). The default is to put the objects at
;                                   the end of the container.
;       PLOT_POSITION:          out, optional, type=fltarr(4\,N)
;                               The standard 4-element plot position for each object in
;                                   `THEOBJECTS`. If an object is not a "data" object,
;                                   then the position will be [0,0,0,0].
;       PLOT_LOCATION:          out, optional, type=intarr(2\,N)
;                               The [col, row] location of each object in `THEOBJECTS`
;                                   where each plot will be placed in the 2D plotting grid.
;                                   If the object is not a "data" object, then its location
;                                   will be [0,0]
;-
function MrPlotManager::Get, $
ALL = All, $
ISA = IsA, $
LOCATION = location, $
PLOT_INDEX = plot_index, $
POSITION = index, $
COUNT = count
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        Count = 0
        return, !Null
    endif

;---------------------------------------------------------------------
;Find by Location ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(location) gt 0 then begin
        ;Convert LOCATION to plot indices, if necessary
        if keyword_set(plot_index) $
            then allPIndex = location $
            else allPIndex = self -> ConvertLocation(location, /TO_PLOT_INDEX)
        
        ;Get all of the objects
        AllObj = self -> MrIDL_Container::Get(/ALL, COUNT=nObj)
        Result = objarr(nObj)
        count = 0
        
        ;Loop through each object
        for i = 0, nObj - 1 do begin
            skip = 0
        
            ;Get the layout and position
            AllObj[i] -> GetProperty, LAYOUT=layout, POSITION=position
            
            ;Find the plot index of the object
            nLayout = n_elements(layout)
            case nLayout of
                0: if n_elements(position) eq 0 $
                    then location = FindFixedLocation(position) $
                    else skip = 1
                3: pIndex = layout[2]
                4: pIndex = self -> ConvertLocation(layout[2:3], /TO_PLOT_INDEX)
                else: skip = 1
            endcase
            
            ;If no valid layout or position was provided, then go to the next object.
            if skip eq 1 then continue
            
            ;Return this object?
            tf_get = isMember(allPIndex, pIndex, N_MATCHES=nGet)
            if nGet eq 0 || tf_get eq 0 $
                then continue $
                else Result[count] = AllObj[i]
            
            ;If we get to here, increase the object count
            count += 1
        endfor
        
        ;Trim the unused elements
        case count of
            0: return, !Null
            1: Result = Result[0]
            else: Result = Result[0:count-1]
        endcase

;---------------------------------------------------------------------
;Normal Get //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        Result = self -> MrIDL_Container::Get(ALL=All, ISA=IsA, POSITION=index, COUNT=count)
    endelse
    
    return, Result
end


;+
;   Remove a Plot or Image object from the list of objects being displayed.
;
; :Params:
;       LOCATION:               in, optional, type=intarr(2\,N), default=[1\,1]
;                               The [col, row] location of the plot to replace
;
;   :Keywords:
;       ALL:                    in, optional, type=boolean, default=0
;                               Remove all objects from the container.
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being replaced.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the legends to the list.
;       FILLHOLES:              in, optional, type=boolean, default=0
;                               Sometimes removing plots can cause holes in the layout.
;                                   Set this keyword to gather all remaining plots
;                                   together at the lowest available plot-indices.
;       POSITION:               in, optional, type=boolean, default=0
;                               The index within the container of the object(s) to be
;                                   removed.
;       TRIMLAYOUT:             in, optional, type=int, default=0
;                               If set, empty rows and columns will be removed.
;-
pro MrPlotManager::Remove, Child_object, $
ALL = all, $
DESTROY = destroy, $
DRAW = draw, $
FILLHOLES=fillHoles, $
POSITION = index, $
TRIMLAYOUT = trimLayout, $
TYPE = type
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Defaults
    SetDefaultValue, draw, 0, /BOOLEAN
    SetDefaultValue, destroy, 1, /BOOLEAN
    SetDefaultValue, fillHoles, 0, /BOOLEAN
    SetDefaultValue, trimLayout, 0, /BOOLEAN
        
;---------------------------------------------------------------------
;Remove All //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if keyword_set(all) then begin
        self -> MrIDL_Container::Remove, /ALL, DESTROY=destroy
        self -> ClearLayout
        return
    endif
        
;---------------------------------------------------------------------
;Remove Indices //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if n_elements(index) gt 0 then begin
        ;Get the objects being removed.
        removeThese = self -> Get(POSITION=index, COUNT=nRemove)
        
        ;Step through all of the objects.
        for i = 0, nRemove - 1 do begin
            ;Get their position and layout.
            removeThese[i] -> GetProperty, LAYOUT=layout, POSITION=position
            
            ;Get their [col, row] location.
            case n_elements(layout) of
                0: thisLoc = self -> FindFixedLocation(position)
                3: thisLoc = self -> ConvertLocation(layout[2:3], layout, /PLOT_INDEX, /TO_COLROW)
                4: thisLoc = layout[2:3]
                else: message, 'Layout has incorrect format. Cannot be removed.'
            endcase
            
            ;Remove from the container and from the layout.
            self -> MrIDL_Container::Remove, removeThese[i], DESTROY=destroy
            self -> RemoveFromLayout, thisLoc
        endfor
    endif
        
;---------------------------------------------------------------------
;Remove Type /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if n_elements(type) gt 0 then begin
        ;Get the objects being removed.
        removeThese = self -> Get(ISA=type, COUNT=nRemove)
        
        ;Step through all of the objects.
        for i = 0, nRemove - 1 do begin
            ;Get their position and layout.
            removeThese[i] -> GetProperty, LAYOUT=layout, POSITION=position
            
            ;Get their [col, row] location.
            case n_elements(layout) of
                0: thisLoc = self -> FindFixedLocation(position)
                3: thisLoc = self -> ConvertLocation(layout[2:3], layout, /PLOT_INDEX, /TO_COLROW)
                4: thisLoc = layout[2:3]
                else: message, 'Layout has incorrect format. Cannot be removed.'
            endcase
            
            ;Remove from the container and from the layout.
            self -> MrIDL_Container::Remove, removeThese[i], DESTROY=destroy
            self -> RemoveFromLayout, thisLoc
        endfor
    endif
        
;---------------------------------------------------------------------
;Remove Child_Objects ////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if n_elements(Child_Object) gt 0 then begin
        nRemove = n_elements(Child_Object)
        
        ;Step through all of the objects.
        for i = 0, nRemove - 1 do begin
            ;Get their position and layout.
            Child_Object[i] -> GetProperty, LAYOUT=layout, POSITION=position
            
            ;Get their [col, row] location.
            case n_elements(layout) of
                0: thisLoc = self -> FindFixedLocation(position)
                3: thisLoc = self -> ConvertLocation(layout[2:3], layout, /PLOT_INDEX, /TO_COLROW)
                4: thisLoc = layout[2:3]
                else: message, 'Layout has incorrect format. Cannot be removed.'
            endcase
            
            ;Remove from the container and from the layout.
            self -> MrIDL_Container::Remove, Child_Object[i], DESTROY=destroy
            self -> RemoveFromLayout, thisLoc
        endfor
    endif
    
;---------------------------------------------------------------------
;Draw ////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Fill holes?
    if fillHoles eq 1 then self -> FillHoles
    
    ;Trim the layout?
    if trimLayout eq 1 then self -> TrimLayout

    ;Draw?
    if keyword_set(draw) then self -> Draw
 
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
;       TOFIXED:            in, optional, type=boolean, default=0
;                           Indicate that an Auto-Updating position is to be replaced
;                               by a fixed position.
;       OUTPOSITION:        out, optional, type=fltarr(4)
;                           The position indicated by `LOCATION` after it has been replaced.
;       OUTLOCATION:        out, optional, type=intarr(2)
;                           The new position, after the one indicated by `LOCATION` has
;                               been replaced.
;       SETLOCATION:        in, optional, type=lonarr(2)
;                           Use this kewyord to set the new [col, row] location of the
;                               plot indicated by `LOCATION`. To replace an Auto-Updating
;                               position with a Fixed position, use the `TOFIXED` keyword.
;       SETPOSITION:        in, optional, type=fltarr(4)
;                           A four-element vector in the form [x0, y0, x1, y1] specifying
;                               the location of the lower-right [x0, y0] and upper-left
;                               [x1, y1] corners of a plot. If set, then the plot given
;                               by `LOCATION` will have its position changed to SETPOSITION.
;                               If `LOCATION` is that of an auto-updating plot, it will
;                               become fixed.
;
; :Uses:
;   Uses the following external programs::
;       MrPlotLayout.pro
;-
pro MrPlotManager::SetPosition, position, theObject, $
LOCATION=Location, $
PLOT_INDEX=plot_index, $
INDEX=index, $
TOFIXED=toFixed
    compile_opt idl2

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    toFixed = keyword_set(toFixed)
    
    ;If no object was given, get it
    if n_elements(theObject) eq 0 then begin
    
        ;Find the location of the plot whose position is being set
        case n_elements(position) of
            2: new_loc = position
            4: new_loc = self -> FindFixedLocation(position)
        endcase
        
        ;Get the object
        theObject = self -> Get(LOCATION=old_loc)
    endif
end


;+
;   Shift all plots located at and after LOCATION up one index value.
;
; :Params:
;
;       LOCATION:           in, required, type=lonarr(2)
;                           The 1-based plot location [col, row] at which to begin 
;                               shifting plots.
;-
pro MrPlotManager::ShiftPlots, location
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    layout_init = self.layout
    
;---------------------------------------------------------------------
;Start and Stop Shift ////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Convert LOCATION to a plot index
    pStartShift = self -> ConvertLocation(location, /TO_PLOT_INDEX)
    
    ;Get the available list indices
    iFree = self -> GetListIndexAvailability(NFREE=nFree)
    if nFree gt 0 then pFree = self -> ConvertLocation(iFree, /LIST_INDEX, /TO_PLOT_INDEX)
    
    ;Which interval is being shifted? If a new row needs to be added,
    ;re-calculate the starting plot index within the new layout.
    if nFree eq 0 || max(pFree gt pStartShift) eq 0 then begin
        layout = self.layout + [0,1]
        pStartShift = self -> ConvertLocation(location, layout, /TO_PLOT_INDEX)
        pStopShift = self -> ConvertLocation([1, layout[1]], layout, /TO_PLOT_INDEX)
    endif else begin
        layout = self.layout
        pStopShift = min(where((pFree gt pStartShift) eq 1))
    endelse
    
;---------------------------------------------------------------------
;Superclass //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Shift the layout so that the layout_positions are up-to-date.
    self -> MrPlotLayout::ShiftPlots, location
    
;---------------------------------------------------------------------
;Shift Plots /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get all of the relevant objects
    theseObj = self -> Get(/ALL, ISA=(*self.gTypes).data, COUNT=nObj)
    
    ;Start shifting them
    for i = 0, nObj - 1 do begin
        theseObj[i] -> GetProperty, LAYOUT=objLayout
        nLayout = n_elements(objLayout)
        if nLayout eq 0 then continue
        
        case nLayout of
            3: thisPIndex = objLayout[2]
            4: thisPIndex = self -> ConvertLocation(objLayout[2:3], layout, /TO_PLOT_INDEX)
            else: message, 'Layout has incorrect format. Cannot be shifted.'
        endcase
        
        if thisPIndex lt pStartShift then continue
        if thisPIndex ge pStopShift then break
        
        ;If we make it to here, shift the plot
        thisPIndex += 1
        newLocation = self -> ConvertLocation(thisPIndex, layout, /PLOT_INDEX, /TO_COLROW)
        newPosition = (*self.layout_positions)[*, newLocation[0]-1, newLocation[1]-1]
        theseObj[i] -> SetProperty, LAYOUT=[layout, newLocation], POSITION=newPosition
    endfor
end


;+
;   The purpose of this method is to trim empty rows and columns from the layout.
;
; :Keywords:
;       DRAW:           in, optional, type=boolean, default=0
;                       If set, the draw method will be called after trimming the layout.
;-
pro MrPlotManager::TrimLayout, $
DRAW = draw
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
        
    ;Call the superclass
    self -> MrPlotLayout::TrimLayout
    
    ;Apply the new positions
    self -> ApplyPositions
    
    ;Draw?
    if keyword_set(draw) then self -> Draw
end


;+
;   Determine which type of object was given.
;
; :Params:
;       OBJREF:             in, required, type=Object
;                           An object reference whose "type" is to be determined. Will
;                               tell you if the object is a Plot or Image.
;
; :Returns:
;       IMA:                out, required, type=string
;                           The type of object that was passed. This is determined by
;                               scanning all of the different lists of objects to find
;                               a match. If no match is found, the empty string is
;                               returned.
;-
function MrPlotManager::WhatAmI, objRef
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, ''
    endif
    
    ;What type of object is it?
    className = typename(objRef)
    case className of
        'MRPLOTOBJECT':  ImA = 'PLOT'
        'MRIMAGEOBJECT': ImA = 'IMAGE'
        'MRCONTOUR':     ImA = 'CONTOUR'
        'WECOLORBAR':    ImA = 'COLORBAR'
        'WETEXT':        ImA = 'TEXT'
        'WEARROW':       ImA = 'ARROW'
        'WEOVERPLOT':    ImA = 'OVERPLOT'
        'WELEGENDITEM':  ImA = 'LEGEND'
        'WEAXIS':        ImA = 'AXIS'
        else:            ImA = ''
    endcase
    
    return, ImA
end


;+
;   The purpose of this method is to create a structure of supported classes for each
;   graphics type.
;-
pro MrPlotManager::Config
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Class names of the supported graphics types
    types = { plot: ['PLOT', 'MRPLOTOBJECT'], $
              image: ['IMAGE', 'MRIMAGEOBJECT'], $
              contour: ['CONTOUR', 'MRCONTOUR'], $
              colorbar: ['WECOLORBAR'], $
              axis: ['WEAXIS'], $
              legend: ['WELEGENDITEM'], $
              arrow: ['WEARROW'], $
              text: ['WETEXT'], $
              overplot: ['WEOVERPLOT'], $
              ImAData: ['PLOT', 'IMAGE', 'CONTOUR'], $     ;TO BE USED WITH ::WHATAMI
              data: ['PLOT', 'MRPLOTOBJECT', 'IMAGE', 'MRIMAGEOBJECT', 'CONTOUR', 'MRCONTOUR'], $
              annotate: ['WECOLORBAR', 'WEAXIS', 'WELEGENDITEM', 'WEARROW', 'WETEXT'], $
              files: ['CDF_PLOT'] $
            }
    
    ;Store them as a class property
    self.gTypes = ptr_new(types, /NO_COPY)

end


;+
;   Clean up after the object is destroy
;-
pro MrPlotManager::cleanup
    compile_opt idl2
    
    ;Destroy all weLegendItem objects
    self -> MrPlotLayout::Cleanup
    self -> MrIDL_Container::Cleanup
    self -> MrCreateGraphic::Cleanup
    
    ;Free pointers
    ptr_free, self.gTypes
end


;+
;   The initialization method.
;
; :Keywords:
;       _REF_EXTRA:                 in, optional, type=structure
;                                   Any keyword accepted by MrPlotLayout__define is also
;                                       accepted for keyword inheritance.
;-
function MrPlotManager::init, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif

    ;Setup the plot window
    if self -> MrPlotLayout::init(_STRICT_EXTRA=extra) eq 0 then return, 0
    
    ;Configure the object
    self -> config
    
    return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrPlotManager__define, class
    compile_opt idl2
    
    define = { MrPlotManager, $
               inherits MrIDL_Container, $      ;An object container
               inherits MrCreateGraphic, $      ;Plots, Images, Colorbars, Text, Arrows, etc.
               inherits MrPlotLayout, $         ;Manage plot layout
               gTypes: ptr_new() $              ;Supported graphics types.
             }
end