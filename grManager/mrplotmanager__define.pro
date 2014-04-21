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
; :Examples:
;   Example 1: Creating and Moving plots
;       ;At any point in this example, call the methods
;       ;   MyObj -> WhichLayout
;       ;   MyObj -> WhichObjects
;       ;To see how the layouts and graphics objects are being updated.
;
;       ;Make the data
;       x = findgen(100)/99.0
;       y = sin(2*!pi*x)
;       z = cos(2*!pi*x)
;
;       ;Create the MrWindow widget and add a plot of Sine and Cosine to locations [1,2] and [3,3]
;       MyObj = obj_new('MrWindow')
;       p1 = MyObj -> Plot(x, y, TITLE='Sin(x)', XTITLE='Time', YTITLE='Amplitude', LAYOUT=[2,2,1,2])
;       p2 = MyObj -> Plot(x, z, TITLE='Cos(x)', XTITLE='Time', YTITLE='Amplitude', LAYOUT=[3,3,3,3])
;
;       ;Move Sin(x) from location [1,2] to [2,2]. Return new position.
;       MyObj -> SetPosition, [1,2], [2,2], OUTPOSITION=position, /DRAW
;
;       ;Remove Sin(x) at [2,2] from the layout into a fixed position, making [2,2] avaialble.
;       MyObj -> SetPosition, [2,2], /TOFIXED, OUTPOSITION=outPos, OUTLOCATION=outLoc, /DRAW
;
;       ;Move a fixed position into the layout at location [1,1]
;       MyObj -> SetPosition, [-1,1], [1,1], OUTPOSITION=outPos, /DRAW
;
;       ;Move location [1,1] to a specific, fixed location, making [1,1] available
;       SetPosition = [0.25, 0.25, 0.75, 0.75]
;       MyObj -> SetPosition, [1,1], SetPosition, OUTLOCATION=outLoc, /DRAW
;
;       ;Move Sin(x) back to [1,1], then remove and trim holes in the layout.
;       MyObj -> SetPosition, [-1,1], [1,1], /DRAW
;       MyObj -> FillHoles, /TRIMLAYOUT, /DRAW
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
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
;       08/29/2013  -   The SetPosition method works. - MRA
;       08/30/2013  -   Added QUIET keyword to the Add method. - MRA
;       09/22/2013  -   Can now Get objects by graphic type. Had to rename PLOT_INDEX to
;                           PINDEX to remove ambiguous keyword abbreviation- MRA
;       09/24/2013  -   Added TRIMLAYOUT keyword to FillHoles. ApplyPositions now adjust
;                           plots properly when the layout is changed. Added the
;                           AdjustLayout_* methods. - MRA
;       09/27/2013  -   Layout and position are now always defined for graphics objects,
;                           with the MrGraphicAtom class. Had to take this into account
;                           in Add and AdjustLayout. Essentially, this entailed setting
;                           UPDATE_LAYOUT=0 while setting the graphic's layout or position. - MRA
;       09/29/2013  -   Get method was only retrieving one type of graphic, not all. Fixed. - MRA
;       2013/11/17  -   ApplyPosition and Add method now set ASPECT, CHARSIZE, XMARGIN,
;                           XGAP, YMARGIN and YGAP so that they layout is uniform. - MRA
;       2013/11/24  -   MrPlotLayout__Define was renamed to MrGrLayout__Define. - MRA
;       2013/12/10  -   Added the SetGlobal method. CHARSIZE affects the layout of the
;                           plotting grid, but is no longer applied to the individual
;                           graphics. - MRA
;       2014/03/09  -   Added the FindByPIndex and FindByColRow methods and removed
;                           the Get method. - MRA
;       2014/03/15  -   SetGlobal can now segregate object classes. - MRA
;       2014/03/26  -   Added MrVector to the list of known graphics. - MRA
;-
;*****************************************************************************************
;+
;   Event handler for the EDIT | Layout menu.
;
; :Private:
;-
pro MrPlotManager::AdjustLayout_Property, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if ptr_valid(new_layout) then ptr_free, new_layout
        void = cgErrorMsg()
        return
    endif
    
    ;Spawn a GUI to get user input about the layout
    new_layout = plotpositions_gui(self.tlb, $
                                   LAYOUT  =  self.GrLayout, $
                                   XGAP    = *self.xgap, $
                                   XMARGIN =  self.oxmargin, $
                                   YGAP    = *self.ygap, $
                                   YMARGIN =  self.oymargin)
    if ptr_valid(new_layout) eq 0 then return

    ;Update the layout
    self -> Refresh, /DISABLE
    self -> SetProperty, LAYOUT   = (*new_layout).layout, $
                         XGAP     = (*new_layout).xgap, $
                         OXMARGIN = (*new_layout).xmargin, $
                         YGAP     = (*new_layout).ygap, $
                         OYMARGIN = (*new_layout).ymargin
    
    ;Redraw
    self -> Refresh
    
    ;Free the pointer
    ptr_free, new_layout
end


;+
;   Event handler for the EDIT | Move menu.
;
; :Private:
;-
pro MrPlotManager::AdjustLayout_Move, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if ptr_valid(colrow) then ptr_free, colrow
        void = cgErrorMsg()
        return
    endif
    
    ;Spawn a GUI to get user input about the old and new locations
    colrow = moveplot_gui(self.tlb)
    if ptr_valid(colrow) eq 0 then return
    
    ;Update the layout
    self -> SetPosition, (*colrow).from, (*colrow).to
    
    ;Redraw
    self -> Draw
    
    ;Free the pointer
    ptr_free, colrow
end


;+
;   Event handler for the EDIT | Remove menu.
;
; :Private:
;-
pro MrPlotManager::AdjustLayout_Remove, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Remove the current object
    if obj_valid(self.focus) then self -> Remove, self.focus
    
    ;Redraw
    self -> Draw
end


;+
;   The purpose of this method is to provide an array-like means of accessing graphics
;   objects within the container. Two options are avaible: the object index within the
;   container or the name of the graphic object.
;
; :Private:
;
; :Params:
;       ISRANGE:            in, required, type=intarr
;                           A vector of 1's and 0's indicating if the corresponding
;                               subscript parameters `SUBSCRIPT1` are index ranges or
;                               index values.
;       SUBSCRIPT1:         in, required, type=intarr/strarr
;                           Index subscript of the graphics object to be returned, or the
;                               class names of the objects to return.
;-
function MrPlotManager::_OverloadBracketsRightSide, isRange, subscript1
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Search for objects by name. Return the first match.
    if size(subscript1, /TNAME) eq 'STRING' then begin
        if MrIsA(subscript1, /SCALAR) eq 0 then message, 'String subscripts must be scalars.'
        upSub1 = strupcase(subscript1)

        ;Get all of the objects
        allObjs = self -> Get(/ALL, COUNT=nObj)
        i = 0
        success = 0
        
        ;Search for the 
        while success eq 0 and i lt nObj do begin
            if strupcase(allObjs[i] -> GetName()) eq upSub1 then begin
                success = 1
                result = allObjs[i]
            endif
            i++
        endwhile
        
        if success eq 0 then result = obj_new()
        
    ;Call the superclass's method
    endif else result = self -> MrIDL_Container::_OverloadBracketsRightSide(isRange, subscript1)

    return, result
end


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
;       QUIET:                  in, optional, type=boolean, default=0
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
        void = cgErrorMsg()
        return
    endif
    
    ;How many objects were given?
    nObj = n_elements(theObjects)
    nIndex = n_elements(index)
    layout_init = self.GrLayout
    
;---------------------------------------------------------------------
;Add to Master List //////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If nothing was given to add, then return
    if nObj eq 0 then return
        
    ;Allocate memory for the positions and locations.
    outLocation = intarr(3, nObj)
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
                print, FORMAT='(%"MrPlotManager::Add: Object \"%s\" at index %i is not recognized. ' + $
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
        theObjects[i] -> GetLayout, POSITION=pos, LAYOUT=layout
        
        ;Record the initial position for comparison.
        if n_elements(pos) eq 0 then pos_init = pos
        
    ;---------------------------------------------------------------------
    ;Check Location //////////////////////////////////////////////////////
    ;---------------------------------------------------------------------

        ;If LAYOUT[0:1]=[0,0], then no layout has been established. In this case, check
        ;the position of the graphic.
        if total(layout[0:1]) eq 0 then begin
            
            ;Check for the default Coyote Graphics location (see MrGrAtom__Define).
            if (n_elements(pos) gt 0) && (array_equal(pos, [0.125, 0.125, 0.925, 0.9]) || $
                (total(pos) eq 0)) then void = temporary(pos)
        
        ;If a plot index was given, make LOC undefined to indicate that the graphic will
        ;be placed at a fixed position.
        endif else if layout[2] eq 0 and n_elements(pos) gt 0 then begin
            void = temporary(loc)

        ;Otherwise, check LAYOUT[3] for a plot index number. If it is > 0, then that is
        ;the location of the plot. Otherwise, we need to generate a location.
        endif else begin
            if layout[2] gt 0 then loc = layout
        endelse

    ;---------------------------------------------------------------------
    ;Generate Location ///////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        ;
        ;Set the location and position of the new plot.
        ;   If LOC is undefined, it will be returned either as an auto-updating location
        ;       (if POS is undefined) or as a fixed location (if POS is defined). If LOC
        ;       is defined and different plot already occupies that location, existing
        ;       plots will be shifted out of the way to make room.
        ;
        ;   If POS is undefined, then Auto-Updating position and location will be returned.
        ;       If POS is defined and LOC is undefined, then the location of the graphic
        ;       will be fixed and placed exactly as specified without adjusting any of the
        ;       other plots.
        ;
        self -> AddToLayout, loc, pos

        ;Set the location and position as object properties. Set UPDATE_LAYOUT=0 to
        ;prevent the graphic from trying to calculate a new position for itself.
        theObjects[i] -> SetLayout, LAYOUT=loc, POSITION=pos, $
                                    ASPECT=*self.aspect, $
                                    OXMARGIN=self.oxmargin, OYMARGIN=self.oymargin, $
                                    IXMARGIN=self.ixmargin, XGAP=self.xgap, $
                                    IYMARGIN=self.iymargin, YGAP=self.ygap, $
                                    UPDATE_LAYOUT=0

        ;Store the location and position if they were created.
        outLocation[*,i] = temporary(loc)
        outPosition[*,i] = temporary(pos)

        ;Add the object to the container.
        self -> MrIDL_Container::Add, theObjects[i]
    endfor
    
;---------------------------------------------------------------------
;Apply New Positions /////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;If the layout has changed, we need to apply the new positions to all plots.
    if array_equal(layout_init, self.GrLayout) eq 0 $
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
        void = cgErrorMsg()
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
        dataObjs[i] -> GetLayout, LAYOUT=layout
        if layout[2] le 0 then continue
        
        ;If GrLayout[0:1] does not match LAYOUT[0:1], then LAYOUT[2] might not be in
        ;the same location when placed in GRLAYOUT. To fix this, first, convert to a
        ;[col,row] in the old layout, then to a plot index in the new layout.
        if array_equal(self.GrLayout, layout[0:1]) eq 0 then begin
            oldColRow = self -> ConvertLocation(layout[2], layout[0:1], /PINDEX, /TO_COLROW)
            pIndex    = self -> ConvertLocation(oldColRow, self.GrLayout, /COLROW, /TO_PINDEX)
            layout = [self.GrLayout, pIndex]
        endif
        
        ;Get the array index at which the position is stored.
        thisAIndex = self -> ConvertLocation(layout[2], self.GrLayout, /PINDEX, /TO_AINDEX)
        
        ;Update the layout and position of each plot.
        position = (*self.layout_positions)[*, thisAIndex]
        dataObjs[i] -> SetLayout, LAYOUT=layout, POSITION=position, UPDATE_LAYOUT=0, $
                                  ASPECT=*self.aspect, $
                                  OXMARGIN=self.oxmargin, OYMARGIN=self.oymargin, $
                                  IXMARGIN=self.ixmargin, XGAP=self.xgap, $
                                  IYMARGIN=self.iymargin, YGAP=self.ygap
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
TRIMLAYOUT=trimLayout, $
DRAW=draw
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
        
;---------------------------------------------------------------------
;Find the Holes //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Which are available?
    tf_available = self -> IsAvailable()
    if MrIsNull(tf_available, -1) then return
    
    ;Where are the holes? Convert array indices to plot indices.
    pHoles = where(tf_available eq 1, nHoles)
    if nHoles gt 0 $
        then pHoles = self -> ConvertLocation(pHoles, /AINDEX, /TO_PINDEX) $
        else return
    
;---------------------------------------------------------------------
;Fill Holes //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get all of the relevant objects
    theseObj = self -> Get(/ALL, ISA=(*self.gTypes).data, COUNT=nObj)

    ;Start shifting them
    for i = 0, nObj - 1 do begin
        theseObj[i] -> GetLayout, LAYOUT=layout
        if layout[2] le 0 then continue

        ;If the plot is before the hole, then skip it.
        if layout[2] le pHoles[0] then continue

        ;If we make it to here, fill a hole.
        newAIndex = self -> ConvertLocation(pHoles[0], /PINDEX, /TO_AINDEX)
        newPosition = (*self.layout_positions)[*, newAIndex]
        theseObj[i] -> SetLayout, LAYOUT=[self.GrLayout, pHoles[0]], POSITION=newPosition, UPDATE_LAYOUT=0
        
        ;Now there is a hole in the old location
        pHoles[0] = layout[2]
        pHoles = pHoles[sort(pHoles)]
    endfor
    
;---------------------------------------------------------------------
;Superclass //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Fill layout holes
    self -> MrGrLayout::FillHoles
    
    ;Trim extra rows and columns?
    if keyword_set(trimLayout) then self -> TrimLayout
    
    if keyword_set(draw) then self -> Draw
end


;+
;   Find a graphic by its [col,row] location.
;
; :Params:
;       COLROW:         in, required, type=intarr(2)
;                       The [column, row] in which the desired graphic is located.
;
; :Returns:
;       OBJECT:         The graphics located at `COLROW`.
;-
function MrPlotManager::FindByColRow, colrow, $
COUNT=count
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        Count = 0
        return, obj_new()
    endif

    ;Convert COLROW to a plot index
    pIndex = self -> ConvertLocation(ColRow, /COLROW, /TO_PINDEX)

    ;Call FindByPIndex
    object = self -> FindByPIndex(pIndex, COUNT=count)

    return, object
end


;+
;   Find a graphic by its [col,row] location.
;
; :Params:
;       PINDEX:         in, required, type=intarr(2)
;                       The plot index, starting with 1 and increasing left to right then
;                           top to bottom,  in which the desired graphic is located.
;
; :Returns:
;       OBJECT:         The graphics located at `PINDEX`.
;-
function MrPlotManager::FindByPIndex, pIndex, $
COUNT=count
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        Count = 0
        return, obj_new()
    endif

    ;Get all of the objects in the container
    allObjs = self -> Get(/ALL, ISA=(*self.gTypes).data, COUNT=count)
    if count eq 0 then return, obj_new()
    
    ;Get all of the plot indices
    objPIndex = intarr(count)
    for i = 0, count - 1 do begin
        allObjs[i] -> GetLayout, LAYOUT=objLayout
        objPIndex[i] = objLayout[2]
    endfor

    ;Find a match
    iMatch = where(objPIndex eq pIndex, count)
    if count eq 0 then return, obj_new()
    if count eq 1 then iMatch = iMatch[0]
    
    ;Return the matching objects
    return, allObjs[iMatch]
end



;+
;   Remove a Plot or Image object from the list of objects being displayed.
;
; :Params:
;       Child_Object:           in, optional, type=object/objarr
;                               The object(s) to be removed.
;
; :Keywords:
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
;       TYPE:                   in, optional, type=string/strarr
;                               An array of object class names of the objects to be removed.
;-
pro MrPlotManager::Remove, Child_object, $
ALL = all, $
DESTROY = destroy, $
DRAW = draw, $
FILLHOLES=fillHoles, $
POSITION = index, $
TRIMLAYOUT = trimLayout, $
TYPE = type
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Defaults
    draw = keyword_set(draw)
    if n_elements(destroy) eq 0 then destroy = 1 else destroy = keyword_set(destroy)
    fillHoles = keyword_set(fillHoles)
    trimLayout = keyword_set(trimLayout)
        
;---------------------------------------------------------------------
;Remove All //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if keyword_set(all) then begin
        self -> MrIDL_Container::Remove, /ALL, DESTROY=destroy
        self -> ClearLayout
        if keyword_set(draw) then self -> Draw
        return
    endif
        
;---------------------------------------------------------------------
;Remove Indices //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if n_elements(index) gt 0 then begin
        ;Get the objects being removed.
        removeThese = self -> Get(POSITION=index, COUNT=nRemove)
        
        ;Append the newly found objects to the input array
        if nRemove gt 0 then if n_elements(Child_Object) eq 0 $
            then Child_Object = [Child_Object, temporary(removeThese)] $
            else Child_Object = temporary(removeThese)
    endif
        
;---------------------------------------------------------------------
;Remove Type /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if n_elements(type) gt 0 then begin
        ;Get the objects being removed.
        removeThese = self -> Get(ISA=type, COUNT=nRemove)
        
        ;Append the newly found objects to the input array
        if nRemove gt 0 then if n_elements(Child_Object) eq 0 $
            then Child_Object = [Child_Object, temporary(removeThese)] $
            else Child_Object = temporary(removeThese)
    endif
        
;---------------------------------------------------------------------
;Remove Child_Objects ////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if n_elements(Child_Object) gt 0 then begin
        nRemove = n_elements(Child_Object)
        
        ;Objects that are of type "data" may fall into the auto-updating plot layout.
        ;As such, when they are removed from the container, they also need to be removed
        ;from the layout.
        oTypes = typename(Child_Object)
        tf_data = IsMember((*self.gTypes).data, oTypes)
        
        ;Step through all of the objects.
        for i = 0, nRemove - 1 do begin
        
            ;Is it a data object?
            if (tf_data[i] eq 1) then begin
                ;Get their position and layout.
                Child_Object[i] -> GetLayout, LAYOUT=layout, POSITION=position
            
                ;Remove from the layout
                self -> RemoveFromLayout, layout[2]
            endif
            
            ;Remove from the container.
            self -> MrIDL_Container::Remove, Child_Object[i], DESTROY=destroy
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
;       OLDLOCATION:        in, required, type=object/{1 | 2 | 4}-element vector
;                           The plot-index, [col, row], or 4-element position of the plot
;                               whose position is to be changed. If an object is provided,
;                               then the old position will be take from it.
;       NEWLOCATION:        in, required, type={1 | 2 | 4}-element vector
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
;       DRAW:               in, optional, type=boolean, default=0
;                           Refresh the draw window when finished.
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
pro MrPlotManager::SetPosition, oldLocation, newLocation, $
DRAW = draw, $
OUTPOSITION = outPosition, $
OUTLOCATION = outLocation, $
TOFIXED = toFixed
    compile_opt idl2

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Defaults
    draw    = keyword_set(draw)
    toFixed = keyword_set(toFixed)
    
;---------------------------------------------------------------------
;Get the Object? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    case n_elements(oldLocation) of
        1: oldPIndex = oldLocation
        2: oldPIndex = self -> ConvertLocation(oldLocation, /COLROW, /TO_PINDEX)
        else: message, 'OLDLOCATION: Incorrect number of elements.'
    endcase
    
    ;Get the object
    theObj = self -> FindByPIndex(oldPIndex, COUNT=count)
    if count eq 0 then message, 'Could not find a graphic at given location.'

    ;If toFixed is set, then the actual position will not change. The plot will simply
    ;be removed from the layout and put into a fixed location.
    if toFixed eq 1 then theObj -> GetLayout, POSITION=newLocation

    case n_elements(newLocation) of
        2: begin
            ;Find a grid that will fit the new location. If a fixed location was given,
            ;make sure it is the next available one.
            if newLocation[0] lt 0 then begin
                newLocation = self -> MakeFixedLocation()
                newGrid = self.GrLayout
            endif else newGrid = newLocation > self.GrLayout
            
            ;Find the plot-index in the new grid
            newPIndex = self -> ConvertLocation(newLocation, newGrid, /COLROW, /TO_PINDEX)
            newLayout = [newGrid, newPIndex]
        endcase
        4: newLayout = newLocation
        else: message, 'NEWLOCATION: Incorrect number of elements.'
    endcase
        
;---------------------------------------------------------------------
;Update the Layout ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Use the superclass to set the position within the layout. An added benefit of
    ;this is that it will check if the locations provided are legitimate. If the interface
    ;between MrWindow and MrGrLayout is sound, then this will also mean that the graphics
    ;object that OLD_POSITION refers to is also legit.
    self -> MrGrLayout::SetPosition, oldPIndex, newLayout, $
                                     OUTPOSITION = outPosition, $
                                     OUTLOCATION = outLocation, $
                                     TOFIXED = toFixed
    
    ;If there was an error, the outputs will not be defined.
    if n_elements(outPosition) eq 0 then return
        
;---------------------------------------------------------------------
;Set the Position ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Update the position and layout    
    theObj -> SetLayout, LAYOUT=outLocation, POSITION=outPosition, UPDATE_LAYOUT=0
        
    ;Draw?
    If keyword_set(draw) then self -> Draw
end


;+
;   The purpose of this method is to provide a means of changing graphics objects
;   globally, like setting the ![PXYZ] system variables.
;-
pro MrPlotManager::SetGlobal, $
ISA=isa, $
CHARSIZE = charsize, $
CHARTHICK = charthick, $
FONT = font, $
TICKLEN = ticklen, $
TITLE = title, $
THICK = thick, $
XCHARSIZE = xcharsize, $
XGRIDSTYLE = xgridstyle, $
XMINOR = xminor, $
XRANGE = xrange, $
XSTYLE = xstyle, $
XTHICK = xthick, $
XTICKFORMAT = xtickformat, $
XTICKLAYOUT = xticklayout, $
XTICKLEN = xticklen, $
XTICKNAME = xtickname, $
XTICKS = xticks, $
XTICKUNITS = xtickunits, $
XTICKV = xtickv, $
XTITLE = xtitle, $
YCHARSIZE = ycharsize, $
YGRIDSTYLE = ygridstyle, $
YMINOR = yminor, $
YRANGE = yrange, $
YSTYLE = ystyle, $
YTHICK = ythick, $
YTICKFORMAT = ytickformat, $
YTICKLAYOUT = yticklayout, $
YTICKLEN = yticklen, $
YTICKNAME = ytickname, $
YTICKS = yticks, $
YTICKUNITS = ytickunits, $
YTICKV = ytickv, $
YTITLE = ytitle, $
ZCHARSIZE = zcharsize, $
ZGRIDSTYLE = zgridstyle, $
ZMINOR = zminor, $
ZRANGE = zrange, $
ZSTYLE = zstyle, $
ZTHICK = zthick, $
ZTICKFORMAT = ztickformat, $
ZTICKLAYOUT = zticklayout, $
ZTICKLEN = zticklen, $
ZTICKNAME = ztickname, $
ZTICKS = zticks, $
ZTICKUNITS = ztickunits, $
ZTICKV = ztickv, $
ZTITLE = ztitle
    compile_opt idl2

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self -> Refresh, DISABLE=~refresh_in
        void = cgErrorMsg()
        return
    endif

    ;Disable refresh
    self -> GetProperty, REFRESH=refresh_in
    self -> Refresh, /DISABLE
    
    ;If we are changing CHARSIZE, set the character size of the window, too
    if n_elements(charsize) gt 0 then self -> SetProperty, CHARSIZE=charsize

    ;Get all of the objects
    allObjs = self -> Get(/ALL, ISA=isa, COUNT=nObjs)
    if nObjs eq 0 then return
    
    ;Step through each object and set the pertinent values
    for i = 0, nObjs - 1 do begin
        oClass = obj_class(allObjs[i])
    
;---------------------------------------------------------------------
;Data Objects ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
        if IsMember((*self.gTypes).data, oClass) then begin
            allObjs[i] -> SetProperty, CHARSIZE    = charsize, $
                                       CHARTHICK   = charthick, $
                                       FONT        = font, $
                                       THICK       = thick, $
                                       TICKLEN     = ticklen, $
                                       TITLE       = title, $
                                       XCHARSIZE   = xcharsize, $
                                       XGRIDSTYLE  = xgridstyle, $
                                       XMINOR      = xminor, $
                                       XRANGE      = xrange, $
                                       XSTYLE      = xstyle, $
                                       XTHICK      = xthick, $
                                       XTICKFORMAT = xtickformat, $
                                       XTICKLAYOUT = xticklayout, $
                                       XTICKLEN    = xticklen, $
                                       XTICKNAME   = xtickname, $
                                       XTICKS      = xticks, $
                                       XTICKUNITS  = xtickunits, $
                                       XTICKV      = xtickv, $
                                       XTITLE      = xtitle, $
                                       YCHARSIZE   = ycharsize, $
                                       YGRIDSTYLE  = ygridstyle, $
                                       YMINOR      = yminor, $
                                       YRANGE      = yrange, $
                                       YSTYLE      = ystyle, $
                                       YTHICK      = ythick, $
                                       YTICKFORMAT = ytickformat, $
                                       YTICKLAYOUT = yticklayout, $
                                       YTICKLEN    = yticklen, $
                                       YTICKNAME   = ytickname, $
                                       YTICKS      = yticks, $
                                       YTICKUNITS  = ytickunits, $
                                       YTICKV      = ytickv, $
                                       YTITLE      = ytitle, $
                                       ZCHARSIZE   = zcharsize, $
                                       ZGRIDSTYLE  = zgridstyle, $
                                       ZMINOR      = zminor, $
                                       ZRANGE      = zrange, $
                                       ZSTYLE      = zstyle, $
                                       ZTHICK      = zthick, $
                                       ZTICKFORMAT = ztickformat, $
                                       ZTICKLAYOUT = zticklayout, $
                                       ZTICKLEN    = zticklen, $
                                       ZTICKNAME   = ztickname, $
                                       ZTICKS      = zticks, $
                                       ZTICKUNITS  = ztickunits, $
                                       ZTICKV      = ztickv, $
                                       ZTITLE      = ztitle
                                       
;---------------------------------------------------------------------
;Axis Objects ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
        endif else if oClass eq 'MRAXIS' then begin
            allObjs[i] -> GetProperty, DIRECTION=direction
            
            case direction of
                'X': begin
                    allObjs[i] -> SetProperty, AXIS_RANGE   = xrange, $
                                               CHARSIZE     = charsize, $
                                               CHARTHICK    = charthick, $
                                               COLOR        = color, $
                                               FONT         = font, $
                                               GRIDSTYLE    = xgridstyle, $
                                               LOG          = xlog, $
                                               MINOR        = xminor, $
                                               SUBTITLE     = xsubtitle, $
                                               STYLE        = xsytle, $
                                               THICK        = xthick, $
                                               TICKFORMAT   = xtickformat, $
                                               TICKINTERVAL = xtickinterval, $
                                               TICKLAYOUT   = xticklayout, $
                                               TICKLEN      = xticklen, $
                                               TICKNAME     = xtickname, $
                                               TICKS        = xticks, $
                                               TICKUNITS    = xtickunits, $
                                               TICKVALUES   = xtickvalues, $
                                               TITLE        = xtitle, $
                                               XCHARSIZE    = xcharsize
                endcase
                
                'Y': begin
                    allObjs[i] -> SetProperty, AXIS_RANGE   = xrange, $
                                               CHARSIZE     = charsize, $
                                               CHARTHICK    = charthick, $
                                               COLOR        = color, $
                                               FONT         = font, $
                                               GRIDSTYLE    = ygridstyle, $
                                               LOG          = ylog, $
                                               MINOR        = yminor, $
                                               SUBTITLE     = ysubtitle, $
                                               STYLE        = ysytle, $
                                               THICK        = ythick, $
                                               TICKFORMAT   = ytickformat, $
                                               TICKINTERVAL = ytickinterval, $
                                               TICKLAYOUT   = yticklayout, $
                                               TICKLEN      = yticklen, $
                                               TICKNAME     = ytickname, $
                                               TICKS        = yticks, $
                                               TICKUNITS    = ytickunits, $
                                               TICKVALUES   = ytickvalues, $
                                               TITLE        = ytitle, $
                                               XCHARSIZE    = ycharsize
                
                endcase
                
                'Z': begin
                    allObjs[i] -> SetProperty, AXIS_RANGE   = xrange, $
                                               CHARSIZE     = charsize, $
                                               CHARTHICK    = charthick, $
                                               COLOR        = color, $
                                               FONT         = font, $
                                               GRIDSTYLE    = zgridstyle, $
                                               LOG          = zlog, $
                                               MINOR        = zminor, $
                                               SUBTITLE     = zsubtitle, $
                                               STYLE        = zsytle, $
                                               THICK        = zthick, $
                                               TICKFORMAT   = ztickformat, $
                                               TICKINTERVAL = ztickinterval, $
                                               TICKLAYOUT   = zticklayout, $
                                               TICKLEN      = zticklen, $
                                               TICKNAME     = ztickname, $
                                               TICKS        = zticks, $
                                               TICKUNITS    = ztickunits, $
                                               TICKVALUES   = ztickvalues, $
                                               TITLE        = ztitle, $
                                               XCHARSIZE    = zcharsize
                endcase
            endcase
                                       
;---------------------------------------------------------------------
;Other Annotation Objects ////////////////////////////////////////////
;---------------------------------------------------------------------
        endif else if IsMember(['MRTEXT', 'WECOLORBAR', 'WELEGENDITEM'], oClass) then begin
            allObjs[i] -> SetProperty, CHARSIZE = charsize, $
                                       CHARTHICK = charthick
        endif
    endfor
        
    ;Reset the refresh state.
    self -> Refresh, DISABLE=~refresh_in
end


;+
;   Shift all plots located at and after COLROW up one index value.
;
; :Params:
;       PINDEX:             in, required, type=int
;                           The plot index location at which to begin shifting plots.
;-
pro MrPlotManager::ShiftPlots, pIndex
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    layout_init = self.GrLayout
    
;---------------------------------------------------------------------
;Start and Stop Shift ////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Start shifting at the given plot index. Get an invariant [col,row] location
    pStartShift = pIndex
    colrow = self -> ConvertLocation(pIndex, /PINDEX, /TO_COLROW)
    
    ;Get the available list indices
    void = self -> IsAvailable(IFREE=pFree, NFREE=nFree, /PINDEX)
    
    ;Which interval is being shifted? If a new row needs to be added,
    ;re-calculate the starting plot index within the new layout. Anticipate
    ;what MrGrLayout::ShiftPlots will do by adding a row to the layout.
    if nFree eq 0 || max(pFree gt pStartShift) eq 0 then begin
        layout = self.GrLayout + [0,1]
        pStartShift = self -> ConvertLocation(colrow, layout, /COLROW, /TO_PINDEX)
        pStopShift = self -> ConvertLocation([1, layout[1]], layout, /COLROW, /TO_PINDEX)
    endif else begin
        layout = self.GrLayout
        pStopShift = pFree[min(where((pFree gt pStartShift) eq 1))]
    endelse
    
;---------------------------------------------------------------------
;Superclass //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Shift the layout so that the layout_positions are up-to-date.
    self -> MrGrLayout::ShiftPlots, pIndex

;---------------------------------------------------------------------
;Shift Plots /////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Get all of the relevant objects
    theseObj = self -> Get(/ALL, ISA=(*self.gTypes).data, COUNT=nObj)
    
    ;Start shifting them
    for i = 0, nObj - 1 do begin
        theseObj[i] -> GetLayout, LAYOUT=objLayout
        
        ;THESEOBJ are in no particular order. Must check them all.
        if objLayout[2] lt pStartShift || objLayout[2] ge pStopShift then continue

        ;If we make it to here, shift the plot
        thisPIndex = objLayout[2] + 1
        aIndex = self -> ConvertLocation(thisPIndex, /PINDEX, /TO_AINDEX)
        newPosition = (*self.layout_positions)[*, aIndex]
        theseObj[i] -> SetLayout, LAYOUT=[self.GrLayout, thisPIndex], POSITION=newPosition, UPDATE_LAYOUT=0
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
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
        
    ;Call the superclass
    self -> MrGrLayout::TrimLayout
    
    ;Apply the new positions
    self -> ApplyPositions
    
    ;Draw?
    if keyword_set(draw) then self -> Draw
end


;+
;   Determine which type of object was given.
;
; :Private:
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
        void = cgErrorMsg()
        return, ''
    endif
    
    ;What type of object is it?
    className = typename(objRef)
    case className of
        'MRAXIS':       ImA = 'AXIS'
        'MRCOLORFILL':  ImA = 'COLORFILL'
        'MRCONTOUR':    ImA = 'CONTOUR'
        'MRIMAGE':      ImA = 'IMAGE'
        'MRPLOT':       ImA = 'PLOT'
        'MRPLOTS':      ImA = 'PLOTS'
        'MRTEXT':       ImA = 'TEXT'
        'MRVECTOR':     ImA = 'VECTOR'
        'WECOLORBAR':   ImA = 'COLORBAR'
        'WEARROW':      ImA = 'ARROW'
        'WEOVERPLOT':   ImA = 'OVERPLOT'
        'WELEGENDITEM': ImA = 'LEGEND'
        else:           ImA = ''
    endcase
    
    return, ImA
end


;+
;   The purpose of this method is to create a structure of supported classes for each
;   graphics type.
;
; :Private:
;
;-
pro MrPlotManager::Config
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Class names of the supported graphics types
    types = { $
              arrow: ['WEARROW'], $
              axis: ['MRAXIS'], $
              colorbar: ['WECOLORBAR'], $
              contour: ['MRCONTOUR'], $
              image: ['MRIMAGE'], $
              legend: ['WELEGENDITEM'], $
              overplot: ['WEOVERPLOT'], $
              plot: ['MRPLOT'], $
              plots: ['MRPLOTS'], $
              polyfill: ['MRCOLORFILL'], $
              text: ['MRTEXT'], $
              vector: ['MRVECTOR'], $
              ImAData: ['PLOT', 'IMAGE', 'CONTOUR', 'VECTOR'], $     ;TO BE USED WITH ::WHATAMI
              data: ['MRPLOT', 'MRIMAGE', 'MRCONTOUR', 'MRVECTOR'], $
              annotate: ['WECOLORBAR', 'MRAXIS', 'WELEGENDITEM', 'WEARROW', 'MRTEXT', 'MRPLOTS', 'WEOVERPLOT', 'MRCOLORFILL'], $
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
    self -> MrGrLayout::Cleanup
    self -> MrIDL_Container::Cleanup
    
    ;Free pointers
    ptr_free, self.gTypes
end


;+
;   The initialization method.
;
; :Keywords:
;       _REF_EXTRA:                 in, optional, type=structure
;                                   Any keyword accepted by MrGrLayout__define is also
;                                       accepted for keyword inheritance.
;-
function MrPlotManager::init, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif

    ;Setup the plot window
    if self -> MrGrLayout::init(_STRICT_EXTRA=extra) eq 0 then return, 0
    
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
               inherits MrIDL_Container, $      ;An object container.
               inherits MrGrLayout, $           ;Manage plot layout.
               inherits IDL_Object, $           ;IDL 8.0 dot referencing.
               gTypes: ptr_new() $              ;Supported graphics types.
             }
end