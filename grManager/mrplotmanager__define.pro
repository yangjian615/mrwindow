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
;-
;*****************************************************************************************
;+
;   Event handler for the EDIT | Layout menu.
;-
pro MrPlotManager::AdjustLayout_Property, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if ptr_valid(new_layout) then ptr_free, new_layout
        void = error_message()
        return
    endif
    
    ;Spawn a GUI to get user input about the layout
    new_layout = plotpositions_gui(self.tlb, $
                                   LAYOUT  = self.GrLayout, $
                                   XGAP    = self.xgap, $
                                   XMARGIN = self.xmargin, $
                                   YGAP    = self.ygap, $
                                   YMARGIN = self.ymargin)
    if ptr_valid(new_layout) eq 0 then return
    
    ;Update the layout
    self -> SetProperty, LAYOUT  = (*new_layout).layout, $
                         XGAP    = (*new_layout).xgap, $
                         XMARGIN = (*new_layout).xmargin, $
                         YGAP    = (*new_layout).ygap, $
                         YMARGIN = (*new_layout).ymargin
    
    ;Redraw
    self -> Draw
    
    ;Free the pointer
    ptr_free, new_layout
end


;+
;   Event handler for the EDIT | Move menu.
;-
pro MrPlotManager::AdjustLayout_Move, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if ptr_valid(colrow) then ptr_free, colrow
        void = error_message()
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
;-
pro MrPlotManager::AdjustLayout_Remove, event
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
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
        void = error_message()
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
        void = error_message()
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
        theObjects[i] -> GetProperty, POSITION=pos, LAYOUT=layout
        
        ;Record the initial position for comparison.
        if n_elements(pos) eq 0 $
            then pos_init = !Null $
            else pos_init = pos
        
    ;---------------------------------------------------------------------
    ;Check Location //////////////////////////////////////////////////////
    ;---------------------------------------------------------------------

        ;If LAYOUT[0:1]=[0,0], then no layout has been established. In this case, check
        ;the position of the graphic.
        if total(layout[0:1]) eq 0 then begin
            
            ;Check for the default Coyote Graphics location (see MrGraphicAtom__Define).
            if (n_elements(pos) gt 0) && (array_equal(pos, [0.125, 0.125, 0.925, 0.9]) || $
                (total(pos) eq 0)) then void = temporary(pos)
        
        ;If the [col,row] location was not specified, but a position was given,
        ;make LOC undefined to indicate that the graphic will be placed at a fixed
        ;position.
        endif else if (total(layout[2:*]) eq 0) and n_elements(pos) gt 0 then begin
            void = temporary(loc)

        ;Otherwise, a layout has been establed. Check LAYOUT[3] for a plot index number.
        ;If it is non-zero, then that is the location of the plot. Otherwise, we need to
        ;generate a location.
        endif else if n_elements(layout) eq 3 then begin
            if  layout[2] gt 0 then loc = layout[2]
        
        ;If a layout has been established and LAYOUT[2:3] is a [col,row] location, then
        ;use it. Otherwise, a location will be generated later.
        endif else if n_elements(layout) eq 4 then begin
            if (layout[2] gt 0) and (layout[3] gt 0) then loc = layout[2:3]
            
        ;If none of the above are true, then the position is ill defined.
        endif else begin
            print, FOMRAT='(%"Cannot add object %s at index %i". Improper layout)', $
                   typename(theObjects[i]), i
            print, 'Layout = ', layout
            continue
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
        theObjects[i] -> SetLayout, LAYOUT=[self.GrLayout[0:1], loc], POSITION=pos, $
                                    ASPECT=*self.aspect, CHARSIZE=self.charsize, $
                                    OXMARGIN=self.oxmargin, OYMARGIN=self.oymargin, $
                                    XMARGIN=self.xmargin, XGAP=self.xgap, $
                                    YMARGIN=self.ymargin, YGAP=self.ygap, $
                                    UPDATE_LAYOUT=0

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
        if nLayout eq 0 || layout[2] eq -1 then continue
        
        ;Get the [col,row] location of the plot. Ensure that the object's layout matches
        ;the current layout.
        case nLayout of
            3: begin
                if array_equal(self.GrLayout, layout[0:1]) eq 0 then begin
                    dataObjs[i] -> SetLayout, LAYOUT=[self.GrLayout, layout[2]], UPDATE_LAYOUT=0
                    thisColRow = self -> ConvertLocation(layout[2], self.GrLayout, /PLOT_INDEX, /TO_COLROW)
                endif else thisColRow = self -> ConvertLocation(layout[2], /PLOT_INDEX, /TO_COLROW)
            endcase
            
            4: begin
                if (array_equal(self.GrLayout, layout[0:1]) eq 0) and (total(layout[2:*]) gt 0) then begin
                    thisPIndex = self -> ConvertLocation(layout[2:3], layout[0:1], /TO_PLOT_INDEX)
                    thisColRow = self -> ConvertLocation(thisPIndex, self.GrLayout, /PLOT_INDEX, /TO_COLROW)
                    dataObjs[i] -> SetLayout, LAYOUT=[self.GrLayout, thisColRow], UPDATE_LAYOUT=0
                endif else thisColRow = layout[2:3]
            endcase
            
            else: message, 'Layout has incorrect format.'
        endcase

        ;Update the positions of each plot.
        position = (*self.layout_positions)[*, thisColRow[0]-1, thisColRow[1]-1]
        dataObjs[i] -> SetLayout, POSITION=position, UPDATE_LAYOUT=0, $
                                  ASPECT=*self.aspect, CHARSIZE=self.charsize, $
                                  OXMARGIN=self.oxmargin, OYMARGIN=self.oymargin, $
                                  XMARGIN=self.xmargin, XGAP=self.xgap, $
                                  YMARGIN=self.ymargin, YGAP=self.ygap
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
    pHoles = where(tf_available eq 0, nHoles) + 1
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
        if nLayout eq 0 || layout[2] eq -1 then continue
        
        case nLayout of
            3: thisPIndex = layout[2]
            4: thisPIndex = self -> ConvertLocation(layout[2:3], self.GrLayout, /TO_PLOT_INDEX)
            else: message, 'Layout has incorrect format. Cannot be shifted.'
        endcase

        ;If the plot is before the hole, then skip it.
        if thisPIndex le pHoles[0] then continue

        ;If we make it to here, fill a hole.
        newLocation = self -> ConvertLocation(pHoles[0], /PLOT_INDEX, /TO_COLROW)
        newPosition = (*self.layout_positions)[*, newLocation[0]-1, newLocation[1]-1]
        theseObj[i] -> SetLayout, LAYOUT=[self.GrLayout, newLocation], POSITION=newPosition, UPDATE_LAYOUT=0
        
        ;Now there is a hole in the old location
        pHoles[0] = thisPIndex
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
FIXED = fixed, $
ISA = IsA, $
LOCATION = location, $
PINDEX = plot_index, $
POSITION = index, $
COUNT = count, $
;TYPE
ANNOTATE = annotate, $
ARROW = arrow, $
AXIS = axis, $
COLORBAR = colorbar, $
CONTOUR = contour, $
DATA = data, $
IMAGE = image, $
LEGEND = legend, $
OVERPLOT = overplot, $
PLOT = plot, $
TEXT = text
    compile_opt idl2

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        Count = 0
        return, obj_new()
    endif
    
    ;Defaults
    SetDefaultValue, fixed, 0
    plot_index = keyword_set(plot_index)

    ;Make sure at most one of these keywords is set.
    if fixed + plot_index gt 1 then message, 'FIXED and PLOT_INDEX are mutually exclusive.'

;---------------------------------------------------------------------
;Find by Fixed Location //////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(location) gt 0 && location[0] lt 0 then begin
        AllObj = self -> MrIDL_Container::Get(/ALL, ISA=(*self.gTypes).data, COUNT=nObj)
        i = 0
        success = 0
        while success eq 0 and i lt nObj do begin
            AllObj[i] -> GetProperty, LAYOUT=oLayout
            if n_elements(oLayout) eq 4 then success = array_equal(location, oLayout[2:3])
            i++
        endwhile

        if success $
            then result = AllObj[i-1] $
            else result = obj_new()
    
;---------------------------------------------------------------------
;Find by Location ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else if n_elements(location) gt 0 then begin
        if min(location[0,*] lt 0) then $
            message, 'Invalid LOCATION. Use /FIXED with 4-elements position.'
        
        ;Determine what LOCATION means.
        if keyword_set(plot_index) then begin
            PIndex_in = location
            nIn = n_elements(PIndex_in)
        endif else if keyword_set(fixed) then begin
            pos_in = location
            nIn = n_elements(pos_in[0,*])
        endif else begin
            PIndex_in = self -> ConvertLocation(location, /TO_PLOT_INDEX)
            nIn = n_elements(PIndex_in)
        endelse

        ;Get all of the data objects
        AllObj = self -> MrIDL_Container::Get(/ALL, ISA=(*self.gTypes).data, COUNT=nObj)
        Result = objarr(nObj)
        count = 0

    ;---------------------------------------------------------------------
    ;Loop through All Objects in Container ///////////////////////////////
    ;---------------------------------------------------------------------
        for i = 0, nObj - 1 do begin
            skip = 0
        
            ;Get the layout and position
            AllObj[i] -> GetProperty, LAYOUT=oLayout, POSITION=oPosition

        ;---------------------------------------------------------------------
        ;Find a Objects at Fixed Positions ///////////////////////////////////
        ;---------------------------------------------------------------------
            
            if fixed eq 1 then begin
                ;See if the current graphic matches any of the positions given
                delta = min(abs(mean(pos_in - rebin(oPosition, 4, nIn), DIMENSION=1)))

                ;If it does, return the graphic. If not, continue to the next one.
                if delta lt 1e-5 $
                    then Result[count] = allObj[i] $
                    else continue

                ;If we get to here, a graphic was stored. Increase the count and
                ;continue to the next graphic object.
                count += 1
                continue
            endif

        ;---------------------------------------------------------------------
        ;Find a Objects in Layout Positions //////////////////////////////////
        ;---------------------------------------------------------------------
            
            ;Find the plot index of the object
            nLayout = n_elements(oLayout)
            case nLayout of
                0: if n_elements(oPosition) eq 0 $
                       then skip = 1 $
                       else location = self -> FindLocation(oPosition, /FIXED)
                3: pIndex = oLayout[2]
                4: pIndex = self -> ConvertLocation(oLayout[2:3], /TO_PLOT_INDEX)
                else: skip = 1
            endcase
            
            ;If no valid layout or position was provided, then go to the next object.
            if skip eq 1 then continue

            ;Return this object? If the plot-index of the current graphic matches any
            ;of the inputs plot-indices, then yes.
            tf_get = isMember(pIndex_in, pIndex, N_MATCHES=nGet)
            if nGet eq 0 || tf_get eq 0 $
                then continue $
                else Result[count] = AllObj[i]
            
            ;If we get to here, increase the object count
            count += 1
        endfor

    ;---------------------------------------------------------------------
    ;Trim Results ////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        case count of
            0: return, !Null
            1: Result = Result[0]
            else: Result = Result[0:count-1]
        endcase

;---------------------------------------------------------------------
;Normal Get //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        ;Get specific object types?
        if keyword_set(annotate) then if n_elements(isa) eq 0 then isa = (*self.gTypes).annotate else isa = [isa, (*self.gTypes).annotate]
        if keyword_set(arrow)    then if n_elements(isa) eq 0 then isa = (*self.gTypes).arrow    else isa = [isa, (*self.gTypes).arrow]
        if keyword_set(axis)     then if n_elements(isa) eq 0 then isa = (*self.gTypes).axis     else isa = [isa, (*self.gTypes).axis]
        if keyword_set(colorbar) then if n_elements(isa) eq 0 then isa = (*self.gTypes).colorbar else isa = [isa, (*self.gTypes).colorbar]
        if keyword_set(contour)  then if n_elements(isa) eq 0 then isa = (*self.gTypes).contour  else isa = [isa, (*self.gTypes).contour]
        if keyword_set(data)     then if n_elements(isa) eq 0 then isa = (*self.gTypes).data     else isa = [isa, (*self.gTypes).data]
        if keyword_set(image)    then if n_elements(isa) eq 0 then isa = (*self.gTypes).image    else isa = [isa, (*self.gTypes).image]
        if keyword_set(legend)   then if n_elements(isa) eq 0 then isa = (*self.gTypes).legend   else isa = [isa, (*self.gTypes).legend]
        if keyword_set(overplot) then if n_elements(isa) eq 0 then isa = (*self.gTypes).overplot else isa = [isa, (*self.gTypes).overplot]
        if keyword_set(plot)     then if n_elements(isa) eq 0 then isa = (*self.gTypes).plot     else isa = [isa, (*self.gTypes).plot]
        if keyword_set(text)     then if n_elements(isa) eq 0 then isa = (*self.gTypes).text     else isa = [isa, (*self.gTypes).text]
        
        ;Make sure All is used with IsA
        if n_elements(IsA) gt 0 then All = 1
        
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
                Child_Object[i] -> GetProperty, LAYOUT=layout, POSITION=position
            
                ;Get their [col, row] location.
                case n_elements(layout) of
                    0: thisLoc = self -> FindFixedLocation(position)
                    3: thisLoc = self -> ConvertLocation(layout[2:3], layout, /PLOT_INDEX, /TO_COLROW)
                    4: thisLoc = layout[2:3]
                    else: message, 'Layout has incorrect format. Cannot be removed.'
                endcase
                
                ;Remove from the layout
                self -> RemoveFromLayout, thisLoc
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
;       OLD_POSITION:       in, required, type=object/{1 | 2 | 4}-element vector
;                           The plot-index, [col, row], or 4-element position of the plot
;                               whose position is to be changed. If an object is provided,
;                               then the old position will be take from it.
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
pro MrPlotManager::SetPosition, old_position, new_position, $
DRAW = draw, $
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

    ;Defaults
    draw    = keyword_set(draw)
    toFixed = keyword_set(toFixed)
    
    oldType = size(old_position, /TYPE)
    
;---------------------------------------------------------------------
;Was an Object Given? ////////////////////////////////////////////////
;---------------------------------------------------------------------
    if oldType eq 11 then begin
        theObj = old_position
        theObj -> GetProperty, LAYOUT=layout, POSITION=position
        
        ;Turn the layout position into a [col, row] location.
        case n_elements(layout) of
            0: oldColRow = self -> FindLocation(position, /FIXED)
            3: oldColRow = self -> ConvertLocation(layout[2], /PLOT_INDEX, /TO_COLROW)
            4: oldColRow = layout[2:3]
            else: message, 'LAYOUT has incorrect format of object provided.'
        endcase
        
;---------------------------------------------------------------------
;Get the Object? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin

        case n_elements(old_position) of
            1: oldColRow = self -> ConvertLocation(old_position, /PLOT_INDEX, /TO_COL_ROW)
            2: begin
                oldColRow = old_position
;                if oldColRow[0] eq -1 then begin
;                    oldColRow = self -> GetPosition(oldColRow)
;                    fixed = 1
;                endif
            endcase
            4: begin
                oldColRow = self -> FindLocation(old_position)
                if count eq 0 then message, 'No object found at OLD_LOCATION. Cannot set position.'
                if count gt 1 then message, 'More than one graphic found. Supply [col, row] instead.'
            endcase
            else: message, 'OLD_POSITION: Incorrect number of elements.'
        endcase
        
        ;Get the object
        theObj = self -> Get(LOCATION=oldColRow, FIXED=fixed)
    endelse
            

    ;If toFixed is set, then the actual position will not change. The plot will simply
    ;be removed from the layout and put into a fixed location.
    if toFixed eq 1 then begin
        theObj -> GetProperty, POSITION=position
        new_position = position
    endif
        
;---------------------------------------------------------------------
;Update the Layout ///////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Use the superclass to set the position within the layout. An added benefit of
    ;this is that it will check if the locations provided are legitimate. If the interface
    ;between MrWindow and MrGrLayout is sound, then this will also mean that the graphics
    ;object that OLD_POSITION refers to is also legit.
    self -> MrGrLayout::SetPosition, oldColRow, new_position, $
                                     OUTPOSITION = outPosition, $
                                     OUTLOCATION = outLocation, $
                                     TOFIXED = toFixed
    
    ;If there was an error, the outputs will not be defined.
    if n_elements(outPosition) eq 0 then return
        
;---------------------------------------------------------------------
;Set the Position ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Update the position and layout    
    theObj -> SetLayout, LAYOUT=[self.GrLayout, outLocation], POSITION=outPosition, UPDATE_LAYOUT=0
        
    ;Draw?
    If keyword_set(draw) then self -> Draw
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
    
    layout_init = self.GrLayout
    
;---------------------------------------------------------------------
;Start and Stop Shift ////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Convert LOCATION to a plot index
    pStartShift = self -> ConvertLocation(location, /TO_PLOT_INDEX)
    
    ;Get the available list indices
    void = self -> IsAvailable(IFREE=pFree, NFREE=nFree, /PLOT_INDEX)
    
    ;Which interval is being shifted? If a new row needs to be added,
    ;re-calculate the starting plot index within the new layout.
    if nFree eq 0 || max(pFree gt pStartShift) eq 0 then begin
        layout = self.GrLayout + [0,1]
        pStartShift = self -> ConvertLocation(location, layout, /TO_PLOT_INDEX)
        pStopShift = self -> ConvertLocation([1, layout[1]], layout, /TO_PLOT_INDEX)
    endif else begin
        layout = self.GrLayout
        pStopShift = pFree[min(where((pFree gt pStartShift) eq 1))]
    endelse
    
;---------------------------------------------------------------------
;Superclass //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Shift the layout so that the layout_positions are up-to-date.
    self -> MrGrLayout::ShiftPlots, location

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
        
        ;THESEOBJ are in no particular order. Must check them all.
        if thisPIndex lt pStartShift then continue
        if thisPIndex ge pStopShift then continue

        ;If we make it to here, shift the plot
        thisPIndex += 1
        newLocation = self -> ConvertLocation(thisPIndex, layout, /PLOT_INDEX, /TO_COLROW)
        newPosition = (*self.layout_positions)[*, newLocation[0]-1, newLocation[1]-1]
        theseObj[i] -> SetLayout, LAYOUT=[layout, newLocation], POSITION=newPosition, UPDATE_LAYOUT=0
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
    self -> MrGrLayout::TrimLayout
    
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
        'MRPLOT':       ImA = 'PLOT'
        'MRIMAGE':      ImA = 'IMAGE'
        'MRCONTOUR':    ImA = 'CONTOUR'
        'WECOLORBAR':   ImA = 'COLORBAR'
        'WETEXT':       ImA = 'TEXT'
        'WEARROW':      ImA = 'ARROW'
        'WEOVERPLOT':   ImA = 'OVERPLOT'
        'WELEGENDITEM': ImA = 'LEGEND'
        'WEAXIS':       ImA = 'AXIS'
        else:           ImA = ''
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
    types = { plot: ['PLOT', 'MRPLOT'], $
              image: ['IMAGE', 'MRIMAGE'], $
              contour: ['CONTOUR', 'MRCONTOUR'], $
              colorbar: ['WECOLORBAR'], $
              axis: ['WEAXIS'], $
              legend: ['WELEGENDITEM'], $
              arrow: ['WEARROW'], $
              text: ['WETEXT'], $
              overplot: ['WEOVERPLOT'], $
              plots: ['MRPLOTS'], $
              ImAData: ['PLOT', 'IMAGE', 'CONTOUR'], $     ;TO BE USED WITH ::WHATAMI
              data: ['PLOT', 'MRPLOT', 'IMAGE', 'MRIMAGE', 'CONTOUR', 'MRCONTOUR'], $
              annotate: ['WECOLORBAR', 'WEAXIS', 'WELEGENDITEM', 'WEARROW', 'WETEXT', 'MRPLOTS', 'WEOVERPLOT'], $
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
        void = error_message()
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