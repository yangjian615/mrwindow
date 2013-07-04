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
;                                   
;-
;*****************************************************************************************
;+
;   Add an object to the master list. This method SHOULD NOT be used directly. Instead,
;   use Add_Plots, Add_Images, etc.
;
; :Params:
;       THEOBJECTS:            in, optional, type=object/obj_arr(N)
;                               The plot object(s) to add to the display.
;
;   :Keywords:
;       LOCATION:               in, out, optional, type=intarr(2\,N)
;                               The [col, row] location at which `THEOBJECTS` will be
;                                   placed into the 2D plotting grid. If none is provided,
;                                   LOCATION will be chosen and returned.
;       POSITION:               in, out, optional, type=intarr(4\,N)
;                               The position of the lower-right and upper-left corners
;                                   of each `THEOBJECTS`. If none is provide, a POSITION
;                                   will be chosen and returned.
;-
pro MrPlotManager::Add, theObjects, $
ADDTOLAYOUT = addtolayout, $
LOCATION = location, $
POSITION = position
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
        
    nObj = n_elements(theObjects)
    SetDefaultValue, addToLayout, 1, /BOOLEAN
        
;---------------------------------------------------------------------
;Add to Master List //////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If nothing was given to add, then return
    if nObj eq 0 then begin
        if ptr_valid(self.allObjects) eq 0 then self.allObjects = ptr_new(/ALLOCATE_HEAP)
        return
    endif

    ;Add the object.
    if ptr_valid(self.allObjects) $
        then *self.allObjects = [*self.allObjects, theObjects] $
        else self.allObjects = ptr_new(theObjects)
        
    ;How many locations and positions were provided?
    nLocs = n_elements(location)/2
    nPos = n_elements(position)/4
    if nLocs eq 0 then location_out = intarr(2, nObj)
    if nPos eq 0 then position_out = fltarr(4, nObj)
    
    ;Make sure a location and position is defined for each plot
    for i = 0, nObj - 1 do begin
        
        ;If no position was given, check the object property
        ;Positions being added are defined. Positions being created are initially undefined.
        if nPos eq 0 $
            then theObjects[i] -> GetProperty, POSITION=thisPosition $
            else thisPosition = position[*,i]

        ;Locations being added are defined. Locations being created are initially undefined.
        if nLocs gt 0 then thisLocation = location[*,i] else void = temporary(thisLocation)

        ;Set the location and position of the new plot.
        self -> SetPositions, thisLocation, POSITION=thisPosition, /ADD, $
                              ADJUST_LAYOUT=adjust_layout
        
        ;Store the location and position if they were created.
        if nLocs eq 0 then location_out[*,i] = temporary(thisLocation)
        if nPos eq 0 then position_out[*,i] = temporary(thisPosition)
    endfor
    
    ;Return the locations and positions if they were not given.
    if nLocs eq 0 then location = temporary(location_out)
    if nPos eq 0 then position = temporary(position_out)
    
end


;+
;   Add an colorbar object.
;
; :Params:
;       CBOBJECTS:              in, optional, type=object/obj_arr
;                               The weLegendItem or colorbar object(s) to add.
;       REFLOCATIONS:           in, optional, type=intarr(2\,N), default=intarr(2\,N)
;                               The positions of plots by which the colorbars are to be
;                                   placed.
;
;   :Keywords:
;       CLEAR:                  in, optional, type=boolean, default=0
;                               If set, clear all legend objects. New ones can be added
;                                   in same call.
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being cleared, replaced, or removed.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the legends to the list.
;       INDEX:                  in, optional, type=int, default=0
;                               The index at which to `REPLACE` or `REMOVE` legend objects.
;       REMOVE:                 in, optional, type=boolean, default=0
;                               If set, remove the legend object and `INDEX` from the list.
;       REPLACE:                in, optional, type=boolean, default=0
;                               If set, replace the object at `INDEX` instead of appending
;                                   to the end of the list.
;
;       LOCATIONS:              in, optional, type=string/strarr, default='RIGHT'
;                               The location of the color bar with respect to the graph.
;                               Used only with `REFPOSITIONS`. Options are::
;                                   'RIGHT' -   To the right of the graph.
;                                   'LEFT'  -   To the left of the graph.
;                                   'TOP'   -   Above the graph.
;                                   'BOTTOM'-   Below the graph.
;       WIDTHS:                 in, optional, type=int/intarr, default=5
;                               The width of the color bars in character units. Used with
;                                   `REFLOCATIONS`.
;       OFFSETS:                in, optional, type=int/intarr, default=3
;                               The offset from the graphs' axes to the color bars' axis.
;                                   Used with `REFLOCATIONS`.
;-
pro MrPlotManager::AddColorBars, cbObjects, refLocations, $
CLEAR = clear, $
DESTROY = destroy, $
DRAW = draw, $
INDEX = index, $
REMOVE = remove, $
REPLACE = replace, $

LOCATIONS = locations, $
OFFSETS = offsets, $
WIDTHS = widths
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    nCB = n_elements(cbObjects)
    nrl = n_elements(refLocations)/2
    if nrl ne nCB && nrl ne 0 then $
        message, 'The # of REFLOCATIONS must match the # of CBOBJECTS.'
    
    SetDefaultValue, destroy, 1, /BOOLEAN
    clear = keyword_set(clear)
    draw = keyword_set(draw)
    remove = keyword_set(remove)
    replace = keyword_set(replace)
    
    if nrl ne 0 then begin
        SetDefaultValue, locations, replicate('', nCB)
        SetDefaultValue, offsets, replicate(3, nCB)
        SetDefaultValue, widths, replicate(5, nCB)
    endif

;---------------------------------------------------------------------
;Replace, Remove, Clear //////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Replace.
    if keyword_set(replace) then begin
        self -> ReplaceColorbars, cbObjects, index, DESTROY=destroy, DRAW=draw
        return
    endif
    
    ;Remove
    if keyword_set(remove) then begin
        self -> Remove, index, DESTROY=destroy, DRAW=draw
        return
    endif
    
    ;Clear
    if keyword_set(clear) then self -> Clear, DESTROY=destroy, DRAW=draw

;---------------------------------------------------------------------
;Add /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If nothing was given to add, then return
    if n_elements(cbObjects) eq 0 then begin
        if ptr_valid(self.colorbars) eq 0 then self.colorbars = ptr_new(/ALLOCATE_HEAP)
        if ptr_valid(self.cblocations) eq 0 then self.cblocations = ptr_new(/ALLOCATE_HEAP)
        if ptr_valid(self.cboffsets) eq 0 then self.cboffsets = ptr_new(/ALLOCATE_HEAP)
        if ptr_valid(self.cbwidths) eq 0 then self.cbwidths = ptr_new(/ALLOCATE_HEAP)
        return
    endif
        
    ;Add the object
    if ptr_valid(self.colorbars) $
        then *self.colorbars = [*self.colorbars, cbObjects] $
        else self.colorbars = ptr_new(cbObjects)
    
    ;Add the location
    if ptr_valid(self.cblocations) $
        then *self.cblocations = [*self.cblocations, locations] $
        else self.cblocations = ptr_new(locations)
    
    ;Add the offsets
    if ptr_valid(self.cboffsets) $
        then *self.cboffsets = [*self.cboffsets, offsets] $
        else self.cboffsets = ptr_new(offsets)
    
    ;Add the location
    if ptr_valid(self.cbreflocs) $
        then *self.cbreflocs = [*self.cbreflocs, refLocations] $
        else self.cbreflocs = ptr_new(refLocations)
    
    ;Add the widths
    if ptr_valid(self.cbwidths) $
        then *self.cbwidths = [*self.cbwidths, widths] $
        else self.cbwidths = ptr_new(widths)

;---------------------------------------------------------------------
;Set the Position ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Set the positions using REFPOSITIONS
    for i = 0, nrl - 1 do begin

        ;If an "empty" position was given, skip it
        if array_equal(refLocations[*,i], [0,0]) then continue
                
        ;set the position
        cbPosition = self -> calcColorBarPosition(refLocations[*,i], OFFSET=offsets, $
                                                  CBLOCATION=locations, WIDTH=widths)

        cbObjects[i] -> SetProperty, POSITION=cbPosition
    endfor
    
    ;Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   Add, remove, replace, or clear image objects from the list. Image objects can of any
;   class so long as the class has SetProperty, GetProperty, and Draw methods as well as
;   a POSITION property. If an image's position is undefined, then the image will be
;   placed into the auto-updating 2D plot grid.
;
; :Params:
;       IMAGEOBJECTS:           in, optional, type=object/obj_arr
;                               The plot object(s) to add to the display.
;
;   :Keywords:
;       ADJUST_LAYOUT:          in, optional, private, type=Boolean, default=0
;                               Sometimes holes are created in the plot layout when a
;                                   plots are added or removed. Set this keyword to remove
;                                   holes in the plot layout.
;       CLEAR:                  in, optional, type=boolean, default=0
;                               If set, clear all legend objects. New ones can be added
;                                   in same call.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the legends to the list.
;       LIST_INDEX              in, optional, type=boolean, default=0
;                               If set, then `LOCATION` is actually the index within at
;                                   which the plot is stored.
;       LOCATION:               in, out, optional, type=intarr(2\,N)
;                               The [col, row] location at which `THEOBJECTS` will be
;                                   placed into the 2D plotting grid. If none is provided,
;                                   LOCATION will be chosen and returned.
;       PLOT_INDEX:             in, optional, type=int, default=0
;                               If set, then `LOCATION` is actually the 1D plot index of
;                                   the plot. The upper-left-most plot has a plot index of
;                                   1, and the plot index increases as you go down the
;                                   column, then across the row.
;       POSITION:               in, out, optional, type=intarr(4\,N)
;                               The position of the lower-right and upper-left corners
;                                   of each `THEOBJECTS`. If none is provide, a POSITION
;                                   will be chosen and returned.
;       REMOVE:                 in, optional, type=boolean, default=0
;                               If set, remove the legend object and `INDEX` from the list.
;       REPLACE:                in, optional, type=boolean, default=0
;                               If set, replace the object at `INDEX` instead of appending
;                                   to the end of the list.
;-
pro MrPlotManager::AddImages, imageObjects, $
ADJUST_LAYOUT = adjust_layout, $
CLEAR = clear, $
DRAW = draw, $
DESTROY = destroy, $
LIST_INDEX = list_index, $
LOCATION = location, $
PLOT_INDEX = plot_index, $
POSITION = position, $
REMOVE = remove, $
REPLACE = replace
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
            
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Defaults
    SetDefaultValue, destroy, 1, /BOOLEAN
    SetDefaultValue, location, [1,1]
    clear = keyword_set(clear)
    draw = keyword_set(draw)
    list_index = keyword_set(list_index)
    plot_index = keyword_set(plot_index)
    remove = keyword_set(remove)
    replace = keyword_set(replace)
        
    nImages = n_elements(imageObjects)
        
;---------------------------------------------------------------------
;Replace Plot Objects ////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Destroy and replace old object, then return
    if keyword_set(remove) then begin
        self -> ReplaceImages, location, $
                               DESTROY=destroy, DRAW=draw, $
                               LIST_INDEX=list_index, PLOT_INDEX=plot_index
        return
    endif
        
;---------------------------------------------------------------------
;Replace Plot Objects ////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Destroy and replace old object, then return
    if keyword_set(replace) then begin
        self -> ReplacePlots, imageObjects, location, $
                              DESTROY=destroy, DRAW=draw, $
                              LIST_INDEX=list_index, PLOT_INDEX=plot_index
        return
    endif
        
;---------------------------------------------------------------------
;Clear all Plot Objects //////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Clear all plots?
    if keyword_set(clear) then self -> Clear, DESTROY=destroy, DRAW=draw, /PLOTS_ONLY
    
;---------------------------------------------------------------------
;Add to Master List //////////////////////////////////////////////////
;---------------------------------------------------------------------
            
    self -> Add, imageObjects, $
                 LOCATION = location, $
                 POSITION = position
    
;---------------------------------------------------------------------
;Add to Image List ///////////////////////////////////////////////////
;---------------------------------------------------------------------

    self -> MrAbstractImage::AddImages, imageObjects
    
    ;Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   Add, remove, replace, or clear plot objects from the list. Plot objects can of any
;   class so long as the class has SetProperty, GetProperty, and Draw methods as well as
;   a POSITION property. If a plot's position is undefined, then it will be placed into
;   the auto-updating 2D plot grid.
;
; :Params:
;       PLOTOBJECTS:            in, optional, type=object/obj_arr
;                               The plot object(s) to add to the display.
;
;   :Keywords:
;       ADJUST_LAYOUT:          in, optional, private, type=Boolean, default=0
;                               Sometimes holes are created in the plot layout when a
;                                   plots are added or removed. Set this keyword to remove
;                                   holes in the plot layout.
;       CLEAR:                  in, optional, type=boolean, default=0
;                               If set, clear all legend objects. New ones can be added
;                                   in same call.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the legends to the list.
;       LIST_INDEX              in, optional, type=boolean, default=0
;                               If set, then `LOCATION` is actually the index within at
;                                   which the plot is stored.
;       LOCATION:               in, out, optional, type=intarr(2\,N)
;                               The [col, row] location at which `THEOBJECTS` will be
;                                   placed into the 2D plotting grid. If none is provided,
;                                   LOCATION will be chosen and returned.
;       PLOT_INDEX:             in, optional, type=int, default=0
;                               If set, then `LOCATION` is actually the 1D plot index of
;                                   the plot. The upper-left-most plot has a plot index of
;                                   1, and the plot index increases as you go down the
;                                   column, then across the row.
;       POSITION:               in, out, optional, type=intarr(4\,N)
;                               The position of the lower-right and upper-left corners
;                                   of each `THEOBJECTS`. If none is provide, a POSITION
;                                   will be chosen and returned.
;       REMOVE:                 in, optional, type=boolean, default=0
;                               If set, remove the legend object and `INDEX` from the list.
;       REPLACE:                in, optional, type=boolean, default=0
;                               If set, replace the object at `INDEX` instead of appending
;                                   to the end of the list.
;-
pro MrPlotManager::AddPlots, plotObjects, $
ADJUST_LAYOUT = adjust_layout, $
CLEAR = clear, $
DRAW = draw, $
DESTROY = destroy, $
LIST_INDEX = list_index, $
LOCATION = location, $
PLOT_INDEX = plot_index, $
POSITION = position, $
REMOVE = remove, $
REPLACE = replace
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
            
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Defaults
    SetDefaultValue, destroy, 1, /BOOLEAN
    clear = keyword_set(clear)
    draw = keyword_set(draw)
    list_index = keyword_set(list_index)
    plot_index = keyword_set(plot_index)
    remove = keyword_set(remove)
    replace = keyword_set(replace)
        
    nPlots = n_elements(plotObjects)
        
;---------------------------------------------------------------------
;Replace Plot Objects ////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Destroy and replace old object, then return
    if keyword_set(remove) then begin
        self -> ReplacePlots, location, $
                              DESTROY=destroy, DRAW=draw, $
                              LIST_INDEX=list_index, PLOT_INDEX=plot_index
        return
    endif
        
;---------------------------------------------------------------------
;Replace Plot Objects ////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Destroy and replace old object, then return
    if keyword_set(replace) then begin
        self -> ReplacePlots, plotObjects, location, $
                              DESTROY=destroy, DRAW=draw, $
                              LIST_INDEX=list_index, PLOT_INDEX=plot_index
        return
    endif
        
;---------------------------------------------------------------------
;Clear all Plot Objects //////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Clear all plots?
    if keyword_set(clear) then self -> Clear, DESTROY=destroy, DRAW=draw, /PLOTS_ONLY
    
;---------------------------------------------------------------------
;Add to Master List //////////////////////////////////////////////////
;---------------------------------------------------------------------
            
    self -> Add, plotObjects, $
                 LOCATION = location, $
                 POSITION = position

;---------------------------------------------------------------------
;Add to MrAbstractPlot List //////////////////////////////////////////
;---------------------------------------------------------------------

    self -> MrAbstractPlot::AddPlots, plotObjects
    
    ;Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   The purpose of this method is to calculate a position of the colorbar based on the
;   plot location of the plot by which it is to be placed.
;
; :Params:
;       LOCATION:               in, required, type=fltarr(4)
;                               The [col, row] location of the plot by which to place
;                                   the colorbar.
;
; :Keywords:
;       CBLOCATION:             in, optional, type=string, default='RIGHT'
;                               Where the colorbar is the be located with respect to
;                                   `POSITION`. Choices are: 'RIGHT', 'LEFT', 'TOP', and
;                                   'BOTTOM'.
;       OFFSET:                 in, optional, type=int, default=3
;                               The offset, in character units, from `POSITION` of the
;                                   colorbar
;       WIDTH:                  in, optional, type=int, default=5
;                               The width of the colorbar, in character units.
;-
function MrPlotManager::calcColorBarPosition, location, $
CBLOCATION = cblocation, $
LIST_INDEX=list_index, $
PLOT_INDEX=plot_index, $
OFFSET = offset, $
WIDTH = width
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, !Null
    endif
    
    ;Does the plot exist?
    exists = self -> plotExists(location, index, $
                                PLOT_INDEX=plot_index, LIST_INDEX=list_index, $
                                /TO_LIST_INDEX)

    if exists eq 0 then return, !Null

;---------------------------------------------------------------------
;Set Positions ///////////////////////////////////////////////////////
;---------------------------------------------------------------------

    position = (*self.plot_positions)[*,index]
    cbPosition = self -> MrAbstractColorbar::calcColorBarPosition(position, $
                         LOCATION=cbLocation, OFFSET=offset, WIDTH=width)
    
    return, cbPosition
end


;+
;   The purpose of this method is to clear all objects from the list of objects being
;   displayed.
;
;   :Keywords:
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being cleared.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the legends to the list.
;       IMAGES_ONLY:            in, optional, type=boolean, default=0
;                               If set, only image objects will be cleared.
;       PLOTS_ONLY:             in, optional, type=boolean, default=0
;                               If set, only plot objects will be cleared.
;       
;-
pro MrPlotManager::Clear, $
DRAW = draw, $
DESTROY = destroy, $
IMAGES_ONLY = images_only, $
PLOTS_ONLY = plots_only
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
            
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Defaults
    SetDefaultValue, destroy, 1, /BOOLEAN
    draw = keyword_set(draw)
    images_only = keyword_set(images_only)
    plots_only = keyword_set(plots_only)
            
;---------------------------------------------------------------------
;Plots Only? /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    if keyword_set(plots_only) then begin
        ;Find which objects are being cleared from the master list.
        void = isMember(*self.plotObjects, *self.allObjects, iClear, NONMEMBER_INDS=iKeep)
        
        ;If all objects are plots, clear everything
        if n_elements(iKeep) eq 0 $
            then self -> Clear, DESTROY=destroy, DRAW=draw $
            else self -> RemovePlots, iClear, DESTROY=destroy, DRAW=draw
            
;---------------------------------------------------------------------
;Images Only? ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    endif else if keyword_set(images_only) then begin
        void = isMember(*self.imageObjects, *self.allObjects, iClear, NONMEMBER_INDS=iKeep)

        if n_elements(iKeep) eq 0 $
            then self -> Clear, DESTROY=destroy, DRAW=draw $
            else self -> RemoveImages, iClear, DESTROY=destroy, DRAW=draw
            
;---------------------------------------------------------------------
;Clear All ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
            
    ;---------------------------------------------------------------------
    ;Clear Superclasses //////////////////////////////////////////////////
    ;---------------------------------------------------------------------
    
        self -> MrAbstractImage::Clear, DESTROY=destroy
        self -> MrAbstractPlot::Clear, DESTROY=destroy
    
    ;---------------------------------------------------------------------
    ;Clear Master List ///////////////////////////////////////////////////
    ;---------------------------------------------------------------------
    
        ;If there are no objects to clear, then return
        if ptr_valid(self.allObjects) eq 0 then return
    
        ;Destroy the objects?
        if keyword_set(destroy) then obj_destroy, *self.allObjects
    
        ;Reset the pointer
        ptr_free, self.allObjects
        self.allObjects = ptr_new(/ALLOCATE_HEAP)
    
        ;Remove all locations and positions
        self -> SetPositions, /CLEAR
    
        ;Draw?
        if keyword_set(draw) then self -> Draw

    endelse
end


;+
;       Display an image.
;
;       If no location is specified, The plot will be placed at the maximum empty
;       plot location. If all plot locations are filled, then a new row will be added 
;       to the layout.
;
; :Params:
;
;   IMAGE:              in, required, type=2xN numeric array
;                       2D array to be displayed as an image
;       X:              in, optional, type=numeric array
;                       A vector representing the abscissa values to be plotted. 
;                           If X is not specified, Y is plotted as a function of point number 
;                           (starting at zero). If both arguments are provided, Y is plotted
;                           as a function of X.
;       Y:              in, required, type=numeric array
;                       The ordinate data to be plotted. Y should be a column vector. If
;                           it is a 1D row vector, the transpose will be taken. If it is
;                           2D, then each column of data will be plotted independently
;                       
; :Keywords:
;
;   DRAW:               in, optional, type=boolean, default=1
;                       If set, the image will be displayed immediately.
;   ERROR:              out, optional, type=int
;                       A named variable into which the error number will be returned, if
;                           an error is generated.
;   LOCATION:           in, optional, type=intarr(2), default=next available plot position
;                       Specify the location at which to put the plot
;   POSITION:           in, optional, type=fltarr(4)
;                       The lower-left and upper-right corners of a plot in normal
;                           coordinates.
;   _REF_EXTRA:         in, optional, type=structure
;                       a structure containing plotting keywords to be inherited
;                           by LINE_PLOTS, IMAGE_PLOTS, or CONTOUR_PLOTS, depending on
;                           the type of plot being made.
;-
pro MrPlotManager::Image, image, x, y, $
ERROR = the_error, $
DRAW = draw, $
LOCATION = location, $
POSITION = position, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Catch any errors.
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    setDefaultValue, draw, 1, /BOOLEAN
    
;---------------------------------------------------------------------
;Determine Plot Locations ////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If no position was given, find a spot for the plot. Do not adjust
    ;the layout yet because no plot has been added.
    if n_elements(position) eq 0 $
        then self -> GetPositions, location, POSITION=position $
        else self -> GetPositions, location, POSITION=position, /FIXED
    
;---------------------------------------------------------------------
;Create the Image Object /////////////////////////////////////////////
;---------------------------------------------------------------------

    theImage = obj_new('MrImageObject', image, x, y, DRAW=0, $
                        POSITION=position, _EXTRA=extra)

;---------------------------------------------------------------------
;Add the Object and Display It ///////////////////////////////////////
;---------------------------------------------------------------------
    self -> AddImages, theImage, LOCATION=location, POSITION=position
    
    ;Display the plot and increase the number of plots
    if draw then self -> Draw
end


;+
;       Add plotting data to the data and meta-data lists.
;
;       If no location is specified, The plot will be placed at the maximum empty
;       plot location. If all plot locations are filled, then a new row will be added 
;       to the layout.
;
; :Params:
;
;       X:              in, optional, type=numeric array
;                       A vector representing the abscissa values to be plotted. 
;                           If X is not specified, Y is plotted as a function of point number 
;                           (starting at zero). If both arguments are provided, Y is plotted
;                           as a function of X.
;       Y:              in, required, type=numeric array
;                       The ordinate data to be plotted. Y should be a column vector. If
;                           it is a 1D row vector, the transpose will be taken. If it is
;                           2D, then each column of data will be plotted independently
;                       
; :Keywords:
;
;   DRAW:               in, optional, type=boolean, default=1
;                       If set, the image will be displayed immediately.
;   ERROR:              out, optional, type=int
;                       A named variable into which the error number will be returned, if
;                           an error is generated.
;   LOCATION:           in, optional, type=intarr(2), default=next available plot position
;                       Specify the location at which to put the plot
;   POSITION:           in, optional, type=fltarr(4)
;                       The lower-left and upper-right corners of a plot in normal
;                           coordinates.
;                       Use this keyword to fix the position of a plot. The position
;                           will not change even if the layout changes. (It will,
;                           however, be resized if the window is resized.)
;   _REF_EXTRA:         in, optional, type=structure
;                       a structure containing plotting keywords to be inherited
;                           by LINE_PLOTS, IMAGE_PLOTS, or CONTOUR_PLOTS, depending on
;                           the type of plot being made.
;-
pro MrPlotManager::Plot, x, y, $
DRAW = draw, $
ERROR = the_error, $
LOCATION = location, $
POSITION = position, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Catch any errors.
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    setDefaultValue, draw, 1, /BOOLEAN
    
;---------------------------------------------------------------------
;Determine Plot Locations ////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If no position was given, find a spot for the plot. Do not adjust
    ;the layout yet because no plot has been added.
    if n_elements(position) eq 0 $
        then self -> GetPositions, location, POSITION=position $
        else self -> GetPositions, location, POSITION=position, /FIXED
    
;---------------------------------------------------------------------
;Create the Plot Objects /////////////////////////////////////////////
;---------------------------------------------------------------------

    thePlot = obj_new('MrPlotObject', x, y, DRAW=0, $
                       POSITION=position, _EXTRA=extra)

;---------------------------------------------------------------------
;Add the Object and Display It ///////////////////////////////////////
;---------------------------------------------------------------------
    self -> AddPlots, thePlot, LOCATION=location, POSITION=position
    
    ;Display the plot and increase the number of plots
    if draw then self -> Draw
end


;+
;   Remove a Plot or Image object from the list of objects being displayed.
;
; :Params:
;       LOCATION:               in, optional, type=intarr(2\,N), default=[1\,1]
;                               The [col, row] location of the plot to replace
;
;   :Keywords:
;       ADJUST_LAYOUT:          in, optional, private, type=Boolean, default=0
;                               Sometimes holes are created in the plot layout when a
;                                   plots are added or removed. Set this keyword to remove
;                                   holes in the plot layout.
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being replaced.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the legends to the list.
;       LIST_INDEX              in, optional, type=boolean, default=0
;                               If set, then `LOCATION` is actually the index within at
;                                   which the plot is stored.
;       PLOT_INDEX:             in, optional, type=int, default=0
;                               If set, then `LOCATION` is actually the 1D plot index of
;                                   the plot. The upper-left-most plot has a plot index of
;                                   1, and the plot index increases as you go down the
;                                   column, then across the row.
;-
pro MrPlotManager::Remove, location, $
ADJUST_LAYOUT = adjust_layout, $
DESTROY = destroy, $
DRAW = draw, $
LIST_INDEX = list_index, $
PLOT_INDEX = plot_index
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
    SetDefaultValue, list_index, 0, /BOOLEAN
    SetDefaultValue, location, [1,1]
    SetDefaultValue, plot_index, 0, /BOOLEAN
    
    ;Convert LOCATION to a list index value if not one already
    if n_elements(location) ne 0 $
        then exists = self -> plotExists(location, iList, PLOT_INDEX=plot_index, $
                                         LIST_INDEX=list_index, /TO_LIST_INDEX) $
        else exists = 0
        
;---------------------------------------------------------------------
;Determine Which to Keep /////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Make sure the plot exists
    if exists eq 0 then message, 'LOCATION does not exist. Cannot remove.'
    
    ;Find which plots are to be kept
    void = self -> plotExists(*self.plot_loc, all_list_index, /TO_LIST_INDEX)
    void = isMember(iList, all_list_index, NONMEMBER_INDS=ikeep)
        
;---------------------------------------------------------------------
;Remove From Superclasses First //////////////////////////////////////
;---------------------------------------------------------------------

    ;The objects to remove
    oRemove = (*self.allObjects)[iList]
    
    ;Plot Objects
    if ptr_valid(self.plotObjects) then begin
        ;Find the indices of objects to remove
        void = isMember(oRemove, *self.plotObjects, iRemove, N_MATCHES=nRemove)
        
        ;Remove them
        if nRemove ne 0 then self -> MrAbstractPlot::Remove, iRemove, DESTROY=0
    endif
    
    ;Image Objects -- same process
    if ptr_valid(self.imageObjects) then begin
        void = isMember(oRemove, *self.imageObjects, iRemove, N_MATCHES=nRemove)
        if nRemove ne 0 then self -> MrAbstractImage::Remove, iRemove, DESTROY=0
    endif
        
;---------------------------------------------------------------------
;Remove From Master List /////////////////////////////////////////////
;---------------------------------------------------------------------

    ;If no plots will remain after removing the desired plots...
    if n_elements(ikeep) eq 0 then begin

        ;Clear all positions and plot positions
        self -> Clear, DESTROY=destroy
        self -> SetPositions, /CLEAR
    
    ;If some plots will remain...
    endif else begin
    
        ;Destroy the objects being removed
        if keyword_set(destroy) then obj_destroy, (*self.allObjects)[iList]
        
        ;Remove the indicated objects
        *self.allObjects = (*self.allObjects)[ikeep]

        ;Remove their locations and positions
        self -> SetPositions, iList, /LIST_INDEX, /REMOVE, ADJUST_LAYOUT=adjust_layout
    endelse
        
;---------------------------------------------------------------------
;Draw ////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Draw?
    if keyword_set(draw) then self -> Draw
 
 end


;+
;   The purpose of this method is to provide a means of replacing image objects.
;
; :Params:
;       THEIMAGES:              in, optional, type=object/obj_arr
;                               The new image objects the will replace those found at
;                                   `LOCATION`.
;       LOCATION:               in, optional, type=int, default=[1\,1]
;                               The [col, row] location of the image to replace
;
;   :Keywords:
;       ADJUST_LAYOUT:          in, optional, private, type=Boolean, default=0
;                               Sometimes holes are created in the plot layout when a
;                                   plots are added or removed. Set this keyword to remove
;                                   holes in the plot layout.
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being replaced.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the legends to the list.
;       LIST_INDEX              in, optional, type=boolean, default=0
;                               If set, then `LOCATION` is actually the index within at
;                                   which the plot is stored.
;       PLOT_INDEX:             in, optional, type=int, default=0
;                               If set, then `LOCATION` is actually the 1D plot index of
;                                   the plot. The upper-left-most plot has a plot index of
;                                   1, and the plot index increases as you go down the
;                                   column, then across the row.
;-
pro MrPlotManager::ReplaceImages, theImages, location, $
ADJUST_LAYOUT = adjust_layout, $
DESTROY = destroy, $
DRAW = draw, $
LIST_INDEX = list_index, $
PLOT_INDEX = plot_index, $
POSITION = position
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Defaults
    SetDefaultValue, destroy, 1, /BOOLEAN
    SetDefaultValue, draw, 0, /BOOLEAN
    SetDefaultValue, list_index, 0, /BOOLEAN
    SetDefaultValue, location, [1,1]
    SetDefaultValue, plot_index, 0, /BOOLEAN
    
    ;Convert LOCATION to a list index value if not one already
    if n_elements(location) ne 0 $
        then exists = self -> plotExists(location, iList, PLOT_INDEX=plot_index, $
                                         LIST_INDEX=list_index, /TO_LIST_INDEX) $
        else exists = 0
    
    ;Make sure the plot exists
    if exists eq 0 then message, 'LOCATION does not exist. Cannot replace.'
        
;---------------------------------------------------------------------
;Replace Objects in Superclass Lists First ///////////////////////////
;---------------------------------------------------------------------

    ;Objects being replaced
    oReplace = (*self.allObjects)[iList]
    
    if ptr_valid(self.imageObjects) then begin
        ;Get the index values of the plots being replaced.
        void = isMember(oReplace, *self.imageObjects, iReplace, NMATCHES=nReplace)
        
        ;Replace those objects.
        if nReplace gt 0 then $
            self -> MrAbstractImage::ReplaceImages, theImages, iReplace, DESTROY=0
    endif
        
;---------------------------------------------------------------------
;Replace in Master List //////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Replace the objects in the master list
    if keyword_set(destroy) then obj_destroy, (*self.allObjects)[iList]
    (*self.allObjects)[iList] = theImages
    
    ;Update its position
    self -> SetPositions
        
;---------------------------------------------------------------------
;Draw? ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    if keyword_set(draw) then self -> Draw
end


;+
;   The purpose of this method is to provide a means of replace plot objects.
;
; :Params:
;       THEPLOTS:               in, optional, type=object/obj_arr
;                               The plot objects being replaced.
;       LOCATION:               in, optional, type=int, default=[1\,1]
;                               The [col, row] location of the plots to replace
;
;   :Keywords:
;       ADJUST_LAYOUT:          in, optional, private, type=Boolean, default=0
;                               Sometimes holes are created in the plot layout when a
;                                   plots are added or removed. Set this keyword to remove
;                                   holes in the plot layout.
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being replaced.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the legends to the list.
;       LIST_INDEX              in, optional, type=boolean, default=0
;                               If set, then `LOCATION` is actually the index within at
;                                   which the plot is stored.
;       PLOT_INDEX:             in, optional, type=int, default=0
;                               If set, then `LOCATION` is actually the 1D plot index of
;                                   the plot. The upper-left-most plot has a plot index of
;                                   1, and the plot index increases as you go down the
;                                   column, then across the row.
;-
pro MrPlotManager::ReplacePlots, thePlots, location, $
ADJUST_LAYOUT = adjust_layout, $
DESTROY = destroy, $
DRAW = draw, $
LIST_INDEX = list_index, $
PLOT_INDEX = plot_index, $
POSITION = position
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Defaults
    SetDefaultValue, destroy, 1, /BOOLEAN
    SetDefaultValue, draw, 0, /BOOLEAN
    SetDefaultValue, list_index, 0, /BOOLEAN
    SetDefaultValue, location, [1,1]
    SetDefaultValue, plot_index, 0, /BOOLEAN
    
    ;Convert LOCATION to a list index value if not one already
    if n_elements(location) ne 0 $
        then exists = self -> plotExists(location, iList, PLOT_INDEX=plot_index, $
                                         LIST_INDEX=list_index, /TO_LIST_INDEX) $
        else exists = 0
    
    ;Make sure the plot exists
    if exists eq 0 then message, 'LOCATION does not exist. Cannot replace.'
        
;---------------------------------------------------------------------
;Replace Objects in Superclass Lists First ///////////////////////////
;---------------------------------------------------------------------

    ;Objects being replaced
    oReplace = (*self.allObjects)[iList]
    
    ;Line Plots?
    if ptr_valid(self.plotObjects) then begin
        ;Get the index values of the plots being replaced.
        void = isMember(oReplace, *self.plotObjects, iReplace, NMATCHES=nReplace)
        
        ;Replace those objects.
        if nReplace gt 0 then $
            self -> MrAbstractPlot::ReplacePlots, thePlots, iReplace, DESTROY=0
    endif
        
;---------------------------------------------------------------------
;Replace in Master List //////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Replace the objects in the master list
    if keyword_set(destroy) then obj_destroy, (*self.allObjects)[iList]
    (*self.allObjects)[iList] = thePlots
    
    ;Update its position
    self -> SetPositions
        
;---------------------------------------------------------------------
;Draw? ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    if keyword_set(draw) then self -> Draw
end


;+
;   The purpose of this method is to provide a means of changing the overall layout of
;   the 2D plotting grid. In addition, individual plot locations and positions can be
;   altered or added by using the the following keywords::
;
;       SETLOCATION -   Set the location of an existing plot
;                           Fixed         -> Auto-Updating
;                           Auto-Updating -> Fixed
;                           Auto-Updating -> Auto-Updating
;
;       SETPOSITION -   Set the position of an existing plot
;                           Auto-Updating -> Fixed
;                           Fixed         -> Fixed
;
;       ADD         -   Add a new location and position to the lists.
;                           Auto-Updating
;                           Fixed
;
; :Params:
;       LOCATION:           in, optional, type=long
;                           The [col, row] location of the plot. Used with `ADD` and
;                               `SETLOCATION`.
;
; :Keywords:
;       ADD:                in, optional, type=Boolean, default=0
;                           If set, `LOCATION` and `POSITION` are appended to their
;                               respective lists. If `LOCATION` is not set, then a new
;                               one will be created. This new `LOCATION` will be "fixed"
;                               if `POSITION` is not present, and "auto-updating" if it is.
;                               New locations and positions will be returned via their
;                               respective parameter and keyword.
;       ADJUST_LAYOUT:      in, optional, private, type=Boolean, default=0
;                           Sometimes holes are created in the plot layout when an auto-
;                               updating plot's location changes. Set this keyword to
;                               remove holes in the plot layout.
;       LAYOUT:             in, out, optional, type=intarr(2)
;                           A two-element vector [col, row] indicating the number of
;                               columns and rows in the plot layout. If set, the layout
;                               and positions will be updated. Furthermore, if the layout
;                               changes as a result of adding, moving, or fixing plots
;                               from the auto-updating grid, then the new layout will be
;                               returned.
;       LIST_INDEX:         in, optional, type=Boolean, default=0
;                           Indicate that `LOCATION` is the index location within the
;                               internal data lists of the plot whose position is to
;                               be altered. To obtain the list index, use the
;                               "plotsPresent" method.
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
;       _REF_EXTRA:         in, optional, type=Structure
;                           Any keyword accepted by MrPlotLayout.pro
;
; :Uses:
;   Uses the following external programs::
;       MrPlotLayout.pro
;-
pro MrPlotManager::SetPositions, location, $
ADD = add, $
ADJUST_LAYOUT = adjust_layout, $
CLEAR = clear, $
LAYOUT = layout, $
LIST_INDEX = list_index, $
POSITION = position, $
REMOVE = remove, $
SETLOCATION = setLocation, $
SETPOSITION = setPosition, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Call the superclass
    self -> MrPlotLayout::SetPositions, location, $
                                        ADD = add, $
                                        ADJUST_LAYOUT = adjust_layout, $
                                        CLEAR = clear, $
                                        LAYOUT = layout, $
                                        LIST_INDEX = list_index, $
                                        POSITION = position, $
                                        REMOVE = remove, $
                                        SETLOCATION = setLocation, $
                                        SETPOSITION = setPosition, $
                                        _EXTRA = extra
     
;---------------------------------------------------------------------
;Reposition Plots ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;
    ; From the Plot method, the following actions have been completed:
    ;   1) Add the plot to *self.plotObjects
    ;   2) Add the new location to *self.plot_loc
    ;   3) Add the new position to *self.plot_positions
    ;   4) Adjusted the layout
    ;
    ; Now we need to do the following:
    ;   5) Reposition each plot.
    ;   6) Reposition the colorbars
    ;
        
    ;If there are no plots, then there is nothing to reposition.
    nplots = n_elements(*self.plot_positions)/4
    if nplots eq 0 then return

    ;Update the position of each plot.
    for i = 0, nplots - 1 do $
        (*self.allObjects)[i] -> SetProperty, POSITION=(*self.plot_positions)[*,i]
    
    ;Update colorbar positions
    nCB = n_elements(*self.colorbars)
    for i = 0, nCB - 1 do begin
        position = self -> calcColorBarPosition((*self.cbreflocs)[*,i], $
                                                CBLOCATION=(*self.cblocations)[*,i], $
                                                OFFSET=(*self.cboffsets)[i], $
                                                WIDTH=(*self.cbwidths)[i])
        
        (*self.colorbars)[i] -> SetProperty, POSITION=position
    endfor
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
    
    ;Plot?
    if n_elements(*self.plotObjects) gt 0 && max(*self.plotObjects eq objRef) eq 1 then begin
        ImA = 'PLOT'
    
    ;Image?
    endif else if n_elements(*self.imageObjects) gt 0 && max(*self.imageObjects eq objRef) eq 1 then begin
        ImA = 'IMAGE'
    
    endif else if n_elements(*self.colorbars) gt 0 && max(*self.colorbars eq objRef) eq 1 then begin
        ImA = 'COLORBAR'
        
    ;Unknown?
    endif else begin
        ImA = ''
        
    endelse
    
    return, ImA

end


;+
;   Clean up after the object is destroy
;-
pro MrPlotManager::cleanup
    compile_opt idl2
    
    ;Destroy all weLegendItem objects
    self -> MrAbstractPlot::Cleanup
    self -> MrAbstractImage::Cleanup
    self -> MrPlotLayout::Cleanup
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
    
    ;Validate the pointer
    self.allObjects = ptr_new(/ALLOCATE_HEAP)
    
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
               inherits MrAbstractColorbar, $   ;Manage colorbar objects
               inherits MrAbstractPlot, $       ;Manage plot objects
               inherits MrAbstractImage, $      ;Manage image objects
               inherits MrPlotLayout, $         ;Manage plot layout
               allObjects: ptr_new() $          ;All positionable objects
             }
end