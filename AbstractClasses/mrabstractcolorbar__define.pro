; docformat = 'rst'
;
; NAME:
;       MrAbstractColorBar__Define
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
;   The purpose of this method is to serve as an abstract class for making, adding, 
;   removing, and drawing cgLegendItem objects. It is meant to be inherited and will
;   not on its own.
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
;       04/25/2013  -   Written by Matthew Argall, extracted from MrLinePlot__define
;       06/17/2013  -   
;-
;*****************************************************************************************
;+
;   Add an colorbar object.
;
; :Params:
;       CBOBJECTS:              in, optional, type=object/obj_arr
;                               The weLegendItem or colorbar object(s) to add.
;       REFPOSITIONS:           in, optional, type=fltarr(4\,N), default=fltarr(4\,N)
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
pro MrAbstractColorBar::AddColorBars, cbObjects, refPositions, $
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
    nRP = n_elements(refPositions)
    if nRP gt 0 then nRP = n_elements(refPositions[0,*])

    if nRP ne nCB && nRP ne 0 then $
        message, 'The # of REFPOSITIONS must match the # of CBOBJECTS.'
    
    SetDefaultValue, destroy, 1, /BOOLEAN
    clear = keyword_set(clear)
    draw = keyword_set(draw)
    remove = keyword_set(remove)
    replace = keyword_set(replace)
    
    if nrp ne 0 then begin
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
    
    ;Set the positions using REFPOSITIONS
    for i = 0, nrp - 1 do begin
        ;If an "empty" position was given, skip it
        if array_equal(refPositions[*,i], [0,0,0,0]) then continue
        
        ;set the position
        cbPosition = self -> calcColorBarPositions(refPositions[*,i], OFFSET=offsets, $
                                                   LOCATION=locations, WIDTH=widths)
        cbObjects[i] -> SetProperty, POSITION=cbPosition
    endfor
        
    
    ;Add the object
    if ptr_valid(self.colorbars) $
        then *self.colorbars = [*self.colorbars, cbObject] $
        else self.colorbars = ptr_new(cbObjects)
    
    ;Add the location
    if ptr_valid(self.cblocations) $
        then *self.cblocations = [*self.cblocations, refPositions] $
        else self.cblocations = ptr_new(refPositions)
    
    ;Add the offsets
    if ptr_valid(self.cboffsets) $
        then *self.cboffsets = [*self.cboffsets, cboffsets] $
        else self.cboffsets = ptr_new(cboffsets)
    
    ;Add the reference locations
    if ptr_valid(self.cbreflocs) $
        then *self.cbreflocs = [*self.cbreflocs, refPositions] $
        else self.cbreflocs = ptr_new(refPositions)
    
    ;Add the widths
    if ptr_valid(self.cbwidths) $
        then *self.cbwidths = [*self.cbwidths, cbwidths] $
        else self.cbwidths = ptr_new(cbwidths)
    
    ;Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   The purpose of this method is to calculate a position of the colorbar based on the
;   position of the plot by which it is to be placed.
;
; :Params:
;       POSITION:               in, required, type=fltarr(4)
;                               The position, in normal coordinates, of the plot next to
;                                   which the colorbar is to be placed.
;
; :Keywords:
;       LOCATION:               in, optional, type=string, default='RIGHT'
;                               Where the colorbar is the be located with respect to
;                                   `POSITION`. Choices are: 'RIGHT', 'LEFT', 'TOP', and
;                                   'BOTTOM'.
;       OFFSET:                 in, optional, type=int, default=3
;                               The offset, in character units, from `POSITION` of the
;                                   colorbar
;       WIDTH:                  in, optional, type=int, default=5
;                               The width of the colorbar, in character units.
;-
function MrAbstractColorBar::calcColorBarPosition, position, $
LOCATION = location, $
OFFSET = offset, $
WIDTH = width
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, fltarr(4)
    endif

    ;x- and y-character sizes in normal coordinates
    xchsize = double(!d.x_ch_size) / double(!d.x_size)
    ychsize = double(!d.y_ch_size) / double(!d.y_size)
    
    ;Defaults
    SetDefaultValue, location, 'RIGHT'
    SetDefaultValue, offset, 3
    SetDefaultValue, width, 5

    ;Calculate the position of the colorbar
    case strupcase(location) of
        'RIGHT': cbPosition = [position[2] + offset*xchsize, $
                               position[1], $
                               position[2] + offset*xchsize + width*xchsize, $
                               position[3]]
        
        'LEFT': cbPosition = [position[0] - offset*xchsize - width*xchsize, $
                              position[1], $
                              position[0] - offset*xchsize, $
                              position[3]]
        
        'TOP': cbPosition = [position[2], $
                              position[1] + offset*ychsize, $
                              position[2], $
                              position[3] + offset*ychsize + width*ychsize]
        
        'BOTTOM': cbPosition = [position[0], $
                                position[1] - offset*ychsize - width*ychsize, $
                                position[0], $
                                position[3] - offset*ychsize]
                                
        else: message, 'Colorbar location "' + location + '" not valid.'
    endcase

    return, cbPosition
end


;+
;   The purpose of this method is to draw the legends in the draw window.
;-
pro MrAbstractColorBar::Draw
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Call each of the Legend's draw methods. Since these are over plots, the
    ;device should already be configured by a previous call to Plot.
    for i = 0, n_elements(*self.colorbars) - 1 do (*self.colorbars)[i] -> Draw
end


;+
;   Clear all of the colorbar objects from the list.
;
; :Keywords:
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being cleared.
;-
pro MrAbstractColorBar::Clear, $
DESTROY = destroy
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

    ;Destroy all plot objects
    if ptr_valid(self.colorbars) && keyword_set(destroy) $
        then obj_destroy, *self.colorbars
    
    ;Free the pointers.
    ptr_free, self.colorbars
    ptr_free, self.cblocations
    ptr_free, self.cboffsets
    ptr_free, self.cbreflocs
    ptr_free, self.cbwidths
    
    ;Reset pointers.
    self.colorbars = ptr_free(/ALLOCATE_HEAP)
    self.cblocations = ptr_free(/ALLOCATE_HEAP)
    self.cboffsets = ptr_free(/ALLOCATE_HEAP)
    self.cbreflocs = ptr_free(/ALLOCATE_HEAP)
    self.cbwidths = ptr_free(/ALLOCATE_HEAP)
end



;+
;   Create a weColorBar object to be placed on the graph.
;
; :Params:
;       REFPOSITION:        in, optional, type=fltarr(4\,N), default=fltarr(4\,N)
;                           The positions of plots by which the colorbars are to be
;                               placed.
;
; :Keywords:
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the legend to the list.
;       LOCATION:           in, optional, type=string, default='RIGHT'
;                           The location of the color bar with respect to the graph::
;                               'RIGHT' -   To the right of the graph.
;                               'LEFT'  -   To the left of the graph.
;                               'TOP'   -   Above the graph.
;                               'BOTTOM'-   Below the graph.
;       WIDTH:              in, optional, type=integer, default=5
;                           The width of the color bar in character units. Used with
;                               `LOCATION`.
;       OFFSET:             in, optional, type=integer, default=3
;                           The offset from the graph's axes to the color bar's axis. Used
;                               with `LOCATION`.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by cgLegendItem.
;   
;-
pro MrAbstractColorBar::ColorBar, refPosition, $
LOCATION = location, $
OFFSET = offset, $
WIDTH = width, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Defaults
    draw = keyword_set(draw)
    SetDefaultValue, location, ''
    SetDefaultValue, offset, 3
    SetDefaultValue, width, 5

    ;Default to putting the color bar to the right of the plot.
    if n_elements(position) eq 0 then location = 'RIGHT'
    case strupcase(location) of
        "": ;Default
        'RIGHT': vertical = 1
        'LEFT': vertical = 1
        'TOP': vertical = 0
        'BOTTOM': vertical = 0
        else: message, 'LOCATION is not a valid location.'
    endcase

    ;Create the color bar
    theColorBar = obj_new('weColorBar', VERTICAL=vertical, _STRICT_EXTRA=extra)

    ;Add the color bar
    self -> addColorBars, theColorBar, refPosition, $
                          LOCATIONS=location, OFFSETS=offset, WIDTHS=width
    
    ;Draw    
    if keyword_set(draw) then self -> Draw
end


;+
;   The purpose of this method is to draw the legends in the draw window.
;-
pro MrAbstractColorBar::Draw
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Call each of the Legend's draw methods. Since these are over plots, the
    ;device should already be configured by a previous call to Plot.
    for i = 0, n_elements(*self.colorbars) - 1 do (*self.colorbars)[i] -> Draw
end



;+
;   Remove a colorbar objects from the list of colorbars.
;
; :Params:
;       INDEX:                  in, optional, type=object/obj_arr
;                               The index of the plot object to be removed.
;
;   :Keywords:
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being replaced.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after removing the plot object.
;-
pro MrAbstractColorbar::Remove, index, $
DESTROY = destroy, $
DRAW = draw
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Defaults.
    SetDefaultValue, destroy, 1, /BOOLEAN
    SetDefaultValue, draw, 0, /BOOLEAN
    SetDefaultValue, index, 0

    ;Total number of plots
    nCB = n_elements(*self.colorbars)

;---------------------------------------------------------------------
;Destroy /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(destroy) then begin
        ;Destroy them
        obj_destroy, (*self.colorbars)[index]
        
        ;Find the indices of the valid objects
        iValid = where(obj_valid(*self.colorbars) eq 1, nValid)
        
        ;Keep only the valid objects
        if nValid ne 0 then begin
            *self.colorbars = (*self.colorbars)[iValid]
            *self.cblocations = (*self.cblocations)[iValid]
            *self.cboffsets = (*self.cboffsets)[iValid]
            *self.cbreflocs = (*self.cbreflocs)[iValid]
            *self.cbwidths = (*self.cbwidths)[iValid]
        
        ;If no valid objects exist, reset the pointer
        endif else begin
            self -> Clear
        endelse

;---------------------------------------------------------------------
;Do Not Destroy //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        ;Find which indices to keep
        iAll = indgen(nCB)
        void = isMember(index, iAll, NONMEMBER_INDS=iKeep)
        
        ;Get rid of the unwanted objects
        if iKeep ne 0 then begin
            *self.colorbars = (*self.colorbars)[iKeep]
            *self.cblocations = (*self.cblocations)[iKeep]
            *self.cboffsets = (*self.cboffsets)[iKeep]
            *self.cbwidths = (*self.cbwidths)[iKeep]
        endif else begin
            self -> Clear, DESTROY=0
        endelse
    endelse

    ;Draw?
    if keyword_set(draw) then self -> Draw
end


;+
;   Add, remove, replace, or clear colorbar objects from the list.
;
; :Params:
;       CBOBJECTS:              in, optional, type=object/obj_arr
;                               The colorbar object(s) to add to the display.
;       INDEX:                  in, optional, type=int/intarr, default=0
;                               The index of the colorbar object that `CBOBJECTS` is
;                                   going to replace.
;
;   :Keywords:
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being replaced.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the legends to the list.
;-
pro MrAbstractColorBar::ReplaceColorbars, cbObjects, index, $
DESTROY = destroy, $
DRAW = draw
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
    SetDefaultValue, index, 0
    
    ;Destroy the plot object that is being replaced
    if keyword_set(destroy) then obj_destroy, (*self.colorbars)[index]
    
    ;Insert the new plot objects in those locations
    (*self.colorbars)[index] = cbObjects
    
    ;Draw?
    if keyword_set(draw) then self -> Draw
end


;+
;   This method provides a means of determining the index location at which each
;   ColorBar is stored. Colorbars are distinguished by their titles.
;-
PRO MrAbstractColorBar::whichColorBars
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ;Allow the Superclass to have a space flush with the left of the display window
    Print, '  ColorBars:'
    
    ;Print the color bar indices and titles
    FOR j = 0, N_Elements(*self.colorbars) - 1 DO BEGIN
        (*self.colorbars)[j] -> GetProperty, TITLE=title
        index = String(j, FORMAT='(i3.3)')
        
        IF N_Elements(title) EQ 0 THEN title = ''
        Print, '    Index: ' + index + '    Title: ' + title
    ENDFOR
END


;+
;   Clean up after the object is destroy
;-
pro MrAbstractColorBar::cleanup
    compile_opt idl2
    
    ;Free pointers
    ptr_free, self.cblocations
    ptr_free, self.cboffsets
    ptr_free, self.cbreflocs
    ptr_free, self.cbwidths
    
    ;Destroy all weLegendItem objects
    if ptr_valid(self.colorbars) then begin
        for i = 0, n_elements(*self.colorbars)-1 do begin
            obj_destroy, (*self.colorbars)[i]
        endfor
        ptr_free, self.colorbars
    endif
end


;+
;   The initialization method. Because MrAbstractColorBar is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractColorBar object will result
;   in an error.
;-
function MrAbstractColorBar::init
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif
    
    message, 'This is an abstract class and must be inherited.'
end


;+
;   The class definition statement.
;-
pro MrAbstractColorBar__define
    compile_opt idl2
    
    class = {MrAbstractColorBar, $
             colorbars: ptr_new(), $            ;Colorbar objects
             cblocations: ptr_new(), $          ;Location of colorbar w/respect to cbreflocs
             cboffsets: ptr_new(), $            ;Offset of colorbar from adjacent plot
             cbreflocs: ptr_new(), $            ;Reference position/location
             cbwidths: ptr_new() $              ;Width of colorbar
            }
    
end