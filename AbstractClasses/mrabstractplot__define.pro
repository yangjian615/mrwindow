; docformat = 'rst'
;
; NAME:
;       MrAbstractPlot__Define
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
;   removing, and drawing plot objects. It is meant to be inherited and will
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
;       05/18/2013  -   Written by Matthew Argall.
;       05/30/2013  -   Renamed the AddPlots method to Add. Separated the Remove, Clear,
;                           and Replace options into their own methods. Left keywords
;                           in the Add method to keep it general. Added the DESTROY
;                           keyword to all of the above. - MRA
;       06/17/2013  -   Added NOEARSE keyword to the Draw procedure. - MRA
;-
;*****************************************************************************************
;+
;   Add, remove, replace, or clear plot objects from the list.
;
; :Params:
;       PLOT_OBJECTS:           in, optional, type=object/obj_arr
;                               The plot object(s) to add to the display.
;
;   :Keywords:
;       CLEAR:                  in, optional, type=boolean, default=0
;                               If set, clear all plot objects. New ones can be added
;                                   in same call.
;       DESTROY:                in, optional, type=boolean, default=1
;                               If set, then when `CLEAR`, `REMOVE`, or `REPLACE` are set
;                                   the objects being cleared, removed, or replaced will
;                                   be destroyed.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the legends to the list.
;       INDEX:                  in, optional, type=int, default=0
;                               The index at which to `REPLACE` or `REMOVE` plot objects.
;       REMOVE:                 in, optional, type=boolean, default=0
;                               If set, remove the plot object and `INDEX` from the list.
;       REPLACE:                in, optional, type=boolean, default=0
;                               If set, replace the object at `INDEX` instead of appending
;                                   to the end of the list.
;-
pro MrAbstractPlot::AddPlots, plot_objects, $
CLEAR = clear, $
DESTROY = destroy, $
DRAW = draw, $
INDEX = index, $
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
    
    ;Defaults
    SetDefaultValue, draw, 0, BOOLEAN=boolean
    SetDefaultValue, destroy, 1, /BOOLEAN
    
    ;Replace.
    if keyword_set(replace) then begin
        self -> ReplacePlots, plot_objects, index, DESTROY=destroy, DRAW=draw
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
    if n_elements(plot_objects) eq 0 then begin
        if ptr_valid(self.plotObjects) eq 0 then self.plotObjects = ptr_new(/ALLOCATE_HEAP)
        return
    endif
    
    ;Add the object.
    if ptr_valid(self.plotObjects) $
        then *self.plotObjects = [*self.plotObjects, plot_objects] $
        else self.plotObjects = ptr_new(plot_objects)
    
    ;Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   The purpose of this method is to draw the plots in the draw window.
;
; :Keywords:
;       NOERASE:                in, optional, type=boolean, default=0
;                               If set, do not erase the window before drawing the first
;                                   plot.
;-
pro MrAbstractPlot::Draw, $
NOERASE = noerase
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Defaults
    noerase = keyword_set(noerase)

    ;Call each of the plots's draw methods.
    for i = 0, n_elements(*self.plotObjects) - 1 do begin
        if i gt 0 then noerase = 1
        
        thePlot = (*self.plotObjects)[i]
        thePlot -> SetProperty, NOERASE=noerase
        thePlot -> Draw
    endfor
end


;+
;   Clear all of the plot objects from the list.
;
; :Keywords:
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being cleared.
;-
pro MrAbstractPlot::Clear, $
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
    if ptr_valid(self.plotObjects) && keyword_set(destroy) $
        then obj_destroy, *self.plotObjects
    
    ;Free the plotObjects pointer and create a new one.
    ptr_free, self.plotObjects
    self.plotObjects = ptr_free(/ALLOCATE_HEAP)
end


;+
;   Create a MrPlotObject object to be drawn in the device window.
;
; :Keywords:
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the plot to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrPlotObject__define.
;   
;-
pro MrAbstractPlot::Plot, $
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

    ;Create the plot
    thePlot = obj_new('MrPlotObject', _STRICT_EXTRA=extra)
    
    ;Add the plot
    self -> addPlots, thePlot
    
    ;Draw    
    if keyword_set(draw) then self -> Draw
end


;+
;   Remove a plot objects from the list of plots.
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
pro MrAbstractPlot::Remove, index, $
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
    nPlots = n_elements(*self.plotObjects)

;---------------------------------------------------------------------
;Destroy /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if keyword_set(destroy) then begin
        ;Destroy them
        obj_destroy, (*self.plotObjects)[index]
        
        ;Find the indices of the valid objects
        iValid = where(obj_valid(*self.plotObjects) eq 1, nValid)
        
        ;Keep only the valid objects
        if nValid ne 0 $
            then *self.plotObjects = *self.plotObjects[iValid] $
        
        ;If no valid objects exist, reset the pointer
        else begin
            ptr_free, self.plotObjects
            self.plotObjects = ptr_new(/ALLOCATE_HEAP)
        endelse

;---------------------------------------------------------------------
;Do Not Destroy //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        ;Find which indices to keep
        iAll = indgen(nPlots)
        void = isMember(index, iAll, NONMEMBER_INDS=iKeep)
        
        ;Get rid of the unwanted objects
        *self.plotObjects = (*self.plotObjects)[iKeep]
    endelse

    ;Draw?
    if keyword_set(draw) then self -> Draw
end


;+
;   Add, remove, replace, or clear plot objects from the list.
;
; :Params:
;       PLOT_OBJECTS:           in, optional, type=object/obj_arr
;                               The plot object(s) to add to the display.
;       INDEX:                  in, optional, type=int/intarr, default=0
;                               The index of the plot object that `PLOT_OBJECTS` is going
;                                   to replace.
;
;   :Keywords:
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being replaced.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the legends to the list.
;-
pro MrAbstractPlot::ReplacePlots, plot_objects, index, $
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
    if keyword_set(destroy) then obj_destroy, (*self.plotObjects)[index]
    
    ;Insert the new plot objects in those locations
    (*self.plotObjects)[index] = plot_objects
    
    ;Draw?
    if keyword_set(draw) then self -> Draw
end


;+
;   This method provides a means of determining the index location at which each
;   Plot is stored. Plots are distinguished by their titles.
;-
PRO MrAbstractPlot::whichPlots
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ;Allow the Superclass to have a space flush with the left of the display window
    Print, '  Plots:'
    
    ;Print the color bar indices and titles
    FOR j = 0, N_Elements(*self.plotObjects) - 1 DO BEGIN
        (*self.plotObjects)[j] -> GetProperty, TITLE=title
        index = String(j, FORMAT='(i3.3)')
        
        IF N_Elements(title) EQ 0 THEN title = ''
        Print, '    Index: ' + index + '    Title: ' + title
    ENDFOR
END


;+
;   Clean up after the object is destroy
;-
pro MrAbstractPlot::cleanup
    compile_opt idl2
    
    ;Destroy all weLegendItem objects
    if ptr_valid(self.plotObjects) then begin
        for i = 0, n_elements(*self.plotObjects)-1 do begin
            obj_destroy, (*self.plotObjects)[i]
        endfor
        ptr_free, self.plotObjects
    endif
end


;+
;   The initialization method. Because MrAbstractPlot is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractPlot object will result
;   in an error.
;-
function MrAbstractPlot::init
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
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrAbstractPlot__define, class
    compile_opt idl2
    
    class = {MrAbstractPlot, $
             plotObjects: ptr_new()}
end