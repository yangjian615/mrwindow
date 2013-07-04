; docformat = 'rst'
;
; NAME:
;       MrAbstractOverPlot__Define
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
;   removing, and drawing cgOverPlot objects. It is meant to be inherited and will
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
;-
;*****************************************************************************************

;+
;   The purpose of this method is to draw the over-plots in the draw window.
;-
pro MrAbstractOverPlot::Draw
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Call each of the OverPlot's draw methods. Since these are over plots, the
    ;device should already be configured by a previous call to Plot.
    for i = 0, n_elements(*self.oplots) - 1 do (*self.oplots)[i] -> Draw
end


;+
;   Add an weOverPlot or cgOverPlot object.
;
; :Params:
;       OVERPLOT_OBJECT:        in, optional, type=object/obj_arr
;                               The weOverPlot or cgOverPlot object(s) to add to the plot.
;
;   :Keywords:
;       CLEAR:                  in, optional, type=boolean, default=0
;                               If set, clear all overplot objects before adding new ones.
;                                   Otherwise, new ones are appended to the end.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the overplot to the list.
;       INDEX:                  in, optional, type=int, default=0
;                               The index at which to `REPLACE` or `REMOVE` overplot objects.
;       REMOVE:                 in, optional, type=boolean, default=0
;                               If set, remove the overplot object and `INDEX` from the list.
;       REPLACE:                in, optional, type=boolean, default=0
;                               If set, replace the object at `INDEX` instead of appending
;                                   to the end of the list.
;-
pro MrAbstractOverPlot::AddOverPlots, overplot_object, $
CLEAR = clear, $
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
    
    ;Remove an overplot?
    if keyword_set(remove) then begin
        if n_elements(index) eq 0 then index = 0
        n_oplots = n_elements(*self.oplots)
        
        case n_oplots of
            0: return
            1: begin
                ;destroy the object and reset the pointer
                obj_destroy, (*self.oplots)[index]
                ptr_free, self.oplots
                self.oplots = ptr_new(/ALLOCATE_HEAP)
            endcase
            
            else: begin
                ;destroy the object, shift the invalid object to the end, truncate
                obj_destroy, (*self.oplots)[index]
                *self.oplots = shift(*self.oplots, -index-1)
                *self.oplots = (*self.oplots)[0:n_oplots-1]
                *self.oplots = shift(*self.oplots, index)
            endelse
        endcase
        
        return
    endif
    
    ;Destroy and replace old object, then return
    if keyword_set(replace) then begin
        if n_elements(index) eq 0 then index = 0
        obj_destroy, (*self.oplots)[index]
        (*self.oplots)[index] = overplot_object
        return
    endif
    
    ;Clear all overplots first?
    if keyword_set(clear) then begin
        if ptr_valid(self.oplots) then begin
            for i = 0, n_elements(self.oplots) - 1 do obj_destroy, (*self.oplots)[i]
        endif
        ptr_free, self.oplots
    endif
    
    ;If nothing was given to add, then return
    if n_elements(overplot_object) eq 0 then begin
        if ptr_valid(self.oplots) eq 0 then self.oplots = ptr_new(/ALLOCATE_HEAP)
        return
    endif
    
    ;Add the object
    if ptr_valid(self.oplots) $
        then *self.oplots = [*self.oplots, overplot_object] $
        else self.oplots = ptr_new(overplot_object)
    
    ;Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   Create a cgOverPlot object to be overplotted onto the graph.
;
; :Params:
;       X:                  in, required, type=any
;                           If Y is given, a vector representing the independent variable
;                               to be plotted. If Y is not given, a vector or array of
;                               representing the dependent variable to be plotted. Each
;                               column of X will then be overplotted as individual vectors
;                               in the same set of axes.
;       Y:                  in, optional, type=any
;                           A vector or array of representing the dependent variable to be
;                               plotted. Each column of Y will then be overplotted
;                               as individual vectors in the same set of axes.
;
; :Keywords:
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the overplot to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weOverPlot__define or cgOverPlot__define
;
;   
;-
pro MrAbstractOverPlot::OverPlot, x, y, $
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

    ;Create a cgOverPlot object
    overplot = obj_new('weOverPlot', x, y, _STRICT_EXTRA=extra)

    ;Add the overplot to the array of overplots
    self -> AddOverPlots, overplot
    
    ;Re-Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   This method provides a means of determining the index location at which each
;   OverPlot is stored. OverPlots are distinguished by their y-titles.
;-
PRO MrAbstractOverPlot::whichOverPlots
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ;Allow the Superclass to have a space flush with the left of the display window
    Print, '  OverPlots:'
    
    ;Print the color bar indices and titles
    FOR j = 0, N_Elements(*self.oplots) - 1 DO BEGIN
        (*self.oplots)[j] -> GetProperty, COLOR=color
        IF N_Elements(color) EQ 0 THEN color = ''
        index = String(j, FORMAT='(i3)')
        
        Print, '    Index: ' + index + '    Color: ' + StrJoin(color, ', ')
    ENDFOR
END


;+
;   This method cleans up after the object is destroyed.
;-
pro MrAbstractOverPlot::cleanup
    compile_opt idl2
    
    ;Destroy all weOverPlot objects
    if ptr_valid(self.oplots) then begin
        for i = 0, n_elements(*self.oplots)-1 do begin
            obj_destroy, (*self.oplots)[i]
        endfor
        ptr_free, self.oplots
    endif
end


;+
;   The initialization method. Because MrAbstractOverPlot is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractOverPlot object will result
;   in an error.
;-
function MrAbstractOverPlot::init
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
pro MrAbstractOverPlot__define
    compile_opt idl2
    
    class = {MrAbstractOverPlot, $
             oplots: ptr_new()}
    
end