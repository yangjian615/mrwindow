; docformat = 'rst'
;
; NAME:
;       MrAbstractLegend__Define
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
;   not functions on its own.
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
;       05/10/2013  -   Upgrade to weLegendItem made LOCATION keyword obsolete. Removed. - MRA
;-
;*****************************************************************************************
;+
;   Add an weLegendItem or cgLegendItem object.
;
; :Params:
;       LEGEND_OBJECT:          in, optional, type=object/obj_arr
;                               The weLegendItem or cgLegendItem object(s) to add to the plot.
;
;   :Keywords:
;       CLEAR:                  in, optional, type=boolean, default=0
;                               If set, clear all legend objects. New ones can be added
;                                   in same call.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the legends to the list.
;       INDEX:                  in, optional, type=int, default=0
;                               The index at which to `REPLACE` or `REMOVE` legend objects.
;       REMOVE:                 in, optional, type=boolean, default=0
;                               If set, remove the legend object and `INDEX` from the list.
;       REPLACE:                in, optional, type=boolean, default=0
;                               If set, replace the object at `INDEX` instead of appending
;                                   to the end of the list.
;-
pro MrAbstractLegend::AddLegends, legend_object, $
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
    
    ;Remove a legend?
    if keyword_set(remove) then begin
        if n_elements(index) eq 0 then index = 0
        n_legends = n_elements(*self.legends)
        
        case n_legends of
            0: return
            1: begin
                ;destroy the object and reset the pointer
                obj_destroy, (*self.legends)[index]
                ptr_free, self.legends
                self.legends = ptr_new(/ALLOCATE_HEAP)
            endcase
            
            else: begin
                ;destroy the object, shift the invalid object to the end, truncate
                obj_destroy, (*self.legends)[index]
                *self.legends = shift(*self.legends, -index-1)
                *self.legends = (*self.legends)[0:n_oplots-1]
                *self.legends = shift(*self.legends, index)
            endcase
        endcase
        
        return
    endif
    
    ;Destroy and replace old object, then return
    if keyword_set(replace) then begin
        if n_elements(index) eq 0 then index = 0
        obj_destroy, (*self.legends)[index]
        (*self.legends)[index] = legend_object
        return
    endif
    
    ;Clear all overplots first?
    if keyword_set(clear) then begin
        if ptr_valid(self.legends) then begin
            for i = 0, n_elements(self.legends) - 1 do obj_destroy, (*self.legends)[i]
        endif
        ptr_free, self.legends
    endif
    
    ;If nothing was given to add, then return
    if n_elements(legend_object) eq 0 then begin
        if ptr_valid(self.legends) eq 0 then self.legends = ptr_new(/ALLOCATE_HEAP)
        return
    endif
    
    ;Add the object
    if ptr_valid(self.legends) $
        then *self.legends = [*self.legends, legend_object] $
        else self.legends = ptr_new(legend_object)
    
    ;Draw
    if keyword_set(draw) then self -> Draw
end

;+
;   The purpose of this method is to draw the legends in the draw window.
;-
pro MrAbstractLegend::Draw
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
    for i = 0, n_elements(*self.legends) - 1 do (*self.legends)[i] -> Draw
end


;+
;   Create a cgLegendItemp object to be placed on the graph.
;
; :Keywords:
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the legend to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by cgLegendItem.
;-
pro MrAbstractLegend::Legend, $
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
    
    ;Create the legend
    theLegend = obj_new('weLegendItem', _STRICT_EXTRA=extra)
    
    ;Add the legend
    self -> AddLegends, theLegend
    
    ;Draw    
    self -> Draw
end


;+
;   This method provides a means of determining the index location at which each
;   Legend is stored. Legends are distinguished by their Titles and Colors.
;-
PRO MrAbstractLegend::whichLegends
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ;Allow the Superclass to have a space flush with the left of the display window
    Print, '  LegendItems:'
    
    ;Print the color bar indices and titles
    FOR j = 0, N_Elements(*self.legends) - 1 DO BEGIN
        (*self.legends)[j] -> GetProperty, TITLE=title, COLOR=color
        index = String(j, FORMAT='(i3)')

        IF N_Elements(title) EQ 0 $
            THEN title = 'LegendItem ' + index $
            ELSE title = StrJoin(title, ', ')
            
        IF N_Elements(color) EQ 0 $
            THEN color = '' $
            ELSE color = StrJoin(color, ', ')
            
        Print, '    Index: ' + index + '    Title: ' + title
        Print, '           ' + '   ' + '    Colors: ' + color
    ENDFOR
END


;+
;   Clean up after the object is destroy
;-
pro MrAbstractLegend::cleanup
    compile_opt idl2
    
    ;Destroy all weLegendItem objects
    if ptr_valid(self.legends) then begin
        for i = 0, n_elements(*self.legends)-1 do begin
            obj_destroy, (*self.legends)[i]
        endfor
        ptr_free, self.legends
    endif
end


;+
;   The initialization method. Because MrAbstractLegend is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractLegend object will result
;   in an error.
;-
function MrAbstractLegend::init
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
pro MrAbstractLegend__define
    compile_opt idl2
    
    class = {MrAbstractLegend, $
             legends: ptr_new()}
    
end