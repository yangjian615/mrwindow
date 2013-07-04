; docformat = 'rst'
;
; NAME:
;       MrAbstractAxis__Define
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
;   removing, and drawing weAxis objects. It is meant to be inherited and will
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
;       05/10/2013  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The purpose of this method is to draw the over-plots in the draw window.
;-
pro MrAbstractAxis::Draw
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
    for i = 0, n_elements(*self.axes) - 1 do (*self.axes)[i] -> Draw
end


;+
;   Add a weAxis object.
;
; :Params:
;       AXIS_OBJECT:        in, optional, type=object/obj_arr
;                               The weAxis object(s) to add to the plot.
;
;   :Keywords:
;       CLEAR:                  in, optional, type=boolean, default=0
;                               If set, clear all axes objects before adding new ones.
;                                   Otherwise, new ones are appended to the end.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the axes to the list.
;       INDEX:                  in, optional, type=int, default=0
;                               The index at which to `REPLACE` or `REMOVE` axis objects.
;       REMOVE:                 in, optional, type=boolean, default=0
;                               If set, remove the axis object and `INDEX` from the list.
;       REPLACE:                in, optional, type=boolean, default=0
;                               If set, replace the object at `INDEX` instead of appending
;                                   to the end of the list.
;-
pro MrAbstractAxis::AddAxes, axis_object, $
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
        n_axes = n_elements(*self.axes)
        
        case n_axes of
            0: return
            1: begin
                ;destroy the object and reset the pointer
                obj_destroy, (*self.axes)[index]
                ptr_free, self.axes
                self.axes = ptr_new(/ALLOCATE_HEAP)
            endcase
            
            else: begin
                ;destroy the object, shift the invalid object to the end, truncate
                obj_destroy, (*self.axes)[index]
                *self.axes = shift(*self.axes, -index-1)
                *self.axes = (*self.axes)[0:n_axes-1]
                *self.axes = shift(*self.axes, index)
            endelse
        endcase
        
        return
    endif
    
    ;Destroy and replace old object, then return
    if keyword_set(replace) then begin
        if n_elements(index) eq 0 then index = 0
        obj_destroy, (*self.axes)[index]
        (*self.axes)[index] = axis_object
        return
    endif
    
    ;Clear all overplots first?
    if keyword_set(clear) then begin
        if ptr_valid(self.axes) then begin
            for i = 0, n_elements(self.axes) - 1 do obj_destroy, (*self.axes)[i]
        endif
        ptr_free, self.axes
    endif
    
    ;If nothing was given to add, then return
    if n_elements(axis_object) eq 0 then begin
        if ptr_valid(self.axes) eq 0 then self.axes = ptr_new(/ALLOCATE_HEAP)
        return
    endif
    
    ;Add the object
    if ptr_valid(self.axes) $
        then *self.axes = [*self.axes, axis_object] $
        else self.axes = ptr_new(axis_object)
    
    ;Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   Create a weAxis object to be overplotted onto the graph.
;
; :Params:
;       XLOC:               in, optional, type=depends
;                           The X location of the axis. 
;       YLOC:               in, optional, type=depends
;                           The Y location of the axis. 
;       ZLOC:               in, optional, type=depends
;                           The Z location of the axis. 
;
; :Keywords:
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the overplot to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weOverPlot__define or cgOverPlot__define
;
;   
;-
pro MrAbstractAxis::Axis, xloc, yloc, zloc, $
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
    theAxis = obj_new('weAxis', xloc, yloc, zloc, _STRICT_EXTRA=extra)

    ;Add the overplot to the array of overplots
    self -> AddAxes, theAxis
    
    ;Re-Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   This method provides a means of determining the index location at which each
;   Axis is stored. Axes are distinguished by their titles.
;-
PRO MrAbstractAxis::whichAxes
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ;Allow the Superclass to have a space flush with the left of the display window
    Print, '  Axes:'
    
    ;Print the color bar indices and titles
    FOR j = 0, N_Elements(*self.axes) - 1 DO BEGIN
        (*self.axes)[j] -> GetProperty, XTITLE=xtitle, YTITLE=ytitle
        index = String(j, FORMAT='(i3)')

        ;Which title is it?        
        IF N_Elements(YTITLE) NE 0 THEN title = ytitle
        IF N_Elements(XTITLE) NE 0 THEN title = xtitle
        IF N_Elements(TITLE)  EQ 0 THEN title = 'Axis ' + index

        Print, '    Index: ' + index + '    Title: ' + title
    ENDFOR
END


;+
;   This method cleans up after the object is destroyed.
;-
pro MrAbstractAxis::cleanup
    compile_opt idl2
    
    ;Destroy all weOverPlot objects
    if ptr_valid(self.axes) then begin
        for i = 0, n_elements(*self.axes)-1 do begin
            obj_destroy, (*self.axes)[i]
        endfor
        ptr_free, self.axes
    endif
end


;+
;   The initialization method. Because MrAbstractAxis is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractAxis object will result
;   in an error.
;-
function MrAbstractAxis::init
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
pro MrAbstractAxis__define
    compile_opt idl2
    
    class = {MrAbstractAxis, $
             axes: ptr_new()}
    
end