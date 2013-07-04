; docformat = 'rst'
;
; NAME:
;       MrAbstractImage__Define
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
;   removing, and drawing image objects. It is meant to be inherited and will
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
;       05/30/2013  -   Written by Matthew Argall.
;       06/17/2013  -   Added the NOERASE keyword to the Draw method. - MRA
;-
;*****************************************************************************************
;+
;   Add, remove, replace, or clear image objects from the list.
;
; :Params:
;       IMAGE_OBJECTS:          in, optional, type=object/obj_arr
;                               The image object(s) to add to the display.
;
;   :Keywords:
;       CLEAR:                  in, optional, type=boolean, default=0
;                               If set, clear all image objects. New ones can be added
;                                   in same call.
;       DRAW:                   in, optional, type=boolean, default=0
;                               Call the Draw method after adding the images to the list.
;       INDEX:                  in, optional, type=int, default=0
;                               The index at which to `REPLACE` or `REMOVE` image objects.
;       REMOVE:                 in, optional, type=boolean, default=0
;                               If set, remove the image object and `INDEX` from the list.
;       REPLACE:                in, optional, type=boolean, default=0
;                               If set, replace the object at `INDEX` instead of appending
;                                   to the end of the list.
;-
pro MrAbstractImage::AddImages, image_objects, $
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

;---------------------------------------------------------------------
;Remove //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    if keyword_set(remove) then begin
        if n_elements(index) eq 0 then index = 0
        nImages = n_elements(*self.imageObjects)
        
        case nImages of
            0: return
            1: begin
                ;destroy the object and reset the pointer
                obj_destroy, (*self.imageObjects)[index]
                ptr_free, self.imageObjects
                self.imageObjects = ptr_new(/ALLOCATE_HEAP)
            endcase
            
            else: begin
                ;destroy the object, shift the invalid object to the end, truncate
                obj_destroy, (*self.imageObjects)[index]
                *self.imageObjects = shift(*self.imageObjects, -index-1)
                *self.imageObjects = (*self.imageObjects)[0:nImages-1]
                *self.imageObjects = shift(*self.imageObjects, index)
            endelse
        endcase
        
        ;Draw?
        if keyword_set(draw) then self -> Draw
        return
    endif

;---------------------------------------------------------------------
;Replace /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    if keyword_set(replace) then begin
        if n_elements(index) eq 0 then index = 0
        obj_destroy, (*self.imageObjects)[index]
        (*self.imageObjects)[index] = image_objects
        
        ;Draw?
        if keyword_set(draw) then self -> Draw
        return
    endif

;---------------------------------------------------------------------
;Clear All ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    if keyword_set(clear) then begin
        if ptr_valid(self.imageObjects) then begin
            for i = 0, n_elements(self.imageObjects) - 1 do obj_destroy, (*self.imageObjects)[i]
        endif
        ptr_free, self.imageObjects
    endif

;---------------------------------------------------------------------
;Add /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If nothing was given to add, then return
    if n_elements(image_objects) eq 0 then begin
        if ptr_valid(self.imageObjects) eq 0 then self.imageObjects = ptr_new(/ALLOCATE_HEAP)
        return
    endif
    
    ;Add the object
    if ptr_valid(self.imageObjects) $
        then *self.imageObjects = [*self.imageObjects, image_objects] $
        else self.imageObjects = ptr_new(image_objects)
    
    ;Draw
    if keyword_set(draw) then self -> Draw
end


;+
;   The purpose of this method is to draw the images in the draw window.
;
; :Keywords:
;       NOERASE:                in, optional, type=boolean, default=0
;                               If set, do not erase the window before drawing the first
;                                   image.
;-
pro MrAbstractImage::Draw, $
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
    for i = 0, n_elements(*self.imageObjects) - 1 do begin
        if i gt 0 then noerase = 1
        
        theImage = (*self.imageObjects)[i]
        theImage -> SetProperty, NOERASE=noerase
        theImage -> Draw
    endfor
end


;+
;   Create a MrImagePlot object to be drawn in the device window.
;
; :Keywords:
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the image to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrImagePlot__define.
;   
;-
pro MrAbstractImage::Image, $
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

    ;Create the color bar
    theImage = obj_new('MrImagePlot', _STRICT_EXTRA=extra)
    
    ;Add the color bar
    self -> addImages, theImage
    
    ;Draw    
    if keyword_set(draw) then self -> Draw
end


;+
;   This method provides a means of determining the index location at which each
;   Image is stored. Images are distinguished by their titles.
;-
PRO MrAbstractImage::whichImages
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ;Allow the Superclass to have a space flush with the left of the display window
    Print, '  Images:'
    
    ;Print the image indices and titles
    FOR j = 0, N_Elements(*self.imageObjects) - 1 DO BEGIN
        (*self.imageObjects)[j] -> GetProperty, TITLE=title
        index = String(j, FORMAT='(i3.3)')
        
        IF N_Elements(title) EQ 0 THEN title = ''
        Print, '    Index: ' + index + '    Title: ' + title
    ENDFOR
END


;+
;   Clean up after the object is destroy
;-
pro MrAbstractImage::cleanup
    compile_opt idl2
    
    ;Destroy all weLegendItem objects
    if ptr_valid(self.imageObjects) then begin
        for i = 0, n_elements(*self.imageObjects)-1 do begin
            obj_destroy, (*self.imageObjects)[i]
        endfor
        ptr_free, self.imageObjects
    endif
end


;+
;   The initialization method. Because MrAbstractImage is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractImage object will result
;   in an error.
;-
function MrAbstractImage::init
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
pro MrAbstractImage__define
    compile_opt idl2
    
    class = {MrAbstractImage, $
             imageObjects: ptr_new()}
end