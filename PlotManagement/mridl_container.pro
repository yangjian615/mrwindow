; docformat = 'rst'
;
; NAME:
;       MrIDL_Container__Define
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
; PURPOSE:
;+
;   The purpose of this method is to add functionality to the IDL_Container class.
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
;   Modification History::
;       08/09/2013  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The purpose of this method is to add functionality to the IDL_Container::Add method. 
;
; :Params:
;       OBJECTS:        in, required, type=object/objarr
;                       An object instance or array of object instances to be added to
;                           the container object.
; :Keywords:
;       CLEAR:          in, optional, type=boolean, default=0
;                       Clear all contained items before adding the new ones.
;       POSITION:       in, optional, type=boolean, default=0
;                       Each index value specifies the position within the container at
;                           which a new object should be placed. If not specified, objects
;                           are added to the end of the list of contained items.
;-
pro MrIDL_Container::Add, Objects, $
CLEAR = clear, $
POSITION = Index
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Clear all contained items?
    if keyword_set(clear) then self -> Remove, /ALL
    
    ;Add the objects
    self -> Add, Objects, POSITION=Index
end


;+
;   The purpose of this method is to set object properties. 
;
; :Params:
;       OLD:            in, required, type=int/object
;                       If an integer is provided, it is the index of the object to be
;                           replaced. If an object, then the object reference to be replace.
;       NEW:            in, required, type=object
;                       The object that will replace `OLD`
;-
pro MrIDL_Container::Replace, old, new
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    szOld = size(old, /TYPE)
    
    ;Was an object given?
    if szOld eq 11 then begin
        ;Is the object in the container?
        tf_contained = self -> IsContained(old, POSITION=index)
        if tf_contained eq 0 then message, 'Object "OLD" not found. Cannot replace.'
        
        ;Remove the old object and add the new one.
        self -> Remove, old
        self -> Add, new, POSITION=index
    
    ;Was an index given?
    endif else begin
        self -> Remove, POSITION=old
        self -> Add, new, POSITION=old
    endelse
end


;+
;   The purpose of this method is to print a list of contained objects and the index
;   at which they can be found.
;-
pro MrIDL_Container::Which
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Get each of the objects.
    oContained = self -> Get(/ALL, ISA=class_name, COUNT=count)
    
    ;Print a header.
    print, '-Index-', '--Class--', FORMAT='(a7, 3x, a9)'
    
    ;Print the index and class name of each object.
    for i = 0, count - 1 do $
        print, i, class_name, FORMAT='(2x, i3, 6x, a0)'

end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrIDL_Container::cleanup
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Cleanup superclasses
    self -> MrIDL_Container::Cleanup
end


;+
;   The purpose of this method is to initialize the MrIDL_Container class.
;-
function MrIDL_Container::init
    compile_opt idl2

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif

    if self -> IDL_Container::INIT() eq 0 then return, 0
    
    return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrIDL_Container__define, class
    compile_opt idl2
    
    class = {MrIDL_Container, $
             inherits IDL_Container}
end