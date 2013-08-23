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
;       08/22/2013  -   Added the DESTROY keyword and Remove method. The Which method
;                           now finds the class names correctly. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to add functionality to the IDL_Container::Add method.
;   Additions include::
;       - Container can be cleared before new objects are added
;       - Objects cleared in this way can be destroyed
;       - POSITION is now named INDEX
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
;       DESTROY:        in, optional, type=boolean, default=0
;                       Destroy all objects being cleared. This is only valid when
;                           `CLEAR` is set.
;-
pro MrIDL_Container::Add, Objects, $
CLEAR = clear, $
DESTROY = destroy, $
INDEX = Index
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Defaults
    SetDefaultValue, destroy, 0, /BOOLEAN
    
    ;Clear all contained items?
    if keyword_set(clear) then self -> Remove, /ALL, DESTROY=destroy
    
    ;Add the objects
    self -> IDL_Container::Add, Objects, POSITION=Index
end


;+
;   The purpose of this method is to provide a means of relacing objects in an
;   IDL Container. 
;
; :Params:
;       OLD:            in, required, type=int/object
;                       If an integer is provided, it is the index of the object to be
;                           replaced. If an object, then the object reference to be replace.
;       NEW:            in, required, type=object
;                       The object that will replace `OLD`
;
; :Keywords:
;       DESTROY:            in, optional, type=boolean, default=0
;                           Destroy the `OLD` object being replaced
;-
pro MrIDL_Container::Replace, old, new, $
DESTROY = destroy
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    szOld = size(old, /TYPE)
    SetDefaultValue, destroy, 0, /BOOLEAN
    
    ;Was an object given?
    if szOld eq 11 then begin
        ;Is the object in the container?
        tf_contained = self -> IsContained(old, POSITION=index)
        if tf_contained eq 0 then message, 'Object "OLD" not found. Cannot replace.'
        
        ;Remove the old object and add the new one.
        self -> Remove, old, DESTROY=destroy
        self -> Add, new, POSITION=index
    
    ;Was an index given?
    endif else begin
        self -> Remove, POSITION=old, DESTROY=destroy
        self -> Add, new, POSITION=old
    endelse
end



;+
;   The purpose of this method is to add functionality to the IDL_Container class.
;   Additions include::
;       - Position has been renamed to Index
;       - Specific classes of objects can be removed.
;       - Child_Object, Index, and Type can be supplied together
;       - Objects can be destroyed while being removed.
;
; :Params:
;       Child_Object:       in, optional, type=object
;                           The object reference(s) for the objects to be removed from
;                               the container. If not provided, and neither are `ALL`,
;                               `INDEX`, and `TYPE`, then the first object in the container
;                               will be removed.
;
; :Keywords:
;       ALL:                in, optional, type=boolean, default=0
;                           Remove all objects from the container.
;       DESTROY:            in, optional, type=boolean, default=0
;                           Destroy all objects being removed.
;       INDEX:              in, optional, type=integer
;                           The index value within the container of the object to be
;                               destroyed.
;       TYPE:               in, optional, type=string/strarr
;                           The class names of the objects to be destroyed.
;-
pro MrIDL_Container::Remove, Child_Object, $
ALL = all, $
DESTROY = destroy, $
INDEX = index, $
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
    SetDefaultValue, destroy, 0, /BOOLEAN
    SetDefaultValue, all, 0, /BOOLEAN
        
;---------------------------------------------------------------------
;Remove All? /////////////////////////////////////////////////////////
;--------------------------------------------------------------------- 
    ;Remove all objects?
    if keyword_set(all) then begin
        if destroy then allObj = self -> Get(/ALL)
        self -> IDL_Container::Remove, /ALL
        if destroy then obj_destroy, allObj
        return
    endif
        
;---------------------------------------------------------------------
;Remove An Index? ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Remove Indices first.
    if n_elements(index) ne 0 then begin
        if destroy then RemoveThese = self -> Get(POSITION=index)
        self -> IDL_Container::Remove, POSITION=index
        if destroy then obj_destroy, RemoveThese
    endif
        
;---------------------------------------------------------------------
;Remove a Specific Type? /////////////////////////////////////////////
;--------------------------------------------------------------------- 
    
    if n_elements(type) ne 0 then begin
        RemoveThese = self -> Get(/ALL, ISA=type)
        self -> IDL_Container::Remove, RemoveThese
        if destroy then obj_destroy, RemoveThese
    endif
        
;---------------------------------------------------------------------
;Remove Children? ////////////////////////////////////////////////////
;--------------------------------------------------------------------- 
    if n_elements(Child_Object) ne 0 then begin
        self -> IDL_Container::Remove, Child_Object
        if destroy then obj_destroy, Child_Object
        
;---------------------------------------------------------------------
;Remove First Object? ////////////////////////////////////////////////
;--------------------------------------------------------------------- 
    
    ;Remove the first object in the container if nothing else is removed.
    endif else if (n_elements(type) eq 0) && (n_elements(index) eq 0) then begin
        if destroy then theObj = self -> Get(POSITION=0)
        self -> IDL_Container::Remove
        if destroy then obj_destroy, theObj
    endif
    
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
    oContained = self -> Get(/ALL, COUNT=count)

    ;Print a header.
    print, '-Index-', '--Class--', FORMAT='(a7, 3x, a9)'
    
    ;Print the index and class name of each object.
    for i = 0, count - 1 do $
        print, i, typename(oContained[i]), FORMAT='(2x, i3, 6x, a0)'

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
    self -> IDL_Container::Cleanup
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