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
;   Enhancements include::
;       - Retrieve index of object within container
;       - Print a list of object in the container
;       - Container can be cleared before adding anything to it.
;       - Objects can be replaced.
;       - Bracket overloading allows access a la ISA and POSITION.
;       - Objects can be removed without destroying them.
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
;       08/23/2013  -   Added the GetIndex method. - MRA
;       2013/11/19  -   Inherit IDL_Object, added the _OverloadBracketsRightSide method. - MRA
;       2013/12/10  -   Added the _OverloadForeach method. - MRA
;       2014/01/10  -   Added negative indexing to _OverloadBRS for single indices. - MRA
;       2014/03/04  -   _OverloadBRS now accepts arrays of indices. - MRA
;       2014/03/06  -   Added the _OverloadPrint method. The WhichObjects method now calls
;                           the _OverloadPrint method. - MRA
;       2014/03/10  -   Renamed the WhichObjects method to PrintInfo. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to provide an array-like means of accessing graphics
;   objects within the container. Like calling the Get method with either the POSITION or
;   ISA keywords.
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
function MrIDL_Container::_OverloadBracketsRightSide, isRange, subscript1
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

;-----------------------------------------------------
;Integer Subscripts \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if MrIsA(subscript1, /INTEGER) then begin
    
        ;If a scalar or array was given, get what was asked for.
        if isRange[0] eq 0 then begin
            ;Negative index?
            iNeg = where(subscript1 lt 0, nNeg)
            if nNeg gt 0 then begin
                nObj = self -> Count()
                subscript1[iNeg] += nObj
            endif
        
            result = self -> Get(POSITION=subscript1)
        
        ;If a range was given, create an index array.
        endif else begin
            position = linspace(subscript1[0], subscript1[1], subscript1[2], /INTERVAL, TYPE=3)
            result = self -> Get(POSITION=position)
        endelse

;-----------------------------------------------------
;String Subscripts \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else if size(subscript1, /TNAME) eq 'STRING' then begin
        result = self -> Get(IsA=subscript1)
        
    endif else message, 'Invalid subscript.'

    return, result
end

;+
;   The purpose of this method is to allow for iteration through the objects within the
;   container via the FOREACH operator.
;
; :Params:
;       VALUE:              in, out, required, type=object
;                           The current object.
;       KEY:                in, out, required, type=int/string
;                           The index of the current object.
;
; :Returns:
;       RESULT:             1 if there is a current element to retrieve, 0 if there are
;                               no more elements.
;-
function MrIDL_Container::_OverloadForeach, value, key
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif
    
    ;Get all of the objects
    allObjs = self -> Get(/ALL, COUNT=nObjs)

    ;Return the current object
    if n_elements(key) eq 0 then key = 0 else key += 1
    
    ;Stop if there are no more objects.
    if key ge nObjs then return, 0

    ;Return the current object
    value = allObjs[key]

    ;Go to the next iteration
    return, 1
end


;+
;   The purpose of this method provide output when the PRINT procedure is called.
;
; :Returns:
;       RESULTS:            A string to be printed by the PRINT procedure.
;-
function MrIDL_Container::_OverloadPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg(/QUIET)
        return, ''
    endif

    ;Print information about the container class
    class  = obj_class(self)
    heapID = obj_valid(self, /GET_HEAP_IDENTIFIER)
    selfStr = string(FORMAT='(a0, 3x, "<", i0, ">")', class, heapID)

    ;Get all of the objects
    allObjs = self -> Get(/ALL, COUNT=nObjs)
    if nObjs eq 0 then return, 'Container is empty.'
    
    ;Identifiers and types
    objIDs = obj_valid(allObjs, /GET_HEAP_IDENTIFIER)
    objClass = MrObj_Class(allObjs)

    ;Header
    printStr      = strarr(1,nObjs+2)
    printStr[0,0] = string(selfStr)
    printStr[0,1] = string('Index', 'HeapID', 'Class', FORMAT='(1x, a5, 4x, a6, 2x, a5)')
    
    ;Print object information
    for i = 0, nObjs-1 do begin
        if objIDs[i] eq 0 then objIDstr = '<invalid>' else objIDstr = strtrim(objIDs[i], 2)
        printStr[0,i+2] = string(i, objIDstr, objClass[i], FORMAT='(i5, 3x, i7, 3x, a0)')
    endfor

    return, printStr
end


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
POSITION = Index
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Clear all contained items?
    if keyword_set(clear) then self -> Remove, /ALL, DESTROY=destroy
    
    ;Add the objects
    self -> IDL_Container::Add, Objects, POSITION=Index
end



;+
;   Get the index within the container at which the given object(s) is stored.
;
; :Params:
;       OBJECTS:        in, required, type=object/objarr
;                       An object instance or array of object instances to be added to
;                           the container object.
;
; :Returns:
;       INDEX:          The index within the container at which `OBJECTS` are stored.
;-
function MrIDL_Container::GetIndex, Objects
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, -1
    endif
    
    ;Get the index at which the object is stored.
    tf_contained = self -> IDL_Container::IsContained(Objects, POSITION=Index)
    return, index
end


;+
;   The purpose of this method is to print a list of contained objects and the index
;   at which they can be found.
;-
pro MrIDL_Container::PrintInfo
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    result = self -> _OverloadPrint()
    print, result
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
        void = cgErrorMsg(/QUIET)
        return
    endif
    
    szOld = size(old, /TYPE)
    SetDefaultValue, destroy, 0, /BOOLEAN
    
    ;Was an object given?
    if szOld eq 11 then begin
        ;Is the object in the container?
        tf_contained = self -> IsContained(old, POSITION=index)
        if tf_contained eq 0 then begin
            message, 'Object "' + obj_class(old) + '" not found. Cannot replace.', /INFORMATIONAL
            return
        endif
        
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
;       - Specific classes of objects can be removed.
;       - Child_Object, Position, and Type can be supplied together
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
;       POSITION:           in, optional, type=integer
;                           The index value within the container of the object to be
;                               destroyed. To see index values, call the WhichObjects
;                               method.
;       TYPE:               in, optional, type=string/strarr
;                           The class names of the objects to be destroyed.
;-
pro MrIDL_Container::Remove, Child_Object, $
ALL = all, $
DESTROY = destroy, $
POSITION = index, $
TYPE = type
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Defaults
    all     = keyword_set(all)
    destroy = keyword_set(destroy)
        
;---------------------------------------------------------------------
;Remove All? /////////////////////////////////////////////////////////
;--------------------------------------------------------------------- 
    ;Remove all objects?
    if keyword_set(all) then begin
        if destroy then allObj = self -> Get(/ALL, COUNT=nObj)
        self -> IDL_Container::Remove, /ALL
        if destroy eq 1 && nObj gt 0 then obj_destroy, allObj
        return
    endif
        
;---------------------------------------------------------------------
;Remove An Index? ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Necessary to remove Indices first. Otherwise indices will change
    ;as specific objects are removed.
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
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrIDL_Container::cleanup
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
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
        void = cgErrorMsg()
        return, 0
    endif

    ;Superclass
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