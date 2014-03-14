; docformat = 'rst'
;
; NAME:
;       MrWindow_Container__Define
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
;   A container class for holding MrWindow objects. This particular container has the
;   following attributes::
;       - Only accepts MrWindow objects
;       - Able to find objects by name
;       - Overload brackets by name or container position.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;	Modification History::
;       2014/03/10  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The purpose of this method is to provide an array-like means of accessing graphics
;   objects within the container. Two options are avaible: the object index within the
;   container or the name of the graphic object.
;
; :Private:
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
function MrWindow_Container::_OverloadBracketsRightSide, isRange, subscript1
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Search for objects by name. Return the first match.
    if size(subscript1, /TNAME) eq 'STRING' then begin
        if MrIsA(subscript1, /SCALAR) eq 0 then message, 'String subscripts must be scalars.'
        
        ;Find the window with the given name
        result = self -> FindByName(subscript1, COUNT=count)
        if count gt 1 then result = result[0]
        
    ;Call the superclass's method
    endif else begin
        result = self -> MrIDL_Container::_OverloadBracketsRightSide(isRange, subscript1)
    endelse

    return, result
end


;+
;   The purpose of this method provide output when the PRINT procedure is called.
;
; :Returns:
;       RESULTS:            A string to be printed by the PRINT procedure.
;-
function MrWindow_Container::_OverloadPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg(/QUIET)
        return, ''
    endif

    ;Information about the container class
    class  = obj_class(self)
    heapID = obj_valid(self, /GET_HEAP_IDENTIFIER)
    selfStr = string(FORMAT='(a0, 3x, "<", i0, ">")', class, heapID)
    
    ;Get all of the objects
    allObjs = self -> Get(/ALL, COUNT=nObjs)
    if nObjs eq 0 then return, 'No MrWindow graphics windows are open.'
    
    ;Identifiers and types
    objIDs = obj_valid(allObjs, /GET_HEAP_IDENTIFIER)
    objClass = MrObj_Class(allObjs)
    
    ;Length of the longest class
    len = strtrim(max(strlen(objClass)), 2)

    ;Header
    printStr = strarr(1,nObjs+2)
    printStr[0,0] = selfStr
    printStr[0,1] = string('Index', 'HeapID', 'Class', 'Name', $
                           FORMAT='(1x, a5, 4x, a6, 2x, a' + len + ', 3x, a4)')

    ;Step through each object
    for i = 0, nObjs-1 do begin
        ;Careful of object array vs. bracket overloading for scalar
        if nObjs eq 1 $
            then name = allObjs    -> GetName() $
            else name = allObjs[i] -> GetName()
        
        if objIDs[i] eq 0 then objIDstr = '<invalid>' else objIDstr = strtrim(objIDs[i], 2)
        printStr[0,i+2] = string(i, objIDstr, objClass[i], name, $
                                 FORMAT='(i5, 3x, i7, 3x, a' + len + ', 3x, a0)')
    endfor

    return, printStr
end


;+
;   Only MrWindow objects can be added to this particular container.
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
function MrWindow_Container::Add, Objects, $
CLEAR = clear, $
DESTROY = destroy, $
POSITION = Index
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Make sure we are only adding MrWindow objects
    if min(MrObj_Class(Objects) eq 'MRWINDOW') eq 0 then $
        message, 'Only MrWindow objects can be added to MrWindow_Container.'
    
    ;Call the superclass method
    self -> MrIDL_Container::Add, Objects, CLEAR=clear, DESTROY=destroy, POSITION=index
end


;+
;   The purpose of this method is to find windows based on the name they were given.
;
; :Params:
;       NAME:           in, required, type=string
;                       Name of a MrWindow graphics window to be retrieved
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of windows found with name `NAME`.
;
; :Returns:
;       RESULT:         All windows with name `NAME`. If `COUNT`=0, an invalid object
;                           reference is returned.
;-
function MrWindow_Container::FindByName, name, $
COUNT=count
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        Count = 0
        return, obj_new()
    endif

    ;Get all of the objects in the container
    allWins = self -> Get(/ALL, COUNT=count)
    if count eq 0 then return, obj_new()
    
    ;Get the names of every window
    allNames = strarr(count)
    for i = 0, count - 1 do allNames[i] = allWins[i] -> GetName()
    
    ;Find a match
    iMatch = where(allNames eq name, count)
    if count eq 0 then return, obj_new()
    if count eq 1 then iMatch = iMatch[0]
    
    ;Return the matching objects
    return, allWins[iMatch]
end


;+
;   Clean up after the object is destroy
;-
pro MrWindow_Container::cleanup
    self -> MrIDL_Container::Cleanup
end


;+
;   The initialization method.
;-
function MrWindow_Container::init
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif

    ;Setup the plot window
    if self -> MrIDL_Container::init() eq 0 then return, 0
    
    return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrWindow_Container__define, class
    compile_opt strictarr
    
    define = { MrWindow_Container, $
               inherits MrIDL_Container $       ;An object container.
             }
end