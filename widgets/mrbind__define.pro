; docformat = 'rst'
;
; NAME:
;       MrBind__Define
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
; PURPOSE
;+
;   The purpose of this class is to provide a means of binding the properties of one
;   object to the properties of another. In this way, when the properties of one object
;   are changed, the matching properties the objects bound to it are changed as well.
;
; :Uses:
;   Uses the following external programs::
;       MGcoHashTable__Define   (Michael Galloy)
;       LinkedList__Define      (Coyote Graphics)
;       error_message           (Coyote Graphics)
;       isMember
;       MrUniq
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
;       10/10/2013  -   Written by Matthew Argall
;       10/13/2013  -   Added the QUIET keyword to Apply_Bindings. - MRA
;-
;*****************************************************************************************
;+
;   Bind the axis of one object to the axis of another so that the zooms are synchronized.
;
; :Params:
;       NAME:               in, required, type=linked list
;                           The name of the binding two which `THEOBJECTS` will be added.
;       THEOBJECTS:         in, required, type=objarr()
;                           Add these objects to `BINDINGLIST`
;-
pro MrBind::Add_Binding, name, theObjects
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Check that the objects are valid
    iValid = where(obj_valid(theObjects) eq 1, nValid)
    if nValid lt 1 then message, 'At least two valid objects must be provided.'

;---------------------------------------------------------------------
;Bind the Objects ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;First get the named binding list
    bindingList = self.bindings -> Get(name, FOUND=found)
    if ~found then message, string(name, FORMAT='(%"Binding \"%s\" not found.")')
    
    ;Number of bindings that already exist for this name
    nSets = bindingList -> Get_Count()
    
    ;Create a new node within the named list
    bindingList -> Add, theObjects
    
    ;If bindings had already existed, then consolidate
    if nSets gt 0 then self -> Consolidate_Bindings, name
end


;+
;   Bind the axis of one object to the axis of another so that the zooms are synchronized.
;
; :Params:
;       NAME:               in, required, type=string
;                           The name of the of the binding. When bindings are applied or
;                               updated, they are referenced by name.
;       OBJECT:             in, required, type=object
;                           Check to see if anything is bound to this object. If so,
;                               make all bound objects have the same range as THEOBJECT.
;                               Which ranges are updated depend on which keywords are set.
;
; :Keywords:
;       APPLYTOOBJECT:      in, optional, type=boolean, default=0
;                           If set, the properties in `_EXTRA` will also be applied to
;                               `OBJECT`. this action is kept separate in case OBJECT is
;                               not currently bound to anything. Setting this automatically
;                               sets `QUIET`=1
;       QUIET:              in, optional, type=boolean, default=0
;                           Setting this keyword also suppresses an error messages saying
;                               `NAME` or `OBJECT` is not found or bound to anything.
;       _EXTRA:             in, required, type=structure
;                           A structure in the form of keyword-value pairs that will
;                               be passed to each object's SetProperty method.
;-
pro MrBind::Apply_Bindings, name, object, $
APPLYTOOBJECT = applyToObject, $
QUIET = quiet, $
_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Apply to the input object?
    applyToObject = keyword_set(applyToObject)
    quiet = keyword_set(quiet)
    
    if applyToObject eq 1 then begin
        object -> SetProperty, _EXTRA=extra
        quiet = 1
    endif
    
    ;First get the named binding list
    bindingList = self.bindings -> Get(name, FOUND=found)
    if ~found then begin
        if (quiet eq 0) then message, string(name, FORMAT='(%"Binding \"%s\" not found.")')
        return
    endif

    ;Number of binding sets with this NAME
    nSets = bindingList -> Get_Count()

    ;Find the set of bindings that contains OBJECT
    iBinding = -1
    counter = 0
    while (counter lt nSets) && (iBinding eq -1) do begin
        boundObjects = bindingList -> Get_Item(counter)
        iBinding = where(boundObjects eq object, nObj, NCOMPLEMENT=nApply, COMPLEMENT=iApply)
        counter++
    endwhile
    
    ;If no object was found, send a message
    if (applyToObject eq 0) then begin
        if (quiet eq 0) then message, 'OBJECT is not bound to anything by this NAME.'
        return
    endif
    
    ;Step through each object bound to OBJECT and update its properties
    for i = 0, nApply - 1 do boundObjects[iApply[i]] -> SetProperty, _EXTRA=extra
end


;+
;   Bind the axis of one object to the axis of another so that the zooms are synchronized.
;
; :Params:
;       NAME:               in, required, type=string
;                           The name of the of the binding. When bindings are applied or
;                               updated, they are referenced by name.
;       OBJECTS:            in, required, type=objarr()
;                           The objects whose properties are to be bound together.
;
; :Keywords:
;       APPLY:              in, optional, type=boolean, default=1
;                           If set, once the bindings are created, they will be applied so
;                               that the axis ranges match. The ranges of `OBJECT2` will
;                               be update to match those of `OBJECT1[0]`.
;       REMOVE:             in, optional, type=boolean, default=0
;                           If set, the given objects will be unbound from their current
;                               binding set.
;       _EXTRA:             in, optional, type=structure
;                           A structure in the form of keyword-value pairs that will
;                               be passed to each object's SetProperty method.
;-
pro MrBind::Bind, name, objects, $
REMOVE = remove, $
_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Apply or Remove bindings?
    apply = keyword_set(apply)
    remove = keyword_set(remove)
    
;---------------------------------------------------------------------
;Remove Bindings? ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;If the binding does not exist yet, create it.
    named_binding = self.bindings -> Get(name, FOUND=found)
    if (found eq 0) then self.bindings -> Put, name, obj_new('LinkedList')
    
    ;Add or Remove the binding?
    if (remove eq 1) $
        then self -> Remove_Binding, name, objects $
        else self -> Add_Binding,    name, objects
        
    ;Apply Bindings?
    if n_elements(extra) gt 0 then self -> Apply_Bindings, name, objects[0], _EXTRA=extra
end


;+
;   The purpose of this method is to check if a particular binding name has been created
;   yet.
;
; :Params:
;       NAME:               in, required, type=string
;                           The name of the of the binding. When bindings are applied or
;                               updated, they are referenced by name.
;
; :Returns:
;       TF_EXISTS:          Returns true (1) a binding with name `NAME` exists and
;                               false (0) if not.
;-
function MrBind::Binding_Exists, name
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Get the hash keys and how many there are
    keys = self.bindings -> keys(COUNT=nKeys)
    
    ;Does the name exist?
    if nKeys eq 0 $
        then tf_exists = 0 $
        else tf_exists = isMember(keys, name)
    
    return, tf_exists
end


;+
;   If an object is found in more than one set of bindings, then consolidate the
;   different sets into one.
;
; :Params:
;       NAME:           in, required, type=string
;                       Name of the binding to be consolidated.
;
; :Uses:
;   Uses the following external programs::
;       error_message.pro (Coyote Graphics)
;       MrUniq.pro
;-
pro MrBind::Consolidate_Bindings, name
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;First get the named binding list
    bindingList = self.bindings -> Get(name, FOUND=found)
    if ~found then message, string(name, FORMAT='(%"Binding \"%s\" not found.")')

    ;How many sets of bindings exist for this name
    nBindings = bindingList -> Get_Count()

;---------------------------------------------------------------------
;One Set of Bindings /////////////////////////////////////////////////
;---------------------------------------------------------------------    
    if nBindings eq 1 then begin
        ;Select only the unique objects that around bound together
        boundObjects = bindingList -> Get_Item(0)
        boundObjects = boundObjects[MrUniq(boundObjects, /SORT)]
        
        ;If only one object is left, remove the binding set from the list
        if n_elements(boundObjects) le 1 then bindingList -> Delete, 0, DESTROY=0

;---------------------------------------------------------------------
;Many Sets of Bindings ///////////////////////////////////////////////
;---------------------------------------------------------------------  
    endif else begin
        ;Get all of the bound objects under this name.
        allObjects = bindingList -> Get_Item(/ALL)
        
        ;Find duplicates
        void = MrUniq(allObjects, /SORT, NCOMPLEMENT=nCopies, COMPLEMENT=iCopies)

    ;---------------------------------------------------------------------
    ;Deal With Duplicates ////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        if nCopies gt 0 then begin
            ;Take the first duplicate only for now
            objToConsolicate = allObjects[iCopies[0]]
            
            ;Step through all of the binding sets
            i = 0
            iKeep = -1
            while i lt nBindings do begin
                boundObjects = bindingList -> Get_Item(i)
            
                ;Find the first instance of the duplicate object
                if iKeep eq -1 then begin
                    void = where(boundObjects eq objToConsolidate, count)
                    
                    ;If the duplicate was found, store the index of the binding set.
                    ;Go to the next bindings set.
                    if count ne 0 then iKeep = i
                    i += 1
                    
                ;Find other instances of the duplicate object
                endif else begin
                    void = where(boundObjects eq objToConsolidate, count)
                    
                    ;When found...
                    if count ne 0 then begin
                        ;Concatenate the sets of bound objects. Do not keep the duplicate.
                        firstBinding = bindingList -> Get(POSITION=iKeep)
                        newBinding   = [firstBinding, boundObjects]
                        newBinding   = newBinding[MrUniq(newBinding, /SORT)]
                        bindingList -> Replace_Item, newBinding, iKeep
                        
                        ;Remove the binding set containing the duplicate from list of bindings
                        bindingList -> Delete, i, DESTROY=0
                        
                        ;Decrease the total number of bindings. Do not increase "i"
                        ;because the i-th binding was removed.
                        nBindings = bindingList -> Get_Count()
                    endif
                endelse
            endwhile
            
            ;Now that one copy is gone, consolidate again to remove those that remain
            self -> Consolidate_Bindings, bindingList
        endif        
    endelse    
end


;+
;   The purpose of this method is to count the number of named bindings that exist.
;
; :Returns:
;       nBindings:          The number of binding names that exist.
;-
function MrBind::Count_Bindings
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Get the number of named bindings.
    nBindings = self.bindings -> Get_Count()
    
    return, nBindings
end


;+
;   Delete a binding name from the hash table.
;
; :Params:
;       NAME:           in, required, type=string
;                       Name of the binding to be removed.
;-
pro MrBind::Delete_Binding, name
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Remove the binding name from the hash.    
    self.bindings -> Remove, name, FOUND=found
    if ~found then message, string(name, FORMAT='(%"Binding \"%s\" not found.")'), /INFORMATIONAL
end


;+
;   The purpose of this method is to count the number of named bindings that exist.
;
; :Returns:
;       nBindings:          The number of binding names that exist.
;-
function MrBind::Get_Names
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    names = self.bindings -> keys()

    return, names    
end


;+
;   Unbind one set of objects from all others. This method is private. Call the Bind
;   method with the /REMOVE keyword set.
;
; :Params:
;       BINDINGLIST:        in, required, type=linked list
;                           A linked list of various sets of object bindings from which
;                               `THEOBJECTS` will be removed.
;       THEOBJECTS:         in, required, type=objarr()
;                           The objects whose axes are to be unbound.
;-
pro MrBind::UnBind, name, theObjects
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;First get the named binding list
    bindingList = self.bindings -> Get(name, FOUND=found)
    if ~found then message, string(name, FORMAT='(%"Binding \"%s\" not found.")')

    ;How many sets of bindings exist for this name
    nBindings = bindingList -> Get_Count()
    
    ;Step through each binding set
    for i = 0, nBindings - 1 do begin
        thisBinding = bindingList -> Get_Item(i)
        void = ismember(theObjects, thisBinding, NONMEMBER_INDS=iKeep, N_NONMEMBERS=nKeep)
        
        ;If only one object will be left in this set of bindings, remove the entire set.
        ;Otherwise, trim out the unwanted bindings.
        if nKeep le 1 $
            then bindingList -> Delete, i $
            else bindingList -> Replace, thisBinding[iKeep], i
    endfor
end


;+
;   Print the heap variable numbers of the objects that are bound together.
;-
pro MrBind::whichBindings
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
;---------------------------------------------------------------------
;Number of Bindings //////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    names = self.bindings -> Keys(COUNT=nKeys)
    print, FORMAT='(%"Bindings: %i")', nKeys
    if nKeys eq 0 then return
    
;---------------------------------------------------------------------
;Step Through Names //////////////////////////////////////////////////
;---------------------------------------------------------------------

    for i = 0, nKeys - 1 do begin
        ;Print the name
        print, FORMAT='(%"  %s:")', names[i]
        
        ;Get the current binding
        thisBinding = self.bindings -> Get(names[i])
        
        ;How many sets of bindings are associated with this name?
        nSets = thisBinding -> Get_Count()

;---------------------------------------------------------------------
;Step Through Sets ///////////////////////////////////////////////////
;---------------------------------------------------------------------
        if nSets eq 0 then continue
        for j = 0, nSets - 1 do begin
            boundObjects = thisBinding -> Get_Item(i)
            print, string(FORMAT='(%"    %i: ")', j), boundObjects
        endfor
        
    endfor
end


;+
;   Clean up after the object is destroy
;-
pro MrBind::cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Destroy the hash table
    obj_destroy, self.bindings
end


;+
;   The initialization method.
;-
function MrBind::init
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Create a hash table. Make the keys strings and values objects.
    self.bindings = obj_new('MGcoHashTable', KEY_TYPE=7, VALUE_TYPE=11)
                         
    return, 1
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrBind__define, class
    compile_opt strictarr
    
    class = { MrBind, $
              bindings: obj_new() $      ;Linked list of object bindings
            }
end