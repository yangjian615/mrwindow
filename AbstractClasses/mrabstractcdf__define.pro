; docformat = 'rst'
;
; NAME:
;       MrPlotWindow__Define
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
;   The purpose of this method is to manage the creation, addition, and removal of plots.
;   Plots, if not given a position, will be fit into a 2D plotting grid that automatically
;   determines plot locations when additional plots are added or removed.
;
;   If a plot object 
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
;       Matthew Argall 2013, All rights reserved
;
; :History:
;	Modification History::
;       06/28/2013  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Add an colorbar object.
;
; :Params:
;       CDFOBJECTS:             in, optional, type=object/obj_arr
;                               The CDF objects to add.
;
;   :Keywords:
;       CLEAR:                  in, optional, type=boolean, default=0
;                               If set, clear all legend objects. New ones can be added
;                                   in same call.
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the cdf objects being cleared, replaced, or
;                                   removed. This will also close the file.
;       INDEX:                  in, optional, type=int, default=0
;                               The index at which to `REPLACE` or `REMOVE` legend objects.
;       REMOVE:                 in, optional, type=boolean, default=0
;                               If set, remove the legend object and `INDEX` from the list.
;       REPLACE:                in, optional, type=boolean, default=0
;                               If set, replace the object at `INDEX` instead of appending
;                                   to the end of the list.
;-
pro MrAbstractCDF::AddCDFs, cdfObjects, $
CLEAR = clear, $
DESTROY = destroy, $
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
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    SetDefaultValue, destroy, 1, /BOOLEAN
    clear = keyword_set(clear)
    remove = keyword_set(remove)
    replace = keyword_set(replace)

;---------------------------------------------------------------------
;Replace, Remove, Clear //////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Replace.
    if keyword_set(replace) then begin
        self -> ReplaceCDF, cdfObjects, index, DESTROY=destroy
        return
    endif
    
    ;Remove
    if keyword_set(remove) then begin
        self -> Remove, index, DESTROY=destroy
        return
    endif
    
    ;Clear
    if keyword_set(clear) then self -> Clear, DESTROY=destroy

;---------------------------------------------------------------------
;Add /////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If nothing was given to add, then return
    if n_elements(cdfObjects) eq 0 then begin
        if ptr_valid(self.cdf_objects) eq 0 then self.cdf_objects = ptr_new(/ALLOCATE_HEAP)
        return
    endif
        
    ;Add the objects
    if ptr_valid(self.cdf_objects) $
        then *self.cdf_objects = [*self.cdf_objects, cdfObjects] $
        else self.cdf_objects = ptr_new(cdfObjects)
end


;+
;   The purpose of this method is to clear all objects from the list of objects being
;   displayed.
;
;   :Keywords:
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being cleared.
;-
pro MrAbstractCDF::Clear, $
DESTROY = destroy
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
            
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Defaults
    SetDefaultValue, destroy, 1, /BOOLEAN
            
;---------------------------------------------------------------------
;Clear All ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Destroy the objects
    if keyword_set(destroy) then obj_destroy, *self.cdf_objects
    
    ;Renew the pointer
    ptr_free, self.cdf_objects
    self.cdf_objects = ptr_new(/ALLOCATE_HEAP)
end


;+
;   The purpose of this method is to open a new CDF file.
;
; :Params:
;       FILENAME:           in, optional, type=string
;                           the filename of the CDF file to be opened
;
; :Returns:
;       CDFOBJECT:          An object reference to the opened CDF file.
;-
function MrAbstractCDF::Open_CDF, filename
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if obj_valid(cdfObject) then obj_destroy, cdfObject
        void = error_message()
        return, obj_new()
    endif
    
    ;Open the CDF and add it to the list.
    cdfObject = obj_new('cdf_read', filename)
    return, cdfObject
end


;+
;   The purpose of this method is to get the filenames of all open files.
;
; :Returns:
;       FILENAMES:          An array of the filenames of all currently open CDF files.
;-
function MrAbstractCDF::getFilenames
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, ''
    endif
    
    nCDF = n_elements(*self.cdf_objects)
    if nCDF eq 0 then return, ''
    
    filenames = strarr(nCDF)
    for i = 0, nCDF do begin
        (*self.cdf_objects)[i] -> GetProperty, FILENAME=fname
        filenames[i] = fname
    endfor

    return, filenames
end


;+
;   Remove a CDF object from the list.
;
; :Params:
;
;   :Keywords:
;       DISPLAY_TYPE:           out, optional, type=string
;                               The type of plot to be made. Options are::
;                                   'TIME_SERIES'
;                                   '3D_SPECTROGRAM'
;       DISPLAY_DIM:            out, optional, type=string
;                               If `DISPLAYOBJ` is not of type 'TIME_SERIES', then this
;                                   is the dimension of `DISPLAYOBJ` that should be
;                                   displayed.
;       FILENAME:               in, out, optional, type=string
;                               The CDF file from which to read data. If not provided, a
;                                   dialog box will appear from which a file can be selected.
;       GROUP_LEADER:           in, optional, type=integer
;                               The group leader of the variable selection gui. Use only when
;                                   `VARIABLE` is undefined.
;       INDEX:                  in, optional, type=int
;                               The index of the CDF object from which to read data. If
;                                   not provided, a new CDF file will be opened.
;       OCOLORBAR:              out, optional, type=object
;                               If the data associated with `VARIABLE` is an image, then
;                                   this keyword will return a colorbar to go along with it.
;       VARIABLE:               in, out, optional, type=string
;                               The name of the variable whose data is to be plotted. If
;                                   not provided, a dialog box will appear from which one
;                                   can be selected.
;-
function MrAbstractCDF::Plot_CDF, $
DISPLAY_TYPE = display_type, $
DISPLAY_DIM = display_dim, $
FILENAME = filename, $
GROUP_LEADER = group_leader, $
INDEX = index, $
OCOLORBAR = oColorbar, $
VARIABLE = variable
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        
        if n_elements(struct) eq 0 then undefine, struct
        if obj_valid(displayObj) then obj_destroy, displayObj
        if obj_valid(oColorbar) then obj_destroy, oColorbar
        
        void = error_message()
        return, obj_new()
    endif
            
;---------------------------------------------------------------------
;Open File ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Check if the filename is already open
    if n_elements(filename) ne 0 then begin
        allFiles = self -> getFilenames()
        tf_open = isMember(allFiles, filename, A_INDICES=index)
    endif
    
    ;Get an existing CDF file?
    if n_elements(index) ne 0 then begin
        cdfObject = (*self.cdf_object)[index]
    
    ;Or create a new one?
    endif else begin
        cdfObject = obj_new('cdf_read', filename, GROUP_LEADER=group_leader)
        if obj_valid(cdfObject) eq 0 then return, obj_new()

        self -> AddCDFs, cdfObject
    endelse

;---------------------------------------------------------------------
;Collect Meta Data ///////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Open the cdf and create a structure for plotting.
    thePlot = cdfObject -> CDF_Read::Plot_CDF(VARIABLE=variable, GROUP_LEADER=group_leader, $
                                              DISPLAY_TYPE=display_type, DISPLAY_DIM=diplay_dim, $
                                              OCOLORBAR=oColorbar)
    
    if obj_valid(thePlot) eq 0 then begin
        if obj_valid(oColorbar) then obj_destroy, oColorbar
        return, obj_new()
    endif else return, thePlot
end


;+
;   Remove a CDF object from the list.
;
; :Params:
;       INDEX:                  in, optional, type=intarr(2\,N), default=[1\,1]
;                               The index location of the cdf object to replace
;
;   :Keywords:
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being replaced.
;-
pro MrAbstractCDF::Remove, index, $
DESTROY = destroy
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
            
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    nCDF = n_elements(*self.cdf_objects)
    setDefaultValue, destroy, 1, /BOOLEAN
    
    ;Return if there is nothing to remove
    if nCDF eq 0 then return
            
;---------------------------------------------------------------------
;Remove //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Which are being kept?
    all_inds = indgen(nCDF)
    void = isMember(index, all_inds, NONMEMBER_INDS=iKeep)
    
    ;If there are no objects being kept, clear them all
    if n_elements(iKeep) eq 0 then begin
        self -> Clear, DESTROY=destroy
    
    ;Otherwise, remove the unwanted ones
    endif else begin    
        ;Destroy the objects?
        if keyword_set(destroy) then obj_destroy, (*self.cdf_objects)[index]
        
        ;Remove them
        *self.cdf_objects = (*self.cdf_objects)[iKeep]
    endelse
 end


;+
;   Replace CDF objects in the list.
;
; :Params:
;       CDFOBJECTS:             in, optional, type=object/obj_arr
;                               The CDF object(s) to add to the list.
;       INDEX:                  in, optional, type=int/intarr, default=0
;                               The index of the CDF object(s) that `CDFOBJECTS` is going
;                                   to replace.
;
;   :Keywords:
;       DESTROY:                in, optional, type=boolean, default=1
;                               Destroy the objects being replaced.
;-
pro MrAbstractCDF::ReplaceCDF, cdfObjects, index, $
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
    SetDefaultValue, index, 0
    
    ;Destroy the plot object that is being replaced
    if keyword_set(destroy) then obj_destroy, (*self.cdf_objects)[index]
    
    ;Insert the new plot objects in those locations
    (*self.cdf_objects)[index] = cdfObjects
end


;+
;   This method provides a means of determining the index location, cdf_id, and filename
;   of each CDF file.
;-
pro MrAbstractCDF::whichCDF
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Allow the Superclass to have a space flush with the left of the display window
    Print, '  CDF files:'
    
    ;Print the color bar indices and titles
    FOR j = 0, N_Elements(*self.cdf_objects) - 1 DO BEGIN
        (*self.cdf_objects)[j] -> GetProperty, CDF_ID=cdf_id, FILENAME=filename
        index = String(j, FORMAT='(i3.3)')
        
        IF N_Elements(filename) EQ 0 THEN filename = ''
        cdf_id = String(cdf_id, FORMAT='(i0)')
        Print, '    Index: ' + index + '    CDF ID: ' + cdf_id + '    Filename: ' + filename
    ENDFOR
end


;+
;   Clean up after the object is destroy
;-
pro MrAbstractCDF::cleanup
    compile_opt idl2
    
    ;Destroy objects
    for i = 0, n_elements(*self.cdf_objects) - 1 do begin
        obj_destroy, (*self.cdf_objects)[i]
    endfor
    
    ;Free pointers
    ptr_free, self.cdf_objects
    
end


;+
;   The initialization method. Because MrAbstractCDF is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrAbstractPlot object will result
;   in an error.
;-
function MrAbstractCDF::init
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif

    ;Cause an error
    message, 'This is an abstract class and does not have an INIT method.'
    
    return, 0
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrAbstractCDF__define, class
    compile_opt idl2
    
    define = { MrAbstractCDF, $
               cdf_objects: ptr_new() $         ;Array of all open cdf objects
             }
end