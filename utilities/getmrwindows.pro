; docformat = 'rst'
;
; NAME:
;       GetMrWindows
;
;*****************************************************************************************
;   Copyright (c) 2013, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
;   The purpose of this program is to return available MrWindow objects.
;
; :Params:
;
; :Author:
;       Matthew Argall::
;		University of New Hampshire
;		Morse Hall, Room 113
;       8 College Rd.
;		Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2013/11/22  -   Written by Matthew Argall
;-
function GetMrWindows, name, $
CURRENT=current, $
NAMES=names
    compile_opt strictarr
    on_error, 2

    ;Make sure the array of windows exists
    defsysv, '!MrWindow_Array', EXISTS=exists
    if exists eq 0 then return, obj_new()
    
    ;How many windows are there?
    nWin = n_elements(*!MrWindow_Array)
    if nWin eq 0 then return, obj_new()

;-----------------------------------------------------
;Current Window \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if keyword_set(current) then begin
        result = (*!MrWindow_Array)[0]
        names = result -> GetName()

;-----------------------------------------------------
;By Name \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else if arg_present(name) then begin
        names = ''
        i = 0
        while temp_name eq '' and i lt nWin do begin
            if (*!MrWindow_Array)[i] -> GetName() eq name then begin
                result = (*!MrWindow_Array)[i]
                names = name
            endif
            i++
        endwhile
        
        ;If no match was found, then return a null object
        if names eq '' then result = obj_new()
        
;-----------------------------------------------------
;All Windows \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else begin
        result = *!MrWindow_Array
        if arg_present(names) then begin
            names = strarr(nWin)
            for i = 0, nWin - 1 do names[i] = result[i] -> GetName()
        endif
    endelse

    return, result
end

