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
;       WINID:          in, optional, type=string
;                       Either the name or the index of the window to be retrieved. The
;                           first window by this name will be returned.
;
; :Keywords:
;       CURRENT:        in, optional, type=boolean, default=0
;                       If set, only the current window will be returned. The default
;                           is to return all windows.
;       NAMES:          out, optional, type=string/strarr
;                       Names of all windows returned.
;       PRINT:          in, optional, type=boolean, defualt=0
;                       If set, information about each open window will be printed.
;                           All other keywords are ignored.
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2013/11/22  -   Written by Matthew Argall
;       2014/02/10  -   !MR_WINDOWS was turned into a container. Updated program. Added
;                           the PRINT keyword. - MRA
;-
function GetMrWindows, winID, $
CURRENT=current, $
NAMES=names, $
PRINT=print
	compile_opt strictarr
	on_error, 2

	;Make sure the array of windows exists
	defsysv, '!MR_WINDOWS', EXISTS=exists
	if exists eq 0 then return, obj_new()

	;How many windows are there?
	nWin = !MR_WINDOWS -> Count()
	if nWin eq 0 then begin
		if keyword_set(print) then print, 'No windows are currently open.'
		return, obj_new()
	endif

;-----------------------------------------------------
; Print \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if keyword_set(print) then begin
		!MR_WINDOWS -> PrintInfo
		result = obj_new()

;-----------------------------------------------------
; Current Window \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if keyword_set(current) then begin
		result = !MR_WINDOWS -> Get()
		names  = result -> GetName()

;-----------------------------------------------------
; By Name \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if MrIsA(winID, 'STRING') then begin
		result = !MR_WINDOWS -> FindByName(winID, COUNT=count)
		if count gt 1 then result = result[0]
		names = winID

;-----------------------------------------------------
; By Index \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if MrIsA(winID, /INTEGER) then begin
		;Obtain the windows
		result = !MR_WINDOWS -> Get(POSITION=winID, COUNT=count)
		if count gt 1 then result = result[0]
	
		;Get the window names
		names = strarr(count)
		for i = 0, count-1 do names[i] = result[i] -> GetName()
	
;-----------------------------------------------------
; All Windows \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;Retrieve all windows
		result = !MR_WINDOWS -> Get(/ALL, COUNT=count)
		
		;Get their names
		if arg_present(names) then begin
			names = strarr(count)
			for i = 0, count - 1 do names[i] = result[i] -> GetName()
		endif
	endelse

	return, result
end

