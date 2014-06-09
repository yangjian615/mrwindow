; docformat = 'rst'
;
; NAME:
;       MrPlotS
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
;   Create a MrPlotS object.
;
; :Params:
;       X:                  in, required, type=numeric
;                           A vector or scalar argument providing the X components of the
;                               points to be connected. If only one argument is specified,
;                               X must be an array of either two or three vectors
;                               (i.e., (2,*) or (3,*)). In this special case, X[0,*] are
;                               taken as the X values, X[1,*] are taken as the `Y` values,
;                               and X[2,*] are taken as the `Z` values.
;       Y:                  in, optional, type=numeric
;                           Y coordinate(s) of the points to be connected.
;       Z:                  in, optional, type=numeric
;                           Z coordinate(s) of the points to be connected.
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrPlotS__define.
;
; :Returns:
;       THEPLOTS:           out, required, type=object
;                           A MrPlotS object reference.
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
;       2013/11/27  -   Written by Matthew Argall.
;-
function MrPlotS, x, y, z, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Add to the current window?
    current = keyword_set(current)

    ;Create the plot
    thePlotS = obj_new('MrPlotS', x, y, z, _STRICT_EXTRA=extra)
    
    return, thePlotS
end
