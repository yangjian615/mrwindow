; docformat = 'rst'
;
; NAME:
;       MrArrow
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
;   Create a weArrow object.
;
; :Params:
;       X0:             in, required, type=integer/float
;                       The x location of the blunt end of the arrow. May be a vector.
;                           Assumes device coordinates.
;       X1:             in, required, type=integer/float
;                       The x location of the sharp end of the arrow. May be a vector.
;                           Assumes device coordinates.
;       Y0:             in, required, type=integer/float
;                       The y location of the blunt end of the arrow. May be a vector.
;                           Assumes device coordinates.
;       Y1:             in, required, type=integer/float
;                       The y location of the sharp end of the arrow. May be a vector.
;                           Assumes device coordinates.
;
; :Keywords:
;       CURRENT:            in, optional, type=boolean, default=0
;                           If set, the plot will be added to the current MrWindow
;                               graphics window.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weArrow__define.
;
; :Returns:
;       THEARROW:           out, required, type=object
;                           A weArrow object reference.
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
;*****************************************************************************************
function MrCreateGraphic::Arrow, x0, y0, x1, y1, $
 CURRENT=current, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Create a cgOverPlot object
    theArrow = obj_new('weArrow', x0, y0, x1, y1, CURRENT=current, _STRICT_EXTRA=extra)
    
    return, theArrow
end
