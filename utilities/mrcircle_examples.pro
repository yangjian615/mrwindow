; docformat = 'rst'
;
; NAME:
;       MrPolygon_Examples
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
;   Examples of how to use MrPolygon__Define.
;
; :Examples:
;   See MrImage_Examples.pro for a series of examples::
;       IDL> void = MrPolygon_Examples()
;       IDL> win  = MrPolygon_Examples(2)
;
; :Params:
;       EXAMPLE:        in, required, type=int
;                       Index number of the example to be excecuted.
;
; :Returns:
;       WIN:            A MrWindow object reference.
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
;       2014/09/22  -   Written by Matthew Argall
;-
function MrCircle_Examples, example
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if obj_valid(p1) then p1 -> Close
        void = cgErrorMSG()
        return, obj_new()
    endif
    
    ;Print a description of each example
    if n_elements(example) eq 0 then begin
        print, [['EXAMPLE    DESCRIPTION'], $
                ['   1       Empty Axes with Three Circles.']]
        return, -1
    endif

;---------------------------------------------------------------------
; Begin Examples /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    case example of
    ;---------------------------------------------------------------------
    ; Exmpty Axes with Three Circles /////////////////////////////////////
    ;---------------------------------------------------------------------
        1: begin
            ;Create an empty set of axes.
            p1 = MrPlot(0, 0, /NODATA, XRANGE=[0,5], YRANGE=[0,5], $
                        TITLE='Test', XTITLE='Time', YTITLE='Data')

            ;Create circles
            r = [2.3, 1.0, 0.45]
            x_center = [2, 1, 4]
            y_center = [3, 0.5, 1]
            cc = MrCircle(r, x_center, y_center, TARGET=p1, /FILL_BACKGROUND, /DATA, $
                          FILL_COLOR=['Magenta', 'Turquoise', 'Indian Red'])
            
            ;Return the window
            win = p1.window
        endcase

;---------------------------------------------------------------------
; No More Examples ///////////////////////////////////////////////////
;---------------------------------------------------------------------
        else: message, 'EXAMPLE must be between 1 and 1.'
    endcase
    
    return, win
end
