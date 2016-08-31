; docformat = 'rst'
;
; NAME:
;       MrAxis_Examples
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
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
;   Examples of how to use MrImage__Define.
;
; :Examples:
;   See MrImage_Examples.pro for a series of examples::
;       IDL> void = MrImage_Examples()
;       IDL> win  = MrImage_Examples(1)
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
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/05/18  -   Written by Matthew Argall
;-
function MrAxis_Examples, example
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if obj_valid(gWin) then gWin -> Close
        MrPrintF, 'LogErr'
        return, obj_new()
    endif
    
    ;Print a description of each example
    if n_elements(example) eq 0 then begin
        print, [['EXAMPLE    DESCRIPTION'], $
                ['   1       Four simple axes']]
        return, -1
    endif

;---------------------------------------------------------------------
; Display a la TV Procedure //////////////////////////////////////////
;---------------------------------------------------------------------
    case example of
        1: begin
            ;Create the window
            gWin = MrWindow(OXMARGIN=[10, 10], OYMARGIN=[4, 4])
            gWin -> Refresh, /DISABLE
            
            ;Create a plot that will serve as the target
            ;   - Make axes invisible
            ;   - Do not include data
            gPlot = MrPlot([0], [0], /CURRENT, /NODATA, XSTYLE=4, YSTYLE=4)
            
            ;Creata set of 4 axes to outline the data
            ;   - The target will be selected automatically
            gAx1 = MrAxis('X', LOCATION='Bottom', TITLE='X Bottom')
            gAx2 = MrAxis('X', LOCATION='Top',    TITLE='X Top')
            gAx3 = MrAxis('Y', LOCATION='Left',   TITLE='Y Left')
            gAx4 = MrAxis('Y', LOCATION='Right',  TITLE='Y Right', COLOR='Red')
        endcase

;---------------------------------------------------------------------
; No More Examples ///////////////////////////////////////////////////
;---------------------------------------------------------------------
        else: message, 'EXAMPLE must be between 1 and 1.'
    endcase
    
    gWin -> Refresh
    return, gWin
end
