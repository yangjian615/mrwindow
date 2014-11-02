; docformat = 'rst'
;
; NAME:
;       Examples_MrPlot
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
;   Examples of how to use MrImage__Define.
;
; :Examples:
;   See MrImage_Examples.pro for a series of examples::
;       IDL> void = Examples_MrImage()
;       IDL> win  = Examples_MrImage(4)
;
; :Params:
;       EXAMPLE:        in, required, type=int
;                       Index number of the example to be excecuted.
;
; :Returns:
;       WIN:            A MrImage object reference.
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
;       2014/11/01  -   Written by Matthew Argall
;-
function Examples_MrPlot, example
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if obj_valid(img) then img -> Close
        void = cgErrorMSG()
        return, obj_new()
    endif
    
    ;Print a description of each example
    if n_elements(example) eq 0 then begin
        print, [['EXAMPLE    DESCRIPTION'], $
                ['   1       Simple plot of x'], $
                ['   2       Simple plot of y vs. x'], $
                ['   3       Plot a set of vectors: 3xN'], $
                ['   4       Time series with error bars'], $
                ['   5       Overplot']]
        return, -1
    endif

;---------------------------------------------------------------------
; Display a la TV Procedure //////////////////////////////////////////
;---------------------------------------------------------------------
    case example of
    ;---------------------------------------------------------------------
    ; Simple Plot ////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        1: begin
            ;Create the data
            data = cgDemoData(1)
            
            ;Create the image
            plt = MrPlot(data, TITLE='My Data', XTITLE='Time (s)', YTITLE='Measurement')
            win = plt.window
        endcase
        
    ;---------------------------------------------------------------------
    ; Simple Plot of Y vs. X /////////////////////////////////////////////
    ;---------------------------------------------------------------------
        2: begin
            ;Create the data
            data = cgDemoData(1)
            time = lindgen(n_elements(data))
            
            ;Create the image
            plt = MrPlot(time, data, TITLE='My Data', XTITLE='Time (s)', YTITLE='Measurement')
            win = plt.window
        endcase
        
    ;---------------------------------------------------------------------
    ; Plot a Set of Vectors //////////////////////////////////////////////
    ;---------------------------------------------------------------------
        3: begin
            ;Create the data
            data = cgDemoData(14)
            time = lindgen(n_elements(data[0,*]))
            
            ;Create the image
            plt = MrPlot(time, data, $
                         DIMENSION = 2, $
                         TITLE     = 'My Data', $
                         XTITLE    = 'Time (s)', $
                         YTITLE    = 'Measurement')
            win = plt.window
        endcase
        
    ;---------------------------------------------------------------------
    ; Time Series with Error Bars ////////////////////////////////////////
    ;---------------------------------------------------------------------
        4: begin
            ;Create the data
            data = reform((cgDemoData(14))[0,*])
            nPts = n_elements(data)
            time = lindgen(nPts)
            xerr = replicate(1.0, nPts)
            yerr = replicate(1.0, nPts)
            
            ;Create the image
            plt = MrPlot(time, data, $
                         ERR_XPLUS  = xerr, $
                         ERR_XMINUS = xerr, $
                         ERR_YPLUS  = yerr, $
                         ERR_YMINUS = yerr, $
                         TITLE      = 'My Data', $
                         XTITLE     = 'Time (s)', $
                         YTITLE     = 'Measurement')
            win = plt.window
        endcase
        
    ;---------------------------------------------------------------------
    ; Overplot ///////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        5: begin
            ;Create the data
            data = cgDemoData(14)
            nPts = n_elements(data[0,*])
            time = lindgen(nPts)
            yrange = [min(data, MAX=dMax), dMax]
            
            ;Create the plot
            plt1 = MrPlot(time, reform(data[0,*]), $
                          NAME   = 'Vx', $
                          TITLE  = 'My Data', $
                          XTITLE = 'Time (s)', $
                          YRANGE = yrange, $
                          YTITLE = 'Measurement')
            
            ;Overplot the other two components
            plt2 = MrPlot(time, reform(data[1,*]), $
                          /CURRENT, $
                          COLOR    = 'Blue', $
                          NAME     = 'Vy', $
                          OVERPLOT = plt1)
            plt3 = MrPlot(time, reform(data[2,*]), $
                          /CURRENT, $
                          COLOR    = 'Red', $
                          NAME     = 'Vz', $
                          OVERPLOT = plt1)
                          
            ;Return the window
            win = plt1.window
        endcase

;---------------------------------------------------------------------
; No More Examples ///////////////////////////////////////////////////
;---------------------------------------------------------------------
        else: message, 'EXAMPLE must be between 1 and 5.'
    endcase
    
    return, win
end
