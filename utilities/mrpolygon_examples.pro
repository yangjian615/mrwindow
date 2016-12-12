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
;       2014/09/21  -   Written by Matthew Argall
;-
function MrPolygon_Examples, example
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if obj_valid(p1) then p1 -> Close
        MrPrintF, 'LogErr'
        return, obj_new()
    endif
    
    ;Print a description of each example
    if n_elements(example) eq 0 then begin
        print, [['EXAMPLE    DESCRIPTION'], $
                ['   1       Single polygon fill.'], $
                ['   2       Multicolored vertices with Line Fill.'], $
                ['   3       Multiple polygons defined via CONNECTIVITY']]
        return, -1
    endif

;---------------------------------------------------------------------
; Begin Examples /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    case example of
    ;---------------------------------------------------------------------
    ; Single Polygon Gill ////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        1: begin
            ;Create the data
            x      = findgen(100)/99.0
            y      = sin(2*!pi*x/0.18)
            
            ;Create the polygon vertices
            dy     = randomu(5, 100)
            xverts = [x, reverse(x)]
            yverts = [y+dy, reverse(y)-dy]
            
            ;Plot the data and polygon
            p1 = MrPlot(x, y, TITLE='Test', XTITLE='Time', YTITLE='Data')
            pf = MrPolygon(xverts, yverts, TARGET=p1, FILL_COLOR='Orange')
            
            ;Change properties
            p1.yrange = [-2, 2]
            p1 -> Order, /BRING_TO_FRONT
            
            ;Return the window
            win = p1.window
        endcase
        
    ;---------------------------------------------------------------------
    ; Multicolored Vertices with Line Fill ///////////////////////////////
    ;---------------------------------------------------------------------
        2: begin
            ;Create the data
            x      = findgen(101)/100.0
            y      = cgDemoData(1)

            ;Create the polygon vertices
            dy     = randomu(5, 101)*5.0
            xverts = [x, reverse(x)]
            yverts = [y+dy, reverse(y)-dy]
            
            ;Plot the data and polygon
            p1 = MrPlot(x, y, TITLE='Test', XTITLE='Time', YTITLE='Data')
            pf = MrPolygon(xverts, yverts, TARGET=p1, /LINE_FILL, $
                           COLOR=['red', 'orange', 'yellow', 'green', 'blue', 'violet'], $
                           FILL_LINESTYLE=2, ORIENTATION=135, FILL_COLOR='Brown', $
                           PSYM='Filled Star', SYMCOLOR=['yellow', 'orange'])
            
            ;Return the window
            win = p1.window
        endcase
        
    ;---------------------------------------------------------------------
    ; Multiple Polygons Defined via CONNECTIVITY /////////////////////////
    ;---------------------------------------------------------------------
        3: begin
            ;Cartesian coordinates defining a circle of radius 1.
            x = cos(2*!pi*findgen(100)/99.0)
            y = sin(2*!pi*findgen(100)/99.0)

            ;Radii
            r1    = 2.3
            r2    = 1.0
            r3    = 0.45
            
            ;Centers
            p1    = [2,3]
            p2    = [1,0.5]
            p3    = [4,1]
            
            ;Connected polygons
            xpoly = [r1*x+p1[0], r2*x+p2[0], r3*x+p3[0]]
            ypoly = [r1*y+p1[1], r2*y+p2[1], r3*y+p3[1]]
            connectivity = [100, lindgen(100), 100, lindgen(100)+100, 100, lindgen(100)+200]
            
            ;Plot the data and polygon
            p1 = MrPlot(0, 0, /NODATA, TITLE='Test', XTITLE='Time', YTITLE='Data', $
                        XRANGE=[0,5], YRANGE=[0,5])
            pf = MrPolygon(xpoly, ypoly, CONNECTIVITY=connectivity, TARGET=p1, $
                           FILL_COLOR=['Magenta', 'Turquoise', 'Indian Red'])
            
            ;Return the window
            win = p1.window
        endcase

;---------------------------------------------------------------------
; No More Examples ///////////////////////////////////////////////////
;---------------------------------------------------------------------
        else: message, 'EXAMPLE must be between 1 and 3.'
    endcase
    
    return, win
end
