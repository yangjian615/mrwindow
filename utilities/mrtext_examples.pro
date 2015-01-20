; docformat = 'rst'
;
; NAME:
;       MrText_Examples
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
;   Examples of how to use MrText_Define.
;
; :Examples:
;   See MrText_Examples.pro for a series of examples::
;       IDL> void = MrText_Examples()
;       IDL> win  = MrText_Examples(1)
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
;   Modification History::
;       2015/01/19  -   Written by Matthew Argall
;-
function MrText_Examples, example
	compile_opt strictarr

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(win) then obj_destroy, win
		if obj_valid(p1)  then p1 -> Close
		void = cgErrorMSG()
		return, obj_new()
	endif

	;Print a description of each example
	if n_elements(example) eq 0 then begin
		print, [['EXAMPLE    DESCRIPTION'], $
		        ['   1       Create a Title for a Plot.'], $
		        ['   2       Multi-line text with border, background, and alignment.'], $
		        ['   3       Recreate ExcellisVIS example: http://exelisvis.com/docs/TEXT.html']]
		return, -1
	endif

	case example of
;---------------------------------------------------------------------
; Create a Title for a Plot ///////////////////////////////////////////
;---------------------------------------------------------------------
		1: begin
			x = 0.01*(FINDGEN(201))
			
			;Create plots
			p1 = MrPlot(x, EXPINT(1, x), THICK=2, YRANGE=[0,2])
			
			;Create text objects
			t1 = MrText(0.5, 0.93, 'Plot Title', TARGET=p1, CHARSIZE=3)
			
			;Return the window
			win = p1.window
		endcase
;---------------------------------------------------------------------
; Create a Title for a Plot ///////////////////////////////////////////
;---------------------------------------------------------------------
		2: begin
			x = 0.01*(FINDGEN(201))
			
			;Create plots
			p1 = MrPlot(x, EXPINT(1, x), THICK=2, YRANGE=[0,2])
			
			;Create text objects
			t1 = MrText(0.5, 0.5, ['One line of text.', 'Two lines of text', 'Three lines of text'], $
			            ALIGNMENT          = 0.5, $
			            BOX_LINESTYLE      = '-', $
			            BOX_COLOR          = 'Red', $
			            /FILL_BACKGROUND, $
			            FILL_COLOR         = 'Blue', $
			            TARGET             = p1, $
			            VERTICAL_ALIGNMENT = 0.5)
			
			;Return the window
			win = p1.window
		endcase
;---------------------------------------------------------------------
; Recreate ExcelisVIS Example ////////////////////////////////////////
;---------------------------------------------------------------------
		3: begin
			x = 0.01*(FINDGEN(201))
			
			;Create plots
			p1 = MrPlot(x, EXPINT(1, x), THICK=2, YRANGE=[0,2])
			p2 = MrPlot(x, EXPINT(2, x), THICK=2, COLOR='Red',   OVERPLOT=p1)
			p3 = MrPlot(x, EXPINT(3, x), THICK=2, COLOR='Green', OVERPLOT=p1)
			
			;Create text objects
			t1 = MrText(0.3, 1.6, $
			           '$\it E_n(z) = \int_{1}^{\infty} ' + $
			           'e^{-zt} t^{-n} dt, \Re(z)\geq  0$', $
			           /DATA, CHARSIZE=2, TARGET=p1)
			t2 = MrText(0.40, 0.80, 'n = 1', /DATA, TARGET=p1)
			t3 = MrText(0.22, 0.55, 'n = 2', /DATA, TARGET=p1)
			t4 = MrText(0.10, 0.20, 'n = 3', /DATA, TARGET=p1)
			
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
