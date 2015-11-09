; docformat = 'rst'
;
; NAME:
;       MrVector_Examples
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
;   Examples of how to use MrVector__Define.
;
; :Examples:
;   See MrVector_Examples.pro for a series of examples::
;       IDL> void = MrVector_Examples()
;       IDL> win  = MrVector_Examples(1)
;
; :Params:
;       EXAMPLE:        in, required, type=int
;                       Index number of the example to be excecuted.
;
; :Returns:
;       WIN:            A MrVector object reference.
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
;       2015/10/10  -   Written by Matthew Argall
;-
function MrVector_Examples, example
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
		        ['   1       Three Vectors with Axes'], $
		        ['   2       Van der Pol Oscillator']]
		return, -1
	endif

	case example of
;---------------------------------------------------------------------
; Van der Pol Oscillator /////////////////////////////////////////////
;---------------------------------------------------------------------
		1: begin
			;Create the data
			x = [-3, 0, 3]
			y = [3, 0, -3]
			vx = [ 3, 3,-3]
			vy = [-3, 0, 3]
			xrange = [-5,5]
			yrange = [-5,5]

			;Create a window
			win = MrWindow(REFRESH=0)

			;Vector plot
			v = MrVector(vx, vy, x, y, /CURRENT, $
			             DATA_LOCATION = 1, $
			             HEAD_SIZE     = 1.0, $
			             HEAD_INDENT   = 0.7, $
			             HEAD_ANGLE    = 15.0, $
			             XRANGE        = xrange, $
			             YRANGE        = yrange, $
			             XTITLE        = 'X', $
			             YTITLE        = 'Y', $
			             TITLE         = 'Van der Pol Oscillator - Phase Portrait')
			
			;Return the window
			win -> Refresh
		endcase
		
;---------------------------------------------------------------------
; Van der Pol Oscillator /////////////////////////////////////////////
;---------------------------------------------------------------------
		2: begin
			;Create the data
			n  = 21
			x  = 10*DINDGEN(n)/(n-1) - 5
			y  = 10*DINDGEN(n)/(n-1) - 5
			xx = REBIN(x, n, n)
			yy = REBIN(TRANSPOSE(y), n, n)
			mu = 1
			xdot = mu*(xx - xx^3/3 - yy)
			ydot = xx/mu

			;Create a window
			win = MrWindow(REFRESH=0)

			;Vector plot
			v = MrVector(xdot, ydot, x, y, /CURRENT, $
			             ARROW_THICK  = 2, $
			             LENGTH_SCALE = 2, $
			             XTITLE      = 'X', $
			             YTITLE      = 'Y', $
			             TITLE       = 'Van der Pol Oscillator - Phase Portrait')
			
			;Return the window
			win -> Refresh
		endcase

;---------------------------------------------------------------------
; No More Examples ///////////////////////////////////////////////////
;---------------------------------------------------------------------
		else: message, 'EXAMPLE must be between 1 and 2.'
	endcase

	return, win
end
