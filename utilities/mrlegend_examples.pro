; docformat = 'rst'
;
; NAME:
;       MrImage_Examples
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
;       IDL> void = MrImage_Examples()
;       IDL> win  = MrImage_Examples(14)
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
;   Modification History::
;       2014/09/09  -   Written by Matthew Argall
;       2015/01/05  -   Added example 2. - MRA
;-
function MrLegend_Examples, example
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
		        ['   1       Recreate ExcellisVIS example: http://exelisvis.com/docs/LEGEND.html'], $
		        ['   2       Horizontal legend with different sample angles, widths, and colors.']]
		return, -1
	endif

	case example of
;---------------------------------------------------------------------
; Recreate ExcelisVIS Example ////////////////////////////////////////
;---------------------------------------------------------------------
		1: begin
			;Create the data
			seed     = 5
			theory   = sin(2.0 * findgen(201) * !PI / 25.0) * exp(-0.02 * findgen(201))
			observed = theory + RANDOMU(seed,201)*0.4-0.2
 
			;Display the first plot.
			plot1 = MrPlot(observed, COLOR='Blue', NAME='Observed')
 
			;Display the second plot.
			plot2 = MrPlot(theory, OVERPLOT=plot1, COLOR='Red', NAME='Theory')
 
			;Add the legend.
			leg = MrLegend(TARGET   = [plot1, plot2], $
			               POSITION = [185, 0.9], $
			               /DATA, $
			               /AUTO_TEXT_COLOR)

			;Return the window
			win = plot1.window
		endcase
		
;---------------------------------------------------------------------
; Showcase Legend Item Properties ////////////////////////////////////
;---------------------------------------------------------------------
		2: begin
			;Create the data
			data = cgDemoData(14)
			time = findgen(n_elements(data[0,*]))
			
			;Create a window
			win = MrWindow(YGAP=0.5)
			
			;Properties
			psym       = -[1, 2, 4]
			sym_color  = ['Magenta', 'Cyan', 'Black']
			sangle     = [0, 45, 90]
			scolor     = ['Blue', 'Forest Green', 'Red']
			swidth     = [5.0, 10.0, 7.0]
			slinestyle = [1, 2, 3]
 
			;Display the plots.
			plot1 = MrPlot(data[0,*], $
			               /CURRENT, $
			               COLOR       = scolor[0], $
			               NAME        = 'X', $
			               PSYM        = psym[0], $
			               LINESTYLE   = MrLineStyle(slinestyle[0]), $
			               SYMCOLOR    = sym_color[0], $
			               TITLE       = 'Time Series Data', $
			               XTICKFORMAT = '(a1)', $
			               YTITLE      = 'X')
			               
			plot2 = MrPlot(data[1,*], $
			               /CURRENT, $
			               COLOR       = scolor[1], $
			               NAME        = 'Y', $
			               PSYM        = psym[1], $
			               LINESTYLE   = MrLineStyle(slinestyle[1]), $
			               SYMCOLOR    = sym_color[1], $
			               XTICKFORMAT = '(a1)', $
			               YTITLE      = 'Y')
			               
			plot3 = MrPlot(data[2,*], $
			               /CURRENT, $
			               COLOR     = scolor[2], $
			               NAME      = 'Z', $
			               PSYM      = psym[2], $
			               LINESTYLE = MrLineStyle(slinestyle[2]), $
			               SYMCOLOR  = sym_color[2], $
			               XTITLE    = 'Time (s)', $
			               YTITLE    = 'Z')
 
			;Add the legend.
			leg = MrLegend(TARGET             = [plot1, plot2, plot3], $
			               HORIZONTAL_SPACING = 3.0, $
			               NAME               = 'Time Series Legend', $
			               ORIENTATION        = 1, $
			               POSITION           = [0.48, 0.9], $
			               SAMPLE_ANGLE       = sangle, $
			               SAMPLE_COLOR       = scolor, $
			               SAMPLE_LINESTYLE   = slinestyle, $
			               SAMPLE_WIDTH       = swidth, $
			               SYMBOL             = psym, $
			               SYM_COLOR          = sym_color, $
			               /SYM_CENTER, $
			               TEXT_COLOR         = 'Black')

			;Return the window
			win = plot1.window
		endcase

;---------------------------------------------------------------------
; No More Examples ///////////////////////////////////////////////////
;---------------------------------------------------------------------
		else: message, 'EXAMPLE must be between 1 and 2.'
	endcase

	return, win
end
