; docformat = 'rst'
;
; NAME:
;       MrColorbar_Examples
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
;   Examples of how to use MrColorbar__Define.
;
; :Examples:
;   See MrImage_Examples.pro for a series of examples::
;       IDL> void = MrContour_Examples()
;       IDL> win  = MrContour_Examples(1)
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
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015/10/05  -   Written by Matthew Argall
;-
function MrColorbar_Examples, example
	compile_opt strictarr

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(img) then img -> Close
		MrPrintF, 'LogErr'
		return, obj_new()
	endif

	;Print a description of each example
	if n_elements(example) eq 0 then begin
		print, [['EXAMPLE    DESCRIPTION'], $
		        ['   1       Vertical Colorbar']]
		return, -1
	endif

	case example of
;---------------------------------------------------------------------
; Vertical Colorbr ///////////////////////////////////////////////////
;---------------------------------------------------------------------
		1: begin
			;Get the data
			file = FILEPATH('surface.dat', SUBDIR=['examples','data'])
			data = READ_BINARY(file, DATA_DIMS=[350,450], DATA_TYPE=2, ENDIAN='little')
			
			;Create an Image
			im = MrImage(data, $
			             RGB_TABLE  = 4, $
			             POSITION   = [0.25,0.05,0.95,0.9], $
			             TITLE      = 'Maroon Bells')

			; Add a colorbar
			c = MrColorbar(TARGET      = im, $
			               ORIENTATION = 1, $
			               POSITION    = [0.15,0.05,0.19,0.9], $
			               TITLE       = 'Elevation (m)')

			; Change some properties
			c.TEXTPOS    = 0
			c.TICKDIR    = 1
			c.BORDER     = 1
			c.COLOR      = 'Blue'
			
			;Window to be returned
			win = im.window
		endcase

;---------------------------------------------------------------------
; No More Examples ///////////////////////////////////////////////////
;---------------------------------------------------------------------
		else: message, 'EXAMPLE must be between 1 and 1.'
	endcase

	return, win
end
