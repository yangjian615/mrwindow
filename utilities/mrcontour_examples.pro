; docformat = 'rst'
;
; NAME:
;       MrContour_Examples
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
;   Examples of how to use MrContour__Define.
;
; :Examples:
;   See MrImage_Examples.pro for a series of examples::
;       IDL> void = MrContour_Examples()
;       IDL> win  = MrContour_Examples(14)
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
;       2015/09/27  -   Written by Matthew Argall
;-
function MrContour_Examples, example
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
		        ['   1       Basic Contour Plot'], $
		        ['   2       Filled Contour Plot'], $
		        ['   3       Filled Contour Plot with Missing Data'], $
		        ['   4       Contour Plot on Image']]
		return, -1
	endif

	case example of
;---------------------------------------------------------------------
; Basic Contour Plot /////////////////////////////////////////////////
;---------------------------------------------------------------------
		1: begin
			; Example Gaussian data.
			data = cgDemoData(26)

			; Set up variables for the contour plot. Normally, these values 
			; would be passed into the program as positional and keyword parameters.
			nLevels  = 10
			xtitle   = 'X Axis'
			ytitle   = 'Y Axis'
			title    = 'Basic Contour Plot'
			
			;Pick the contours
			minValue = Floor(Min(data))
			levels   = cgConLevels(data, NLEVELS=nLevels, MINVALUE=minValue)

			; Set up a "window" for the plot. The PostScript output will have
			; the same aspect ratio as the graphics window on the display.
			win = MrWindow(XSIZE=600, YSIZE=500, WINDOW_TITLE='Basic Contour Plot')

			; Draw the filled contour plot.
			c = MrContour( data, /CURRENT, $
			               NLEVELS = nLevels, $
			               LEVELS  = levels, $
			               COLOR   = 'purple', $
			               XTITLE  = xtitle, $
			               YTITLE  = ytitle, $
			               TITLE   = title )
			
			;Return the window
			return, win
		endcase
		
;---------------------------------------------------------------------
; Filled Contour Plot ////////////////////////////////////////////////
;---------------------------------------------------------------------
		2: begin
			; Example Gaussian data.
			data = cgDemoData(26)

			; Set up variables for the contour plot. Normally, these values 
			; would be passed into the program as positional and keyword parameters.
			nLevels    = 10
			xtitle     = 'X Axis'
			ytitle     = 'Y Axis'
			position   = [0.125, 0.125, 0.9, 0.800]
			cbposition = [0.125, 0.865, 0.9, 0.895]
			cbTitle    = 'Data Value'

			; Set up a "window" for the plot. The PostScript output will have
			; the same aspect ratio as the graphics window on the display.
			win = MrWindow(OYMARGIN=[4,6], XSIZE=600, YSIZE=500, WINDOW_TITLE='Filled Contour Plot')

			;Create a color palette
			cgLoadCT, 33, NCOLORS=nlevels, BOTTOM=1, CLIP=[30,255], RGB_TABLE=rgb_table
			rgb_indices = indgen(nlevels)

			;Contour Levels
			minValue = Floor(Min(data))
			maxValue = Ceil(Max(data))
			nLevels  = 10
			levels   = cgConLevels(data, NLevels=10, MinValue=minValue)
			
			;Create the contour plot
			c = MrContour( data, /CURRENT, $
			               /FILL, $
			               LEVELS      = levels, $
			               RANGE       = [minValue, maxValue], $
			               RGB_INDICES = rgb_indices, $
			               RGB_TABLE   = rgb_table, $
			               /OUTLINE, $
			               XTITLE      = xtitle, $
			               YTITLE      = ytitle )
			
			;Draw the color bar.
			cb = MrColorbar( BOTTOM     = 1, $
			                 LOCATION   = 'TOP', $
			                 WIDTH      = 1, $
			                 /DISCRETE, $
			                 TARGET     = c, $
			                 TLOCATION  = 'TOP', $
			                 TITLE      = cbTitle )

			;Return the window
			return, win
		endcase

;---------------------------------------------------------------------
; Filled Contour Plot with Missing Data //////////////////////////////
;---------------------------------------------------------------------
		3: begin
			; Example Gaussian data.
			data = cgDemoData(26)
			dims = Size(data, /DIMENSIONS)

			;Create 25 random missing data points, plus a few larger gaps
			missingIndices       = RandomU(-3L, 25) * dims[0] * dims[1]
			data[missingIndices] = !Values.F_NaN
			data[18:45,40:50]    = !Values.F_NaN
			data[75:78, 20:40]   = !Values.F_NaN

			; Set up variables for the contour plot. Normally, these values 
			; would be passed into the program as positional and keyword parameters.
			nLevels    = 10
			xtitle     = 'X Axis'
			ytitle     = 'Y Axis'
			position   = [0.125, 0.125, 0.9, 0.800]
			cbposition = [0.125, 0.865, 0.9, 0.895]
			cbTitle    = 'Data Value'

			; Set up a "window" for the plot. The PostScript output will have
			; the same aspect ratio as the graphics window on the display.
			win = MrWindow(OYMARGIN=[4,6], XSIZE=600, YSIZE=500, $
			               WINDOW_TITLE='Filled Contours With Missing Data')

			;Create a color palette
			cgLoadCT, 33, NCOLORS=nlevels, BOTTOM=1, CLIP=[30,255], RGB_TABLE=rgb_table
			rgb_indices = indgen(nlevels) + 1

			;Contour Levels
			minValue = Floor(Min(data, /NAN))
			maxValue = Ceil(Max(data, /NAN))
			nLevels  = 10
			levels   = cgConLevels(data, NLevels=10, MinValue=minValue)
			
			;Create the contour plot
			;   - Must use CELL_FILL so that individual pixels are ignored.
			c = MrContour( data, /CURRENT, $
			               /CELL_FILL, $
			               LEVELS      = levels, $
			               RANGE       = [minValue, maxValue], $
			               RGB_INDICES = rgb_indices, $
			               RGB_TABLE   = rgb_table, $
			               /OUTLINE, $
			               XTITLE      = xtitle, $
			               YTITLE      = ytitle )
			
			;Draw the color bar.
			cb = MrColorbar( BOTTOM     = 1, $
			                 LOCATION   = 'TOP', $
			                 WIDTH      = 1, $
			                 /DISCRETE, $
			                 NCOLORS    = nLevels, $
			                 TARGET     = c, $
			                 TLOCATION  = 'TOP', $
			                 TITLE      = cbTitle )

			;Return the window
			return, win
		endcase
		
;---------------------------------------------------------------------
; Contour Plot on Image //////////////////////////////////////////////
;---------------------------------------------------------------------
		4: begin
			; Example Gaussian data.
			data = cgDemoData(26)

			; Set up variables for the contour plot. Normally, these values 
			; would be passed into the program as positional and keyword parameters.
			nLevels    = 10
			xtitle     = 'X Axis'
			ytitle     = 'Y Axis'
			position   = [0.125, 0.125, 0.9, 0.800]
			cbposition = [0.125, 0.865, 0.9, 0.895]
			cbTitle    = 'Data Value'

			; Set up a "window" for the plot. The PostScript output will have
			; the same aspect ratio as the graphics window on the display.
			win = MrWindow(OYMARGIN=[4,6], XSIZE=600, YSIZE=500, WINDOW_TITLE='Contour Plot on Image')

			;Create a color palette
			cgLoadCT, 33, CLIP=[30,255], RGB_TABLE=rgb_table

			;Contour Levels
			minValue = Floor(Min(data))
			maxValue = Ceil(Max(data))
			nLevels  = 10
			levels   = cgConLevels(data, NLevels=10, MinValue=minValue)

			;Create the image
			im = MrImage( data, /CURRENT, $
			              /AXES, $
			              /SCALE, $
			              RGB_TABLE = rgb_table, $
			              RANGE     = [minValue, maxValue], $
			              XRANGE    = [10, 20], $
			              XTITLE    = xtitle, $
			              YRANGE    = [-100, 100], $
			              YTITLE    = ytitle )
			
			;Create the contour plot
			c = MrContour( data, /CURRENT, $
			               LEVELS   = levels, $
			               RANGE    = [minValue, maxValue], $
			               OVERPLOT = im, $
			               XTITLE   = xtitle, $
			               YTITLE   = ytitle )
			
			;Draw the color bar.
			cb = MrColorbar( BOTTOM    = 1, $
			                 LOCATION  = 'TOP', $
			                 WIDTH     = 1, $
			                 TARGET    = im, $
			                 TLOCATION = 'TOP', $
			                 TITLE     = cbTitle )

			;Return the window
			return, win
		endcase

;---------------------------------------------------------------------
; No More Examples ///////////////////////////////////////////////////
;---------------------------------------------------------------------
		else: message, 'EXAMPLE must be between 1 and 4.'
	endcase

	return, win
end
