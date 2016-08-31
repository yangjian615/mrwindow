; docformat = 'rst'
;
; NAME:
;   MrColorbar__Define
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
; PURPOSE:
;+
;   The purpose of this program is to create a color bar object that can drawn on a
;   data plot.
;
; :Categories:
;    Graphics
;    
; :Examples:
;   Create a simple image and a poorly located color bar::
;       image = dist(256)
;       cgImage, image, /Axes
;       cb = obj_new('MrColorbar', /VERTICAL, /DRAW)
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
;     Change History::
;       2015/10/05  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Calculate the position of the colorbar relative to the graphic by which it is placed.
;
; :Private:
;-
FUNCTION MrColorbar::CalcPosition
	Compile_Opt StrictArr

	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN, FltArr(4)
	ENDIF

	;Allocate memory
	cbPosition = FltArr(4)

	;Get the TARGET's position
	self.target -> GetProperty, POSITION=position, DEVICE=gDevice, NORMAL=gNorm

	;The default coordinate system is "normal".
	gNorm   = keyword_set(gNorm)
	gDevice = keyword_set(gDevice)
	if gNorm + gDevice eq 0 then gNorm = 1

	;Convert to Normal Coordinates
	IF gNorm EQ 0 THEN BEGIN
		position = self.target -> ConvertCoord(position, /TO_NORMAL, $
		                                       DEVICE = gDev, $
		                                       NORMAL = gNorm, $
		                                       DATA   = gData)
	ENDIF

	;Covnert from character units to normal coordinates.
	xcharsize = Double(!d.x_ch_size) / Double(!d.x_size) * self.charsize
	ycharsize = Double(!d.y_ch_size) / Double(!d.y_size) * self.charsize
	xoffset   = xcharsize * self.offset
	yoffset   = ycharsize * self.offset
	hwidth    = xcharsize * self.width
	vwidth    = ycharsize * self.width
	
	;Location of colorbar
	;   - Text to [left, bottom, right, top] ==> Put colorbar on [left, bottom, right, top]
	IF self.location EQ '' THEN BEGIN
		CASE 1 OF
			self.orientation EQ 1 && self.textpos EQ 1: location = 'RIGHT'
			self.orientation EQ 1 && self.textpos EQ 0: location = 'LEFT'
			self.orientation EQ 0 && self.textpos EQ 1: location = 'TOP'
			self.orientation EQ 0 && self.textpos EQ 0: location = 'BOTTOM'
			ELSE: message, 'Incorrect property values for ORIENTATION and LOCATION.'
		ENDCASE
	ENDIF ELSE BEGIN
		location = self.location
	ENDELSE

	;Vertical
	CASE location OF
		'TOP': BEGIN
			cbPosition[0] = position[0]
			cbPosition[1] = position[3] + yoffset
			cbPosition[2] = position[2]
			cbPosition[3] = position[3] + yoffset + vwidth
		ENDCASE
	
		'BOTTOM': BEGIN
			cbPosition[0] = position[0]
			cbPosition[1] = position[1] - yoffset - vwidth
			cbPosition[2] = position[2]
			cbPosition[3] = position[1] - yoffset
		ENDCASE
	
		'RIGHT': BEGIN
			cbPosition[0] = position[2] + xoffset
			cbPosition[1] = position[1]
			cbPosition[2] = position[2] + xoffset + hwidth
			cbPosition[3] = position[3]
		ENDCASE
	
		'LEFT': BEGIN
			cbPosition[0] = position[0] - xoffset - hwidth
			cbPosition[1] = position[1]
			cbPosition[2] = position[0] - xoffset
			cbPosition[3] = position[3]
		ENDCASE
	ENDCASE

	;Make sure the position fits within the window
	IF cbPosition[0] LT 0 OR cbPosition[2] GT 1 THEN Message, 'The colorbar does not fit within the window.', /INFORMATIONAL
	IF cbPosition[1] LT 0 OR cbPosition[3] GT 1 THEN Message, 'The colorbar does not fit within the window.', /INFORMATIONAL

	;Return the position
	return, cbPosition
END


;+
;   Calculate the position of the colorbar relative to the graphic by which it is placed.
;
; :Private:
;-
PRO MrColorbar::CreateBar
	Compile_Opt StrictArr
	on_error, 2
	
	;
	; TODO: Take colors from the PALETTE object property.
	;
	
	;Get the number of colors
	;   - The PALETTE object has exactly the number of colors we need.
	;   - The color indices start at 0 and ends at NCOLORS-1
	;   - There should be no need to manipulate the color arrangement
	self.palette -> GetProperty, NCOLORS=ncolors

	;Create the colorbar
	;   - Vertical   bars must be short in first dimension
	;   - Horizontal bars must be short in second dimension
	IF self.orientation $
		THEN bar = Rebin(BIndGen(1, ncolors), 20, ncolors) $
		ELSE bar = Rebin(BIndGen(ncolors), ncolors, 20)

	; Scale the color bar.
	IF N_Elements(*self.clamp) NE 0 THEN BEGIN
		;Clamp the colorbar between a specified data range.
		byterange = BytScl(clamp, MIN=self.range[0], MAX=self.range[1])
		tempbar   = BytScl(bar, TOP=(ncolors-1) < (255-bottom)) + bottom
		bar       = BytScl(bar, TOP=(ncolors-1) < (255-bottom), MIN=byterange[0], MAX=byterange[1]) + bottom 
		
		;Pick a neutral index for out of bounds values
		IF N_Elements(neutralIndex) EQ 0 THEN BEGIN
			neutralBottom = ncolors-1 < 255
			neutralTop    = bottom
		ENDIF ELSE BEGIN
			neutralBottom = neutralIndex
			neutralTop    = neutralIndex
		ENDELSE
		
		;Color values below the minimum
		i = Where(tempbar LT byterange[0], count)
		IF count GT 0 THEN bar[i] = neutralBottom
		
		;Color values above the maximum
		i = Where(tempbar GT byterange[1], count)
		IF count GT 0 THEN bar[i] = neutralTop
	ENDIF
	
	;Save the colorbar
	*self.bar = temporary(bar)
END


;+
; This method draws the color bar object. 
;-
PRO MrColorbar::Draw, $
NOERASE=noerase
	Compile_Opt StrictArr

	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL
		IF n_elements(table_in) GT 0 THEN tvlct, table_in
		MrPrintF, 'LogErr'
		RETURN
	ENDIF

	IF N_Elements(noerase) EQ 0 THEN noerase = *self.noerase

	; Set up PostScript device for working with colors.
	IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8

	;If a graphic or a cbLocation was given...
	IF Obj_Valid(self.target) THEN BEGIN
		;Update the range
		self.target -> GetProperty, RANGE=range, LOG=log
		self.range  = range
		self.log    = log
		
		;Determine the colorbar position
		IF Array_Equal(self.position, [0.0, 0.0, 0.0, 0.0]) $
			THEN position = self -> CalcPosition()
	ENDIF
	
	if n_elements(position) eq 0 then position = self.position
	
	;Load the colortable
	tvlct, table_in, /GET
	self.palette -> GetProperty, RGB_TABLE=rgb_table
	tvlct, rgb_table

	; Let's do this in decomposed color, if possible.
	cgSetColorState, 1, CURRENTSTATE=currentState

	;Draw the colorbar
	self -> doColorbar, position
	self -> doAxes, position
	IF self.taper NE 0 THEN self -> doTaper, position

	;Return to the old color state
	cgSetColorState, currentState
	
	tvlct, table_in
	self -> SaveCoords
END


;+
; This method draws the color bar object. 
;-
PRO MrColorbar::doAxes, position
	Compile_Opt StrictArr
	on_error, 2

	;Adjust postscript output.
	if !d.name eq 'PS' then begin
		tcharsize = MrPS_Rescale(self.tcharsize, /CHARSIZE)
		charsize  = MrPS_Rescale(self.charsize,  /CHARSIZE)
		charthick = MrPS_Rescale(self.charthick, /CHARTHICK)
		thick     = MrPS_Rescale(self.thick,     /THICK)
	endif else begin
		tcharsize = self.tcharsize
		charsize  = self.charsize
		charthick = self.charthick
		thick     = self.thick
	endelse

	;Convert color triple to color24
	color    = cgColor24(self.color)
	subtitle = cgCheckForSymbols(self.subtitle)
	title    = cgCheckForSymbols(self.title)
	ticklen  = self.tickdir ? -self.ticklen : self.ticklen

	;Draw a border?
	IF self.border THEN BEGIN
		xstyle   = 1
		ystyle   = 1
		IF self.discrete THEN BEGIN
			xticklen = 1.0
			yticklen = 1.0
		ENDIF
	ENDIF ELSE BEGIN
		xstyle   = 5
		ystyle   = 5
	ENDELSE

;-----------------------------------------------------
; Vertical \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; The basic idea is to establish a dataspace using PLOT
	; to which the color bar AXIS can attach itself. PLOT
	; also serves as the border of the colorbar.
	;
	; The axis opposite to the annotation axis (determined
	; by TEXTPOS) is left blank, with no tickmarks and no
	; annotations ([XYZ]TICK=0.001, [XYZ]TICKFORMAT='(a1)')
	;
	; PLOT is always used to draw the axis on the left or
	; bottom of the colorbar. If the annotations are on that
	; side, then PLOT is allowed to draw the annotations and
	; tickmarks while AXIS draws the opposite, blank axis.
	; PLOT will use [XYZ]STYLE=9 to leave an empty space for
	; AXIS.
	;
	; AXIS is always used to draw the top or right axis, and
	; is annotated only if TEXTPOS indicates it should be.
	;
	; If TITLE is on the same side of the axis as the
	; annotations, then AXIS or PLOT is allowed to draw it.
	; Otherwise, XYOUTS draws the title.
	;

	;VERTICAL
	IF self.orientation THEN BEGIN
		;ANNOTATE LEFT
		IF self.textpos EQ 1 THEN BEGIN
			;TITLE RIGHT
			IF self.tlocation EQ 'LEFT' THEN BEGIN
				PLOT, self.range, self.range, /NODATA, /NOERASE, $
				      COLOR         =       color, $
				      CHARSIZE      =       charsize, $
				      CHARTHICK     =       charthick, $
				      DATA          =  self.data, $
				      FONT          =  self.font, $
				      POSITION      =       position, $
				      XSTYLE        =       xstyle, $
				      XTHICK        =       thick, $
				      XTICKFORMAT   =       '(A1)', $
				      XTICKLAYOUT   = *self.ticklayout, $
				      XTICKS        =       1, $
				      XTITLE        =       '', $
				      XTICKLEN      =       xticklen, $
				      YLOG          =  self.log, $
				      YMINOR        =  self.minor, $
				      YTHICK        =       thick, $
				      YTICKFORMAT   =       '(A1)', $
				      YTICKNAME     = *self.tickname, $
				      YTICKS        =  self.major, $
				      YTICKINTERVAL = *self.tickinterval, $
				      YTICKLAYOUT   = *self.ticklayout, $
				      YTICKLEN      =       yticklen, $
				      YSTYLE        =       ystyle, $
				      YTITLE        =       ''
				
				;Left axis with title on right
				AXIS, YAXIS = 1, $
				      CHARSIZE      =       charsize, $
				      CHARTHICK     =       charthick, $
				      COLOR         =       color, $
				      FONT          =  self.font, $
				      SUBTITLE      =       subtitle, $
				      T3D           =  self.t3d, $
				      XTITLE        =       "", $
				      YLOG          =  self.log, $
				      YMINOR        =  self.minor, $
				      YRANGE        =  self.range, $
				      YSTYLE        =  self.style, $
				      YTHICK        =       thick, $
				      YTICKFORMAT   =  self.tickformat, $
				      YTICKINTERVAL = *self.tickinterval, $
				      YTICKLAYOUT   = *self.ticklayout, $
				      YTICKLEN      =       ticklen, $
				      YTICKNAME     = *self.tickname, $
				      YTICKS        =  self.major, $
				      YTICKUNITS    = *self.tickunits, $
				      YTICKV        = *self.tickvalues, $
				      YTITLE        =       ''

				;Location of the title on the side of the axis without annotations
				truecharsize = Float(!D.X_CH_SIZE * tcharsize) / !D.X_SIZE
				yloc         = (position[3] - position[1]) / 2.0 + position[1]
				xloc         = position[0] - (1.5 * truecharsize)
				
				;Draw the title
				XYOUTS, xloc, yloc, title, /NORMAL, $
				        ALIGNMENT   =      0.5, $
				        CHARSIZE    =      tcharsize, $
				        CHARTHICK   =      charthick, $
				        COLOR       =      color, $
				        FONT        = self.font, $
				        ORIENTATION =      -270
			
			;TITLE LEFT
			ENDIF ELSE BEGIN
				PLOT, self.range, self.range, /NODATA, /NOERASE, $
				      COLOR         =       color, $
				      CHARSIZE      =       charsize, $
				      CHARTHICK     =       charthick, $
				      DATA          =  self.data, $
				      FONT          =  self.font, $
				      POSITION      =       position, $
				      THICK         =  self.thick, $
				      XSTYLE        =       xstyle, $
				      XTHICK        =       thick, $
				      XTICKFORMAT   =       '(A1)', $
				      XTICKLAYOUT   = *self.ticklayout, $
				      XTICKLEN      =       xticklen, $
				      XTICKS        =       1, $
				      XTITLE        =       '', $
				      YLOG          =  self.log, $
				      YMINOR        =  self.minor, $
				      YSTYLE        =       ystyle, $
				      YTHICK        =       thick, $
				      YTICKFORMAT   =      '(A1)', $
				      YTICKNAME     = *self.tickname, $
				      YTICKS        =  self.major, $
				      YTICKINTERVAL = *self.tickinterval, $
				      YTICKLAYOUT   = *self.ticklayout, $
				      YTICKLEN      =       yticklen, $
				      YTITLE        =       ''

				;Draw the axis with title and annotations on same side
				AXIS, YAXIS =      1, $
				      CHARSIZE      =       charsize, $
				      CHARTHICK     =       charthick, $
				      COLOR         =       color, $
				      FONT          =  self.font, $
				      SUBTITLE      =       subtitle, $
				      T3D           =  self.t3d, $
				      XTITLE        =       "", $
				      YLOG          =  self.log, $
				      YMINOR        =  self.minor, $
				      YRANGE        =  self.range, $
				      YSTYLE        =  self.style, $
				      YTHICK        =       thick, $
				      YTICKFORMAT   =  self.tickformat, $
				      YTICKINTERVAL = *self.tickinterval, $
				      YTICKLAYOUT   = *self.ticklayout, $
				      YTICKLEN      =       ticklen, $
				      YTICKNAME     = *self.tickname, $
				      YTICKS        =  self.major, $
				      YTICKUNITS    = *self.tickunits, $
				      YTICKV        = *self.tickvalues, $
				      YTITLE        =       title
			ENDELSE

		;ANNOTATE RIGHT
		ENDIF ELSE BEGIN
			;TITLE RIGHT
			IF self.tlocation EQ 'RIGHT' THEN BEGIN
				;Establish data coordinate system
				PLOT, self.range, self.range, /NODATA, /NOERASE, $
				      COLOR         =       color, $
				      CHARSIZE      =       charsize, $
				      CHARTHICK     =       charthick, $
				      DATA          =  self.data, $
				      FONT          =  self.font, $
				      POSITION      =       position, $
				      XSTYLE        =       xstyle, $
				      XTHICK        =       thick, $
				      XTICKFORMAT   =       '(A1)', $
				      XTICKLAYOUT   = *self.ticklayout, $
				      XTICKLEN      =        xticklen, $
				      XTICKS        =       1, $
				      XTITLE        =       '', $
				      YLOG          =   self.log, $
				      YMINOR        =   self.minor, $
				      YSTYLE        =       ystyle, $
				      YTHICK        =       thick, $
				      YTICKFORMAT   =       '(A1)', $
				      YTICKNAME     = *self.tickname, $
				      YTICKS        =  self.major, $
				      YTICKINTERVAL = *self.tickinterval, $
				      YTICKLAYOUT   = *self.ticklayout, $
				      YTICKLEN      =       yticklen, $
				      YTITLE        =       ''

				;Left axis with title on left
				AXIS, YAXIS = 0, $
				      CHARSIZE      =       charsize, $
				      CHARTHICK     =       charthick, $
				      COLOR         =       color, $
				      FONT          =  self.font, $
				      SUBTITLE      =       subtitle, $
				      T3D           =  self.t3d, $
				      XTITLE        =       "", $
				      YLOG          =  self.log, $
				      YMINOR        =  self.minor, $
				      YRANGE        =  self.range, $
				      YSTYLE        =  self.style, $
				      YTHICK        =       thick, $
				      YTICKFORMAT   =  self.tickformat, $
				      YTICKINTERVAL = *self.tickinterval, $
				      YTICKLAYOUT   = *self.ticklayout, $
				      YTICKLEN      =       ticklen, $
				      YTICKNAME     = *self.tickname, $
				      YTICKS        =  self.major, $
				      YTICKUNITS    = *self.tickunits, $
				      YTICKV        = *self.tickvalues, $
				      YTITLE        =       ''

				truecharsize =      Float(!D.X_CH_SIZE * tcharsize) / !D.X_SIZE
				yloc =      (position[3] - position[1]) / 2.0 + position[1]
				xloc =      position[2] + (2.0 * truecharsize)
				
				XYOUTS, xloc, yloc, title, /NORMAL, $
				        COLOR       =      color, $
				        CHARTHICK   =      charthick, $
				        ALIGNMENT   =      0.5, $
				        FONT        = self.font, $
				        CHARSIZE    = self.tcharsize, $
				        ORIENTATION =      -270
			;TITLE LEFT
			ENDIF ELSE BEGIN
				PLOT, self.range, self.range, /NODATA, /NOERASE, $
				      COLOR         =      color, $
				      CHARSIZE      =      charsize, $
				      CHARTHICK     =      charthick, $
				      DATA          = self.data, $
				      FONT          = self.font, $
				      POSITION      =      position, $
				      XSTYLE        =      xstyle, $
				      XTHICK        =       thick, $
				      XTICKFORMAT   =      '(A1)', $
				      XTICKLAYOUT   = *self.ticklayout, $
				      XTICKLEN      =       xticklen, $
				      XTICKS        =      1, $
				      XTITLE        =      '', $
				      YLOG          =  self.log, $
				      YMINOR        =  self.minor, $
				      YSTYLE        =       ystyle, $
				      YTHICK        =       thick, $
				      YTICKFORMAT   =      '(A1)', $
				      YTICKNAME     = *self.tickname, $
				      YTICKS        =  self.major, $
				      YTICKINTERVAL = *self.tickinterval, $
				      YTICKLAYOUT   = *self.ticklayout, $
				      YTICKLEN      =       yticklen, $
				      YTITLE        =       ''
    
				AXIS, YAXIS = 0, $
				      CHARSIZE      =       charsize, $
				      CHARTHICK     =       charthick, $
				      COLOR         =       color, $
				      FONT          =  self.font, $
				      SUBTITLE      =       subtitle, $
				      T3D           =  self.t3d, $
				      XTITLE        =       "", $
				      YLOG          =  self.log, $
				      YMINOR        =  self.minor, $
				      YRANGE        =  self.range, $
				      YSTYLE        =  self.style, $
				      YTHICK        =       thick, $
				      YTICKFORMAT   =  self.tickformat, $
				      YTICKINTERVAL = *self.tickinterval, $
				      YTICKLAYOUT   = *self.ticklayout, $
				      YTICKLEN      =       ticklen, $
				      YTICKNAME     = *self.tickname, $
				      YTICKS        =  self.major, $
				      YTICKUNITS    = *self.tickunits, $
				      YTICKV        = *self.tickvalues, $
				      YTITLE        =       title
			ENDELSE
		ENDELSE

;-----------------------------------------------------
; Horizontal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	ENDIF ELSE BEGIN
		;ANNOTATE TOP
		IF self.textpos EQ 1 THEN BEGIN

			PLOT, self.range, self.range, /NODATA, /NOERASE, $
			      COLOR         =      color, $
			      CHARSIZE      =      charsize, $
			      CHARTHICK     =      charthick, $
			      DATA          = self.data, $
			      FONT          = self.font, $
			      POSITION      =      position, $
			      XMINOR        =  self.minor, $
			      XTHICK        =       thick, $
			      XTICKFORMAT   =      '(A1)', $
			      XTICKNAME     = *self.tickname, $
			      XTICKS        =  self.major, $
			      XTICKINTERVAL = *self.tickinterval, $
			      XTICKLAYOUT   = *self.ticklayout, $
			      XTICKLEN      =       xticklen, $
			      XLOG          =  self.log, $
			      XSTYLE        =       9, $
			      XTITLE        =       '', $
			      YSTYLE        =      Ystyle, $
			      YTHICK        =       thick, $
			      YTICKFORMAT   =      '(A1)', $
			      YTICKLAYOUT   = *self.ticklayout, $
			      YTICKLEN      =       yticklen, $
			      YTICKS        =      1, $
			      YTITLE        =      ''

			AXIS, XAXIS = 1, $
			      CHARSIZE      =       charsize, $
			      CHARTHICK     =       charthick, $
			      COLOR         =       color, $
			      FONT          =  self.font, $
			      SUBTITLE      =       subtitle, $
			      T3D           =  self.t3d, $
			      XLOG          =  self.log, $
			      XMINOR        =  self.minor, $
			      XRANGE        =  self.range, $
			      XSTYLE        =  self.style, $
			      XTHICK        =       thick, $
			      XTICKFORMAT   =  self.tickformat, $
			      XTICKINTERVAL = *self.tickinterval, $
			      XTICKLAYOUT   = *self.ticklayout, $
			      XTICKLEN      =       ticklen, $
			      XTICKNAME     = *self.tickname, $
			      XTICKS        =  self.major, $
			      XTICKUNITS    = *self.tickunits, $
			      XTICKV        = *self.tickvalues, $
			      XTITLE        =       '', $
			      YTITLE        =       ''
			
			IF title NE '' THEN BEGIN
				xloc = (position[2] - position[0]) / 2.0 + position[0]
				CASE self.tlocation OF
					'TOP': BEGIN
						truecharsize = Float(!D.Y_CH_SIZE * charsize) / !D.Y_SIZE
						yloc = position[3] + (2.25 * truecharsize)
					END
					'BOTTOM': BEGIN
						truecharsize = Float(!D.Y_CH_SIZE * tcharsize) / !D.Y_SIZE
						yloc = position[1] - (1.5 * truecharsize)
					END
					'RIGHT': Message, 'Illegal specification for title position: ' + self.tlocation
					'LEFT':  Message, 'Illegal specification for title position: ' + self.tlocation
					ELSE:    Message, 'Illegal specification for title position: ' + self.tlocation
				ENDCASE
				
				XYOUTS, xloc, yloc, title, /NORMAL, $
				        COLOR     =      color, $
				        CHARTHICK =      charthick, $
				        ALIGNMENT =      0.5, $
				        FONT      = self.font, $
				        CHARSIZE  = self.tcharsize
			ENDIF

		;ANNOTATE BOTTOM
		ENDIF ELSE BEGIN

			PLOT, self.range, self.range, /NODATA, /NOERASE, $
			      COLOR         =      color, $
			      CHARSIZE      =      charsize, $
			      CHARTHICK     =      charthick, $
			      DATA          = self.data, $
			      FONT          = self.font, $
			      POSITION      =      position, $
			      XMINOR        =  self.minor, $
			      XTHICK        =       thick, $
			      XTICKFORMAT   =      '(A1)', $
			      XTICKNAME     = *self.tickname, $
			      XTICKS        =  self.major, $
			      XTICKINTERVAL = *self.tickinterval, $
			      XTICKLAYOUT   = *self.ticklayout, $
			      XTICKLEN      =       xticklen, $
			      XLOG          =  self.log, $
			      XSTYLE        =       xstyle, $
			      XTITLE        =       '', $
			      YSTYLE        =      ystyle, $
			      YTHICK        =       thick, $
			      YTICKFORMAT   =      '(A1)', $
			      YTICKLAYOUT   = *self.ticklayout, $
			      YTICKLEN      =       yticklen, $
			      YTICKS        =      1, $
			      YTITLE        =      ''

			AXIS, XAXIS = 0, $
			      CHARSIZE      =       charsize, $
			      CHARTHICK     =       charthick, $
			      COLOR         =       color, $
			      FONT          =  self.font, $
			      SUBTITLE      =       subtitle, $
			      T3D           =  self.t3d, $
			      XLOG          =  self.log, $
			      XMINOR        =  self.minor, $
			      XRANGE        =  self.range, $
			      XSTYLE        =  self.style, $
			      XTHICK        =       thick, $
			      XTICKFORMAT   =  self.tickformat, $
			      XTICKINTERVAL = *self.tickinterval, $
			      XTICKLAYOUT   = *self.ticklayout, $
			      XTICKLEN      =       ticklen, $
			      XTICKNAME     = *self.tickname, $
			      XTICKS        =  self.major, $
			      XTICKUNITS    = *self.tickunits, $
			      XTICKV        = *self.tickvalues, $
			      XTITLE        =       '', $
			      YTITLE        =       ''

			IF title NE "" THEN BEGIN
				xloc = (position[2] - position[0]) / 2.0 + position[0]
				CASE self.tlocation OF
					'TOP': BEGIN
						truecharsize = Float(!D.Y_CH_SIZE * charsize) / !D.Y_SIZE
						yloc = position[3] + (0.75 * truecharsize)
					END
					'BOTTOM': BEGIN
						truecharsize = Float(!D.Y_CH_SIZE * charsize) / !D.Y_SIZE
						yloc = position[1] - (2.00 * truecharsize) - $
						(Float(!D.Y_CH_SIZE * tcharsize) / !D.Y_SIZE)
					END
					'RIGHT': Message, 'Illegal specification for title position: ' + self.tlocation
					'LEFT':  Message, 'Illegal specification for title position: ' + self.tlocation
					ELSE:    Message, 'Illegal specification for title position: ' + self.tlocation
				ENDCASE
				
				XYOUTS, xloc, yloc, title, /NORMAL, $
				        COLOR     =      color, $
				        CHARTHICK =      charthick, $
				        ALIGNMENT =      0.5, $
				        FONT      = self.font, $
				        CHARSIZE  = self.tcharsize
			ENDIF
		ENDELSE
	ENDELSE
END


;+
; This method draws the color bar object. 
;-
PRO MrColorbar::doColorbar, position
	Compile_Opt StrictArr
	on_error, 2

	
	; Are scalable pixels available on the device?
	IF (!D.Flags AND 1) NE 0 THEN scalablePixels = 1 ELSE scalablePixels = 0

	; Get starting locations in NORMAL coordinates.
	xstart = position[0]
	ystart = position[1]

	; Get the size of the bar in NORMAL coordinates.
	xsize = (position[2] - position[0])
	ysize = (position[3] - position[1])

	; Display the color bar in the window. Sizing is
	; different for PostScript and regular display.
	IF scalablePixels THEN BEGIN

		; Display the color bar.
		cgSetColorState, 0
		TV, *self.bar, xstart, ystart, XSIZE=xsize, YSIZE=ysize, /NORMAL
		cgSetColorState, 1 

	ENDIF ELSE BEGIN
		bar = CONGRID(*self.bar, CEIL(xsize*!D.X_VSize), CEIL(ysize*!D.Y_VSize))

		; Display the color bar.
		cgSetColorState, 0
		TV, bar, xstart, ystart, /Normal
		cgSetColorState, 1

	ENDELSE
END


;+
;   Calculate the position of the colorbar relative to the graphic by which it is placed.
;
; :Private:
;-
PRO MrColorbar::doTaper, position
	Compile_Opt StrictArr
	on_error, 2
	
	p = position
	;VERTICAL
	IF self.orientation THEN BEGIN
		;Taper Max
		IF self.taper eq 1 || self.taper eq 3 THEN BEGIN
			phalf = (p[2]-p[0])/2.0 + p[0]
			pdist = (p[2]-p[0]) * oob_factor
			PolyFill, [p[0], phalf, p[2], p[0]], $
			          [p[3], pdist+p[3], p[3], p[3]], /Normal, Color=cgColor(oob_high)
			PlotS, [p[0], phalf, p[2], p[0]], $
			       [p[3], pdist+p[3], p[3], p[3]], /Normal, Color=color
		ENDIF
		
		;Taper Min
		IF self.taper eq 1 || self.taper eq 2 THEN BEGIN
			phalf = (p[2]-p[0])/2.0 + p[0]
			pdist = (p[2]-p[0]) * oob_factor
			PolyFill, [p[0], phalf, p[2], p[0]], $
			          [p[1], p[1]-pdist, p[1], p[1]], /Normal, Color=cgColor(oob_low)
			PlotS, [p[0], phalf, p[2], p[0]], $
			       [p[1], p[1]-pdist, p[1], p[1]], /Normal, Color=color            
		ENDIF
	
	;HORIZONTAL
	ENDIF ELSE BEGIN
		;Taper Max
		IF self.taper eq 1 || self.taper eq 3 THEN BEGIN
			phalf = (p[3]-p[1])/2.0 + p[1]
			pdist = (p[3]-p[1]) * oob_factor
			PolyFill, [p[2], p[2]+pdist, p[2], p[2]], $
			          [p[1], phalf, p[3], p[1]], /Normal, Color=cgColor(oob_high)
			PlotS, [p[2], p[2]+pdist, p[2], p[2]], $
			       [p[1], +phalf, p[3], p[1]], /Normal, Color=color            
		ENDIF
		
		;Taper Min
		IF self.taper eq 1 || self.taper eq 2 THEN BEGIN
			phalf = (p[3]-p[1])/2.0 + p[1]
			pdist = (p[3]-p[1]) * oob_factor
			PolyFill, [p[0], p[0]-pdist, p[0], p[0]], $
			          [p[1], phalf, p[3], p[1]], /Normal, Color=cgColor(oob_low)
			PlotS, [p[0], p[0]-pdist, p[0], p[0]], $
			       [p[1], +phalf, p[3], p[1]], /Normal, Color=color            
		ENDIF
	 ENDELSE
END


;+
; This method obtains the current properties of the object. 
; 
; :Keywords:
;-
PRO MrColorbar::GetProperty, $
BORDER=border, $
CLAMP=clamp, $
DISCRETE=discrete, $
HIDE=hide, $
LOCATION=location, $
NEUTRAL_INDEX=neutral_index, $
OFFSET=offset, $
OOB_FACTOR=oob_factor, $
OOB_HIGH=oob_high, $
OOB_LOW=oob_low, $
ORIENTATION=orientation, $
PALETTE=palette, $
POSITION=position, $
RANGE=range, $
RGB_TABLE=rgb_table, $
TARGET=target, $
TAPER=taper, $
TCHARSIZE=tcharsize, $
TEXT_COLOR=text_color, $
TEXTPOS=textpos, $
TEXTTHICK=textthick, $
TICKDIR=tickdir, $
TLOCATION=tlocation, $
TRADITIONAL=traditional, $
WIDTH=width, $

;Axis Properties
CHARSIZE=charsize, $
CHARTHICK=charthick, $
COLOR=color, $
DATA=data, $
FONT=font, $
GRIDSTYLE=gridstyle, $
LOG=log, $
MAJOR=major, $
MINOR=minor, $
SUBTITLE=subtitle, $
STYLE=sytle, $
T3D=t3d, $
THICK=thick, $
TICKFORMAT=tickformat, $
TICKINTERVAL=tickinterval, $
TICKLAYOUT=ticklayout, $
TICKLEN=ticklen, $
TICKNAME=tickname, $
TICKUNITS=tickunits, $
TICKVALUES=tickvalues, $
TITLE=title, $
ZVALUE=zvalue, $
_REF_EXTRA=extra
	Compile_Opt StrictArr

	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN
	ENDIF

	;Superclass Keywords
	IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::GetProperty, _STRICT_EXTRA=extra

	;Colorbar Properties
	IF Arg_Present(border)        THEN border        = self.border
	IF Arg_Present(clamp)         THEN clamp         = self.clamp
	IF Arg_Present(discrete)      THEN discrete      = self.discrete
	IF Arg_Present(location)      THEN location      = self.location
	IF Arg_Present(neutral_index) THEN neutral_index = self.neutral_index
	IF Arg_Present(offset)        THEN offset        = self.offset
	IF Arg_Present(oob_factor)    THEN oob_factor    = self.oob_factor
	IF Arg_Present(oob_high)      THEN oob_high      = self.oob_high
	IF Arg_Present(oob_low)       THEN oob_low       = self.oob_low
	IF Arg_Present(orientation)   THEN orientation   = self.orientation
	IF Arg_Present(palette)       THEN palette       = self.palette
	IF Arg_Present(position)      THEN position      = self.position
	IF Arg_Present(range)         THEN range         = self.range
	IF Arg_Present(rgb_table)     THEN self.palette -> GetProperty, RGB_TABLE=rgb_table
	IF Arg_Present(taper)         THEN taper         = self.taper
	IF Arg_Present(tcharsize)     THEN tcharsize     = self.tcharsize
	IF Arg_Present(text_color)    THEN text_color    = self.text_color
	IF Arg_Present(textpos)       THEN textpos       = self.textpos
	IF Arg_Present(textthick)     THEN textthick     = self.textthick
	IF Arg_Present(tickdir)       THEN tickdir       = self.tickdir
	IF Arg_Present(tlocation)     THEN tlocation     = self.tlocation
	IF Arg_Present(width)         THEN width         = self.width
	IF Arg_Present(range)         THEN range         = self.range
	IF Arg_Present(reverse)       THEN reverse       = self.reverse
	IF Arg_Present(right)         THEN right         = self.right
	IF Arg_Present(textthick)     THEN texttick      = self.texthick
	IF Arg_Present(ticklen)       THEN ticklen       = self.ticklen
	IF Arg_Present(top)           THEN top           = self.top
	IF Arg_Present(vertical)      THEN vertical      = self.vertical
	IF Arg_Present(xlog)          THEN xlog          = self.xlog
	IF Arg_Present(ylog)          THEN ylog          = self.ylog

	;Axis properties
	IF Arg_Present(charsize)      NE 0 THEN charsize      =  self.charsize
	IF Arg_Present(charthick)     NE 0 THEN charthick     =  self.charthick
	IF Arg_Present(color)         NE 0 THEN color         =  self.color
	IF Arg_Present(data)          NE 0 THEN data          =  self.data
	IF Arg_Present(font)          NE 0 THEN font          =  self.font
	IF Arg_Present(gridstyle)     NE 0 THEN gridstyle     =  self.gridstyle
	IF Arg_Present(log)           NE 0 THEN log           =  self.log
	IF Arg_Present(neutral_index) NE 0 THEN neutral_index = *self. NEutral_index
	IF Arg_Present(major)         NE 0 THEN major         =  self.major
	IF Arg_Present(minor)         NE 0 THEN minor         =  self.minor
	IF Arg_Present(subtitle)      NE 0 THEN subtitle      =  self.subtitle
	IF Arg_Present(style)         NE 0 THEN style         =  self.style
	IF Arg_Present(t3d)           NE 0 THEN t3d           =  self.t3d
	IF Arg_Present(thick)         NE 0 THEN thick         =  self.thick
	IF Arg_Present(tickformat)    NE 0 THEN tickformat    = *self.tickformat
	IF Arg_Present(tickinterval)  NE 0 THEN tickinterval  = *self.tickinterval
	IF Arg_Present(ticklayout)    NE 0 THEN ticklayout    = *self.ticklayout
	IF Arg_Present(ticklen)       NE 0 THEN ticklen       =  self.ticklen
	IF Arg_Present(tickname)      NE 0 THEN tickname      = *self.tickname
	IF Arg_Present(tickunits)     NE 0 THEN tickunits     = *self.tickunits
	IF Arg_Present(tickvalues)    NE 0 THEN tickvalues    = *self.tickvalues
	IF Arg_Present(title)         NE 0 THEN title         =  self.title
	IF Arg_Present(zvalue)        NE 0 THEN zvalue        =  self.zvalue

	;Objects
	IF Arg_Present(target) THEN BEGIN
		IF Obj_Valid(self.target) $
			THEN target = self.target $
			ELSE target = Obj_New()
	ENDIF
END


;+
; This method sets the properties of the object.
;
; :Keywords:
;-
pro MrColorbar::SetProperty, $
BORDER=border, $
CLAMP=clamp, $
DISCRETE=discrete, $
LOCATION=location, $
NEUTRAL_INDEX=neutral_index, $
OFFSET=offset, $
OOB_FACTOR=oob_factor, $
OOB_HIGH=oob_high, $
OOB_LOW=oob_low, $
ORIENTATION=orientation, $
POSITION=position, $
RANGE=range, $
RGB_TABLE=rgb_table, $
TARGET=target, $
TAPER=taper, $
TCHARSIZE=tcharsize, $
TEXT_COLOR=text_color, $
TEXTPOS=textpos, $
TEXTTHICK=textthick, $
TICKDIR=tickdir, $
TLOCATION=tlocation, $
TRADITIONAL=traditional, $
WIDTH=width, $

;Axis Properties
CHARSIZE=charsize, $
CHARTHICK=charthick, $
COLOR=color, $
DATA=data, $
FONT=font, $
GRIDSTYLE=gridstyle, $
LOG=log, $
MAJOR=major, $
MINOR=minor, $
SUBTITLE=subtitle, $
STYLE=style, $
T3D=t3d, $
THICK=thick, $
TICKFORMAT=tickformat, $
TICKINTERVAL=tickinterval, $
TICKLAYOUT=ticklayout, $
TICKLEN=ticklen, $
TICKNAME=tickname, $
TICKUNITS=tickunits, $
TICKVALUES=tickvalues, $
TITLE=title, $
ZVALUE=zvalue, $
_REF_EXTRA=extra
	Compile_Opt StrictArr

	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN
	ENDIF
	
	;These properties require use to update the colorbar
	nrgb    = N_Elements(rgb_table)
	ntarget = n_elements(target)
	norient = N_Elements(orientation)

;-----------------------------------------------------
;Sync Colorbar with Graphic \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;If a TARGET was provided, it was so that the colorbar could use its
	;color palette and data range
	IF ntarget NE 0 THEN BEGIN
		IF Obj_Valid(target) THEN BEGIN
			;Grab the target's palette and range
			target -> GetProperty, PALETTE=palette, RANGE=range, LOG=log
			self.target  = target
			self.palette = palette
		ENDIF
	ENDIF

;-----------------------------------------------------
;Object Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Superclass
	IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra

	;Colorbar Properties
	IF N_Elements(border)        NE 0 THEN  self.border      = border
	IF N_Elements(clamp)         NE 0 THEN *self.clamp       = clamp
	IF N_Elements(discrete)      NE 0 THEN  self.discrete    = discrete
	IF N_Elements(location)      NE 0 THEN  self.location    = strupcase(location)
	IF N_Elements(offset)        NE 0 THEN  self.offset      = offset
	IF N_Elements(oob_factor)    NE 0 THEN  self.oob_factor  = oob_factor
	IF N_Elements(oob_high)      NE 0 THEN  self.oob_high    = cgColor(oob_high, /TRIPLE)
	IF N_Elements(oob_low)       NE 0 THEN  self.oob_low     = cgColor(oob_low,  /TRIPLE)
	IF norient                   NE 0 THEN  self.orientation = orientation
	IF N_Elements(position)      GT 0 THEN  self.position    = position
	IF N_Elements(range)         NE 0 THEN  self.range       = range
	IF N_Elements(taper)         NE 0 THEN  self.taper       = taper
	IF N_Elements(tcharsize)     NE 0 THEN  self.tcharsize   = tcharsize
	IF N_Elements(text_color)    NE 0 THEN  self.text_color  = cgColor(text_color, /TRIPLE)
	IF N_Elements(textpos)       NE 0 THEN  self.textpos     = textpos
	IF N_Elements(textthick)     NE 0 THEN  self.textthick   = textthick
	IF N_Elements(tickdir)       NE 0 THEN  self.tickdir     = tickdir
	IF N_Elements(tlocation)     NE 0 THEN  self.tlocation   = tlocation
	IF N_Elements(width)         NE 0 THEN  self.width       = width
	
	;Axis properties
	IF N_Elements(charsize)     NE 0 THEN  self.charsize     = charsize
	IF N_Elements(charthick)    NE 0 THEN  self.charthick    = charthick
	IF N_Elements(color)        NE 0 THEN  self.color        = cgColor(color, /TRIPLE)
	IF N_Elements(data)         NE 0 THEN  self.data         = data
	IF N_Elements(font)         NE 0 THEN  self.font         = font
	IF N_Elements(gridstyle)    NE 0 THEN  self.gridstyle    = gridstyle
	IF N_Elements(log)          NE 0 THEN  self.log          = log
	IF N_Elements(major)        NE 0 THEN  self.major        = major < 59
	IF N_Elements(minor)        NE 0 THEN  self.minor        = minor
	IF N_Elements(subtitle)     NE 0 THEN  self.subtitle     = subtitle
	IF N_Elements(style)        NE 0 THEN  self.style        = style + ((style and 1) eq 0)
	IF N_Elements(t3d)          NE 0 THEN  self.t3d          = t3d
	IF N_Elements(thick)        NE 0 THEN  self.thick        = thick
	IF N_Elements(tickformat)   NE 0 THEN  self.tickformat   = tickformat
	IF N_Elements(tickinterval) NE 0 THEN *self.tickinterval = tickinterval
	IF N_Elements(ticklayout)   NE 0 THEN *self.ticklayout   = ticklayout
	IF N_Elements(ticklen)      NE 0 THEN  self.ticklen      = ticklen
	IF N_Elements(tickname)     NE 0 THEN *self.tickname     = tickname
	IF N_Elements(tickunits)    NE 0 THEN *self.tickunits    = tickunits
	IF N_Elements(tickvalues)   NE 0 THEN *self.tickvalues   = tickvalues
	IF N_Elements(title)        NE 0 THEN  self.title        = title
	IF N_Elements(zvalue)       NE 0 THEN  self.zvalue       = zvalue

	;RGB_TABLE
	;   - Do not destroy a TARGET's palette.
	IF nrgb NE 0 THEN BEGIN
		;Set the color table
		IF obj_valid(self.target) THEN BEGIN
			self.target  = obj_new()
			self.palette = obj_new('MrColorPalette', rgb_table)
		ENDIF ELSE BEGIN
			self.palette -> SetProperty, RGB_TABLE=rgb_table
		ENDELSE
		
		;Get the number of colors
		self.palette -> GetProperty, NCOLORS=ncolors
		
		;Make sure the neutral index conforms
		IF N_Elements(*self.neutral_index) GT 0 $
			THEN IF *self.neutral_index GT ncolors-1 THEN self.neutral_index = ncolors-1
		
		;Check discrete colorbar
		IF self.discrete && self.major NE ncolors $
			THEN self.major = ncolors < 59
	ENDIF
	
	;NEUTRAL INDEX
	IF N_Elements(neutral_index) NE 0 THEN BEGIN
		self.palette -> GetProperty, NCOLORS
		if self.neutral_index gt ncolors-1 $
			then self.neutral_index = neutral_index $
			else message, 'NEUTRAL_INDEX is outside the color table range.', /INFORMATIONAL
	endif
	
	;ORIENTATION
	;   - Assist with TLOCATION
	IF N_Elements(orientation) GT 0 THEN BEGIN
		self.orientation = Keyword_Set(orientation)
		IF self.orientation EQ 1 THEN BEGIN
			IF self.tlocation EQ 'BOTTOM' || self.tlocation EQ 'TOP' $
				THEN self.tlocation = self.tlocation eq 'BOTTOM' ? 'LEFT' : 'RIGHT'
		ENDIF ELSE BEGIN
			IF self.tlocation EQ 'LEFT' || self.tlocation EQ 'RIGHT' $
				THEN self.tlocation = self.tlocation eq 'LEFT' ? 'BOTTOM' : 'TOP'
		ENDELSE
	ENDIF
	
	;COLORBAR
	IF ntarget GT 0 && Obj_Valid(target) || (nrgb + norient) GT 0 $
		THEN self -> CreateBar

	self.window -> Draw
END


;+
; The clean-up routine for the object. Destroy pointers, etc.
;-
PRO MrColorbar::cleanup
	Compile_Opt StrictArr

	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN
	ENDIF

	;Destroy the color palette only if it did not come from a target.
	IF ~Obj_Valid(self.target) THEN Obj_Destroy, self.palette

	;Free Pointers
	Ptr_Free, self.bar
	Ptr_Free, self.clamp
	Ptr_Free, self.neutral_index
	Ptr_Free, self.tickinterval
	Ptr_Free, self.ticklayout
	Ptr_Free, self.tickname
	Ptr_Free, self.tickunits
	Ptr_Free, self.tickvalues

	;Cleanup the superclasses
	self -> MrGrAtom::Cleanup
END


;+
;   The initialization method of the object. Called automatically when the object
;   is created. Note that several [X, Y, Z] Graphics Keywords are ignored in favor
;   of their axis-neutral names (sans [X, Y, Z]). See keyword descriptions below for
;   details.
;    
; :Keywords:
;       BORDER:             in, optional, type=boolean, default=0
;                           If set, draw a border around the colorbar. The default is
;                               to draw the colorbar and one axis.
;       CLAMP:              in, optional
;                           
;       DISCRETE:           in, optional, type=boolean, default=0
;                           If set, create discrete color blocks in the colorbar. Works
;                               best with a handful of colors (e.g. 8-15).
;       NAME:               in, optional, type=string, default='MRCOLORBAR'
;                           Name of the colorbar. Used to retrieve the graphic from
;                               the parent window object.
;       NEUTRAL_INDEX:      in, optional
;                           
;       OFFSET:             in, optional, type=float, default=1.5
;                           Offset of the colorbar from the edge of its `TARGET`, in
;                               character units. Used only when `POSITION` is determined
;                               automatically.
;       OOB_FACTOR:         in, optional
;                           
;       OOB_HIGH:           in, optional
;                           
;       OOB_LOW:            in, optional
;                           
;       ORIENTATION:        in, optional, type=integer, default=1
;                           Orientation of the colorbar. Choices are::
;                               0  -  Horizontal
;                               1  -  Vertical
;       LOCATION:           in, optional, type=string
;                           Location of the colorbar with respect to its `TARGET` when
;                               `POSITION` is undefined. Choices are::
;                                   'TOP'     -  Above the parent (horizontal colorbar)
;                                   'BOTTOM'  -  Below the parent (horizontal colorbar)
;                                   'LEFT'    -  To the right of the parent (vertical colorbar)
;                                   'RIGHT'   -  To the left  of the parent (vertical colorbar)
;                               'TOP' ('RIGHT') is the default for horizontal (vertical)
;                               colorbars.
;       POSITION:           in, optional, type=4x1 fltarr, default=fltarr(4)
;                           Position vector [x0, y0, x1, y1] specifying the location of
;                               lower left and upper-right corners of the colorbar. If all
;                               elements are 0.0, then the position is determined
;                               automatically from the `ORIENTATION`, `LOCATION`, `OFFSET`,
;                               and `WIDTH` keywords.
;
;
;    annotatecolor: in, optional, type=string, default="opposite"
;       The name of the "annotation color" to use. The names are those for
;       cgCOLOR. If this keyword is used, the annotation color is loaded after
;       the color bar is displayed. This keyword is provided to maintain backward 
;       compatibility, but also to solve the potential problem of an extra line showing up
;       in the color bar when the COLOR keyword is used in indexed color mode. In other words,
;       use ANNOTATECOLOR in place of COLOR for complete color model independent results.
;    bottom: in, optional, type=integer, default=0
;       The lowest color index of the colors to be loaded in the color bar.
;    brewer: in, optional, type=boolean, default=0
;         This keyword is used only if the `CTIndex` keyword is used to select a color table number.
;         Setting this keyword allows Brewer color tables to be used.
;    charpercent: in, optional, type=float, default=0.85                 
;       A value from 0.0 go 1.0 that is multiplied by the CHARSIZE to produce
;       the character size for the color bar. This value is only used if CHARSIZE is 
;       undefined. This keyword is primarily useful for using color bars in resizeable 
;       graphics windows (cgWindow).
;    charsize: in, optional, type=float
;       The character size of the color bar annotations. Default is cgDefCharsize()*charPercent.
;    clamp: in, optional, type=float
;        A two-element array in data units. The color bar is clamped to these
;        two values. This is mostly of interest if you are "window-leveling"
;        an image. The clamp is set to the "window" of the color bar.
;        Normally, when you are doing this, you would like the colors outside
;        the "window" to be set to a neutral color. Use the NEUTRALINDEX keyword
;        to set the netural color index in the color bar. (See the Examples section
;        for more information.)
;    color: in, optional, type=string
;        The name of the color to use for color bar annotations. Ignored unless passed 
;        the name of a cgColor color. The default value is to use the ANNOTATECOLOR.
;    ctindex: in, optional, type=integer
;         The index number of a color table. The `Brewer` and `Reverse` keywords will be checked
;         to see how to load the color table into the `Palette` keyword. This keyword will take
;         precidence over any colors that are loaded with the `Palette` keyword. 
;    discrete: in, optional, type=boolean, default=0
;         Set this keyword to configure certain properties of the color bar to make
;         discrete color blocks for the color bar. This works best if you are using
;         a handful of colors in the color bar (e.g, 8-16).
;    divisions: in, optional, type=integer, default=0
;         The number of divisions to divide the bar into. There will be (divisions + 1) annotations. 
;         When set to 0 (the default), the IDL Plot command detemines the number of divisions used.
;    draw: in, optional, type=boolean, default=0
;         Draw the color bar immediately.
;    fit: in, optional, type=boolean, default=0
;       If this keyword is set, the colorbar "fits" itself to the normalized
;       coordinates of the last graphics command executed. In other words, for
;       a horizontal color bar, postition[[0,2]] = !X.Window, and for a vertical
;       color bar, position[[1,3]] = !Y.Window. Other positions are adjusted
;       to put the colorbar "reasonably" close to the plot. The fit many not always
;       be accurate. If you are fitting to an image, be sure to set the SAVE keyword
;       on cgImage to establish a data coordinate system.
;    format: in, optional, type=string, default=""
;       The format of the color bar annotations. The default is to let the IDL Plot command 
;       determine how the color bar labels are formatted.
;    TARGET:            in, optional, type=object
;                       The graphic that the colorbar describes. If no target is given,
;                           all currently selected targets with a color palette will
;                           be given a colorbar. If no graphics are selected, and neither
;                           `CTINDEX` or `PALETTE` are given, no object will be created.
;                           If either of the color palette keywords are present, the
;                           colorbar will be placed in the current window.
;    invertcolors: in, optional, type=boolean, default=0
;       Setting this keyword inverts the colors in the color bar.
;    maxrange: in, optional
;       The maximum data value for the color bar annotation. Default is NCOLORS.
;    minrange: in, optional, type=float, default=0.0
;       The minimum data value for the bar annotation. 
;    minor: in, optional, type=integer, default=2
;       The number of minor tick divisions. 
;    ncolors: in, optional, type=integer, default=256
;       This is the number of colors in the color bar.
;    neutralindex: in, optional, type=integer   
;       This is the color index to use for color bar values outside the
;       clamping range when clamping the color bar with the CLAMP keyword.
;       If this keyword is absent, the highest color table value is used
;       for low range values and the lowest color table value is used
;       for high range values, in order to provide contrast with the
;       clamped region. (See the Examples section for more information.)
;    oob_factor: in, optional, type=float, default=1.0
;       The default is to make the length of the out-of-bounds triangle the
;       same distance as the height (or width, in the case of a vertical
;       color bar) of the color bar. If you would prefer a shorted triangle length, 
;       set this keyword to a value less than zero (e.g., 0.5). If you prefer a 
;       longer length, set this keyword to a value greater than zero. The "standard"
;       length will be multiplied by this value.
;    oob_high: in, optional, type=string
;       The name of an out-of-bounds high color. This color will be represented
;       by a triangle on the right or top of the color bar. If the color is
;       a string byte value (e.g., "215"), then this color in the current color
;       table is used. The color can also be a three-element color triple 
;       (e.g., [240, 200, 65]). Note, you can CANNOT use a long integer as
;       a color table index number with this keyword. If you want to use a 
;       color table index number, be sure the number is a short integer, byte
;       value, or a string (e.g, OOB_HIGH=200S, OOB_HIGH=200B, or OOB_HIGH='200').
;    oob_low: in, optional, type=string
;       The name of an out-of-bounds low color. This color will be represented
;       by a triangle on the left or bottom of the color bar. If the color is
;       a string byte value (e.g., "215"), then this color in the current color
;       table is used. The color can also be a three-element color triple 
;       (e.g., [240, 200, 65]). Note, you can CANNOT use a long integer as
;       a color table index number with this keyword. If you want to use a 
;       color table index number, be sure the number is a short integer, byte
;       value, or a string (e.g, OOB_HIGH=200S, OOB_HIGH=200B, or OOB_HIGH='200').
;    offset: in, optional, type=int/float, default=0.04
;       The offset, in normalized units, of the colorbar from the `graphic` by which
;       it is placed. Use only with the `graphic`.
;    palette: in, optional, type=byte
;       A color palette containing the RGB color vectors to use for the color
;       bar. The program will sample NCOLORS from the color palette. 
;    range: in, optional, type=float
;       A two-element vector of the form [min, max]. Provides an alternative 
;       and faster way way of setting the MINRANGE and MAXRANGE keywords.
;    reverse: in, optional, type=boolean, default=0
;       An alternative keyword name (one I can actually remember!) for the INVERTCOLORS keyword.
;       It reverses the colors in the color bar.
;    right: in, optional, type=boolean, default=0   
;       This puts the labels on the right-hand side of a vertical color bar. It applies 
;       only to vertical color bars.
;    tickinterval: in, optional, type=float
;       Set this keyword to the interval spacing of major tick marks. Use this keyword in
;       place of XTickInterval or YTickInterval keywords.
;    tickname: in, optional, type=string                 
;       A string array of names or values for the color bar tick marks. There should be
;       `divisions` + 1 tick names in the array.
;    tcharsize: in, optional, type=float
;       The title size. By default, the same as `Charsize`. Note that this keyword is
;       ignored for vertical color bars unless the title location (`TLocation`) is on
;       the opposite side of the color bar from the color bar labels. This is a consequence
;       of being upable to determine the length of color bar labels programmatically in this
;       orientation.
;    textthick: in, optional, type=float, default=1.0
;        Sets the thickness of the textual annotations on the color bar.
;    tlocation: in, optional, type=string
;       The title location, which allows the user to set the title location independently 
;       of the colorbar labels. May be "TOP" or "BOTTOM" for horizontal color bars, and
;       "LEFT" or "RIGHT" for vertical color bars.
;    top: in, optional, type=boolean, default=0
;       This puts the labels on top of the bar rather than under it. The keyword only 
;       applies if a horizontal color bar is rendered.
;    vertical: in, optional, type=boolean, default=0
;       Setting this keyword give a vertical color bar. The default is a horizontal color bar.
;    width: in, optional, type=int/float, default=0.08
;       Width of the colorbar in normalized units. Used only with the `graphic` keyword.
;    xlog: in, optional, type=boolean, default=0
;       Set this keyword to use logarithmic scaling for the colorbar data range.
;    ylog: in, optional, type=boolean, default=0
;       Set this keyword to use logarithmic scaling for the colorbar data range.
;    _ref_extra: in, optional
;         Any keyword appropriate for the cgColorBar command is also accepted by keyword
;         inheritance.
FUNCTION MrColorbar::INIT, $
;ALIGNMENT=alignemnt, $
;CHARPERCENT=charpercent, $
BORDER=border, $
CLAMP=clamp, $
DISCRETE=discrete, $
NAME=name, $
NEUTRAL_INDEX=neutral_ndex, $
OFFSET = offset, $
OOB_FACTOR=oob_factor, $
OOB_HIGH=oob_high, $
OOB_LOW=oob_low, $
ORIENTATION=orientation, $
LOCATION=location, $
POSITION=position, $
RANGE=range, $
TARGET=target, $
TAPER=taper, $
TCHARSIZE=tcharsize, $
TEXT_COLOR = text_color, $
TEXTPOS=textpos, $
TICKDIR=tickdir, $
TLOCATION=tlocation, $
WIDTH = width, $

;Color Table
BOTTOM    = bottom, $
BREWER    = brewer, $
CTINDEX   = ctindex, $
NCOLORS   = ncolors, $
RGB_TABLE = rgb_table, $
REVERSE   = reverse, $

;Axis Properties
CHARSIZE=charsize, $
CHARTHICK=charthick, $
COLOR=color, $
DATA=data, $
FONT=font, $
GRIDSTYLE=gridstyle, $
LOG=log, $
MAJOR=major, $
MINOR=minor, $
SUBTITLE=subtitle, $
STYLE=sytle, $
T3D=t3d, $
THICK=thick, $
TICKFORMAT=tickformat, $
TICKINTERVAL=tickinterval, $
TICKLAYOUT=ticklayout, $
TICKLEN=ticklen, $
TICKNAME=tickname, $
TICKUNITS=tickunits, $
TICKVALUES=tickvalues, $
TITLE=title, $
ZVALUE=zvalue, $
_REF_EXTRA=extra
	Compile_Opt StrictArr

	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN, 0
	ENDIF

;-----------------------------------------------------
; Target \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;If a target was not given, a palette must be given.
	if n_elements(rgb_table) eq 0 then begin
		if n_elements(target) eq 0 then begin
			target = self -> _GetTarget(/ALL, /ANY, COUNT=nTargets)
			if nTargets eq 0 then message, 'Insert MrColorbar failed. No targets available.'
		endif
	endif

;-----------------------------------------------------
; Superclasses \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Superclass. Use the target's window. If no target was given, get the current window.
	success = self -> MrGrAtom::INIT(TARGET=target, /CURRENT, NAME=name, HIDE=hide, WINREFRESH=refreshIn)
	if success eq 0 then message, 'Unable to initialize MrGrAtom'

;-----------------------------------------------------
; Validate Pointers \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	self.bar           = Ptr_New(/ALLOCATE_HEAP)
	self.clamp         = Ptr_New(/ALLOCATE_HEAP)
	self.neutral_index = Ptr_New(/ALLOCATE_HEAP)
	self.tickinterval  = Ptr_New(/ALLOCATE_HEAP)
	self.ticklayout    = Ptr_New(/ALLOCATE_HEAP)
	self.tickname      = Ptr_New(/ALLOCATE_HEAP)
	self.tickunits     = Ptr_New(/ALLOCATE_HEAP)
	self.tickvalues    = Ptr_New(/ALLOCATE_HEAP)

;-----------------------------------------------------
; Set Color Palette \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; If no RGB_TABLE was given, then TARGET must have been provided.
	; In this case, the SetProperty method will steal the color palette
	; from the target.
	;
	IF N_Elements(rgb_table) GT 0 THEN BEGIN
		self.palette = obj_new('MrColorPalette', rgb_table, $
		                                         BOTTOM    = bottom, $
		                                         BREWER    = brewer, $
		                                         FILENAME  = file, $
		                                         NCOLORS   = ncolors, $
		                                         REVERSE   = reverse)
		self.palette -> GetProperty, NCOLORS=ncolors
	ENDIF ELSE BEGIN
		target -> GetProperty, PALETTE=tpal
		tpal   -> GetProperty, NCOLORS=ncolors
	ENDELSE

;-----------------------------------------------------
; TARGET And LOCATION \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;If an object is provided, then its RANGE, CTINDEX, and PALETTE will
	;be adopted by the colorbar within the SetProperty method. Furthermore, it will
	;be assumed that you want the colorbar placed next to the graphic, so CBLOCATION
	;will default to 'RIGHT'
	;
	;In the case that CBLOCATION is not the empty string (''), e.g. when GRAPHIC is
	;provided, the colorbar will be positioned next to the last item displayed (or GRAPHIC,
	;if present). As such, RIGHT, TOP, TLOCATION, and VERTICAL must remain as-is (event
	;if they are undefined) until the SetProperty method is called below. Within SetProperty,
	;default values will be chosen for these quantities.
	;

	;Graphic object
	self.target = Obj_New()

	;CBLOCATION must also remain as-is if a graphic was provided.
	IF (N_Elements(target) EQ 0) || (Obj_Valid(target) EQ 0) THEN BEGIN
		SetDefaultValue, cbLocation, ''
		SetDefaultValue, top, 1, /BOOLEAN
		SetDefaultValue, vertical, 0, /BOOLEAN
		SetDefaultValue, right, 0, /BOOLEAN
		SetDefaultValue, tlocation, 'TOP'
	ENDIF

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	border        = Keyword_Set(border)
	discrete      = Keyword_Set(discrete)
	invertcolors  = Keyword_Set(invertcolors)
	log           = Keyword_Set(log)
	reverse       = Keyword_Set(reverse)
	orientation   = keyword_set(orientation)
	traditional   = keyword_set(traditional)
;	setDefaultValue, charpercent,   0.85
	setDefaultValue, charsize,      1.5
	setDefaultValue, color,         (traditional ? 'white' : 'black')
	setDefaultValue, font,          -1
	setDefaultValue, format,        ''
	setDefaultValue, location,      ''
	setDefaultValue, major,         0
	setDefaultValue, minor,         2
	setDefaultValue, oob_factor,    1.0
	setDefaultValue, oob_high,      'Antique White'
	setDefaultValue, oob_low,       'Charcoal'
	setDefaultValue, offset,        1.5
	setDefaultValue, position,      [0.0, 0.0, 0.0, 0.0]
	setDefaultValue, range,         [0, nColors]
	setDefaultValue, style,         1
	setDefaultValue, text_color,    color
	setDefaultValue, title,         ''
	setDefaultValue, ticklen,       0.25
	setDefaultValue, tcharsize,     charsize
	setDefaultValue, textthick,     1.0
	setDefaultValue, width,         1.5
	if n_elements(style) eq 0 then style = 1 else style = style + ((style and 1) eq 0)

	; Default to right (vertical) or top (horizontal)
	IF N_Elements(textpos) EQ 0 THEN textpos = 1
		
	; Title location
	IF N_Elements(tlocation) EQ 0 $
		THEN IF orientation EQ 1 THEN tlocation = (textpos EQ 1) ? 'RIGHT' : 'LEFT' $
		ELSE IF orientation EQ 0 THEN tlocation = (textpos EQ 1) ? 'TOP'   : 'BOTTOM'

	; A plot command limitation restricts the number of divisions to 59.
	major = major < 59 

	; If the user asked for discrete colors, set some keywords appropriately.
	; This really should not be used for more than 16 or colors, but I don't
	; want to limit it for the user. The maximum value is 59.
	IF discrete THEN BEGIN
		major   = ncolors < 59
		ticklen = 1.0
		minor   = 0
	ENDIF

	; You can't have a format set *and* use tickname.
	IF N_ELEMENTS(tickname) NE 0 THEN tickformat = ""

	; You can't specify both DIVISIONS and a tick interval, so fix that here.
	IF N_Elements(tickinterval) NE 0 THEN divisions = 0

;-----------------------------------------------------
; Set Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Will cause the BAR property to be created
	self -> SetProperty, BORDER        = border, $
	                     CLAMP         = clamp, $
	                     DISCRETE      = discrete, $
	                     NEUTRAL_INDEX = neutral_index, $
	                     OOB_FACTOR    = oob_factor, $
	                     OOB_HIGH      = oob_high, $
	                     OOB_LOW       = oob_low, $
	                     OFFSET        = offset, $
	                     ORIENTATION   = orientation, $
	                     LOCATION      = location, $
	                     POSITION      = position, $
	                     RANGE         = range, $
	                     TARGET        = target, $
	                     TAPER         = taper, $
	                     TCHARSIZE     = tcharsize, $
	                     TEXT_COLOR    = text_color, $
	                     TEXTPOS       = textpos, $
	                     TICKDIR       = tickdir, $
	                     TLOCATION     = tlocation, $
	                     WIDTH         = width, $
	                     
	                     CHARSIZE      = charsize, $
	                     CHARTHICK     = charthick, $
	                     COLOR         = color, $
	                     DATA          = data, $
	                     FONT          = font, $
	                     GRIDSTYLE     = gridstyle, $
	                     LOG           = log, $
	                     MAJOR         = major, $
	                     MINOR         = minor, $
	                     SUBTITLE      = subtitle, $
	                     STYLE         = style, $
	                     T3D           = t3d, $
	                     THICK         = thick, $
	                     TICKFORMAT    = tickformat, $
	                     TICKINTERVAL  = tickinterval, $
	                     TICKLAYOUT    = ticklayout, $
	                     TICKLEN       = ticklen, $
	                     TICKNAME      = tickname, $
	                     TICKUNITS     = tickunits, $
	                     TICKVALUES    = tickvalues, $
	                     TITLE         = title, $
	                     ZVALUE        = zvalue

	;Turn refresh back on?
	if n_elements(target) eq 0 $
		then self.window -> Refresh $
		else if refreshIn then self.window -> Refresh

	RETURN, 1
end


;+
; The class definition module for the object.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO MrColorbar__define, class
	
	class = { MrColorbar, $
	          inherits MrGrAtom, $
	         
	          ;MrColorbar Properties
	          bar:           Ptr_New(), $
	          target:        Obj_New(), $
	          palette:       Obj_New(), $
	         
	          ;Colorbar Properties
;	          annotatecolor: '', $
;	          charpercent:   0.0, $
	          border:        0B, $
	          clamp:         Ptr_New(), $
	          discrete:      0B, $
	          divisions:     0S, $
	          location:      '', $
	          neutral_index: Ptr_New(), $
	          offset:        0.0, $
	          oob_factor:    0.0, $
	          oob_high:      [0B, 0B, 0B], $
	          oob_low:       [0B, 0B, 0B], $
	          orientation:   0B, $
	          position:      [0.0, 0.0, 0.0, 0.0], $
	          range:         [0.0D, 0.0D], $
	          taper:         0B, $
	          tcharsize:     0.0, $
	          text_color:    [0B, 0B, 0B], $
	          textpos:       0B, $
	          tickdir:       0B, $
	          tlocation:     '', $
	          width:         0.0, $
	          
	          ;Axis properties
	          charsize:     0.0, $
	          charthick:    0.0, $
	          color:        [0B, 0B, 0B], $
	          data:         0B, $
	          font:         0S, $
	          gridstyle:    0B, $
	          log:          0B, $
	          major:        0S, $
	          minor:        0S, $
	          subtitle:     '', $
	          style:        0B, $
	          t3d:          0B, $
	          thick:        0.0, $
	          tickformat:   '', $
	          tickinterval: Ptr_New(), $
	          ticklayout:   Ptr_New(), $
	          ticklen:      0.0, $
	          tickname:     Ptr_New(), $
	          tickunits:    Ptr_New(), $
	          tickvalues:   Ptr_New(), $
	          title:        '', $
	          zvalue:       0.0 $
	        }
end