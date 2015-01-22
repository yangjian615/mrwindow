; docformat = 'rst'
;
; NAME:
;       MrLegend__Define
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
;   Create a legend.
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
;    Modification History::
;       2014/06/10  -   Written by Matthew Argall
;       2014/11/24  -   Shift the legend down by half a character size when a box is not
;                           being drawn so that LOCATION indicates the top of the first
;                           legend item, not its middle. - MRA
;       2014/12/22  -   Completely rewritten. Independent of cgLegendItem. Based on IDL's
;                           Legend() function. Includes MrLegend_Item objects, MARGINS
;                           and SYM* properties.
;       2015/01/15  -   RELATIVE is now independent of axis range (i.e. [max, min]). - MRA
;-
;*****************************************************************************************
;+
;   Draw the legend item.
;
; :Params:
;       X:          in, required, type=float
;                   X-coordinate of the upper-left corner of the legend item.
;       Y:          in, required, type=float
;                   Y-coordinate of the upper-left corner of the legend item.
;
; :Keywords:
;       HEIGHT:     out, optional, type=float
;                   Height of the legend item, in normalized coordinates.
;       WIDTH:      out, optional, type=float
;                   Width of the legend item, in normalized coordinates.
;-
PRO MrLegend_Item::Draw, x, y, $
HEIGHT=height, $
WIDTH=width
	Compile_Opt idl2
	
	;Character sizes
	x_char = Float(!D.X_Ch_Size) / !D.X_Size
	y_char = Float(!D.Y_Ch_Size) / !D.Y_Size

;-----------------------------------------------------
; Line Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Length and height of line
	length = self.sample_width * x_char * Cos(self.sample_angle * !dtor)
	height = self.sample_width * x_char * Sin(self.sample_angle * !dtor)

	;The minimum height of a legend item is one character height (heigth of text).
	;	- Shift the line down to the middle of the text.
	IF Abs(height) LT y_char $
		THEN y_shift = 0.5 * (y_char - Abs(height)) $
		ELSE y_shift = 0.0
	
	;Endpoints of line
	CASE 1 OF
		;Quadrant I
		self.sample_angle LE 90: BEGIN
			x0 = x                   ;Left
			y1 = y  - y_shift        ;Top
			y0 = y1 - Abs(height)    ;Bottom
			x1 = x0 + Abs(length)    ;Right
		ENDCASE
		
		;Quadrant II
		self.sample_angle LE 180: BEGIN
			x1 = x                   ;Left
			y1 = y  - y_shift        ;Top
			y0 = y1 - Abs(height)    ;Bottom
			x0 = x1 + Abs(length)    ;Right
		ENDCASE
		
		;Quadrant III
		self.sample_angle LE 270: BEGIN
			x1 = x                   ;Left
			y0 = y  - y_shift        ;Top
			y1 = y0 - Abs(height)    ;Bottom
			x0 = x1 + Abs(length)    ;Right
		ENDCASE
		
		;Quadrant IV
		self.sample_angle LE 360: BEGIN
			x0 = x                   ;Left
			y0 = y  - y_shift        ;Top
			y1 = y0 - Abs(height)    ;Bottom
			x1 = x0 + Abs(length)    ;Right
		ENDCASE
	ENDCASE
		
	
	;Center of line
	xLeft   = Min([x0, x1], MAX=xRight)
	yBottom = Min([y0, y1], MAX=yTop)
	xCenter = xLeft   + (xRight - xLeft)   / 2.0
	yCenter = yBottom + (yTop   - yBottom) / 2.0
	
;-----------------------------------------------------
; Item Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF self.auto_text_color $
		THEN self.target -> GetProperty, COLOR=text_color $
		ELSE text_color = *self.text_color

	;Convert to IDL-accepted value
	label            = cgCheckForSymbols(self.label)
	text_color       = cgColor(text_color)
	psym             = cgSymCat(*self.symbol)
	sample_color     = cgColor(*self.sample_color)
	sample_linestyle = MrLineStyle(*self.sample_linestyle)
	sym_color        = cgColor(*self.sym_color)
	
	;Convert thickensses and sizes
	IF !D.Name EQ 'PS' THEN BEGIN
		sample_thick = MrPS_ReScale(self.sample_thick, /THICK)
		sym_size     = MrPS_ReScale(self.sym_size,     /CHARSIZE)
		sym_thick    = MrPS_ReScale(self.sym_thick,    /CHARTHICK)
		text_size    = MrPS_ReScale(self.text_size,    /CHARSIZE)
		text_thick   = MrPS_ReScale(self.text_thick,   /CHARTHICK)
	ENDIF ELSE BEGIN
		sample_thick = self.sample_thick
		sym_size     = self.sym_size
		sym_thick    = self.sym_thick
		text_size    = self.text_size
		text_thick   = self.text_thick
	ENDELSE
	
;-----------------------------------------------------
; Draw \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Draw the line
	IF self.sample_width NE 0 && sample_linestyle NE 6 THEN BEGIN
		PlotS, [x0, x1], [y0, y1], $
		       /NORMAL, $
		       COLOR     = sample_color, $
		       LINESTYLE = sample_linestyle, $
		       THICK     = sample_thick
	ENDIF
	
	;Symbols
	IF psym NE 0 THEN BEGIN
		;If symbols are on the ends, add two 1/2 symbol widths.
		IF self.sym_center EQ 0 THEN BEGIN
			length += sym_size * x_char
			xsym    = [x0, x1]
			ysym    = [y0, y1]
		ENDIF ELSE BEGIN
			xsym    = [xCenter, xCenter]
			ysym    = [yCenter, yCenter]
		ENDELSE
		
		;Draw the symbols
		PlotS, xsym, ysym, $
		       COLOR    = sym_color, $
		       /NORMAL, $
		       PSYM     = psym, $
		       SYMSIZE  = sym_size, $
		       THICK    = sym_thick
	ENDIF
	
	;Draw the text to determine its width.
	x_shift = (self.sample_width EQ 0 ? 0.0 : 2.0) * x_char
	y_shift = 0.5 * y_char
	XYOutS, xRight + x_shift, yCenter - y_shift, label, $
	        /NORMAL, $
	        COLOR     = text_color, $
	        CHARSIZE  = text_size, $
	        WIDTH     = width
	        CHARTHICK = text_thick

	;Finalize height and width of legend item
	;	- Add space between line and text, plus text width
	;	- Each item is at least one character tall
	width += x_shift + Abs(length)
	height  = Abs(height) > y_char
END


;+
;   Retrieve properties of the object.
;
; :Keywords:
;       AUTO_TEXT_COLOR:        out, optional, type=boolean
;                               If set, `TEXT_COLOR` will be the color of `TARGET`.
;       LABEL:                  out, optional, type=string
;                               Legend item text.
;       TEXT_COLOR:             out, optional, type=string/short/long/bytarr(3)
;                               The text color's name, color table index, 24-bit color, or
;                                   RGB-triple.
;       TEXT_SIZE:              out, optional, type=float
;                               Multiplier of the default character size.
;       TEXT_THICK:             out, optional, type=integer
;                               Multiplier of the default text thickness.
;       TARGET:                 out, optional, type=object
;                               Graphic object that `LABEL` describes.
;       SAMPLE_ANGLE:           out, optional, type=float
;                               Angle at which to draw the model line.
;       SAMPLE_COLOR:           out, optional, type=string/short/long/bytarr(3)
;                               The line color's name, color table index, 24-bit color, or
;                                   RGB-triple.
;       SAMPLE_LINESTYLE:       out, optional, type=float
;                               Style with with to draw the line. Choices are::
;                                   0   '-'     "Solid
;                                   1   '.'     "Dot"
;                                   2   '--'    "Dash"
;                                   3   '-.'    "Dash_Dot"
;                                   4   '-:'    "Dash_Dot_Dot"
;                                   5   '__'    "Long_Dash"
;                                   6   ' '     "None"
;       SAMPLE_MAGNITUDE:       out, optional, type=float/string
;                               For vector graphics, the representative vector length. Can
;                                   be "Min", "Mean", or "Max", which will take the
;                                   minimum, mean, or maximum data sample for the length
;                                   of the vector.
;       SAMPLE_THICK:           out, optional, type=float
;                               Thickness of the sample line.
;       SAMPLE_WIDTH:           out, optional, type=float
;                               Length of the sample line.
;       SYMBOL:                 out, optional, type=integer/string
;                               Name or number of the symbol used to mark the sample line.
;       SYM_CENER:              out, optional, type=boolean
;                               If set, symbols will be centered on the sample line. The
;                                   defualt is to put a symbol on each end of the line.
;       SYM_SIZE:               out, optional, type=float
;                               Size of the symbols.
;       SYM_THICK:              out, optional, type=integer
;                               Thickness of the symbols.
;-
PRO MrLegend_Item::GetProperty, $
AUTO_TEXT_COLOR=auto_text_color, $
LABEL=label, $
TEXT_COLOR=text_color, $
TEXT_SIZE=text_size, $
TEXT_THICK=text_thick, $
TARGET=target, $
SAMPLE_ANGLE=sample_angle, $
SAMPLE_COLOR=sample_color, $
SAMPLE_LINESTYLE=sample_linestyle, $
SAMPLE_MAGNITUDE=sample_magnitude, $
SAMPLE_THICK=sample_thick, $
SAMPLE_WIDTH=sample_width, $
SYMBOL=psym, $
SYM_CENTER=sym_center, $
SYM_COLOR=sym_color, $
SYM_SIZE=sym_size, $
SYM_THICK=sym_thick
	Compile_Opt idl2
	On_Error, 2
	
	IF Arg_Present(auto_text_color)  GT 0 THEN auto_text_color  =  self.auto_text_color	
	IF Arg_Present(label)            GT 0 THEN label            =  self.label
	IF Arg_Present(sample_color)     GT 0 THEN sample_color     = *self.sample_color
	IF Arg_Present(sym_color)        GT 0 THEN sym_color        = *self.sym_color
	IF Arg_Present(sample_angle)     GT 0 THEN sample_angle     =  self.sample_angle
	IF Arg_Present(sample_linestyle) GT 0 THEN sample_linestyle = *self.sample_linestyle
	IF Arg_Present(sample_magnitude) GT 0 THEN sample_magnitude =  self.sample_magnitude
	IF Arg_Present(sample_thick)     GT 0 THEN sample_thick     =  self.sample_thick
	IF Arg_Present(sample_width)     GT 0 THEN sample_width     =  self.sample_width
	IF Arg_Present(psym)             GT 0 THEN psym             = *self.symbol
	IF Arg_Present(sym_center)       GT 0 THEN sym_center       =  self.sym_center
	IF Arg_Present(sym_size)         GT 0 THEN sym_size         =  self.sym_size
	IF Arg_Present(sym_thick)        GT 0 THEN sym_thick        =  self.sym_thick
	IF Arg_Present(target)           GT 0 THEN target           =  self.target
	IF Arg_Present(text_color)       GT 0 THEN text_color       = *self.text_color
	IF Arg_Present(text_size)        GT 0 THEN text_size        =  self.text_size
	IF Arg_Present(text_thick)       GT 0 THEN text_thick       =  self.text_thick
END


;+
;   Retrieve properties of the object.
;
; :Keywords:
;       AUTO_TEXT_COLOR:        in, optional, type=boolean
;                               If set, `TEXT_COLOR` will be the color of `TARGET`.
;       LABEL:                  in, optional, type=string
;                               Legend item text.
;       TEXT_COLOR:             in, optional, type=string/short/long/bytarr(3)
;                               The text color's name, color table index, 24-bit color, or
;                                   RGB-triple.
;       TEXT_SIZE:              in, optional, type=float
;                               Multiplier of the default character size.
;       TEXT_THICK:             in, optional, type=integer
;                               Multiplier of the default text thickness.
;       TARGET:                 in, optional, type=object
;                               Graphic object that `LABEL` describes.
;       SAMPLE_ANGLE:           in, optional, type=float
;                               Angle at which to draw the model line.
;       SAMPLE_COLOR:           in, optional, type=string/short/long/bytarr(3)
;                               The line color's name, color table index, 24-bit color, or
;                                   RGB-triple.
;       SAMPLE_LINESTYLE:       in, optional, type=float
;                               Style with with to draw the line. Choices are::
;                                   0   '-'     "Solid
;                                   1   '.'     "Dot"
;                                   2   '--'    "Dash"
;                                   3   '-.'    "Dash_Dot"
;                                   4   '-:'    "Dash_Dot_Dot"
;                                   5   '__'    "Long_Dash"
;                                   6   ' '     "None"
;       SAMPLE_MAGNITUDE:       in, optional, type=float/string
;                               For vector graphics, the representative vector length. Can
;                                   be "Min", "Mean", or "Max", which will take the
;                                   minimum, mean, or maximum data sample for the length
;                                   of the vector.
;       SAMPLE_THICK:           in, optional, type=float
;                               Thickness of the sample line.
;       SAMPLE_WIDTH:           in, optional, type=float
;                               Length of the sample line.
;       SYMBOL:                 in, optional, type=integer/string
;                               Name or number of the symbol used to mark the sample line.
;       SYM_CENER:              in, optional, type=boolean
;                               If set, symbols will be centered on the sample line. The
;                                   defualt is to put a symbol on each end of the line.
;       SYM_SIZE:               in, optional, type=float
;                               Size of the symbols.
;       SYM_THICK:              in, optional, type=integer
;                               Thickness of the symbols.
;-
PRO MrLegend_Item::SetProperty, $
AUTO_TEXT_COLOR=auto_text_color, $
LABEL=label, $
SAMPLE_ANGLE=sample_angle, $
SAMPLE_COLOR=sample_color, $
SAMPLE_LINESTYLE=sample_linestyle, $
SAMPLE_MAGNITUDE=sample_magnitude, $
SAMPLE_THICK=sample_thick, $
SAMPLE_WIDTH=sample_width, $
SYMBOL=psym, $
SYM_CENTER=sym_center, $
SYM_COLOR=sym_color, $
SYM_SIZE=sym_size, $
SYM_THICK=sym_thick, $
TARGET=target, $
TEXT_COLOR=text_color, $
TEXT_SIZE=text_size, $
TEXT_THICK=text_thick
	Compile_Opt idl2
	On_Error, 2
	
	;Set Properties
	IF N_Elements(auto_text_color)  GT 0 THEN  self.auto_text_color  = Keyword_Set(auto_text_color)	
	IF N_Elements(label)            GT 0 THEN  self.label            = label
	IF N_Elements(sample_color)     GT 0 THEN *self.sample_color     = sample_color
	IF N_Elements(sym_color)        GT 0 THEN *self.sym_color        = sym_color
	IF N_Elements(sample_angle)     GT 0 THEN  self.sample_angle     = sample_angle MOD 360.0
	IF N_Elements(sample_linestyle) GT 0 THEN *self.sample_linestyle = sample_linestyle
	IF N_Elements(sample_magnitude) GT 0 THEN  self.sample_magnitude = sample_magnitude
	IF N_Elements(sample_thick)     GT 0 THEN  self.sample_thick     = sample_thick
	IF N_Elements(sample_width)     GT 0 THEN  self.sample_width     = sample_width
	IF N_Elements(psym)             GT 0 THEN *self.symbol           = psym
	IF N_Elements(sym_center)       GT 0 THEN  self.sym_center       = sym_center
	IF N_Elements(sym_size)         GT 0 THEN  self.sym_size         = sym_size
	IF N_Elements(sym_thick)        GT 0 THEN  self.sym_thick        = sym_thick
	IF N_Elements(text_color)       GT 0 THEN *self.text_color       = text_color
	IF N_Elements(text_size)        GT 0 THEN  self.text_size        = text_size
	IF N_Elements(text_thick)       GT 0 THEN  self.text_thick       = text_thick

	;Set Target
	IF N_Elements(target) GT 0 THEN BEGIN
		IF Obj_Valid(target) $
			THEN self.target = target $
			ELSE Message, 'TARGET must be a valid object.'
	ENDIF
	
	;AUTO_TEXT_COLOR depends on TARGET
	IF N_Elements(auto_text_color)  GT 0 THEN  self.auto_text_color  = Keyword_Set(auto_text_color)	
	IF self.auto_text_color && Obj_Valid(self.target) EQ 0 THEN BEGIN
		Message, 'No valid target. Setting AUTO_TEXT_COLOR to 0.', /INFORMATIONAL
		self.auto_text_color = 0B
	ENDIF
END


;+
;   Clean up after the object is destroyed.
;-
PRO MrLegend_Item::Cleanup
	Ptr_Free, self.text_color
	Ptr_Free, self.sample_color
	Ptr_Free, self.sample_linestyle
	Ptr_Free, self.symbol
	Ptr_Free, self.sym_color
END


;+
;   Initialization method.
;
; :Keywords:
;       AUTO_TEXT_COLOR:        in, optional, type=boolean
;                               If set, `TEXT_COLOR` will be the color of `TARGET`.
;       LABEL:                  in, optional, type=string
;                               Legend item text.
;       TEXT_COLOR:             in, optional, type=string/short/long/bytarr(3)
;                               The text color's name, color table index, 24-bit color, or
;                                   RGB-triple.
;       TEXT_SIZE:              in, optional, type=float
;                               Multiplier of the default character size.
;       TEXT_THICK:             in, optional, type=integer
;                               Multiplier of the default text thickness.
;       TARGET:                 in, optional, type=object
;                               Graphic object that `LABEL` describes.
;       SAMPLE_ANGLE:           in, optional, type=float
;                               Angle at which to draw the model line.
;       SAMPLE_COLOR:           in, optional, type=string/short/long/bytarr(3)
;                               The line color's name, color table index, 24-bit color, or
;                                   RGB-triple.
;       SAMPLE_LINESTYLE:       in, optional, type=float
;                               Style with with to draw the line. Choices are::
;                                   0   '-'     "Solid
;                                   1   '.'     "Dot"
;                                   2   '--'    "Dash"
;                                   3   '-.'    "Dash_Dot"
;                                   4   '-:'    "Dash_Dot_Dot"
;                                   5   '__'    "Long_Dash"
;                                   6   ' '     "None"
;       SAMPLE_MAGNITUDE:       in, optional, type=float/string
;                               For vector graphics, the representative vector length. Can
;                                   be "Min", "Mean", or "Max", which will take the
;                                   minimum, mean, or maximum data sample for the length
;                                   of the vector.
;       SAMPLE_THICK:           in, optional, type=float
;                               Thickness of the sample line.
;       SAMPLE_WIDTH:           in, optional, type=float
;                               Length of the sample line.
;       SYMBOL:                 in, optional, type=integer/string
;                               Name or number of the symbol used to mark the sample line.
;       SYM_CENER:              in, optional, type=boolean
;                               If set, symbols will be centered on the sample line. The
;                                   defualt is to put a symbol on each end of the line.
;       SYM_SIZE:               in, optional, type=float
;                               Size of the symbols.
;       SYM_THICK:              in, optional, type=integer
;                               Thickness of the symbols.
;-
FUNCTION MrLegend_Item::Init, $
AUTO_TEXT_COLOR=auto_text_color, $
LABEL=label, $
SAMPLE_ANGLE=sample_angle, $
SAMPLE_COLOR=sample_color, $
SAMPLE_LINESTYLE=sample_linestyle, $
SAMPLE_MAGNITUDE=sample_magnitude, $
SAMPLE_THICK=sample_thick, $
SAMPLE_WIDTH=sample_width, $
SYMBOL=psym, $
SYM_CENTER=sym_center, $
SYM_COLOR=sym_color, $
SYM_SIZE=sym_size, $
SYM_THICK=sym_thick, $
TARGET=target, $
TEXT_COLOR=text_color, $
TEXT_SIZE=text_size, $
TEXT_THICK=text_thick
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		void = cgErrorMSG()
		RETURN, 0
	ENDIF
	
	auto_text_color = Keyword_Set(auto_text_color)
	
	;Check target for label and color
	IF Obj_Valid(target) THEN BEGIN
		target -> GetProperty, NAME=target_name, COLOR=target_color
		IF N_Elements(label) EQ 0 THEN label = target_name
		IF auto_text_color THEN BEGIN
			text_color   = target_color
			sample_color = target_color
			sym_color    = target_color
		ENDIF
	ENDIF ELSE BEGIN
		IF auto_text_color THEN BEGIN
			Message, 'No valid target. Setting AUTO_TEXT_COLOR to 0.', /INFORMATIONAL
			auto_text_color = 0B
		ENDIF
	ENDELSE
	
	;Defaults
	text_color   = MrDefaultColor(text_color,   NCOLORS=1)
	sample_color = MrDefaultColor(sample_color, NCOLORS=1, DEFAULT=text_color)
	sym_color    = MrDefaultColor(sym_color,    NCOLORS=1, DEFAULT=text_color)
	IF N_Elements(label)            EQ 0 THEN label            = 'Legend Item'
	IF N_Elements(sample_angle)     EQ 0 THEN sample_angle     = 0.0
	IF N_Elements(sample_linestyle) EQ 0 THEN sample_linestyle = 'Solid_Line'
	IF N_Elements(sample_magnitude) EQ 0 THEN sample_magnitude = 5
	IF N_Elements(sample_thick)     EQ 0 THEN sample_thick     = 1.0
	IF N_Elements(sample_width)     EQ 0 THEN sample_width     = 5
	IF N_Elements(psym)             EQ 0 THEN psym             = 'None'
	IF N_Elements(sym_center)       EQ 0 THEN sym_center       = 0B
	IF N_Elements(sym_size)         EQ 0 THEN sym_size         = 1.5
	IF N_Elements(sym_thick)        EQ 0 THEN sym_thick        = 1.0
	IF N_Elements(text_size)        EQ 0 THEN text_size        = 1.5
	IF N_Elements(text_thick)       EQ 0 THEN text_thick       = 1.0
	
	;Allocate Heap
	self.text_color       = Ptr_New(/ALLOCATE_HEAP)
	self.sample_color     = Ptr_New(/ALLOCATE_HEAP)
	self.sample_linestyle = Ptr_New(/ALLOCATE_HEAP)
	self.symbol           = Ptr_New(/ALLOCATE_HEAP)
	self.sym_color        = Ptr_New(/ALLOCATE_HEAP)
	
	;Set Object Properties
	self -> SetProperty, AUTO_TEXT_COLOR  = auto_text_color, $
	                     LABEL            = label, $
	                     TEXT_COLOR       = text_color, $
	                     TEXT_SIZE        = text_size, $
	                     TEXT_THICK       = text_thick, $
	                     TARGET           = target, $
	                     SAMPLE_ANGLE     = sample_angle, $
	                     SAMPLE_COLOR     = sample_color, $
	                     SAMPLE_LINESTYLE = sample_linestyle, $
	                     SAMPLE_MAGNITUDE = sample_magnitude, $
	                     SAMPLE_THICK     = sample_thick, $
	                     SAMPLE_WIDTH     = sample_width, $
	                     SYMBOL           = psym, $
	                     SYM_CENTER       = sym_center, $
	                     SYM_COLOR        = sym_color, $
	                     SYM_SIZE         = sym_size, $
	                     SYM_THICK        = sym_thick
	
	RETURN, 1
END


;+
; The class definition module for the object.
; 
; :Params:
;     CLASS:        out, optional, type=struct
;                   The class definition as a structure variable.
;
; :Fields:
;       AUTO_TEXT_COLOR:        If set, `TEXT_COLOR` will be the color of `TARGET`.
;       LABEL:                  Legend item text.
;       TEXT_COLOR:             Legend item color.
;       TEXT_SIZE:              Size of legend text.
;       TEXT_THICK:             Thickness of legend text
;       TARGET:                 Graphic object that `LABEL` describes.
;       SAMPLE_ANGLE:           Angle at which to draw the model line.
;       SAMPLE_COLOR:           Color of sample line.
;       SAMPLE_LINESTYLE:       Style of sample line.
;       SAMPLE_MAGNITUDE:       Magnitude of sample line (if a vector).
;       SAMPLE_THICK:           Thickness of the sample line.
;       SAMPLE_WIDTH:           Length of the sample line.
;       SYMBOL:                 Symbol to mark sample line.
;       SYM_CENER:              Center symbols on the line?
;       SYM_SIZE:               Size of the symbols.
;       SYM_THICK:              Thickness of the symbols.
;-
PRO MrLegend_Item__Define, class

	class = { MrLegend_Item, $
	          auto_text_color:  0B, $
	          label:            '', $
	          text_color:       Ptr_New(), $
	          text_size:        0.0, $
	          text_thick:       0.0, $
	          target:           Obj_New(), $
	          sample_angle:     0.0, $
	          sample_color:     Ptr_New(), $
	          sample_linestyle: Ptr_New(), $
	          sample_magnitude: 0.0, $
	          sample_thick:     0.0, $
	          sample_width:     0.0, $
	          symbol:           Ptr_New(), $
	          sym_center:       0B, $
	          sym_color:        Ptr_New(), $
	          sym_size:         0.0, $
	          sym_thick:        0.0 $
	        }
END

;+
;   Add an item to the legend.
;
; :Keywords:
;       AUTO_TEXT_COLOR:        in, optional, type=boolean
;                               If set, `TEXT_COLOR` will be the color of `TARGET`.
;       LABEL:                  in, optional, type=string
;                               Legend item text.
;       TEXT_COLOR:             in, optional, type=string/short/long/bytarr(3)
;                               The text color's name, color table index, 24-bit color, or
;                                   RGB-triple.
;       TEXT_SIZE:              in, optional, type=float
;                               Multiplier of the default character size.
;       TEXT_THICK:             in, optional, type=integer
;                               Multiplier of the default text thickness.
;       TARGET:                 in, optional, type=object
;                               Graphic object that `LABEL` describes.
;       SAMPLE_ANGLE:           in, optional, type=float
;                               Angle at which to draw the model line.
;       SAMPLE_COLOR:           in, optional, type=string/short/long/bytarr(3)
;                               The line color's name, color table index, 24-bit color, or
;                                   RGB-triple.
;       SAMPLE_LINESTYLE:       in, optional, type=float
;                               Style with with to draw the line. Choices are::
;                                   0   '-'     "Solid
;                                   1   '.'     "Dot"
;                                   2   '--'    "Dash"
;                                   3   '-.'    "Dash_Dot"
;                                   4   '-:'    "Dash_Dot_Dot"
;                                   5   '__'    "Long_Dash"
;                                   6   ' '     "None"
;       SAMPLE_MAGNITUDE:       in, optional, type=float/string
;                               For vector graphics, the representative vector length. Can
;                                   be "Min", "Mean", or "Max", which will take the
;                                   minimum, mean, or maximum data sample for the length
;                                   of the vector.
;       SAMPLE_THICK:           in, optional, type=float
;                               Thickness of the sample line.
;       SAMPLE_WIDTH:           in, optional, type=float
;                               Length of the sample line.
;       SYMBOL:                 in, optional, type=integer/string
;                               Name or number of the symbol used to mark the sample line.
;       SYM_CENER:              in, optional, type=boolean
;                               If set, symbols will be centered on the sample line. The
;                                   defualt is to put a symbol on each end of the line.
;       SYM_SIZE:               in, optional, type=float
;                               Size of the symbols.
;       SYM_THICK:              in, optional, type=integer
;                               Thickness of the symbols.
;-
PRO MrLegend::Add, $
AUTO_TEXT_COLOR=auto_text_color, $
LABEL=label, $
TEXT_COLOR=text_color, $
TEXT_SIZE=text_size, $
TEXT_THICK=text_thick, $
TARGET=target, $
SAMPLE_ANGLE=sample_angle, $
SAMPLE_COLOR=sample_color, $
SAMPLE_LINESTYLE=sample_linestyle, $
SAMPLE_MAGNITUDE=sample_magnitude, $
SAMPLE_THICK=sample_thick, $
SAMPLE_WIDTH=sample_width, $
SYMBOL=psym, $
SYM_CENTER=sym_center, $
SYM_COLOR=sym_color, $
SYM_SIZE=sym_size, $
SYM_THICK=sym_thick
	Compile_Opt idl2
	On_Error, 2

	;Create the legend item.
	item = Obj_New('MrLegend_Item', $
	               AUTO_TEXT_COLOR  = auto_text_color, $
	               LABEL            = label, $
	               SAMPLE_ANGLE     = sample_angle, $
	               SAMPLE_COLOR     = sample_color, $
	               SAMPLE_LINESTYLE = sample_linestyle, $
	               SAMPLE_MAGNITUDE = sample_magnitude, $
	               SAMPLE_THICK     = sample_thick, $
	               SAMPLE_WIDTH     = sample_width, $
	               SYMBOL           = psym, $
	               SYM_CENTER       = sym_center, $
	               SYM_COLOR        = sym_color, $
	               SYM_SIZE         = sym_size, $
	               SYM_THICK        = sym_thick, $
	               TARGET           = target, $
	               TEXT_COLOR       = text_color, $
	               TEXT_SIZE        = text_size, $
	               TEXT_THICK       = text_thick)
	
	;Add to container
	IF Obj_Valid(item) THEN self.items -> Add, item
END


;+
;   Helper function for ::Draw. Calculates position of the legend box.
;
;   The size of the legend contents cannot be determined until they are drawn. This method
;   draws a phantom legend in order to determine its size.
;-
PRO MrLegend::CalculateBoxSize
	Compile_Opt idl2

	; Catch the error.
	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL
		IF N_Elements(pixID) GT 0 && windowAvailable(pixID) EQ 1 THEN BEGIN
			WDelete, pixID
			WSet, currentID
		ENDIF
		IF N_Elements(currentState) NE 0 THEN cgSetColorState, currentState
		void = cgErrorMsg()
		RETURN
	ENDIF

;-----------------------------------------------------
; Corner of Reference \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	self -> GetProperty, 0, TARGET=target

	;Convert to normalized coordinates
	CASE 1 OF
		;DATA
		self.data: location = target -> ConvertCoord(self.position, /DATA, /TO_NORMAL)
		
		;RELATIVE
		self.relative: BEGIN
			;Convert from relative to data coordinates.
			;   - If the range is [max, min], we must apply the normalization differently
			target -> GetProperty, XRANGE=xrange, YRANGE=yrange
			IF xrange[1] GT xrange[0] $
			    THEN xpos = (xrange[1] - xrange[0]) *        self.position[0]  + xrange[0] $
			    ELSE xpos = (xrange[0] - xrange[1]) * (1.0 - self.position[0]) + xrange[1]
			IF yrange[1] GT yrange[0] $
			    THEN ypos = (yrange[1] - yrange[0]) *        self.position[1]  + yrange[0] $
			    ELSE ypos = (yrange[0] - yrange[1]) * (1.0 - self.position[1]) + yrange[1]

			;Convert from data to normal coordinates.
			location = target -> ConvertCoord([xpos, ypos], /DATA, /TO_NORMAL)
		ENDCASE
		
		;DEVICE
		self.device: location = target -> ConvertCoord(self.position, /DEVICE, /TO_NORMAL)
		
		;NORMAL
		self.normal: location = self.position
	ENDCASE

	bx0 = location[0]
	by1 = location[1]

;-----------------------------------------------------
; Where to Draw \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Character sizes
	x_char = Float(!D.X_Ch_Size) / !D.X_Size
	y_char = Float(!D.Y_Ch_Size) / !D.Y_Size

	;Size of window
	xsize = !D.X_Size
	ysize = !D.Y_Size

	; In order to figure out how wide the box is, we first need to draw the text.
	;	- If windows are not supported, we have to draw outside the visible area.
	;	- For other devices, create a pixmap window and draw in the middle.
	IF ((!D.Flags AND 256) EQ 0) THEN BEGIN
		xx    = 0.1
		yy    = 1.25
	ENDIF ELSE BEGIN
		;Current window
		currentID = !D.Window
	
		;Open an invisible window
		Window, XSIZE=xsize, YSIZE=ysize, /PIXMAP, /FREE
		pixID = !D.Window

		;Draw in the middle of the window
		xx = 0.5
		yy = 0.5
	ENDELSE

;-----------------------------------------------------
; Legend Width \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	allItems    = self.items -> Get(/ALL, COUNT=nItems)
	item_width  = FltArr(nItems)
	item_height = FltArr(nItems)
		
	;Draw in the background color. Do so in decomposed color.
	cgSetColorState, 1, Current=currentState

	;For each legend item
	FOR j = 0, nItems - 1 DO BEGIN
		;Draw the legend item to determine the space it will take up.
		allItems[j]   -> Draw, xx, yy, HEIGHT=height, WIDTH=length
		item_width[j]  = length
		item_height[j] = height
	ENDFOR

	;Set the width
	*self.width = item_width
		
	;Return to the original color state.
	cgSetColorState, currentState

	;Delete invisible window
	IF N_Elements(pixID) NE 0 THEN BEGIN
		WDelete, pixID
		IF currentID GE 0 THEN WSet, currentID
	ENDIF

;-----------------------------------------------------
; Final Position \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Horizontal
	IF self.orientation THEN BEGIN
		bx1 = bx0 + total(item_width) + total(self.margins[[0,2]]*x_char) + (nItems-1)*self.horizontal_spacing*x_char
		by0 = by1 - max(item_height)  - total(self.margins[[1,3]]*y_char)
	
	;Vertical
	ENDIF ELSE BEGIN
		bx1 = bx0 + max(item_width)    + total(self.margins[[0,2]]*x_char)
		by0 = by1 - total(item_height) - total(self.margins[[1,3]]*y_char) - (nItems-1)*self.vertical_spacing*y_char
	ENDELSE

	;Dimensions of the box
	xlength = bx1 - bx0
	ylength = by1 - by0
	
	;Alignment
	bx0 = bx0 - xlength * self.align_horizontal
	bx1 = bx1 - xlength * self.align_horizontal
	by0 = by0 + ylength * (1.0 - self.align_vertical)
	by1 = by1 + ylength * (1.0 - self.align_vertical)

	;Save the box position
	self.bx_pos = [bx0, by0, bx1, by1]
END


;+
;   Draw the legend.
;
; :Keywords:
;       NOERASE:        in, optional, type=boolean, defualt=1
;                       If set, the graphics window will not be erased before drawing.
;-
PRO MrLegend::Draw, $
NOERASE=noerase
	Compile_Opt idl2

	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /Cancel
		self.bx_pos = FltArr(4)
		IF N_Elements(thisFont) NE 0 THEN !P.Font = thisFont
		IF N_Elements(incomingColorState) THEN cgSetColorState, incomingColorState
		void = cgErrorMsg()
		RETURN
	ENDIF
	
	;Hide?
	IF self.hide EQ 1 THEN RETURN

	;Determine the position of the box
	self -> CalculateBoxSize
	x_char = Float(!D.X_Ch_Size) / !D.X_Size
	y_char = Float(!D.Y_Ch_Size) / !D.Y_Size

	; We want to draw in decomposed color, if possible.
	TVLCT, r, g, b, /GET
	cgSetColorState, 1, Current=incomingColorState

	;Convert properties
	color     = cgColor(*self.color)
	linestyle = MrLinestyle(*self.linestyle)

;-----------------------------------------------------
; Draw the Box \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Fill the background?
	;   - cgColor does not like when FILL_COLOR is the empty string.
	IF ~(MrIsA(*self.fill_color, 'STRING') && *self.fill_color EQ '') THEN BEGIN
        fill_color = cgColor(*self.fill_color)
		cgColorFill, POSITION=self.bx_pos, COLOR=fill_color
	ENDIF

	;Draw the box outline.
	IF linestyle NE 6 THEN BEGIN
		PlotS, self.bx_pos[[0,0,2,2,0]], self.bx_pos[[3,1,1,3,3]], $
		       /NORMAL, $
		       COLOR = color, $
		       THICK = self.thick
	ENDIF
	
;-----------------------------------------------------
; Draw Legend Items \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	allItems = self.items -> Get(/ALL, COUNT=nItems)
	x = self.bx_pos[0] + self.margins[0]*x_char
	y = self.bx_pos[3] - self.margins[3]*y_char

	;Step through each item
	FOR j = 0, nItems - 1 DO BEGIN
		;Draw the item
		allItems[j] -> Draw, x, y, HEIGHT=height, WIDTH=width
		
		;Shift to next item: Verticle or Horizontal.
		IF self.orientation $
			THEN x += width   + self.horizontal_spacing * x_char $
			ELSE y -= (height + self.vertical_spacing   * y_char)
	ENDFOR

	;Restore starting color state.
	cgSetColorState, incomingColorState
	TVLCT, r, g, b
END


;+
;   Find a legend item object by its label.
;
; :Params:
;       LABEL:          in, required, type=string
;                       Label of the legend item to be found. See IDL's StrMatch function
;                           to see which and how special characters are handled.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of legend items found that have `LABEL` as a label.
;       FOLD_CASE:      in, optional, type=boolean, default=0
;                       If set, the search for `LABEL` will be case insensitive.
;-
FUNCTION MrLegend::FindByLabel, label, $
COUNT=count, $
FOLD_CASE=fold_case
	Compile_Opt idl2
	On_Error, 2

	;Assume we cannot find the label.
	count = 0

	;Step through all labels
	i = 0
	all_items = self.items -> Get(/ALL, COUNT=nItems)
	WHILE i LT nItems && count EQ 0 DO BEGIN
		;Get the label and check for a match.
		all_items[i] -> GetProperty, LABEL=test_label
		IF StrMatch(label, test_label, FOLD_CASE=fold_case) THEN BEGIN
			item_match = all_items[i]
			count += 1
		ENDIF
		
		;Continue to the next item.
		i++
	ENDWHILE
	
	;Add to container
	IF count EQ 0 THEN item_match = Obj_New()
	RETURN, item_match
END


;+
;   Get a legend item object, its position, and/or its label.
;
; :Params:
;       ITEM:           in, required, type=objref/integer/string
;                       The object reference, container position, or label of the legend
;                           item to be retrieved.
;
; :Keywords:
;       LABEL:          out, optional, type=string
;                       Label of the legend item.
;       POSITION:       out, optional, type=integer
;                       Position within the items container of the legend item.
;-
FUNCTION MrLegend::GetItem, item, $
POSITION=position, $
LABEL=label
	Compile_Opt idl2
	On_Error, 2

	;Position in container
	IF MrIsA(item, /NUMBER) THEN BEGIN
		the_item = self.items -> Get(POSITION=item)
	
	;Label
	ENDIF ELSE IF Size(item, /TNAME) EQ 'STRING' THEN BEGIN
		the_item = self -> FindByLabel(item, COUNT=count)
		IF count EQ 0 THEN Message, 'No item with label "' + item + '" found.'
		
	
	;Legend_Item
	ENDIF ELSE IF Size(item, /TNAME) EQ 'OBJREF' THEN BEGIN
		IF Obj_IsA(item, 'MrLegend_Item') $
			THEN the_item = item $
			ELSE Message, 'ITEM must be a MrLegend_Item object.'
	
	;Unknown
	ENDIF ELSE BEGIN
		Message, 'ITEM must be a container position, label, or legend item object.'
	ENDELSE
	
	;Return the item's container position or its label?
	IF Arg_Present(position) THEN void = self.items -> IsContained(the_item, POSITION=position)
    IF Arg_Present(label)    THEN the_item -> GetProperty, LABEL=label
	
	RETURN, the_item
END    


;+
;   Get the position of the legend in normal coordinates: [x0, y0, x1, y1]. (x0, y0) are
;   the coordinates of the lower left-corner of the legend, while (x1, y1) are those of
;   the upper-right corner.
;-
FUNCTION MrLegend::GetPosition
    Compile_Opt idl2
    
    ;Catch errors
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN, [0,0]
    ENDIF
    
    ;Calculate the length and width of the legend
    dimensions = self -> LegendSize()

    ;Determine the position based on the location of the upper-left corner.
    position = fltarr(4)
    
    ;Do we need to calculate the location?
    if n_elements(*self.location) eq 1 $
        then location = self -> calcLegendLocation() $
        else location = *self.location
        
    position[[0,3]] = location
    position[1] = location[0] - dimensions[0]
    position[2] = location[1] + dimensions[1]
        
    RETURN, POSITION
END    


;+
; This method obtains properties from the object.
;
; :Params:
;       ITEM:               in, optional, type=objref/integer/string
;                           The object reference, container position, or label of the legend
;                               item whose properties are to be set.
;
; :Keywords:
;       ALIGNMENT:          in, optional, type=string
;                           Shortcut keyword for specifying `HORIZONTAL_ALIGNMENT` and
;                               `VERTICAL_ALIGNMENT` at the same time. Choices are::
;                                   'E'     'East'
;                                   'N'     'North'
;                                   'NE'    'NorthEast'
;                                   'NW'    'NorthWest'
;                                   'S'     'South'
;                                   'SE'    'SouthEast'
;                                   'SW'    'SouthWest'
;                                   'W'     'West'
;       COLOR:              in, optional, type=string/integer/bytarr(3)
;                           Name, index, 24-bit number, or RGB-triple of the color of the
;                               bounding box of the legend.
;       DATA:               in, optional, type=boolean
;                           If set, `POSITION` is provided in data coordinates. The data
;                               coordinates used are those of the target of the legend
;                               item at position 0.
;       DEVICE:             in, optional, type=boolean
;                           If set, `POSITION` is provided in device coordinates.
;       FILL_COLOR:         in, optional, type=string/integer/bytarr(3)
;                           Name, index, 24-bit number, or RGB-triple of the color with
;                               which to fill the background of the legend.
;       HARDWARE:           in, optional, type=boolean, default=0
;                           If set, hardware fonts will be used.
;       HORIZONTAL_ALIGNMENT:   in, optional, type=float/string
;                               Horizontal alignment with respect to `POSITION`. A value
;                                   of 0 (1) indicates left- (right-) aligned. The strings
;                                   'LEFT', 'CENTER', and 'RIGHT' may also be used.
;       HORIZONTAL_SPACING:     in, optional, type=float
;                               Spacing between legend items when `ORIENTATION`=0, in
;                                   units of character size.
;       LINESTYLE:          in, optional, type=float
;                           Style with with to draw the line. Choices are::
;                               0   '-'     "Solid
;                               1   '.'     "Dot"
;                               2   '--'    "Dash"
;                               3   '-.'    "Dash_Dot"
;                               4   '-:'    "Dash_Dot_Dot"
;                               5   '__'    "Long_Dash"
;                               6   ' '     "None"
;       MARGINS:            in, optional, type=fltarr(4)
;                           A vector specifying the [left, bottom, right, top] margins
;                               of the legend items, in character units. Margins are the
;                               distance from the edge of the legend box to the legend
;                               item space.
;       NORMAL:             in, optional, type=boolean
;                           If set, `POSITION` is given in normalized coordinates.
;       ORIENTATION:        in, optional, type=integer
;                           A value of 1 (0) indicates legend items will be stacked
;                               horizontally (vertically)
;       POSITION:           in, optional, type=fltarr(2)
;                           Location of the [right, top] corner of the legend.
;       RELATIVE:           in, optional, type=boolean
;                           If set, `POSITION` is normalized with respect to the target's
;                               data space. See `DATA`.
;       THICK:              in, optional, type=integer
;                           Thickness of the legend box's outline.
;       TT_FONT:            in, optional, type=string
;                           Name of the true-type font to use.
;       VERTICAL_ALIGNMENT: in, optional, type=float/string
;                           Vertical alignment with respect to `POSITION`. A value
;                               of 0 (1) indicates bottom- (top-) aligned. The strings
;                               'BOTTOM', 'CENTER', and 'TOP' may also be used.
;       VERTICAL_SPACING:   in, optional, type=float
;                           Spacing between legend items when `ORIENTATION`=0, in units
;                               of character sizes.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrGrAtom::GetProperty is also accepted
;                               via keyword inheritance. If `ITEM` is given, any
;                               keyword accepted by MrLegend_Item::GetProperty is accepted.
;-
PRO MrLegend::GetProperty, item, $
ALIGNMENT=alignment, $
BX_POS=bx_pos, $
COLOR=color, $
DATA=data, $
DEVICE=device, $
FILL_COLOR=fill_color, $
HARDWARE=hardware, $
HORIZONTAL_ALIGNMENT=horizontal_alignment, $
HORIZONTAL_SPACING=horizontal_spacing, $
LINESTYLE=linestyle, $
MARGINS=margins, $
NORMAL=normal, $
ORIENTATION=orientation, $
POSITION=position, $
RELATIVE=relative, $
THICK=thick, $
TT_FONT=tt_font, $
VERTICAL_ALIGNMENT=vertical_alignment, $
VERTICAL_SPACING=vertical_spacing, $
_REF_EXTRA=extra
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN
    ENDIF

    ;Get an item's properties
	IF N_Elements(item) GT 0 THEN BEGIN
		oItem = self -> GetItem(item)
		oItem -> GetProperty, _STRICT_EXTRA=extra
		RETURN
	ENDIF

    ;Get legend properties
	IF Arg_Present(alignment)            THEN alignment            =  self.alignment
	IF Arg_Present(bx_pos)               THEN bx_pos               =  self.bx_pos
	IF Arg_Present(color)                THEN color                = *self.color
	IF Arg_Present(fill_color)           THEN fill_color           = *self.fill_color
	IF Arg_Present(hardware)             THEN hardware             =  self.hardware
	IF Arg_Present(horizontal_alignment) THEN horizontal_alignment = *self.horizontal_alignment
	IF Arg_Present(horizontal_spacing)   THEN horizontal_spacing   =  self.horizontal_spacing
	IF Arg_Present(linestyles)           THEN linestyles           = *self.linestyles
	IF Arg_Present(margins)              THEN margins              =  self.margins
	IF Arg_Present(position)             THEN position             =  self.position
	IF Arg_Present(orientation)          THEN orientation          =  self.orientation
	IF Arg_Present(thick)                THEN thick                =  self.thick
	IF Arg_Present(tt_font)              THEN tt_font              = *self.tt_font
	IF Arg_Present(data)                 THEN data                 =  self.data
	IF Arg_Present(device)               THEN device               =  self.device
	IF Arg_Present(normal)               THEN normal               =  self.normal
	IF Arg_Present(relative)             THEN relative             =  self.relative
	IF Arg_Present(vertical_alignment)   THEN vertical_alignment   = *self.vertical_alignment
	IF Arg_Present(vertical_spacing)     THEN vertical_spacing     =  self.vertical_spacing

	;Keywords for MrGrAtom
	IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::GetProperty, _STRICT_EXTRA=extra
END


;+
;   Determine if the coordate made by X and Y lies within the object.
;
; :Params:
;       X:              in, required, type=numeric scalar
;                       X coordinate 
;       Y:              in, optional, type=numeric scalar/array
;                       Y coordinate
;       POSITION:       in, optional, type=fltarr(4), default=*self.position
;                       Determine if [`X`,`Y`] is inside this POSITION.
;
; :Keywords:
;       _REF_EXTRA:     in, out, optional, type=any
;                       Any keyword accepted by MrGraphicAtom::IsInside is also accepted
;                           for keyword inheritance.
;
; :Returns:
;       TF_INSIDE:      Returns true (1) if [x,y] lies within POSITION. False (0) otherwise.
;-
FUNCTION MrLegend::IsInside, x, y, position, $
NORMAL = normal, $
_REF_EXTRA=extra
    Compile_Opt idl2
    
    ;Catch errors
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN, [0,0]
    ENDIF
    
    ;Get the position
    if n_elements(position) eq 0 then begin
        position = self -> GetPosition()
        normal = 1
    endif

    ;Call the superclass
    tf_inside = self -> MrGrAtom::IsInside(x, y, position, $
                                           NORMAL=normal, $
                                           _STRICT_EXTRA=extra)
    
    RETURN, tf_inside
END


;+
;   Remove an item from the legend.
;
; :Params:
;       ITEM:               in, optional, type=objref/integer/string
;                           The object reference, container position, or label of the legend
;                               item to be removed.
;-
PRO MrLegend::Remove, item
	Compile_Opt idl2
	On_Error, 2

	;Get the object reference of the legend item being removed.
	the_item = self -> GetItem(item)
	
	;Remove the item and destroy it.
	self.items -> Remove, the_item, /DESTROY
END


;+
; This method obtains properties from the object.
;
; :Params:
;       ITEM:               in, optional, type=objref/integer/string
;                           The object reference, container position, or label of the legend
;                               item whose properties are to be set.
;
; :Keywords:
;       ALIGNMENT:          in, optional, type=string
;                           Shortcut keyword for specifying `HORIZONTAL_ALIGNMENT` and
;                               `VERTICAL_ALIGNMENT` at the same time. Choices are::
;                                   'E'     'East'
;                                   'N'     'North'
;                                   'NE'    'NorthEast'
;                                   'NW'    'NorthWest'
;                                   'S'     'South'
;                                   'SE'    'SouthEast'
;                                   'SW'    'SouthWest'
;                                   'W'     'West'
;       COLOR:              in, optional, type=string/integer/bytarr(3)
;                           Name, index, 24-bit number, or RGB-triple of the color of the
;                               bounding box of the legend.
;       DATA:               in, optional, type=boolean
;                           If set, `POSITION` is provided in data coordinates. The data
;                               coordinates used are those of the target of the legend
;                               item at position 0.
;       DEVICE:             in, optional, type=boolean
;                           If set, `POSITION` is provided in device coordinates.
;       FILL_COLOR:         in, optional, type=string/integer/bytarr(3)
;                           Name, index, 24-bit number, or RGB-triple of the color with
;                               which to fill the background of the legend.
;       HARDWARE:           in, optional, type=boolean, default=0
;                           If set, hardware fonts will be used.
;       HORIZONTAL_ALIGNMENT:   in, optional, type=float/string
;                               Horizontal alignment with respect to `POSITION`. A value
;                                   of 0 (1) indicates left- (right-) aligned. The strings
;                                   'LEFT', 'CENTER', and 'RIGHT' may also be used.
;       HORIZONTAL_SPACING:     in, optional, type=float
;                               Spacing between legend items when `ORIENTATION`=0, in
;                                   units of character size.
;       LINESTYLE:          in, optional, type=float
;                           Style with with to draw the line. Choices are::
;                               0   '-'     "Solid
;                               1   '.'     "Dot"
;                               2   '--'    "Dash"
;                               3   '-.'    "Dash_Dot"
;                               4   '-:'    "Dash_Dot_Dot"
;                               5   '__'    "Long_Dash"
;                               6   ' '     "None"
;       MARGINS:            in, optional, type=fltarr(4)
;                           A vector specifying the [left, bottom, right, top] margins
;                               of the legend items, in character units. Margins are the
;                               distance from the edge of the legend box to the legend
;                               item space.
;       NORMAL:             in, optional, type=boolean
;                           If set, `POSITION` is given in normalized coordinates.
;       ORIENTATION:        in, optional, type=integer
;                           A value of 1 (0) indicates legend items will be stacked
;                               horizontally (vertically)
;       POSITION:           in, optional, type=fltarr(2)
;                           Location of the [right, top] corner of the legend.
;       RELATIVE:           in, optional, type=boolean
;                           If set, `POSITION` is normalized with respect to the target's
;                               data space. See `DATA`.
;       THICK:              in, optional, type=integer
;                           Thickness of the legend box's outline.
;       TT_FONT:            in, optional, type=string
;                           Name of the true-type font to use.
;       VERTICAL_ALIGNMENT: in, optional, type=float/string
;                           Vertical alignment with respect to `POSITION`. A value
;                               of 0 (1) indicates bottom- (top-) aligned. The strings
;                               'BOTTOM', 'CENTER', and 'TOP' may also be used.
;       VERTICAL_SPACING:   in, optional, type=float
;                           Spacing between legend items when `ORIENTATION`=0, in units
;                               of character sizes.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrGrAtom::SetProperty is also accepted
;                               via keyword inheritance. If `ITEM` is given, any
;                               keyword accepted by MrLegend_Item::SetProperty is accepted.
;-
PRO MrLegend::SetProperty, item, $
ALIGNMENT=alignment, $
AUTO_TEXT_COLOR=auto_text_color, $
COLOR=color, $
DATA=data, $
DEVICE=device, $
FILL_COLOR=fill_color, $
HARDWARE=hardware, $
HORIZONTAL_ALIGNMENT=horizontal_alignment, $
HORIZONTAL_SPACING=horizontal_spacing, $
LABEL=label, $
LINESTYLE=linestyle, $
MARGINS=margins, $
NORMAL=normal, $
ORIENTATION=orientation, $
POSITION=position, $
RELATIVE=relative, $
SAMPLE_ANGLE=sample_angle, $
SAMPLE_COLOR=sample_color, $
SAMPLE_MAGNITUDE=sample_magnitude, $
SAMPLE_WIDTH=sample_width, $
SAMPLE_LENGTH=sample_length, $
SAMPLE_LINESTYLE=sample_linestyle, $
SYMBOL=psym, $
SYM_COLOR=sym_color, $
SYM_CENTER=sym_center, $
SYM_SIZE=sym_size, $
SYM_THICK=sym_thick, $
TARGET=target, $
TEXT_COLOR=text_color, $
TEXT_SIZE=text_size, $
TEXT_THICK=text_thick, $
THICK=thick, $
TT_FONT=tt_font, $
VERTICAL_ALIGNMENT=vertical_alignment, $
VERTICAL_SPACING=vertical_spacing, $
_REF_EXTRA=extra
	Compile_Opt idl2

	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /Cancel
		void = cgErrorMsg()
		RETURN
	ENDIF

;-----------------------------------------------------
; A Legend Item \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF N_Elements(item) GT 0 THEN BEGIN
		oItem = self -> GetItem(item)
		oItem -> SetProperty, AUTO_TEXT_COLOR  = auto_text_color, $
		                      LABEL            = label, $
		                      TEXT_COLOR       = text_color, $
		                      TEXT_SIZE        = text_size, $
		                      TEXT_THICK       = text_thick, $
		                      TARGET           = target, $
		                      SAMPLE_ANGLE     = sample_angle, $
		                      SAMPLE_COLOR     = sample_color, $
		                      SAMPLE_LINESTYLE = sample_linestyle, $
		                      SAMPLE_MAGNITUDE = sample_magnitude, $
		                      SAMPLE_THICK     = sample_thick, $
		                      SAMPLE_WIDTH     = sample_width, $
		                      SYMBOL           = psym, $
		                      SYM_CENTER       = sym_center, $
		                      SYM_COLOR        = sym_color, $
		                      SYM_SIZE         = sym_size, $
		                      SYM_THICK        = sym_thick
		RETURN
	ENDIF

	
;-----------------------------------------------------
; Other Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF N_Elements(color)              NE 0 THEN *self.color              = color
	IF N_Elements(fill_color)         NE 0 THEN *self.fill_color         = fill_color
	IF N_Elements(hardware)           NE 0 THEN  self.hardware           = Keyword_Set(hardware)
	IF N_Elements(horizontal_spacing) NE 0 THEN  self.horizontal_spacing = horizontal_spacing
	IF N_Elements(linestyle)          NE 0 THEN *self.linestyle          = linestyle
	IF N_Elements(margins)            NE 0 THEN  self.margins            = margins
	IF N_Elements(position)           NE 0 THEN  self.position           = position
	IF N_Elements(orientation)        NE 0 THEN  self.orientation        = Keyword_Set(orientation)
	IF N_Elements(thick)              NE 0 THEN  self.thick              = thick
	IF N_Elements(tt_font)            NE 0 THEN *self.tt_font            = tt_font
	IF N_Elements(vertical_spacing)   NE 0 THEN  self.vertical_spacing   = vertical_spacing

	;MrGrAtom
	IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra

;-----------------------------------------------------
; Coordinate System \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;DATA, DEVICE, NORMAL, and RELATIVE depend on each other.
	;   - DATA takes precendence, so set last.
	;   - RELATIVE automatically sets DATA = 1
	IF N_Elements(device) GT 0 THEN BEGIN
		self.device = Keyword_Set(device)
		IF self.device THEN BEGIN
			self.data     = 0B
			self.normal   = 0B
			self.relative = 0B
		ENDIF
	ENDIF

	IF N_Elements(normal) GT 0 THEN BEGIN
		self.normal = Keyword_Set(normal)
		IF self.normal THEN BEGIN
			self.data     = 0B
			self.device   = 0B
			self.relative = 0B
		ENDIF
	ENDIF

	;A relative position is with respect to the dataspace.
	IF N_Elements(relative) GT 0 THEN BEGIN
		self.relative = Keyword_Set(relative)
		IF self.relative THEN BEGIN
			self.normal = 0B
			self.data   = 0B
			self.device = 0B
		ENDIF
	ENDIF

	IF N_Elements(data) GT 0 THEN BEGIN
		self.data = Keyword_Set(data)
		IF self.data THEN BEGIN
			self.device = 0B
			self.normal = 0B
		ENDIF
	ENDIF

;-----------------------------------------------------
; Alignment \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF N_Elements(alignment) GT 0 THEN BEGIN
		align = StrUpCase(alignment)

		;Convert short names to long names.
		CASE align OF
			'C':  align = 'CENTER'
			'E':  align = 'EAST'
			'N':  align = 'NORTH'
			'NE': align = 'NORTHEAST'
			'NW': align = 'NORTHWEST'
			'S':  align = 'SOUTH'
			'SE': align = 'SOUTHEAST'
			'SW': align = 'SOUTHWEST'
			'W':  align = 'WEST'
			ELSE: ;Continue
		ENDCASE

		;Set vertical and horizontal alignment.
		CASE align OF
			'CENTER': BEGIN
				vertical_alignment   = 'Center'
				horizontal_alignment = 'Center'
			ENDCASE
			'EAST': BEGIN
				vertical_alignment   = 'Center'
				horizontal_alignment = 'Right'
			ENDCASE
			'NORTH': BEGIN
				vertical_alignment   = 'Top'
				horizontal_alignment = 'Center'
			ENDCASE
			'NORTHEAST': BEGIN
				vertical_alignment   = 'Top'
				horizontal_alignment = 'Right'
			ENDCASE
			'NORTHWEST': BEGIN
				vertical_alignment   = 'Top'
				horizontal_alignment = 'Left'
			ENDCASE
			'SOUTH': BEGIN
				vertical_alignment   = 'Bottom'
				horizontal_alignment = 'Center'
			ENDCASE
			'SOUTHEAST': BEGIN
				vertical_alignment   = 'Bottom'
				horizontal_alignment = 'Right'
			ENDCASE
			'SOUTHWEST': BEGIN
				vertical_alignment   = 'Bottom'
				horizontal_alignment = 'Left'
			ENDCASE
			'WEST': BEGIN
				vertical_alignment   = 'Center'
				horizontal_alignment = 'Left'
			ENDCASE
			ELSE: Message, 'Alignment "' + alignment + '" invalid.'
		ENDCASE
	ENDIF

	;Vertical alignment offset
	IF N_Elements(vertical_alignment) GT 0 THEN BEGIN
		;String specifying offset location.
		IF Size(vertical_alignment, /TNAME) EQ 'STRING' THEN BEGIN
			valign = StrUpCase(vertical_alignment)
			CASE valign OF
				'TOP':    self.align_vertical = 1.0
				'CENTER': self.align_vertical = 0.5
				'BOTTOM': self.align_vertical = 0.0
				ELSE: Message, 'Vertical Alignment option "' + vertical_alignment + ' not valid.', /INFORMATIONAL
			ENDCASE
		
		;Normalized offset
		ENDIF ELSE BEGIN
			self.align_vertical = vertical_alignment
		ENDELSE
		
		;Set the property
		*self.vertical_alignment = vertical_alignment
	ENDIF
	
	;Horizontal Alignment
	IF N_Elements(horizontal_alignment) GT 0 THEN BEGIN
		;String specifying offset location.
		IF Size(horizontal_alignment, /TNAME) EQ 'STRING' THEN BEGIN
			halign = StrUpCase(horizontal_alignment)
			CASE halign OF
				'RIGHT':  self.align_horizontal = 1.0
				'CENTER': self.align_horizontal = 0.5
				'LEFT':   self.align_horizontal = 0.0
				ELSE: Message, 'Horizontal alignment option "' + horizontal_alignment + ' not valid.', /INFORMATIONAL
			ENDCASE
		
		;Normalized offset
		ENDIF ELSE BEGIN
			self.align_horizontal = horizontal_alignment
		ENDELSE
		
		;Set the property
		*self.horizontal_alignment = horizontal_alignment
	ENDIF

	self.window -> Draw
END


;+
;   Clean up after the legend is destroyed.
;-
PRO MrLegend::CLEANUP
	
	;Free pointers
	Ptr_Free, self.color
	Ptr_Free, self.fill_color
	Ptr_Free, self.horizontal_alignment
	Ptr_Free, self.linestyle
	Ptr_Free, self.tt_font
	Ptr_Free, self.vertical_alignment
	Ptr_Free, self.width

	;Destroy legend items
	Obj_Destroy, self.items

	;Cleanup Superclasses
	self -> MrGrAtom::Cleanup
END


;+
; This method creates an instance of the object.
;
; :Params:
;       ITEM:               in, optional, type=objref/integer/string
;                           The object reference, container position, or label of the legend
;                               item whose properties are to be set.
;
; :Keywords:
;       ALIGNMENT:          in, optional, type=string, default='NE'
;                           Shortcut keyword for specifying `HORIZONTAL_ALIGNMENT` and
;                               `VERTICAL_ALIGNMENT` at the same time. ALIGNMENT takes
;                               precedence over those two keywords. Choices are::
;                                   'E'     'East'
;                                   'N'     'North'
;                                   'NE'    'NorthEast'
;                                   'NW'    'NorthWest'
;                                   'S'     'South'
;                                   'SE'    'SouthEast'
;                                   'SW'    'SouthWest'
;                                   'W'     'West'
;       COLOR:              in, optional, type=string/integer/bytarr(3)
;                           Name, index, 24-bit number, or RGB-triple of the color of the
;                               bounding box of the legend. The default is chosen with
;                               `MrDefaultColor`.
;       DATA:               in, optional, type=boolean, default=0
;                           If set, `POSITION` is provided in data coordinates. The data
;                               coordinates used are those of the target of the legend
;                               item at position 0.
;       DEVICE:             in, optional, type=boolean, default=0
;                           If set, `POSITION` is provided in device coordinates.
;       FILL_COLOR:         in, optional, type=string/integer/bytarr(3), default='White'
;                           Name, index, 24-bit number, or RGB-triple of the color with
;                               which to fill the background of the legend.
;       HARDWARE:           in, optional, type=boolean, default=0
;                           If set, hardware fonts will be used.
;       HORIZONTAL_ALIGNMENT:   in, optional, type=float/string, default='Right'
;                               Horizontal alignment with respect to `POSITION`. A value
;                                   of 0 (1) indicates left- (right-) aligned. The strings
;                                   'LEFT', 'CENTER', and 'RIGHT' may also be used. If
;                                   `ALIGNMENT` is provided, this keyword is ignored.
;       HORIZONTAL_SPACING:     in, optional, type=float, default=3.0
;                               Spacing between legend items when `ORIENTATION`=1, in
;                                   units of character size.
;       LINESTYLE:          in, optional, type=float, default='Solid_Line'
;                           Style with with to draw the line. Choices are::
;                               0   '-'     "Solid_Line"
;                               1   '.'     "Dot"
;                               2   '--'    "Dash"
;                               3   '-.'    "Dash_Dot"
;                               4   '-:'    "Dash_Dot_Dot_Dot"
;                               5   '__'    "Long_Dash"
;                               6   ' '     "None"
;       MARGINS:            in, optional, type=fltarr(4)
;                           A vector specifying the [left, bottom, right, top] margins
;                               of the legend items, in character units. Margins are the
;                               distance from the edge of the legend box to the legend
;                               item space.
;       NORMAL:             in, optional, type=boolean
;                           If set, `POSITION` is given in normalized coordinates. This is
;                               the default if `DEVICE`, `DATA`, and `RELATIVE` are not set.
;       ORIENTATION:        in, optional, type=integer
;                           A value of 1 (0) indicates legend items will be stacked
;                               horizontally (vertically)
;       POSITION:           in, optional, type=fltarr(2)
;                           Location of the [right, top] corner of the legend.
;       RELATIVE:           in, optional, type=boolean, default=0
;                           If set, `POSITION` is normalized with respect to the target's
;                               data space. See `DATA`.
;       THICK:              in, optional, type=float, default=1
;                           Thickness of the legend box's outline.
;       TT_FONT:            in, optional, type=string
;                           Name of the true-type font to use.
;       VERTICAL_ALIGNMENT: in, optional, type=float/string, default='Top'
;                           Vertical alignment with respect to `POSITION`. A value
;                               of 0 (1) indicates bottom- (top-) aligned. The strings
;                               'BOTTOM', 'CENTER', and 'TOP' may also be used. This
;                               keyword is ignored if `ALIGNMENT` is used.
;       VERTICAL_SPACING:   in, optional, type=float, default=0.5
;                           Spacing between legend items when `ORIENTATION`=0, in units
;                               of character sizes.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrGrAtom::SetProperty is also accepted
;                               via keyword inheritance. If `ITEM` is given, any
;                               keyword accepted by MrLegend_Item::SetProperty is accepted.
;-
FUNCTION MrLegend::INIT, $
TARGET=target, $
DATA=data, $
DEVICE=device, $
NORMAL=normal, $
RELATIVE=relative, $

;Legend Position
ALIGNMENT=alignment, $
MARGINS=margins, $
ORIENTATION=orientation, $
POSITION=position, $
HORIZONTAL_ALIGNMENT=horizontal_alignment, $
HORIZONTAL_SPACING=horizontal_spacing, $
VERTICAL_ALIGNMENT=vertical_alignment, $
VERTICAL_SPACING=vertical_spacing, $

;Legend Boundary
COLOR=color, $
LINESTYLE=linestyle, $
THICK=thick, $
FILL_COLOR=fill_color, $

;Legend Lines
SAMPLE_ANGLE=sample_angle, $
SAMPLE_COLOR=sample_color, $
SAMPLE_MAGNITUDE=sample_magnitude, $
SAMPLE_WIDTH=sample_width, $
SAMPLE_LINESTYLE=sample_linestyle, $

;Legend Symbols, $
SYMBOL=psym, $
SYM_COLOR=sym_color, $
SYM_CENTER=sym_center, $
SYM_SIZE=sym_size, $
SYM_THICK=sym_thick, $

;Legend Text
AUTO_TEXT_COLOR=auto_text_color, $
HARDWARE=hardware, $
LABEL=label, $
TEXT_COLOR=text_color, $
TEXT_SIZE=text_size, $
TEXT_THICK=text_thick, $
TT_FONT=tt_font, $

;GrAtom Properties
_REF_EXTRA=extra
	Compile_Opt strictarr

	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /Cancel
		void = cgErrorMsg()
		RETURN, 0
	ENDIF

;-----------------------------------------------------
;Target & Title \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	nLabels  = n_elements(label)
	nTargets = n_elements(target)

	;Get targets
	if nTargets eq 0 then begin
		target = self -> _GetTarget(/ALL, /ANY, COUNT=nTargets)
		if nTargets eq 0 then message, 'Insert MrLegend failed. No targets available.'
	endif

	;Get the title from the targets?
	if n_elements(label) eq 0 then begin
		label = strarr(nTargets)
		for i = 0, nTargets-1 do label[i] = target[i] -> GetName()
	endif
	nLegends = n_elements(label)
	IF nTargets GT 1 && nLegends NE nTargets THEN $
		Message, 'TARGET and LABEL must have the same number of elements.'

;-----------------------------------------------------
;Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Coordinates
	normal   = keyword_set(normal)
	data     = keyword_set(data)
	device   = keyword_set(device)
	relative = keyword_set(relative)
	if normal + data + device + relative eq 0 then normal = 1
	if normal + data + device + relative gt 1 then $
		message, 'DATA, DEVICE, NORMAL, and RELATIVE keywords are mutually exclusive.'

	;Colors
	color        = MrDefaultColor(color,        NCOLORS=1)
	text_color   = MrDefaultColor(text_color,   NCOLORS=nLegends)
	sample_color = MrDefaultColor(sample_color, NCOLORS=nLegends, DEFAULT=text_color)
	sym_color    = MrDefaultColor(sym_color,    NCOLORS=nLegends, DEFAULT=text_color)
	
	IF N_Elements(sample_angle)     EQ 0 THEN sample_angle     = Replicate(0.0,          nLegends)
	IF N_Elements(sample_linestyle) EQ 0 THEN sample_linestyle = Replicate('Solid_Line', nLegends)
	IF N_Elements(sample_magnitude) EQ 0 THEN sample_magnitude = Replicate(0.0,          nLegends)
	IF N_Elements(sample_width)     EQ 0 THEN sample_width     = Replicate(5.0,          nLegends)
	IF N_Elements(psym)             EQ 0 THEN psym             = Replicate(0,            nLegends)
	
	IF N_Elements(sample_angle)     EQ 1 THEN sample_angle     = Replicate(sample_angle,     nLegends)
	IF N_Elements(sample_linestyle) EQ 1 THEN sample_linestyle = Replicate(sample_linestyle, nLegends)
	IF N_Elements(sample_magnitude) EQ 1 THEN sample_magnitude = Replicate(sample_magnitude, nLegends)
	IF N_Elements(sample_width)     EQ 1 THEN sample_width     = Replicate(sample_width,     nLegends)
	IF N_Elements(psym)             EQ 1 THEN psym             = Replicate(psym,             nLegends)
	
	;Position
	if n_elements(position) eq 0 then begin
		position  = [0.9, 0.9]
		alignment = 'NE'
		normal    = 1
		data      = 0
		device    = 0
		relative  = 0
	endif
	
	;Other defaults
	if n_elements(fill_color)           eq 0 then fill_color           = MrDefaultColor(/BACKGROUND)
	if n_elements(horizontal_alignment) eq 0 then horizontal_alignment = 'Left'
	if n_elements(horizontal_spacing)   eq 0 then horizontal_spacing   = 3.0
	if n_elements(linestyle)            eq 0 then linestyle            = 'Solid_Line'
	if n_elements(margins)              eq 0 then margins              = [1.0, 0.5, 1.0, 0.5]
	if n_elements(thick)                eq 0 then thick                = 1.0
	if n_elements(vertical_alignment)   eq 0 then vertical_alignment   = 'Top'
	if n_elements(vertical_spacing)     eq 0 then vertical_spacing     = 0.5

;---------------------------------------------------------------------
;Window //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Superclass
    ;   - Takes the window from the target, but does not set the target.
    success = self -> MrGrAtom::INIT(HIDE=hide, TARGET=target, WINREFRESH=refreshIn, _STRICT_EXTRA=extra)
    if success eq 0 then message, 'Unable to initialize MrGrAtom'
    
;---------------------------------------------------------------------
; Allocate Heap //////////////////////////////////////////////////////
;---------------------------------------------------------------------
	self.items                = Obj_New('MrIDL_Container')
	self.color                = Ptr_New(/ALLOCATE_HEAP)
	self.fill_color           = Ptr_New(/ALLOCATE_HEAP)
	self.horizontal_alignment = Ptr_New(/ALLOCATE_HEAP)
	self.linestyle            = Ptr_New(/ALLOCATE_HEAP)
	self.tt_font              = Ptr_New(/ALLOCATE_HEAP)
	self.vertical_alignment   = Ptr_New(/ALLOCATE_HEAP)
	self.width                = Ptr_New(/ALLOCATE_HEAP)

;---------------------------------------------------------------------
; Create Legend Items ////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Step through each legend item.
	nLabels = N_Elements(label)
	FOR i = 0, nLegends - 1 DO BEGIN
		IF nLabels GT 0 THEN temp_label = label[i]
		IF i GE nTargets $
			THEN void        = Temporary(temp_target) $
			ELSE temp_target = target[i]
	
		;Add a legend item for each label given.
		self -> Add, AUTO_TEXT_COLOR  = auto_text_color, $
		             LABEL            = temp_label, $
		             TEXT_COLOR       = text_color[i], $
		             TEXT_SIZE        = text_size, $
		             TEXT_THICK       = text_thick, $
		             TARGET           = temp_target, $
		             SAMPLE_ANGLE     = sample_angle[i], $
		             SAMPLE_COLOR     = sample_color[i], $
		             SAMPLE_LINESTYLE = sample_linestyle[i], $
		             SAMPLE_MAGNITUDE = sample_magnitude[i], $
		             SAMPLE_THICK     = sample_thick, $
		             SAMPLE_WIDTH     = sample_width[i], $
		             SYMBOL           = psym[i], $
		             SYM_CENTER       = sym_center, $
		             SYM_COLOR        = sym_color[i], $
		             SYM_SIZE         = sym_size, $
		             SYM_THICK        = sym_thick
	ENDFOR

;---------------------------------------------------------------------
; Set Properties /////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Other Properties
	self -> SetProperty, ALIGNMENT            = alignment, $
	                     COLOR                = color, $
	                     DATA                 = data, $
	                     DEVICE               = device, $
	                     FILL_COLOR           = fill_color, $
	                     HARDWARE             = hardware, $
	                     HORIZONTAL_ALIGNMENT = horizontal_alignment, $
	                     HORIZONTAL_SPACING   = horizontal_spacing, $
	                     LINESTYLE            = linestyle, $
	                     MARGINS              = margins, $
	                     NORMAL               = normal, $
	                     ORIENTATION          = orientation, $
	                     POSITION             = position, $
	                     RELATIVE             = relative, $
	                     THICK                = thick, $
	                     TT_FONT              = tt_font, $
	                     VERTICAL_ALIGNMENT   = vertical_alignment, $
	                     VERTICAL_SPACING     = vertical_spacing

	;Turn refresh back on?
	if refreshIn then self.window -> Refresh

	RETURN, 1
END


;+
; The class definition module for the object.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;
; :Fields:
;       ALIGNMENT_VERTICAL:     `VERTICAL_ALIGNMENT` as a floating point number.
;       ALIGNMENT_HORIZONTAL:   `HORIZONTAL_ALIGNMENT` as a floating point number.
;       BX_POS:                 Final position of the legend.
;       COLOR:                  Color of the legend border.
;       DATA:                   Indicates `POSITION` was given in data coordinates.
;       DEVICE:                 Indicates `POSITION` was given in device coordinates.
;       FILL_COLOR:             Color of the legend background.
;       HARDWARE:               Indicates hardware fonts should be used.
;       HORIZONTAL_ALIGNMENT:   Horizontal alignment of the legend with respect to `POSITION`.
;       LINESTYLE:              Linestyle of the legend border.
;       MARGINS:                Padding between legend border and legend items.
;       NORMAL:                 Indicates `POSITION` was given in normal coordinates.
;       ORIENTATION:            Indicates a horizontally or vertically stacked legend.
;       POSITION:               Position of the [right, top] corner of the legend.
;       RELATIVE:               Indicates `POSITION` was given in relative coordinates.
;       THICK:                  Thickness of the legend border.
;       TT_FONT:                Name of the true-type font to use for the legend items.
;       VERTICAL_ALIGNMENT:     Vertical alignment of the legend with respect to `POSITION`.
;       WIDTH:                  Width of the legend.
;-
PRO MrLegend__Define, class
	Compile_Opt strictarr
	          
	class = { MrLegend, $
	          INHERITS MrGrAtom, $
	          items:     Obj_New(), $
	          data:      0B, $
	          normal:    0B, $
	          relative:  0B, $
	          device:    0B, $

	          ;Legend position
	          align_vertical:       0.0, $
	          align_horizontal:     0.0, $
	          horizontal_alignment: Ptr_New(), $
	          horizontal_spacing:   0.0, $
	          margins:              FltArr(4), $
	          orientation:          0B, $
	          position:             FltArr(2), $
	          vertical_alignment:   Ptr_New(), $
	          vertical_spacing:     0.0, $

	          ;Legend Box
	          bx_pos:               FltArr(4), $
	          color:                Ptr_New(), $
	          linestyle:            Ptr_New(), $
	          thick:                0.0, $
	          fill_color:           Ptr_New(), $
	          width:                Ptr_New(), $
	          
	          ;Legend Text
	          hardware:             0B, $
	          tt_font:              Ptr_New() $
	        }
END