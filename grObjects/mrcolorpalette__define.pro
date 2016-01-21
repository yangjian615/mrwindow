; docformat = 'rst'
;
; NAME:
;       MrColorPalette
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
; PURPOSE:
;+
;   Create a color palette object for loading colors into the color table.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :Copyright:
;       Matthew Argall 2015
;
; :History:
;   Modification History::
;       2015/10/03  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The purpose of this method is to set object properties. 
;
; :Keywords:
;-
pro MrColorPalette::GetProperty, $
BLUE_VALUES=blue_values, $
GREEN_VALUES=green_values, $
NCOLORS=ncolors, $
RED_VALUES=red_values, $
RGB_TABLE=rgb_table, $
ROW=row
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Get Properties
	if arg_present(blue_values)  ne 0 then blue_values  = (*self.rgb_table)[*,2]
	if arg_present(green_values) ne 0 then green_values = (*self.rgb_table)[*,1]
	if arg_present(ncolors)      ne 0 then ncolors      =   self.ncolors
	if arg_present(red_values)   ne 0 then red_values   = (*self.rgb_table)[*,0]
	if arg_present(rgb_table)    ne 0 then rgb_table    =  *self.rgb_table
	if arg_present(row)          ne 0 then row          =   self.row
end


;+
;   Get colors from the color palette
;
; :Params:
;       INDEX:      in, required, type=byte/bytarr
;                   Index into the color pallete at which colors will be saved
;
; :Keywords:
;       COLOR24:    in, optional, type=boolean, default=0
;                   If set, colors will be returned as 24-bit color values.
;
; :Returns:
;       COLORS:     An array of Nx3 color triples, or if the ROW property is
;                       set, a 3xN array of color triples, or if `COLOR24` is
;                       set, a vector of long integer 24-bit color values.
;-
function MrColorPalette::GetRGB, index
COLOR24=color24
	compile_opt idl2
	on_error, 2
	
	;Make sure indices are in range
	if max(index) gt self.ncolors || min(index) lt 0 then $
		message, 'Given color index out of palette range.'

	;Get the colors
	colors = (*self.rgb_table)[index,*]

	;Convert to 24-bit colors
	if keyword_set(color24) $
		then colors = cgColor24(colors) $
		else if self.row && n_elements(index) gt 1 then colors = transpose(colors)

	return, colors
end


;+
;   Get names of the color tables within a colot table file.
;
; :Params:
;       FILE:       in, required, type=string
;                   Name of the color table file from which to extract color
;                       table names. If not given, the IDL color table will be
;                       used.
;
; :Keywords:
;       BREWER:     in, optional, type=boolean, default=0
;                   If set, the default `FILE` will be the brewer color table file.
;
; :Returns:
;       CT_NAMES:   Names of the color tables in `FILE`.
;-
function MrColorPalette::GetNames, file
BREWER=brewer
	compile_opt idl2
	on_error, 2
	
	;Default table
	brewer = Keyword_Set(brewer)
	IF N_Elements(file) EQ 0 && self.file EQ '' THEN BEGIN
		;Brewer or IDL color table?
		file = brewer ? self.ct_file_brewer : self.ct_file_idl 

		; Try to locate the brewer file. 
		IF brewer && file EQ '' THEN BEGIN
			Message, 'Cannot find the Brewer color table file "fsc_brewer.tbl."' + $
			         ' Using normal IDL color tables.', /INFORMATIONAL
			file = self.ct_file_idl
		ENDIF
	ENDIF

	; Open and read the color table files.
	OPENR, lun, file, /GET_LUN
	ntables = 0B
	READU, lun, ntables

	; Read the table names, if required, and return.
	ct_names = BytArr(32, ntables)
	Point_LUN, lun, ntables * 768L + 1
	READU,     lun, ct_names
	FREE_LUN,  lun
	ct_names = StrTrim(ct_names, 2)

	return, ct_names
end


;+
;   This is a drop-in replacement for the IDL-supplied program LOADCT.
;   The same keywords used with LOADCT apply. In addition, a REVERSE keyword
;   is supplied to reverse the color table vectors, and a CLIP keyword is
;   supplied to be able to clip the normal LOADCT color table. This is
;   extremely useful if you wish to use a reduced number of colors. All color 
;   table loading is handled silently. And Brewer color tables can be loaded.
;
; :Params:
;       TABLE:      in, optional, type=integer, default=0
;                   Optional color table number to load. Integer from 0 to the number of  
;                       tables in the color table file, minus 1. 
;       
; :Keywords:
;       BREWER:     in, optional, type=boolean, default=0
;                   Set this keyword if you wish to use the Brewer Colors, as
;                       implemented in the Coyote Library file, fsc_brewer.tbl.
;                       This program will look first in the $IDL_DIR/resource/colors 
;                       directory for the color table file, and failing to find it there
;                       will look in the same directory that the source code of this
;                       program is located, then in the IDL path. Finally, if it still
;                       can't find the file, it will ask you to locate it. If you can't
;                       find it, the program will simply return without loading a color
;                       table.
;       BOTTOM:     in, optional, type=integer, default=0
;                   The lowest color table index. The colors in the color table start
;                       loading here.
;       CLIP:       in, optional, type=integer
;                   A one- or two-element integer array that indicates how to clip the
;                       original color table vectors. This is useful if you are
;                       restricting the number of colors, and do not which to have black
;                       or white (the usual color table end members) in the loaded color
;                       table. CLIP[0] is the lower bound. (A scalar value of CLIP is
;                       treated as CLIP[0].) CLIP[1] is the upper bound. For example, to
;                       load a blue-temperature color bar with only blue colors, you might
;                       type this::
;
;                           IDL> cgLoadCT, 1, CLIP=[110,240]
;                           IDL> CINDEX
;
;                       Or, alternatively, if you wanted to include white at the upper end
;                       of the color table::
;
;                           IDL> cgLoadCT, 1, CLIP=110
;                           IDL> CINDEX
;       FILENAME:   in, optional, type='string'
;                   The name of a color table file to open. By default colors1.tbl in the
;                       the IDL resource directory.
;       NCOLORS:    in, optional, type=integer, default=256
;                       The number of colors to be loaded into the color table.
;       REVERSE:    in, optional, type=boolean, default=0
;                   If this keyword is set, the color table vectors are reversed.
;       ROW:        in, optional, type=boolean, default=0
;                       Set this keyword to indicate you are getting the RGB_TABLE
;                       vectors for use in the IDL's object graphics routines. Whereas
;                       TVLCT expects color tables to be 256x3 (column vectors), the
;                       object graphics routines expect them to be 3x256 (row vectors).
;                       Setting this keyword will transpose the vectors before they are
;                       returned.
;       SILENT:     in, optional, type=boolean, default=1
;                   This keyword is provided ONLY for compatibility with LOADCT. All
;                       color table manipulations are handled silently.
;         
; :Examples:
;    Suppose you wanted to create a color table that displayed negative values with
;    red-temperature values and positive values with blue-temperature values, and you
;    would like the red-temperature values to be reversed in the color table (so dark
;    colors adjoin in the color table and indicate values near zero). You could do this::
;
;        cgLoadCT, 0
;        cgLoadCT, 3, /REVERSE, CLIP=[32,240], BOTTOM=1, NCOLORS=10
;        cgLoadCT, 1, CLIP=[64, 245], BOTTOM=11, NCOLORS=10
;        cgColorbar, NCOLORS=20, BOTTOM=1, DIV=10, RANGE=[-10,10]
;
;    Here is an example that shows the difference between LOADCT and cgLoadCT::
;
;        ERASE, COLOR=cgCOLOR('Charcoal)
;        LoadCT, 5, NCOLORS=8
;        cgColorbar, NCOLORS=8, DIVISIONS=8, POSITION=[0.1, 0.65, 0.9, 0.75], XMINOR=0, XTICKLEN=1
;        cgLoadCT, 5, NCOLORS=8, CLIP=[16, 240]
;        cgColorbar, NCOLORS=8, DIVISIONS=8, POSITION=[0.1, 0.35, 0.9, 0.45], XMINOR=0, XTICKLEN=1
;-
PRO MrColorPalette::LoadCT, table, $
BREWER=brewer, $
BOTTOM=bottom, $
CLIP = clip, $
FILENAME=file, $
NCOLORS=ncolors, $
REVERSE=reverse, $
ROW=row, $
SILENT=silent
	Compile_Opt idl2

	; Error handling.
	Catch, theError
	IF theError NE 0 THEN BEGIN
		catch, /CANCEL
		void = cgErrorMSG(/QUIET)
		IF N_Elements(lun) NE 0 THEN Free_Lun, lun
		RETURN
	ENDIF

	;Check keywords and arguments.
	;   !D.TABLE_SIZE must be established. This is done in ::INIT
	brewer  = Keyword_Set(brewer)
	reverse = Keyword_Set(reverse)
	IF N_Elements(file)    EQ 0 THEN file    = brewer ? self.ct_file_brewer : self.ct_file_idl 
	IF N_Elements(table)   EQ 0 THEN table   = 0
	IF N_Elements(bottom)  EQ 0 THEN bottom  = 0 ELSE bottom = 0 > bottom < (!D.TABLE_SIZE-1)
	IF N_Elements(clip)    EQ 0 THEN clip    = [0,255]
	IF N_Elements(clip)    EQ 1 THEN clip    = [clip, 255]
	IF N_Elements(ncolors) EQ 0 THEN ncolors = !D.TABLE_SIZE - bottom
	clip = 0 > clip < 255

	; Try to locate the brewer file. 
	IF brewer && file EQ '' THEN BEGIN
		Message, 'Cannot find the Brewer color table file "fsc_brewer.tbl."' + $
		         ' Using normal IDL color tables.', /INFORMATIONAL
		file = self.ct_file_idl
	ENDIF

	; Open and read the color table files.
	OPENR, lun, file, /GET_LUN
	ntables = 0B
	READU, lun, ntables

	; Make sure table number is within range.
	IF (table GE ntables) OR (table LT 0) THEN $
		Message, 'Table number must be from 0 to ' + StrTrim(Fix(ntables)-1,2) + '.'

	; Read the color table.
	;   - TABLE is the desired color table index, and each color
	;     table contains 3 sets of 256 values
	theTables = Assoc(lun, BytArr(256), 1)
	r = theTables[table*3]
	g = theTables[table*3+1]
	b = theTables[table*3+2]

	; Close the file.
	FREE_LUN, lun

	; Clip the colors.
	r = r[clip[0]:clip[1]]
	g = g[clip[0]:clip[1]]
	b = b[clip[0]:clip[1]]
	nclipcolors = (clip[1]-clip[0]) + 1

	; Interpolate to the number of colors asked for.
	IF ncolors NE nclipcolors THEN BEGIN
		p = (Lindgen(ncolors) * nclipcolors) / (ncolors-1)
		r = r[p]
		g = g[p]
		b = b[p]
	ENDIF

	; Need to reverse the colors?
	IF reverse THEN BEGIN
		r = Reverse(r)
		g = Reverse(g)
		b = Reverse(b)
	ENDIF

	; Save the color table.
	*self.rgb_table = [[r], [g], [b]]
	self.ncolors    = ncolors
END


;+
;   The purpose of this method is to set object properties. 
;
; :Keywords:
;-
pro MrColorPalette::SetProperty, $
BLUE_VALUES=blue_values, $
CTINDEX=ctindex, $
GREEN_VALUES=green_values, $
RED_VALUES=red_values, $
RGB_TABLE=rgb_table, $
ROW=row
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

;-------------------------------------------------------
; Check RGB Table //////////////////////////////////////
;-------------------------------------------------------
	;RGB_TABLE can be one of three thigns
	if n_elements(rgb_table) gt 0 then begin
		;What was given?
		;   - Scalar color table index
		;   - Vector of color names
		;   - 3xN or Nx3 array of color triples
		sz = size(rgb_table)
		case 1 of
			sz[0] eq 0 && sz[2] eq 1:                 ctidnex   = rgb_table
			sz[sz[0]+2] eq 7:                         the_table = cgColor(rgb_table, /TRIPLE)
			sz[0] eq 2 && (sz[1] eq 3 || sz[2] eq 3): the_table = rgb_table
			else: message, 'RGB_TABLE must be a colot table index, color names, or array of color triples.'
		endcase
	endif

;-------------------------------------------------------
; Mark Properties that are Being Set ///////////////////
;-------------------------------------------------------
	nr = n_elements(red_values)
	ng = n_elements(green_values)
	nb = n_elements(blue_values)
	
	;Return rows with ::GetProperty?
	if n_elements(row) gt 0 then self.row = keyword_set(row)

;-------------------------------------------------------
; RGB Table ////////////////////////////////////////////
;-------------------------------------------------------
	if n_elements(the_table) gt 0 then begin
		;Check size
		case 1 of
			sz[sz[0]+2]/3 gt 256: message, 'RGB_TABLE must have fewer than 256 RGB triples.'
			sz[2]         eq   3: *self.rgb_table = rgb_table
			sz[1]         eq   3: *self.rgb_table = transpose(rgb_table)
			else:                 message, 'RGB_TABLE must be an Nx3 array.'
		endcase
		
		;Set relevant properties
		self.ncolors = sz[sz[0]+2] / 3

;-------------------------------------------------------
; Red, Green, Blue Vectors /////////////////////////////
;-------------------------------------------------------
	endif else if nr + ng + nb gt 0 then begin
		;Make sure they have the same number of elements
		N = nr > ng > nb
		if nr ne N && nr ne 0 then message, 'Red, Green, and Blue vector lengths must match.'
		IF ng ne N && ng ne 0 then message, 'Red, Green, and Blue vector lengths must match.'
		if nb ne N && nb ne 0 then message, 'Red, Green, and Blue vector lengths must match.'
		if N gt 256 then message, 'Red, Green, and Blue vectors must have no more than 256 elements.'
		
		;Make sure all are defined
		if nr eq 0 then red_values   = bytarr(N)
		if ng eq 0 then green_values = bytarr(N)
		if nb eq 0 then blue_values  = bytarr(N)
		
		;Did the size of the color table change?
		if N ne self.ncolors then *self.rgb_table = bytarr(N, 3)
		
		;Update the color table
		if nr gt 0 then (*self.rgb_table)[0,0] = red_values
		if ng gt 0 then (*self.rgb_table)[0,1] = green_values
		if nb gt 0 then (*self.rgb_table)[0,2] = blue_values
		
		;Save number of colors
		self.ncolors = N

;-------------------------------------------------------
; Color Table //////////////////////////////////////////
;-------------------------------------------------------
	endif else if n_elements(ctindex) gt 0 then begin
		self -> LoadCT, ctindex, _STRICT_EXTRA=extra
	endif
end


;+
;   Set colors within the color palette
;
; :Params:
;       INDEX:      in, required, type=byte/bytarr
;                   Index into the color pallete at which colors will be saved
;       COLOR:      in, required, type=string/strarr/byte/bytarr/Nx3 bytarr
;                   A vector of color names, vector of red color values, or an
;                       array of RGB color triples.
;       G:          in, optional, type=bytarr
;                   Vector of green color values. Ignored unless `COLOR` is a vector of
;                       red color values.
;       B:          in, optional, type=bytarr
;                   Vector of blue color values. Ignored unless `COLOR` is a vector of
;                       red color values.
;-
pro MrColorPalette::SetRGB, index, color, g, b
	compile_opt idl2
	on_error, 2
	
	sz = size(color)
	
	;Were string color names given?
	if sz[sz[0]]+1 eq 7 then begin
		colors = cgColor(color, /TRIPLE)
		r      = colors[*,0]
		g      = colors[*,1]
		b      = (temporary(colors))[*,2]
	
	;Nx3 or 3xN array
	endif else if sz[0] eq 2 then begin
		if sz[2] eq 3 then begin
			r = color[*,0]
			g = color[*,1]
			b = color[*,2]
		endif else if sz[1] eq 3 then begin
			r = reform(color[0,*])
			g = reform(color[1,*])
			b = reform(color[2,*])
		endif else begin
			message, 'COLOR has incorrect size.'
		endelse
	
	;Red values
	endif else begin
		r = color
	endelse
	
	;Vec
	ni = n_elements(index)
	nr = n_elements(r)
	ng = n_elements(g)
	nb = n_elements(b)
	if ni ne nr || ni ne ng || ni ne nb then message, 'Index, Red, Green, and Blue vector lengths must match.'
	if max(index) gt self.ncolors then message, 'Given color index out of palette range.'
	
	;Set the colors
	(*self.rgb_table)[index,0] = r
	(*self.rgb_table)[index,1] = g
	(*self.rgb_table)[index,2] = b
end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrColorPalette::cleanup
	ptr_free, self.rgb_table
end


;+
;   Load colors into the current color table
;
; Calling Sequence::
;      color_palette = MrColorPalette()
;      color_palette = MrColorPalette(rgb_table)
;      color_palette = MrColorPalette(ctIndex)
;      color_palette = MrColorPalette( [r [, g [, b]]] )
;    
; :Params:
;       RGB:        in, optional, type=integer, default=current color table
;                   Either an Nx3 color table, a scalar color table index, or an
;                       N-element byte array of red color values.
;       G:          in, optional, type=bytarr
;                   Vector of green color values. Ignored unless `RGB` is a vector of
;                       red color values.
;       B:          in, optional, type=bytarr
;                   Vector of blue color values. Ignored unless `RGB` is a vector of
;                       red color values.
;       
; :Keywords:
;       BREWER:     in, optional, type=boolean, default=0
;                   Set this keyword if you wish to use the Brewer Colors, as
;                       implemented in the Coyote Library file, fsc_brewer.tbl.
;                       This program will look first in the $IDL_DIR/resource/colors 
;                       directory for the color table file, and failing to find it there
;                       will look in the same directory that the source code of this
;                       program is located, then in the IDL path. Finally, if it still
;                       can't find the file, it will ask you to locate it. If you can't
;                       find it, the program will simply return without loading a color
;                       table.
;       BOTTOM:     in, optional, type=integer, default=0
;                   The lowest color table index. The colors in the color table start
;                       loading here.
;       CLIP:       in, optional, type=integer
;                   A one- or two-element integer array that indicates how to clip the
;                       original color table vectors. This is useful if you are
;                       restricting the number of colors, and do not which to have black
;                       or white (the usual color table end members) in the loaded color
;                       table. CLIP[0] is the lower bound. (A scalar value of CLIP is
;                       treated as CLIP[0].) CLIP[1] is the upper bound. For example, to
;                       load a blue-temperature color bar with only blue colors, you might
;                       type this::
;
;                           IDL> cgLoadCT, 1, CLIP=[110,240]
;                           IDL> CINDEX
;
;                       Or, alternatively, if you wanted to include white at the upper end
;                       of the color table::
;
;                           IDL> cgLoadCT, 1, CLIP=110
;                           IDL> CINDEX
;       RGB_TABLE:  out, optional, type=btye
;                   If this keyword is set to a named variable, the color table is
;                       returned as an [NCOLORS,3] array and no colors are loaded in
;                       the display.
;       FILENAME:   in, optional, type='string'
;                   The name of a color table file to open. By default colors1.tbl in the
;                       the IDL resource directory.
;       NCOLORS:    in, optional, type=integer, default=256
;                       The number of colors to be loaded into the color table.
;       REVERSE:    in, optional, type=boolean, default=0
;                   If this keyword is set, the color table vectors are reversed.
;       ROW:        in, optional, type=boolean, default=0
;                       Set this keyword to indicate you are getting the RGB_TABLE
;                       vectors for use in the IDL's object graphics routines. Whereas
;                       TVLCT expects color tables to be 256x3 (column vectors), the
;                       object graphics routines expect them to be 3x256 (row vectors).
;                       Setting this keyword will transpose the vectors before they are
;                       returned.
;       SILENT:     in, optional, type=boolean, default=1
;                   This keyword is provided ONLY for compatibility with LOADCT. All
;                       color table manipulations are handled silently.
;-
FUNCTION MrColorPalette::Init, rgb, g, b, $
BOTTOM=bottom, $
BREWER=brewer, $
CLIP=clip, $
CTINDEX=ctindex, $
FILENAME=file, $
NCOLORS=ncolors, $
REVERSE=reverse, $
ROW=row, $
SILENT=silent
	compile_opt strictarr

	catch, theerror
	if theerror ne 0 then begin
		catch, /cancel
		if n_elements(rr) gt 0 then tvlct, rr, gg, bb
		MrPrintF, 'LogErr'
		return, 0
	endif

	; Be sure !D.TABLE_SIZE is established.
	IF (!D.NAME EQ 'X') AND (!D.WINDOW EQ -1) THEN BEGIN
		Window, /Free, /Pixmap, XSIZE=10, YSIZE=10
		WDelete, !D.WINDOW
	ENDIF
	
	;Number of parameters with data given
	nparams = n_elements(rgb)     eq 0 ? 0 $
	              : n_elements(g) eq 0 ? 1 $
	              : n_elements(b) eq 0 ? 2 $
	              : 3
	
	;
	; Obtain RGB_TABLE from the input parameters
	;   - Use the currently loaded color table
	;   - A scalar integer implies that a color table index was given
	;   - An array implies an RGB_TABLE
	;   - Red, green, blue values
	;
	case nparams of
		0:    tvlct, rgb_table, /GET
		1:    if MrIsA(rgb, /INTEGER, /SCALAR) then ctindex = rgb else rgb_table = rgb
		else: r = rgb
	endcase
	
	;Allocate heap to the color table
	self.rgb_table = ptr_new(/ALLOCATE_HEAP)
	
	;Default IDL color table
	self.ct_file_idl = Filepath('colors1.tbl', SUBDIRECTORY=['resource', 'colors'])
		
	;Brewer file
	brewerfilepath      = Filepath( ROOT_DIR=cgSourceDir(), 'fsc_brewer.tbl')
	self.ct_file_brewer = File_Search(brewerfilepath, Count=count)

	
	;Load a color table
	if n_elements(ctindex) gt 0 then begin
		self -> LoadCT, ctindex, $
		                BREWER   = brewer, $
		                BOTTOM   = bottom, $
		                CLIP     = clip, $
		                FILENAME = file, $
		                NCOLORS  = ncolors, $
		                REVERSE  = reverse, $
		                ROW      = row, $
		                SILENT   = silent
	
	;Save a color palette
	endif else begin
		self -> SetProperty, BLUE_VALUES  = b, $
		                     GREEN_VALUES = g, $
		                     RGB_TABLE    = rgb_table, $
		                     RED_VALUES   = r
	endelse

	return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrColorPalette__define, class
	compile_opt strictarr
	on_error, 2
	
	class = { MrColorPalette, $
	          inherits IDL_Object, $
	          ct_file_idl:    '', $
	          ct_file_brewer: '', $
	          file:           '', $
	          ncolors:        0U, $
	          rgb_table:      ptr_new() $      ;Color table to be loaded
	        }
end