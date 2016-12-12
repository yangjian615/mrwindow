; docformat = 'rst'
;
; NAME:
;       MrVector__Define
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
; PURPOSE:
;+
;   An object wrapper for cgVelocityVectors.
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMsg.pro
;       MrGraphicsKeywords__define.pro
;       MrGrDataAtom__define.pro
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
;       2014/03/26  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The purpose of this method is to print information about the object's properties
;   when the PRINT procedure is used.
;-
function MrVector::_OverloadPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, "''"
    endif
    
    undefined = '<undefined>'
    undefObj = '<NullObject>'
    default = '<IDL_Default>'
    joinStr = '   '
    
    ;First, get the results from the superclasses
    atomKeys = self -> MrGrAtom::_OverloadPrint()
    grKeys = self -> MrGraphicsKeywords::_OverloadPrint()
    layKeys = self.layout -> _OverloadPrint()

    ;Class Properties
    dimension = string('Dimension', '=', self.dimension, FORMAT='(a-26, a-2, i0)')
    nsum      = string('NSum',      '=', self.nsum,      FORMAT='(a-26, a-2, i1)')
    overplot  = string('OverPlot',  '=', self.overplot,  FORMAT='(a-26, a-2, i1)')
    polar     = string('Polar',     '=', self.polar,     FORMAT='(a-26, a-2, i1)')
    xlog      = string('Xlog',      '=', self.xlog,      FORMAT='(a-26, a-2, i1)')
    ylog      = string('YLog',      '=', self.ylog,      FORMAT='(a-26, a-2, i1)')
    ynozero   = string('YNoZero',   '=', self.ynozero,   FORMAT='(a-26, a-2, i1)')
    
    label     = string('Label', '=', "'" + self.label + "'", FORMAT='(a-26, a-2, a0)')
    
    max_value = string('Max_Value', '=', FORMAT='(a-26, a-2)')
    min_value = string('Min_Value', '=', FORMAT='(a-26, a-2)')
    symcolor  = string('SymColor',  '=', FORMAT='(a-26, a-2)')
    target    = string('Target',    '=', FORMAT='(a-26, a-2)')
    
    ;Pointers
    if n_elements(*self.max_value) eq 0 then max_value += default else max_value += string(*self.max_value, FORMAT='(f0)')
    if n_elements(*self.min_value) eq 0 then min_value += default else min_value += string(*self.min_value, FORMAT='(f0)')
    if n_elements(*self.symcolor)  eq 0 then symcolor += "''"     else symcolor  += strjoin(string(*self.symcolor, FORMAT='(a0)'), joinStr)
    if n_elements(*self.target) eq 0 then target += undefObj else target += strjoin(MrObj_Class(*self.target), joinStr)
    
    ;Put MrVector properties together
    selfStr = obj_class(self) + '  <' + strtrim(obj_valid(self, /GET_HEAP_IDENTIFIER), 2) + '>'
    plotKeys = [ dimension, $
                 nsum, $
                 overplot, $
                 polar, $
                 xlog, $
                 ylog, $
                 ynozero, $
                 label, $
                 max_value, $
                 min_value, $
                 symcolor, $
                 target $
               ]

    ;Group everything in alphabetical order
    result = [[atomKeys], [grKeys], [layKeys], [transpose(plotKeys)]]
    result = [[selfStr], ['  ' + transpose(result[sort(result)])]]
    
    return, result
end


;+
;   The purpose of this method is to print information about the object's properties
;   when implied print is used.
;-
function MrVector::_OverloadImpliedPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, "''"
    endif
    
    result = self -> _OverloadPrint()
    
    return, result
end


;+
;   The purpose of this method is to draw the plot in the draw window.
;-
pro MrVector::Draw, $
NOERASE = noerase
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	if self.hide then return

	;Do this in decomposed color
	cgSetColorState, 1, CURRENT=current_state

	;Draw the plot
	if self.overplot then begin
		;Restore target's coordinate system. Make sure that the overplot
		;is positioned correctly.
		self.target -> RestoreCoords
		position = [!x.window[0], !y.window[0], $
		            !x.window[1], !y.window[1]]
		self.layout -> SetProperty, POSITION=position, UPDATE_LAYOUT=0
	
		;Overplot
		if ~self.overplot then self -> doAxes
		self -> doVectors
		self -> SaveCoords
	endif else begin
		self -> doAxes, NOERASE=noerase
		self -> SaveCoords
		self -> PrepVectors
		self -> doVectors
	endelse
	
	;Save the dataspace
	self -> SaveCoords
	
	;Return to the original color state
	cgSetColorState, current_state
end


;+
;   The purpose of this method is to do the actual plotting. Basically, having this here
;   merely to saves space in the Draw method.
;
; :Private:
;-
pro MrVector::doAxes, $
NOERASE=noerase, $
XSTYLE=xstyle, $
YSTYLE=ystyle
	compile_opt strictarr
	on_error, 2

	self.layout -> GetProperty, CHARSIZE=charsize, POSITION=position
	
	background = cgColor(*self.background)
	color      = cgColor24(*self.color)

	;Draw the plot.
	plot, [0], [0], /NODATA, $
	      MAX_VALUE     = *self.max_value, $
	      MIN_VALUE     = *self.min_value, $
;	      NSUM          = *self.nsum, $
	      POLAR         =  self.polar, $
	      XLOG          =  self.xlog, $
	      YLOG          =  self.ylog, $
	      YNOZERO       =  self.ynozero, $
	      BACKGROUND    =       background, $
	      CHARSIZE      =       charsize, $
	      CHARTHICK     = *self.charthick, $
;	      CLIP          = *self.clip, $
	      COLOR         =       color, $
;	      DATA          = *self.data, $
;	      DEVICE        = *self.device, $
	      NORMAL        =       1, $
	      FONT          = *self.font, $
	      LINESTYLE     = *self.linestyle, $
;	      NOCLIP        = *self.noclip, $
	      NOERASE       = *self.noerase, $
	      POSITION      =       position, $
;	      PSYM          = *self.psym, $
	      SUBTITLE      = *self.subtitle, $
;	      SYMSIZE       = *self.symsize, $
	      T3D           = *self.t3D, $
	      THICK         = *self.thick, $
	      TICKLEN       = *self.ticklen, $
	      TITLE         = *self.title, $
	      XCHARSIZE     = *self.xcharsize, $
	      XGRIDSTYLE    = *self.xgridstyle, $
;	      XMARGIN       = *self.xmargin, $
	      XMINOR        = *self.xminor, $
	      XRANGE        = *self.xrange, $
	      XSTYLE        = *self.xstyle, $
	      XTHICK        = *self.xthick, $
;	      XTICK_GET     = *self.xtick_get, $
	      XTICKFORMAT   = *self.xtickformat, $
	      XTICKINTERVAL = *self.xtickinterval, $
	      XTICKLAYOUT   = *self.xticklayout, $
	      XTICKLEN      = *self.xticklen, $
	      XTICKNAME     = *self.xtickname, $
	      XTICKS        = *self.xticks, $
	      XTICKUNITS    = *self.xtickunits, $
	      XTICKV        = *self.xtickv, $
	      XTITLE        = *self.xtitle, $
	      YCHARSIZE     = *self.ycharsize, $
	      YGRIDSTYLE    = *self.ygridstyle, $
;	      YMARGIN       = *self.ymargin, $
	      YMINOR        = *self.yminor, $
	      YRANGE        = *self.yrange, $
	      YSTYLE        = *self.ystyle, $
	      YTHICK        = *self.ythick, $
;	      YTICK_GET     = *self.ytick_get, $
	      YTICKFORMAT   = *self.ytickformat, $
	      YTICKINTERVAL = *self.ytickinterval, $
	      YTICKLAYOUT   = *self.yticklayout, $
	      YTICKLEN      = *self.yticklen, $
	      YTICKNAME     = *self.ytickname, $
	      YTICKS        = *self.yticks, $
	      YTICKUNITS    = *self.ytickunits, $
	      YTICKV        = *self.ytickv, $
	      YTITLE        = *self.ytitle, $
	      ZCHARSIZE     = *self.zcharsize, $
	      ZGRIDSTYLE    = *self.zgridstyle, $
;	      ZMARGIN       = *self.zmargin, $
	      ZMINOR        = *self.zminor, $
	      ZRANGE        = *self.zrange, $
	      ZSTYLE        = *self.zstyle, $
	      ZTHICK        = *self.zthick, $
;	      ZTICK_GET     = *self.ztick_get, $
	      ZTICKFORMAT   = *self.ztickformat, $
	      ZTICKINTERVAL = *self.ztickinterval, $
	      ZTICKLAYOUT   = *self.zticklayout, $
	      ZTICKLEN      = *self.zticklen, $
	      ZTICKNAME     = *self.ztickname, $
	      ZTICKS        = *self.zticks, $
	      ZTICKUNITS    = *self.ztickunits, $
	      ZTICKV        = *self.ztickv, $
	      ZTITLE        = *self.ztitle, $
	      ZVALUE        = *self.zvalue
end




;+
;   The purpose of this method is to do the actual plotting. Basically, having this here
;   merely to saves space in the Draw method.
;
; :Private:
;-
pro MrVector::doVectors, $
NOERASE=noerase
	compile_opt strictarr
	on_error, 2

	;Get the colors
	color = cgColor24(*self.vector_colors)

	; Process each arrow in the vector.
	FOR i = 0L, N_Elements(*self.vx)-1 DO BEGIN   

		;Vertices are saved in NORMAL coordinates. Must convert to DEVICE
;		p = self -> ConvertCoord((*self.xvert)[*,i], (*self.yvert)[*,i], /NORMAL, /TO_DEVICE)

		;Arrow shaft for PLOTS
		x_shaft = (*self.xvert)[0:1,i]   ;[p[0,0], p[0,1]]
		y_shaft = (*self.yvert)[0:1,i]   ;[p[1,0], p[1,1]]
		
		;Arrowhead for POLYFILL
		x_head = (*self.xvert)[[1,2,4,3,1],i]   ;[p[0,1], p[0,2], p[0,4], p[0,3], p[0,1]]
		y_head = (*self.yvert)[[1,2,4,3,1],i]   ;[p[1,1], p[1,2], p[1,4], p[1,3], p[1,1]]

		;Filled Arrow Head
		IF self.arrow_style eq 1 THEN BEGIN
			Plots, x_shaft, y_shaft, /DEVICE, $
;			       CLIP      = *self.clip, $
			       COLOR     =       color[i], $
			       LINESTYLE = *self.linestyle, $
			       NOCLIP    = *self.noclip, $
;			       PSYM      = *self.psym, $
;			       SYMSIZE   = *self.symsize, $
			       T3D       = *self.t3d, $
			       THICK     =  self.arrow_thick;, $
;			       Z         = *self.z
			Polyfill, x_head, y_head, /DEVICE, $
;			          LINE_FILL   = *self.line_fill, $
;			          PATTERN     = *self.pattern, $
;			          SPACING     = *self.spacing, $
;			          TRANSPARENT = *self.transparent, $
;			          CLIP        = *self.clip, $
			          COLOR       =       color[i], $
;			          LINESTYLE   = *self.linestyle, $
			          NOCLIP      = *self.noclip, $
;			          ORIENTATION = *self.orientation, $
			          T3D         = *self.t3d, $
			          THICK       = *self.thick;, $
;			          Z           = *self.z
		ENDIF ELSE BEGIN
			Plots, x_shaft, y_shaft, /DEVICE, $
;			       CLIP      = *self.clip, $
			       COLOR       =       color[i], $
			       LINESTYLE = *self.linestyle, $
			       NOCLIP    = *self.noclip, $
;			       PSYM      = *self.psym, $
;			       SYMSIZE   = *self.symsize, $
			       T3D       = *self.t3d, $
			       THICK     =  self.arrow_thick;, $
;			       Z         = *self.z
			PlotS, x_head, y_head, /DEVICE, $
;			          LINE_FILL   = *self.line_fill, $
;			          PATTERN     = *self.pattern, $
;			          SPACING     = *self.spacing, $
;			          TRANSPARENT = *self.transparent, $
;			          CLIP        = *self.clip, $
			          COLOR       =       color[i], $
;			          LINESTYLE   = *self.linestyle, $
			          NOCLIP      = *self.noclip, $
;			          ORIENTATION = *self.orientation, $
			          T3D         = *self.t3d, $
			          THICK       = *self.thick;, $
;			          Z           = *self.z
		ENDELSE
	ENDFOR
	
end


;+
;   The purpose of this method is to retrieve data
;
; :Examples:
;   To get the vector sizes::
;       myGraphic -> GetData, velx, vely
;
;   To get the vector sizes and their locations::
;       myGraphic -> GetData, velx, vely, posx, posy
;
; :Params:
;       VELX:           out, required, type=integer/float
;                       An array containing the X component of the particle velocity vector.
;       VELY:           out, required, type=integer/float
;                       An array containing the Y component of the particle velocity vector.
;       POSX:           out, optional, type=integer/float
;                       An array containing the X posiiton of the particle velocity vector. The
;                           shaft end of the arrow vector is positioned here.
;       POSY:           out, optional, type=integer/float
;                       An array containing the Y posiiton of the particle velocity vector. The
;                           shaft end of the arrow vector is positioned here.
;-
pro MrVector::GetData, velx, vely, posx, posy
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Retrieve the data
    case n_params() of
        2: begin
            velx = *self.velx
            vely = *self.vely
        endcase
        
        4: begin
            velx = *self.velx
            vely = *self.vely
            posx = *self.posx
            poxy = *self.posy
        endcase
        
        else: message, 'Incorrect number of parameters.'
    endcase
end


;+
;   The purpose of this method is to retrieve object properties
;
; :Keywords:
;       CHARSIZE:       out, optional, type=float
;                       Scale factor for IDL's default character size.
;       FRACTION:       out, optional, type=float, default=1.0
;                       A number between 0.0 and 1.0 indicating the fraction of the vectors to
;                           draw on the plot. Vectors are selected randomly from the total population,
;                           unless the `Ordered` keyword is set, in which case they are selected
;                           in an ordered, systematic fashion. For example, Fraction=0.5 will select
;                           every other input vector.
;       HIDE:           out, optional, type=boolean, default=0
;                       If set, the graphic will not be displayed.
;       HSIZE:          out, optional, type=float
;                       The size of the the arrow head. By default 1/100th the width
;                           of the device. (!D.X_SIZE / 100.)
;       HTHICK:         out, optional, type=float
;                       The thickness of the line drawing the arrowhead of the vector. The
;                           default is 3 for the PostScript device and 1 for all other devices.
;       LAYOUT:         out, optional, type=intarr(3)
;                       A vector of the form [nCols, nRows, pIndex], where the first
;                           two elements are the number of columns and rows of plots
;                           whithin the plotting grid, and pIndex is the plot index,
;                           starting with 1 (one) in the upper-left corner of the
;                           grid and increasing first left-to-right then top-to-bottom.
;                           If LAYOUT and `POSITION` are not provided, the graphic
;                           will be placed at the next avaible pIndex location.
;       LENGTH:         out, optional, type=float, default=0.075
;                       The length of the `ReferenceVector` in normalized units. All vectors 
;                           are scaled according to this length.
;       LINESTYLE:      out, optional, type=integer, default=0
;                       The line style of the arrow. Line style integers as in PLOT.
;       MAPCOORD:       out, optional, type=object
;                       A map coordinate object that describes the map projection
;                           and datum used to specify the vector locations.
;       NAME:           out, optional, type=string, default=Object class of the graphic
;                       A string specifying the name of the graphic. It can be used
;                           retrieve the graphic using bracket array notation.
;       ORDERED:        out, optional, type=boolean, default=0
;                       If this keyword is set, and the `Fraction` keyword is used, the fraction
;                           of vectors used in the plot are chosen from the entire population in a
;                           systematic, ordered fashion.
;       POSITION:       out, optional, type=fltarr(4)
;                       A vector of the form [x0, y0, x1, y1], where (x0,y0) specify
;                           the location of the lower-left corner and (x1,y1) specify
;                           the location of the upper-right corner of the graphic.
;       SOLID:          out, optional, type=boolean, default=0
;                       Set this keyword to draw solid, filled arrows.
;       THICK:          out, optional, type=float
;                       The thickness of the line drawing the shaft of the arrow. The
;                           default is 3 for the PostScript device and 1 for all other devices.
;       V_CLIP:         out, optional, type=float
;                       A four-element array giving the clipping rectangle [xo, yo, x1, y1].
;                           The default clipping rectangle is the plot area. See the
;                           documenation for the IDL graphics keyword CLIP.
;       V_COLORS:       out, optional
;                       A scalar or vector of colors the same size as `velx`. May be bytes,
;                           short integers, or strings. Bytes and short integers are
;                           treated as indices into the current color  table. The default
;                           is "opposite".
;       V_NOCLIP:       out, optional, type=boolean, default=1
;                       Set the keyword to zero to clip vector output to the clipping
;                           rectangle specified in the `V_CLIP` keyword. The default is
;                           to leave the vectors unclipped. See the documentation for the
;                           IDL graphics keyword NOCLIP.
;       V_REF:          out, optional, type=float
;                       The magnitude of a reference vector that is used to scale all other
;                           vectors before display.
;       _EXTRA:         out, optional
;                       Any keywords appropriate for any superclass can be used to
;                           create the graphic.
;-
pro MrVector::GetProperty, $
;Vector Properties
ARROW_STYLE=arrow_style, $
ARROW_THICK=arrow_thick, $
DATA_LOCATION=data_location, $
HEAD_ANGLE=head_angle, $
HEAD_INDENT=head_indent, $
HEAD_PROPORTIONAL=head_proportional, $
HEAD_SIZE=head_size, $
LENGTH_SCALE=length_scale, $
RGB_TABLE=rgb_table, $
SYMBOL=symbol, $
SYM_COLOR=sym_color, $
SYM_FILLED=sym_filled, $
SYM_FILL_COLOR=sym_fill_color, $
SYM_SIZE=sym_size, $
SYM_THICK=sym_thick, $
USE_DEFAULT_COLOR=use_default_color, $
VECTOR_COLORS=vector_colors, $

;Vector Keywords
FRACTION=fraction, $
HTHICK=hthick, $
LINESTYLE=linestyle, $
ORDERED=ordered, $
THICK=thick, $

;Direct Graphics Keywords
_REF_EXTRA=extra
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif
	
	;MrVector Properties
	if arg_present(arrow_style)       gt 0 then arrow_style       =  self.arrow_style
	if arg_present(arrow_thick)       gt 0 then arrow_thick       =  self.arrow_thick
	if arg_present(data_location)     gt 0 then data_location     =  self.data_location
	if arg_present(head_angle)        gt 0 then head_angle        =  self.head_angle
	if arg_present(head_indent)       gt 0 then head_indent       =  self.head_indent
	if arg_present(head_proportional) gt 0 then head_proportional =  self.head_proportional
	if arg_present(head_size)         gt 0 then head_size         =  self.head_size
	if arg_present(length_scale)      gt 0 then length_scale      =  self.length_scale
	if arg_present(rgb_table)         gt 0 then self.palette     ->  GetProperty, RGB_TABLE=rgb_table
	if arg_present(symbol)            gt 0 then symbol            = *self.psym
	if arg_present(sym_color)         gt 0 then sym_color         =  self.sym_color
	if arg_present(sym_size)          gt 0 then sym_size          =  self.sym_size
	if arg_present(sym_filled)        gt 0 then sym_filled        =  self.sym_filled
	if arg_present(sym_fill_color)    gt 0 then sym_fill_color    =  self.sym_fill_color
	if arg_present(sym_thick)         gt 0 then sym_thick         =  self.sym_thick
	if arg_present(use_default_color) gt 0 then use_default_color =  self.use_default_color
	if arg_present(vector_colors)     gt 0 then vector_colors     = *self.vector_colors

	;MrVector Properties
	if arg_present(fraction)  then fraction  =  self.fraction
	if arg_present(hsize)     then hsize     =  self.hsize
	if arg_present(hthick)    then hthick    =  self.hthick
	if arg_present(length)    then length    =  self.length
	if arg_present(linestyle) then linestyle = *self.linestyle
	if arg_present(ordered)   then ordered   =  self.ordered
	if arg_present(solid)     then solid     =  self.solid
	if arg_present(thick)     then thick     = *self.thick
	if arg_present(v_clip)    then v_clip    = *self.v_clip
	if arg_present(v_colors)  then v_colors  = *self.v_colors
	if arg_present(v_noclip)  then v_noclip  =  self.v_noclip
	if arg_present(v_ref)     then v_ref     = *self.v_ref

	;MrDataAtom Properties
	if arg_present(hide)      then hide        = self.hide
	if arg_present(name)      then name        = self.name
	if arg_present(position)  then position    = self.layout -> GetPosition()
	if arg_present(layout)    then layout      = self.layout -> GetLayout()
	if arg_present(charsize)  then self.layout -> GetProperty, CHARSIZE=charsize

	if arg_present(mapCoord) then begin
		if ptr_valid(self.mapcoord) && obj_valid(*self.mapCoord) $
			then mapCoord = self.mapCoord $
			else mapCoord = obj_new()
	endif

	;Superclass Keywords
	if n_elements(extra) gt 0 $
		then self -> MrGrDataAtom::GetProperty, _STRICT_EXTRA=extra
end



;+
;   The purpose of this method is to do the actual plotting. Basically, having this here
;   merely to saves space in the Draw method.
;
; :Private:
;-
pro MrVector::PrepVectors
	compile_opt strictarr
	on_error, 2

;---------------------------------------------------------------------
; Scale Vectors //////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Calculate the angle (radians) of the vector [-pi, pi].
	angle = atan(*self.vy, double(*self.vx))

	; Calculate scaled velocities in normalized coordinate units.
	;   - Abs(VX) / |V| is the normalized magntitude
	;   - LENGTH_SCALE * Cos(ANGLE) is the scaled magnitude and orientation of the vector
	scaled_vx = self.scale * self.length_scale * (abs(*self.vx) * cos(angle) / self.vmag)
	scaled_vy = self.scale * self.length_scale * (abs(*self.vy) * sin(angle) / self.vmag)
;	scaled_vx = self.scale * self.length_scale * (*self.vx / self.vmag)
;	scaled_vy = self.scale * self.length_scale * (*self.vy / self.vmag)

;---------------------------------------------------------------------
; Caclulate Endpoints ////////////////////////////////////////////////
;---------------------------------------------------------------------

	; What kind of coordinate system are you using? You need to know to 
	; calcuate the arrow end of the vector.
	case 1 of
		self.device: begin
			xy = self -> ConvertCoord(*self.x, *self.y, /DEVICE, /TO_NORMAL)
			x0 = reform(xy[0,*])
			y0 = reform(xy[1,*])
		endcase

		self.normal: begin  
			x0 = *self.x
			y0 = *self.y
		endcase

		else: begin
			xy = self -> ConvertCoord(*self.x, *self.y, /DATA, /TO_NORMAL)
			x0 = reform(xy[0,*])
			y0 = reform(xy[1,*])
		endcase
	endcase

	;Make sure (x,y) have the same dimensions as (vx,vy)
	xsz  = size(x0)
	ysz  = size(y0)
	vxsz = size(scaled_vx)
	vysz = size(scaled_vy)
	if xsz[0] eq 1 && vxsz[0] eq 2 then x0 = rebin(x0, xsz[xsz[0]+2], ysz[ysz[0]+2])
	if ysz[0] eq 1 && vysz[0] eq 2 then y0 = rebin(reform(y0, 1, ysz[ysz[0]+2]), xsz[xsz[0]+2], ysz[ysz[0]+2])
	
	;Location of the tip of the arrow
	x1 = x0 + scaled_vx
	y1 = y0 + scaled_vy

;---------------------------------------------------------------------
; Re-Center //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	if self.data_location ne 0 then begin
		;Cases for DATA_LOCATION
		;   1 - Move (x1,y1) toward (x0,y0) by half the  vector length
		;   2 - Move (x1,y1) toward (x0,y0) by the whole vector length
		factor = self.data_location eq 1B ? -0.5 : -1.0
		
		;Length of vector in x and y
		dx = factor * (x1 - x0) 
		dy = factor * (y1 - y0)
		
		;Displace vector to re-center.
		x0 += dx
		y0 += dy
		x1 += temporary(dx)
		y1 += temporary(dy)
	endif

;---------------------------------------------------------------------
; Arrow Head //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;
	; If the arrow tail is anchored at A (x0,y0) and points toward
	; B (x1,y1), then the vector pointing from A to B is given by
	;
	;     v = (x1-x0, y1-y0)
	;
	; We rotate this vector by the angle +/- HEAD_ANGLE to obtain
	; vectors that are parallel to the edges of the arrow head.
	;
	;     v' = | cos(phi)  -sin(phi) | v
	;          | sin(phi)   cos(phi) |
	;
	;     vx' = (x1-x0)*cos(phi) - (y1-y0)*sin(phi)
	;     vy' = (x1-x0)*sin(phi) + (y1-y0)*cos(phi)
	;
	;     vx' = dx*cos(phi) - dy*sin(phi)
	;     vy' = dx*sin(phi) + dy*cos(phi)
	;
	; Next, multiply by a scale factor to make the head the correct
	; size
	;
	;     v' =  s * v'
	;
	; This scale factor is expressed as a fraction of the magnitude
	; of the vector. This can be achieved by turning v into a unit
	; vector like so
	;
	;     s = % * |v|
	;
	;     vx' = s * ( dx/|v| * cos(phi) - dy/|v| * sin(phi) )
	;     vy' = s * ( dx/|v| * sin(phi) + dy/|v| * cos(phi) )
	;
	; If HEAD_PROPORTIONAL is set, we want the scale factor to be
	; a percentage of the individual vector. If it is not set, then
	; it is a percentage of the median vector magnitude.
	;
	; Finally, we subtract v' from v to obtain the location of the
	; vertices of the arrowhead.
	;

	;Convert to device coordinates
	;   - This prevents the drawn vectors from being distorted when
	;     the window is resized.
	p0 = self -> ConvertCoord( x0, y0, /NORMAL, /TO_DEVICE)
	p1 = self -> ConvertCoord( x1, y1, /NORMAL, /TO_DEVICE)

	if vxsz[0] eq 2 then begin
		;Recover previous dimensions
		p0 = reform(p0, 3, vxsz[1], vxsz[2])
		p1 = reform(p1, 3, vxsz[1], vxsz[2])
		
		;Extract points.
		x0 = reform(p0[0,*,*])
		x1 = reform(p1[0,*,*])
		y0 = reform((temporary(p0))[1,*,*])
		y1 = reform((temporary(p1))[1,*,*])
	endif else begin
		x0 = reform(p0[0,*])
		x1 = reform(p1[0,*])
		y0 = reform((temporary(p0))[1,*])
		y1 = reform((temporary(p1))[1,*])
	endelse
		

	;Vector components and angle
	dx    = x1 - x0
	dy    = y1 - y0
	angle = self.head_angle * !dpi/180.0D

	;Normalize, but watch out for |v| = 0
	vmag  = sqrt(dx^2 + dy^2)
	igood = where(vmag ne 0, ngood, COMPLEMENT=ibad, NCOMPLEMENT=nbad)
	if ngood gt 0 then begin
		dx[igood] /= vmag[igood]
		dy[igood] /= vmag[igood]
	endif
	if nbad gt 0 then begin
		dx[ibad] = 0
		dy[ibad] = 0
	endif

	;Scale factor
	if self.head_proportional $
		then hsize = self.head_size * vmag $
		else hsize = self.head_size * (n_elements(vmag) eq 1 ? vmag : median(vmag))

	;Compute vertices of head
	x2 = x1 - hsize * ( dx*cos(angle) - dy*sin(angle) )
	y2 = y1 - hsize * ( dx*sin(angle) + dy*cos(angle) )
	x3 = x1 - hsize * ( dx*cos(-angle) - dy*sin(-angle) )
	y3 = y1 - hsize * ( dx*sin(-angle) + dy*cos(-angle) )

;---------------------------------------------------------------------
; Connect Back to Shaft //////////////////////////////////////////////
;---------------------------------------------------------------------

	;
	; Now, we want to determine where these vertices connect back to the shaft.
	; This is determined, in part, by HEAD_INDENT. Since the vertex we are
	; looking for is along the arrow shaft, we can perform the same methodology
	; as above, but with an angle of 0 degrees.
	;
	;     v' =  s * | cos(0)  -sin(0) | v
	;               | sin(0)   cos(0) |
	;
	;     vx' = s * (x1-x0)
	;     vy' = s * (y1-y0)
	;
	; The scale factor in this case is the projection of the arrow head onto
	; the arrow shaft, normalized by the length of the arrow.
	;
	;     dx_shaft = (x1 - x0)
	;     dy_shaft = (y1 - y0)
	;     dx_head  = (x3 - x1)
	;     dy_head  = (y3 - y1)
	;     s = (dx_head,  dy_head)  dot (dx_shaft, dy_shaft) / sqrt(dx_shaft^2 + dy_shaft^2)
	;       = (dx_head * dx_shaft   +   dy_head * dy_shaft) / sqrt(dx_shaft^2 + dy_shaft^2)
	;
	; Note that above, we have already divided dx and dy by |V|, so there is
	; no need in the step below to divide by the magnitude.
	;
	; But then we need to adjust by HEAD_INDENT.
	;
	s  = ( (x3-x1)*dx + (y3-y1)*dy ) ;/ vmag
	s *= (1.0 - self.head_indent)
	x4 = x1 + s*temporary(dx)
	y4 = y1 + temporary(s)*temporary(dy)

;---------------------------------------------------------------------
; Save the Data //////////////////////////////////////////////////////
;---------------------------------------------------------------------
	if vxsz[0] eq 2 then begin
		*self.xvert = transpose([[[x0]], [[x1]], [[x2]], [[x3]], [[x4]]])
		*self.yvert = transpose([[[y0]], [[y1]], [[y2]], [[y3]], [[y4]]])
		*self.xvert = reform(*self.xvert, 5, vxsz[vxsz[0]+2], /OVERWRITE)
		*self.yvert = reform(*self.yvert, 5, vysz[vysz[0]+2], /OVERWRITE)
	endif else begin
		*self.xvert = transpose([[x0], [x1], [x2], [x3], [x4]])
		*self.yvert = transpose([[y0], [y1], [y2], [y3], [y4]])
	endelse
end


;+
;   The purpose of this method is to set data
;
; :Examples:
;   To set the vector sizes::
;       myGraphic -> SetData, velx, vely
;
;   To set the vector sizes and their locations::
;       myGraphic -> SetData, velx, vely, posx, posy
;
; :Params:
;       VELX:           in, required, type=integer/float
;                       An array containing the X component of the particle velocity vector.
;       VELY:           in, required, type=integer/float
;                       An array containing the Y component of the particle velocity vector.
;       POSX:           in, optional, type=integer/float
;                       An array containing the X posiiton of the particle velocity vector. The
;                           shaft end of the arrow vector is positioned here.
;       POSY:           in, optional, type=integer/float
;                       An array containing the Y posiiton of the particle velocity vector. The
;                           shaft end of the arrow vector is positioned here.
;-
pro MrVector::SetData, vx, vy, x, y
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Number of parameters with valid data
	nparams = n_elements(vx)       eq 0 ? 0 $
	              : n_elements(vy) eq 0 ? 1 $
	              : n_elements(x)  eq 0 ? 2 $
	              : n_elements(y)  eq 0 ? 3 $
	              : 4
	if nparams ne 2 && nparams ne 4 then message, 'Incorrect number of parameters.'
	
	;Vx and Vy must have the same dimensions
	vxsz = size(vx)
	vysz = size(vy)
	if ~array_equal(vxsz[1:vxsz[0]], vysz[1:vysz[0]]) $
		then message, 'VX and VY must have the same dimension sizes.'
	
	;Number of elements along the x- and y-dimensions
	nx = vxsz[0] eq 1 ? vxsz[1] : vxsz[2]
	ny = vysz[0] eq 1 ? vysz[1] : vysz[2]
	
	;Check X and Y
	if nparams eq 4 then begin
		xsz = size(x)
		ysz = size(y)
		
		;If X does not match new data size, create default positions
		;   - X-dimension of X must have NX elements
		;   - Y-dimension of X must have NY elements, if X is 2D
		;   - If one of these condisions is not met, create default position
		if (xsz[1] ne nx) || (xsz[0] eq 2 && xsz[2] ne ny) $
			then message, 'X must be an NX or NXxNY array.'
			
		;If Y does not match new data size, create default positions
		;   - Y-dimension of Y must have NY elements
		;   - X-dimension of Y must have NX elements, if Y is 2D
		;   - If one of these condisions is not met, create default position
		if ((ysz[0] eq 1 && ysz[1] ne ny) || (ysz[0] eq 2 && ysz[2] ne ny)) || $
		   (ysz[0] eq 2 && ysz[1] ne nx) $
			then message, 'Y must be an NY OR NXxNY array.'
			
		;Save the data
		*self.x  = x
		*self.y  = y
		*self.vx = vx
		*self.vy = vy
		
		
	;Must define positions
	endif else begin
		xsz = size(*self.x)
		ysz = size(*self.y)
		
		;If X does not match new data size, create default positions
		;   - X-dimension of X must have NX elements
		;   - Y-dimension of X must have NY elements, if X is 2D
		;   - If one of these condisions is not met, create default position
		if (xsz[1] ne nx) || (xsz[0] eq 2 && xsz[2] ne ny) $
			then *self.x = lonarr(vxsz[1])
			
		;If Y does not match new data size, create default positions
		;   - Y-dimension of Y must have NY elements
		;   - X-dimension of Y must have NX elements, if Y is 2D
		;   - If one of these condisions is not met, create default position
		if ((ysz[0] eq 1 && ysz[1] ne ny) || (ysz[0] eq 2 && ysz[2] ne ny)) || $
		   (ysz[0] eq 2 && ysz[1] ne nx) $
			then *self.y = vysz[0] eq 1 ? lonarr(vysz[1]) : lonarr(vysz[2])
		
		;Set the data
		*self.vx = vx
		*self.vy = vy
	endelse
	
	;Get the magntitude of the vectors
	self.vmag = double(max(sqrt((*self.vx)^2 + (*self.vy)^2)))
	
	;Define ranges
	*self.xrange = [min(*self.x, MAX=xmax), xmax]
	*self.yrange = [min(*self.y, MAX=ymax), ymax]

	;Refresh the graphics window
	self.window -> Draw
end


;+
;   The purpose of this method is to set object properties. 
;
; :Keywords:
;       CHARSIZE:       in, optional, type=float
;                       Scale factor for IDL's default character size.
;       FRACTION:       in, optional, type=float, default=1.0
;                       A number between 0.0 and 1.0 indicating the fraction of the vectors to
;                           draw on the plot. Vectors are selected randomly from the total population,
;                           unless the `Ordered` keyword is set, in which case they are selected
;                           in an ordered, systematic fashion. For example, Fraction=0.5 will select
;                           every other input vector.
;       HIDE:           in, optional, type=boolean, default=0
;                       If set, the graphic will not be displayed.
;       HSIZE:          in, optional, type=float
;                       The size of the the arrow head. By default 1/100th the width
;                           of the device. (!D.X_SIZE / 100.)
;       HTHICK:         in, optional, type=float
;                       The thickness of the line drawing the arrowhead of the vector. The
;                           default is 3 for the PostScript device and 1 for all other devices.
;       LAYOUT:         in, optional, type=intarr(3)
;                       A vector of the form [nCols, nRows, pIndex], where the first
;                           two elements are the number of columns and rows of plots
;                           whithin the plotting grid, and pIndex is the plot index,
;                           starting with 1 (one) in the upper-left corner of the
;                           grid and increasing first left-to-right then top-to-bottom.
;                           If LAYOUT and `POSITION` are not provided, the graphic
;                           will be placed at the next avaible pIndex location.
;       LENGTH:         in, optional, type=float, default=0.075
;                       The length of the `ReferenceVector` in normalized units. All vectors 
;                           are scaled according to this length.
;       LINESTYLE:      in, optional, type=integer, default=0
;                       The line style of the arrow. Line style integers as in PLOT.
;       MAPCOORD:       in, optional, type=object
;                       A map coordinate object (e.g., cgMap) that describes the map projection
;                           and datum used to specify the vector locations. Note that this could also be a 
;                           map structure as returned from MAP_PROJ_INIT, but in that case the user is 
;                           responsible for setting up the XY map coordinate space independently and 
;                           outside of this program. This coordinate object will be used to transform
;                           lat/lon locations into the XY locations of the map projection.
;       NAME:           in, optional, type=string, default=Object class of the graphic
;                       A string specifying the name of the graphic. It can be used
;                           retrieve the graphic using bracket array notation.
;       ORDERED:        in, optional, type=boolean, default=0
;                       If this keyword is set, and the `Fraction` keyword is used, the fraction
;                           of vectors used in the plot are chosen from the entire population in a
;                           systematic, ordered fashion.
;       POSITION:       in, optional, type=fltarr(4)
;                       A vector of the form [x0, y0, x1, y1], where (x0,y0) specify
;                           the location of the lower-left corner and (x1,y1) specify
;                           the location of the upper-right corner of the graphic.
;       SOLID:          in, optional, type=boolean, default=0
;                       Set this keyword to draw solid, filled arrows.
;       THICK:          in, optional, type=float
;                       The thickness of the line drawing the shaft of the arrow. The
;                           default is 3 for the PostScript device and 1 for all other devices.
;       V_CLIP:         in, optional, type=float
;                       A four-element array giving the clipping rectangle [xo, yo, x1, y1].
;                           The default clipping rectangle is the plot area. See the
;                           documenation for the IDL graphics keyword CLIP.
;       V_COLORS:       in, optional
;                       A scalar or vector of colors the same size as `velx`. May be bytes,
;                           short integers, or strings. Bytes and short integers are
;                           treated as indices into the current color  table. The default
;                           is "opposite".
;       V_NOCLIP:       in, optional, type=boolean, default=1
;                       Set the keyword to zero to clip vector output to the clipping
;                           rectangle specified in the `V_CLIP` keyword. The default is
;                           to leave the vectors unclipped. See the documentation for the
;                           IDL graphics keyword NOCLIP.
;       V_REF:          in, optional, type=float
;                       The magnitude of a reference vector that is used to scale all other
;                           vectors before display.
;       _EXTRA:         in, optional
;                       Any keywords appropriate for any superclass can be used to
;                           create the graphic.
;-
pro MrVector::SetProperty, $
;Vector Properties
ARROW_STYLE=arrow_style, $
ARROW_THICK=arrow_thick, $
DATA_LOCATION=data_location, $
HEAD_ANGLE=head_angle, $
HEAD_INDENT=head_indent, $
HEAD_PROPORTIONAL=head_proportional, $
HEAD_SIZE=head_size, $
LENGTH_SCALE=length_scale, $
MIN_VALUE=min_value, $
MAX_VALUE=max_value, $
RGB_TABLE=rgb_table, $
SYMBOL=symbol, $
SYM_COLOR=sym_color, $
SYM_FILLED=sym_filled, $
SYM_FILL_COLOR=sym_fill_color, $
SYM_SIZE=sym_size, $
SYM_THICK=sym_thick, $
USE_DEFAULT_COLOR=use_default_color, $
VECTOR_COLORS=vector_colors, $

;Vector Keywords
FRACTION=fraction, $
HTHICK=hthick, $
LINESTYLE=linestyle, $
ORDERED=ordered, $
THICK=thick, $

;Direct Graphics Properties
COLOR=color, $
PSYM=psym, $
SYMSIZE=symsize, $
XSTYLE=xstyle, $
XTITLE=xtitle, $
YSTYLE=ystyle, $
YTITLE=ytitle, $
ZTITLE=ztitle, $
_REF_EXTRA=extra
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif
	
	;MrVector Properties
	if n_elements(arrow_style)       gt 0 then  self.arrow_style       = arrow_style
	if n_elements(arrow_thick)       gt 0 then  self.arrow_thick       = arrow_thick
	if n_elements(data_location)     gt 0 then  self.data_location     =  0   < data_location < 2
	if n_elements(head_angle)        gt 0 then  self.head_angle        =  0.0 < head_angle    < 90.0
	if n_elements(head_indent)       gt 0 then  self.head_indent       = -1.0 < head_indent   < 1.0
	if n_elements(head_proportional) gt 0 then  self.head_proportional = keyword_set(head_proportional)
	if n_elements(head_size)         gt 0 then  self.head_size         = head_size
	if n_elements(length_scale)      gt 0 then  self.length_scale      = length_scale
	if n_elements(min_value)         gt 0 then *self.min_value         = min_value
	if n_elements(max_value)         gt 0 then *self.max_value         = max_value
	if n_elements(symbol)            gt 0 then *self.psym              = symbol
	if n_elements(sym_color)         gt 0 then  self.sym_color         = cgColor(sym_color)
	if n_elements(sym_size)          gt 0 then *self.sym_size          = sym_size
	if n_elements(sym_thick)         gt 0 then  self.sym_thick         = sym_thick
	
	;Default Color
	ncolor = n_elements(color)
	ndefc  = n_elements(use_default_color)
	if ncolor + ndefc gt 0 then begin
		tf_defc = 0B

		;COLOR
		if ncolor gt 0 then begin
			_color = cgColor(color, /TRIPLE)
			if ~array_equal(_color, *self.color) then begin
				tf_defc = 1B
				*self.color = temporary(_color)
			endif
		endif
	
		;USE_DEFAULT_COLOR
		if ndefc gt 0 then begin
			usedefc = keyword_set(use_default_color)
			if usedefc && ~self.use_default_color then begin
				tf_defc = 1B
				self.use_default_color = temporary(usedefc)
			endif
		endif

		;VECTOR_COLORS
		vector_colors = rebin(*self.color, n_elements(*self.vx), 3)
	endif
	
	;VECTOR_COLORS
	if n_elements(vector_colors) gt 0 then begin
		vxsz = size(*self.vx)
		vcsz = size(vector_colors)
		
		;If a scalar, expand it
		if vcsz[vcsz[0]+2] eq 1 then begin
			;Color name given?
			if vcsz[vcsz[0]+1] eq 7 $
				then v_colors = cgColor(vector_colors, /TRIPLE) $
				else v_colors = self.palette -> GetRGB(v_colors)
			
			;Expand
			*self.vector_colors = rebin(v_colors, vxsz[vxsz[0]+2], 3)
		
		;Does VECTOR_COLORS have the same number of elements as VX?
		endif else if vxsz[vxsz[0]+2] eq vcsz[vcsz[0]+2] then begin
			;String color names or color table indices?
			if vcsz[vcsz[0]+1] eq 7 $
				then *self.vector_colors = cgColor(vector_colors) $
				else *self.vector_colors = self.palette -> GetRGB(vector_colors)
		
		;Were Nx3 color triples given?
		;   - Must have same number of colors as elements in VX
		endif else if vcsz[0] eq 2 && vcsz[2] eq 3 && vcsz[1] eq vxsz[vxsz[0]+2] then begin
			*self.vector_colors = vector_colors
			
		;Were 3xN color triples given?
		;   - Must have same number of colors as elements in VX
		endif else if vcsz[0] eq 2 && vcsz[1] eq 3 && vcsz[2] eq vxsz[vxsz[0]+2] then begin
			*self.vector_colors = transpose(vector_colors)
		
		;Incorrect size
		endif else begin
			message, 'VECTOR_COLORS must have the same number of color indices or color triples as vectors in VX.', /INFORMATIONAL
		endelse
	endif

	;MrVector Properties
	if n_elements(fraction)  gt 0 then  self.fraction  = fraction
	if n_elements(hsize)     gt 0 then  self.hsize     = hsize
	if n_elements(hthick)    gt 0 then  self.hthick    = hthick
	if n_elements(length)    gt 0 then  self.length    = length
	if n_elements(linestyle) gt 0 then *self.linestyle = linestyle
	if n_elements(ordered)   gt 0 then  self.ordered   = ordered
	if n_elements(thick)     gt 0 then *self.thick     = thick

;---------------------------------------------------------------------
;Superclass Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
	if n_elements(xtitle) gt 0 then *self.xtitle = cgCheckForSymbols(xtitle)
	if n_elements(ytitle) gt 0 then *self.ytitle = cgCheckForSymbols(ytitle)
	if n_elements(ztitle) gt 0 then *self.ztitle = cgCheckForSymbols(ztitle)
	if n_elements(xstyle) gt 0 then *self.xstyle = xstyle + ((xstyle and 1) eq 0)
	if n_elements(ystyle) gt 0 then *self.ystyle = ystyle + ((ystyle and 1) eq 0)

	;Data Atom
	if n_elements(extra) gt 0 $
		then self -> MrGrDataAtom::SetProperty, _EXTRA=extra

	;Refresh the graphics window
	self.window -> Draw
end


;+
;   Clean up after the object is destroyed.
;-
pro MrVector::cleanup
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;free all pointers
	ptr_free, self.x
	ptr_free, self.y
	ptr_free, self.vx
	ptr_free, self.vy
	ptr_free, self.xvert
	ptr_free, self.yvert
	ptr_free, self.vector_colors

	;Cleanup the superclass.
	self -> MrGrDataAtom::CleanUp
end


;+
;   Initialization method.
;
; :Params:
;       VELX:           in, required, type=integer/float
;                       An array containing the X component of the particle velocity vector.
;       VELY:           in, required, type=integer/float
;                       An array containing the Y component of the particle velocity vector.
;       POSX:           in, optional, type=integer/float
;                       An array containing the X posiiton of the particle velocity vector. The
;                           shaft end of the arrow vector is positioned here.
;       POSY:           in, optional, type=integer/float
;                       An array containing the Y posiiton of the particle velocity vector. The
;                           shaft end of the arrow vector is positioned here.
;
; :Keywords:
;       CURRENT:        in, optional, type=boolean, default=0
;                       If set, the graphic will be added to the current window. If
;                           not, a new window will be created.
;       FRACTION:       in, optional, type=float, default=1.0
;                       A number between 0.0 and 1.0 indicating the fraction of the vectors to
;                           draw on the plot. Vectors are selected randomly from the total population,
;                           unless the `Ordered` keyword is set, in which case they are selected
;                           in an ordered, systematic fashion. For example, Fraction=0.5 will select
;                           every other input vector.
;       HIDE:           in, optional, type=boolean, default=0
;                       If set, the graphic will not be displayed.
;       HSIZE:          in, optional, type=float
;                       The size of the the arrow head. By default 1/100th the width
;                           of the device. (!D.X_SIZE / 100.)
;       HTHICK:         in, optional, type=float
;                       The thickness of the line drawing the arrowhead of the vector. The
;                           default is 3 for the PostScript device and 1 for all other devices.
;       LAYOUT:         in, optional, type=intarr(3)
;                       A vector of the form [nCols, nRows, pIndex], where the first
;                           two elements are the number of columns and rows of plots
;                           whithin the plotting grid, and pIndex is the plot index,
;                           starting with 1 (one) in the upper-left corner of the
;                           grid and increasing first left-to-right then top-to-bottom.
;                           If LAYOUT and `POSITION` are not provided, the graphic
;                           will be placed at the next avaible pIndex location.
;       LENGTH:         in, optional, type=float, default=0.075
;                       The length of the `ReferenceVector` in normalized units. All vectors 
;                           are scaled according to this length.
;       LINESTYLE:      in, optional, type=integer, default=0
;                       The line style of the arrow. Line style integers as in PLOT.
;       MAPCOORD:       in, optional, type=object
;                       A map coordinate object (e.g., cgMap) that describes the map projection
;                           and datum used to specify the vector locations. Note that this could also be a 
;                           map structure as returned from MAP_PROJ_INIT, but in that case the user is 
;                           responsible for setting up the XY map coordinate space independently and 
;                           outside of this program. This coordinate object will be used to transform
;                           lat/lon locations into the XY locations of the map projection.
;       NAME:           in, optional, type=string, default=Object class of the graphic
;                       A string specifying the name of the graphic. It can be used
;                           retrieve the graphic using bracket array notation.
;       ORDERED:        in, optional, type=boolean, default=0
;                       If this keyword is set, and the `Fraction` keyword is used, the fraction
;                           of vectors used in the plot are chosen from the entire population in a
;                           systematic, ordered fashion.
;       OVERPLOT:       in, optional, type=boolean/object
;                       Set equal to 1 or to a graphic object refrence. If set to 1,
;                           the graphic will be overploted onto an existing graphic
;                           in the current window. If a graphic is selected, it will
;                           be the target. If no graphics are selected, the highest
;                           ordered graphic will be the target. If no window is open,
;                           a new window will be created. If set to a graphic's object
;                           refrece, that graphic is used as the target of the overplot.
;       POSITION:       in, optional, type=fltarr(4)
;                       A vector of the form [x0, y0, x1, y1], where (x0,y0) specify
;                           the location of the lower-left corner and (x1,y1) specify
;                           the location of the upper-right corner of the graphic.
;       SOLID:          in, optional, type=boolean, default=0
;                       Set this keyword to draw solid, filled arrows.
;       THICK:          in, optional, type=float
;                       The thickness of the line drawing the shaft of the arrow. The
;                           default is 3 for the PostScript device and 1 for all other devices.
;       V_CLIP:         in, optional, type=float
;                       A four-element array giving the clipping rectangle [xo, yo, x1, y1].
;                           The default clipping rectangle is the plot area. See the
;                           documenation for the IDL graphics keyword CLIP.
;       V_COLORS:       in, optional
;                       A scalar or vector of colors the same size as `velx`. May be bytes,
;                           short integers, or strings. Bytes and short integers are
;                           treated as indices into the current color  table. The default
;                           is "opposite".
;       V_NOCLIP:       in, optional, type=boolean, default=1
;                       Set the keyword to zero to clip vector output to the clipping
;                           rectangle specified in the `V_CLIP` keyword. The default is
;                           to leave the vectors unclipped. See the documentation for the
;                           IDL graphics keyword NOCLIP.
;       V_REF:          in, optional, type=float
;                       The magnitude of a reference vector that is used to scale all other
;                           vectors before display.
;       WINDOW_TITLE:   in, optional, type=string, default='MrWindow'
;                       If a new window is created, the title to be placed on the
;                           window's title bar.
;       _EXTRA:         in, optional
;                       Any keywords appropriate for any superclass can be used to
;                           create the graphic.
;-
function MrVector::init, vx, vy, x, y, $
ARROW_STYLE=arrow_style, $
ARROW_THICK=arrow_thick, $
DATA_LOCATION=data_location, $
HEAD_ANGLE=head_angle, $
HEAD_INDENT=head_indent, $
HEAD_PROPORTIONAL=head_proportional, $
HEAD_SIZE=head_size, $
LENGTH_SCALE=length_scale, $
MIN_VALUE=min_value, $
MAX_VALUE=max_value, $
RGB_TABLE=rgb_table, $
SYMBOL=symbol, $
SYM_COLOR=sym_color, $
SYM_FILLED=sym_filled, $
SYM_FILL_COLOR=sym_fill_color, $
SYM_SIZE=sym_size, $
SYM_THICK=sym_thick, $
USE_DEFAULT_COLOR=use_default_color, $
VECTOR_COLORS=vector_colors, $

;Direct graphics keywords
CLIP=clip, $

;MrGraphics Keywords
CURRENT = current, $
HIDE = hide, $
LAYOUT = layout, $
NAME = name, $
OVERPLOT = overplot, $
POSITION = position, $
WINDOW_TITLE=window_title, $

;Vector Keywords
FRACTION=fraction, $
HSIZE=hsize, $
HTHICK=hthick, $
LENGTH=length, $
LINESTYLE=linestyle, $
MAPCOORD=mapCoord, $
ORDERED=ordered, $
SOLID=solid, $
THICK=thick, $

;Graphics Keywords
XRANGE=xrange, $
YRANGE=yrange, $
XSTYLE=xstyle, $
YSTYLE=ystyle, $
XTITLE=xtitle, $
YTITLE=ytitle, $
ZTITLE=ztitle, $
_REF_EXTRA=extra
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return, 0
	endif

;---------------------------------------------------------------------
;Superclass Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
	success = self -> MrGrDataAtom::Init(CURRENT=current,  HIDE=hide, $
	                  LINESTYLE=linestyle, LAYOUT=layout, NAME=name, OVERPLOT=overplot, $
	                  POSITION=position, REFRESH=refresh, THICK=thick, _EXTRA=extra)
	if success eq 0 then message, 'Unable to initialize MrDataAtom.'

;---------------------------------------------------------------------
;Defaults and Heap ///////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Allocate Heap
	self.min_value     = ptr_new(/ALLOCATE_HEAP)
	self.max_value     = ptr_new(/ALLOCATE_HEAP)
	self.x             = ptr_new(/ALLOCATE_HEAP)
	self.y             = ptr_new(/ALLOCATE_HEAP)
	self.vx            = ptr_new(/ALLOCATE_HEAP)
	self.vy            = ptr_new(/ALLOCATE_HEAP)
	self.xvert         = ptr_new(/ALLOCATE_HEAP)
	self.yvert         = ptr_new(/ALLOCATE_HEAP)
	self.vector_colors = ptr_new(/ALLOCATE_HEAP)
	
	;Defaults
	auto_color        = keyword_set(auto_color)
	head_proportional = keyword_set(head_proportional)
	ordered           = keyword_set(ordered)
	solid             = keyword_set(solid)
	use_default_color = n_elements(use_default_color) eq 0 ? 1 : keyword_set(use_default_color)
	clip              = n_elements(clip)              eq 0 ? 1 : keyword_set(clip)
	if n_elements(auto_color)    eq 0 then auto_range    = 0
	if n_elements(auto_range)    eq 0 then auto_range    = [0.0, 0.0]
	if n_elements(arrow_style)   eq 0 then arrow_style   = 0
	if n_elements(arrow_thick)   eq 0 then arrow_thick   = 1.0
	if n_elements(data_location) eq 0 then data_location = 0
;	if n_elements(fraction)      eq 0 then fraction      = 1.0
	if n_elements(head_angle)    eq 0 then head_angle    = 30.0
	if n_elements(head_indent)   eq 0 then head_indent   = 0.4
	if n_elements(head_size)     eq 0 then head_size     = 0.1
	if n_elements(length_scale)  eq 0 then length_scale  = 1.0
;	if n_elements(linestyle)     eq 0 then linestyle     = 0
;	if n_elements(thick)         eq 0 then thick         = 1.0
;	if n_elements(hthick)        eq 0 then hthick        = thick   ;After THICK
	if n_elements(xtitle)        eq 0 then xtitle        = ''
	if n_elements(xstyle)        eq 0 then xstyle        = 1
	if n_elements(ystyle)        eq 0 then ystyle        = 1
	if n_elements(ytitle)        eq 0 then ytitle        = ''
	if n_elements(ztitle)        eq 0 then ztitle        = ''

	;Arbitrary default scale. LENGTH_SCALE alters this number.
	self.scale = 0.075
	
;---------------------------------------------------------------------
; Set Data ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Set these properties before setting data
	;   - Required for ::PrepVectors
	self.data_location     = data_location
	self.length_scale      = length_scale
	self.head_angle        = head_angle
	self.head_indent       = head_indent
	self.head_proportional = head_proportional
	self.head_size         = head_size
	
	;Set the data
	self -> SetData, vx, vy, x, y

;---------------------------------------------------------------------
; Colors /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Set the color palette
	;   - Will be needed to grab VECTOR_COLORS
	self.palette = obj_new('MrColorPalette', rgb_table)
	if ~obj_valid(self.palette) then return, 0

	if n_elements(color)          eq 0 then color          = 'black'
	if n_elements(sym_color)      eq 0 then sym_color      = color
	if n_elements(sym_fill_color) eq 0 then sym_fill_color = color
	if n_elements(vector_colors)  eq 0 then vector_colors  = color
	
	;Use the vector color as the missing color no matter what
	;   - Note that USE_DEFAULT_COLOR defaults to 1, so must be
	;     explicitly set to 0.
	if use_default_color then sym_color = color

;---------------------------------------------------------------------
;Set Data and Properties /////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Set colors here
	;   - MrGraphicsKeywords initializes *SELF.COLOR to 'OPPOSITE'
	;   - This is a little antiquated. We want it to be a color triple.
	*self.color             = cgColor(color, /TRIPLE)
	 self.sym_color         = cgColor(sym_color, /TRIPLE)
	 self.sym_fill_color    = cgColor(sym_fill_color, /TRIPLE)
	 self.use_default_color = use_default_color

	;Set Properties
	self -> SetProperty, ARROW_STYLE       = arrow_style, $
	                     ARROW_THICK       = arrow_thick, $
	                     MIN_VALUE         = min_value, $
	                     MAX_VALUE         = max_value, $
	                     SYMBOL            = symbol, $
;	                     SYM_COLOR         = sym_color, $
	                     SYM_FILLED        = sym_filled, $
;	                     SYM_FILL_COLOR    = sym_fill_color, $
	                     SYM_SIZE          = sym_size, $
	                     SYM_THICK         = sym_thick, $
;	                     USE_DEFAULT_COLOR = use_default_color, $
	                     VECTOR_COLORS     = vector_colors, $
	                     
	                     ;Vector Keywords
	                     FRACTION  = fraction, $
	                     HTHICK    = hthick, $
	                     LINESTYLE = linestyle, $
	                     ORDERED   = ordered, $
	                     THICK     = thick, $
	                     
	                     ;Direct Graphics Properties
;	                     COLOR   = color, $
	                     PSYM    = psym, $
	                     SYMSIZE = symsize, $
	                     XRANGE  = xrange, $
	                     XSTYLE  = xstyle, $
	                     XTITLE  = xtitle, $
	                     YRANGE  = yrange, $
	                     YSTYLE  = ystyle, $
	                     YTITLE  = ytitle, $
	                     ZTITLE  = ztitle
	
	;Save the initial data range
	self.init_xrange = *self.xrange
	self.init_yrange = *self.yrange

	;Refresh the window?
	if refresh then self -> Refresh

	return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrVector__define, class
	compile_opt strictarr
	
	class = { MrVector, $
	          inherits MrGrDataAtom, $
	         
	          ;Data Properties
	          x:     ptr_new(), $
	          y:     ptr_new(), $
	          vx:    ptr_new(), $
	          vy:    ptr_new(), $
	          vmag:  0.0D, $
	          scale: 0.0, $
	          
	          ;Arrow vertices
	          xvert: ptr_new(), $
	          yvert: ptr_new(), $
	          
	          ;Vector Properties
	          arrow_style:       0B, $
	          arrow_thick:       1.0, $
	          auto_color:        0B, $
	          auto_range:        dblarr(2), $
	          data_location:     0B, $
	          head_angle:        0.0, $
	          head_indent:       0.0, $
	          head_proportional: 0B, $
	          head_size:         0.0, $
	          length_scale:      0.0, $
	          palette:           obj_new(), $
	          sym_color:         bytarr(1,3), $
	          sym_filled:        0B, $
	          sym_fill_color:    bytarr(1,3), $
	          sym_thick:         0.0, $
	          use_default_color: 0B, $
	          vector_colors:     ptr_new(), $
	          
	          
	          ;cgVelocityVector Properties
	          fraction:  0.0, $
	          hthick:    0.0, $
	          length:    0.0, $
	          ordered:   0B, $
	          solid:     0B, $
	         
	          ;Graphics Properties
	          polar:     0B, $               ;create a polar plot?
	          ynozero:   0B, $               ;do not make ymin=0
	          label:     '', $               ;label to replace a title -- from cgPlot
	         
	          ;MrVector Properties
	          init_xrange: dblarr(2), $      ;Initial y-range
	          init_yrange: dblarr(2) $       ;Initial x-range
	        }
end