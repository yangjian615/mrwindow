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
        void = cgErrorMsg()
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
        void = cgErrorMsg()
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
        void = cgErrorMsg()
        return
    endif

    if self.hide then return

    ;Draw the plot
    if self.overplot then begin
        ;Restore target's coordinate system. Make sure that the overplot
        ;is positioned correctly.
        self.target -> RestoreCoords
        position = [!x.window[0], !y.window[0], $
                    !x.window[1], !y.window[1]]
        self.layout -> SetProperty, POSITION=position, UPDATE_LAYOUT=0
        
        ;Overplot
        self -> doPlot
        self -> SaveCoords
    endif else begin
        self -> doPlot, NOERASE=noerase
        self -> SaveCoords
    endelse
end


;+
;   The purpose of this method is to do the actual plotting. Basically, having this here
;   merely to saves space in the Draw method.
;
; :Private:
;-
pro MrVector::doPlot, $
NOERASE=noerase
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    if n_elements(noerase) eq 0 then noerase = *self.noerase
    self.layout -> GetProperty, CHARSIZE=charsize, POSITION=position

    ;Draw the plot.
    cgDrawVectors, *self.velx, *self.vely, *self.posx, *self.posy, $
             ;MrVector Keywords
             CLIP      = ~self.v_noclip, $
             CRECT     = *self.v_clip, $
             FRACTION  =  self.fraction, $
             HSIZE     =  self.hsize, $
             HTHICK    =  self.hthick, $
             LENGTH    =  self.length, $
             LINESTYLE = *self.linestyle, $
             MAPCOORD  = *self.mapCoord, $
             ORDERED   =  self.ordered, $
             OVERPLOT  =  self.overplot, $
             SOLID     =  self.solid, $
             THICK     = *self.thick, $
             VECCOLORS = *self.v_colors, $
             REFERENCEVECTOR = *self.v_ref, $
             
             ;MrDataAtom Keywords
             CHARSIZE  =       charsize, $
             LABEL     =  self.label, $
             MAX_VALUE = *self.max_value, $
             MIN_VALUE = *self.min_value, $
             POLAR     =  self.polar, $
             POSITION  =       position, $
             XLOG      =  self.xlog, $
             YLOG      =  self.ylog, $
             YNOZERO   =  self.ynozero, $
               
             ;MrGraphicsKeywords
             AXISCOLOR     = *self.axiscolor, $
             BACKGROUND    = *self.background, $
             CHARTHICK     = *self.charthick, $
             COLOR         = *self.color, $
             DATA          =  self.data, $
             DEVICE        =  self.device, $
             NORMAL        =  self.normal, $
             FONT          = *self.font, $
             NOCLIP        = *self.noclip, $
             NODATA        = *self.nodata, $
             NOERASE       =       noerase, $
             PSYM          = *self.psym, $
             SUBTITLE      = *self.subtitle, $
             SYMSIZE       = *self.symsize, $
             T3D           = *self.t3d, $
             TICKLEN       = *self.ticklen, $
             TITLE         = *self.title, $
             XCHARSIZE     = *self.xcharsize, $
             XGRIDSTYLE    = *self.xgridstyle, $
             XMINOR        = *self.xminor, $
             XRANGE        = *self.xrange, $
             XSTYLE        = *self.xstyle, $
             XTHICK        = *self.xthick, $
             XTICK_GET     = *self.xtick_get, $
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
             YMINOR        = *self.yminor, $
             YRANGE        = *self.yrange, $
             YSTYLE        = *self.ystyle, $
             YTHICK        = *self.ythick, $
             YTICK_GET     = *self.ytick_get, $
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
             ZMARGIN       = *self.zmargin, $
             ZMINOR        = *self.zminor, $
             ZRANGE        = *self.zrange, $
             ZSTYLE        = *self.zstyle, $
             ZTHICK        = *self.zthick, $
             ZTICK_GET     = *self.ztick_get, $
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
        void = cgErrorMsg()
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
V_CLIP=v_clip, $
V_COLORS=v_colors, $
V_NOCLIP=v_noclip, $
V_REF=v_ref, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
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
    
    ;Refresh the graphics window
    self.window -> Draw
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
pro MrVector::SetData, velx, vely, posx, posy
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Dimensions    
    dimsVx  = size(velx, /DIMENSIONS)
    ndimsVx = size(velx, /N_DIMENSIONS)
    
    ;Retrieve the data
    case n_params() of
        2: begin
            ;Create position vectors
            posx = fltarr(dimsVx[0])
            if ndimsVx eq 1 then posy = fltarr(dimsVx[0]) else posy = fltarr(dimsVx[1])
        endcase
        
        4: ;Do nothing
        
        else: message, 'Incorrect number of parameters.'
    endcase
    
    dimsX  = size(posx, /DIMENSIONS)
    ndimsX = size(posx, /N_DIMENSIONS)
    if ndimsX ne ndimsVx then begin
        if dimsX[0] ne dimsVx[0] then $
            message, 'POSX must have the same length as VELX[*,0].'
        
        ;Make the position 2D
        *self.posx = rebin(posx, dimsVx)
    endif else begin
        *self.posx = posx
    endelse
    
    dimsY  = size(posy, /DIMENSIONS)
    ndimsY = size(posy, /N_DIMENSIONS)
    if ndimsY ne ndimsVx then begin
        if dimsY[0] ne dimsVx[1] then $
            message, 'POSX must have the same length as VELX[0,*].'
        
        ;Make the position 2D
        *self.posy = rebin(1 # posy, dimsVx)
    endif else begin
        *self.posy = posy
    endelse
    
    ;Set the data
    *self.velx = velx
    *self.vely = vely    
    
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
V_CLIP=v_clip, $
V_COLORS=v_colors, $
V_NOCLIP=v_noclip, $
V_REF=v_ref, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;MrVector Properties
    if n_elements(fraction)  gt 0 then  self.fraction  = fraction
    if n_elements(hsize)     gt 0 then  self.hsize     = hsize
    if n_elements(hthick)    gt 0 then  self.hthick    = hthick
    if n_elements(length)    gt 0 then  self.length    = length
    if n_elements(linestyle) gt 0 then *self.linestyle = linestyle
    if n_elements(ordered)   gt 0 then  self.ordered   = ordered
    if n_elements(solid)     gt 0 then  self.solid     = solid
    if n_elements(thick)     gt 0 then *self.thick     = thick
    if n_elements(v_clip)    gt 0 then *self.v_clip    = v_clip
    if n_elements(v_colors)  gt 0 then *self.v_colors  = v_colors
    if n_elements(v_noclip)  gt 0 then  self.v_noclip  = v_noclip
    if n_elements(v_ref)     gt 0 then *self.v_ref     = v_ref

    ;Map object
    if n_elements(mapCoord) gt 0 then begin
        if obj_valid(mapCoord) then begin
            *self.mapCoord = mapCoord
        endif else begin
            ptr_free, self.mapCoord
            self.mapCoord = ptr_new(/ALLOCATE_HEAP)
        endelse
    endif
;---------------------------------------------------------------------
;Superclass Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Data Atom
    if n_elements(extra) gt 0 then $
        self -> MrGrDataAtom::SetProperty, _EXTRA=extra
    
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
        void = cgErrorMsg()
        return
    endif
    
    ;free all pointers
    ptr_free, self.posx
    ptr_free, self.posy
    ptr_free, self.velx
    ptr_free, self.vely
    ptr_free, self.v_clip
    ptr_free, self.v_colors
    
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
function MrVector::init, velx, vely, posx, posy, $
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
V_CLIP=v_clip, $
V_COLORS=v_colors, $
V_NOCLIP=v_noclip, $
V_REF=v_ref, $

;Graphics Keywords
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
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
    ordered  = keyword_set(ordered)
    solid    = keyword_set(solid)
    v_noclip = n_elements(v_noclip) eq 0 ? 1 : keyword_set(v_noclip)
    if n_elements(fraction)  eq 0 then fraction  = 1.0
    if n_elements(hthick)    eq 0 then hthick    = 1.0
    if n_elements(lenth)     eq 0 then length    = 0.075
    if n_elements(linestyle) eq 0 then linestyle = 0
    if n_elements(thick)     eq 0 then thick     = 1
    
    ;Allocate Heap
    self.posx     = ptr_new(/ALLOCATE_HEAP)
    self.posy     = ptr_new(/ALLOCATE_HEAP)
    self.velx     = ptr_new(/ALLOCATE_HEAP)
    self.vely     = ptr_new(/ALLOCATE_HEAP)
    self.v_clip   = ptr_new(/ALLOCATE_HEAP)
    self.v_colors = ptr_new(/ALLOCATE_HEAP)
    self.v_ref    = ptr_new(/ALLOCATE_HEAP)
    
    ;Objects -- cgDrawVector requires the mapCoord object to be a valid structure.
    ;           An invalid map coordinate object cannot be given.
    self.mapCoord = ptr_new(/ALLOCATE_HEAP)

;---------------------------------------------------------------------
;Set Data and Properties /////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Set the data
    if n_elements(posx) gt 0 $
        then self -> SetData, velx, vely, posx, posy $
        else self -> SetData, velx, vely
    
    ;Set Properties
    self -> SetProperty, FRACTION=fraction, $
                         HSIZE=hsize, $
                         HTHICK=hthick, $
                         LENGTH=length, $
                         MAPCOORD=mapCoord, $
                         ORDERED=ordered, $
                         SOLID=solid, $
                         V_CLIP=v_clip, $
                         V_COLORS=v_colors, $
                         V_NOCLIP=v_noclip, $
                         V_REF=v_ref
    
    ;Set the default head size now that a window has been chosen.
    currentWin = GetMrWindows(/CURRENT)
    self.window -> SetCurrent
    if n_elements(hsize) eq 0 then hsize = !d.x_size / 100.0
    self.hsize = hsize
    currentWin -> SetCurrent

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
              velx: ptr_new(), $
              vely: ptr_new(), $
              posx: ptr_new(), $
              posy: ptr_new(), $
              
              ;cgVelocityVector Properties
              fraction:  0.0, $
              hsize:     0.0, $
              hthick:    0.0, $
              length:    0.0, $
              mapCoord:  ptr_new(), $
              ordered:   0B, $
              solid:     0B, $
              v_clip:    ptr_new(), $
              v_colors:  ptr_new(), $
              v_noclip:  0B, $
              v_ref:     ptr_new(), $
             
              ;Graphics Properties
              polar:     0B, $               ;create a polar plot?
              ynozero:   0B, $               ;do not make ymin=0
              label:     '', $               ;label to replace a title -- from cgPlot
             
              ;MrVector Properties
              init_xrange: dblarr(2), $      ;Initial y-range
              init_yrange: dblarr(2) $       ;Initial x-range
            }
end