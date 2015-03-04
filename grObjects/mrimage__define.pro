; docformat = 'rst'
;
; NAME:
;       MrImage__Define
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
;   The purpose of this method is to create an image object with set, get, and draw
;   methods.
;
; :Examples:
;   See MrImage_Examples.pro for a series of examples::
;       IDL> void = MrImage_Examples()
;       IDL> win  = MrImage_Examples(14)
;
; :Uses:
;   Uses the following external programs::
;       cgDemoData.pro      (Coyote Graphics)
;       cgErrorMsg.pro      (Coyote Graphics)
;       cgPlot.pro          (Coyote Graphics)
;       setDefaultValue.pro (Coyote Graphics)
;       MrGrDataAtom__define.pro
;       MrLog.pro
;       linspace.pro
;       logspace.pro
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
;	Modification History::
;       2014/09/09  -   Written by Matthew Argall
;       2014/09/15  -   Removed the RLOG property. ::PrepImage scales the image correctly.
;                           Out-of-range pixels determined before change from polar to
;                           cartesian coordinates. Added the doPolarAxes method. - MRA
;       2014/09/17  -   MISSING_INDEX is only loaded when necessary. - MRA
;       2014/10/05  -   SetPalette sets quantities independent of whether a color table or
;                           color table index is used. Allows proper scaling of image.
;                           Missing color is loaded at time of draw. - MRA
;       2015/02/23  -   Determine polar ranges better. Do not draw axes if OVERPLOT is set. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to print information about the object's properties
;   when the PRINT procedure is used.
;-
function MrImage::_OverloadPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, "''"
    endif
    
    ;Useful strings
    undefined = '<undefined>'
    joinStr = '   '
    
    ;Superclasses
    grKeys = self -> MrGrDataAtom::_OverloadPrint()
    
    ;Color Table Index
    if n_elements(*self.ctindex) eq 0 $
        then ctindex = undefined $
        else ctindex = string(*self.ctindex, FORMAT='(i0)')
    
    ;Data Position
    if n_elements(*self.data_pos) eq 0 $
        then data_pos = undefined $
        else data_pos = '[' + strjoin(string(*self.data_pos, FORMAT='(f0)'), ', ') + ']'
    
    ;Missing Value
    if n_elements(*self.missing_value) eq 0 $
        then missing_value = undefined $
        else missing_value = '[' + strjoin(string(*self.missing_value, FORMAT='(f)'), ', ') + ']'
        
    axes           = string('Axes',           '=', self.axes,           FORMAT='(a-26, a-2, i1)')
    bottom         = string('Bottom',         '=', self.bottom,         FORMAT='(a-26, a-2, i3)')
    brewer         = string('Brewer',         '=', self.brewer,         FORMAT='(a-26, a-2, i1)')
    center         = string('Center',         '=', self.center,         FORMAT='(a-26, a-2, i1)')
    ctindex        = string('CTIndex',        '=',      ctindex,        FORMAT='(a-26, a-2, a0)')
    data_pos       = string('Data_Pos',       '=',      data_pos,       FORMAT='(a-26, a-2, a0)')
    idisplay       = string('iDisplay',       '=', self.idisplay,       FORMAT='(a-26, a-2, i0)')
    log            = string('Log',            '=', self.log,            FORMAT='(a-26, a-2, i1)')
    missing_value  = string('Missing_Value',  '=',      missing_value,  FORMAT='(a-26, a-2, a0)')
    missing_color  = string('Missing_Color',  '=', self.missing_color,  FORMAT='(a-26, a-2, a0)')
    missing_index  = string('Missing_Index',  '=', self.missing_index,  FORMAT='(a-26, a-2, i0)')
    nan            = string('NaN',            '=', self.nan,            FORMAT='(a-26, a-2, i1)')
    paint          = string('Paint',          '=', self.paint,          FORMAT='(a-26, a-2, i1)')
    palette        = string('Palette',        '=', 'BYTARR(255,3)',     FORMAT='(a-26, a-2, a0)')
    polar          = string('Polar',          '=', self.polar,          FORMAT='(a-26, a-2, i1)')
    pol_rcolor     = string('Pol_RColor',     '=', self.pol_rcolor,     FORMAT='(a-26, a-2, a0)')
    pol_rlinestyle = string('Pol_RLineStyle', '=', self.pol_rlinestyle, FORMAT='(a-26, a-2, i1)')
    pol_tcolor     = string('Pol_TColor',     '=', self.pol_tcolor,     FORMAT='(a-26, a-2, a0)')
    pol_tlinestyle = string('Pol_TLineStyle', '=', self.pol_tlinestyle, FORMAT='(a-26, a-2, i1)')
    pol_thick      = string('Pol_Thick',      '=', self.pol_thick,      FORMAT='(a-26, a-2, f0)')
    range          = string('Range',          '=', self.range,          FORMAT='(a-26, a-2, "[", f0, ", ", f0, "]")')
    scale          = string('Scale',          '=', self.scale,          FORMAT='(a-26, a-2, i1)')
    top            = string('Top',            '=', self.top,            FORMAT='(a-26, a-2, i1)')
    tv             = string('TV',             '=', self.tv,             FORMAT='(a-26, a-2, i1)')
        
    selfStr = obj_class(self) + '  <' + strtrim(obj_valid(self, /GET_HEAP_IDENTIFIER), 2) + '>'
    imKeys = [ [axes], $
               [bottom], $
               [brewer], $
               [center], $
               [ctindex], $
               [data_pos], $
               [idisplay], $
               [log], $
               [missing_value], $
               [missing_color], $
               [missing_index], $
               [nan], $
               [paint], $
               [palette], $
               [polar], $
               [pol_rcolor], $
               [pol_rlinestyle], $
               [pol_tcolor], $
               [pol_tlinestyle], $
               [pol_thick], $
               [range], $
               [scale], $
               [top], $
               [tv] $
             ]

    result = [[grKeys], ['  ' + imKeys]]
    result = [[selfStr], [result[0, sort(result)]]]
    
    return, result
end


;+
;   The purpose of this method is to print information about the object's properties
;   when implied print is used.
;-
function MrImage::_OverloadImpliedPrint
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
;   Clear data from interal storage
;-
pro MrImage::ClearData, $
ALL=all, $
CENTERS=centers, $
CORNERS=corners, $
OCORNERS=oCorners, $
DELTAS=deltas, $
INDEP=indep, $
DEP=dep, $
X0=x0, $
X1=x1, $
XDELTA_PLUS=xdelta_plus, $
XDELTA_MINUS=xdelta_minus, $
XMAX=xmax, $
XMIN=xmin, $
Y0=y0, $
Y1=y1, $
YDELTA_PLUS=ydelta_plus, $
YDELTA_MINUS=ydelta_minus, $
YMAX=ymax, $
YMIN=ymin
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Individual quantities
    all          = keyword_set(all)
    centers      = keyword_set(centers)
    deltas       = keyword_set(deltas)
    corners      = keyword_set(corners)
    oCorners     = keyword_set(ocornsers)
    indep        = keyword_set(indep)
    dep          = keyword_set(dep)
    x0           = keyword_set(x0)
    x1           = keyword_set(x1)
    xdelta_plus  = keyword_set(xdelta_plus)
    xdelta_minus = keyword_set(xdelta_minus)
    xmax         = keyword_set(xmax)
    xmin         = keyword_set(xmin)
    y0           = keyword_set(y0)
    y1           = keyword_set(y1)
    ydelta_plus  = keyword_set(ydelta_plus)
    ydelta_minus = keyword_set(ydelta_minus)
    ymax         = keyword_set(ymax)
    ymin         = keyword_set(ymin)
    
    ;All?
    if all then begin
        centers = 1
        deltas  = 1
        corners = 1
    endif
    
    ;Centers?
    if centers then begin
        indep = 1
        dep   = 1
    endif
    
    ;Deltas?
    if deltas then begin
        xdelta_plus  = 1
        xdelta_minus = 1
        ydelta_plus  = 1
        ydelta_minus = 1
    endif
    
    ;Corners?
    if corners then begin
        xmax = 1
        xmin = 1
        ymax = 1
        ymin = 1
    endif
    
    ;Output Corners?
    if oCorners then begin
        x0 = 1
        x1 = 1
        y0 = 1
        y1 = 1
    endif
            
    ;Pixel Centers
    if dep   then if n_elements(*self.dep)   gt 0 then void = temporary(*self.dep)
    if indep then if n_elements(*self.indep) gt 0 then void = temporary(*self.indep)
    
    ;Pixel Corners
    if xmin then if n_elements(*self.xmin) gt 0 then void = temporary(*self.xMin)
    if xmax then if n_elements(*self.xmax) gt 0 then void = temporary(*self.xMax)
    if ymin then if n_elements(*self.ymin) gt 0 then void = temporary(*self.yMin)
    if ymax then if n_elements(*self.ymax) gt 0 then void = temporary(*self.yMax)
    
    ;Pixel Corners -- Output
    if x0 then if n_elements(*self.x0) gt 0 then void = temporary(*self.x0)
    if x1 then if n_elements(*self.x1) gt 0 then void = temporary(*self.x1)
    if y0 then if n_elements(*self.y0) gt 0 then void = temporary(*self.y0)
    if y1 then if n_elements(*self.y1) gt 0 then void = temporary(*self.y1)
    
    ;Pixel Deltas
    if xdelta_minus then if n_elements(*self.xdelta_minus) gt 0 then void = temporary(*self.xdelta_minus)
    if xdelta_plus  then if n_elements(*self.xdelta_plus)  gt 0 then void = temporary(*self.xdelta_plus)
    if ydelta_minus then if n_elements(*self.ydelta_minus) gt 0 then void = temporary(*self.ydelta_minus)
    if ydelta_plus  then if n_elements(*self.ydelta_plus)  gt 0 then void = temporary(*self.ydelta_plus)
end


;+
;   The purpose of this method is to draw the plot in the draw window. The plot is
;   first buffered into the pixmap for smoother opteration (by allowing motion events
;   to copy from the pixmap instead of redrawing the plot, the image does not flicker).
;-
pro MrImage::Draw, $
NOERASE=noerase
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if n_elements(r)           gt 0 then tvlct, r, g, b
	    if n_elements(init_decomp) gt 0 then cgSetColorState, init_decomp
	    if n_elements(p_current)   gt 0 then !P = p_current
	    if n_elements(x_current)   gt 0 then !X = x_current
	    if n_elements(y_current)   gt 0 then !Y = y_current
        void = cgErrorMsg()
        return
    endif
    
    if self.hide then return
    
	;Change color states
	;   - Save the initial color table
	tvlct, r, g, b, /GET
	cgSetColorState, 0, CURRENTSTATE=init_decomp

    ;Store the current system variables
    p_current = !P
    x_current = !X
    y_current = !Y
    
    ;Restore coordinates
    if obj_valid(self.target) $
        then self.target -> RestoreCoords $
        else self        -> RestoreCoords
        
;---------------------------------------------------------------------
; Display the Image //////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Erase?
    if n_elements(noerase)  eq 0 then noerase = self.noerase
    if keyword_set(noerase) eq 0 then cgErase, self.background
    
    ;Load the palette
    tvlct, self.palette
    
    ;Load the missing color
    if self.missing_color ne '' then tvlct, cgColor(self.missing_color, /TRIPLE), self.missing_index

    ;Now display the image
    case 1 of
        self.paint: self -> doPaint, XLOG=xlog, XRANGE=xrange, $
                                     YLOG=ylog, YRANGE=yrange
        self.tv:    self -> doTV
        else:       self -> doImage
    endcase
    
;---------------------------------------------------------------------
; Draw Axes //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Box-style axes.
	if self.overplot eq 0 then if self.axes && ( (self.polar eq 0) || ((self.pol_axstyle and 1) gt 0) ) then begin
	    if self.paint eq 0 then begin
	        xrange = *self.xrange
	        yrange = *self.yrange
	        xlog   =  self.xlog
	        ylog   =  self.ylog
	    endif

	    self.layout -> GetProperty, POSITION=position, CHARSIZE=charsize
    
        ;Adjust postscript output.
        if !d.name eq 'PS' then begin
            charsize  = MrPS_Rescale(charsize,        /CHARSIZE)
            charthick = MrPS_Rescale(*self.charthick, /CHARTHICK)
            thick     = MrPS_Rescale(*self.thick,     /THICK)
        endif else begin
            charthick = *self.charthick
            thick     = *self.thick
        endelse

        cgplot, [0], [0], $
              /NODATA, $
              /NOERASE, $
              BACKGROUND    =  cgColor(*self.background), $
              CHARSIZE      =       charsize, $
              CHARTHICK     =       charthick, $
              COLOR         =  cgColor(*self.axiscolor), $
              DEVICE        =  self.device, $
              FONT          = *self.font, $
              NORMAL        =  self.normal, $
              POSITION      =       position, $
              SUBTITLE      = *self.subtitle, $
              THICK         =       thick, $
              TICKLEN       = *self.ticklen, $
              TITLE         =  cgCheckForSymbols(*self.title), $
              
              XCHARSIZE     = *self.xcharsize, $
              XGRIDSTYLE    = *self.xgridstyle, $
              XLOG          =       xlog, $
              XMINOR        = *self.xminor, $
              XRANGE        =       xrange, $
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
              XTITLE        =  cgCheckForSymbols(*self.xtitle), $
              
              YCHARSIZE     = *self.ycharsize, $
              YGRIDSTYLE    = *self.ygridstyle, $
              YLOG          =       ylog, $
              YMINOR        = *self.yminor, $
              YRANGE        =       yrange, $
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
              YTITLE        =  cgCheckForSymbols(*self.ytitle)

              ;Graphics keywords that are not used
              ; - CLIP --> DATA_POS
;              AXISCOLOR:     Ptr_New(), $
;              CLIP:          Ptr_New(), $
;              DATA:          0B, $
;              LINESTYLE:     Ptr_New(), $

    endif

    ;Polar axes?
    if self.overplot eq 0 $
        then if self.axes && self.polar then self -> DoPolarAxes, XRANGE=xrange, YRANGE=yrange

    ;Save the coordinate system
    self -> SaveCoords
    
    ;No axes
    if self.axes eq 0 then begin
        !P = p_current
        !X = x_current
        !Y = y_current
    endif

;---------------------------------------------------------------------
;RESET COLOR TABLE AND DEVICE ////////////////////////////////////////
;---------------------------------------------------------------------

	tvlct, r, g, b
	cgSetColorState, init_decomp
end


;+
;   The purpose of this method is to do the actual plotting.
;
; :Private:
;-
pro MrImage::doImage
    compile_opt strictarr
    on_error, 2
    
;---------------------------------------------------------------------
; Position Within Axes ///////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Was a data position explicitly given?
    ;   - Make sure it is always within the data range
    if n_elements(*self.data_pos) gt 0 && $
       ~array_equal((*self.data_pos)[[0,1]], (*self.data_pos)[[2,3]]) $
    then begin
        data_pos = *self.data_pos
        data_pos[0] >= (*self.xrange)[0]
        data_pos[1] >= (*self.yrange)[0]
        data_pos[2] <= (*self.xrange)[1]
        data_pos[3] <= (*self.yrange)[1]
        
        ;Find the index range of the data position
        ixrange = MrIndexRange(*self.indep, data_pos[[0,2]], STRIDE=xstride)
        iyrange = MrIndexRange(*self.dep,   data_pos[[1,3]], STRIDE=ystride)
    
    ;If not, the data position is the data range
    endif else begin
        ;Get the index range into the data
        ixrange = MrIndexRange(*self.indep, *self.xrange, STRIDE=xstride)
        iyrange = MrIndexRange(*self.dep,   *self.yrange, STRIDE=ystride)
        
        ;Set the data position
        data_pos        = dblarr(4)
        data_pos[[0,2]] = (*self.indep)[ixrange]
        data_pos[[1,3]] = (*self.dep)[iyrange]
    endelse
                
    ;We need to set the !P, !X, and !Y system variables in order for CONVERT_COORD
    ;to be able to convert from DATA to DEVICE coordinates. The only way to do that
    ;is to create an invisible plot.
    position = self.layout -> GetPosition()
    plot, [0,0], /NODATA, /NOERASE, XLOG=self.xlog, YLOG=self.ylog, $
                 XRANGE=*self.xrange, YRANGE=*self.yrange, XSTYLE=5, YSTYLE=5, $
                 POSITION=position, DEVICE=self.device, NORMAL=self.normal

    ;Convert from data to device coordinates.
    data_pos = convert_coord(data_pos[[0,2]], data_pos[[1,3]], /DATA, /TO_DEVICE)
    data_pos = reform(data_pos[0:1,0:1], 4, 1)

;---------------------------------------------------------------------
; Position of Axes ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Position of the image
    ;   - Convert to device coordinates
    if self.normal then begin
        position[[0,2]] = floor(position[[0,2]] * !d.x_vsize)
        position[[1,3]] = floor(position[[1,3]] * !d.y_vsize)
    endif
    
    ;If a data position was provided, switch to that now.
    if n_elements(data_pos) gt 0 $
        then image_position = data_pos $
        else image_position = position

    ;adjust the image position a little to fit inside the axes, not on top of them
    xsize = (image_position[2] - image_position[0])  - 1
    ysize = (image_position[3] - image_position[1])  - 1
    xstart = image_position[0] + 1
    ystart = image_position[1] + 1

;---------------------------------------------------------------------
; Display the Image //////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Include only those pixels that are inside the data range.
    iData   = [ixrange[0], iyrange[0], ixrange[1], iyrange[1]]

    ;size the image differently, depending out the output window
    if !D.Name eq 'PS' then begin
        tv, (*self.img_out)[ixrange[0]:ixrange[1]:xstride, iyrange[0]:iyrange[1]:ystride], $
            xstart, ystart, $
            XSIZE=xsize, YSIZE=ysize
    endif else begin
        tv, congrid((*self.img_out)[ixrange[0]:ixrange[1]:xstride, iyrange[0]:iyrange[1]:ystride], xsize, ysize), $
            xstart, ystart
    endelse
end


;+
;   Paint the image pixel-by-pixel.
;
; :Keywords:
;       POSITION:       in, optional, type=fltarr(4)
;                       Position of the image: [x0, y0, x1, y1].
;       XLOG:           out, optional, type=boolean
;                       Indicates whether the x-axis should be drawn in a log- or linear-
;                           scale (0 and 1, respectively). For polar plots, the radius
;                           cannot be negative, so we take the log of the radius and plot
;                           in a linear scale.
;       XRANGE:         out, optional, type=fltarr(4)
;                       Range spanned by the x-axis. For polar plots, it may be necessary
;                           to change the axis range in order to display the data
;                           (see `XLOG`). Also, polar ranges must be converted to
;                           cartesian ranges.
;       YLOG:           out, optional, type=boolean
;                       Indicates whether the y-axis should be drawn in a log- or linear-
;                           scale (0 and 1, respectively). For polar plots, the radius
;                           cannot be negative, so we take the log of the radius and plot
;                           in a linear scale.
;       YRANGE:         out, optional, type=fltarr(4)
;                       Range spanned by the y-axis. For polar plots, it may be necessary
;                           to change the axis range in order to display the data
;                           (see `XLOG`). Also, polar ranges must be converted to
;                           cartesian ranges.
;-
pro MrImage::doPaint, $
XLOG=xlog, $
XRANGE=xrange, $
YLOG=ylog, $
YRANGE=yrange
    compile_opt strictarr
    on_error, 2

    ;For ease of referencing
    xrange = *self.xrange
    yrange = *self.yrange
    img    =  self.img_out
    x0     =  self.x0
    x1     =  self.x1
    y0     =  self.y0
    y1     =  self.y1
    xlog   =  self.xlog
    ylog   =  self.ylog
    
;---------------------------------------------------------------------
; Pixels Inside Data Range ///////////////////////////////////////////
;---------------------------------------------------------------------
    ;Pick all pixels with at least one corner inside the image.
    ;   - Skip pixels that are entirely outside the data range.
    ;   - This is independent of NOCLIP.
    ;   - Do before any conversion from polar to cartesian.
    inds = where(*x1 gt xrange[0] and $
                 *x0 lt xrange[1] and $
                 *y1 gt yrange[0] and $
                 *y0 lt yrange[1], nInds)
             
    ;If no points exist within the range, then return
    if nInds eq 0 then return

    ;Get the 2D indices so that the image is not passed as a 1D array.
    dims = size(*img, /DIMENSIONS)
    inds = array_indices(dims, inds, /DIMENSIONS)
    iCol = reform(inds[0,*])
    iRow = reform((temporary(inds))[1,*])

;---------------------------------------------------------------------
; Polar Considerations ///////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Do not log-scale for polar plots, even if RLOG is set.
    ;   - The log of a negative radius is NaN and will cause error.
    ;   - Instead, display ALog10(radius) on a linear scale.
    if self.polar then begin
        if self.xlog then begin
            xlog   = 0
            xrange = MrLog(xrange) > 1e-3
            x0     = ptr_new(MrLog(*x0))    ;Do not overwrite original pointer.
            x1     = ptr_new(MrLog(*x1))    ;Do not overwrite original pointer.
        endif

        ;Ensure YRANGE is in the range of [0, 2*!pi]
        twopi = 2.0 * !pi
        if yrange[0] lt 0.0 $
            then yrange[0] = yrange[0] * ceil(-yrange[0]/twopi) * twopi $
            else if yrange[0] gt twopi then yrange[0] = yrange[0] mod twopi
        
        if yrange[1] lt 0.0 $
            then yrange[1] = yrange[1] * ceil(-yrange[1]/twopi) * twopi $
            else if yrange[1] gt twopi then yrange[1] = yrange[1] mod twopi
        
        ;Check range
        ;   - For XRANGE, we want to show the entire upper and/or lower-hp (half plane)
        ;   - For YRANGE, we want to show the entire right and/or left-hp
        pa_range_x = yrange
        pa_range_y = yrange
        
        ;Initial [XY]RANGE. Helps determine which half plane we are in.
        xr = xrange[1] * cos(yrange)
        yr = xrange[1] * sin(yrange)

    ;---------------------------------------------------------------------
    ; XRANGE /////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        ;Left-Half Plane
        if (xr[0] ge 0.0) && (xr[1] ge 0.0) then begin
            case 1 of
                ;One plane if
                ;   - YRANGE[0] in  I and YRANGE[1] in  I and YRANGE[1] ccw from YRANGE[0]
                ;   - YRANGE[0] in IV and YRANGE[1] in IV and YRANGE[1] ccw from YRANGE[0]
                ;   - YRANGE[0] in IV and YRANGE[1] in  I
                ( (yrange[0] le 0.5*!pi) && ( (yrange[1] le 0.5*!pi) && (yrange[1] ge yrange[0]) ) ): pa_range_x = [0.5*!pi, 0.0]
                ( (yrange[0] ge 1.5*!pi) && ( (yrange[1] ge 1.5*!pi) && (yrange[1] ge yrange[1]) ) ): pa_range_x = [0.5*!pi, 0.0]
                ( (yrange[0] ge 1.5*!pi) &&   (yrange[1] le 0.5*!pi) ):                               pa_range_x = [0.5*!pi, 0.0]
                else: pa_range_x = [!pi, 0.0]
            endcase
        
        ;Right-Half Plane
        endif else if (xr[0] le 0) && (xr[1] le 0) then begin
            case 1 of
                ;One plane if
                ;   - YRANGE[0] in II or III and
                ;     YRANGE[1] in II or III and YRANGE[1] ccw from YRANGE[0]
                ( (yrange[0] ge 0.5*!pi) && (yrange[0] le 1.5*!pi) ) && $
                ( (yrange[1] ge 0.5*!pi) && (yrange[1] ge 1.5*!pi) && (yrange[1] ge yrange[1]) ): pa_range_x = [!pi, 0.5*!pi]
                else: pa_range_x = [!pi, 0.0]
            endcase
        
        ;Left- and Right-Half planes
        endif else begin
            pa_range_x = [!pi, 0.0]
        endelse

    ;---------------------------------------------------------------------
    ; YRANGE /////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        ;Upper-Half Plane
        if (yr[0] ge 0.0) && (yr[1] ge 0.0) then begin
            ;One plane if
            ;   - YRANGE[0] in I or II and
            ;     YRANGE[1] in I or II and YRANGE[1] ccw from YRANGE[0]
            if (yrange[0] le !pi) && ( (yrange[1] le 0.5*!pi) && (yrange[1] ge yrange[0]) ) $
                then pa_range_y = [0.0,     0.5*!pi] $
                else pa_range_y = [1.5*!pi, 0.5*!pi]
        
        ;Lower-Half Plane
        endif else if (xr[0] le 0) && (xr[1] le 0) then begin
            ;One plane if
            ;   - YRANGE[0] in III or IV and
            ;     YRANGE[1] in III or IV and YRANGE[1] ccw from YRANGE[0]
            if (yrange[0] ge !pi) && ( (yrange[1] ge !pi) && (yrange[1] ge yrange[0]) ) $
                then pa_range_y = [1.5*!pi,     !pi] $
                else pa_range_y = [1.5*!pi, 0.5*!pi]
        
        ;Left- and Right-Half planes
        endif else begin
            pa_range_y = [1.5*!pi, 0.5*!pi]
        endelse

        ;Convert from polar to cartesian
        yrange = xrange[1] * sin(pa_range_y)
        xrange = xrange[1] * cos(pa_range_x)
    endif

;---------------------------------------------------------------------
; Establish Coordinate System ////////////////////////////////////////
;---------------------------------------------------------------------
    ;A data coordinate system must be established.
    ;   - Draw invisible axes.
    position = self.layout -> GetPosition()
    plot, [0], [0], /NODATA, /NOERASE, XLOG=xlog, YLOG=ylog, $
                    XRANGE=xrange, YRANGE=yrange, XSTYLE=5, YSTYLE=5, $
                    POSITION=position, DEVICE=self.device, NORMAL=self.normal

;---------------------------------------------------------------------
; Draw Pixels ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Draw each pixel
    ;   - Use only the pixels that are within the axis ranges.
    for i = 0, nInds - 1 do begin
        j = iCol[i]
        k = iRow[i]

        ;Skip pixels with invalid locations
        if total(finite([(*x0)[j,k], (*x1)[j,k], (*y0)[j,k], (*y1)[j,k]])) ne 4 $
            then continue
            
        ;Polar image
        ;   - Convert to cartesian coordinates.
        if self.polar then begin
            xx1 = (*x0)[j,k] * cos((*y0)[j,k])
            xx2 = (*x1)[j,k] * cos((*y0)[j,k])
            xx3 = (*x1)[j,k] * cos((*y1)[j,k])
            xx4 = (*x0)[j,k] * cos((*y1)[j,k])
            
            yy1 = (*x0)[j,k] * sin((*y0)[j,k])
            yy2 = (*x1)[j,k] * sin((*y0)[j,k])
            yy3 = (*x1)[j,k] * sin((*y1)[j,k])
            yy4 = (*x0)[j,k] * sin((*y1)[j,k])
            
            xpoly = [xx1, xx2, xx3, xx4, xx1]
            ypoly = [yy1, yy2, yy3, yy4, yy1]
        endif else begin
            xpoly = [(*x0)[j,k], (*x1)[j,k], (*x1)[j,k], (*x0)[j,k], (*x0)[j,k]]
            ypoly = [(*y0)[j,k], (*y0)[j,k], (*y1)[j,k], (*y1)[j,k], (*y0)[j,k]]
        endelse
        
        ;Clip pixels that straddle the axis range
        if *self.noclip eq 0 then begin
            if (xpoly[0] lt xrange[0]) && (xpoly[1] gt xrange[0]) then xpoly[[0,3,4]] = xrange[0]
            if (xpoly[0] lt xrange[1]) && (xpoly[1] gt xrange[1]) then xpoly[[1,2]]   = xrange[1]
            if (ypoly[0] lt yrange[0]) && (ypoly[1] gt yrange[0]) then ypoly[[0,1,4]] = yrange[0]
            if (ypoly[0] lt yrange[1]) && (ypoly[1] gt yrange[1]) then ypoly[[2,3]]   = yrange[1]

            ;Skip pixels that are outside of the axis range.
            ;   - Must do this after converting to cartesian coordinates.
            if (xpoly[0] lt xrange[0]) || (xpoly[1] gt xrange[1]) || $
               (ypoly[0] lt yrange[0]) || (ypoly[1] gt yrange[1]) $
            then continue
        endif

        ;Paint the image
        polyfill, xpoly, ypoly, COLOR=(*img)[j,k]
    endfor
end


;+
;   The purpose of this method is to do the actual plotting.
;
; :Private:
;-
pro MrImage::doPolarAxes, $
XRANGE=xrange, $
YRANGE=yrange
    compile_opt strictarr
    on_error, 2

    self.layout -> GetProperty, CHARSIZE=charsize

;---------------------------------------------------------------------
; Horizontal Axis ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if (self.pol_axstyle and 2) gt 0 then begin
        axis, 0, 0, XAXIS=0, $
                    /DATA, $
                    /NOERASE, $
                    /SAVE, $
                    CHARSIZE      =  charsize, $
                    CHARTHICK     = *self.charthick, $
                    COLOR         =  cgColor(*self.axiscolor), $
                    FONT          = *self.font, $
                    SUBTITLE      = *self.subtitle, $
                    TICKLEN       = *self.ticklen, $
                    XCHARSIZE     = *self.xcharsize, $
                    XGRIDSTYLE    = *self.xgridstyle, $
                    XLOG          =  xlog, $
                    XMINOR        = *self.xminor, $
                    XRANGE        =  xrange, $
                    XSTYLE        = *self.xstyle, $
                    XTICK_GET     = *self.xtick_get, $
                    XTICKFORMAT   = *self.xtickformat, $
                    XTICKINTERVAL = *self.xtickinterval, $
                    XTICKLAYOUT   = *self.xticklayout, $
                    XTICKLEN      = *self.xticklen, $
                    XTICKNAME     = *self.xtickname, $
                    XTICKS        = *self.xticks, $
                    XTICKUNITS    = *self.xtickunits, $
                    XTICKV        = *self.xtickv;, $
                    ;XTITLE        =  cgCheckForSymbols(*self.xtitle)
    endif

;---------------------------------------------------------------------
; Vertical Axis //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Since, this is the radius, use the x-axis properties
    if (self.pol_axstyle and 4) gt 0 then begin
        axis, 0, 0, YAXIS=0, $
                    /DATA, $
                    /NOERASE, $
                    /SAVE, $
                    CHARSIZE      =  charsize, $
                    CHARTHICK     = *self.charthick, $
                    COLOR         =  cgColor(*self.axiscolor), $
                    FONT          = *self.font, $
                    SUBTITLE      = *self.subtitle, $
                    TICKLEN       = *self.ticklen, $
                    YCHARSIZE     = *self.xcharsize, $
                    YGRIDSTYLE    = *self.xgridstyle, $
                    YLOG          =  xlog, $
                    YMINOR        = *self.xminor, $
                    YRANGE        =  yrange, $
                    YSTYLE        = *self.xstyle, $
                    YTICK_GET     =  ytick_get, $
                    YTICKFORMAT   = *self.xtickformat, $
                    YTICKINTERVAL = *self.xtickinterval, $
                    YTICKLAYOUT   = *self.xticklayout, $
                    YTICKLEN      = *self.xticklen, $
                    YTICKNAME     = *self.xtickname, $
                    YTICKS        = *self.xticks, $
                    YTICKUNITS    = *self.xtickunits, $
                    YTICKV        = *self.xtickv;, $
                    ;YTITLE        =  cgCheckForSymbols(*self.xtitle)
    endif

    ;Take positive tickmarks
    ;   - If none are positive, take the absolute value
    xtick_get = *self.xtick_get
    ipos = where(xtick_get gt 0, npos)
    if npos gt 0 $
        then xtick_get = xtick_get[ipos] $
        else xtick_get = abs(xtick_get)

    ;Pick the most tickmarks.
    if n_elements(ytick_get) eq 0 then ytick_get = *self.ytick_get
    if n_elements(ytick_get) gt npos $
        then xtick_get = ytick_get $
        else xtick_get = *self.xtick_get
    
;---------------------------------------------------------------------
; Concentric Circles /////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Minimum and maximum radius and polar angle
    _xrange = [min(*self.x0, /NAN), max(*self.x0, /NAN)]
    _yrange = [min(*self.y0, /NAN), max(*self.y1, /NAN)]
    if self.xlog then _xrange = MrLog(_xrange) > 1e-3
    
    ;Create a circle of unit radius
    xcenter = 0
    ycenter = 0
    radius  = 1.0
    points  = linspace(_yrange[0], _yrange[1], 100)
    x = xcenter + radius * cos(points)
    y = ycenter + radius * sin(points)
    
    ;Draw concentric circles
    ring_loc = n_elements(*self.xtickv) gt 0 ? *self.xtickv : xtick_get
    nRings   = n_elements(ring_loc)
    for i = 0, nRings - 1 do begin
        plots, x*ring_loc[i], y*ring_loc[i], $
               /DATA, $
               COLOR     = cgColor(self.pol_rcolor), $
               LINESTYLE = self.pol_rlinestyle, $
               THICK     = self.pol_thick
;               NOCLIP    =  noclip, $
;               PSYM      =  psym, $
;               SYMSIZE   =  symsize, $
;               Z         =  zvalue
    endfor
    
;---------------------------------------------------------------------
; Radial Lines ///////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;If no tick information is given, create something.
    if n_elements(*self.ytickv) + n_elements(*self.yticks) + n_elements(*self.ytickinterval) eq 0 then begin
        _yint = (_yrange[1] - _yrange[0]) * !radeg
        case 1 of
            _yint ge 90: ytickinterval = 45 * !dtor
            _yint ge 45: ytickinterval = 15 * !dtor
            _yint ge 20: ytickinterval = 10 * !dtor
            _yint ge 10: ytickinterval =  5 * !dtor
            _yint ge  4: ytickinterval =  2 * !dtor
            _yint ge  2: ytickinterval =  1 * !dtor
            else: ;Do nothing
        endcase
    endif else begin
        ytickinterval = *self.ytickinterval
    endelse

    ;Let IDL determine the best tickmarks by creating an invisible axis.
    axis, YAXIS         =  0, $
          YRANGE        =  yrange, $
          YSTYLE        =  5, $
          YTICK_GET     = *self.ytick_get, $
          YTICKV        = *self.ytickv, $
          YTICKS        = *self.yticks, $
          YTICKINTERVAL =  ytickinterval
          
    ;Get the tickmarks
    if n_elements(*self.ytickv) gt 0 $
        then ytickmarks = *self.ytickv $
        else ytickmarks = *self.ytick_get
    nmarks = n_elements(ytickmarks)

    ;Draw the radial marks
    for i = 0, nmarks - 1 do begin
        ;Location of the tickmark on the outer circle
        x1 = _xrange[1]*cos(ytickmarks[i])
        y1 = _xrange[1]*sin(ytickmarks[i])
        
        ;Extend to box-axis
        x = xrange[1]
        y = (y1/x1)*x
        if y gt yrange[1] || y lt yrange[0] then begin
            y = yrange[1]
            x = (x1/y1)*y
        endif
    
        ;Draw the radial lines
        plots, [0, x], [0, y] , $
               /DATA, $
               COLOR     = cgColor(self.pol_tcolor), $
               LINESTYLE = self.pol_tlinestyle, $
               THICK     = self.pol_thick
;               NOCLIP    =  noclip, $
;               PSYM      =  psym, $
;               SYMSIZE   =  symsize, $
;               Z         =  zvalue
    endfor
    
end


;+
;   Paint the image pixel-by-pixel.
;-
pro MrImage::doTV
    compile_opt strictarr
    on_error, 2

    ;Use the TV procedure.
    case self.nparams of
        1: tv, *self.img_out
        2: tv, *self.img_out, *self.indep
        3: tv, *self.img_out, *self.indep, *self.dep
        else: message, 'TV unavailable.'
    endcase
end


;+
;   The purpose of this method is to retrieve data
;
; :Calling Sequence:
;       myPlot -> GetData, image
;       myPlot -> GetData, image, x, y
;       myPlot -> GetData, image, x, y, x1, y1
;
; :Params:
;       IMAGE:          out, required, type=numeric array
;                       A named variable into which the image data will be returned.
;       X:              out, optional, type=numeric array
;                       A named variable into which the independent variable data will
;                           be returned. `Y` must also be provided.
;       Y:              out, optional, type=numeric array
;                       A named variable into which the dependent variable data will
;                           be returned. `Y` must also be provided.
;       X1:             out, optional, type=numeric array
;                       A named variable into which the x-corrdinate of the upper-right
;                           corner of each pixel in `IMAGE` will be returned. If present,
;                           then `X` represents the x-coordinate of the lower-right corner
;                           of each pixel. Must be used in conjunction with `Y1`.
;       Y1:             out, optional, type=numeric array
;                       A named variable into which the y-corrdinate of the upper-right
;                           corner of each pixel in `IMAGE` will be returned. If present,
;                           then `Y` represents the y-coordinate of the lower-right corner
;                           of each pixel.
;-
pro MrImage::GetData, image, x, y, x0, y0, x1, y1
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
        1: image = *self.image
        3: begin
            image = *self.image
            x = *self.indep
            y = *self.dep
        endcase
        5: begin
            image = *self.image
            x = *self.Xmin
            y = *self.Ymin
            x0 = *self.Xmax
            y0 = *self.Ymax
        endcase
        7: begin
            image = *self.image
            x = *self.Xmin
            y = *self.Ymin
            x0 = *self.xdelta_minus
            x1 = *self.xdelta_plus
            y0 = *self.ydelta_minus
            y1 = *self.ydelta_plus
        endcase
        else: message, 'Incorrect number of parameters.'
    endcase
end


;+
;   The purpose of this method is to retrieve object properties
;
; :Keywords:
;       BOTTOM:             out, optional, type=byte
;                           If `SCALE` is set, then this is the minimum value of the
;                               scaled image.
;       CTINDEX:            out, optional, type=int
;                           The color table index of a color table to be loaded.
;       IDISPLAY:           in, optional, type=boolean, default=0
;                           The index at which a 2D cut is to be taken. Applicable only
;                               for > 2D image data.
;       AXES:               out, optional, type=boolean
;                           Draw a set of axes around the image.
;       INIT_XRANGE:        out, optional, type=fltarr(2)
;                           The initial state of the XRANGE keyword. This is used to reset
;                               the zoom to its original state.
;       INIT_YRANGE:        out, optional, type=fltarr(2)
;                           The initial state of the YRANGE keyword. This is used to reset
;                               the zoom to its original state.
;       MAX_VALUE:          out, optional, type=float
;                           The maximum value plotted. Any values larger than this are
;                               treated as missing.
;       MIN_VALUE:          out, optional, type=float
;                           The minimum value plotted. Any values smaller than this are
;                               treated as missing.
;       MISSING_VALUE:      out, optional, type=any
;                           A value within `IMAGE` to be treated as missing data.
;       MISSING_COLOR:      out, optional, type=string, default=`FGCOLOR`
;                           The color name of the color in which missing data will be
;                               displayed.
;       NAN:                out, optional, type=boolean
;                           Look for NaN's when scaling the image. Treat them as missing
;                               data.
;       PALETTE:            out, optional, type=bytarr(3\,256)
;                           An [r,g,b] Color table to be loaded before the image is displayed.
;                               This takes precedence over `CTINDEX`.
;       POLAR:              in, optional, type=boolean, default=0
;                           If set, the image will be plotted in polar coordinates, with
;                               `X` and `Y` being the radius and polar angle, respectively.
;       RANGE:              out, optional, type=fltarr(2)
;                           The [minimum, maximum] values of the image to be displayed.
;                               Setting this will cause the color bars to saturated at
;                               the given values.
;       SCALE:              out, optional, type=boolean, default=0
;                           Byte-scale the image.
;       TOP:                out, optional, type=byte
;                           If `SCALE` is set, this will be the maximum value of the
;                               scaled image.
;       TV:                 out, optional, type=Boolean, default=0
;                           If set the image position will be determined by IDL's TV
;                               procedure.
;       X_POS:              out, optional, type=int
;                           If the `TV` keyword is in use, then this specifies the x-
;                               position of the image as specified by the IDL's TV command.
;       XLOG:               out, optional, type=boolean
;                           Indicates that a log scale is used on the x-axis
;       Y_POS:              out, optional, type=int
;                           If the `TV` keyword is in use, then this specifies the y-
;                               position of the image as specified by the IDL's TV command.
;       YLOG:               out, optional, type=boolean
;                           Indicates that a log scale is used on the y-axis
;       _REF_EXTRA:         out, optional, type=any
;                           Keyword accepted by the superclasses are also accepted for
;                               keyword inheritance.
;-
pro MrImage::GetProperty, $
;MrImage Keywords
CHARSIZE = charsize, $
IDISPLAY = iDisplay, $
INIT_XRANGE = init_xrange, $
INIT_YRANGE = init_yrange, $
LAYOUT = layout, $
POSITION = position, $
TV = tv, $
XMIN = xmin, $
XMAX = xmax, $
YMIN = ymin, $
YMAX = ymax, $

;POLAR Plot Options
POLAR = polar, $
POL_AXSTYLE = pol_axstyle, $
POL_RCOLOR = pol_rcolor, $
POL_RLINESTYLE = pol_rlinestyle, $
POL_TCOLOR = pol_tcolor, $
POL_TLINESTYLE = pol_tlinestyle, $
POL_THICK = pol_thick, $

;MrImage.pro Keywords
AXES = axes, $
BOTTOM = bottom, $
CENTER = center, $
CTINDEX = ctindex, $
DATA_POS = data_pos, $
LOG = log, $
MISSING_VALUE = missing_value, $
MISSING_COLOR = missing_color, $
NAN = nan, $
NOCLIP = noclip, $
PAINT = paint, $
PALETTE = palette, $
RANGE = range, $
RLOG = rlog, $
SCALE = scale, $
TOP = top, $

;Graphics Keywords
MAX_VALUE = max_value, $
MIN_VALUE = min_value, $
XLOG = xlog, $
YLOG = ylog, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;MrImage Properties
    if arg_present(charsize)    then self.layout -> GetProperty, CHARSIZE=charsize
    if arg_present(iDisplay)    then iDisplay    =  self.iDisplay
    if arg_present(INIT_XRANGE) then init_xrange =  self.init_xrange
    if arg_present(INIT_YRANGE) then init_yrange =  self.init_yrange
    if arg_present(layout)      then layout      =  self.layout -> GetLayout()
    if arg_present(log)         then log         =  self.log
    if arg_present(position)    then position    =  self.layout -> GetPosition()
    if arg_present(rlog)        then rlog        =  self.rlog
    if arg_present(xmin) then if n_elements(*self.Xmin) gt 0 then xmin = *self.Xmin
    if arg_present(xmax) then if n_elements(*self.Xmax) gt 0 then xmax = *self.Xmax
    if arg_present(ymin) then if n_elements(*self.Ymin) gt 0 then ymin = *self.Ymin
    if arg_present(ymax) then if n_elements(*self.Ymax) gt 0 then ymax = *self.Ymax
    
    ;Polar options
    if arg_present(pol_axstyle)    gt 0 then pol_axstyle    = self.pol_axstyle
    if arg_present(pol_thick)      gt 0 then pol_thick      = self.pol_thick
    if arg_present(pol_rcolor)     gt 0 then pol_rcolor     = self.pol_rcolor
    if arg_present(pol_rlinestyle) gt 0 then pol_rlinestyle = self.pol_rlinestyle
    if arg_present(pol_tcolor)     gt 0 then pol_tcolor     = self.pol_tcolor
    if arg_present(pol_tlinestyle) gt 0 then pol_tlinestyle = self.pol_tlinestyle
    
    ;Graphics Properties
    if arg_present(MAX_VALUE) and n_elements(*self.MAX_VALUE) ne 0 then max_value = *self.max_value
    if arg_present(MIN_VALUE) and n_elements(*self.MIN_VALUE) ne 0 then min_value = *self.min_value
    if arg_present(XLOG)     then xlog = self.xlog
    if arg_present(YLOG)     then ylog = self.ylog

    ;mraImage.pro Properties
    if arg_present(AXES)           then axes          =  self.axes
    if arg_present(BOTTOM)         then bottom        =  self.bottom
    if arg_present(RANGE)          then range         =  self.range
    if arg_present(MISSING_COLOR)  then missing_color =  self.missing_color
    if arg_present(PALETTE)        then palette       =  self.palette
    if arg_present(NAN)            then nan           =  self.nan
    if arg_present(SCALE)          then scale         =  self.scale
    if arg_present(TOP)            then top           =  self.top
    if arg_present(noclip)         && n_elements(*self.noclip)        gt 0 then noclip        =  self.noclip
    if arg_present(CTINDEX)        && n_elements(*self.ctindex)       gt 0 then ctindex       = *self.ctindex
    if arg_present(data_pos)       && n_elements(*self.data_pos)      gt 0 then data_pos      = *self.data_pos
    if arg_present(MISSING_VALUE)  && n_elements(*self.missing_value) gt 0 then missing_value = *self.missing_value
    
    if arg_present(center) then center = self.center
    if arg_present(paint)  then paint  = self.paint
    if arg_present(polar)  then polar  = self.polar
    
    ;MrGraphicsKeywords Properties
    if n_elements(EXTRA) ne 0 then begin
        self -> MrGrAtom::GetProperty, _EXTRA=extra
        self -> MrGraphicsKeywords::GetProperty, _EXTRA=extra
    endif
end


;+
;   Prepare the image to be displayed.
;-
pro MrImage::PrepImage
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Editable copy
    img_out = *self.image

;---------------------------------------------------------------------
; Log-Scale //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if self.log then begin
        img_out = MrLog(img_out)
        
        ;Check for infinities
        iInf = where(finite(img_out, /INFINITY), nInf)
        
        ;Swap infinities for NaNs so that they are removed.
        if nInf gt 0 then begin
            img_out[iInf] = !values.f_nan
            self.nan      = 1
        endif
    endif

;---------------------------------------------------------------------
; Prepare a Mask of Missing Values ///////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(*self.missing_value) gt 0 || self.nan then begin
        ;Convert to float
        ;   - All missing values are converted to NaNs temporarily.
        ;   - Allows setting /NAN keyword in, e.g. BytScl.
        imgType = size(img_out, /TNAME) 
        case imgType of
            'FLOAT':  ;Do nothing
            'DOUBLE': ;Do nothing
            else:     img_out = float(img_out)
        endcase
    
        ;Create a mask. 1=display, 0=hide (mask)
        mask = bytarr(size(img_out, /DIMENSIONS)) + 1B
        
        ;look for NaN's and missing values
        if self.nan $
            then iNaN = where(finite(img_out, /NAN), nNaN) $
            else nNaN = 0
        if n_elements(*self.missing_value) gt 0 $
            then iMissing = where(img_out eq *self.missing_value, nMissing) $
            else nMissing = 0
        
        ;Mask non-data values
        if nNaN     gt 0 then mask[iNaN]     = 0B
        if nMissing gt 0 then mask[iMissing] = 0B

        ;Find missing values
        ikeep = where(mask eq 1, nkeep, COMPLEMENT=iMask, NCOMPLEMENT=nmask)
        
        ;Set them equal to NaN
        if nMask gt 0 then begin
            if imgType eq 'DOUBLE' $
                then img_out[iMask] = !values.d_nan $
                else img_out[iMask] = !values.f_nan
        endif
    endif

;---------------------------------------------------------------------
; Scale the Image ////////////////////////////////////////////////////
;---------------------------------------------------------------------	
	if self.scale then begin
	    range = self.log ? MrLog(self.range) : self.range

        ;Scale the image
        ;   - Set the NaN flag
        img_out = bytscl(img_out, $
                         MIN=range[0], MAX=range[1], $
                         TOP=self.top-self.bottom, $
                         /NAN)
        
        ;BYTSCL scales between 0 and TOP. Bump everything up by BOTTOM.
        if self.bottom ne 0 then img_out += self.bottom
	endif
	
	;Replace missing values with missing index
	if n_elements(nMask) gt 0 $
        then if nMask gt 0 then img_out[iMask] = self.missing_index
	
	;Store the result
	*self.img_out = img_out
end


;+
;   Set the color palette.
;-
pro MrImage::SetPalette, $
BOTTOM = bottom, $
BREWER = brewer, $
CTINDEX = ctindex, $
MISSING_COLOR = missing_color, $
MISSING_INDEX = missing_index, $
NCOLORS = ncolors, $
PALETTE = palette, $
REVERSE = reverse, $
TOP = top
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

;---------------------------------------------------------------------
; Set Properties /////////////////////////////////////////////////////
;--------------------------------------------------------------------- 
    if n_elements(brewer)        gt 0 then self.brewer        = keyword_set(brewer)
    if n_elements(missing_color) gt 0 then self.missing_color = missing_color
    if n_elements(missing_index) gt 0 then self.missing_index = missing_index
    if n_elements(reverse)       gt 0 then self.reverse       = keyword_set(reverse)
    
    ;PALETTE takes precedence over CTINDEX.
    if n_elements(ctindex) gt 0 then *self.ctindex = ctindex
    if n_elements(palette) gt 0 then begin
        self.palette = palette
        void = temporary(*self.ctindex)
    endif

    ;Rescale?
    ;   - If the SCALE property is set, ::PrepImage will make use of TOP and BOTTOM
    ;       when byte-scaling the image.
    ;   - If they change, must rescale.
    ;   - NCOLORS takes precedence over TOP
    nCol       = n_elements(nColors)
    nBottom    = n_elements(bottom)
    nTop       = n_elements(top)
    scale_flag = 0
    if nBottom + nTop + nCol gt 0 then begin
        scale_flag = 1
        if nBottom gt 0 then self.bottom = 0B > bottom < 255B
        if nTop    gt 0 then self.top    = 0B > top    < 255B
        if nCol    gt 0 then self.top    = 0B > self.bottom + nColors - 1 < 255B
    endif
    nColors = self.top - self.bottom + 1

;---------------------------------------------------------------------
; Hide Missing Color? ////////////////////////////////////////////////
;---------------------------------------------------------------------  
    ;Missing index
    ;   - Check if the missing index is at the top/bottom of the color table.
    ;       o Adjust the top/bottom to hide it.
    ;   - The missing color is loaded into the color table at time of draw.
    ;       o Prevent color palette contamination
    ;       o Facilitate change of missing color.
    if self.missing_color ne '' then begin
        if self.missing_index eq !d.table_size-1 then self.top    = self.top    < !d.table_size-2
        if self.missing_index eq 0B              then self.bottom = self.bottom > 1B
    endif

;---------------------------------------------------------------------
;Load a Color Table Index? ///////////////////////////////////////////
;---------------------------------------------------------------------    
    if n_elements(*self.ctindex) gt 0 then begin
        ;Load the color table
        cgLoadCT, *self.ctindex, $
                  BOTTOM    =  self.bottom, $
                  NCOLORS   =       nColors, $
                  REVERSE   =  self.reverse, $
                  BREWER    =  self.brewer, $
                  RGB_TABLE =       tempPalette
        self.palette = temporary(tempPalette)
    endif

;---------------------------------------------------------------------
; Cleanup ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Does the image now need to be scaled?
    if scale_flag then if self.scale then self -> PrepImage
    
    ;Redraw
    self.window -> Draw
end


;+
;   The purpose of this method is to calculate pixel locations.
;
; :Private:
;
; :Keywords:
;       CENTER:         in, optional, type=boolean, default=0
;                       If set, `X` and `Y` are the data locations of the center of each
;                           pixel to be painted on the display. The default is to use
;                           `X` and `Y` as the location of the bottom-left corner of each
;                           pixel. Ignored if more than 3 parameters are given.
;       DIMENSIONS:     in, optional, type=boolean, default=0
;                       If set, `IMAGE` represents the dimensions of and image, not
;                           an actual image.
;       XLOG:           in, optional, type=boolean, default=0
;                       If set, the X-axis will be log-scaled. Ignored if more than
;                           3 parameters are given.
;       YLOG:           in, optional, type=boolean, default=0
;                       If set, the Y-axis will be log-scaled. Ignored if more than
;                           3 parameters are given.
;-
pro MrImage::SetPixelLocations, x, y, x0, y0, x1, y1, $
CENTER=center, $
XLOG=xlog, $
YLOG=ylog
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
 
;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    xlog   = n_elements(xlog)   eq 0 ? self.xlog   : keyword_set(xlog)
    ylog   = n_elements(ylog)   eq 0 ? self.ylog   : keyword_set(ylog)
    center = n_elements(center) eq 0 ? self.center : keyword_set(center)

    ;Polar plots cannot have YLOG set.
    if self.polar && ylog then begin
        message, 'YLOG cannot be set for polar plots. Setting YLOG=0', /INFORMATIONAL
        ylog = 0
    endif 

    ;Paint the image?
    ;   - SetData will set PAINT=1 if more than just X and Y were given. Leave as is.
    ;   - If only X and Y were given, we need to decide if the image needs to be pained.
    if n_params() eq 2 then begin
        if xlog + ylog + center + self.polar gt 0 $
            then self.paint = 1 $
            else self.paint = 0
    endif

    ;If we are not painting, then pixel locations do not need to be determined.
    if self.paint eq 0 then return
    
;---------------------------------------------------------------------
;Pixel Locations /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    dims = size(*self.image, /DIMENSIONS)
    dims = dims[0:1]
    
    ;Determine the corners of each pixel
    ;   - Valid for cartesian and polar plots.
    ;   - Additional work for /RLOG (see SetProperty method).
    case n_params() of
        2: MrPixelPoints,  dims, x, y,                 Xmin, Ymin, Xmax, Ymax, /DIMENSIONS, CENTER=center, XLOG=xlog, YLOG=ylog
        4: MrPixelCorners, dims, x, y, x0, y0,         Xmin, Ymin, Xmax, Ymax, /DIMENSIONS
        6: MrPixelDeltas,  dims, x, y, x0, y0, x1, y1, Xmin, Ymin, Xmax, Ymax, /DIMENSIONS
        else: message, 'Incorrect number of parameters.'
    endcase

    ;Set object properties
     self.xlog   = xlog
     self.ylog   = ylog
     self.center = center
    *self.x0     = xMin
    *self.x1     = xMax
    *self.y0     = yMin
    *self.y1     = yMax
    
    self.window -> Draw
end


;+
;   Set data. Once data is set, the data range will be determined. The NAN property will
;   be set if any NaNs are present in the image. The SCALE property will be set if the
;   image is outside the range of [0,255].
;
;
; Calling Sequence::
;       myGraphic -> GetData, image
;       myGraphic -> GetData, image, x, y
;       myGraphic -> GetData, image, x, y, x0, y0
;       myGraphic -> GetData, image, x, y, x0, y0, x1, y1
;
; :Params:
;       THEIMAGE:       in, required, type=numeric array
;                       Image data.
;       X:              in, optional, type=numeric array
;                       Independent variable data. `Y` must also be provided.
;       Y:              in, optional, type=numeric array
;                       Dependent variable data will.
;       X1:             in, optional, type=numeric array
;                       X-corrdinate of the upper-right corner of each pixel in `IMAGE`.
;                           If present, then `X` represents the x-coordinate of the lower-
;                           right corner of each pixel. Must beused in conjunction with `Y1`.
;       Y1:             in, optional, type=numeric array
;                       Y-corrdinate of the upper-right corner of each pixel in `IMAGE`.
;                           If present, then `Y` represents the y-coordinate of the lower-
;                           right corner of each pixel.
; :Keywords:
;-
pro MrImage::SetData, theImage, x, y, x0, y0, x1, y1, $
TV=tv
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Image dimensions
    imDims = size(theImage, /DIMENSIONS)
    nDims  = size(theImage, /N_DIMENSIONS)
    if n_elements(tv) gt 0 then self.tv = keyword_set(tv)
    
    ;Check number of parameters given
    ;   - n_params() reports the number of arg_present(param).
    ;   - No not count undefined inputs.
    nParams = n_elements(theImage)  eq 0 ? 0 $
                : n_elements(x)  eq 0 ? 1 $
                : n_elements(y)  eq 0 ? 2 $
                : n_elements(x0) eq 0 ? 3 $
                : n_elements(y0) eq 0 ? 4 $
                : n_elements(x1) eq 0 ? 5 $
                : n_elements(y1) eq 0 ? 6 $
                : 7

    ;Retrieve the data
    case nParams of
    ;---------------------------------------------------------------------
    ; Change the Image ///////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        1: begin
            *self.image = theImage
            if self.nparams eq 0 then self.nparams = 1B
        endcase
    
    ;---------------------------------------------------------------------
    ; TV Positioning /////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        2: begin
            if self.tv eq 0 then message, 'Must set TV=1 or provide the Y location.'
            if n_elements(x) ne 1 then message, 'X must be a scalar.'
            
            ;Clear the data
            self -> ClearData, /ALL
            
            ;Set the data
            *self.indep  = x
            *self.image  = theImage
            self.nparams = 2B
        endcase
    
    ;---------------------------------------------------------------------
    ; Pixel Centers //////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        3: begin
            if self.tv then begin
                if n_elements(x) ne 1 then message, 'X must be a scalar when TV=1.'
                if n_elements(y) ne 1 then message, 'Y must be a scalar when TV=1.'
            endif
            
            ;Clear data
            self -> ClearData, /ALL
            
            ;Store the data
            *self.image = theImage
            *self.indep = x
            *self.dep   = y
            
            ;Find the pixel locations
            if self.tv eq 0 then self -> SetPixelLocations, x, y
            self.nparams = 3B
        endcase
        
    ;---------------------------------------------------------------------
    ; Pixel Corners //////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        5: begin
            ;Clear data
            self -> ClearData, /ALL
            
            ;Paint each pixel
            ;   - Must be done before calling ::SetPixelLocations.
            self.paint   = 1B
            self.nparams = 5B
        
            ;Set the data
            *self.image = theImage
            *self.xMin = x
            *self.xMax = x0
            *self.yMin = y
            *self.yMax = y0
            self -> SetPixelLocations, x, y, x0, y0
        endcase
        
    ;---------------------------------------------------------------------
    ; Pixel Deltas ///////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        7: begin
            ;Clear data
            self -> ClearData, /ALL
            
            ;Paint the pixels
            ;   - Must be set before calling ::SetPixelLocations
            self.paint   = 1B
            self.nparams = 7B
            
            ;Store the data
            *self.image        = theImage
            *self.indep        = x
            *self.dep          = y
            *self.xdelta_minus = x0
            *self.xdelta_plus  = x1
            *self.ydelta_minus = y0
            *self.ydelta_plus  = y1
            self -> SetPixelLocations, x, y, x0, y0, x1, y1
        endcase
        else: message, 'Incorrect number of parameters.'
    endcase
        
;---------------------------------------------------------------------
; Dataspace Ranges ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;XRANGE
    case 1 of
        n_elements(*self.x0)    gt 0: *self.xrange = [min(*self.x0, /NAN), max(*self.x1, /NAN)]
        n_elements(*self.indep) gt 0: *self.xrange = [min(*self.indep, MAX=xMax, /NAN), xmax]
        else:                         *self.xrange = [self.xlog, imDims[0]-1]
    endcase
    
    ;YRANGE
    case 1 of
        n_elements(*self.y0)   gt 0: *self.yrange = [min(*self.y0, /NAN), max(*self.y1, /NAN)]
        n_elements(*self.dep)  gt 0: *self.yrange = [min(*self.dep, MAX=yMax, /NAN), yMax]
        else:                        *self.yrange = [self.ylog, imDims[1]-1]
    endcase


;---------------------------------------------------------------------
; Define Dataspace ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;X & Y
    ;   - To make the image zoomable, we need to know the data location of each pixel.
    ;   - These are the pixel centers. If no pixel locations were given, then create them.
    if n_elements(*self.indep) eq 0 then begin
        if self.xlog $
            then *self.indep = logspace(alog10((*self.xrange)[0]), alog10((*self.xrange)[1]), imDims[0]) $
            else *self.indep = linspace((*self.xrange)[0], (*self.xrange)[1], imDims[0])
    endif
    if n_elements(*self.dep) eq 0 then begin
        if self.ylog $
            then *self.dep = logspace(alog10((*self.yrange)[0]), alog10((*self.yrange)[1]), imDims[1]) $
            else *self.dep = linspace((*self.yrange)[0], (*self.yrange)[1], imDims[1])
    endif
        
;---------------------------------------------------------------------
; Scale, NaN, Range //////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;NAN
    iNotFinite = where(finite(*self.image) eq 0, nNotFinite)
    if nNotFinite gt 0 then self.nan = 1B
    
    ;SCALE
    ;   - Determine the min and max values.
    ;   - Avoid missing values.
    if n_elements(*self.missing_value) gt 0 then begin
        iNotMissing = where(*self.image ne *self.missing_value, nNotMissing)
        if nNotMissing gt 0 $
            then imMin = min((*self.image)[iNotMissing], NAN=self.nan, MAX=imMax) $
            else imMin = min((*self.image), NAN=self.nan, MAX=imMax)
    endif else begin
        imMin = min((*self.image), NAN=self.nan, MAX=imMax)
    endelse
    if imMin lt 0 || imMax gt 255 then self.scale = 1B

    ;RANGE
    self.range = [imMin, imMax]

;---------------------------------------------------------------------
; Prep the Image /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    self -> PrepImage
    
    self.window -> draw
end


;+
;   The purpose of this method is to set object properties. 
;
; :Keywords:
;       BOTTOM:             in, optional, type=byte
;                           If `SCALE` is set, then this is the minimum value of the
;                               scaled image.
;       CTINDEX:            in, optional, type=int
;                           The color table index of a color table to be loaded.
;       IDISPLAY:           in, optional, type=boolean, default=0
;                           The index at which a 2D cut is to be taken. Applicable only
;                               for > 2D image data.
;       AXES:               in, optional, type=boolean
;                           Draw a set of axes around the image.
;       MAX_VALUE:          in, optional, type=float
;                           The maximum value plotted. Any values larger than this are
;                               treated as missing.
;       MIN_VALUE:          in, optional, type=float
;                           The minimum value plotted. Any values smaller than this are
;                               treated as missing.
;       MISSING_VALUE:      in, optional, type=any
;                           A value within `IMAGE` to be treated as missing data.
;       MISSING_COLOR:      in, optional, type=string, default=`FGCOLOR`
;                           The color name of the color in which missing data will be
;                               displayed.
;       NAN:                in, optional, type=boolean
;                           Look for NaN's when scaling the image. Treat them as missing
;                               data.
;       PALETTE:            in, optional, type=bytarr(3\,256)
;                           Color table to be loaded before the image is displayed.
;       POLAR:              in, optional, type=boolean, default=0
;                           If set, the image will be plotted in polar coordinates, with
;                               `X` and `Y` being the radius and polar angle, respectively.
;       RANGE:              in, optional, type=fltarr(2)
;                           The [minimum, maximum] values of the image to be displayed.
;                               Setting this will cause the color bars to saturated at
;                               the given values.
;       SCALE:              in, optional, type=boolean, default=0
;                           Byte-scale the image.
;       TOP:                in, optional, type=byte
;                           If `SCALE` is set, this will be the maximum value of the
;                               scaled image.
;       TV:                 in, optional, type=Boolean, default=0
;                           If set the image position will be determined by IDL's TV
;                               procedure.
;       XLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the x-axis
;       YLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the y-axis
;       _REF_EXTRA:         in, optional, type=any
;                           Keyword accepted by the superclasses are also accepted for
;                               keyword inheritance.
;-
pro MrImage::SetProperty, $
;MrImage Keywords
IDISPLAY = iDisplay, $
TV = tv, $
      
;mraImage Keywords
AXES = axes, $
CENTER = center, $
DATA_POS = data_pos, $
LOG = log, $
MISSING_VALUE = missing_value, $
NAN = nan, $
NOCLIP = noclip, $
RANGE = range, $
SCALE = scale, $

;POLAR Plot Options
POLAR = polar, $
POL_AXSTYLE = pol_axstyle, $
POL_RCOLOR = pol_rcolor, $
POL_RLINESTYLE = pol_rlinestyle, $
POL_TCOLOR = pol_tcolor, $
POL_TLINESTYLE = pol_tlinestyle, $
POL_THICK = pol_thick, $

;Graphics Keywords
XLOG = xlog, $
YLOG = ylog, $
_REF_EXTRA = extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    reprep_flag = 0B

    ;MrImage Keywords
    if n_elements(iDisplay)    ne 0 then self.iDisplay = iDisplay
    if n_elements(TV)          ne 0 then self.tv = keyword_set(tv)

    ;mraImage.pro Properties
    if n_elements(axes)           gt 0 then  self.axes           = keyword_set(axes)
    if n_elements(data_pos)       gt 0 then *self.data_pos       = data_pos
    if n_elements(noclip)         ne 0 then *self.noclip         = keyword_set(noclip)
    if n_elements(pol_axstyle)    gt 0 then  self.pol_axstyle    = pol_axstyle
    if n_elements(pol_thick)      gt 0 then  self.pol_thick      = pol_thick
    if n_elements(pol_rcolor)     gt 0 then  self.pol_rcolor     = pol_rcolor
    if n_elements(pol_rlinestyle) gt 0 then  self.pol_rlinestyle = pol_rlinestyle
    if n_elements(pol_tcolor)     gt 0 then  self.pol_tcolor     = pol_tcolor
    if n_elements(pol_tlinestyle) gt 0 then  self.pol_tlinestyle = pol_tlinestyle
    
    ;SCALE
    ;   - RANGE is set.
    nRange = n_elements(range)
    if nRange gt 0 then begin
        ;User-defined range?
        if range[0] ne range[1] then begin
            self.range = range
            if self.scale eq 0 then scale = 1
            
        ;Automatic range
        endif else begin
            self.range = [min(*self.image, MAX=maxIm, /NAN), maxIm]
        endelse
    endif
    
    ;PREP-IMAGE
    ;   - These keywords require the image to be re-prepped
    nScale   = n_elements(scale)
    nNaN     = n_elements(nan)
    nMissing = n_elements(missing_value)
    nLog     = n_elements(log)
    nRange   = n_elements(range)
    if nScale + nNaN + nMissing + nLog + nRange gt 0 then begin
        if nScale   gt 0 then  self.scale         = keyword_set(scale)
        if nNaN     gt 0 then  self.nan           = keyword_set(nan)
        if nMissing gt 0 then *self.missing_value = missing_value
        if nLog     gt 0 then  self.log           = keyword_set(log)
        reprep_flag = 1B
    endif
    
    ;PAINT
    ;   - Parameters used to determine if image is painted or pasted.
    ;   - It is set automatically in ::SetData and ::SetPixelLocations.
    ;   - It can only be toggled with the CENTER, POLAR, and [XYR]LOG keywords
    ;   - It takes priority over DPOSITION. If PAINT is set, DPOSITION is ignored in ::Draw.
    nxlog   = n_elements(xlog)
    nylog   = n_elements(ylog)
    nCenter = n_elements(center)
    nPolar  = n_elements(polar)
    if nxlog + nylog + nCenter + nPolar gt 0 then begin
        if nPolar  gt 0 then self.polar  = keyword_set(polar)
        if nxlog   gt 0 then self.xlog   = keyword_set(xlog)
        if nylog   gt 0 then self.ylog   = keyword_set(ylog)
        if nCenter gt 0 then self.center = keyword_set(center)

        ;If only X and Y were given, pixel locations must be recalculated
        if self.nparams eq 3 && self.tv eq 0 $
            then self -> SetPixelLocations, *self.indep, *self.dep
    endif
    
    ;Superclass properties
    if n_elements(extra) gt 0 $
        then self -> MrGrDataAtom::SetProperty, _EXTRA=extra

    ;Prep the image?
    if reprep_flag then self -> PrepImage

    ;Refresh the window
    self.window -> Draw
end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrImage::cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;free all pointers
    ptr_free, self.image
    ptr_free, self.img_out
    ptr_free, self.dep
    ptr_free, self.indep
    ptr_free, self.x0
    ptr_free, self.x1
    ptr_free, self.y0
    ptr_free, self.y1
    ptr_free, self.ctindex
    ptr_free, self.data_pos
    ptr_free, self.missing_value
    ptr_free, self.xdelta_minus
    ptr_free, self.xdelta_plus
    ptr_free, self.Xmin
    ptr_free, self.Xmax
    ptr_free, self.ydelta_minus
    ptr_free, self.ydelta_plus
    ptr_free, self.Ymin
    ptr_free, self.Ymax
    
    ;Superclasses
    self -> MrGrDataAtom::cleanup
end


;+
;   The purpose of this method is to initialize the MrImages class.
;
; :Params:
;       IMAGE:          in, required, type=NxM numeric array
;                       Image to be displayed
;       X:              in, optional, type=scalar/Nx1 numeric
;                       If a scalar, then positioning is that of IDL's TV function. If a
;                           vector the same size as the first dimension of `IMAGE`, then
;                           the data coordinates of each pixel. See also, `PAINT`.
;       Y:              in, optional, type=scalar/Mx1 numeric
;                       If a scalar, then positioning is that of IDL's TV function. If a
;                           vector the same size as the second dimension of `IMAGE`, then
;                           the data coordinates of each pixel. See also, `PAINT`.
;       X0:             in, optional, type=1D vector/2D array
;                       The x-coordinate, in data coordinates, of the upper-right corner
;                           of each pixel in `IMAGE`. If supplied, then `X` markes the
;                           x-coordinate of the lower-left corner of each pixel in `IMAGE`.
;                           Must be used with `Y0`. If given, `PAINT` will be set to 1.
;       Y0:             in, optional, type=1D vector/2D array
;                       The y-coordinate, in data coordinates, of the upper-right corner
;                           of each pixel in `IMAGE`. If supplied, then `Y` markes the
;                           y-coordinate of the lower-left corner of each pixel in `IMAGE`.
;                           Must be used with `X0`. If given, `PAINT` will be set to 1.
;       X1:             in, optional, type=scalar/1D vector/2D array
;                       If provided, then `X` and `Y` are the "center" locations of each
;                           pixel of `IMAGE`. `X0` and X1 mark the displacement to the
;                           left and right edges of each pixel, respectively. X1 and `Y1`
;                           must be supplied together. If provided, `PAINT` will be set.
;       Y1:             in, optional, type=scalar/1D vector/2D array
;                       If provided, then `X` and `Y` are the "center" locations of each
;                           pixel of `IMAGE`. `Y0` and Y1 mark the displacement to the
;                           bottom and top edges of each pixel, respectively. `X1` and Y1
;                           must be supplied together. If provided, `PAINT` will be set.
;
; :Keywords:
;       AXES:               in, optional, type=boolean, default=0
;                           Draw a set of axes around the image.
;       BOTTOM:             in, optional, type=byte, default=0
;                           If `SCALE` is set, then this is the minimum value of the
;                               scaled image.
;       CTINDEX:            in, optional, type=int
;                           The color table index of a color table to be loaded.
;       IDISPLAY:           in, optional, type=boolean, default=0
;                           Normally, `IMAGE` is assumed to be 2D with the dimensions 
;                               ordered as [x,y]. If `IMAGE` is >2D, data dimensions are
;                               assumed to be ordered [x,y,A,B,C,...] and `IDISPLAY` is
;                               the index within the dimensions [A,B,C,...] at which the
;                               2D image will be displayed.
;       MIN_VALUE:          in, optional, type=float
;                           The minimum value plotted. Any values smaller than this are
;                               treated as missing.
;       MISSING_VALUE:      in, optional, type=any
;                           A value within `IMAGE` to be treated as missing data.
;       MISSING_COLOR:      in, optional, type=string, default=`FGCOLOR`
;                           The color name of the color in which missing data will be
;                               displayed.
;       MAX_VALUE:          in, optional, type=float
;                           The maximum value plotted. Any values larger than this are
;                               treated as missing.
;       NAN:                in, optional, type=boolean, default=0
;                           Look for NaN's when scaling the image. Treat them as missing
;                               data.
;       PALETTE:            in, optional, type=bytarr(3\,256)
;                           Color table to be loaded before the image is displayed.
;       POL_AXSTYLE:        in, optional, type=integer, default=1
;                           A bit-wise mask determining how axes should be drawn when
;                               the `POLAR` keyword is set. Options are::
;                                   1   -   Box axes
;                                   2   -   Horizontal axis through r = 0
;                                   4   -   Vertical axis through r = 0
;       RANGE:              in, optional, type=fltarr(2)
;                           The [minimum, maximum] values of the image to be displayed.
;                               Setting this will cause the color bars to saturated at
;                               the given values.
;       SCALE:              in, optional, type=boolean, default=0
;                           Byte-scale the image.
;       TOP:                in, optional, type=byte, default=255
;                           If `SCALE` is set, this will be the maximum value of the
;                               scaled image.
;       TV:                 in, optional, type=Boolean, default=0
;                           If set, `X` and `Y` are taken to be the position of the image,
;                               as defined by IDL's TV procedure.
;       XLOG:               in, optional, type=boolean, default=0
;                           Indicates that a log scale is used on the x-axis
;       XRANGE:             in, optional, type=fltarr(2), default=[min(`X`)\, max(`X`)]
;                           The x-axis range over which the data will be displayed.
;       YLOG:               in, optional, type=boolean, default=0
;                           Indicates that a log scale is used on the y-axis
;       YRANGE:             in, optional, type=fltarr(2), default=[min(`Y`)\, max(`Y`)]*1.05
;                           The y-axis range over which the data will be displayed.
;       _REF_EXTRA:         in, optional, type=any
;                           Keyword accepted by the superclasses are also accepted for
;                               keyword inheritance.
;-
function MrImage::init, theImage, x, y, x0, y0, x1, y1, $
;MrImage Keywords
CURRENT = current, $
HIDE = hide, $
IDISPLAY = idisplay, $
KEEP_ASPECT = keep_aspect, $
LAYOUT = layout, $
NAME = name, $
POSITION = position, $
TV = tv, $

;POLAR Plot Options
POLAR = polar, $
POL_AXSTYLE = pol_axstyle, $
POL_RCOLOR = pol_rcolor, $
POL_RLINESTYLE = pol_rlinestyle, $
POL_TCOLOR = pol_tcolor, $
POL_TLINESTYLE = pol_tlinestyle, $
POL_THICK = pol_thick, $

;IMAGE_PLOTS Keywords
AXES = axes, $
AXISCOLOR = axiscolor, $
BOTTOM = bottom, $
CENTER = center, $
CTINDEX = ctindex, $
DATA_POS = data_pos, $
LOG = log, $
MISSING_VALUE = missing_value, $
MISSING_COLOR = missing_color, $
MISSING_INDEX = missing_index, $
NAN = nan, $
PAINT = paint, $
PALETTE = palette, $
RANGE = range, $
SCALE = scale, $
TOP = top, $

;Graphics Keywords
TITLE = title, $
XLOG = xlog, $
XRANGE = xrange, $
XTICKLEN = xticklen, $
XTITLE = xtitle, $
YLOG = ylog, $
YRANGE = yrange, $
YTICKLEN = yticklen, $
YTITLE = ytitle, $
_REF_EXTRA = extra
    compile_opt strictarr

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif

;---------------------------------------------------------------------
; Superclasses ///////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Sets up window -- Must be done before calling any method that subsequently
    ;                  calls the draw method.
    success = self -> MrGrDataAtom::Init(CURRENT=current, HIDE=hide, LAYOUT=layout, $
                                         NAME=name, OVERPLOT=overplot, POSITION=position, $
                                         REFRESH=refresh, WINDOW_TITLE=window_title, $
                                         _EXTRA=extra)
    if success eq 0 then message, 'Unable to initialize superclass MrGrDataAtom.'

;---------------------------------------------------------------------
; Defaults ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;PAINT-related keywords
;    if n_elements(xFillVal) eq 0 then xFillVal = !values.f_nan
;    if n_elements(yFillVal) eq 0 then yFillVal = !values.f_nan
    
    ;Coordinates
	;   - Default to normal coordinates
	normal = keyword_set(normal)
	device = keyword_set(device)
	if normal + device eq 0 then normal = 1
	if normal + device gt 1 then $
	    message, 'DEVICE and NORMAL are mutually exclusive.'
	
	;Polar options
	if n_elements(pol_thick)      eq 0 then pol_thick      = 1
	if n_elements(pol_axstyle)    eq 0 then pol_axstyle    = 1
	if n_elements(pol_rlinestyle) eq 0 then pol_rlinestyle = 1
	if n_elements(pol_tlinestyle) eq 0 then pol_tlinestyle = 1
    
    ;Make sure the titles are defined so we can use cgCheckForSymbols in ::Draw.
    if n_elements(title)  eq 0 then  title = ''
    if n_elements(xtitle) eq 0 then xtitle = ''
    if n_elements(ytitle) eq 0 then ytitle = ''
    
    ;Have tickmarks pointing outward instead of inward.
    if n_elements(xticklen) eq 0 then xticklen = -0.02
    if n_elements(yticklen) eq 0 then yticklen = -0.02
    
;---------------------------------------------------------------------
;Allocate Heap to Pointers ///////////////////////////////////////////
;---------------------------------------------------------------------
    self.image         = ptr_new(/ALLOCATE_HEAP)
    self.img_out       = ptr_new(/ALLOCATE_HEAP)
    self.indep         = ptr_new(/ALLOCATE_HEAP)
    self.dep           = ptr_new(/ALLOCATE_HEAP)
    self.x0            = ptr_new(/ALLOCATE_HEAP)
    self.x1            = ptr_new(/ALLOCATE_HEAP)
    self.y0            = ptr_new(/ALLOCATE_HEAP)
    self.y1            = ptr_new(/ALLOCATE_HEAP)
    self.ctindex       = ptr_new(/ALLOCATE_HEAP)
    self.data_pos      = ptr_new(/ALLOCATE_HEAP)
    self.missing_value = ptr_new(/ALLOCATE_HEAP)
    self.xdelta_minus  = ptr_new(/ALLOCATE_HEAP)
    self.xdelta_plus   = ptr_new(/ALLOCATE_HEAP)
    self.ydelta_minus  = ptr_new(/ALLOCATE_HEAP)
    self.ydelta_plus   = ptr_new(/ALLOCATE_HEAP)
    self.xMax          = ptr_new(/ALLOCATE_HEAP)
    self.xMin          = ptr_new(/ALLOCATE_HEAP)
    self.yMax          = ptr_new(/ALLOCATE_HEAP)
    self.yMin          = ptr_new(/ALLOCATE_HEAP)
        
;---------------------------------------------------------------------
; Color Palette //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Colors
	;   - Foreground, Background, and Missing Index.
	;   - Default to putting the missing value at the top of the color table.
	;   - Use as much of the color table as possible.
	;   - Make sure a color table or color palette was given.
	brewer  = keyword_set(brewer)
	reverse = keyword_set(reverse)
	if n_elements(color)         eq 0 then color         = 'black'
	if n_elements(axiscolor)     eq 0 then axiscolor     = color
	if n_elements(pol_rcolor)    eq 0 then pol_rcolor    = color
	if n_elements(pol_tcolor)    eq 0 then pol_tcolor    = color
    if n_elements(background)    eq 0 then background    = 'white'
    if n_elements(missing_color) eq 0 then missing_color = ''
    if n_elements(missing_index) eq 0 then missing_index = !d.table_size - 1
    if n_elements(top)           eq 0 then top           = !d.table_size - 1
	if n_elements(palette) eq 0 && n_elements(ctindex) eq 0 then ctindex = 13

	;Set the palette
	;   - Do not set the SCALE property before now.
	self -> SetPalette, BOTTOM        = bottom, $
                        BREWER        = brewer, $
                        CTINDEX       = ctindex, $
                        MISSING_COLOR = missing_color, $
                        MISSING_INDEX = missing_index, $
                        NCOLORS       = ncolors, $
                        PALETTE       = palette, $
                        REVERSE       = reverse, $
                        TOP           = top

;---------------------------------------------------------------------
; Set Data ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;
    ; Set properties needed for ::PrepImage and ::SetPixelLocations
    ; before either of them are called in ::SetData
    ;
    
    ;Pixel-related properties
    ;   - Those not set in the colors section above.
    self.log   = keyword_set(log)
    self.polar = keyword_set(polar)
    self.tv    = keyword_set(tv)
    
    ;Image output-related properties
    ;   - Scale the image if range[0] ne range[1] (user-given range)
    if n_elements(range) eq 0 then range = [0,0]
    if range[0] ne range[1]   then scale = 1B
    self.scale = keyword_set(scale)
	self.nan   = keyword_set(nan)
    if n_elements(missing_value) gt 0 then *self.missing_value = missing_value

    ;Set the data
    self -> SetData, theImage, x, y, x0, y0, x1, y1
    
;---------------------------------------------------------------------
;Check/Set Keywords //////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;RANGE
    ;   - Automatic range has been set in ::SetData.
    if range[0] ne range[1] then self.range = range

    ;Set the object properties
    ;   - [XY]RANGE must be set after the data is set.
    self -> SetProperty, AXES           = axes, $
                         AXISCOLOR      = axiscolor, $
                         CENTER         = center, $
                         DATA_POS       = data_pos, $
                         DEVICE         = device, $
                         IDISPLAY       = iDisplay, $
                         MAX_VALUE      = max_value, $
                         MIN_VALUE      = min_value, $
                         NOCLIP         = noclip, $
                         NORMAL         = normal, $
                         POL_AXSTYLE    = pol_axstyle, $
                         POL_RCOLOR     = pol_rcolor, $
                         POL_RLINESTYLE = pol_rlinestyle, $
                         POL_TCOLOR     = pol_tcolor, $
                         POL_TLINESTYLE = pol_tlinestyle, $
                         POL_THICK      = pol_thick, $
                         TITLE          = title, $
                         XLOG           = xlog, $
                         XRANGE         = xrange, $
                         XTICKLEN       = xticklen, $
                         XTITLE         = xtitle, $
                         YLOG           = ylog, $
                         YRANGE         = yrange, $
                         YTICKLEN       = yticklen, $
                         YTITLE         = ytitle

;---------------------------------------------------------------------
;Styles and Ranges ///////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Make sure the x- and y-style keywords have the 2^0 bit set to force
    ;exact axis ranges.    
    if n_elements(*self.xstyle) eq 0 $
        then *self.xstyle = 1 $
        else *self.xstyle += ~(*self.xstyle and 1)
        
    if n_elements(*self.ystyle) eq 0 $
        then *self.ystyle = 1 $
        else *self.ystyle += ~(*self.ystyle and 1)
            
    ;Set the initial x- and y-range
    self.init_xrange = *self.xrange
    self.init_yrange = *self.yrange
    self.init_range  =  self.range

    ;Refresh the graphics?
    if refresh then self -> Refresh

    return, 1
end


;+
;   Object class definition
;-
pro MrImage__define
    compile_opt strictarr
    
    define = { MrImage, $
               inherits MrGrDataAtom, $
              
               ;Data
               indep:   ptr_new(), $             ;independent variable
               dep:     ptr_new(), $             ;dependent variable
               image:   ptr_new(), $             ;image to be displayed
               img_out: ptr_new(), $
               x0:      ptr_new(), $
               x1:      ptr_new(), $
               y0:      ptr_new(), $
               y1:      ptr_new(), $
               tv: 0B, $                         ;indicate that a TV position was given
               
               ;Type of data given
               nparams: 0B, $
              
               ;MrImage Properties
               idisplay:    0L, $                ;Index to display (>3D images)
               init_range:  dblarr(2), $         ;Initial image range
               init_xrange: dblarr(2), $         ;Initial x-range
               init_yrange: dblarr(2), $         ;Initial y-range
               
               ;POLAR options
               polar:          0B, $            ;Create a polar image?
               pol_axstyle:    0S, $
               pol_rcolor:     '', $
               pol_rlinestyle: 0S, $
               pol_tcolor:     '', $
               pol_tlinestyle: 0S, $
               pol_thick:      0S, $
               
               ;mraImage Keywords
               axes:          0B, $             ;Draw axes around the image?
               bottom:        0B, $             ;If scaled, minimum scaled value
               brewer:        0B, $             ;Use a brewer color table?
               center:        0B, $             ;Center of pixel locations was given?
               ctindex:       ptr_new(), $      ;Color index to load
               data_pos:      ptr_new(), $      ;A data position for the image
               missing_value: ptr_new(), $      ;Value to be treated as missing
               missing_color: '', $             ;Color of missing value
               missing_index: 0B, $             ;Color table index of missing color
               nan:           0B, $             ;Search for NaN's when scaling?
               log:           0B, $             ;Log-scale the image.
               paint:         0B, $             ;Paint the image pixel-by-pixel?
               palette:       bytarr(256,3), $  ;Color table to be loaded
               range:         dblarr(2), $      ;Range at which the color table saturates
               reverse:       0B, $             ;Reverse the color table?
               scale:         0B, $             ;Byte-scale the image
               top:           0B, $             ;If scaled, maximum scaled value
               xdelta_minus:  ptr_new(), $      ;Distance from center of pixel to left edge
               xdelta_plus:   ptr_new(), $      ;Distance from center of pixel to right edge
               xMin:          ptr_new(), $      ;Left edge of pixels
               xMax:          ptr_new(), $      ;Right edge of pixels
               ydelta_minus:  ptr_new(), $      ;Distance from center of pixel to bottom edge
               ydelta_plus:   ptr_new(), $      ;Distance from center of pixel to top edge
               Ymin:          ptr_new(), $      ;Bottom of pixels
               Ymax:          ptr_new() $       ;Top of pixels
             }
end