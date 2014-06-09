; docformat = 'rst'
;
; NAME:
;       MrImage__Define
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
;+
;   The purpose of this method is to create an image object with set, get, and draw
;   methods.
;
; :Examples:
;   Display an image as the TV procedure would::
;       data = cgDemoData(12)
;       im = MrImage(data, /TV)
;
;   Tile the image as TV would::
;       data = cgDemoData(12)
;       dims = size(data, /DIMENSIONS)
;       theWin = MrWindow(XSIZE=dims[0]*2, YSIZE=dims[1]*2)
;       for i = 0, 3 do !Null = MrImage(data, i, /TV, /CURRENT)
;  
;   Position the image as TV would::
;       data = cgDemoData(12)
;       theIm = MrImage(data, 30, 40, /TV)
;
;   Display the image centered in the window::
;       data = cgDemoData(12)
;       theIm = MrImage(data)
;
;   Display the image in color::
;       data = cgDemoData(12)
;       theIm = MrImage(data, CTINDEX=34)
;
;   Display the image with coordinate axes
;       data = cgDemoData(12)
;       theIm = MrImage(data, CTINDEX=24, /AXES, TITLE='M51 Whirlpool Galaxy', $
;                       YTITLE='Light Years', XTITLE='Distance (1000km)')
;
;   Give the image a data space with evenly spaced grid::
;       data = cgDemoData(12)
;       distance   = findgen(dims[0]) / (dims[0]-1)*9.46
;       lightYears = findgen(dims[1]) / (dims[1]-1)
;       theIm = MrImage(data, distance, lightYears, CTINDEX=24, /AXES, TITLE='M51 Whirlpool Galaxy', $
;                       YTITLE='Light Years', XTITLE='Distance (10$\up12$km)')
;
;   Display an image on a log scale::
;       data = cgDemoData(12)
;       distance   = findgen(dims[0]) / (dims[0]-1)*9.46
;       lightYears = findgen(dims[1]) / (dims[1]-1)
;       theIm = MrImage(data, distance, lightYears, CTINDEX=24, /AXES, TITLE='M51 Whirlpool Galaxy', $
;                       YTITLE='Light Years', XTITLE='Distance (10$\up12$km)', /XLOG, /YLOG, $
;                       XRANGE=[0.1, 10], YRANGE=[0.1, 1])
;
;   Provide the location of the center of each pixel::
;       data = dist(20)
;       dims = size(data, /DIMENSIONS)
;       x_center = findgen(dims[0]) + 1
;       y_center = findgen(dims[0]) + 1
;       theImage = MrImage(data, x_center, y_center, /CENTER, /AXES, $
;                          CTINDEX=22, XRANGE=[0,21], YRANGE=[0,21])
;
;   Provie the location of the corners of each pixel::
;       data = dist(20)
;       dims = size(data, /DIMENSIONS)
;       x_ll = findgen(dims[0]) + 0.25
;       x_ur = findgen(dims[0]) + 0.75
;       y_ll = findgen(dims[1]) + 0.1
;       y_ur = findgen(dims[1]) + 1.1
;       theImage = MrImage(data, x_ll, y_ll, x_ur, y_ur, /AXES, CTINDEX=22)
;
;   Provide the location of the center of each pixel as well as the offset to the edges::
;       data = dist(20)
;       dims = size(data, /DIMENSIONS)
;       x_center = findgen(dims[0]) + 0.5
;       x_dminus = 0.5
;       x_dplus  = linspace(0, 0.5, dims[0])
;       y_center = findgen(dims[1]) + 0.5
;       y_dminus = 0.5
;       y_dplus = linspace(0, 0.5, dims[1])
;       theImage = MrImage(data, x_center, y_center, x_dminus, y_dminus, x_dplus, y_dplus, $
;                          /AXES, CTINDEX=22)
;
; :Uses:
;   Uses the following external programs::
;       cgDemoData.pro (Coyote Graphics)
;       cgErrorMsg.pro (Coyote Graphics)
;       setDefaultValue.pro (Coyote Graphics)
;       mraImage.pro
;       MrGrDataAtom__define.pro
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
;       04/22/2013  -   Written by Matthew Argall
;       05/04/2013  -   Do not allow zooming outside of the data range. - MRA
;       05/10/2013  -   Inherit MrAbstractColorBar. - MRA
;       05/12/2013  -   Inherit MrAbstractAxes. Repurposed AXES keyword for adding
;                           MrAbstractAxes objects. Added IMAXES to draw a set of axes
;                           around the image. - MRA
;       06/13/2013  -   Renamed from MrImagePlot__define.pro to MrImage__define.pro.
;                           Removed all gui-window related keywords. Do not inherit
;                           MrPlotLayout or MrDrawWindow. Renamed IMRANGE to RANGE to be
;                           consistent across the various different types of objects. - MRA
;       07/10/2013  -   Added the iDisplay property to handle >2D image data. Disinherit
;                           MrAbstractAxes, MrAbstractColorbar, MrAbstractLegend. Change
;                           the IMAXES keyword back to AXES. - MRA
;       08/01/2013  -   Added the ConvertCoord method. - MRA
;       08/03/2013  -   Added the PALETTE property. - MRA
;       08/12/2013  -   Added the LOG property. - MRA
;       08/13/2013  -   Removed the LOG property because it causes a discrpency between
;                           the image data and the data being plotted. This causes the
;                           automatic zooming to go awry. - MRA
;       08/23/2013  -   Added the LAYOUT keyword, removed the COLORBARS and ADDCOLORBAR
;                           keywords. Inherit MrIDL_Container. - MRA
;       09/21/2013  -   NAN keyword was not checked when finding default RANGE. Fixed.
;                           RANGE is now exclusively an input. - MRA
;       09/23/2013  -   Draw all objects in the container. Position and layout properties
;                           moved to MrGraphicAtom. - MRA
;       09/29/2013  -   Ensure that the layout is updated only when a layout keyword is
;                           passed in. - MRA
;       2013/10/25  -   The position of the image is now always defined via the DPOSITION
;                           keyword to IMAGE_PLOTS. This makes zooming simpler. - MRA
;       2013/11/17  -   Added the _TF_Paint and SetPixelLocations methods. Use MrImage.pro
;                           instead of Image_Plots.pro to allow for [XY]LOG-scaling of
;                           images and ability to draw image pixels in different sizes. - MRA
;       2013/11/20  -   MrIDL_Container and MrGraphicAtom is disinherited. Inherit instead
;                           MrGrAtom and MrLayout. - MRA
;       2013/11/22  -   Renamed DRAW to REFRESH. Refreshing is now done automatically.
;                           Call the Refresh method with the DISABLE keyword set to
;                           temporarily turn of Refresh. - MRA
;       2013/12/30  -   CTIndex takes precedence over PALETTE when both were present. Now,
;                           if PALETTE is set, CTINDEX is cleared, and vice versa. - MRA
;       2014/01/24  -   Added the _OverloadImpliedPrint and _OverloadPrint methods. - MRA
;       2014/02/28  -   Find ranges using MIN() and MAX(). - MRA
;       2014/03/10  -   Disinherit MrLayout, but keep it as an object property. Added
;                           the GetLayout method. Picking an initial window is no longer
;                           an obscure process. - MRA
;       2014/03/26  -   Rewrote the SetData method to deal with the different number
;                           of parameters. Remove all determination of ranges and
;                           in/dependent variable data from the INIT method. PAINT is
;                           now an internal property only. [XY]LOG successfully update
;                           the pixel locations when 3 parameters are given. Inherit
;                           MrGrDataAtom and removed duplicate properties and methods. - MRA
;       2014/05/15  -   XLOG, YLOG, and CENTER now work properly, regardless of the number
;                           of inputs given. - MRA
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
    
    ;Object Properties
    tv       = string('TV',       '=', self.tv,       FORMAT='(a-26, a-2, i1)')
    idisplay = string('iDisplay', '=', self.idisplay, FORMAT='(a-26, a-2, i0)')
    center   = string('Center',   '=', self.center,   FORMAT='(a-26, a-2, i1)')
    paint    = string('Paint',    '=', self.paint,    FORMAT='(a-26, a-2, i1)')
    
    ;Pointers
    axes          = string('Axes',          '=', FORMAT='(a-26, a-2)')
    bottom        = string('Bottom',        '=', FORMAT='(a-26, a-2)')
    ctindex       = string('CTIndex',       '=', FORMAT='(a-26, a-2)')
    data_pos      = string('Data_Pos',      '=', FORMAT='(a-26, a-2)')
    missing_value = string('Missing_Value', '=', FORMAT='(a-26, a-2)')
    missing_color = string('Missing_Color', '=', FORMAT='(a-26, a-2)')
    nan           = string('NaN',           '=', FORMAT='(a-26, a-2)')
    palette       = string('Palette',       '=', FORMAT='(a-26, a-2)')
    range         = string('Range',         '=', FORMAT='(a-26, a-2)')
    scale         = string('Scale',         '=', FORMAT='(a-26, a-2)')
    top           = string('Top',           '=', FORMAT='(a-26, a-2)')
    
    ;Value or Undefined?
    if n_elements(*self.axes)          eq 0 then axes          += undefined else axes          += string(*self.axes,          FORMAT='(i1)')
    if n_elements(*self.bottom)        eq 0 then bottom        += undefined else bottom        += string(*self.bottom,        FORMAT='(i0)')
    if n_elements(*self.ctindex)       eq 0 then ctindex       += undefined else ctindex       += string(*self.ctindex,       FORMAT='(i0)')
    if n_elements(*self.data_pos)      eq 0 then data_pos      += undefined else data_pos      += string(*self.data_pos,      FORMAT='(4(f0, 3x))')
    if n_elements(*self.missing_value) eq 0 then missing_value += undefined else missing_value += string(*self.missing_value, FORMAT='(f0)')
    if n_elements(*self.missing_color) eq 0 then missing_color += undefined else missing_color += string(*self.missing_color, FORMAT='(a0)')
    if n_elements(*self.nan)           eq 0 then nan           += undefined else nan           += string(*self.nan,           FORMAT='(i1)')
    if n_elements(*self.range)         eq 0 then range         += undefined else range         += string(*self.range,         FORMAT='(2(f0, 3x))')
    if n_elements(*self.scale)         eq 0 then scale         += undefined else scale         += string(*self.scale,         FORMAT='(i1)')
    if n_elements(*self.top)           eq 0 then top           += undefined else top           += string(*self.top,           FORMAT='(i0)')
    if n_elements(*self.max_value)     eq 0 then max_value     += undefined else max_value     += string(*self.max_value,     FORMAT='(f0)')
    if n_elements(*self.min_value)     eq 0 then min_value     += undefined else min_value     += string(*self.min_value,     FORMAT='(f0)')

    ;Get the help string
    if n_elements(*self.palette) gt 0 then begin
        help, *self.palette, OUTPUT=tempStr
        palette += tempStr
    endif else palette += undefined 
    
    selfStr = obj_class(self) + '  <' + strtrim(obj_valid(self, /GET_HEAP_IDENTIFIER), 2) + '>'
    imKeys = [ tv, $
               idisplay, $
               center, $
               paint, $
               axes, $
               bottom, $
               ctindex, $
               data_pos, $
               missing_value, $
               missing_color, $
               nan, $
               palette, $
               range, $
               scale, $
               top $
             ]

    result = [[grKeys], [transpose(imKeys)]]
    result = [[selfStr], ['  ' + result[sort(result)]]]
    
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
CENTERS=centers, $
CORNERS=corners, $
DELTAS=deltas, $
INDEP=indep, $
DEP=dep, $
XDELTA_PLUS=xdelta_plus, $
XDELTA_MINUS=xdelta_minus, $
XMAX=xmax, $
XMIN=xmin, $
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
    indep        = keyword_set(indep)
    dep          = keyword_set(dep)
    xdelta_plus  = keyword_set(xdelta_plus)
    xdelta_minus = keyword_set(xdelta_minus)
    xmax         = keyword_set(xmax)
    xmin         = keyword_set(xmin)
    ydelta_plus  = keyword_set(ydelta_plus)
    ydelta_minus = keyword_set(ydelta_minus)
    ymax         = keyword_set(ymax)
    ymin         = keyword_set(ymin)
    
    ;Centers?
    if keyword_set(centers) then begin
        indep = 1
        dep   = 1
    endif
    
    ;Deltas?
    if keyword_set(deltas) then begin
        xdelta_plus  = 1
        xdelta_minus = 1
        ydelta_plus  = 1
        ydelta_minus = 1
    endif
    
    ;Corners?
    if keyword_set(corners) then begin
        xmax = 1
        xmin = 1
        ymax = 1
        ymin = 1
    endif
            
    ;Pixel Centers.
    if dep then if n_elements(*self.dep) gt 0 then begin
        ptr_free, self.dep
        self.dep = ptr_new(/ALLOCATE_HEAP)
    endif
    if indep then if n_elements(*self.indep) gt 0 then begin
        ptr_free, self.indep
        self.indep = ptr_new(/ALLOCATE_HEAP)
    endif
    
    ;Pixel Corners
    if xmin then if n_elements(*self.xmin) gt 0 then begin
        ptr_free, self.xmin
        self.xmin = ptr_new(/ALLOCATE_HEAP)
    endif
    if xmax then if n_elements(*self.xmax) gt 0 then begin
        ptr_free, self.xmax
        self.xmax = ptr_new(/ALLOCATE_HEAP)
    endif
    if ymin then if n_elements(*self.ymin) gt 0 then begin
        ptr_free, self.ymin
        self.ymin = ptr_new(/ALLOCATE_HEAP)
    endif
    if ymax then if n_elements(*self.ymax) gt 0 then begin
        ptr_free, self.ymax
        self.ymax = ptr_new(/ALLOCATE_HEAP)
    endif
    
    ;Pixel Deltas
    if xdelta_minus then if n_elements(*self.xdelta_minus) gt 0 then begin
        ptr_free, self.xdelta_minus
        self.xdelta_minus = ptr_new(/ALLOCATE_HEAP)
    endif
    if xdelta_plus then if n_elements(*self.xdelta_plus) gt 0 then begin
        ptr_free, self.xdelta_plus
        self.xdelta_plus = ptr_new(/ALLOCATE_HEAP)
    endif
    if ydelta_minus then if n_elements(*self.ydelta_minus) gt 0 then begin
        ptr_free, self.ydelta_minus
        self.ydelta_minus = ptr_new(/ALLOCATE_HEAP)
    endif
    if ydelta_plus then if n_elements(*self.ydelta_plus) gt 0 then begin
        ptr_free, self.ydelta_plus
        self.ydelta_plus = ptr_new(/ALLOCATE_HEAP)
    endif
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
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    if self.hide then return
    
    ;Now display the image
    self -> doImage, NOERASE=noerase
    self -> SaveCoords
end


;+
;   The purpose of this method is to do the actual plotting.
;
; :Private:
;-
pro MrImage::doImage, $
NOERASE=noerase
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Plot TV-style
    if self.tv then begin
        case self.nparams of
            1: mraImage, *self.image, /TV, NOERASE=noerase
            2: mraImage, *self.image, *self.indep, /TV, NOERASE=noerase
            3: mraImage, *self.image, *self.indep, *self.dep, /TV, NOERASE=noerase
            else: message, 'Incorrect number of parameters for the TV command.'
        endcase
        return
    endif

    if n_elements(noerase) eq 0 then noerase = *self.noerase

;---------------------------------------------------------------------
; PAINT PIXEL-BY-PIXEL? //////////////////////////////////////////////
;---------------------------------------------------------------------
    if self.paint then begin
        ;Pick all pixels with at least corner inside the image.
        inds = where(*self.Xmax gt (*self.xrange)[0] and $
                     *self.Xmin lt (*self.xrange)[1] and $
                     *self.Ymax gt (*self.yrange)[0] and $
                     *self.Ymin lt (*self.yrange)[1], nInds)
        
        ;Get the 2D indices so that the image is not passed as a 1D array.
        dims = size(*self.image, /DIMENSIONS)
        inds = array_indices(dims, inds, /DIMENSIONS)
        icol = inds[0,*]
        irow = inds[1,*]

;---------------------------------------------------------------------
; DATA POSITION? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        data_pos = dblarr(4)
        ixrange  = getIndexRange(*self.indep, *self.xrange)
        iyrange  = getIndexRange(*self.dep,   *self.yrange)
        data_pos[[0,2]] = (*self.indep)[ixrange]
        data_pos[[1,3]] = (*self.dep)[iyrange]
        iData           = [ixrange[0], iyrange[0], ixrange[1], iyrange[1]]
    endelse

    ;Get layout properties.
    self.layout -> GetProperty, POSITION=position, CHARSIZE=charsize

;---------------------------------------------------------------------
;DATA POSITION? //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if self.paint eq 0 then begin
        MraImage, (*self.image)[iData[0]:iData[2], iData[1]:iData[3], self.iDisplay], $
                  (*self.indep)[iData[0]:iData[2]], (*self.dep)[iData[1]:iData[3]], $
                
                  ;IMAGE_PLOTS Keywords
                  AXES          = *self.axes, $
                  BOTTOM        = *self.bottom, $
                  CTINDEX       = *self.ctindex, $
                  DPOSITION     =       data_pos, $
                  NAN           = *self.nan, $
                  SCALE         = *self.scale, $
                  RANGE         = *self.range, $
                  MISSING_VALUE = *self.missing_value, $
                  MISSING_COLOR = *self.missing_color, $
                  PALETTE       = *self.palette, $
                  TOP           = *self.top, $
                
                  ;MrLayout Keywords
                  POSITION      =       position, $
                  CHARSIZE      =       charsize, $
                
                  ;Graphics Keywords
                  MAX_VALUE     =       max_value, $
                  MIN_VALUE     =       min_value, $
                
                  ;MrGraphicsKeywords
 ;                 AXISCOLOR     = *self.axiscolor, $
                  BACKGROUND    = *self.background, $
                  CHARTHICK     = *self.charthick, $
                  CLIP          = *self.clip, $
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
                  THICK         = *self.thick, $
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
  ;                YMINOR        = *self.yminor, $
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
                  YTITLE        = *self.ytitle;, $
                
 ;                ZCHARSIZE=*self.zcharsize, $
 ;                ZGRIDSTYLE=*self.zgridstyle, $
 ;                ZMARGIN=*self.zmargin, $
 ;                ZMINOR=*self.zminor, $
 ;                ZRANGE=*self.zrange, $
 ;                ZSTYLE=*self.zstyle, $
 ;                ZTHICK=*self.zthick, $
 ;                ZTICK_GET=*self.ztick_get, $
 ;                ZTICKFORMAT=*self.ztickformat, $
 ;                ZTICKINTERVAL=*self.ztickinterval, $
 ;                ZTICKLAYOUT=*self.zticklayout, $
 ;                ZTICKLEN=*self.zticklen, $
 ;                ZTICKNAME=*self.ztickname, $
 ;                ZTICKS=*self.zticks, $
 ;                ZTICKUNITS=*self.ztickunits, $
 ;                ZTICKV=*self.ztickv, $
 ;                ZTITLE=*self.ztitle, $
 ;                ZVALUE=*self.zvalue
 
;---------------------------------------------------------------------
;PAINT ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        MraImage, (*self.image)[icol, irow], $
                   (*self.Xmin)[icol, irow], $
                   (*self.Ymin)[icol, irow], $
                   (*self.Xmax)[icol, irow], $
                   (*self.Ymax)[icol, irow], $
                
                  ;IMAGE_PLOTS Keywords
                  AXES          = *self.axes, $
                  BOTTOM        = *self.bottom, $
                  CTINDEX       = *self.ctindex, $
                  DPOSITION     = *self.data_pos, $
                  NAN           = *self.nan, $
                  SCALE         = *self.scale, $
                  RANGE         = *self.range, $
                  MISSING_VALUE = *self.missing_value, $
                  MISSING_COLOR = *self.missing_color, $
                  PALETTE       = *self.palette, $
                  POLAR         =  self.polar, $
                  TOP           = *self.top, $
                
                  ;MrGraphicAtom Keywords
                  POSITION      =       position, $
                  CHARSIZE      =       charsize, $
                
                  ;Graphics Keywords
                  MAX_VALUE     = max_value, $
                  MIN_VALUE     = min_value, $
                  XLOG          = self.xlog, $
                  YLOG          = self.ylog, $
                
                  ;MrGraphicsKeywords
 ;                 AXISCOLOR     = *self.axiscolor, $
                  BACKGROUND    = *self.background, $
                  CHARTHICK     = *self.charthick, $
                  CLIP          = *self.clip, $
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
                  THICK         = *self.thick, $
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
  ;                YMINOR        = *self.yminor, $
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
                  YTITLE        = *self.ytitle
    endelse
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

;MrImage.pro Keywords
AXES = axes, $
BOTTOM = bottom, $
CENTER = center, $
CTINDEX = ctindex, $
DATA_POS = data_pos, $
MISSING_VALUE = missing_value, $
MISSING_COLOR = missing_color, $
NAN = nan, $
PAINT = paint, $
PALETTE = palette, $
POLAR = polar, $
RANGE = range, $
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
    if arg_present(position)    then position    =  self.layout -> GetPosition()
    if arg_present(xmin) then if n_elements(*self.Xmin) gt 0 then xmin = *self.Xmin
    if arg_present(xmax) then if n_elements(*self.Xmax) gt 0 then xmax = *self.Xmax
    if arg_present(ymin) then if n_elements(*self.Ymin) gt 0 then ymin = *self.Ymin
    if arg_present(ymax) then if n_elements(*self.Ymax) gt 0 then ymax = *self.Ymax
        
    ;Graphics Properties
    if arg_present(MAX_VALUE) and n_elements(*self.MAX_VALUE) ne 0 then max_value = *self.max_value
    if arg_present(MIN_VALUE) and n_elements(*self.MIN_VALUE) ne 0 then min_value = *self.min_value
    if arg_present(XLOG)     then xlog = self.xlog
    if arg_present(YLOG)     then ylog = self.ylog

    ;mraImage.pro Properties
    if arg_present(AXES)          && n_elements(*self.AXES)          gt 0 then axes          = *self.axes
    if arg_present(BOTTOM)        && n_elements(*self.BOTTOM)        gt 0 then bottom        = *self.bottom
    if arg_present(CTINDEX)       && n_elements(*self.CTINDEX)       gt 0 then ctindex       = *self.ctindex
    if arg_present(data_pos)      && n_elements(*self.data_pos)      gt 0 then data_pos      = *self.data_pos
    if arg_present(RANGE)         && n_elements(*self.RANGE)         gt 0 then range         = *self.range
    if arg_present(MISSING_VALUE) && n_elements(*self.MISSING_VALUE) gt 0 then missing_value = *self.missing_value
    if arg_present(MISSING_COLOR) && n_elements(*self.MISSING_COLOR) gt 0 then missing_color = *self.missing_color
    if arg_present(PALETTE)       && n_elements(*self.palette)       gt 0 then palette       = *self.palette
    if arg_present(NAN)           && n_elements(*self.NAN)           gt 0 then nan           = *self.nan
    if arg_present(SCALE)         && n_elements(*self.SCALE)         gt 0 then scale         = *self.scale
    if arg_present(TOP)           && n_elements(*self.TOP)           gt 0 then top           = *self.top
    
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
    *self.Xmin  = Xmin
    *self.Xmax  = Xmax
    *self.Ymin  = Ymin
    *self.Ymax  = Ymax
    
    self.window -> Draw
end


;+
;   The purpose of this method is to retrieve data
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

    ;Retrieve the data
    case n_params() of
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
            self -> ClearData, /DELTAS, /CORNERS
            
            ;Set the data
            *self.indep = x
            *self.image = theImage
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
            self -> ClearData, /DELTAS, /CORNERS
            
            ;Store the data
            *self.image = theImage
            *self.indep = x
            *self.dep = y
            
            ;Find the pixel locations
            if self.tv eq 0 then self -> SetPixelLocations, x, y
            self.nparams = 3B
        endcase
        
    ;---------------------------------------------------------------------
    ; Pixel Corners //////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        5: begin
            ;Clear data
            self -> ClearData, /DELTAS, /CENTERS
        
            ;Set the data
            *self.image = theImage
            self -> SetPixelLocations, x, y, x0, y0
            
            ;Paint each pixel
            self.paint = 1B
            self.nparams = 5B
        endcase
        
    ;---------------------------------------------------------------------
    ; Pixel Deltas ///////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        7: begin
            ;Clear data
            self -> ClearData, /DELTAS
            
            ;Store the data
            *self.image = theImage
            *self.indep = x
            *self.dep   = y
            *self.xdelta_minus = x0
            *self.xdelta_plus  = x1
            *self.ydelta_minus = y0
            *self.ydelta_plus  = y1
            self -> SetPixelLocations, x, y, x0, y0, x1, y1
            
            ;Paint the pixels
            self.paint = 1B
            self.nparams = 7B
        endcase
        else: message, 'Incorrect number of parameters.'
    endcase
            
    ;Get the new ranges
    *self.range = [min(theImage, /NAN, MAX=imMax), imMax]
    if n_elements(*self.xmin) eq 0 $
        then *self.xrange = [self.xlog, imDims[0]-1] $
        else *self.xrange = [min(*self.xmin), max(*self.xmax)]
    if n_elements(*self.ymin) eq 0 $
        then *self.yrange = [self.ylog, imDims[1]-1] $
        else *self.yrange = [min(*self.ymin), max(*self.ymax)]

    ;To make the image zoomable, we need to know the data location of each pixel.
    ;These are the pixel centers. If no pixel centers were given, then create them
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
BOTTOM = bottom, $
CTINDEX = ctindex, $
CENTER = center, $
DATA_POS = data_pos, $
MISSING_VALUE = missing_value, $
MISSING_COLOR = missing_color, $
NAN = nan, $
PALETTE = palette, $
POLAR = polar, $
RANGE = range, $
SCALE = scale, $
TOP = top, $

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

    ;MrImage Keywords
    if n_elements(iDisplay)    ne 0 then self.iDisplay = iDisplay
    if n_elements(TV)          ne 0 then self.tv = keyword_set(tv)

    ;mraImage.pro Properties
    if n_elements(AXES)          ne 0 then *self.axes          = keyword_set(axes)
    if n_elements(BOTTOM)        ne 0 then *self.bottom        = bottom
    if n_elements(CTINDEX)       ne 0 then *self.ctindex       = ctindex
    if n_elements(data_pos)      gt 0 then *self.data_pos      = data_pos
    if n_elements(MISSING_VALUE) ne 0 then *self.missing_value = missing_value
    if n_elements(MISSING_COLOR) ne 0 then *self.missing_color = missing_color
    if n_elements(NAN)           ne 0 then *self.nan           = keyword_set(nan)
    if n_elements(RANGE)         ne 0 then *self.range         = range
    if n_elements(SCALE)         ne 0 then *self.scale         = keyword_set(scale)
    if n_elements(TOP)           ne 0 then *self.top           = top
    
    ;CTIndex takes precedence over PALETTE, so it must be reset.
    if n_elements(PALETTE) gt 0 then begin
        *self.palette = palette
        void = temporary(*self.cgIndex)
    endif
    
    ;Log-scale the axes or pixel centers given?
    nxlog   = n_elements(xlog)
    nylog   = n_elements(ylog)
    nCenter = n_elements(center)
    nPolar  = n_elements(polar)
    if nxlog + nylog + nCenter + nPolar gt 0 then begin
        if nxlog   gt 0 then self.xlog   = keyword_set(xlog)
        if nylog   gt 0 then self.ylog   = keyword_set(ylog)
        if nCenter gt 0 then self.center = keyword_set(center)
        if nPolar  gt 0 then self.polar  = keyword_set(polar)
        
        ;If only X and Y were given, pixel locations have to be recalculated
        ;   - Cannot easily determine if the image needs to be painted or pasted
        ;   - PAINT will be set automatically
        ;   - (If more than 3 paramaters were given, PAINT=1 no matter what.)
        if self.nparams eq 3 && self.tv eq 0 $
            then self -> SetPixelLocations, *self.indep, *self.dep
    endif
    
    ;Superclass properties
    if n_elements(extra) gt 0 $
        then self -> MrGrDataAtom::SetProperty, _EXTRA=extra
    
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
    ptr_free, self.dep
    ptr_free, self.indep
    ptr_free, self.axes
    ptr_free, self.bottom
    ptr_free, self.ctindex
    ptr_free, self.data_pos
    ptr_free, self.missing_color
    ptr_free, self.missing_value
    ptr_free, self.nan
    ptr_free, self.palette
    ptr_free, self.range
    ptr_free, self.scale
    ptr_free, self.top
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
POLAR = polar, $
TV = tv, $
      
;IMAGE_PLOTS Keywords
AXES = axes, $
BOTTOM = bottom, $
CENTER = center, $
CTINDEX = ctindex, $
DATA_POS = data_pos, $
MISSING_VALUE = missing_value, $
MISSING_COLOR = missing_color, $
NAN = nan, $
PAINT = paint, $
PALETTE = palette, $
RANGE = range, $
SCALE = scale, $
TOP = top, $

;Graphics Keywords
MAX_VALUE = max_value, $
MIN_VALUE = min_value, $
XLOG = xlog, $
XRANGE = xrange, $
YLOG = ylog, $
YRANGE = yrange, $
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
                      NAME=name, OVERPLOT=overplot, POSITION=position, REFRESH=refresh, $
                      WINDOW_TITLE=window_title, _EXTRA=extra)
    if success eq 0 then message, 'Unable to initialize superclass MrGrDataAtom.'

;---------------------------------------------------------------------
; Defaults ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    imDims = size(theImage, /DIMENSIONS)
    nDims = size(theImage, /N_DIMENSIONS)
    if nDims ne 2 and nDims ne 3 then message, 'IMAGE must be a 2D or 3D array.'
    
    ;Defaults
    setDefaultValue, center,   0, /BOOLEAN
    setDefaultValue, gui,      1, /BOOLEAN
    setDefaultValue, iDisplay, 0
    setDefaultValue, paint,    0, /BOOLEAN
    setDefaultValue, polar,    0, /BOOLEAN
    setDefaultValue, tv,       0, /BOOLEAN
    setDefaultValue, xsize,  600
    setDefaultValue, xlog,     0, /BOOLEAN
    setDefaultValue, ylog,     0, /BOOLEAN
    setDefaultValue, ysize,  340
        
;---------------------------------------------------------------------
;Allocate Heap to Pointers ///////////////////////////////////////////
;---------------------------------------------------------------------
    self.image         = ptr_new(/ALLOCATE_HEAP)
    self.indep         = ptr_new(/ALLOCATE_HEAP)
    self.dep           = ptr_new(/ALLOCATE_HEAP)
    self.axes          = ptr_new(/ALLOCATE_HEAP)
    self.bottom        = ptr_new(/ALLOCATE_HEAP)
    self.ctindex       = ptr_new(/ALLOCATE_HEAP)
    self.data_pos      = ptr_new(/ALLOCATE_HEAP)
    self.max_value     = ptr_new(/ALLOCATE_HEAP)
    self.min_value     = ptr_new(/ALLOCATE_HEAP)
    self.missing_value = ptr_new(/ALLOCATE_HEAP)
    self.missing_color = ptr_new(/ALLOCATE_HEAP)
    self.nan           = ptr_new(/ALLOCATE_HEAP)
    self.palette       = ptr_new(/ALLOCATE_HEAP)
    self.range         = ptr_new(/ALLOCATE_HEAP)
    self.scale         = ptr_new(/ALLOCATE_HEAP)
    self.top           = ptr_new(/ALLOCATE_HEAP)
    self.xdelta_minus  = ptr_new(/ALLOCATE_HEAP)
    self.xdelta_plus   = ptr_new(/ALLOCATE_HEAP)
    self.ydelta_minus  = ptr_new(/ALLOCATE_HEAP)
    self.ydelta_plus   = ptr_new(/ALLOCATE_HEAP)
    self.Xmax          = ptr_new(/ALLOCATE_HEAP)
    self.Xmin          = ptr_new(/ALLOCATE_HEAP)
    self.Ymax          = ptr_new(/ALLOCATE_HEAP)
    self.Ymin          = ptr_new(/ALLOCATE_HEAP)

;---------------------------------------------------------------------
; Set Data ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    self.tv = keyword_set(tv)

    ;Must set TV first.
    ;   Setting the data will also set the ranges. Take care of user-supplied
    ;   ranges below.
    case n_params() of
        1: self -> SetData, theImage
        2: self -> SetData, theImage, x
        3: self -> SetData, theImage, x, y
        5: self -> SetData, theImage, x, y, x0, y0
        7: self -> SetData, theImage, x, y, x0, y0, x1, y1
        else: message, 'Incorrect number of elements.'
    endcase
        
;---------------------------------------------------------------------
;Check/Set Keywords //////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Set the object properties
    ;   RANGE must be set after the data is set.
    self -> SetProperty, AXES = axes, $
                         BOTTOM = bottom, $
                         CTINDEX = ctindex, $
                         CENTER = center, $
                         DATA_POS = data_pos, $
                         IDISPLAY = iDisplay, $
                         MAX_VALUE = max_value, $
                         MIN_VALUE = min_value, $
                         MISSING_VALUE = missing_value, $
                         MISSING_COLOR = missing_color, $
                         NAN = nan, $
                         PALETTE = palette, $
                         POLAR = polar, $
                         RANGE = range, $
                         SCALE = scale, $
                         TOP = top, $
                         TV = tv, $
                         XLOG = xlog, $
                         XRANGE = xrange, $
                         YLOG = ylog, $
                         YRANGE = yrange
    
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
    self.init_range  = *self.range

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
               indep: ptr_new(), $               ;independent variable
               dep:   ptr_new(), $               ;dependent variable
               image: ptr_new(), $               ;image to be displayed
               tv: 0B, $                         ;indicate that a TV position was given
               
               ;Type of data given
               nparams: 0B, $
              
               ;MrImage Properties
               idisplay:    0L, $                ;Index to display (>3D images)
               init_range:  dblarr(2), $         ;Initial image range
               init_xrange: dblarr(2), $         ;Initial x-range
               init_yrange: dblarr(2), $         ;Initial y-range
              
               ;mraImage Keywords
               axes: ptr_new(), $                ;Draw axes around the image?
               bottom: ptr_new(), $              ;If scaled, minimum scaled value
               center: 0B, $                     ;Center of pixel locations was given?
               ctindex: ptr_new(), $             ;Color index to load
               data_pos: ptr_new(), $            ;A data position for the image
               missing_value: ptr_new(), $       ;Value to be treated as missing
               missing_color: ptr_new(), $       ;Color of missing value
               nan: ptr_new(), $                 ;Search for NaN's when scaling?
               paint: 0B, $                      ;Paint the image pixel-by-pixel?
               palette: ptr_new(), $             ;Color table to be loaded
               polar: 0B, $                      ;Create a polar image?
               range: ptr_new(), $               ;Range at which the color table saturates
               scale: ptr_new(), $               ;Byte-scale the image
               top: ptr_new(), $                 ;If scaled, maximum scaled value
               xdelta_minus: ptr_new(), $
               xdelta_plus:  ptr_new(), $
               Xmin: ptr_new(), $                ;X-location of bottom-left corner of pixels
               Xmax: ptr_new(), $                ;X-location of upper-right corner of pixels
               ydelta_minus: ptr_new(), $
               ydelta_plus:  ptr_new(), $
               Ymin: ptr_new(), $                ;Y-location of bottom-left corner of pixels
               Ymax: ptr_new() $                 ;Y-location of upper-right corner of pixels
             }
end