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
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2012
;
; :History:
;	Modification History::
;       04/22/2013  -   Written by Matthew Argall
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
;-
;*****************************************************************************************
;+
;   The purpose of this method is to draw the plot in the draw window. The plot is
;   first buffered into the pixmap for smoother opteration (by allowing motion events
;   to copy from the pixmap instead of redrawing the plot, the image does not flicker).
;-
pro MrImage::Draw, $
NOERASE=noerase
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Now draw the plot
    if self.hide eq 0 then begin
        self -> doImage, NOERASE=noerase
        self -> SaveCoords
    endif
end


;+
;   The purpose of this method is to do the actual plotting.
;
; :History:
;   Modification History::
;       05/04/2013  -   Do not allow zooming outside of the data range. - MRA
;-
pro MrImage::doImage, $
NOERASE=noerase
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Plot TV-style
    if self.tv then begin
        image_plots, *self.image, *self.x_pos, *self.y_pos
        return
    endif

    if n_elements(noerase) eq 0 then noerase = *self.noerase
    
    

;---------------------------------------------------------------------
;PAINT PIXEL-BY-PIXEL? ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if self.paint then begin
        ;Find all finite values
        ixFinite = where(finite(*self.Xmin) eq 1 and finite(*self.Xmax) eq 1, nxFinite)
        iyFinite = where(finite(*self.Ymin) eq 1 and finite(*self.Ymax) eq 1, nyFinite)

        ;Find all finite values that lie within [XY]RANGE
        ix = where((*self.Xmin)[ixFinite] ge (*self.xrange)[0] and (*self.Xmax)[ixFinite] lt (*self.xrange)[1], nx)
        iy = where((*self.Ymin)[iyFinite] ge (*self.yrange)[0] and (*self.Ymax)[iyFinite] lt (*self.yrange)[1], ny)

        ;Find all values that do not match the above requirements
        if nx eq 0 then begin
            nxRemove = n_elements(*self.Xmin)
            ixRemove = lindgen(nxRemove)
        endif else ixRemove = where(histogram(ixFinite[ix], MIN=0, MAX=product(size(*self.Xmin, /DIMENSIONS))) eq 0, nxRemove)
            
        if ny eq 0 then begin
            nyRemove = n_elements(*self.Ymin)
            iyRemove = lindgen(nyRemove)
        endif else iyRemove = where(histogram(iyFinite[iy], MIN=0, MAX=product(size(*self.Ymin, /DIMENSIONS))) eq 0, nyRemove)
        
        ;Copy the pixel locations
        Xmin = *self.Xmin
        Xmax = *self.Xmax
        Ymin = *self.Ymin
        Ymax = *self.Ymax
        
        ;Set out-of-bounds values to !NaN
        if nxRemove gt 0 then begin
            Xmin[ixRemove] = !values.f_nan
            Xmax[ixRemove] = !values.f_nan
        endif
        
        ;Set out-of-bounds values to !NaN
        if nyRemove gt 0 then begin
            Ymin[iyRemove] = !values.f_nan
            Ymax[iyRemove] = !values.f_nan
        endif
        
;---------------------------------------------------------------------
;DATA POSITION? //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        nDep = n_elements(*self.dep)
        nIndep = n_elements(*self.indep)
        data_pos = fltarr(4) + !values.f_nan
        idata = intarr(4)
        
        ;If XRANGE and YRANGE are bigger than the actual range of DEP and INDEP, then use DATAPOS
        if (*self.xrange)[0] lt (*self.indep)[0] then begin
            data_pos[0] = (*self.indep)[0]
            idata[0] = 0
        endif
    
        if (*self.xrange)[1] gt (*self.indep)[nIndep-1] then begin
            data_pos[2] = (*self.indep)[nIndep-1]
            idata[2] = nIndep-1
        endif
    
        if (*self.yrange)[0] lt (*self.dep)[0] then begin
            data_pos[1] = (*self.dep)[0]
            idata[1] = 0
        endif
    
         if (*self.yrange)[1] gt (*self.dep)[nDep-1] then begin
            data_pos[3] = (*self.dep)[nDep-1]
            idata[3] = nDep-1
         endif

    ;---------------------------------------------------------------------
    ;RANGE Must Match Data Values ////////////////////////////////////////
    ;---------------------------------------------------------------------
        ixrange = getIndexRange(*self.indep, *self.xrange)
        iyrange = getIndexRange(*self.dep,   *self.yrange)

        ;If XRANGE is not being used for a data position, then make sure it matches
        ;exact values of INDEP
        if finite(data_pos[0]) eq 0 then begin
            data_pos[0] = (*self.indep)[ixrange[0]]
            iData[0] = ixrange[0]
        endif
    
        if finite(data_pos[2]) eq 0 then begin
            data_pos[2] = (*self.indep)[ixrange[1]]
            iData[2] = ixrange[1]
        endif
    
        if finite(data_pos[1]) eq 0 then begin
            data_pos[1] = (*self.dep)[iyrange[0]]
            iData[1] = iyrange[0]
        endif
    
        if finite(data_pos[3]) eq 0 then begin
            data_pos[3] = (*self.dep)[iyrange[1]]
            iData[3] = iyrange[1]
        endif

        *self.data_pos = data_pos
    endelse

;---------------------------------------------------------------------
;DATA POSITION? //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if self.paint eq 0 then begin
        maImage, (*self.image)[iData[0]:iData[2], iData[1]:iData[3], self.iDisplay], $
                 (*self.indep)[iData[0]:iData[2]], (*self.dep)[iData[1]:iData[3]], $
                
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
                 TOP           = *self.top, $
                
                 ;MrGraphicAtom Keywords
                 POSITION      =  self.position, $
                
                 ;Graphics Keywords
                 MAX_VALUE     = max_value, $
                 MIN_VALUE     = min_value, $
                
                 ;weGraphicsKeywords
;                 AXISCOLOR     = *self.axiscolor, $
                 BACKGROUND    = *self.background, $
                 CHARSIZE      =  self.charsize, $
                 CHARTHICK     = *self.charthick, $
                 CLIP          = *self.clip, $
                 COLOR         = *self.color, $
                 DATA          = *self.data, $
                 DEVICE        = *self.device, $
                 NORMAL        = *self.normal, $
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
    
        maImage, *self.image, Xmin, Ymin, Xmax, Ymax, $
                
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
                 TOP           = *self.top, $
                
                 ;MrGraphicAtom Keywords
                 POSITION      =  self.position, $
                
                 ;Graphics Keywords
                 MAX_VALUE     = max_value, $
                 MIN_VALUE     = min_value, $
                 XLOG          = self.xlog, $
                 YLOG          = self.ylog, $
                
                 ;weGraphicsKeywords
;                 AXISCOLOR     = *self.axiscolor, $
                 BACKGROUND    = *self.background, $
                 CHARSIZE      =  self.charsize, $
                 CHARTHICK     = *self.charthick, $
                 CLIP          = *self.clip, $
                 COLOR         = *self.color, $
                 DATA          = *self.data, $
                 DEVICE        = *self.device, $
                 NORMAL        = *self.normal, $
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
;       PALETTE:            out, optional, type=bytarr(3,256)
;                           An [r,g,b] Color table to be loaded before the image is displayed.
;                               This takes precedence over `CTINDEX`.
;       P_SYSVAR:           out, optional, type=structure
;                           The !P system variable state associated with this plot.
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
;Data keywords
X_POS = x_pos, $
Y_POS = y_pos, $

;MrImage Keywords
IDISPLAY = iDisplay, $
INIT_XRANGE = init_xrange, $
INIT_YRANGE = init_yrange, $
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
RANGE = range, $
SCALE = scale, $
TOP = top, $

;Graphics Keywords
MAX_VALUE = max_value, $
MIN_VALUE = min_value, $
XLOG = xlog, $
YLOG = ylog, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Data Properties
    if arg_present(X_POS) and n_elements(*self.X_POS) ne 0 then x_pos = *self.x_pos
    if arg_present(Y_POS) and n_elements(*self.Y_POS) ne 0 then y_pos = *self.y_pos

    ;MrImage Properties
    if arg_present(iDisplay)    then iDisplay    =  self.iDisplay
    if arg_present(INIT_XRANGE) then init_xrange =  self.init_xrange
    if arg_present(INIT_YRANGE) then init_yrange =  self.init_yrange
    if arg_present(xmin) then if n_elements(*self.Xmin) gt 0 then xmin = *self.Xmin
    if arg_present(xmax) then if n_elements(*self.Xmax) gt 0 then xmax = *self.Xmax
    if arg_present(ymin) then if n_elements(*self.Ymin) gt 0 then ymin = *self.Ymin
    if arg_present(ymax) then if n_elements(*self.Ymax) gt 0 then ymax = *self.Ymax
        
    ;Graphics Properties
    if arg_present(MAX_VALUE) and n_elements(*self.MAX_VALUE) ne 0 then max_value = *self.max_value
    if arg_present(MIN_VALUE) and n_elements(*self.MIN_VALUE) ne 0 then min_value = *self.min_value
    if arg_present(XLOG)     then xlog = self.xlog
    if arg_present(YLOG)     then ylog = self.ylog

    ;IMAGE_PLOTS.PRO Properties
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
    if arg_present(paint)  then paint  =  self.paint
    
    ;weGraphicsKeywords Properties
    if n_elements(EXTRA) ne 0 then begin
        self -> MrLayout::GetProperty, _EXTRA=extra
        self -> MrGrAtom::GetProperty, _EXTRA=extra
        self -> weGraphicsKeywords::GetProperty, _EXTRA=extra
    endif
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
pro MrImage::GetData, image, x, y, x1, y1
    compile_opt idl2
    
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
            x1 = *self.Xmax
            y1 = *self.Ymax
        endcase
        else: message, 'Incorrect number of parameters.'
    endcase
end


;+
;   The purpose of this method is to calculate pixel locations.
;-
pro MrImage::SetPixelLocations, x, y, x0, y0, x1, y1, $
CENTER=center, $
DRAW=draw, $
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
    if n_elements(xlog) gt 0 $
        then self.xlog = keyword_set(self.xlog) $
        else xlog = self.xlog
        
    if n_elements(ylog) gt 0 $
        then self.ylog = keyword_set(self.ylog) $
        else ylog = self.ylog
        
    if n_elements(center) gt 0 $
        then self.center = keyword_set(self.center) $
        else center = self.center
    
;---------------------------------------------------------------------
;Pixel Locations /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    dims = size(*self.image, /DIMENSIONS)
    dims = dims[0:1]
    
    case n_params() of
        2: MrPixelPoints,  dims, x, y, Xmin, Ymin, Xmax, Ymax, CENTER=self.center, /DIMENSIONS, XLOG=self.xlog, YLOG=self.ylog
        4: MrPixelCorners, dims, x, y, x0, y0, Xmin, Ymin, Xmax, Ymax, /DIMENSIONS
        6: MrPixelDeltas,  dims, x, y, x0, y0, x1, y1, Xmin, Ymin, Xmax, Ymax, /DIMENSIONS
        else: message, 'Incorrect number of parameters.'
    endcase
    
    *self.Xmin = Xmin
    *self.Xmax = Xmax
    *self.Ymin = Ymin
    *self.Ymax = Ymax
    
    if keyword_set(draw) then self -> Draw
end


;+
;   The purpose of this method is to retrieve data
;
; :Calling Sequence:
;       myGraphic -> GetData, image
;       myGraphic -> GetData, image, x, y
;       myGraphic -> GetData, image, x, y, x1, y1
;
; :Params:
;       IMAGE:          in, required, type=numeric array
;                       Image data will be returned.
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
;-
pro MrImage::SetData, image, x, y, x1, y1
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
        1: *self.image = image
        3: begin
            *self.image = image
            *self.indep = x
            *self.dep = y
        endcase
        4: begin
            *self.Xmin = x
            *self.Ymin = y
            *self.Xmax = y2
            *self.Ymax = y1
        endcase
        else: message, 'Incorrect number of parameters.'
    endcase
    
    self.window.draw
end


;+
;   The purpose of this method is to set the layout of a plot while maintaining the
;   automatically updating grid.
;
; :Params:
;       LAYOUT:             in, required, type=intarr(3)/intarr(4)
;                           A vector of the form [nCols, nRos, index] or
;                               [nCols, nRows, col, row], indicating the number of columns
;                               and rows in the overall layout (nCols, nRows), the index
;                               where the plot is to be placed ("index", starting with 1
;                               and increasing left->right then top->bottom). Alternatively,
;                               "col" and "row" are the column and row in which the plot
;                               is to be placed. Ignored if `POSITION` is present.
;
; :Keywords:
;       POSITION:           in, optional, type=fltarr(4)
;                           A vector of the form [x0, y0, x1, y1] specifying the lower-left
;                               and upper-right corners of the plot, in normal coordinates.
;                               If this keyword is given, `LAYOUT` is ignored. This keyword
;                               should not be used. Instead use the SetProperty method
;                               (or dot-referencing in IDL 8.0+). This is only meant
;                               for use by MrPlotManager__Define for synchronizing layout
;                               properties with the window.
;       UPDATE_LAYOUT:      in, optional, type=boolean, default=1
;                           Indicate that the layout is to be updated. All graphics within
;                               the graphics window will be adjusted. This keyword is
;                               used by MrPlotManager__Define when applying the layout
;                               grid to each plot. It is not meant to be used elsewhere.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrLayout::SetProperty is also accepted
;                               for keyword inheritance.
;-
pro MrImage::SetLayout, layout, $
POSITION=position, $
UPDATE_LAYOUT=update_layout, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        if n_elements(init_refresh) gt 0 $
            then self.window -> Refresh, DISABLE=~init_refresh
        return
    endif
    
    ;Default to updating the layout
    SetDefaultValue, update_layout, 1, /BOOLEAN
    
    ;Turn refresh off.
    self.window -> GetProperty, REFRESH=init_refresh
    self.window -> Refresh, /DISABLE
    
    ;If we are updating the layout, let the window take care of things.
    if update_layout then begin
        if n_elements(position) gt 0 $
            then self.window -> SetPosition, self.layout[2], position $
            else self.window -> SetPosition, self.layout[2], layout
        
        ;Adjust other aspects of the layout.
        if n_elements(extra) gt 0 then self.window -> SetProperty, _EXTRA=extra
    
    ;If we are not updating the layout...
    endif else begin
        self -> MrLayout::SetProperty, LAYOUT=layout, POSITION=position, UPDATE_LAYOUT=0, $
                                      _STRICT_EXTRA=extra
    endelse
    
    ;Reset the refresh state.
    self.window -> Refresh, DISABLE=~init_refresh
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
;       IMAGE:              in, optional, type=any
;                           The image to be displayed.
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
;       PALETTE:            in, optional, type=bytarr(3,256)
;                           Color table to be loaded before the image is displayed.
;       P_SYSVAR:           in, optional, type=structure
;                           The !P system variable state associated with this plot.
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
;       X_POS:              in, optional, type=int
;                           If the `TV` keyword is in use, then this specifies the x-
;                               position of the image as specified by the IDL's TV command.
;       XLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the x-axis
;       Y_POS:              in, optional, type=int
;                           If the `TV` keyword is in use, then this specifies the y-
;                               position of the image as specified by the IDL's TV command.
;       YLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the y-axis
;       _REF_EXTRA:         in, optional, type=any
;                           Keyword accepted by the superclasses are also accepted for
;                               keyword inheritance.
;-
pro MrImage::SetProperty, $
;Data keywords
X_POS = x_pos, $
Y_POS = y_pos, $

;MrImage Keywords
CHARSIZE = charsize, $
HIDE = hide, $
IDISPLAY = iDisplay, $
NAME = name, $
TV = tv, $
      
;IMAGE_PLOTS Keywords
AXES = axes, $
BOTTOM = bottom, $
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
POSITION = position, $
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

    ;Data
    if n_elements(x_pos) ne 0 then *self.x_pos = x_pos
    if n_elements(y_pos) ne 0 then *self.y_pos = y_pos

    ;MrImage Keywords
    if n_elements(hide)        gt 0 then self.hide = keyword_set(hide)
    if n_elements(iDisplay)    ne 0 then self.iDisplay = iDisplay
    if n_elements(name)        gt 0 then self.name = name
    if n_elements(TV)          ne 0 then self.tv = keyword_set(tv)

    ;IMAGE_PLOTS.PRO Properties
    if n_elements(AXES)          ne 0 then *self.axes = keyword_set(axes)
    if n_elements(BOTTOM)        ne 0 then *self.bottom = bottom
    if n_elements(CTINDEX)       ne 0 then *self.ctindex = ctindex
    if n_elements(data_pos)      gt 0 then *self.data_pos = data_pos
    if n_elements(MISSING_VALUE) ne 0 then *self.missing_value = missing_value
    if n_elements(MISSING_COLOR) ne 0 then *self.missing_color = missing_color
    if n_elements(NAN)           ne 0 then *self.nan = keyword_set(nan)
    if n_elements(PALETTE)       ne 0 then *self.palette = palette
    if n_elements(RANGE)         ne 0 then *self.range = range
    if n_elements(SCALE)         ne 0 then *self.scale = keyword_set(scale)
    if n_elements(TOP)           ne 0 then *self.top = top
    
    ;Graphics Properties
    if n_elements(MAX_VALUE) ne 0 then *self.max_value = max_value
    if n_elements(MIN_VALUE) ne 0 then *self.min_value = min_value
    if n_elements(XLOG)      ne 0 then  self.xlog = keyword_set(xlog)
    if n_elements(YLOG)      ne 0 then  self.ylog = keyword_set(ylog)
    
    if n_elements(position)  gt 0 then  self -> SetLayout, POSITION=position
    if n_elements(charsize)  gt 0 then  self -> SetLayout, CHARSIZE=charsize, UPDATE_LAYOUT=0
    
    ;Check PAINT after XLOG and YLOG have been set
    if n_elements(paint) gt 0 then self.paint = keyword_set(paint)
    
    ;Superclass properties
    nExtra = n_elements(extra)
    if nExtra gt 0 then begin
        ;MrGrAtom -- Pick out the keywords here to use _STRICT_EXTRA instead of _EXTRA
        atom_kwds = ['HIDE', 'NAME']
        void = IsMember(atom_kwds, extra, iAtom, N_MATCHES=nAtom, NONMEMBER_INDS=iExtra, N_NONMEMBER=nExtra)
        if nAtom gt 0 then self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra[iAtom]
    
        ;weGraphicsKeywords Properties
        if nExtra gt 0 then self -> weGraphicsKeywords::SetProperty, _STRICT_EXTRA=extra[iExtra]
    endif
        
    ;Refresh the window
    self.window -> Draw
end


;+
;   The purpose of this method is to determine if an image is to be painted or not.
;-
function MrImage::_TF_Paint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    if *self.xlog + *self.ylog gt 0 || *self.paint eq 1 || $
       n_elements(*self.Xmin) gt 0  || n_elements(*self.Xmax) gt 0 || $
       n_elements(*self.Ymin) gt 0  || n_elements(*self.Ymax) gt 0 $
        then return, 1 $
        else return, 0
end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrImage::cleanup
    compile_opt idl2
    
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
    ptr_free, self.x_pos
    ptr_free, self.y_pos
    ptr_free, self.axes
    ptr_free, self.bottom
    ptr_free, self.ctindex
    ptr_free, self.data_pos
    ptr_free, self.max_value
    ptr_free, self.min_value
    ptr_free, self.missing_color
    ptr_free, self.missing_value
    ptr_free, self.nan
    ptr_free, self.palette
    ptr_free, self.range
    ptr_free, self.scale
    ptr_free, self.top
    ptr_free, self.Xmin
    ptr_free, self.Xmax
    ptr_free, self.Ymin
    ptr_free, self.Ymax
    
    ;Superclasses
    self -> weGraphicsKeywords::CleanUp
    self -> MrGrAtom::CleanUp
    self -> MrLayout::CleanUp
end


;+
;   The purpose of this method is to initialize the MrImages class.
;
; :Params:
;       IMAGE:          in, required, type=NxM numeric array
;                       Image to be displayed
;       X:              in, optional, type=scalar, N-elements numeric
;                       If a scalar, then positioning is that of IDL's TV function. If a
;                           vector the same size as the first dimension of `IMAGE`, then
;                           the data coordinates of each pixel. See also, `PAINT`.
;       Y:              in, optional, type=scalar, N-elements numeric
;                       If a scalar, then positioning is that of IDL's TV function. If a
;                           vector the same size as the second dimension of `IMAGE`, then
;                           the data coordinates of each pixel. See also, `PAINT`.
;       X0:             in, optional, type=1D vector/2D array
;                       The x-coordinate, in data coordinates, of the upper-right corner
;                           if each pixel in `IMAGE`. If supplied, then `X` markes the
;                           x-coordinate of the lower-left corner of each pixel in `IMAGE`.
;                           Must be used with `Y0`. If given, `PAINT` will be set to 1.
;       Y0:             in, optional, type=1D vector/2D array
;                       The y-coordinate, in data coordinates, of the upper-right corner
;                           if each pixel in `IMAGE`. If supplied, then `Y` markes the
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
;       DRAW:               in, optional, type=boolean, default=1
;                           If set, the data will be drawn to the plot. DRAW=1 always if
;                               `GUI`=1.
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
;       PALETTE:            in, optional, type=bytarr(3,256)
;                           Color table to be loaded before the image is displayed.
;       POSITION:           in, optional, type=fltarr(4)
;                           A vector of the form [x0, y0, x1, y1], where [x0,y0] and [x1,y1]
;                               specify the position of the lower-left and upper-right
;                               corner of the plotting region, respectively.
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
;
; :Uses:
;   Uses the following external programs::
;       setDefaultValue.pro (Coyote Graphics)
;       error_message.pro (Coyote Graphics)
;       weGraphicsKeywords__define.pro
;       MrGetWindow.pro
;       linspace.pro
;       logspace.pro
;
; :History:
;   Modification History::
;       05/04/2013  -   DEP, INDEP, XRANGE, and YRANGE are now defined and consistent.
;                           This is necessary for zooming purposes. For images, the zoom
;                           is not over data coordinates, but over pixels, which uses a
;                           combination of device coordinates and index values within
;                           IMAGE. The index values that pick a subregion of the image are
;                           then used to select the proper range within DEP and INDEP, 
;                           which in turn set XRANGE and YRANGE.
;       05/04/2013  -   ::Draw is now only called if GUI=0. ::Notify_Realize will call
;                           call ::Draw when GUI=1
;       05/04/2013  -   Added DRAW, WINID and PIXID keywords. - MRA
;       05/04/2013  -   Added DRAW, WINID and PIXID keywords. - MRA
;       05/06/2013  -   Initialize MrDrawWindow properties at the beginning without
;                           building or realizing the GUI. This allows the defaults to
;                           be in effect if the image is displayed in an existing
;                           MrDrawWindow (i.e. when WINID and PIXID are both given). - MRA
;       05/09/2013  -   Use N_Elements instead of N_Params so that undefined x and y
;                           can be provided. - MRA
;-
function MrImage::init, image, x, y, x0, y0, x1, y1, $
;MrImage Keywords
CURRENT = current, $
IDISPLAY = idisplay, $
KEEP_ASPECT = keep_aspect, $
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
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Check the image
    imDims = size(image, /DIMENSIONS)
    nDims = size(image, /N_DIMENSIONS)
    if nDims ne 2 and nDims ne 3 then message, 'IMAGE must be a 2D or 3D array.'
    
    ;Defaults
    setDefaultValue, gui, 1, /BOOLEAN
    setDefaultValue, tv, 0, /BOOLEAN
    setDefaultValue, iDisplay, 0
    setDefaultValue, xsize, 600
    setDefaultValue, xlog, 0, /BOOLEAN
    setDefaultValue, ylog, 0, /BOOLEAN
    setDefaultValue, center, 0, /BOOLEAN
    setDefaultValue, ysize, 340
    setDefaultValue, paint, 0, /BOOLEAN
    if xlog + ylog gt 0 || n_params() gt 3 then paint = 1

;---------------------------------------------------------------------
;Superclass Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
    ;
    ;If values appear in the call to the superclass's INIT method,
    ;they will be over-ridden by like value if it appears in the
    ;EXTRA structure
    ;

    ;weGraphicsKeywords
    if self -> weGraphicsKeywords::INIT(AXISCOLOR='black', _EXTRA=extra) eq 0 then $
        message, 'Unable to initialize MrGraphicsKeywords.'
        
    ;MrLayout
    if self -> MrLayout::INIT(_EXTRA=extra) eq 0 then $
        message, 'Unable to initialize MrLayout.'
        
;---------------------------------------------------------------------
;Allocate Heap to Pointers ///////////////////////////////////////////
;---------------------------------------------------------------------
    self.image         = ptr_new(/ALLOCATE_HEAP)
    self.indep         = ptr_new(/ALLOCATE_HEAP)
    self.dep           = ptr_new(/ALLOCATE_HEAP)
    self.x_pos         = ptr_new(/ALLOCATE_HEAP)
    self.y_pos         = ptr_new(/ALLOCATE_HEAP)
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
    self.Xmax          = ptr_new(/ALLOCATE_HEAP)
    self.Xmin          = ptr_new(/ALLOCATE_HEAP)
    self.Ymax          = ptr_new(/ALLOCATE_HEAP)
    self.Ymin          = ptr_new(/ALLOCATE_HEAP)

    ;If REFRESH=1 three things happen: If the call to MrGrAtom is
    ;   1. before here, none of the pointers are valid and calls to SetProperty by MrGrAtom
    ;      cause errors.
    ;   2. here, then when MrGrAtom calls the SetProperty
    ;      method, none of the data will be loaded into the object.
    ;   3. after the call to SetProperty so that all of the data is loaded, the initial
    ;      call to SetProperty will not have a valid self.window property
    ;
    ;To fix problem 1 and 3, put the call to MrGrAtom here. To fix problem 2,
    ;turn Refresh off.
    if keyword_set(current) then begin
        theWin = GetMrWindows(/CURRENT)
        theWin -> GetProperty, REFRESH=init_refresh
        theWin -> Refresh, /DISABLE
    endif

    ;Graphic Atom
    if self -> MrGrAtom::INIT(CURRENT=current, _EXTRA=extra) eq 0 then $
        message, 'Unable to initialize MrGrAtom.'

;---------------------------------------------------------------------
;Input Parameters ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Assign dependent and independent variables.
    nx = n_elements(x)
    ny = n_elements(y)
    if ny ne 0 then begin
        indep = x
        dep = y
    endif else if nx ne 0 then indep = x

;---------------------------------------------------------------------
;TV Positioning? /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    nDep = n_elements(dep)
    nIndep = n_elements(indep)
    self.x_pos = ptr_new(/ALLOCATE_HEAP)
    self.y_pos = ptr_new(/ALLOCATE_HEAP)

    ;Is TV positioning to be used? -- Leave DEP and INDEP undefined afterwards.
    if keyword_set(tv) then begin
        case nDep of 
            1: *self.x_pos = temporary(dep)
            else: message, 'X must be scalar if TV is set.'
        endcase
            
        case nIndep of
            0: ;Do nothing
            1: *self.y_pos = temporary(indep)
            else: message, 'Y must be a scalar or undefined if TV is set.'
        endcase
    endif

;---------------------------------------------------------------------
;Set Dependent Variable and XRANGE ///////////////////////////////////
;---------------------------------------------------------------------
    ;
    ; The goal is to create an image class that is zoomable. As such, [XY]RANGE must
    ; be defined so that data coordinate system is establishable (even if the coordinates
    ; simply span the image size). Independent and Dependent variables must be defined
    ; so that a map exists between the data coordinates and the image's pixel/index
    ; locations.
    ;

    ;--------------------
    ;DEPENDENT VARIABLE |
    ;--------------------
    if n_elements(indep) eq 0 then begin
        ;Make sure XRANGE is defined.
        if n_elements(xrange) eq 0 then $
            if keyword_set(xlog) then xrange = [1, imdims[0]-1] $
                                 else xrange = [0, imDims[0]-1]
    
        ;Create DEP such that it is the size of IMAGE[*,i] and spans XRANGE
        if keyword_set(xlog) $
            then setDefaultValue, indep, logspace(alog10(xrange[0]), alog10(xrange[1]), imDims[0]) $
            else setDefaultValue, indep, linspace(xrange[0], xrange[1], imDims[0])

    ;Otherwise, make the xrange span all of DEP
    endif else if n_elements(xrange) eq 0 then xrange = [indep[0], indep[imDims[0]-1]]

    ;----------------------
    ;INDEPENDENT VARIABLE |
    ;----------------------
    if n_elements(dep) eq 0 then begin
        ;Make sure YRANGE is defined.
        if n_elements(yrange) eq 0 then $
            if keyword_set(ylog) then yrange = [1, imDims[1]-1] $
                                 else yrange = [0, imDims[1]-1]
                                 
        ;Create INDEP such that it is the size of IMAGE[i,*] and spans YRANGE
        if keyword_set(ylog) $
            then setDefaultValue, dep, logspace(alog10(yrange[0]), alog10(yrange[1]), imDims[1]) $
            else setDefaultValue, dep, linspace(yrange[0], yrange[1], imDims[1])
            
    ;Otherwise, make the yrange span all of INDEP
    endif else if n_elements(yrange) eq 0 then yrange = [dep[0], dep[imDims[1]-1]]
        
;---------------------------------------------------------------------
;Check/Set Keywords //////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Image range
    if n_elements(range) eq 0 $
        then imRange = [min(image, NAN=nan, max=imMax), imMax] $
        else imRange = range

    ;Set the data
    self -> SetData, image, indep, dep

    ;Set the object properties
    self -> SetProperty, AXES = axes, $
                         BOTTOM = bottom, $
                         CTINDEX = ctindex, $
                         DATA_POS = data_pos, $
                         IDISPLAY = iDisplay, $
                         MAX_VALUE = max_value, $
                         MIN_VALUE = min_value, $
                         MISSING_VALUE = missing_value, $
                         MISSING_COLOR = missing_color, $
                         NAN = nan, $
                         PAINT = paint, $
                         PALETTE = palette, $
                         RANGE = imRange, $
                         SCALE = scale, $
                         TOP = top, $
                         TV = tv, $
                         XLOG = xlog, $
                         XRANGE = xrange, $
                         YLOG = ylog, $
                         YRANGE = yrange
        
;---------------------------------------------------------------------
;Pixel Locations /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    case n_params() of
        1: ;Do nothing
        3: if xlog + ylog + center gt 0 $
            then self -> SetPixelLocations, indep, dep, CENTER=center, XLOG=xlog, YLOG=ylog
        5: self -> SetPixelLocations, indep, dep, x0, y0
        7: self -> SetPixelLocations, indep, dep, x0, y0, x1, y1
    endcase
    
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
    self.init_xrange = xrange
    self.init_yrange = yrange
    self.init_range = imRange
    
    ;Refresh the graphics?
    if keyword_set(current) $
        then theWin -> Refresh, DISABLE=~init_refresh $
        else self.window -> Draw

    return, 1
end


;+
;   Object class definition
;-
pro MrImage__define
    compile_opt idl2
    
    define = {MrImage, $
              inherits MrGrAtom, $
              inherits MrLayout, $
              inherits weGraphicsKeywords, $
              
              ;Data
              x_pos: ptr_new(), $               ;the x-position (see TV)
              y_pos: ptr_new(), $               ;the y-position (see TV)
              indep: ptr_new(), $               ;independent variable
              dep:   ptr_new(), $               ;dependent variable
              image: ptr_new(), $               ;image to be displayed
              tv: 0B, $                         ;indicate that a TV position was given
              
              ;MrImage Properties
              idisplay: 0L, $                   ;Index to display (>3D images)
              init_range: dblarr(2), $          ;Initial image range
              init_xrange: dblarr(2), $         ;Initial x-range
              init_yrange: dblarr(2), $         ;Initial y-range
              
              ;MrImage.pro Keywords
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
              range: ptr_new(), $               ;Range at which the color table saturates
              scale: ptr_new(), $               ;Byte-scale the image
              top: ptr_new(), $                 ;If scaled, maximum scaled value
              Xmin: ptr_new(), $                ;X-location of bottom-left corner of pixels
              Xmax: ptr_new(), $                ;X-location of upper-right corner of pixels
              Ymin: ptr_new(), $                ;Y-location of bottom-left corner of pixels
              Ymax: ptr_new(), $                ;Y-location of upper-right corner of pixels
              
              ;Plotting Keywords
              max_value: ptr_new(), $           ;maximum value displayed in plot
              min_value: ptr_new(), $           ;minimum value displayed in plot
              xlog: 0B, $                       ;log-scale the x-axis?
              ylog: 0B}                         ;log-scale the y-axis?
end