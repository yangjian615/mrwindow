; docformat = 'rst'
;
; NAME:
;       MrImageObject__Define
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
;       06/13/2013  -   Renamed from MrImagePlot__define.pro to MrImageObject__define.pro.
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
;-
;*****************************************************************************************
;+
;   The purpose of this method is to draw the plot in the draw window. The plot is
;   first buffered into the pixmap for smoother opteration (by allowing motion events
;   to copy from the pixmap instead of redrawing the plot, the image does not flicker).
;-
pro MrImageObject::Draw, $
NOERASE=noerase
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Now draw the plot to the pixmap
    self -> doPlot, NOERASE=noerase

    ;Save the system variables
    self.x_sysvar = !X
    self.y_sysvar = !Y
    self.p_sysvar = !P
end


;+
;   The purpose of this method is to do the actual plotting.
;
; :History:
;   Modification History::
;       05/04/2013  -   Do not allow zooming outside of the data range. - MRA
;-
pro MrImageObject::doPlot, $
NOERASE=noerase
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Plot TV-style
    if self.tv then begin
        image_plots, *self.image, *self.x_pos, *self.y_pos
        return
    endif

    if n_elements(noerase) eq 0 then noerase = *self.noerase

;---------------------------------------------------------------------
;Keep Zoom within Data Range /////////////////////////////////////////
;---------------------------------------------------------------------
    ;MrDrawWindow__define allows plots to be zoomed outside of the data range. Since images
    ;are zoomed by index value, not data range, subvert this behavior.
    imDims = size(*self.image, /DIMENSIONS)
    if (*self.xrange)[0] lt (*self.indep)[0] then (*self.xrange)[0] = (*self.indep)[0]
    if (*self.xrange)[1] gt (*self.indep)[imDims[0]-1] then (*self.xrange)[1] = (*self.indep)[imDims[0]-1]

;    if (*self.yrange)[0] lt (*self.dep)[0] then (*self.yrange)[0] = (*self.dep)[0]
;    if (*self.yrange)[1] gt (*self.dep)[imDims[1]-1] then (*self.yrange)[1] = (*self.dep)[imDims[1]-1]

;---------------------------------------------------------------------
;Image Zoom //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Get the index range for the zoomed image. Make sure it lies within the interval.
    ixrange = value_locate(*self.indep, *self.xrange)
    if (*self.indep)[ixrange[0]] ne (*self.xrange)[0] then ixrange[0] += 1
    
;    iyrange = value_locate(*self.dep, *self.yrange)
;    if (*self.indep)[iyrange[0]] ne (*self.yrange)[0] then iyrange[0] += 1
    iyrange=[0,-1]

;---------------------------------------------------------------------
;Display Image ///////////////////////////////////////////////////////
;---------------------------------------------------------------------

   image_plots, (*self.image)[ixrange[0]:ixrange[1], iyrange[0]:iyrange[1], self.iDisplay], $
                *self.indep, *self.dep, $
                
                ;IMAGE_PLOTS Keywords
                AXES = *self.axes, $
                BOTTOM = *self.bottom, $
                CTINDEX = *self.ctindex, $
                NAN = *self.nan, $
                SCALE = *self.scale, $
                RANGE = *self.range, $
                MISSING_VALUE = *self.missing_value, $
                MISSING_COLOR = *self.missing_color, $
                PALETTE = *self.palette, $
                TOP = *self.top, $
                
                ;Graphics Keywords
                MAX_VALUE = max_value, $
                MIN_VALUE = min_value, $
                XLOG = xlog, $
                YLOG = ylog, $
                
                ;weGraphicsKeywords
                AXISCOLOR=*self.axiscolor, $
                BACKGROUND=*self.background, $
                CHARSIZE=*self.charsize, $
                CHARTHICK=*self.charthick, $
                CLIP=*self.clip, $
                COLOR=*self.color, $
                DATA=*self.data, $
                DEVICE=*self.device, $
                NORMAL=*self.normal, $
                FONT=*self.font, $
                NOCLIP=*self.noclip, $
                NODATA=*self.nodata, $
                NOERASE=noerase, $
                POSITION=*self.position, $
                PSYM=*self.psym, $
                SUBTITLE=*self.subtitle, $
                SYMSIZE=*self.symsize, $
                T3D=*self.t3d, $
                THICK=*self.thick, $
                TICKLEN=*self.ticklen, $
                TITLE=*self.title, $
                XCHARSIZE=*self.xcharsize, $
                XGRIDSTYLE=*self.xgridstyle, $
                XMINOR=*self.xminor, $
                XRANGE=*self.xrange, $
                XSTYLE=*self.xstyle, $
                XTHICK=*self.xthick, $
                XTICK_GET=*self.xtick_get, $
                XTICKFORMAT=*self.xtickformat, $
                XTICKINTERVAL=*self.xtickinterval, $
                XTICKLAYOUT=*self.xticklayout, $
                XTICKLEN=*self.xticklen, $
                XTICKNAME=*self.xtickname, $
                XTICKS=*self.xticks, $
                XTICKUNITS=*self.xtickunits, $
                XTICKV=*self.xtickv, $
                XTITLE=*self.xtitle, $
                YCHARSIZE=*self.ycharsize, $
                YGRIDSTYLE=*self.ygridstyle, $
                YMINOR=*self.yminor, $
                YRANGE=*self.yrange, $
                YSTYLE=*self.ystyle, $
                YTHICK=*self.ythick, $
                YTICK_GET=*self.ytick_get, $
                YTICKFORMAT=*self.ytickformat, $
                YTICKINTERVAL=*self.ytickinterval, $
                YTICKLAYOUT=*self.yticklayout, $
                YTICKLEN=*self.yticklen, $
                YTICKNAME=*self.ytickname, $
                YTICKS=*self.yticks, $
                YTICKUNITS=*self.ytickunits, $
                YTICKV=*self.ytickv, $
                YTITLE=*self.ytitle;, $
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
;       DEP:                out, optional, type=any
;                           The position (as specified by the TV procedure) of the image
;                               or the data associated with the dependent variable.
;       INDEP:              out, optional, type=any
;                           The position (as specified by the TV procedure) of the image
;                               or the data associated with the independent variable.
;       IDISPLAY:           in, optional, type=boolean, default=0
;                           The index at which a 2D cut is to be taken. Applicable only
;                               for > 2D image data.
;       IMAGE:              out, optional, type=any
;                           The image to be displayed.
;       AXES:               out, optional, type=boolean
;                           Draw a set of axes around the image.
;       INIT_XRANGE:        out, optional, type=fltarr(2)
;                           The initial state of the XRANGE keyword. This is used to reset
;                               the zoom to its original state.
;       INIT_YRANGE:        out, optional, type=fltarr(2)
;                           The initial state of the YRANGE keyword. This is used to reset
;                               the zoom to its original state.
;       LAYOUT:             out, optional, type=intarr(3)/intarr(4)
;                           A vector specifying [# columns, # rows, index], or
;                               [# columns, # rows, column, row] of the plot layout and
;                               plot position. "index" increases first across then down.
;                               All numbers start with 1. If `POSITION` is also specified,
;                               this keyword is ignored.
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
;       X_SYSVAR:           out, optional, type=structure
;                           The !X system variable state associated with the image
;       XLOG:               out, optional, type=boolean
;                           Indicates that a log scale is used on the x-axis
;       Y_POS:              out, optional, type=int
;                           If the `TV` keyword is in use, then this specifies the y-
;                               position of the image as specified by the IDL's TV command.
;       Y_SYSVAR:           out, optional, type=structure
;                           The !Y system variable state associated with the image
;       YLOG:               out, optional, type=boolean
;                           Indicates that a log scale is used on the y-axis
;       _REF_EXTRA:         out, optional, type=structure
;                           Any keyword accepted by IDL's Plot procedure or MrImageObject
;-
pro MrImageObject::GetProperty, $
;Data keywords
IMAGE = image, $
INDEP = indep, $
DEP = dep, $
X_POS = x_pos, $
Y_POS = y_pos, $

;MrImageObject Keywords
IDISPLAY = iDisplay, $
INIT_XRANGE = init_xrange, $
INIT_YRANGE = init_yrange, $
LAYOUT = layout, $
TV = tv, $
P_SYSVAR = p_sysvar, $
X_SYSVAR = x_sysvar, $
Y_SYSVAR = y_sysvar, $

;IMAGE_PLOTS Keywords
AXES = axes, $
BOTTOM = bottom, $
CTINDEX = ctindex, $
MISSING_VALUE = missing_value, $
MISSING_COLOR = missing_color, $
NAN = nan, $
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
        void = error_message()
        return
    endif

    ;Data Properties
    if arg_present(IMAGE) and n_elements(*self.IMAGE) ne 0 then image = *self.image
    if arg_present(X_POS) and n_elements(*self.X_POS) ne 0 then x_pos = *self.x_pos
    if arg_present(Y_POS) and n_elements(*self.Y_POS) ne 0 then y_pos = *self.y_pos
    if arg_present(DEP)   and n_elements(*self.DEP)   ne 0 then dep = *self.dep
    if arg_present(INDEP) and n_elements(*self.INDEP) ne 0 then indep = *self.indep

    ;MrImageObject Properties
    if arg_present(iDisplay)    then iDisplay    =  self.iDisplay
    if arg_present(INIT_XRANGE) then init_xrange =  self.init_xrange
    if arg_present(INIT_YRANGE) then init_yrange =  self.init_yrange
    if arg_present(layout)      then layout      = *self.layout
    if arg_present(p_sysvar)    then p_sysvar    =  self.p_sysvar
    if arg_present(x_sysvar)    then x_sysvar    =  self.x_sysvar
    if arg_present(y_sysvar)    then y_sysvar    =  self.y_sysvar
        
    ;Graphics Properties
    if arg_present(MAX_VALUE) and n_elements(*self.MAX_VALUE) ne 0 then max_value = *self.max_value
    if arg_present(MIN_VALUE) and n_elements(*self.MIN_VALUE) ne 0 then min_value = *self.min_value
    if arg_present(XLOG)      and n_elements(*self.XLOG)      ne 0 then xlog = *self.xlog
    if arg_present(YLOG)      and n_elements(*self.YLOG)      ne 0 then ylog = *self.ylog

    ;IMAGE_PLOTS.PRO Properties
    if arg_present(AXES)        and n_elements(*self.AXES)        ne 0 then axes = *self.axes
    if arg_present(BOTTOM)      and n_elements(*self.BOTTOM)      ne 0 then bottom = *self.bottom
    if arg_present(CTINDEX)     and n_elements(*self.CTINDEX)     ne 0 then ctindex = *self.ctindex
    if arg_present(RANGE)       and n_elements(*self.RANGE)       ne 0 then range = *self.range
    if arg_present(MISSING_VALUE) and n_elements(*self.MISSING_VALUE) ne 0 then missing_value = *self.missing_value
    if arg_present(MISSING_COLOR) and n_elements(*self.MISSING_COLOR) ne 0 then missing_color = *self.missing_color
    if arg_present(PALETTE)     and n_elements(*self.palette)     ne 0 then palette = *self.palette
    if arg_present(NAN)         and n_elements(*self.NAN)         ne 0 then nan = *self.nan
    if arg_present(SCALE)       and n_elements(*self.SCALE)       ne 0 then scale = *self.scale
    if arg_present(TOP)         and n_elements(*self.TOP)         ne 0 then top = *self.top
    
    ;LINE_PLOTS.PRO Properties
    if arg_present(YAX_RIGHT) and n_elements(YAX_RIGHT) ne 0 then yax_right = *self.yax_right
    
    ;weGraphicsKeywords Properties
    if n_elements(EXTRA) ne 0 then $
        self -> weGraphicsKeywords::GetProperty, _STRICT_EXTRA=extra
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
;       DEP:                in, optional, type=any
;                           The position (as specified by the TV procedure) of the image
;                               or the data associated with the dependent variable.
;       INDEP:              in, optional, type=any
;                           The position (as specified by the TV procedure) of the image
;                               or the data associated with the independent variable.
;       IMAGE:              in, optional, type=any
;                           The image to be displayed.
;       IDISPLAY:           in, optional, type=boolean, default=0
;                           The index at which a 2D cut is to be taken. Applicable only
;                               for > 2D image data.
;       AXES:               in, optional, type=boolean
;                           Draw a set of axes around the image.
;       INIT_XRANGE:        in, optional, type=fltarr(2)
;                           The initial state of the XRANGE keyword. This is used to reset
;                               the zoom to its original state.
;       INIT_YRANGE:        in, optional, type=fltarr(2)
;                           The initial state of the YRANGE keyword. This is used to reset
;                               the zoom to its original state.
;       LAYOUT:             in, optional, type=intarr(3)/intarr(4)
;                           A vector specifying [# columns, # rows, index], or
;                               [# columns, # rows, column, row] of the plot layout and
;                               plot position. "index" increases first across then down.
;                               All numbers start with 1. If `POSITION` is also specified,
;                               this keyword is ignored.
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
;       X_SYSVAR:           in, optional, type=structure
;                           The !X system variable state associated with the image
;       XLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the x-axis
;       Y_POS:              in, optional, type=int
;                           If the `TV` keyword is in use, then this specifies the y-
;                               position of the image as specified by the IDL's TV command.
;       Y_SYSVAR:           in, optional, type=structure
;                           The !Y system variable state associated with the image
;       YLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the y-axis
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by IDL's Plot procedure or MrImageObject
;-
pro MrImageObject::SetProperty, $
DRAW = draw, $

;Data keywords
DEP = dep, $
IMAGE = image, $
INDEP = indep, $
X_POS = x_pos, $
Y_POS = y_pos, $

;MrImageObject Keywords
IDISPLAY = iDisplay, $
INIT_XRANGE = init_xrange, $
INIT_YRANGE = init_yrange, $
LAYOUT = layout, $
TV = tv, $
P_SYSVAR = p_sysvar, $
X_SYSVAR = x_sysvar, $
Y_SYSVAR = y_sysvar, $
      
;IMAGE_PLOTS Keywords
AXES = axes, $
BOTTOM = bottom, $
CTINDEX = ctindex, $
MISSING_VALUE = missing_value, $
MISSING_COLOR = missing_color, $
NAN = nan, $
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
        void = error_message()
        return
    endif

    ;Data
    if n_elements(x_pos) ne 0 then *self.x_pos = x_pos
    if n_elements(y_pos) ne 0 then *self.y_pos = y_pos
    if n_elements(indep) ne 0 then *self.indep = indep
    if n_elements(dep)   ne 0 then *self.dep = dep
    if n_elements(image) ne 0 then *self.image = image

    ;MrImageObject Keywords
    if n_elements(iDisplay)    ne 0 then self.iDisplay = iDisplay
    if n_elements(INIT_XRANGE) ne 0 then self.init_xrange = init_xrange
    if n_elements(INIT_YRANGE) ne 0 then self.init_yrange = init_yrange
    if n_elements(P_SYSVAR)    ne 0 then self.p_sysvar = p_sysvar
    if n_elements(X_SYSVAR)    ne 0 then self.x_sysvar = x_sysvar
    if n_elements(Y_SYSVAR)    ne 0 then self.y_sysvar = y_sysvar
    if n_elements(TV)          ne 0 then self.tv = keyword_set(tv)
    if n_elements(LAYOUT)      ne 0 then begin
        *self.layout = layout
        *self.position = MrPlotLayout(layout)
    endif

    ;IMAGE_PLOTS.PRO Properties
    if n_elements(AXES)          ne 0 then *self.axes = keyword_set(axes)
    if n_elements(BOTTOM)        ne 0 then *self.bottom = bottom
    if n_elements(CTINDEX)       ne 0 then *self.ctindex = ctindex
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
    if n_elements(XLOG)      ne 0 then *self.xlog = keyword_set(xlog)
    if n_elements(YLOG)      ne 0 then *self.ylog = keyword_set(ylog)
    
    ;weGraphicsKeywords Properties
    if n_elements(EXTRA) ne 0 then self -> weGraphicsKeywords::SetProperty, _STRICT_EXTRA=extra
        
    if keyword_set(draw) then self -> Draw
end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrImageObject::cleanup
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
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
    ptr_free, self.layout
    ptr_free, self.max_value
    ptr_free, self.min_value
    ptr_free, self.missing_color
    ptr_free, self.missing_value
    ptr_free, self.nan
    ptr_free, self.palette
    ptr_free, self.range
    ptr_free, self.scale
    ptr_free, self.top
    ptr_free, self.xlog
    ptr_free, self.ylog
    
    ;Superclasses
    self -> MrIDL_Container::Cleanup
    self -> weGraphicsKeywords::Cleanup
end


;+
;   The purpose of this method is to initialize the MrImageObjects class.
;
; :Params:
;       IMAGE:              in, required, type=any
;                           The image to be displayed.
;       X:                  in, optional, type=any
;                           If `TV` is set, this is the x-position of the image, as
;                               specified by the TV procedure. Otherwise, if `Y` is not
;                               given, these are the coordinates of the second dimension
;                               of `IMAGE`. IF `Y` is given, it represents the
;                               coordinates of the first dimension of `IMAGE`
;       Y:                  in, optional, type=any
;                           If `TV` is set, this is the y-position of the image, as
;                               specified by the TV procedure. Otherwise, it represents
;                               the coodinates of the 2nd dimenion of `IMAGE`. X and Y
;                               must have the same number of elements as the dimensions
;                               of IMAGE.
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
;       LAYOUT:             in, optional, type=intarr(3)/intarr(4)
;                           The location of the plot in a 2D plotting grid. The first two
;                               elements specify the total number of columns and rows in
;                               the 2D layout. If 3-elements exist, the third specifies
;                               the overall position of the plot: [ncols, nrows, index].
;                               If 4-elements, the column and row in which the plot is to
;                               be placed: [ncols, nrows, col, row]. "index" begins at 1
;                               the with plot in the upper-left corner, then increases
;                               first down, then right.
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
;                           Any keyword accepted by IDL's Plot procedure or image_plots.pro
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
function MrImageObject::init, image, x, y, $
;MrImageObject Keywords
DRAW = draw, $
IDISPLAY = idisplay, $
KEEP_ASPECT = keep_aspect, $
LAYOUT = layout, $
TV = tv, $
      
;IMAGE_PLOTS Keywords
AXES = axes, $
BOTTOM = bottom, $
CTINDEX = ctindex, $
MISSING_VALUE = missing_value, $
MISSING_COLOR = missing_color, $
NAN = nan, $
PALETTE = palette, $
RANGE = range, $
SCALE = scale, $
TOP = top, $

;Graphics Keywords
MAX_VALUE = max_value, $
MIN_VALUE = min_value, $
POSITION = position, $
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
        void = error_message()
        return, 0
    endif
    
    setDefaultValue, gui, 1, /BOOLEAN
    setDefaultValue, tv, 0, /BOOLEAN
    setDefaultValue, draw, 1, /BOOLEAN
    setDefaultValue, iDisplay, 0
    setDefaultValue, xsize, 600
    setDefaultValue, ysize, 340
    
    ;Call the superclass init method. Prevent some Coyote Graphics
    ;defaults from taking effect. The EXTRA structure has precedence over
    ;the keywords, so if AXISCOLOR, COLOR, or CHARSIZE are supplied by the user,
    ;those values will be used.
    status = self -> weGraphicsKeywords::INIT(AXISCOLOR='black', COLOR='black', CHARSIZE=1.0, $
                                              _STRICT_EXTRA=extra)
    if status eq 0 then return, 0

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
    ndep = n_elements(dep)
    nindep = n_elements(indep)

    ;Is TV positioning to be used?
    if keyword_set(tv) then begin
        if ndep eq 0 $
            then self.x_pos = ptr_new(/ALLOCATE_HEAP) $
            else self.x_pos = dep
            
        if nindep eq 0 $
            then self.y_pos = ptr_new(/ALLOCATE_HEAP) $
            else self.y_pos = indep

        ;Undefine dep and indep so that they can be filled propertly in the next step        
        void = temporary(dep)
        void = temporary(indep)
    endif else begin
        self.x_pos = ptr_new(/ALLOCATE_HEAP)
        self.y_pos = ptr_new(/ALLOCATE_HEAP)
    endelse

;---------------------------------------------------------------------
;Set Dependent Variable and XRANGE ///////////////////////////////////
;---------------------------------------------------------------------
    imDims = size(image, /DIMENSIONS)

    ;If no dependent variable was given
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


;---------------------------------------------------------------------
;Set Independent Variable and YRANGE /////////////////////////////////
;---------------------------------------------------------------------
    ;If no dependent variable was given
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
    
    ;IMAGE_PLOTS Properties
    self.image         = ptr_new(/ALLOCATE_HEAP)
    self.indep         = ptr_new(/ALLOCATE_HEAP)
    self.dep           = ptr_new(/ALLOCATE_HEAP)
    self.bottom        = ptr_new(/ALLOCATE_HEAP)
    self.ctindex       = ptr_new(/ALLOCATE_HEAP)
    self.axes          = ptr_new(/ALLOCATE_HEAP)
    self.range         = ptr_new(/ALLOCATE_HEAP)
    self.layout        = ptr_new(/ALLOCATE_HEAP)
    self.min_value     = ptr_new(/ALLOCATE_HEAP)
    self.missing_value = ptr_new(/ALLOCATE_HEAP)
    self.missing_color = ptr_new(/ALLOCATE_HEAP)
    self.max_value     = ptr_new(/ALLOCATE_HEAP)
    self.nan           = ptr_new(/ALLOCATE_HEAP)
    self.palette       = ptr_new(/ALLOCATE_HEAP)
    self.scale         = ptr_new(/ALLOCATE_HEAP)
    self.top           = ptr_new(/ALLOCATE_HEAP)
    
    ;LINE_PLOTS properties
    self.xlog   = ptr_new(/ALLOCATE_HEAP)
    self.xrange = ptr_new(/ALLOCATE_HEAP)
    self.ylog   = ptr_new(/ALLOCATE_HEAP)
    self.yrange = ptr_new(/ALLOCATE_HEAP)
    
    ;Image range
    if n_elements(range) eq 0 $
        then imRange = [min(image, NAN=nan, max=imMax), imMax] $
        else imRange = range

    ;Set the object properties
    self -> SetProperty, IMAGE = image, $
                         DEP = dep, $
                         INDEP = indep, $
                         AXES = axes, $
                         BOTTOM = bottom, $
                         CTINDEX = ctindex, $
                         IDISPLAY = iDisplay, $
                         LAYOUT = layout, $
                         MAX_VALUE = max_value, $
                         MIN_VALUE = min_value, $
                         MISSING_VALUE = missing_value, $
                         MISSING_COLOR = missing_color, $
                         NAN = nan, $
                         PALETTE = palette, $
                         POSITION = position, $
                         RANGE = imRange, $
                         SCALE = scale, $
                         TOP = top, $
                         TV = tv, $
                         XLOG = xlog, $
                         XRANGE = xrange, $
                         YLOG = ylog, $
                         YRANGE = yrange

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

    return, 1
end


;+
;   Object class definition
;-
pro MrImageObject__define
    compile_opt idl2
    
    define = {MrImageObject, $
              inherits MrIDL_Container, $
              inherits MrGraphicAtom, $
              inherits weGraphicsKeywords, $
              
              ;Data
              x_pos: ptr_new(), $               ;the x-position (see TV)
              y_pos: ptr_new(), $               ;the y-position (see TV)
              indep: ptr_new(), $               ;independent variable
              dep:   ptr_new(), $               ;dependent variable
              image: ptr_new(), $               ;image to be displayed
              tv: 0, $                          ;indicate that a TV position was given
              
              ;MrImageObject Properties
              idisplay: 0, $                    ;Index to display (>3D images)
              init_range: dblarr(2), $          ;Initial image range
              init_xrange: dblarr(2), $         ;Initial x-range
              init_yrange: dblarr(2), $         ;Initial y-range
              layout: ptr_new(), $              ;Location of image in 2D layout
              p_sysvar: !P, $                   ;Save the P system variable
              x_sysvar: !X, $                   ;Save the X system variable
              y_sysvar: !Y, $                   ;Save the Y system variable
              
              ;IMAGE_PLOTS Keywords
              axes: ptr_new(), $                ;Draw axes around the image?
              bottom: ptr_new(), $              ;If scaled, minimum scaled value
              ctindex: ptr_new(), $             ;Color index to load
              missing_value: ptr_new(), $       ;Value to be treated as missing
              missing_color: ptr_new(), $       ;Color of missing value
              nan: ptr_new(), $                 ;Search for NaN's when scaling?
              palette: ptr_new(), $             ;Color table to be loaded
              range: ptr_new(), $               ;Range at which the color table saturates
              scale: ptr_new(), $               ;Byte-scale the image
              top: ptr_new(), $                 ;If scaled, maximum scaled value
              
              ;Plotting Keywords
              max_value: ptr_new(), $           ;maximum value displayed in plot
              min_value: ptr_new(), $           ;minimum value displayed in plot
              xlog: ptr_new(), $                ;log-scale the x-axis?
              ylog: ptr_new()}                  ;log-scale the y-axis?
end