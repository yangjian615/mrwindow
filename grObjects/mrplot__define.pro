; docformat = 'rst'
;
; NAME:
;       MrPlot__Define
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
;   The purpose of this method is to create an object out of the cgPlot routine.
;
; :Examples:
;   Plot an Nx2 array as two line plots on a single axis.
;       x = findgen(101)/100
;       y = sin(2*!pi*x)
;       z = cos(2*!pi*x)
;       a = obj_new('MrPlot', x, [[y],[z]], DIMENSION=2, TITLE='Sin(x) & Cos(x)', $
;                                   COLOR=['black', 'blue'], XTITLE='Time (s)', $
;                                   YTITLE='Amplitude', /DRAW)
;       obj_destroy, a
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
;       Matthew Argall 2013
;
; :History:
;   Modification History::
;       05/18/2013  -   Written by Matthew Argall
;       06/28/2013  -   Added the DIMENSION keyword. Removed the AXES and COLORBARS
;                           properties because the former was not being used and the
;                           latter interfered with the COLOR keyword. - MRA
;       07/04/2013  -   Added the COLOR keyword so that each column or row in DIMENSION
;                           can be plotted in a different color. - MRA
;       07/31/2013  -   Added the ConvertCoord method. - MRA
;       08/09/2013  -   Inherit MrIDL_Container - MRA
;       08/10/2013  -   Added the LAYOUT keyword. - MRA
;       08/22/2013  -   Added the NOERASE keyword to Draw. Was forgetting to set the
;                           position property in SetProperty. Fixed. - MRA
;       08/23/2013  -   Added the IsInside method. - MRA
;       08/30/2013  -   Missed the SYMCOLOR keyword. Now included. - MRA
;       09/08/2013  -   Number of default colors now matches the number of dimensions
;                           being plotted. - MRA
;                           PLOT_INDEX keyword in IsAvailable now works. - MRA
;       09/27/2013  -   Use N_Elements instead of N_Params in case `Y` is present but 
;                           undefined. Position and layout properties are now handled
;                           by MrGraphicAtom. Renamed from MrPlotObject to MrPlot. - MRA
;       09/29/2013  -   Ensure that the layout is updated only when a layout keyword is
;                           passed in. - MRA
;       10/07/2013  -   Added the HIDE keyword. - MRA
;       2013/11/17  -   CHARSIZE is now a MrGraphicAtom property. Use _EXTRA instead of
;                           _STRICT_EXTRA in some cases to make setting and getting
;                           properties easier and to reduce list of keywords. - MRA
;       2013/11/20  -   MrIDL_Container and MrGraphicAtom is disinherited. Inherit instead
;                           MrGrAtom and MrLayout. - MRA
;       2013/11/21  -   Added the doOverplot and TF_Overplot methods as well as the
;                           OVERPLOT property. Renamed DRAW to REFRESH. Refreshing is now
;                           done automatically. Call the Refresh method with the DISABLE
;                           keyword set to temporarily turn of Refresh. - MRA
;       2013/11/23  -   Added the SetLayout and Overplot methods. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to draw the plot in the draw window.
;-
pro MrPlot::Draw, $
NOERASE = noerase
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    if self.hide then return

    ;Draw the plot
    if self.overplot then begin
        self.target -> RestoreCoords
        self -> doOverplot
        self -> SaveCoords
    endif else begin
        self -> doPlot, NOERASE=noerase
        self -> SaveCoords
    endelse
end


;+
;   The purpose of this method is to do the actual plotting. Basically, having this here
;   merely to saves space in the Draw method.
;-
pro MrPlot::doPlot, $
NOERASE=noerase
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    if n_elements(noerase) eq 0 then noerase = *self.noerase

    ;Draw the plot.
    wePlot, *self.indep, *self.dep, $
            DIMENSION = self.dimension, $
            OUTPUT    = output, $
            LABEL     = self.label, $
            
            ;cgPlot Keywords
            SYMCOLOR  = *self.symcolor, $
                   
            ;MrGraphicsAtom Keywords
            POSITION  = self.position, $

            ;Graphics Keywords
            MAX_VALUE = *self.max_value, $
            MIN_VALUE = *self.min_value, $
            XLOG      = *self.xlog, $
            YLOG      = *self.ylog, $
            YNOZERO   = *self.ynozero, $
               
            ;weGraphicsKeywords
            AXISCOLOR     = *self.axiscolor, $
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
;   The purpose of this method is to do the actual overplotting. Basically, having this
;   here merely to saves space in the Draw method.
;-
pro MrPlot::doOverplot

    catch, theerror
    if theerror ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Get the dimensions of the independent variable.
    weoplot, *self.indep, *self.dep, $

             ;weOPlot Keywords
             COLOR     = *self.color, $
             DIMENSION =  self.dimension, $
             LINESTYLE = *self.linestyle, $
             PSYM      = *self.psym, $
             SYMCOLOR  = *self.symcolor, $
             SYMSIZE   = *self.symsize, $
             THICK     = *self.thick, $

             ;cgOPlot Keywords
             CHARSIZE  =  self.charsize, $

             ;OPlot Keywords
             NSUM      = *self. nsum, $
             POLAR     = *self. polar, $
             CLIP      = *self. clip, $
             NOCLIP    = *self. noclip, $
             T3D       = *self. t3d, $
             ZVALUE    = *self. zvalue
END
  

;+
;   The purpose of this method is to retrieve data
;
; :Calling Sequence:
;       myGraphic -> SetData, y
;       myGraphic -> SetData, x, y
;
; :Params:
;       X:              out, required, type=numeric array
;                       If this is the only argument, the dependent variable data is
;                           returned. If `Y` is also present, X will be the independent
;                           variable's data.
;       Y:              out, optional, type=numeric array
;                       If present, the dependent variable's data will be returned
;-
pro MrPlot::GetData, x, y
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Retrieve the data
    case n_params() of
        1: x = *self.dep
        2: begin
            x = *self.indep
            y = *self.dep
        endcase
        else: message, 'Incorrect number of parameters.'
    endcase
end


;+
;   The purpose of this method is to determine if overplotting is being done.
;
; :Params:
;       TARGET:             out, optional, type=object
;                           If `TF_OVERPLOT`=1, then the overplot target will be returned.
;
; :Returns:
;       TF_OVERPLOT:        True (1) if overplotting, false (0) if not.
;-
function MrPlot::GetOverplot, target
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0B
    endif
    
    ;Get the overplot state and the target.
    tf_overplot = self.overplot
    if tf_overplot then if arg_present(target) then target = self.target
    
    return, tf_overplot
end


;+
;   The purpose of this method is to retrieve object properties
;
; :Keywords:
;       DIMENSION:          in, optional, type=int
;                           The dimension over which to plot.
;       INIT_XRANGE:        out, optional, type=fltarr(2)
;                           The initial state of the XRANGE keyword. This is used to reset
;                               the zoom to its original state.
;       INIT_YRANGE:        out, optional, type=fltarr(2)
;                           The initial state of the YRANGE keyword. This is used to reset
;                               the zoom to its original state.
;       LABEL:              out, optional, type=string
;                           A label is similar to a plot title, but it is aligned to the
;                               left edge of the plot and is written in hardware fonts.
;                               Use of the label keyword will suppress the plot title.
;       MAX_VALUE:          out, optional, type=float
;                           The maximum value plotted. Any values larger than this are
;                               treated as missing.
;       MIN_VALUE:          out, optional, type=float
;                           The minimum value plotted. Any values smaller than this are
;                               treated as missing.
;       NSUM:               out, optional, type=integer
;                           The presence of this keyword indicates the number of data
;                               points to average when plotting.
;       POLAR:              out, optional, type=boolean
;                           Indicates that X and Y are actually R and Theta and that the
;                               plot is in polar coordinates.
;       XLOG:               out, optional, type=boolean
;                           Indicates that a log scale is used on the x-axis
;       YLOG:               out, optional, type=boolean
;                           Indicates that a log scale is used on the y-axis
;       YNOZERO:            out, optional, type=boolean, default=0
;                           Inhibit setting the y  axis value to zero when all Y > 0 and
;                               no explicit minimum is set.
;       _REF_EXTRA:         out, optional, type=any
;                           Keyword accepted by the superclasses are also accepted for
;                               keyword inheritance.
;-
pro MrPlot::GetProperty, $
;MrPlot Properties
DIMENSION = dimension, $
INIT_XRANGE = init_xrange, $
INIT_YRANGE = init_yrange, $
LABEL = label, $

;cgPlot Properties
OVERPLOT = overplot, $
SYMCOLOR = symcolor, $

;Graphics Properties
MAX_VALUE = max_value, $
MIN_VALUE = min_value, $
NSUM = nsum, $
POLAR = polar, $
XLOG = xlog, $
YLOG = ylog, $
YNOZERO = ynozero, $

;weGraphicsKeywords Properties
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;MrPlot Properties
    if arg_present(dimension)   then dimension   =  self.dimension
    if arg_present(init_xrange) then init_xrange =  self.init_xrange
    if arg_present(init_yrange) then init_yrange =  self.init_yrange
    if arg_present(label)       then label       =  self.label
    if arg_present(overplot)    then if n_elements(*self.overplot) gt 0 then overplot = *self.overplot
    
    ;cgPlot Properties
    if arg_present(symcolor) and n_elements(*self.symcolor)  ne 0 then symcolor = *self.symcolor
    
    ;Graphics Properties
    if arg_present(MAX_VALUE) and n_elements(*self.MAX_VALUE) ne 0 then max_value = *self.max_value
    if arg_present(MIN_VALUE) and n_elements(*self.MIN_VALUE) ne 0 then min_value = *self.min_value
    if arg_present(nsum)      and n_elements(*self.nsum)      ne 0 then nsum = *self.nsum
    if arg_present(XLOG)      and n_elements(*self.XLOG)      ne 0 then xlog = *self.xlog
    if arg_present(YLOG)      and n_elements(*self.YLOG)      ne 0 then ylog = *self.ylog
    if arg_present(POLAR)     and n_elements(*self.POLAR)     ne 0 then polar = *self.polar
    if arg_present(YNOZERO)   and n_elements(*self.YNOZERO)   ne 0 then ynozero = *self.ynozero
    
    ;Get all of the remaining keywords from weGraphicsKeywords
    if n_elements(EXTRA) gt 0 then begin
        self -> MrGrAtom::GetProperty, _EXTRA=extra
        self -> MrLayout::GetProperty, _EXTRA=extra
        self -> weGraphicsKeywords::GetProperty, _EXTRA=extra
    endif
end


;+
;   The purpose of this method is to change the plot from a normal plot to an overplot
;   and vice versa.
;
; :Params:
;       TARGET:             in, optional, type=objref
;                           An MrGraphicObject onto which this Plot will be overplotted.
;                               If not present, the currently selected graphic will be
;                               used.
;
; :Keywords:
;       DISABLE:            in, optional, type=boolean, default=0
;                           Convert an overplot to a plot. If set, then `TARGET` may be
;                               a 4-element vector of the form [x0, y0, x1, y1] that
;                               specifies the lower-right and upper-left corners of the
;                               plot. If `TARGET` is not provided, the plot will be placed
;                               at the next available layout location.
;-
pro MrPlot::Overplot, target, $
DISABLE=disable
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        if n_elements(init_refresh) gt 0 then self.window -> Refresh, DISABLE=~init_refresh
        return
    endif
    
    ;Disable refreshing
    self.window -> GetProperty, REFRESH=init_refresh
    self.window -> Refresh, /DISABLE
    
    ;Disable overplotting
    if keyword_set(disable) then begin
        self.window -> Make_Location, location
        self.window -> SetPosition, self.layout[2], location
        self.target = obj_new()
        self.overplot = 0B

    ;Enable overplotting        
    endif else begin
        ;If not TARGET was specified, get the currently selected graphic
        if n_elements(target) eq 0 then target = self.window -> GetSelect()

        ;Ensure we can overplot on top of the target graphic
        oplottable = ['MrPlot', 'MrImage', 'MrContour']
        if IsMember(oplottable, obj_class(target), /FOLD_CASE) eq 0 || obj_valid(target) eq 0 $
            then message, 'TARGET must be valid and of class ' + strjoin(oplottable, ' ')

        ;Get the position
        target -> GetProperty, POSITION=position
        
        ;Remove SELF from layout.
        self.window -> SetPosition, self.layout[2], position
        self.overplot = 1B
        self.target = target
    endelse
    
    ;Re-enable refreshing
    self.window -> Refresh, DISABLE=~init_refresh
end


;+
;   The purpose of this method is to set data
;
; :Calling Sequence:
;       myGraphic -> SetData, y
;       myGraphic -> SetData, x, y
;
; :Params:
;       X:              in, required, type=numeric array
;                       If this is the only argument, then X represents the dependent
;                           variable data. If `Y` is also given, then X represents the
;                           independent variable data.
;       Y:              in, optional, type=numeric array
;                       The dependent variable data.
;-
pro MrPlot::SetData, x, y
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Retrieve the data
    case n_params() of
        1: *self.dep = x
        2: begin
            *self.indep = x
            *self.dep = y
        endcase
        else: message, 'Incorrect number of parameters.'
    endcase
    
    ;Refresh the graphics window
    self.window -> Draw
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
pro MrPlot::SetLayout, layout, $
POSITION=position, $
UPDATE_LAYOUT=update_layout, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
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
;       DIMENSION:          in, optional, type=int
;                           The dimension over which to plot.
;       LABEL:              in, optional, type=string
;                           A label is similar to a plot title, but it is aligned to the
;                               left edge of the plot and is written in hardware fonts.
;                               Use of the label keyword will suppress the plot title.
;       MAX_VALUE:          in, optional, type=float
;                           The maximum value plotted. Any values larger than this are
;                               treated as missing.
;       MIN_VALUE:          in, optional, type=float
;                           The minimum value plotted. Any values smaller than this are
;                               treated as missing.
;       NSUM:               in, optional, type=integer
;                           The presence of this keyword indicates the number of data
;                               points to average when plotting.
;       POLAR:              in, optional, type=boolean
;                           Indicates that X and Y are actually R and Theta and that the
;                               plot is in polar coordinates.
;       POSITION:           in, optional, type=fltarr(4)
;                           A vector of the form [x0, y0, x1, y1] specifying the location
;                               of the lower-left and upper-right corners of the graphic,
;                               in normalized coordinates.
;       XLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the x-axis
;       YLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the y-axis
;       YNOZERO:            in, optional, type=boolean, default=0
;                           Inhibit setting the y  axis value to zero when all Y > 0 and
;                               no explicit minimum is set.
;       _REF_EXTRA:         in, optional, type=any
;                           Keyword accepted by the MrGrAtom and weGraphicsKeywords are
;                               also accepted for keyword inheritance.
;-
pro MrPlot::SetProperty, $
;MrPlot Properties
DIMENSION = dimension, $
LABEL = label, $

;cgPlot Properties
SYMCOLOR = symcolor, $

;Graphics Properties
MAX_VALUE = max_value, $
MIN_VALUE = min_value, $
NSUM = nsum, $
POLAR = polar, $
XLOG = xlog, $
YLOG = ylog, $
YNOZERO = ynozero, $

;weGraphics Properties
POSITION = position, $
XSTYLE = xstyle, $          ;Check explicitly so that the 2^0 bit is always set
YSTYLE = ystyle, $          ;Check explicitly so that the 2^0 bit is always set
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;MrPlot Properties
    if n_elements(DIMENSION)   ne 0 then  self.dimension = dimension
    if n_elements(LABEL)       ne 0 then  self.label = label
    
    ;cgPlot Properties
    if n_elements(SYMCOLOR) ne 0 then *self.symcolor = symcolor
    
    ;Graphics Properties
    if n_elements(MAX_VALUE) ne 0 then *self.max_value = max_value
    if n_elements(MIN_VALUE) ne 0 then *self.min_value = min_value
    if n_elements(NSUM)      ne 0 then *self.nsum = nsum
    if n_elements(POLAR)     ne 0 then *self.polar = keyword_set(polar)
    if n_elements(XLOG)      ne 0 then *self.xlog = keyword_set(xlog)
    if n_elements(YLOG)      ne 0 then *self.ylog = keyword_set(ylog)
    if n_elements(YNOZERO)   ne 0 then *self.ynozero = keyword_set(ynozero)
    if n_elements(xstyle)    ne 0 then *self.xstyle = ~(xstyle and 1) + xstyle
    if n_elements(ystyle)    ne 0 then *self.ystyle = ~(ystyle and 1) + ystyle
    
    if n_elements(position) gt 0 then self -> SetLayout, POSITION=position

;---------------------------------------------------------------------
;Superclass Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
    nExtra = n_elements(extra)
    if nExtra gt 0 then begin
        ;MrGrAtom -- Pick out the keywords here to use _STRICT_EXTRA instead of _EXTRA
        atom_kwds = ['HIDE', 'NAME']
        void = IsMember(atom_kwds, extra, iAtom, N_MATCHES=nAtom, NONMEMBER_INDS=iExtra, N_NONMEMBER=nExtra)
        if nAtom gt 0 then self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra[iAtom]
    
        ;weGraphicsKeywords Properties
        if nExtra gt 0 then self -> weGraphicsKeywords::SetProperty, _STRICT_EXTRA=extra[iExtra]
    endif
    
    ;Refresh the graphics window
    self.window -> Draw
end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrPlot::cleanup
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;free all pointers
    ptr_free, self.indep
    ptr_free, self.dep
    ptr_free, self.max_value
    ptr_free, self.min_value
    ptr_free, self.nsum
    ptr_free, self.xlog
    ptr_free, self.ylog
    ptr_free, self.polar
    ptr_free, self.symcolor
    
    ;Cleanup the superclass.
    self -> weGraphicsKeywords::CLEANUP
    self -> MrGrAtom::CleanUp
    self -> MrLayout::CleanUp
end


;+
;   The purpose of this method is to create line plot in a zoomable, resizeable window
;   that contains several analysis options (with more to be added). Only certain features
;   are available at any one time, but all can be selected from the menu bar.
;
; :Params:
;       X:                  in, required, type=any
;                           If Y is given, a vector representing the independent variable
;                               to be plotted. If Y is not given, a vector or array of
;                               representing the dependent variable to be plotted. Each
;                               column of X will then be overplotted as individual vectors
;                               in the same set of axes.
;       Y:                  in, optional, type=any
;                           A vector or array of representing the dependent variable to be
;                               plotted. Each column of Y will then be overplotted
;                               as individual vectors in the same set of axes.
;
; :Keywords:
;       COLOR:              in, optional, type=string/strarr, default='opposite'
;                           Color of the line plots. If `DIMENSION` is used, there must
;                               be one color per component.
;       DIMENSION:          in, optional, type=int, default=0
;                           The dimension over which to plot. As an example, say `Y` is
;                               an N1xN2 array and settind DIMENSION=2. Then, N1 plots
;                               will be overplotted on top of each other, one for each
;                               DATA[i,*]. If DIMENSION=0, then a single plot of all
;                               points will be made.
;       DRAW:               in, optional, type=boolean, default=1
;                           If set, the data will be drawn to the plot. DRAW=1 always if
;                               `GUI`=1.
;       MAX_VALUE:          in, optional, type=float
;                           The maximum value plotted. Any values larger than this are
;                               treated as missing.
;       MIN_VALUE:          in, optional, type=float
;                           The minimum value plotted. Any values smaller than this are
;                               treated as missing.
;       POLAR:              in, optional, type=boolean
;                           Indicates that X and Y are actually R and Theta and that the
;                               plot is in polar coordinates.
;       POSITION:           in, optional, type=fltarr(4)
;                           A vector of the form [x0, y0, x1, y1], where [x0,y0] and [x1,y1]
;                               specify the position of the lower-left and upper-right
;                               corner of the plotting region, respectively.
;       XLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the x-axis
;       XRANGE:             in, optional, type=fltarr(2), default=[min(`X`)\, max(`X`)]
;                           The x-axis range over which the data will be displayed.
;       YLOG:               in, optional, type=boolean
;                           Indicates that a log scale is used on the y-axis
;       YNOZERO:            in, optional, type=boolean, default=0
;                           Inhibit setting the y  axis value to zero when all Y > 0 and
;                               no explicit minimum is set.
;       YRANGE:             in, optional, type=fltarr(2), default=[min(`X`)\, max(`X`)]
;                           The y-axis range over which the data will be displayed.
;       _REF_EXTRA:         in, optional, type=any
;                           Keywords accepted by the any of the superclasses are also
;                               accepted for keyword inheritcance.
;
; :Uses:
;   Uses the following external programs::
;       binary.pro (Coyote Graphics)
;       weGraphicsKeywords__define.pro (Coyote Graphics)
;       error_message.pro (Coyote Graphics)
;       MrDrawWindow__define.pro
;       MrPlotLayout.pro
;       MrGetWindow.pro
;-
function MrPlot::init, x, y, $
;MrPlot Keywords
CURRENT = current, $
DIMENSION = dimension, $

;cgPlot Keywords
OVERPLOT = target, $
SYMCOLOR = symcolor, $

;Graphics Keywords
COLOR = color, $
MAX_VALUE = max_value, $
MIN_VALUE = min_value, $
NSUM = nsum, $
POLAR = polar, $
XRANGE = xrange, $
XLOG = xlog, $
YLOG = ylog, $
YNOZERO = ynozero, $
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

    dims = size(x, /DIMENSIONS)
    setDefaultValue, dimension, 0
    setDefaultValue, refresh, 1, /BOOLEAN
    nx = n_elements(x)
    ny = n_elements(y)

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
;Allocate Memory to Pointers /////////////////////////////////////////
;---------------------------------------------------------------------

    self.indep = ptr_new(/ALLOCATE_HEAP)
    self.dep = ptr_new(/ALLOCATE_HEAP)
    self.min_value = ptr_new(/ALLOCATE_HEAP)
    self.max_value = ptr_new(/ALLOCATE_HEAP)
    self.nsum = ptr_new(/ALLOCATE_HEAP)
    self.xlog = ptr_new(/ALLOCATE_HEAP)
    self.ylog = ptr_new(/ALLOCATE_HEAP)
    self.polar = ptr_new(/ALLOCATE_HEAP)
    self.symcolor = ptr_new(/ALLOCATE_HEAP)
    self.ynozero = ptr_new(/ALLOCATE_HEAP)
    
    self.target = obj_new()

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
;Dependent and Independent Variables /////////////////////////////////
;---------------------------------------------------------------------

    ;Figure out the dependent variable
    if ny eq 0 $
        then dep = x $
        else dep = y
    
    ;Make the independent variable index the chosen DIMENSION of y.
    if ny eq 0 then begin
        if dimension eq 0 $
            then indep = lindgen(n_elements(dep)) $
            else indep = lindgen(dims[dimension])
    
    ;The independent variable was given.
    endif else begin
        dims = size(y, /dimensions)
        indep = x
    endelse
    
    ;Make sure arrays were given, not scalars
    if n_elements(indep) eq 1 then indep = [indep]
    if n_elements(dep) eq 1 then dep = [dep]

    ;The dimension not being plotted.
    case dimension of
        0: xdim = 0
        1: xdim = 2
        2: xdim = 1
    endcase
        
    ;Number of defaults to use.
    if xdim eq 0 then nDefaults = 1 else nDefaults = dims[xdim-1]
    
;---------------------------------------------------------------------
;Normal Plot? ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Set the initial x- and y-range
    if n_elements(xrange) eq 0 then xrange = [min(indep, max=maxIndep), maxIndep]
    if n_elements(yrange) eq 0 then begin
        yrange = [min(dep, max=maxdep), maxdep]
        yrange += [-abs(yrange[0]), abs(yrange[1])]*0.05
    endif
    self.init_xrange = xrange
    self.init_yrange = yrange

    ;Pick a set of default colors so not everything is the same color.
    default_colors = ['opposite', 'Blue', 'Forest_Green', 'Red', 'Magenta', 'Orange']
    if nDefaults gt 5 then default_colors = [default_colors, replicate('opposite', nDefaults-5)]
    if nDefaults eq 1 then d_color = default_colors[0] else d_color = default_colors[1:nDefaults]
    SetDefaultValue, color, d_color

    ;Set the data
    self -> SetData, indep, dep

    ;Set the object properties
    self -> SetProperty, COLOR = color, $
                         DIMENSION = dimension, $
                         LABEL = label, $
                         MAX_VALUE = max_value, $
                         MIN_VALUE = min_value, $
                         NSUM = nsum, $
                         POLAR = polar, $
                         SYMCOLOR = symcolor, $
                         XLOG = keyword_set(xlog), $
                         XRANGE = xrange, $
                         YLOG = keyword_set(ylog), $
                         YNOZERO = ynozero, $
                         YRANGE = yrange
    
    ;Overplot?
    if n_elements(target) gt 0 then begin
        if size(target, /TNAME) eq 'OBJREF' $
            then self -> Overplot, target $
            else if keyword_set(target) then self -> Overplot
    endif

    ;Make sure the x- and y-style keywords have the 2^0 bit set to force
    ;exact axis ranges.    
    if n_elements(*self.xstyle) eq 0 $
        then *self.xstyle = 1 $
        else *self.xstyle += ~(*self.xstyle and 1)
        
    if n_elements(*self.ystyle) eq 0 $
        then *self.ystyle = 1 $
        else *self.ystyle += ~(*self.ystyle and 1)

    ;Refresh the graphics?
    if keyword_set(current) $
        then theWin -> Refresh, DISABLE=~init_refresh $
        else self.window -> Draw

    return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrPlot__define, class
    compile_opt idl2
    
    class = {MrPlot, $
             ;Data Properties
             indep: ptr_new(), $               ;independent variable
             dep: ptr_new(), $                 ;dependent variable
             
             ;Graphics Properties
             max_value: ptr_new(), $           ;maximum value displayed in plot
             min_value: ptr_new(), $           ;minimum value displayed in plot
             xlog: ptr_new(), $                ;log-scale the x-axis?
             ylog: ptr_new(), $                ;log-scale the y-axis?
             polar: ptr_new(), $               ;create a polar plot?
             ynozero: ptr_new(), $             ;do not make ymin=0
             nsum: ptr_new(), $                ;*number of points to average when plotting
             
             ;cgPlot Properties
             overplot: 0B, $                   ;Overplot?
             symcolor: ptr_new(), $            ;color of each symbol
             target: obj_new(), $              ;Overplot target
             label: '', $                      ;*
             
             ;MrPlot Properties
             dimension: 0, $                   ;The over which plots will be made
             init_xrange: dblarr(2), $         ;Initial y-range
             init_yrange: dblarr(2), $         ;Initial x-range
             
             ;Inheritances -- At the bottom so that IDL calls MrPlot's methods before
             ;                it calls the superclass methods.
             inherits MrGrAtom, $
             inherits MrLayout, $
             inherits weGraphicsKeywords $
            }
end