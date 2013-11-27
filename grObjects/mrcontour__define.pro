; docformat = 'rst'
;
; NAME:
;       MrContour__Define
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
;   The purpose of this method is to create an object out of the cgContour routine.
;
; :Example:
;   Reproduce the "Filled Contour Plot" example in the 
;   `Coyote Graphics Gallery <http://www.idlcoyote.com/gallery/>`::
;
;       data = cgDemoData(26)
;       minValue = Floor(Min(data))
;       maxValue = Ceil(Max(data))
;       nLevels = 10
;       xtitle = 'X Axis'
;       ytitle = 'Y Axis'
;       position =   [0.125, 0.125, 0.9, 0.800]
;       cbposition = [0.125, 0.865, 0.9, 0.895]
;       cbTitle = 'Data Value'
;       cgLoadCT, 33, NColors=nlevels, Bottom=1, CLIP=[30,255]
;       contourLevels = cgConLevels(data, NLevels=10, MinValue=minValue)
;
;       filledCon = obj_new('MrContour', data, /FILL, LEVELS=contourLevels, $
;                           C_COLORS=bindgen(nlevels)+1B, /OUTLINE, POSITION=position, $
;                           XTITLE=xtitle, YTITLE=ytitle, DRAW=0)
;                       
;       conCB = obj_new('weColorbar', NColors=nlevels, Bottom=1, Position=cbposition, $
;                       Range=[MinValue, MaxValue], Divisions=nlevels, /Discrete, $
;                       Title=cbTitle, TLocation='Top')
;   
;       filledCon -> Add, conCB
;       filledCon -> Draw
;       
;       obj_destroy, filledCon
;       obj_destroy, conCB
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
;       08/20/2013  -   Written by Matthew Argall
;       08/23/2013  -   Added the NOERASE keyword to Draw. - MRA
;       08/24/2013  -   Added init_[xy]range and [pxy]_sysvar properties. X and Y
;                           coordinates, if not provided, are made to index the dimensions
;                           of DATA. [XY]RANGE is defined and [XY]STYLE is made to have
;                           the 2^0 bit set. - MRA
;       09/26/2013  -   Added the GRAPHIC keyword. Removed the contour method. - MRA
;       2013/11/17  -   CHARSIZE is now a MrGraphicAtom property. Use _EXTRA instead of
;                           _STRICT_EXTRA in some cases to make setting and getting
;                           properties easier and to reduce list of keywords. Renamed
;                           GRAPHIC to TARGET to match IDL v8.0+ - MRA
;       2013/11/20  -   Disinherit MrIDL_Container. Add NAME property. - MRA
;       2013/11/20  -   MrIDL_Container and MrGraphicAtom is disinherited. Inherit instead
;                           MrGrAtom and MrLayout. - MRA
;       2013/11/22  -   Renamed DRAW to REFRESH. Refreshing is now done automatically.
;                           Call the Refresh method with the DISABLE keyword set to
;                           temporarily turn of Refresh. - MRA
;       2013/11/25  -   Added the GetData, SetData, GetOverplot, Overplot, and SetLayout
;                           methods. cgContour throws an error if MAP_OBJECT is an invalid
;                           object reference. Fixed by changing the class property from
;                           an object to a pointer. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to draw the plot in the draw window. The plot is
;   first buffered into the pixmap for smoother opteration (by allowing motion events
;   to copy from the pixmap instead of redrawing the plot, the image does not flicker).
;-
pro MrContour::Draw, $
NOERASE=noerase, $
OLEVELS=oLevels, $
PATH_INFO=path_info, $
PATH_XY=path_xy
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    GetOLevels = Arg_Present(oLevels)
    GetPath_Info = Arg_Present(path_info)
    GetPath_XY = Arg_Present(path_xy)
    
    ;Get path information without drawing anything
    if GetPath_Info || GetPath_XY then begin
        self -> doContour, NOERASE=noerase, $
                           OLEVELS=oLevels, $
                           PATH_INFO=path_info, $
                           PATH_XY=path_xy
    endif
    
    ;Return if we are hiding
    if self.hide then return
    
    ;Overplot?
    if obj_valid(self.overplot) then begin
        self.target -> RestoreCoords
        self -> doContour, NOERASE=noerase, $
                           OLEVELS=oLevels, $
                           /OVERPLOT
        self -> SaveCoords

    ;Normal contour?
    endif else begin
        self -> doContour, NOERASE=noerase, $
                           OLEVELS=oLevels
        self -> SaveCoords
    endelse
end


;+
;   The purpose of this method is to do the actual plotting. Basically, having this here
;   merely to saves space in the Draw method.
;-
pro MrContour::doContour, $
NOERASE=noerase, $
OLEVELS=oLevels, $
PATH_INFO=path_info, $
PATH_XY=path_xy

    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    GetPath_Info = Arg_Present(path_info)
    GetPath_XY = Arg_Present(path_xy)

    if n_elements(noerase) eq 0 then noerase = *self.noerase

;-----------------------------------------------------
;Draw the Contour Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;
    ; PATH_INFO and PATH_XY both suppress output. If neither are desired, call
    ; cgContour without them.
    ;

    if GetPath_Info eq 0 && GetPath_XY eq 0 then begin
        cgContour, *self.c_data, *self.xcoords, *self.ycoords, $
                
                   ;cgContour Keywords
;                   ASPECT           = *self.aspect, $
                   LABEL            =  self.label, $
;                   LAYOUT           = *self.layout, $
                   MAP_OBJECT       =  *self.map_object, $
                   OLEVELS          =       oLevels, $
                   ONIMAGE          =  self.onImage, $
                   OUTCOLOR         = *self.outcolor, $
                   OUTFILENAME      =  self.outfilename, $
                   OUTLINE          =  self.outline, $
                   OUTPUT           =  self.output, $
                   OVERPLOT         =  self.overplot, $
                   PALETTE          = *self.palette, $
                   TRADITIONAL      =  self.traditional, $
    
                   ;Contour Keywords
                   C_ANNOTATION     = *self.c_annotation, $
                   C_CHARSIZE       = *self.c_charsize, $
                   C_CHARTHICK      = *self.c_charthick, $
                   C_COLORS         = *self.c_colors, $
                   C_LABELS         = *self.c_labels, $
                   C_LINESTYLE      = *self.c_linestyle, $
                   C_ORIENTATION    = *self.c_orientation, $
                   C_SPACING        = *self.c_spacing, $
                   C_THICK          = *self.c_thick, $
                   CELL_FILL        = *self.cell_fill, $
                   CLOSED           = *self.closed, $
                   DOWNHILL         = *self.downhill, $
                   FILL             = *self.fill, $
                   FOLLOW           = *self.follow, $
                   IRREGULAR        = *self.irregular, $
                   ISOTROPIC        = *self.isotropic, $
                   LEVELS           = *self.levels, $
                   NLEVELS          = *self.nlevels, $
                   MAX_VALUE        = *self.max_value, $
                   MIN_VALUE        = *self.min_value, $
                   MISSINGVALUE     = *self.missingvalue, $
                   PATH_DATA_COORDS = *self.path_data_coords, $
                   PATH_DOUBLE      = *self.path_double, $
                   PATH_FILENAME    = *self.path_filename, $
                   RESOLUTION       = *self.resolution, $
                   TRIANGULATION    = *self.triangulation, $
                   XLOG             = *self.xlog, $
                   YLOG             = *self.ylog, $
                   
                   ;MrGraphicsAtom Keywords
                   POSITION      = self.position, $
              
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
                   NOERASE       = noerase, $
;                   PSYM          = *self.psym, $
                   SUBTITLE      = *self.subtitle, $
;                   SYMSIZE       = *self.symsize, $
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
    ;               YTICK_GET     = *self.ytick_get, $
                   YTICKFORMAT   = *self.ytickformat, $
    ;               YTICKINTERVAL = *self.ytickinterval, $
    ;               YTICKLAYOUT   = *self.yticklayout, $
    ;               YTICKLEN      = *self.yticklen, $
    ;               YTICKNAME     = *self.ytickname, $
    ;               YTICKS        = *self.yticks, $
    ;               YTICKUNITS    = *self.ytickunits, $
    ;               YTICKV        = *self.ytickv, $
                   YTITLE        = *self.ytitle;, $
    ;               ZCHARSIZE     = *self.zcharsize, $
    ;               ZGRIDSTYLE    = *self.zgridstyle, $
    ;               ZMARGIN       = *self.zmargin, $
    ;               ZMINOR        = *self.zminor, $
    ;               ZRANGE        = *self.zrange, $
    ;               ZSTYLE        = *self.zstyle, $
    ;               ZTHICK        = *self.zthick, $
    ;               ZTICK_GET     = *self.ztick_get, $
    ;               ZTICKFORMAT   = *self.ztickformat, $
    ;               ZTICKINTERVAL = *self.ztickinterval, $
    ;               ZTICKLAYOUT   = *self.zticklayout, $
    ;               ZTICKLEN      = *self.zticklen, $
    ;               ZTICKNAME     = *self.ztickname, $
    ;               ZTICKS        = *self.zticks, $
    ;               ZTICKUNITS    = *self.ztickunits, $
    ;               ZTICKV        = *self.ztickv, $
    ;               ZTITLE        = *self.ztitle, $
    ;               ZVALUE        = *self.zvalue
    
        ;Collect the contour information
        if n_elements(oLevels) gt 0 then *self.oLevels = oLevels
        return
    endif

;-----------------------------------------------------
;PATH_INFO and PATH_XY Suppress Output \\\\\\\\\\\\\\\
;-----------------------------------------------------

    cgContour, *self.c_data, *self.xcoords, *self.ycoords, $
            
               ;cgContour Keywords
               ASPECT           = *self.aspect, $
               LABEL            =  self.label, $
               LAYOUT           = *self.layout, $
               MAP_OBJECT       =  self.map_object, $
               OLEVELS          =       oLevels, $
               ONIMAGE          =  self.onImage, $
               OUTCOLOR         = *self.outcolor, $
               OUTFILENAME      =  self.outfilename, $
               OUTLINE          =  self.outline, $
               OUTPUT           =  self.output, $
               OVERPLOT         =  self.overplot, $
               PALETTE          = *self.palette, $
               TRADITIONAL      =  self.traditional, $

               ;Contour Keywords
               C_ANNOTATION     = *self.c_annotation, $
               C_CHARSIZE       = *self.c_charsize, $
               C_CHARTHICK      = *self.c_charthick, $
               C_COLORS         = *self.c_colors, $
               C_LABELS         = *self.c_labels, $
               C_LINESTYLE      = *self.c_linestyle, $
               C_ORIENTATION    = *self.c_orientation, $
               C_SPACING        = *self.c_spacing, $
               C_THICK          = *self.c_thick, $
               CELL_FILL        = *self.cell_fill, $
               CLOSED           = *self.closed, $
               DOWNHILL         = *self.downhill, $
               FILL             = *self.fill, $
               FOLLOW           = *self.follow, $
               IRREGULAR        = *self.irregular, $
               ISOTROPIC        = *self.isotropic, $
               LEVELS           = *self.levels, $
               NLEVELS          = *self.nlevels, $
               MAX_VALUE        = *self.max_value, $
               MIN_VALUE        = *self.min_value, $
               MISSINGVALUE     = *self.missingvalue, $
               PATH_DATA_COORDS = *self.path_data_coords, $
               PATH_DOUBLE      = *self.path_double, $
               PATH_FILENAME    = *self.path_filename, $
               RESOLUTION       = *self.resolution, $
               TRIANGULATION    = *self.triangulation, $
               PATH_INFO        =       path_info, $
               PATH_XY          =       path_xy, $
               XLOG             = *self.xlog, $
               YLOG             = *self.ylog, $
          
               ;weGraphicsKeywords
               AXISCOLOR     = *self.axiscolor, $
               BACKGROUND    = *self.background, $
               CHARSIZE      = *self.charsize, $
               CHARTHICK     = *self.charthick, $
               CLIP          = *self.clip, $
               COLOR         = *self.color, $
               DATA          = *self.data, $
               DEVICE        = *self.device, $
               NORMAL        = *self.normal, $
               FONT          = *self.font, $
               NOCLIP        = *self.noclip, $
               NODATA        = *self.nodata, $
               NOERASE       = *self.noerase, $
               POSITION      = *self.position, $
;               PSYM          = *self.psym, $
               SUBTITLE      = *self.subtitle, $
;               SYMSIZE       = *self.symsize, $
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
               YTICKFORMAT   = *self.ytickformat;, $
;               YTICKINTERVAL = *self.ytickinterval, $
;               YTICKLAYOUT   = *self.yticklayout, $
;               YTICKLEN      = *self.yticklen, $
;               YTICKNAME     = *self.ytickname, $
;               YTICKS        = *self.yticks, $
;               YTICKUNITS    = *self.ytickunits, $
;               YTICKV        = *self.ytickv, $
;               YTITLE        = *self.ytitle, $
;               ZCHARSIZE     = *self.zcharsize, $
;               ZGRIDSTYLE    = *self.zgridstyle, $
;               ZMARGIN       = *self.zmargin, $
;               ZMINOR        = *self.zminor, $
;               ZRANGE        = *self.zrange, $
;               ZSTYLE        = *self.zstyle, $
;               ZTHICK        = *self.zthick, $
;               ZTICK_GET     = *self.ztick_get, $
;               ZTICKFORMAT   = *self.ztickformat, $
;               ZTICKINTERVAL = *self.ztickinterval, $
;               ZTICKLAYOUT   = *self.zticklayout, $
;               ZTICKLEN      = *self.zticklen, $
;               ZTICKNAME     = *self.ztickname, $
;               ZTICKS        = *self.zticks, $
;               ZTICKUNITS    = *self.ztickunits, $
;               ZTICKV        = *self.ztickv, $
;               ZTITLE        = *self.ztitle, $
;               ZVALUE        = *self.zvalue
    
    if n_elements(oLevels)   gt 0 then *self.oLevels = oLevels
    if n_elements(path_info) gt 0 then *self.path_info = path_info
    if n_elements(path_xy)   gt 0 then *self.path_xy = path_xy

end


;+
;   The purpose of this method is to retrieve data
;
; :Calling Sequence:
;       myPlot -> GetData, z
;       myPlot -> GetData, z, x, y
;
; :Params:
;       Z:              in, required, type=numeric array
;                       A one- or two-dimensional array containing the values that make
;                           up the contour surface.
;       X:              in, optional, type=numeric array
;                       A vector or two-dimensional array specifying the X coordinates
;                           for the contour surface.
;       Y:              in, optional, type=numeric array
;                       A vector or two-dimensional array specifying the Y coordinates
;                           for the contour surface.
;-
pro MrContour::GetData, z, x, y
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Retrieve the data
    case n_params() of
        1: z = *self.c_data
        3: begin
            z = *self.c_data
            x = *self.xcoords
            y = *self.ycoords
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
function MrContour::GetOverplot, target
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
;   The purpose of this method is to set object properties. 
;
; :Keywords:
;       ASPECT:         out, optional, type=float
;                       Set this keyword to a floating point ratio that represents the aspect ratio 
;                           (ysize/xsize) of the resulting plot. The plot position may change as a result
;                           of setting this keyword. Note that `Aspect` cannot be used when plotting with
;                           !P.MULTI.
;       AXISCOLOR:      out, optional, type=string/integer
;                       If this keyword is a string, the name of the axis color. 
;                           Otherwise, the keyword is assumed to be a color index into the current color table.
;       AXESCOLOR:      out, optional, type=string/integer
;                       Provisions for bad spellers.
;       BACKGROUND:     out, optional, type=string/integer
;                       If this keyword is a string, the name of the background color. 
;                           Otherwise, the keyword is assumed to be a color index into the current color table.
;       C_ANNOTATION:   out, optional, type=string
;                       The label to be drawn on each contour. Normally contours are labeled with their
;                           value. This vector of strings may substitute for those values.
;       C_CHARSIZE:     out, optional, type=float
;                       The character size of the annotations used on the contour lines themselves.
;                           By default, 75% of `Charsize`.
;       C_CHARTHICK:    out, optional, type=integer
;                       The thickness of the characters used to annotate contour labels.
;       C_COLORS:       out, optional, type=integer/string vector
;                       Set to the index values of the contour colors or to named colors. Must contain
;                           the same number of colors as the number of requested contour levels.
;       C_LABELS:       out, optional, type=integer
;                       A vector that specifies which contour levels to label. If used, the LABEL
;                           keyword is ignored.
;       C_LINESTYLE:    out, optional, type=integer/intarr
;                       The line style used to draw each contour (cyclical).
;       C_ORIENTATION:  out, optional, type=float
;                       If the `FILL` or `CELL_FILL` keyword is set, this keyword can be set
;                           to the angle, in degrees counterclockwise from the horizontal,
;                           of the lines used to fill contours. If neither `C_ORIENTATION`
;                           nor `C_SPACING` are specified, the contours are solid filled.
;       C_SPACING:      out, optional, type=float
;                       If the `FILL` or `CELL_FILL` keyword is set, this keyword can be
;                           used to control the distance, in centimeters, between the lines
;                           used to fill the contours.
;       C_THICK:        out, optional, type=fltarr
;                       The line used to draw each contour level (cyclical).
;       CELL_FILL:      out, optional, type=boolean
;                       Set to indicate filled contours should be created using the "cell fill" method.
;                           This keyword should always be set if displaying filled contours on map projections
;                           or if missing data is present in the data you are contouring.
;       CLOSED:         out, optional, type=boolean
;                       Close contours that intersect the plot boundaries. Set CLOSED=0
;                           along with `PATH_INFO` and/or `PATH_XY` to return path
;                           information for contours that are not closed.
;       COLOR:          out, optional, type=string/integer
;                       If this keyword is a string, the name of the data color. By default, same as AXISCOLOR.
;                           Otherwise, the keyword is assumed to be a color index into the current color table.
;       FILL:           out, optional, type=boolean
;                       Set to indicate filled contours should be created.
;       IRREGULAR:      out, optional, type=boolean
;                       If this keyword is set, the data, x, and y input parameters are taken to be
;                           irregularly gridded data, the the data is gridded for use in the contour plot
;                           using the Triangulate and Trigrid method. The resolution of the gridded output
;                           is set by the RESOLUTION keyword.
;       ISOTROPIC:      out, optional, type=boolean
;                       Force the scaling of the X and Y axes to be equal.
;       LABEL:          out, optional, type=integer
;                       An number that tells how to label contour levels. A 0 means
;                           no contour levels are labelled. A 1 (the default) means all contour levels are
;                           labelled. A 2 means label every 2nd contour level is labelled. A 3 means every 
;                           3rd contour level is labelled, and so on.
;       LAYOUT:         out, optional, type=intarr(3)
;                       This keyword specifies a grid with a graphics window and determines where the
;                           graphic should appear. The syntax of LAYOUT is three numbers: [ncolumns, nrows, location].
;                           The grid is determined by the number of columns (ncolumns) by the number of 
;                           rows (nrows). The location of the graphic is determined by the third number. The
;                           grid numbering starts in the upper left (1) and goes sequentually by column and then
;                           by row.
;       LEVELS:         out, optional, type=any
;                       A vector of data levels to contour. If used, NLEVELS is ignored. If missing, 
;                           NLEVELS is used to construct N equally-spaced contour levels.
;       MAP_OBJECT:     out, optional, type=object
;                       If you are overplotting (OVERPLOT=1) on a map projection set up with Map_Proj_Init
;                           and using projected meter space, rather than lat/lon space, then you can use this
;                           keyword to provide a cgMap object that will allow you to convert the `x` and `y`
;                           grid parameters from longitude and latitude, respectively, to projected meter space
;                           before the contour is displayed. Note, you MUST pass the `x` and `y` grid parameters 
;                           to cgContour if you are overplotting on a map projection. There is no checking to
;                           be sure these parameters are in the correct longitude and latitude range, respectively.
;       MAX_VALUE:      out, optional, type=any
;                       Data points with values above this value are ignored.
;       MIN_VALUE:      out, optional, type=any
;                       Data points with values below this value are ignored.
;       MISSINGVALUE:   out, optional, type=any
;                       Use this keyword to identify any missing data in the input data values.
;       NAME:           out, optional, type=string
;                       Name of the graphic.
;       NLEVELS:        out, optional, type=integer
;                       If the Contour plot LEVELS keyword is not used, this keyword will produce this
;                           number of equally spaced contour intervals. Unlike the Contour NLEVELS keyword,
;                           this keyword actually works!
;       OLEVELS:        out, optional
;                       Set to a named variable to return the actual contour levels used in the program.
;                           Unfortunately, output variables cannot be returned if the cgContour command is
;                           being executed in a cgWindow.
;       ONIMAGE:        out, optional, type=boolean
;                       If this keyword is set, and an image has been display previously with cgImage,
;                           then the contour plot will determine the location of the image in the display
;                           window and overplot itself onto that image.
;       OUTCOLOR:       out, optional, type=string
;                       The color of the contour lines when the `Outline` keyword is used.
;       OUTFILENAME:    out, optional, type=string
;                       If the `Output` keyword is set, the user will be asked to supply an output
;                           filename, unless this keyword is set to a non-null string. In that case, the
;                           value of this keyword will be used as the filename and there will be no dialog
;                           presented to the user.
;       OUTLINE:        out, optional, type=boolean
;                       This keyword applies only if the `Fill` keyword is set. It will draw the
;                           contour lines on top of the filled contour. It draws the outline in the `OutColor`.
;       OUTPUT:         out, optional, type=string
;                       Set this keyword to the type of output desired. Possible values are these::
;            
;                           'PS'   - PostScript file
;                           'EPS'  - Encapsulated PostScript file
;                           'PDF'  - PDF file
;                           'BMP'  - BMP raster file
;                           'GIF'  - GIF raster file
;                           'JPEG' - JPEG raster file
;                           'PNG'  - PNG raster file
;                           'TIFF' - TIFF raster file
;            
;                       Or, you can simply set this keyword to the name of the output file, and the type of
;                           file desired will be determined by the file extension. If you use this option, the
;                           user will not be prompted to supply the name of the output file.
;            
;                           All raster file output is created through PostScript intermediate files (the
;                           PostScript files will be deleted), so ImageMagick and Ghostview MUST be installed 
;                           to produce anything other than PostScript output. (See cgPS2PDF and PS_END for 
;                           details.) And also note that you should NOT use this keyword when doing multiple 
;                           plots. The keyword is to be used as a convenient way to get PostScript or raster 
;                           output for a single graphics command. Output parameters can be set with cgWindow_SetDefs.
;       PALETTE:        out, optional, type=byte
;                       A (256x3) color palette containing the RGB color vectors to use for coloring contours.
;                           Contour colors will be sampled from the color table palette into the number 
;                           of contour levels required. If the palette is NOT 256 elements in length, then
;                           it is assumed that the length corresponds to the number of levels to be contoured.
;    PATH_DATA_COORDS:  out, optional, type=boolean
;                       indicate that the `PATH_FILENAME`, `PATH_INFO`, and `PATH_XY` 
;                           keywords should return  vertex and contour value information
;                           as doubles
;       PATH_FILENAME:  out, optional, type=boolean
;                       Specifies the name of a file to contain the contour positions.
;       PATH_INFO:      out, optional, type=array of structures
;                       Set this keyword to a named variable that will return path
;                           information for the contours.
;       PATH_XY:        out, optional, type=fltarr
;                       Set this keyword to a named variable that returns the coordinates
;                           of a set of closed polygons defining the closed paths of the
;                           contours
;       RESOLUTION:     out, optional, type=integer array
;                       If the IRREGULAR keyword is set, this keyword specifies the X and Y resolution
;                           in a two element integer array of the final gridded data that is sent to the 
;                           contour plot.
;       TRADITIONAL:    out, optional, type=boolean
;                       If this keyword is set, the traditional color scheme of a black background for
;                            graphics windows on the display is used and PostScript files always use a white background.
;       _REF_EXTRA:     out, optional, type=any
;                       Keyword accepted by the superclasses are also accepted for
;                           keyword inheritance.
;-
pro MrContour::GetProperty, $
;MrContour Properties
TARGET=target, $

;cgContour Properties
AXISCOLOR=axiscolor, $
AXESCOLOR=axescolor, $
BACKGROUND=sbackground, $
LABEL=label, $
MAP_OBJECT=map_object, $
MISSINGVALUE=missingvalue, $
OLEVELS=olevels, $
ONIMAGE=onImage, $
OUTCOLOR=outcolor, $
OUTFILENAME=outfilename, $
OUTLINE=outline, $
OUTPUT=output, $
PALETTE=palette, $
TRADITIONAL=traditional, $

;Contour Properties
C_ANNOTATION=c_annotation, $
C_CHARSIZE=c_charsize, $
C_CHARTHICK=c_charthick, $
C_COLORS=c_colors, $
C_LABELS=c_labels, $
C_LINESTYLE=c_linestyle, $
C_ORIENTATION=c_orientation, $
C_SPACING=c_spacing, $
C_THICK=c_thick, $
CELL_FILL=cell_fill, $
CLOSED=closed, $
DOWNHILL=downhill, $
FILL=fill, $
FOLLOW=follow, $
IRREGULAR=irregular, $
ISOTROPIC=isotropic, $
LEVELS=levels, $
NLEVELS=nlevels, $
MAX_VALUE=max_value, $
MIN_VALUE=min_value, $
PATH_DATA_COORDS=path_data_coords, $
PATH_DOUBLE=path_double, $
PATH_FILENAME=path_filename, $
PATH_INFO=path_info, $
PATH_XY=path_xy, $
RESOLUTION=resolution, $
TRIANGULATION=triangulation, $
XLOG=xlog, $
YLOG=ylog, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;cgContour Properties
    if arg_present(axiscolor)     ne 0 then axiscolor     = *self.axiscolor
    if arg_present(axescolor)     ne 0 then axescolor     = *self.axiscolor
    if arg_present(background)    ne 0 then background    = *self.background
    if arg_present(label)         ne 0 then label         =  self.label
    if arg_present(olevels)       ne 0 then olevels       = *self.olevels
    if arg_present(onimage)       ne 0 then onimage       =  self.onimage
    if arg_present(outcolor)      ne 0 then outcolor      = *self.outcolor
    if arg_present(outfilename)   ne 0 then outfilename   =  self.outfilename
    if arg_present(outline)       ne 0 then outline       =  self.outline
    if arg_present(output)        ne 0 then output        =  self.output
    if arg_present(palette)       ne 0 then palette       = *self.palette
    if arg_present(traditional)   ne 0 then traditional   =  self.traditional
    
    ;Contour Properties
    if arg_present(c_annotation)  ne 0 then c_annotation  = *self.c_annotation
    if arg_present(c_charsize)    ne 0 then c_charsize    = *self.c_charsize
    if arg_present(c_charthick)   ne 0 then c_charthick   = *self.c_charthick
    if arg_present(c_colors)      ne 0 then c_colors      = *self.c_colors
    if arg_present(c_labels)      ne 0 then c_labels      = *self.c_labels
    if arg_present(c_linestyle)   ne 0 then c_linestyle   = *self.c_linestyle
    if arg_present(c_orientation) ne 0 then c_orientation = *self.c_orientation
    if arg_present(c_spacing)     ne 0 then c_spacing     = *self.c_spacing
    if arg_present(c_thick)       ne 0 then c_thick       = *self.c_thick
    if arg_present(cell_fill)     ne 0 then cell_fill     = *self.cell_fill
    if arg_present(fill)          ne 0 then fill          = *self.fill
    if arg_present(closed)        ne 0 then closed        = *self.closed
    if arg_present(downhill)      ne 0 then downhill      = *self.downhill
    if arg_present(follow)        ne 0 then follow        = *self.follow
    if arg_present(irregular)     ne 0 then irregular     = *self.irregular
    if arg_present(isotropic)     ne 0 then isotropic     = *self.isotropic
    if arg_present(levels)        ne 0 then levels        = *self.levels
    if arg_present(max_value)     ne 0 then max_value     = *self.max_value
    if arg_present(min_value)     ne 0 then min_value     = *self.min_value
    if arg_present(missingvalue)  ne 0 then missingvalue  = *self.missingvalue
    if arg_present(nlevels)       ne 0 then nlevels       = *self.nlevels
    if arg_present(path_filename) ne 0 then path_filename = *self.path_filename
    if arg_present(path_info)     ne 0 then path_info     = *self.path_info
    if arg_present(path_xy)       ne 0 then path_xy       = *self.path_xy
    if arg_present(triangulation) ne 0 then triangulation = *self.triangulation
    if arg_present(path_double)   ne 0 then path_double   = *self.path_double
    if arg_present(resolution)    ne 0 then resolution    = *self.resolution
    if arg_present(xlog)          ne 0 then xlog          =  self.xlog
    if arg_present(ylog)          ne 0 then ylog          =  self.ylog
    if arg_present(path_data_coords)   ne 0 then path_data_coords = *self.path_data_coords

    ;Objects
    if arg_present(map_object) ne 0 then if obj_valid(self.map_object) $
        then map_object = self.map_object $
        else map_object = obj_new()
    
    ;Superclass properties
    if n_elements(extra) ne 0 then begin
        self -> MrLayout::GetProperty, _EXTRA=extra
        self -> MrGrAtom::GetProperty, _EXTRA=extra
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
pro MrContour::Overplot, target, $
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
        self.window -> SetPosition, (*self.layout)[2:*], location
        self.overplot = 0B
        self.target = obj_new()

    ;Enable overplotting        
    endif else begin
        ;If not TARGET was specified, get the currently selected graphic
        if n_elements(target) eq 0 then target = self.window -> GetSelect()

        ;Make we can overplot on top of the target graphic
        oplottable = ['MrPlot', 'MrImage', 'MrContour']
        if IsMember(oplottable, obj_class(target), /FOLD_CASE) eq 0 || obj_valid(target) eq 0 $
            then message, 'TARGET must be valid and of class ' + strjoin(oplottable, ' ')

        ;Get the position
        target -> GetProperty, POSITION=position
        
        ;Remove SELF from layout.
        self.window -> SetPosition, (*self.layout)[2:*], position
        self.overplot = 1B
        self.target = target
    endelse
    
    ;Re-enable refreshing
    self.window -> Refresh, DISABLE=~init_refresh
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
pro MrContour::SetLayout, layout, $
POSITION=position, $
UPDATE_LAYOUT=update_layout, $
_REF_EXTRA=extra
    compile_opt idl2
    
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
            then self.window -> SetPosition, (*self.layout)[2:*], position $
            else self.window -> SetPosition, (*self.layout)[2:*], layout
        
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
;   The purpose of this method is to retrieve data
;
; :Calling Sequence:
;       myGraphic -> GetData, z
;       myGraphic -> GetData, z, x, y
;
; :Params:
;       Z:              in, required, type=numeric array
;                       A one- or two-dimensional array containing the values that make
;                           up the contour surface.
;       X:              in, optional, type=numeric array
;                       A vector or two-dimensional array specifying the X coordinates
;                           for the contour surface.
;       Y:              in, optional, type=numeric array
;                       A vector or two-dimensional array specifying the Y coordinates
;                           for the contour surface.
;-
pro MrContour::SetData, z, x, y
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
        1: *self.c_data = z
        3: begin
            *self.c_data = z
            *self.xcoords = x
            *self.ycoords = y
        endcase
        else: message, 'Incorrect number of parameters.'
    endcase
    
    self.window -> draw
end


;+
;   The purpose of this method is to set object properties. 
;
; :Keywords:
;       AXISCOLOR:      in, optional, type=string/integer
;                       If this keyword is a string, the name of the axis color. 
;                           Otherwise, the keyword is assumed to be a color index into the current color table.
;       AXESCOLOR:      in, optional, type=string/integer
;                       Provisions for bad spellers.
;       BACKGROUND:     in, optional, type=string/integer
;                       If this keyword is a string, the name of the background color. 
;                           Otherwise, the keyword is assumed to be a color index into the current color table.
;       C_ANNOTATION:   in, optional, type=string
;                       The label to be drawn on each contour. Normally contours are labeled with their
;                           value. This vector of strings may substitute for those values.
;       C_CHARSIZE:     in, optional, type=float
;                       The character size of the annotations used on the contour lines themselves.
;                           By default, 75% of `Charsize`.
;       C_CHARTHICK:    in, optional, type=integer
;                       The thickness of the characters used to annotate contour labels.
;       C_COLORS:       in, optional, type=integer/string vector
;                       Set to the index values of the contour colors or to named colors. Must contain
;                           the same number of colors as the number of requested contour levels.
;       C_LABELS:       in, optional, type=integer
;                       A vector that specifies which contour levels to label. If used, the LABEL
;                           keyword is ignored.
;       C_LINESTYLE:    in, optional, type=integer/intarr
;                       The line style used to draw each contour (cyclical).
;       C_ORIENTATION:  in, optional, type=float
;                       If the `FILL` or `CELL_FILL` keyword is set, this keyword can be set
;                           to the angle, in degrees counterclockwise from the horizontal,
;                           of the lines used to fill contours. If neither `C_ORIENTATION`
;                           nor `C_SPACING` are specified, the contours are solid filled.
;       C_SPACING:      in, optional, type=float
;                       If the `FILL` or `CELL_FILL` keyword is set, this keyword can be
;                           used to control the distance, in centimeters, between the lines
;                           used to fill the contours.
;       C_THICK:        in, optional, type=fltarr
;                       The line used to draw each contour level (cyclical).
;       CELL_FILL:      in, optional, type=boolean
;                       Set to indicate filled contours should be created using the "cell fill" method.
;                           This keyword should always be set if displaying filled contours on map projections
;                           or if missing data is present in the data you are contouring.
;       CLOSED:         in, optional, type=boolean
;                       Close contours that intersect the plot boundaries. Set CLOSED=0
;                           along with `PATH_INFO` and/or `PATH_XY` to return path
;                           information for contours that are not closed.
;       COLOR:          in, optional, type=string/integer
;                       If this keyword is a string, the name of the data color. By default, same as AXISCOLOR.
;                           Otherwise, the keyword is assumed to be a color index into the current color table.
;       FILL:           in, optional, type=boolean
;                       Set to indicate filled contours should be created.
;       IRREGULAR:      in, optional, type=boolean
;                       If this keyword is set, the data, x, and y input parameters are taken to be
;                           irregularly gridded data, the the data is gridded for use in the contour plot
;                           using the Triangulate and Trigrid method. The resolution of the gridded output
;                           is set by the RESOLUTION keyword.
;       ISOTROPIC:      in, optional, type=boolean
;                       Force the scaling of the X and Y axes to be equal.
;       LABEL:          in, optional, type=integer
;                       An number that tells how to label contour levels. A 0 means
;                           no contour levels are labelled. A 1 (the default) means all contour levels are
;                           labelled. A 2 means label every 2nd contour level is labelled. A 3 means every 
;                           3rd contour level is labelled, and so on.
;       LEVELS:         in, optional, type=any
;                       A vector of data levels to contour. If used, NLEVELS is ignored. If missing, 
;                           NLEVELS is used to construct N equally-spaced contour levels.
;       MAP_OBJECT:     in, optional, type=object
;                       If you are overplotting (OVERPLOT=1) on a map projection set up with Map_Proj_Init
;                           and using projected meter space, rather than lat/lon space, then you can use this
;                           keyword to provide a cgMap object that will allow you to convert the `x` and `y`
;                           grid parameters from longitude and latitude, respectively, to projected meter space
;                           before the contour is displayed. Note, you MUST pass the `x` and `y` grid parameters 
;                           to cgContour if you are overplotting on a map projection. There is no checking to
;                           be sure these parameters are in the correct longitude and latitude range, respectively.
;       MAX_VALUE:      in, optional, type=any
;                       Data points with values above this value are ignored.
;       MIN_VALUE:      in, optional, type=any
;                       Data points with values below this value are ignored.
;       MISSINGVALUE:   in, optional, type=any
;                       Use this keyword to identify any missing data in the input data values.
;       NLEVELS:        in, optional, type=integer
;                       If the Contour plot LEVELS keyword is not used, this keyword will produce this
;                           number of equally spaced contour intervals. Unlike the Contour NLEVELS keyword,
;                           this keyword actually works!
;       ONIMAGE:        in, optional, type=boolean
;                       If this keyword is set, and an image has been display previously with cgImage,
;                           then the contour plot will determine the location of the image in the display
;                           window and overplot itself onto that image.
;       OUTCOLOR:       in, optional, type=string
;                       The color of the contour lines when the `Outline` keyword is used.
;       OUTFILENAME:    in, optional, type=string
;                       If the `Output` keyword is set, the user will be asked to supply an output
;                           filename, unless this keyword is set to a non-null string. In that case, the
;                           value of this keyword will be used as the filename and there will be no dialog
;                           presented to the user.
;       OUTLINE:        in, optional, type=boolean
;                       This keyword applies only if the `Fill` keyword is set. It will draw the
;                           contour lines on top of the filled contour. It draws the outline in the `OutColor`.
;       OUTPUT:         in, optional, type=string
;                       Set this keyword to the type of output desired. Possible values are these::
;            
;                           'PS'   - PostScript file
;                           'EPS'  - Encapsulated PostScript file
;                           'PDF'  - PDF file
;                           'BMP'  - BMP raster file
;                           'GIF'  - GIF raster file
;                           'JPEG' - JPEG raster file
;                           'PNG'  - PNG raster file
;                           'TIFF' - TIFF raster file
;            
;                       Or, you can simply set this keyword to the name of the output file, and the type of
;                           file desired will be determined by the file extension. If you use this option, the
;                           user will not be prompted to supply the name of the output file.
;            
;                           All raster file output is created through PostScript intermediate files (the
;                           PostScript files will be deleted), so ImageMagick and Ghostview MUST be installed 
;                           to produce anything other than PostScript output. (See cgPS2PDF and PS_END for 
;                           details.) And also note that you should NOT use this keyword when doing multiple 
;                           plots. The keyword is to be used as a convenient way to get PostScript or raster 
;                           output for a single graphics command. Output parameters can be set with cgWindow_SetDefs.
;       PALETTE:        in, optional, type=byte
;                       A (256x3) color palette containing the RGB color vectors to use for coloring contours.
;                           Contour colors will be sampled from the color table palette into the number 
;                           of contour levels required. If the palette is NOT 256 elements in length, then
;                           it is assumed that the length corresponds to the number of levels to be contoured.
;    PATH_DATA_COORDS:  in, optional, type=boolean
;                       indicate that the `PATH_FILENAME`, `PATH_INFO`, and `PATH_XY` 
;                           keywords should return  vertex and contour value information
;                           as doubles
;       PATH_FILENAME:  in, optional, type=boolean
;                       Specifies the name of a file to contain the contour positions.
;       POSITION:           in, optional, type=fltarr(4)
;                           A vector of the form [x0, y0, x1, y1] specifying the location
;                               of the lower-left and upper-right corners of the graphic,
;                               in normalized coordinates.
;       RESOLUTION:     in, optional, type=integer array
;                       If the IRREGULAR keyword is set, this keyword specifies the X and Y resolution
;                           in a two element integer array of the final gridded data that is sent to the 
;                           contour plot.
;       TRADITIONAL:    in, optional, type=boolean
;                       If this keyword is set, the traditional color scheme of a black background for
;                           graphics windows on the display is used and PostScript files always use a white background.
;       _REF_EXTRA:     in, optional, type=any
;                       Keyword accepted by the superclasses are also accepted for
;                           keyword inheritance.
;-
pro MrContour::SetProperty, $
;cgContour Properties
AXISCOLOR=axiscolor, $
AXESCOLOR=axescolor, $
BACKGROUND=background, $
LABEL=label, $
MAP_OBJECT=map_object, $
MISSINGVALUE=missingvalue, $
ONIMAGE=onImage, $
OUTCOLOR=outcolor, $
OUTFILENAME=outfilename, $
OUTLINE=outline, $
OUTPUT=output, $
PALETTE=palette, $
TRADITIONAL=traditional, $

;Contour Properties
C_ANNOTATION=c_annotation, $
C_CHARSIZE=c_charsize, $
C_CHARTHICK=c_charthick, $
C_COLORS=c_colors, $
C_LABELS=c_labels, $
C_LINESTYLE=c_linestyle, $
C_ORIENTATION=c_orientation, $
C_SPACING=c_spacing, $
C_THICK=c_thick, $
CELL_FILL=cell_fill, $
CLOSED=closed, $
DOWNHILL=downhill, $
FILL=fill, $
FOLLOW=follow, $
IRREGULAR=irregular, $
ISOTROPIC=isotropic, $
LEVELS=levels, $
NLEVELS=nlevels, $
MAX_VALUE=max_value, $
MIN_VALUE=min_value, $
PATH_DATA_COORDS=path_data_coords, $
PATH_DOUBLE=path_double, $
PATH_FILENAME=path_filename, $
RESOLUTION=resolution, $
TRIANGULATION=triangulation, $
XLOG=xlog, $
YLOG=ylog, $

;Graphics Keywords
POSITION = position, $
XSTYLE   = xstyle, $
YSTYLE   = ystyle, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
        
    ;Bad spellers...
    if n_elements(axesColor) ne 0 && n_elements(axisColor) eq 0 then axisColor = axesColor

    ;cgContour Properties
    if n_elements(axiscolor)     ne 0 then *self.axiscolor     = axiscolor
    if n_elements(background)    ne 0 then *self.background    = background
    if n_elements(label)         ne 0 then  self.label         = label
    if n_elements(missingvalue)  ne 0 then *self.missingvalue  = missingvalue
    if n_elements(onimage)       ne 0 then  self.onimage       = onimage
    if n_elements(outcolor)      ne 0 then *self.outcolor      = outcolor
    if n_elements(outfilename)   ne 0 then  self.outfilename   = outfilename
    if n_elements(outline)       ne 0 then  self.outline       = outline
    if n_elements(output)        ne 0 then  self.output        = output
    if n_elements(palette)       ne 0 then *self.palette       = palette
    if n_elements(traditional)   ne 0 then  self.traditional   = traditional
    
    ;Contour Properties
    if n_elements(c_annotation)  ne 0 then *self.c_annotation  = c_annotation
    if n_elements(c_charsize)    ne 0 then *self.c_charsize    = c_charsize
    if n_elements(c_charthick)   ne 0 then *self.c_charthick   = c_charthick
    if n_elements(c_colors)      ne 0 then *self.c_colors      = c_colors
    if n_elements(c_labels)      ne 0 then *self.c_labels      = c_labels
    if n_elements(c_linestyle)   ne 0 then *self.c_linestyle   = c_linestyle
    if n_elements(c_orientation) ne 0 then *self.c_orientation = c_orientation
    if n_elements(c_spacing)     ne 0 then *self.c_spacing     = c_spacing
    if n_elements(c_thick)       ne 0 then *self.c_thick       = c_thick
    if n_elements(cell_fill)     ne 0 then *self.cell_fill     = cell_fill
    if n_elements(fill)          ne 0 then *self.fill          = fill
    if n_elements(closed)        ne 0 then *self.closed        = closed
    if n_elements(downhill)      ne 0 then *self.downhill      = downhill
    if n_elements(follow)        ne 0 then *self.follow        = follow
    if n_elements(irregular)     ne 0 then *self.irregular     = irregular
    if n_elements(isotropic)     ne 0 then *self.isotropic     = isotropic
    if n_elements(levels)        ne 0 then *self.levels        = levels
    if n_elements(max_value)     ne 0 then *self.max_value     = max_value
    if n_elements(min_value)     ne 0 then *self.min_value     = min_value
    if n_elements(nlevels)       ne 0 then *self.nlevels       = nlevels
    if n_elements(nlevels)       ne 0 then *self.nlevels       = nlevels
    if n_elements(path_filename) ne 0 then *self.path_filename = path_filename
    if n_elements(triangulation) ne 0 then *self.triangulation = triangulation
    if n_elements(path_double)   ne 0 then *self.path_double   = path_double
    if n_elements(resolution)    ne 0 then *self.resolution    = resolution
    if n_elements(xlog)          ne 0 then  self.xlog          = xlog
    if n_elements(ylog)          ne 0 then  self.ylog          = ylog
    if n_elements(path_data_coords)   ne 0 then *self.path_data_coords = path_data_coords
    
    if n_elements(position) gt 0 then self -> SetLayout, POSITION=position

    ;Map Object
    if n_elements(map_object) gt 0 and obj_valid(map_object) then begin
        if n_elements(*self.map_object) gt 0 then begin
            if obj_valid(*self.map_object) then obj_destroy, *self.map_object
            *self.map_object = map_object
        endif else *self.map_object = map_object
    endif
    
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
    
    if n_elements(xstyle) ne 0 then *self.xstyle = ~(xstyle and 1) + xstyle
    if n_elements(ystyle) ne 0 then *self.ystyle = ~(ystyle and 1) + ystyle
    
    self.window -> Draw
end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrContour::cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif              
    
    ;Free pointers
    ptr_free, self.c_data
    ptr_free, self.xcoords
    ptr_free, self.ycoords
    ptr_free, self.axiscolor
    ptr_free, self.background
    ptr_free, self.c_annotation
    ptr_free, self.c_charsize
    ptr_free, self.c_charthick
    ptr_free, self.c_colors
    ptr_free, self.c_labels
    ptr_free, self.c_linestyle
    ptr_free, self.c_orientation
    ptr_free, self.c_spacing
    ptr_free, self.c_thick
    ptr_free, self.cell_fill
    ptr_free, self.closed
    ptr_free, self.downhill
    ptr_free, self.fill
    ptr_free, self.follow
    ptr_free, self.irregular
    ptr_free, self.isotropic
    ptr_free, self.levels
    ptr_free, self.nlevels
    ptr_free, self.max_value
    ptr_free, self.min_value
    ptr_free, self.missingvalue
    ptr_free, self.olevels
    ptr_free, self.palette
    ptr_free, self.path_data_coords
    ptr_free, self.path_double
    ptr_free, self.path_filename
    ptr_free, self.path_info
    ptr_free, self.path_xy
    ptr_free, self.resolution
    ptr_free, self.triangulation
    ptr_free, self.xlog
    ptr_free, self.ylog
    
    ;Destroy objects
    undefine, self.map_object

    ;Cleanup the remaining keywords by calling the superclass. This must be done because
    ;the superclasses method has been over-ridden here.
    self -> weGraphicsKeywords::CleanUp
    self -> MrGrAtom::CleanUp
    self -> MrLayout::CleanUp
end


;+
; For more information, see::
;       `IDL Contour command <http://www.exelisvis.com/docs/CONTOUR_Procedure.html>`
;       `cgContour <http://www.idlcoyote.com/idldoc/cg/cgcontour.html>`
;       `cgGraphicsCommands <http://www.idlcoyote.com/idldoc/cg/cggraphicskeywords__define.html>`
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Params:
;       DATA:           in, required, type=any
;                       A one- or two-dimensional array containing the values that make 
;                           up the contour surface.
;       X:              in, optional, type=any
;                       A vector or two-dimensional array specifying the X coordinates for
;                           the contour surface.
;       Y:              in, optional, type=any
;                       A vector or two-dimensional array specifying the Y coordinates for
;                           the contour surface.
;       
; :Keywords:
;       ASPECT:         in, optional, type=float, default=none
;                       Set this keyword to a floating point ratio that represents the aspect ratio 
;                           (ysize/xsize) of the resulting plot. The plot position may change as a result
;                           of setting this keyword. Note that `Aspect` cannot be used when plotting with
;                           !P.MULTI.
;       AXISCOLOR:      in, optional, type=string/integer, default='opposite'
;                       If this keyword is a string, the name of the axis color. 
;                           Otherwise, the keyword is assumed to be a color index into the current color table.
;       AXESCOLOR:      in, optional, type=string/integer
;                       Provisions for bad spellers.
;       BACKGROUND:     in, optional, type=string/integer, default='background'
;                       If this keyword is a string, the name of the background color. 
;                           Otherwise, the keyword is assumed to be a color index into the current color table.
;       C_ANNOTATION:   in, optional, type=string
;                       The label to be drawn on each contour. Normally contours are labeled with their
;                           value. This vector of strings may substitute for those values.
;       C_CHARSIZE:     in, optional, type=float
;                       The character size of the annotations used on the contour lines themselves.
;                           By default, 75% of `Charsize`.
;       C_CHARTHICK:    in, optional, type=integer, default=1
;                       The thickness of the characters used to annotate contour labels.
;       C_COLORS:       in, optional, type=integer/string vector
;                       Set to the index values of the contour colors or to named colors. Must contain
;                           the same number of colors as the number of requested contour levels.
;       C_LABELS:       in, optional, type=integer
;                       A vector that specifies which contour levels to label. If used, the LABEL
;                           keyword is ignored.
;       C_LINESTYLE:    in, optional, type=integer/intarr
;                       The line style used to draw each contour (cyclical).
;       C_ORIENTATION:  in, optional, type=float
;                       If the `FILL` or `CELL_FILL` keyword is set, this keyword can be set
;                           to the angle, in degrees counterclockwise from the horizontal,
;                           of the lines used to fill contours. If neither `C_ORIENTATION`
;                           nor `C_SPACING` are specified, the contours are solid filled.
;       C_SPACING:      in, optional, type=float
;                       If the `FILL` or `CELL_FILL` keyword is set, this keyword can be
;                           used to control the distance, in centimeters, between the lines
;                           used to fill the contours.
;       C_THICK:        in, optional, type=fltarr
;                       The line used to draw each contour level (cyclical).
;       CELL_FILL:      in, optional, type=boolean, default=0
;                       Set to indicate filled contours should be created using the "cell fill" method.
;                           This keyword should always be set if displaying filled contours on map projections
;                           or if missing data is present in the data you are contouring.
;       CLOSED:         in, optional, type=boolean
;                       Close contours that intersect the plot boundaries. Set CLOSED=0
;                           along with `PATH_INFO` and/or `PATH_XY` to return path
;                           information for contours that are not closed.
;       COLOR:          in, optional, type=string/integer, default='black'
;                       If this keyword is a string, the name of the data color. By default, same as AXISCOLOR.
;                           Otherwise, the keyword is assumed to be a color index into the current color table.
;       FILL:           in, optional, type=boolean, default=0
;                       Set to indicate filled contours should be created.
;       TARGET:         in, optional, type=object, default=obj_new()
;                       If this keyword is set equal to a graphics object, then the
;                           contour plot will determine the location of the graphic in
;                           display window and overplot itself onto that image.
;       IRREGULAR:      in, optional, type=boolean
;                       If this keyword is set, the data, x, and y input parameters are taken to be
;                           irregularly gridded data, the the data is gridded for use in the contour plot
;                           using the Triangulate and Trigrid method. The resolution of the gridded output
;                           is set by the RESOLUTION keyword.
;       ISOTROPIC:      in, optional, type=boolean, default=0
;                       Force the scaling of the X and Y axes to be equal.
;       LABEL:          in, optional, type=integer, default=1
;                       An number that tells how to label contour levels. A 0 means
;                           no contour levels are labelled. A 1 (the default) means all contour levels are
;                           labelled. A 2 means label every 2nd contour level is labelled. A 3 means every 
;                           3rd contour level is labelled, and so on.
;       LEVELS:         in, optional, type=any
;                       A vector of data levels to contour. If used, NLEVELS is ignored. If missing, 
;                           NLEVELS is used to construct N equally-spaced contour levels.
;       MAP_OBJECT:     in, optional, type=object
;                       If you are overplotting (OVERPLOT=1) on a map projection set up with Map_Proj_Init
;                           and using projected meter space, rather than lat/lon space, then you can use this
;                           keyword to provide a cgMap object that will allow you to convert the `x` and `y`
;                           grid parameters from longitude and latitude, respectively, to projected meter space
;                           before the contour is displayed. Note, you MUST pass the `x` and `y` grid parameters 
;                           to cgContour if you are overplotting on a map projection. There is no checking to
;                           be sure these parameters are in the correct longitude and latitude range, respectively.
;       MAX_VALUE:      in, optional, type=any
;                       Data points with values above this value are ignored.
;       MIN_VALUE:      in, optional, type=any
;                       Data points with values below this value are ignored.
;       MISSINGVALUE:   in, optional, type=any
;                       Use this keyword to identify any missing data in the input data values.
;       NAME:           in, optional, type=string, default='MrContour'
;                       Specifies the name of the graphic.
;       NLEVELS:        in, optional, type=integer, default=6
;                       If the Contour plot LEVELS keyword is not used, this keyword will produce this
;                           number of equally spaced contour intervals. Unlike the Contour NLEVELS keyword,
;                           this keyword actually works!
;       OLEVELS:        out, optional
;                       Set to a named variable to return the actual contour levels used in the program.
;                           Unfortunately, output variables cannot be returned if the cgContour command is
;                           being executed in a cgWindow.
;       ONIMAGE:        in, optional, type=boolean, default=0
;                       If this keyword is set, and an image has been display previously with cgImage,
;                           then the contour plot will determine the location of the image in the display
;                           window and overplot itself onto that image.
;       OUTCOLOR:       in, optional, type=string, default='charcoal'
;                       The color of the contour lines when the `Outline` keyword is used.
;       OUTFILENAME:    in, optional, type=string
;                       If the `Output` keyword is set, the user will be asked to supply an output
;                           filename, unless this keyword is set to a non-null string. In that case, the
;                           value of this keyword will be used as the filename and there will be no dialog
;                           presented to the user.
;       OUTLINE:        in, optional, type=boolean, default=0
;                       This keyword applies only if the `Fill` keyword is set. It will draw the
;                           contour lines on top of the filled contour. It draws the outline in the `OutColor`.
;       OUTPUT:         in, optional, type=string, default=""
;                       Set this keyword to the type of output desired. Possible values are these::
;            
;                           'PS'   - PostScript file
;                           'EPS'  - Encapsulated PostScript file
;                           'PDF'  - PDF file
;                           'BMP'  - BMP raster file
;                           'GIF'  - GIF raster file
;                           'JPEG' - JPEG raster file
;                           'PNG'  - PNG raster file
;                           'TIFF' - TIFF raster file
;            
;                       Or, you can simply set this keyword to the name of the output file, and the type of
;                           file desired will be determined by the file extension. If you use this option, the
;                           user will not be prompted to supply the name of the output file.
;            
;                           All raster file output is created through PostScript intermediate files (the
;                           PostScript files will be deleted), so ImageMagick and Ghostview MUST be installed 
;                           to produce anything other than PostScript output. (See cgPS2PDF and PS_END for 
;                           details.) And also note that you should NOT use this keyword when doing multiple 
;                           plots. The keyword is to be used as a convenient way to get PostScript or raster 
;                           output for a single graphics command. Output parameters can be set with cgWindow_SetDefs.
;       OVERPLOT:       in, optional, type=boolean, default=0
;                       Set this keyword to overplot the contours onto a previously established
;                           data coordinate system.
;       PALETTE:        in, optional, type=byte
;                       A (256x3) color palette containing the RGB color vectors to use for coloring contours.
;                           Contour colors will be sampled from the color table palette into the number 
;                           of contour levels required. If the palette is NOT 256 elements in length, then
;                           it is assumed that the length corresponds to the number of levels to be contoured.
;    PATH_DATA_COORDS:  in, optional, type=boolean, default=0
;                       indicate that the `PATH_FILENAME`, `PATH_INFO`, and `PATH_XY` 
;                           keywords should return  vertex and contour value information
;                           as doubles
;       PATH_FILENAME:  in, optional, type=boolean, default=0
;                       Specifies the name of a file to contain the contour positions.
;       PATH_INFO:      out, optional, type=array of structures
;                       Set this keyword to a named variable that will return path
;                           information for the contours.
;       PATH_XY:        out, optional, type=fltarr
;                       Set this keyword to a named variable that returns the coordinates
;                           of a set of closed polygons defining the closed paths of the
;                           contours
;       RESOLUTION:     in, optional, type=integer array, default=[41\,41]
;                       If the IRREGULAR keyword is set, this keyword specifies the X and Y resolution
;                           in a two element integer array of the final gridded data that is sent to the 
;                           contour plot.
;       TRADITIONAL:    in, optional, type=boolean, default=0
;                        If this keyword is set, the traditional color scheme of a black background for
;                            graphics windows on the display is used and PostScript files always use a white background.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword appropriate for the `cgGraphicsKeywords class <http://www.idlcoyote.com/programs/cggraphicskeywords__define.pro>` 
;                           is allowed in the program.
;-
FUNCTION MrContour::Init, data, x, y, $
;MrContour Keywords
CURRENT = current, $
OVERPLOT=target, $

;CONTOUR KEYWORDS
C_ANNOTATION=c_annotation, $
C_CHARSIZE=c_charsize, $
C_CHARTHICK=c_charthick, $
C_COLORS=c_colors, $
C_LABELS=c_labels, $
C_LINESTYLE=c_linestyle, $
C_ORIENTATION=c_orientation, $
C_SPACING=c_spacing, $
C_THICK=c_thick, $
CELL_FILL=cell_fill, $
CLOSED=closed, $
COLOR=color, $
DRAW=draw, $
DOWNHILL=downhill, $
FILL=fill, $
FOLLOW=follow, $
IRREGULAR=irregular, $
ISOTROPIC=isotropic, $
LEVELS=levels, $
MAX_VALUE=max_value, $
MIN_VALUE=min_value, $
NLEVELS=nlevels, $
PATH_DATA_COORDS=path_data_coords, $
PATH_DOUBLE=path_double, $
PATH_FILENAME=path_filename, $
PATH_INFO=path_info, $
PATH_XY=path_xy, $
RESOLUTION=resolution, $
TRIANGULATION=triangulation, $
XLOG=xlog, $
YLOG=ylog, $

;cgContour Keywords
AXISCOLOR=axiscolor, $
AXESCOLOR=axescolor, $
BACKGROUND=sbackground, $
LABEL=label, $
MAP_OBJECT=map_object, $
MISSINGVALUE=missingvalue, $
OLEVELS=olevels, $
ONIMAGE=onImage, $
OUTCOLOR=outcolor, $
OUTFILENAME=outfilename, $
OUTLINE=outline, $
OUTPUT=output, $
PALETTE=palette, $
TRADITIONAL=traditional, $

;weGraphicsKeywords
_REF_EXTRA=extra
    
    Compile_Opt idl2

    catch, theerror
    if theerror ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif

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
;ALLOCATE HEAP ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;
    ;Allocate heap for the variables
    ;
    ;   This must be done before MrGrAtom is initialized. MrGrAtom creates a MrWindow
    ;   object and adds this object to MrWindow's container. In doing so, the Set and
    ;   Get Property methods are called. If heap is not allocated, checks for
    ;   N_ELEMENTS(*SELF.[]) will result in "Unable to dereference NULL pointer" errors.
    ;
    self.c_data           = Ptr_New(/ALLOCATE_HEAP)
    self.xcoords          = Ptr_New(/ALLOCATE_HEAP)
    self.ycoords          = Ptr_New(/ALLOCATE_HEAP)
    self.axiscolor        = Ptr_New(/ALLOCATE_HEAP)
    self.background       = Ptr_New(/ALLOCATE_HEAP)
    self.c_annotation     = Ptr_New(/ALLOCATE_HEAP)
    self.c_charsize       = Ptr_New(/ALLOCATE_HEAP)
    self.c_charthick      = Ptr_New(/ALLOCATE_HEAP)
    self.c_colors         = Ptr_New(/ALLOCATE_HEAP)
    self.c_labels         = Ptr_New(/ALLOCATE_HEAP)
    self.c_linestyle      = Ptr_New(/ALLOCATE_HEAP)
    self.c_orientation    = Ptr_New(/ALLOCATE_HEAP)
    self.c_spacing        = Ptr_New(/ALLOCATE_HEAP)
    self.c_thick          = Ptr_New(/ALLOCATE_HEAP)
    self.cell_fill        = Ptr_New(/ALLOCATE_HEAP)
    self.closed           = Ptr_New(/ALLOCATE_HEAP)
    self.downhill         = Ptr_New(/ALLOCATE_HEAP)
    self.fill             = Ptr_New(/ALLOCATE_HEAP)
    self.follow           = Ptr_New(/ALLOCATE_HEAP)
    self.irregular        = Ptr_New(/ALLOCATE_HEAP)
    self.isotropic        = Ptr_New(/ALLOCATE_HEAP)
    self.levels           = Ptr_New(/ALLOCATE_HEAP)
    self.nlevels          = Ptr_New(/ALLOCATE_HEAP)
    self.max_value        = Ptr_New(/ALLOCATE_HEAP)
    self.min_value        = Ptr_New(/ALLOCATE_HEAP)
    self.missingvalue     = Ptr_New(/ALLOCATE_HEAP)
    self.olevels          = Ptr_New(/ALLOCATE_HEAP)
    self.outcolor         = Ptr_New(/ALLOCATE_HEAP)
    self.palette          = Ptr_New(/ALLOCATE_HEAP)
    self.path_data_coords = Ptr_New(/ALLOCATE_HEAP)
    self.path_double      = Ptr_New(/ALLOCATE_HEAP)
    self.path_filename    = Ptr_New(/ALLOCATE_HEAP)
    self.path_info        = Ptr_New(/ALLOCATE_HEAP)
    self.path_xy          = Ptr_New(/ALLOCATE_HEAP)
    self.resolution       = Ptr_New(/ALLOCATE_HEAP)
    self.triangulation    = Ptr_New(/ALLOCATE_HEAP)
    self.xlog             = Ptr_New(/ALLOCATE_HEAP)
    self.ylog             = Ptr_New(/ALLOCATE_HEAP)
    
    ;Initialize Objects
    self.target           = Obj_New()
    self.map_object       = Ptr_New(/ALLOCATE_HEAP)

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
;Defaults ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Provision for bad spellers
    if n_elements(axisColor) eq 0 and n_elements(axesColor) ne 0 then axisColor = axesColor

    ;Output path information (suppresses drawing of contours)
    GetPath_Info = Arg_Present(path_info)
    GetPath_XY = Arg_Present(path_xy)
    
    ;Defaults
    SetDefaultValue, label, 1, /BOOLEAN
    SetDefaultValue, onImage, 0B, /BOOLEAN
    SetDefaultValue, outfilename, ''
    SetDefaultValue, outline, 0B, /BOOLEAN
    SetDefaultValue, output, ''
    SetDefaultValue, overplot, 0B, /BOOLEAN
    SetDefaultValue, traditional, 0B, /BOOLEAN
    
;---------------------------------------------------------------------
;Define Data /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;If X and Y and not defined, plot DATA against its subscripts.
    nx = n_elements(x)
    ny = n_elements(y)
    if nx eq 0 && ny eq 0 then begin
        dims = size(data, /DIMENSIONS)
        xcoords = lindgen(dims[0])
        ycoords = lindgen(dims[1])
            
     endif else if nx gt 0 && ny gt 0 then begin   
        xcoords = x
        ycoords = y

    endif else message, 'Incorrect number of parameters.'
    
;---------------------------------------------------------------------
;Set Object Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
    self -> SetData, data, xcoords, ycoords

    ;Set the object properties
    self -> SetProperty, $
                         ;cgContour Keywords
                         AXISCOLOR=axiscolor, $
                         AXESCOLOR=axescolor, $
                         BACKGROUND=background, $
                         COLOR=color, $
                         LABEL=label, $
                         MAP_OBJECT=map_object, $
                         ONIMAGE=onImage, $
                         OUTCOLOR=outcolor, $
                         OUTFILENAME=outfilename, $
                         OUTLINE=outline, $
                         OUTPUT=output, $
                         MISSINGVALUE=missingvalue, $
                         PALETTE=palette, $
                         TRADITIONAL=traditional, $
                         
                         ;Contour Keywords
                         C_ANNOTATION=c_annotation, $
                         C_CHARSIZE=c_charsize, $
                         C_CHARTHICK=c_charthick, $
                         C_COLORS=c_colors, $
                         C_LABELS=c_labels, $
                         C_LINESTYLE=c_linestyle, $
                         C_ORIENTATION=c_orientation, $
                         C_SPACING=c_spacing, $
                         C_THICK=c_thick, $
                         CELL_FILL=cell_fill, $
                         CLOSED=closed, $
                         DOWNHILL=downhill, $
                         FILL=fill, $
                         FOLLOW=follow, $
                         IRREGULAR=irregular, $
                         ISOTROPIC=isotropic, $
                         LEVELS=levels, $
                         NLEVELS=nlevels, $
                         MAX_VALUE=max_value, $
                         MIN_VALUE=min_value, $
                         PATH_DATA_COORDS=path_data_coords, $
                         PATH_DOUBLE=path_double, $
                         PATH_FILENAME=path_filename, $
                         RESOLUTION=resolution, $
                         TRIANGULATION=triangulation, $
                         XLOG=xlog, $
                         YLOG=ylog
    
    ;Overplot?
    if n_elements(target) gt 0 then begin
        if size(target, /TNAME) eq 'OBJREF' $
            then self -> Overplot, target $
            else if keyword_set(target) then self -> Overplot
    endif
            

;---------------------------------------------------------------------
;Set Ranges and Styles ///////////////////////////////////////////////
;---------------------------------------------------------------------

    ;For zooming purposes, [XY]STYLE must have the 2^0 bit set
    if n_elements(*self.xstyle) eq 0 $
        then *self.xstyle = 1 $
        else *self.xstyle += ~(*self.xstyle and 1)
        
    if n_elements(*self.ystyle) eq 0 $
        then *self.ystyle = 1 $
        else *self.ystyle += ~(*self.ystyle and 1)

    ;Make sure the [XY]RANGE is set.
    if n_elements(*self.xrange) eq 0 then *self.xrange = [min(xcoords, max=XMax), XMax]
    if n_elements(*self.yrange) eq 0 then *self.yrange = [min(ycoords, max=YMax), YMax]
    self.init_xrange = *self.xrange
    self.init_yrange = *self.yrange

;---------------------------------------------------------------------
;Draw ////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Refresh the graphics?
    if keyword_set(current) $
        then theWin -> Refresh, DISABLE=~init_refresh $
        else self.window -> Draw
    
;    if keyword_set(draw) then begin
;        if GetPath_Info || GetPath_XY then begin
;            self -> draw, OLEVELS=oLevels, $
;                          PATH_INFO=path_info, $
;                          PATH_XY=path_xy
;        endif else begin
;            self -> draw, OLEVELS=oLevels
;        endelse
;    endif

    return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrContour__define, class
    compile_opt idl2
    on_error, 2
    
    class = { MrContour, $
              inherits MrLayout, $
              inherits MrGrAtom, $
              inherits weGraphicsKeywords, $
              
              ;Data properties
              c_data: Ptr_New(), $
              xcoords: Ptr_New(), $
              ycoords: Ptr_New(), $
              
              ;MrContour Properties
              target: obj_new(), $
              init_xrange: fltarr(2), $
              init_yrange: fltarr(2), $
              ;cgContour Properties
              label: 0, $
              map_object: Ptr_New(), $      ;cgContour throws an error for invalid objects
              missingvalue: Ptr_New(), $
              olevels: Ptr_New(), $
              onImage: 0B, $
              outcolor: Ptr_New(), $
              outfilename: '', $
              outline: 0B, $
              output: '', $
              palette: Ptr_New(), $
              traditional: 0B, $
              
              ;Contour Properties
              c_annotation: Ptr_New(), $
              c_charsize: Ptr_New(), $
              c_charthick: Ptr_New(), $
              c_colors: Ptr_New(), $
              c_labels: Ptr_New(), $
              c_linestyle: Ptr_New(), $
              c_orientation: Ptr_New(), $
              c_spacing: Ptr_New(), $
              c_thick: Ptr_New(), $
              cell_fill: Ptr_New(), $
              closed: Ptr_New(), $
              downhill: Ptr_New(), $
              fill: Ptr_New(), $
              follow: Ptr_New(), $
              irregular: Ptr_New(), $
              isotropic: Ptr_New(), $
              levels: Ptr_New(), $
              nlevels: Ptr_New(), $
              max_value: Ptr_New(), $
              min_value: Ptr_New(), $
              overplot: 0B, $
              path_data_coords: Ptr_New(), $
              path_double: Ptr_New(), $
              path_filename: Ptr_New(), $
              path_info: Ptr_New(), $
              path_xy: Ptr_New(), $
              resolution: Ptr_New(), $
              triangulation: Ptr_New(), $
              xlog: Ptr_New(), $
              ylog: Ptr_New() $
            }
end