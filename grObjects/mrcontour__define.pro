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
; :Examples:
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
;       2013/12/02  -   Added the GetPath method. - MRA
;       2014/03/12  -   Overplot and Current are now applied in a more straigh-forward
;                           manner. Disinherited MrLayout, but kept it as a object
;                           property. Added the Getlayout method. - MRA
;       2014/03/26  -   Inherit MrGrDataAtom and remove duplicate keywords/methods. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to draw the plot in the draw window. The plot is
;   first buffered into the pixmap for smoother opteration (by allowing motion events
;   to copy from the pixmap instead of redrawing the plot, the image does not flicker).
;-
pro MrContour::Draw, $
NOERASE=noerase
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Return if we are hiding
    if self.hide then return
    
    ;Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Going to have to do all of this in decomposed color, if possible.
    cgSetColorState, 1, CURRENTSTATE=currentState
    
    ;Overplot?
    if self.overplot then begin
        ;Restore target's coordinate system. Make sure that the overplot
        ;is positioned correctly.
        self.target -> RestoreCoords
        position = [!x.window[0], !y.window[0], $
                    !x.window[1], !y.window[1]]
        self.layout -> SetProperty, POSITION=position, UPDATE_LAYOUT=0
        
        ;Overplot
        self -> doContour, NOERASE=noerase, $
                           OLEVELS=oLevels
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
PATH_DATA_COORDS=path_data_coords, $
PATH_DOUBLE=path_double, $
PATH_FILENAME=path_filename, $
PATH_INFO=path_info, $
PATH_XY=path_xy

    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    tf_GetPath = arg_present(path_info) || arg_present(path_xy) || n_elements(path_filename) gt 0
    

    if n_elements(noerase) eq 0 then noerase = *self.noerase
    
    self.layout -> GetProperty, POSITION=position, CHARSIZE=charsize

;-----------------------------------------------------
;Draw the Contour Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;
    ; PATH_INFO and PATH_XY both suppress output. If neither are desired, call
    ; cgContour without them.
    ;
    if tf_getpath eq 0 then begin
        cgContour, *self.c_data, *self.xcoords, *self.ycoords, $
                
                   ;cgContour Keywords
;                   ASPECT           = *self.aspect, $
                   LABEL            =  self.label, $
;                   LAYOUT           = *self.layout, $
                   MAP_OBJECT       = *self.map_object, $
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
                   RESOLUTION       = *self.resolution, $
                   TRIANGULATION    = *self.triangulation, $
                   XLOG             =  self.xlog, $
                   YLOG             =  self.ylog, $
                   
                   ;MrLayout Keywords
                   CHARSIZE      =       charsize, $
                   POSITION      =       position, $
              
                   ;MrGraphicsKeywords
                   AXISCOLOR     = *self.axiscolor, $
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
               LAYOUT           =  self.layout, $
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
               PATH_DATA_COORDS =       path_data_coords, $
               PATH_DOUBLE      =       path_double, $
               PATH_FILENAME    =       path_filename, $
               PATH_INFO        =       path_info, $
               PATH_XY          =       path_xy, $
               RESOLUTION       = *self.resolution, $
               TRIANGULATION    = *self.triangulation, $
               XLOG             =  self.xlog, $
               YLOG             =  self.ylog, $
                   
               ;MrLayout Keywords
               CHARSIZE      =       charsize, $
               POSITION      =       position, $
          
               ;MrGraphicsKeywords
               AXISCOLOR     = *self.axiscolor, $
               BACKGROUND    = *self.background, $
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
;               YTHICK        = *self.ythick, $
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
        void = cgErrorMsg()
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
;   The purpose of this method is to get contour path information. See IDL's `Contour
;   Procedure <http://exelisvis.com/docs/CONTOUR_Procedure.html>` for more information
;
; :Params:
;       PATH_INFO:          out, required, type=structure
;                           Named variable that will return path information for the
;                               contours.
;       PATH_XY:            out, optional, type=structure
;                           named variable that returns the coordinates of a set of closed
;                               polygons defining the closed paths of the contours.
;
; :Keywords:
;       OLEVELS:            out, optional, type=fltarr
;                           Set to a named variable to return the actual contour levels
;                               used in the program.
;       PATH_DATA_COORDS:   in, optional, type=boolean, default=0
;                           If set, path info will be returned in data coordinates.
;       PATH_DOUBLE:        in, optional, type=boolean, default=0
;                           If set, path information will be returned in double precision.
;       PATH_FILENAME:      in, optional, type=string
;                           If provided, path information will be output to this file.
;-
pro MrContour::GetPath, path_info, path_xy, $
OLEVELS=oLevels, $
PATH_DATA_COORDS=path_data_coords, $
PATH_DOUBLE=path_double, $
PATH_FILENAME=path_filename
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if windowavailable(tempPix) then wdelete, tempPix
        void = cgErrorMsg()
        return
    endif
    
    ;Defaults
    path_data_coords = keyword_set(path_data_coords)
    path_double = keyword_set(path_double)
    
    ;Create a temporary pixmap window so that the empty axes generated by Contour
    ;does not interfere with what is already plotted.
    tempPix = MrGetWindow(/FREE, /PIXMAP)
    
    ;Get path information without drawing anything
    self -> doContour, OLEVELS=oLevels, $
                       PATH_DATA_COORDS=path_data_coords, $
                       PATH_FILENAME=path_filename, $
                       PATH_INFO=path_info, $
                       PATH_XY=path_xy
    
    ;Delete the pixmap window
    wdelete, tempPix
end


;+
;   The purpose of this method is to set object properties. 
;
; :Keywords:
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
;       MISSINGVALUE:   out, optional, type=any
;                       Use this keyword to identify any missing data in the input data values.
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
RANGE=range, $
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
PATH_DATA_COORDS=path_data_coords, $
PATH_DOUBLE=path_double, $
PATH_FILENAME=path_filename, $
PATH_INFO=path_info, $
PATH_XY=path_xy, $
RESOLUTION=resolution, $
TRIANGULATION=triangulation, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;cgContour Properties
    if arg_present(axiscolor)     ne 0 then axiscolor     = *self.axiscolor
    if arg_present(axescolor)     ne 0 then axescolor     = *self.axiscolor
    if arg_present(background)    ne 0 then background    = *self.background
    if arg_present(label)         ne 0 then label         =  self.label
    if arg_present(layout)        ne 0 then layout        =  self.layout -> GetLayout()
    if arg_present(olevels)       ne 0 then olevels       = *self.olevels
    if arg_present(onimage)       ne 0 then onimage       =  self.onimage
    if arg_present(outcolor)      ne 0 then outcolor      = *self.outcolor
    if arg_present(outfilename)   ne 0 then outfilename   =  self.outfilename
    if arg_present(outline)       ne 0 then outline       =  self.outline
    if arg_present(output)        ne 0 then output        =  self.output
    if arg_present(palette)       ne 0 then palette       = *self.palette
    if arg_present(position)      ne 0 then position      =  self.position -> GetPosition()
    if arg_present(range)         ne 0 then range         =  self.range
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
    if arg_present(missingvalue)  ne 0 then missingvalue  = *self.missingvalue
    if arg_present(nlevels)       ne 0 then nlevels       = *self.nlevels
    if arg_present(path_filename) ne 0 then path_filename = *self.path_filename
    if arg_present(path_info)     ne 0 then path_info     = *self.path_info
    if arg_present(path_xy)       ne 0 then path_xy       = *self.path_xy
    if arg_present(triangulation) ne 0 then triangulation = *self.triangulation
    if arg_present(path_double)   ne 0 then path_double   = *self.path_double
    if arg_present(resolution)    ne 0 then resolution    = *self.resolution
    if arg_present(path_data_coords)   ne 0 then path_data_coords = *self.path_data_coords

    ;Objects
    if arg_present(map_object) ne 0 then if obj_valid(self.map_object) $
        then map_object = self.map_object $
        else map_object = obj_new()
    
    ;Superclass properties
    if n_elements(extra) ne 0 then self -> MrGrDataAtom::GetProperty, _EXTRA=extra
end


;+
;   Set the color palette.
;-
pro MrContour::SetPalette, $
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
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
;
; The goal is to create an contour class that is zoomable. As such, [XY]RANGE must
; be defined so that data coordinate system is establishable (even if the coordinates
; simply span the image size). Independent and Dependent variables must be defined
; so that a map exists between the data coordinates and the image's pixel/index
; locations.
;
    
    ;Retrieve the data
    case n_params() of
        1: begin
            *self.c_data = z
        endcase
        3: begin
            *self.c_data = z
            *self.xcoords = x
            *self.ycoords = y
        endcase
        else: message, 'Incorrect number of parameters.'
    endcase
            
    ;Set the ranges -- If no data is defined, use the dimensions of the contour. Make
    ;                  sure not to have the minimum equal to zero if we are log-scaling
    cDims = size(z, /DIMENSIONS)
    self.range  = [min(z, /NAN, MAX=zMax), zMax]
    if n_elements(*self.xcoords) eq 0 $
        then *self.xrange = [self.xlog, cDims[0]-1] $
        else *self.xrange = [min(*self.xcoords, /NAN, MAX=xMax), xMax]
    if n_elements(*self.ycoords) eq 0 $
        then *self.yrange = [self.ylog, cDims[0]-1] $
        else *self.yrange = [min(*self.ycoords, /NAN, MAX=yMax), yMax]
    
    ;Set the 
    if n_elements(*self.xcoords) eq 0 then begin
        if self.xlog $
            then *self.xcoords = logspace(alog10((*self.xrange)[0]), alog10((*self.xrange)[1]), cDims[0]) $
            else *self.xcoords = linspace((*self.xrange)[0], (*self.xrange)[1], cDims[0])
    endif
    if n_elements(*self.ycoords) eq 0 then begin
        if self.ylog $
            then *self.ycoords = logspace(alog10((*self.yrange)[0]), alog10((*self.yrange)[1]), cDims[1]) $
            else *self.ycoords = linspace((*self.yrange)[0], (*self.yrange)[1], cDims[1])
    endif
    
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
RANGE=range, $
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
PATH_DATA_COORDS=path_data_coords, $
PATH_DOUBLE=path_double, $
PATH_FILENAME=path_filename, $
RESOLUTION=resolution, $
TRIANGULATION=triangulation, $
XLOG=xlog, $
YLOG=ylog, $

;Graphics Keywords
CHARSIZE = charsize, $
POSITION = position, $
XSTYLE   = xstyle, $
YSTYLE   = ystyle, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
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
    if n_elements(range)         ne 0 then  self.range         = range
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
    if n_elements(nlevels)       ne 0 then *self.nlevels       = nlevels
    if n_elements(nlevels)       ne 0 then *self.nlevels       = nlevels
    if n_elements(path_filename) ne 0 then *self.path_filename = path_filename
    if n_elements(triangulation) ne 0 then *self.triangulation = triangulation
    if n_elements(path_double)   ne 0 then *self.path_double   = path_double
    if n_elements(resolution)    ne 0 then *self.resolution    = resolution
    if n_elements(xlog)          ne 0 then  self.xlog          = xlog
    if n_elements(ylog)          ne 0 then  self.ylog          = ylog
    if n_elements(path_data_coords)   ne 0 then *self.path_data_coords = path_data_coords

    ;Map Object
    if n_elements(map_object) gt 0 and obj_valid(map_object) then begin
        if n_elements(*self.map_object) gt 0 then begin
            if obj_valid(*self.map_object) then obj_destroy, *self.map_object
            *self.map_object = map_object
        endif else *self.map_object = map_object
    endif
    
    ;Superclass properties
    if n_elements(extra) gt 0 then self -> MrGrDataAtom::SetProperty, _EXTRA=extra
    
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
        void = cgErrorMsg()
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
    
    ;Destroy objects
    undefine, self.map_object

    ;Superclass cleanup
    self -> MrGrDataAtom::CleanUp
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
;       FILL:           in, optional, type=boolean, default=0
;                       Set to indicate filled contours should be created.
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
CURRENT=current, $
HIDE=hide, $
LAYOUT=layout, $
NAME=name, $
OVERPLOT=overplot, $
POSITION=position, $

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

;MrGraphicsKeywords
XLOG=xlog, $
XRANGE=xrange, $
YLOG=ylog, $
YRANGE=yrange, $
_REF_EXTRA=extra
    
    compile_opt strictarr

    catch, theerror
    if theerror ne 0 then begin
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
    
    ;Initialize Objects
    self.map_object       = Ptr_New(/ALLOCATE_HEAP)
    
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
    SetDefaultValue, traditional, 0B, /BOOLEAN
    
;---------------------------------------------------------------------
;Set Object Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(x) gt 0 $
        then self -> SetData, data, x, y $
        else self -> SetData, data

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
    self.init_xrange = *self.xrange
    self.init_yrange = *self.yrange

;---------------------------------------------------------------------
;Draw ////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Get the path information, if requested.
    if GetPath_Info || GetPath_XY $
        then self -> GetPath, path_info, path_xy, OLEVELS=oLevels, $
                              PATH_DATA_COORDS=path_data_coords, PATH_DOUBLE=path_double, $
                              PATH_FILENAME=path_filename

    ;Refresh the graphics?
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
pro MrContour__define, class
    compile_opt strictarr
    on_error, 2
    
    class = { MrContour, $
              inherits MrGrDataAtom, $
              
              ;Data properties
              c_data:  Ptr_New(), $
              xcoords: Ptr_New(), $
              ycoords: Ptr_New(), $
              
              ;MrContour Properties
              init_xrange:   fltarr(2), $
              init_yrange:   fltarr(2), $
              ;cgContour Properties
              label:         0, $
              map_object:    Ptr_New(), $      ;cgContour throws an error for invalid objects
              missingvalue:  Ptr_New(), $
              olevels:       Ptr_New(), $
              onImage:       0B, $
              outcolor:      Ptr_New(), $
              outfilename:   '', $
              outline:       0B, $
              output:        '', $
              palette:       Ptr_New(), $
              range:         DblArr(2), $
              traditional:   0B, $
              
              ;Contour Properties
              c_annotation:  Ptr_New(), $
              c_charsize:    Ptr_New(), $
              c_charthick:   Ptr_New(), $
              c_colors:      Ptr_New(), $
              c_labels:      Ptr_New(), $
              c_linestyle:   Ptr_New(), $
              c_orientation: Ptr_New(), $
              c_spacing:     Ptr_New(), $
              c_thick:       Ptr_New(), $
              cell_fill:     Ptr_New(), $
              closed:        Ptr_New(), $
              downhill:      Ptr_New(), $
              fill:          Ptr_New(), $
              follow:        Ptr_New(), $
              irregular:     Ptr_New(), $
              isotropic:     Ptr_New(), $
              levels:        Ptr_New(), $
              nlevels:       Ptr_New(), $
              path_data_coords: Ptr_New(), $
              path_double:   Ptr_New(), $
              path_filename: Ptr_New(), $
              path_info:     Ptr_New(), $
              path_xy:       Ptr_New(), $
              resolution:    Ptr_New(), $
              triangulation: Ptr_New()  $
            }
end