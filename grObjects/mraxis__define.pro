; docformat = 'rst'
;
; NAME:
;   MrAxis__Define
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
;   The purpose of this program is to create an axis object object that can drawn on a
;   data plot.
;
; :Categories:
;    Graphics
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
;     Change History::
;       05/10/2013  -   Written by Matthew Argall
;       05/11/2013  -   Made XAXIS, YAXIS, ZAXIS pointers. Values of 0 and 1 have specific
;                           meaning for the Axis procedure. Made the rest of the object
;                           properties pointers for consistency with the cgLibrary. Added
;                           AXLOCATION and AXOFFSET keywords as well as the calcAxLocation
;                           mehtod. [XYZ]Axes do not interfere with each other when begin
;                           set. Added the DATA, DEVICE, and NORMAL keywords for clarity
;                           on positioning. Setting AXLOCATION sets NORMAL=1. - MRA
;       05/13/2013  -   Renamed AXLOCATION to LOCATION and AXOFFSET to OFFSET. - MRA
;       2013-10-25  -   Added the TARGET keyword. - MRA
;       2013/11/20  -   Inherit MrGrAtom. - MRA
;       2014/01/11  -   calcAxisPosition now returns the axis position via positional
;                           parameters. Also, calls to SetProperty have been removed
;                           to avoid infinite drawing loops. Now displayed in a MrWindow
;                           widget. Added the CHARSIZE and [XYZ]MARGIN keywords. Added
;                           the ::doAxis method. - MRA.
;       2014/01/20  -   Renamed from weAxis__define to MrAxis__Define. ::calcAxisPosition
;                           was renamed to ::SetLocation. Added ::SetAxis. The Init, Set,
;                           and GetProperty methods now use axis-neutral keywords. Input
;                           parameters have changed and MrAxis now behaves more like the
;                           Axis function graphic. - MRA
;       2014/01/21  -   OFFSET is now a float, not an integer. - MRA
;       2014/03/10  -   If TARGET is not give, the currently selected graphic is used.
;                           This models the behavior of function graphics. - MRA
;       2014/06/25  -   cgAxis is always called with the SAVE keyword set. If the save
;                           property is not set, then the old coordinate space will be
;                           restored. This allows the ::ConvertCoord method to function.
;                           Can now retrieve location of axis. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to draw the axis on the display window.
;
; :Keywords:
;       NOERASE:        in, optional, type=boolean, default=0
;                       If set, the device will not be erased before drawing. The default
;                           is to clear the display before drawing the graphic.
;-
PRO MrAxis::Draw, $
NOERASE=noerase
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        if n_elements(currentState) gt 0 then cgSetColorState, currentState
        if n_elements(rr) gt 0 then tvlct, rr, gg, bb
        void = cgErrorMsg()
        RETURN
    ENDIF

    IF self.hide THEN RETURN
    
    ;Do this in decomposed color, if possible
    cgSetColorState, 1, CURRENTSTATE=currentState
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8

    ; Draw the axis. Do this in Decomposed color, if possible.
    cgSetColorState, 1, CURRENTSTATE=currentState
    
    ;Get the input color table.
    TVLCT, rr, gg, bb, /Get

    ;Make the axis "sticky" so that it moves around with its target.
    IF MrIsA(*self.location, 'STRING') THEN self -> SetLocation
    
    ;Get the current coordinats
    x_sysvar = !X
    y_sysvar = !Y
    z_sysvar = !Z
    p_sysvar = !P
    
    ;Draw the axis and save its coordinate space
    self -> doAxis
    self -> SaveCoords

    ;Restore the old coordinate space
    IF self.save EQ 0 THEN BEGIN
        !X = x_sysvar
        !Y = y_sysvar
        !Z = z_sysvar
        !P = p_sysvar
    ENDIF
    
    ;Reset the color state
    cgSetColorState, currentState

    ;Restore the color tables.
    IF (!D.Name NE 'Z') THEN TVLCT, rr, gg, bb
END


;+
;   This method draws the axis object.
;
; :Private:
;
; :Keywords:
;       NOERASE:        in, optional, type=boolean, default=0
;                       If set, the device will not be erased before drawing. The default
;                           is to clear the display before drawing the graphic.
;-
PRO MrAxis::doAxis, $
NOERASE=noerase
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Count the number of parameters.
    nparams = n_elements(*self.xloc) eq 0 ? 0 : $
              n_elements(*self.yloc) eq 0 ? 1 : $
              n_elements(*self.zloc) eq 0 ? 2 : $
              3
    
    ;Adjust for postscript output.
    if !d.name eq 'PS' then begin
        charsize  = MrPS_Rescale( self.charsize,  /CHARSIZE)
        charthick = MrPS_Rescale(*self.charthick, /CHARTHICK)
        thick     = MrPS_Rescale(*self.thick,     /THICK)
    endif else begin
        charsize  =  self.charsize
        charthick = *self.charthick
        thick     = *self.thick
    endelse

    ;Create the color
    color = cgColor(*self.color)

    ;Call cgAxis with the correct number of parameters.
    CASE nparams OF
        0: Axis, SAVE    =  1B, $
                 XAXIS   = *self.xaxis, $
                 XLOG    =  self.xlog, $
                 YAXIS   = *self.yaxis, $
                 YLOG    =  self.ylog, $
                 YNOZERO =  self.ynozero, $
                 ZAXIS   = *self.zaxis, $
;                 ZLOG    =  self.zlog, $                    ; !!!!!!!!!!!!!
                 
                 ;cgGraphicsKeywords
                 AM_PM         = *self.am_pm, $              ; !!!!!!!!!!!!!
;                AXISCOLOR     = *self.axiscolor, $
;                BACKGROUND    = *self.background, $
                 CHARSIZE      =       charsize, $
                 CHARTHICK     =       charthick, $
;                CLIP          = *self.clip, $
                 COLOR         =       color, $
                 DATA          =  self.data, $
                 DAYS_OF_WEEK  = *self.days_of_week, $       ; !!!!!!!!!!!!!!
                 DEVICE        =  self.device, $
                 NORMAL        =  self.normal, $
                 FONT          = *self.font, $
                 MONTHS        = *self.months, $             ; !!!!!!!!!!!!!!
;                NOCLIP        = *self.noclip, $
                 NODATA        = *self.nodata, $
                 NOERASE       = *self.noerase, $
;                POSITION      = *self.position, $
;                PSYM          = *self.psym, $
                 SUBTITLE      = *self.subtitle, $
;                SYMSIZE       = *self.symsize, $
                 T3D           = *self.t3d, $
;                THICK         = *self.thick, $
                 TICKLEN       = *self.ticklen, $
;                 TITLE         = *self.title, $             ; !!!!!!!!!!!!!
                 XCHARSIZE     = *self.xcharsize, $
                 XGRIDSTYLE    = *self.xgridstyle, $
                 XMARGIN       = *self.xmargin, $
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
                 YMARGIN       = *self.ymargin, $
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

        ;Keywords commented out above have been removed
        1: Axis, *self.xloc, $
                 SAVE    =  1B, $
                 XAXIS   = *self.xaxis, $
                 XLOG    =  self.xlog, $
                 YAXIS   = *self.yaxis, $
                 YLOG    =  self.ylog, $
                 YNOZERO =  self.ynozero, $
                 ZAXIS   = *self.zaxis, $
                 ZLOG    =  self.zlog, $
                 
                 ;cgGraphicsKeywords
                 CHARSIZE      =       charsize, $
                 CHARTHICK     =       charthick, $
                 COLOR         =       color, $
                 DATA          =  self.data, $
                 DEVICE        =  self.device, $
                 NORMAL        =  self.normal, $
                 FONT          = *self.font, $
                 NODATA        = *self.nodata, $
                 NOERASE       = *self.noerase, $
                 SUBTITLE      = *self.subtitle, $
                 T3D           = *self.t3d, $
                 TICKLEN       = *self.ticklen, $
                 XCHARSIZE     = *self.xcharsize, $
                 XGRIDSTYLE    = *self.xgridstyle, $
                 XMARGIN       = *self.xmargin, $
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
                 YMARGIN       = *self.ymargin, $
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
                   
        2: Axis, *self.xloc, *self.yloc, $
                 SAVE    =  1B, $
                 XAXIS   = *self.xaxis, $
                 XLOG    =  self.xlog, $
                 YAXIS   = *self.yaxis, $
                 YLOG    =  self.ylog, $
                 YNOZERO =  self.ynozero, $
                 ZAXIS   = *self.zaxis, $
                 ZLOG    =  self.zlog, $
                 
                 ;cgGraphicsKeywords
                 CHARSIZE      =       charsize, $
                 CHARTHICK     =       charthick, $
                 COLOR         =       color, $
                 DATA          =  self.data, $
                 DEVICE        =  self.device, $
                 NORMAL        =  self.normal, $
                 FONT          = *self.font, $
                 NODATA        = *self.nodata, $
                 NOERASE       = *self.noerase, $
                 SUBTITLE      = *self.subtitle, $
                 T3D           = *self.t3d, $
                 TICKLEN       = *self.ticklen, $
                 XCHARSIZE     = *self.xcharsize, $
                 XGRIDSTYLE    = *self.xgridstyle, $
                 XMARGIN       = *self.xmargin, $
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
                 YMARGIN       = *self.ymargin, $
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
                   
        3: Axis, *self.xloc, *self.yloc, *self.zloc, $
                 SAVE    =  1B, $
                 XAXIS   = *self.xaxis, $
                 XLOG    =  self.xlog, $
                 YAXIS   = *self.yaxis, $
                 YLOG    =  self.ylog, $
                 YNOZERO =  self.ynozero, $
                 ZAXIS   = *self.zaxis, $
                 ZLOG    =  self.zlog, $
               
                 ;cgGraphicsKeywords
                 CHARSIZE      =       charsize, $
                 CHARTHICK     =       charthick, $
                 COLOR         =       color, $
                 DATA          = *self.data, $
                 DEVICE        = *self.device, $
                 NORMAL        = *self.normal, $
                 FONT          = *self.font, $
                 NODATA        = *self.nodata, $
                 NOERASE       = *self.noerase, $
                 SUBTITLE      = *self.subtitle, $
                 T3D           = *self.t3d, $
                 TICKLEN       = *self.ticklen, $
                 XCHARSIZE     = *self.xcharsize, $
                 XGRIDSTYLE    = *self.xgridstyle, $
                 XMARGIN       = *self.xmargin, $
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
                 YMARGIN       = *self.ymargin, $
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
      ENDCASE
END


;+
;   Initialization method.
;
;   Note:
;       Keywords below are the same as the regular direct graphics keywords, but are
;       axis-independent (i.e., they do not have X, Y, or Z appended to them). Notable
;       exceptions include::
;           AXIS_RANGE  -   replaces [XYZ]RANGE
;           TICKVALUES  -   replaces [XYZ]TICKV
;
;       For more information about any of the keywords, see `IDL's online help page
;       <http://exelisvis.com/docs/graphkeyw.html>`
;
; :Params:
;       DIRECTION:          out, required, type=string
;                           The type of axis to be made. Choices are::
;                               "X" - An x-axis
;                               "Y" - A y-axis
;                               "Z" - A z-axis 
;       
; :Keywords:
;       AXIS_RANGE:         out, optional, type=dblarr(2)
;                           Data range of the axis.
;       CHARSIZE:           out, optional, type=float
;                           Scale factor for IDL's default character size.
;       CURRENT:            out, optional, type=boolean
;                           If set, the axis will be added to the current MrWindow
;                               graphics window.
;       CHARTHICK:          out, optional, type=integer
;                           Multiple of IDL's default character thickness with which to 
;                               draw axis labels and titles.
;       COLOR:              out, optional, type=string
;                           Color of the axis.
;       DATA:               out, optional, type=boolean
;                           If set, the axis will be locked to the data space and will
;                               move about if the data range of `TARGET` changes. The
;                               default is to set the axis at a fixed location.
;       FONT:               out, optional, type=integer
;                           The type of font desired::
;                               -1  -   Hershey vector-drawn fonts
;                                0  -   Device fonts
;                                1  -   TrueType fonts
;       GRIDSTYLE:          out, optional, type=integer
;                           Linestyle used for plot tickmarks and grids (Set `TICKLEN`=1
;                               to create a grid). Options are::
;                                   0 - Solid
;                                   1 - Dotted
;                                   2 - Dashed
;                                   3 - Dash-Dot
;                                   4 - Dash Dot Dot
;                                   5 - Long Dash
;       LOCATION:           out, optional, type=string/intarr(2)/intarr(3)
;                           Location of the axis with respect to its `TARGET`. If a string
;                               is provided, it must be one of the following::
;                                   "BOTTOM"    -   For an x-axis, place below target
;                                   "LEFT"      -   For a  y-axis, place to the left of target
;                                   "RIGHT"     -   For a  y-axis, place to the right of target
;                                   "TOP"       -   for an x-axis, place above target.
;                               If a 2-element vector is given, it specifies the x- and y-
;                               location of the axis in data coordinates. If a 3-element
;                               vector is given, the 3rd element specifies the z-coordinate.
;       LOG:                out, optional, type=boolean
;                           Log-scale the axis.
;       MINOR:              out, optional, type=integer
;                           Number of minor tickmarks to use. The default is determined
;                               by IDL when the plot is made.
;       OFFSET:             out, optional, type=float
;                           Offset from the `TARGET` graphic, in character units.
;       SUBTITLE:           out, optional, type=string
;                           A subtitle to be placed beneath the title.
;       STYLE:              out, optional, type=integer
;                           Bit-wise axis style options include::
;                                1 - Force exact axis
;                                2 - Extend axis range
;                                4 - Suppress entire axis
;                                8 - Suppress box style axis (draw axis on only one side of plot)
;                               16 - Inhibit setting Y-axis minimum to 0 (Y-axis only)
;       T3D:                out, optional, type=boolean
;                           If set, output will be transformed into 3D using !P.T
;       TARGET:             out, optional, type=object
;                           The graphic with which the axis should be associated. If not
;                               give, the currently selected graphic will be chosen.
;       THICK:              out, optional, type=float
;                           Scale factor controlling the thickness of the axis and its
;                               tickmarks
;       TICKDIR:            out, optional, type=byte
;                           Direction of the tick marks. 0 indicates above or to the right
;                               of the axis, 1 indicates below or to the left.
;       TICKFORMAT:         out, optional, type=string/strarr
;                           Format to be applied to tick labels. See the `online help for
;                               more details.
;       TICKINTERVAL:       out, optional, type=integer
;                           Interval between major tickmarks. The default is determined
;                               by IDL when the plot is created.
;       TICKLAYOUT:         out, optional, type=integer
;                           Tick layout style to be applied to each level of the axis.
;                               Optional are 0, 1, or 2. See the online help for details.
;       TICKLEN:            out, optional, type=float
;                           Length of each tickmark, in normal coordinates.
;       TICKNAME:           out, optional, type=string/strarr
;                           Names to be used for each ticklabel.
;       TICKS:              out, optional, type=integer
;                           Number of major tick mark intervals. There are TICKS+1 tick
;                               labels on the axis.
;       TICKUNITS:          out, optional, type=string/strarr
;                           Units to be used for axis tick labelling. See the online
;                               help for details.
;       TICKVALUES:         out, optional, type=number/numeric array
;                           Values at which major tickmarks should be drawn. If not
;                               provided (an neither is `TICKNAME`), then IDL will
;                               determine them when the plot is created.
;       TITLE:              out, optional, type=string
;                           Title to be placed on the axis. See `TICKDIR`.
;       _REF_EXTRA:         out, optional, type=any
;                           Any keyword accepted by MrGrLayout superclass is also
;                               accepted for keyword inheritance.
;-
PRO MrAxis::GetProperty, $
CHARSIZE=charsize, $
DIRECTION=direction, $
LOCATION=location, $
OFFSET = offset, $
SAVE=save, $
TARGET=target, $
TICKDIR=tickdir, $
XYZ_LOC=xyz_loc, $

;Direct Graphics Keywords
AXIS_RANGE=axis_range, $
CHARTHICK=charthick, $
COLOR=color, $
DATA=data, $
DEVICE=device, $
FONT=font, $
GRIDSTYLE=gridstyle, $
LOG=log, $
MINOR=minor, $
NODATA=noData, $
NORMAL=normal, $
SUBTITLE=subtitle, $
STYLE=style, $
T3D=t3d, $
THICK=thick, $
TICKFORMAT=tickformat, $
TICKINTERVAL=tickinterval, $
TICKLAYOUT=ticklayout, $
TICKLEN=ticklen, $
TICKNAME=tickname, $
TICKS=ticks, $
TICKUNITS=tickunits, $
TICKVALUES=tickvalues, $
TITLE=title, $
XCHARSIZE=xcharsize, $
XMARGIN=xmargin, $
YCHARSIZE=ycharsize, $
YMARGIN=ymargin, $
ZCHARSIZE=zcharsize, $
ZMARGIN=zmargin, $
ZVALUE=zvalue, $
_REF_EXTRA=extra
    Compile_Opt StrictArr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Location
    IF Arg_Present(xyz_loc) THEN BEGIN
        IF N_Elements(*self.zloc) GT 0 $
            THEN xyz_loc = [*self.xloc, *self.yloc] $
            ELSE xyz_loc = [*self.xloc, *self.yloc, *self.zloc]
    ENDIF
    
    ;Object Properties
    IF Arg_Present(charsize)  GT 0 THEN charsize  =  self.charsize
    IF Arg_Present(direction) GT 0 THEN direction =  self.direction
    IF Arg_Present(location)  NE 0 THEN location  = *self.location
    IF Arg_Present(offset)    NE 0 THEN offset    =  self.offset
    IF Arg_Present(save)      NE 0 THEN save      =  self.save
    IF Arg_Present(tickdir)   GT 0 THEN tickdir   =  self.tickdir

    IF Arg_Present(target) GT 0 THEN IF Obj_Valid(self.target) GT 0 $
        THEN target = self.target $
        ELSE target = Obj_New()
    
    ;Superclass properties
    self -> MrGrAtom::GetProperty, _STRICT_EXTRA=extra
    self -> MrGraphicsKeywords::GetProperty, CHARTHICK=charthick, $
                                             COLOR=color, $
                                             DATA=data, $
                                             DEVICE=device, $
                                             FONT=font, $
                                             XCHARSIZE=xcharsize, $
                                             YCHARSIZE=ycharsize, $
                                             ZCHARSIZE=zcharsize, $
                                             ZVALUE=zvalue    

    ;Set other axis properties
    CASE self.direction OF
        'X': BEGIN
            IF Arg_Present(gridstyle)    GT 0 THEN gridstyle    = *self.xgridstyle
            IF Arg_Present(log)          GT 0 THEN log          =  self.xlog
            IF Arg_Present(minor)        GT 0 THEN minor        = *self.xminor
            IF Arg_Present(axis_range)   GT 0 THEN axis_range   = *self.xrange
            IF Arg_Present(style)        GT 0 THEN style        = *self.xstyle
            IF Arg_Present(thick)        GT 0 THEN thick        = *self.xthick
            IF Arg_Present(tickformat)   GT 0 THEN tickformat   = *self.xtickformat
            IF Arg_Present(tickinterval) GT 0 THEN tickinterval = *self.xtickinterval
            IF Arg_Present(ticklayout)   GT 0 THEN ticklayout   = *self.xticklayout
            IF Arg_Present(ticklen)      GT 0 THEN ticklen      = *self.xticklen
            IF Arg_Present(tickname)     GT 0 THEN tickname     = *self.xtickname
            IF Arg_Present(ticks)        GT 0 THEN ticks        = *self.xticks
            IF Arg_Present(tickunits)    GT 0 THEN tickunits    = *self.xtickunits
            IF Arg_Present(tickvalues)   GT 0 THEN tickvalues   = *self.xtickv
            IF Arg_Present(title)        GT 0 THEN title        = *self.xtitle
        ENDCASE
        
        'Y': BEGIN
            IF Arg_Present(gridstyle)    GT 0 THEN gridstyle    = *self.ygridstyle
            IF Arg_Present(log)          GT 0 THEN log          =  self.ylog
            IF Arg_Present(minor)        GT 0 THEN minor        = *self.yminor
            IF Arg_Present(axis_range)   GT 0 THEN axis_range   = *self.yrange
            IF Arg_Present(style)        GT 0 THEN style        = *self.ystyle
            IF Arg_Present(thick)        GT 0 THEN thick        = *self.ythick
            IF Arg_Present(tickformat)   GT 0 THEN tickformat   = *self.ytickformat
            IF Arg_Present(tickinterval) GT 0 THEN tickinterval = *self.ytickinterval
            IF Arg_Present(ticklayout)   GT 0 THEN ticklayout   = *self.yticklayout
            IF Arg_Present(ticklen)      GT 0 THEN ticklen      = *self.yticklen
            IF Arg_Present(tickname)     GT 0 THEN tickname     = *self.ytickname
            IF Arg_Present(ticks)        GT 0 THEN ticks        = *self.yticks
            IF Arg_Present(tickunits)    GT 0 THEN tickunits    = *self.ytickunits
            IF Arg_Present(tickvalues)   GT 0 THEN tickvalues   = *self.ytickv
            IF Arg_Present(title)        GT 0 THEN title        = *self.ytitle
        ENDCASE
        
        'Z': BEGIN
            IF Arg_Present(gridstyle)    GT 0 THEN gridstyle    = *self.zgridstyle
            IF Arg_Present(log)          GT 0 THEN log          =  self.zlog
            IF Arg_Present(minor)        GT 0 THEN minor        = *self.zminor
            IF Arg_Present(axis_range)   GT 0 THEN axis_range   = *self.zrange
            IF Arg_Present(style)        GT 0 THEN style        = *self.zstyle
            IF Arg_Present(thick)        GT 0 THEN thick        = *self.zthick
            IF Arg_Present(tickformat)   GT 0 THEN tickformat   = *self.ztickformat
            IF Arg_Present(tickinterval) GT 0 THEN tickinterval = *self.ztickinterval
            IF Arg_Present(ticklayout)   GT 0 THEN ticklayout   = *self.zticklayout
            IF Arg_Present(ticklen)      GT 0 THEN ticklen      = *self.zticklen
            IF Arg_Present(tickname)     GT 0 THEN tickname     = *self.ztickname
            IF Arg_Present(ticks)        GT 0 THEN ticks        = *self.zticks
            IF Arg_Present(tickunits)    GT 0 THEN tickunits    = *self.ztickunits
            IF Arg_Present(tickvalues)   GT 0 THEN tickvalues   = *self.ztickv
            IF Arg_Present(title)        GT 0 THEN title        = *self.ztitle
        ENDCASE
    ENDCASE
END


;+
;   Calculate the axis location.
;
; :Private:
;
; :Params:
;       LOCATION:       out, optional, type=string/intarr(2)/intarr(3)
;                       Location of the axis with respect to its `TARGET`. If a string
;                           is provided, it must be one of the following::
;                               "BOTTOM"    -   For an x-axis, place below target
;                               "LEFT"      -   For a  y-axis, place to the left of target
;                               "RIGHT"     -   For a  y-axis, place to the right of target
;                               "TOP"       -   for an x-axis, place above target.
;                           If a 2-element vector is given, it specifies the x- and y-
;                           location of the axis in data coordinates. If a 3-element
;                           vector is given, the 3rd element specifies the z-coordinate.
;-
PRO MrAxis::SetLocation, location
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Pick the current location by default.
    IF N_Elements(location) EQ 0 THEN location = *self.location

;---------------------------------------------------------------------
;String Location /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    IF MrIsA(location, 'STRING') THEN BEGIN

        ;If a target was given, restore its coordinate system
        IF Obj_Valid(self.target) THEN self.target -> RestoreCoords

        ;Get the reference position.
        refPos = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]

        ;Get the character sizes    
        xchsize = Double(!D.X_CH_Size) / Double(!D.X_Size) * (self.charsize)
        ychsize = Double(!D.Y_CH_Size) / Double(!D.Y_Size) * (self.charsize)

    ;---------------------------------------------------------------------
    ;X-Axis //////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        IF self.direction EQ 0 THEN BEGIN
            CASE StrUpCase(location) OF
                'CENTER': BEGIN
                    xloc = refPos[0] ;ignored, but necessary
                    yloc = (refPos[3] + refPos[1])/2.0
                ENDCASE
            
                'TOP': BEGIN
                    yloc = refPos[3] + self.offset*ychsize
                    xloc = refPos[0] ;ignored, but necessary
                ENDCASE
    
                'BOTTOM': BEGIN
                    yloc = refPos[1] - self.offset*ychsize
                    xloc = refPos[0] ;ignored, but necessary
                ENDCASE
                
                ELSE: Message, "X-Axis locations must be {'BOTTOM' | 'CENTER' | 'TOP'}"
            ENDCASE
        
    ;---------------------------------------------------------------------
    ;Y-Axis //////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        ENDIF ELSE IF self.direction EQ 1 THEN BEGIN

            ;Calculate the axis position
            CASE StrUpCase(location) OF
                'CENTER': BEGIN
                    xloc = (refPos[2] + refPos[0]) / 2.0
                    yloc = refPos[1] ;ignored but useful
                ENDCASE
                
                'RIGHT': BEGIN
                    xloc = refPos[2] + self.offset*xchsize
                    yloc = refPos[1] ;ignored but useful
                ENDCASE
                
                'LEFT': BEGIN
                    xloc = refPos[0] - self.offset*xchsize
                    yloc = refPos[1] ;ignored but useful
                ENDCASE
                ELSE: Message, "Y-Axis locations must be {'CENTER' | 'LEFT' | 'RIGHT'}"
            ENDCASE
        
    ;---------------------------------------------------------------------
    ;Z-Axis //////////////////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        ENDIF ELSE BEGIN
            upLoc = StrUpCase(location)
            
            ;Calculate the axis position
            CASE upLoc[0] OF
                'BOTTOM': yloc = refPos[1] - self.offset*ychsize
                'CENTER': xloc = (refPos[2] + refPos[0]) / 2.0
                'RIGHT':  xloc = refPos[2] + self.offset*xchsize
                'LEFT':   xloc = refPos[0] - self.offset*xchsize
                'TOP':    yloc = refPos[3] + self.offset*ychsize
                ELSE: Message, "Y-Axis locations must be {'CENTER' | 'LEFT' | 'RIGHT'}"
            ENDCASE

            ;Calculate the axis position
            CASE upLoc[1] OF
                'BOTTOM': yloc = refPos[1] - self.offset*ychsize
                'CENTER': yloc = (refPos[3] + refPos[1])/2.0
                'RIGHT':  xloc = refPos[2] + self.offset*xchsize
                'LEFT':   xloc = refPos[0] - self.offset*xchsize
                'TOP':    yloc = refPos[3] + self.offset*ychsize
                ELSE: Message, "Y-Axis locations must be {'CENTER' | 'LEFT' | 'RIGHT'}"
            ENDCASE
            
            ;Make sure both X and Y locations were defined.
            IF N_Elements(xloc) EQ 0 AND N_Elements(yloc) EQ 0 $
                THEN Message, string(FORMAT='("%Invalid LOCATION combinations: [%s, %s]")', upLoc)
        ENDELSE
        
;---------------------------------------------------------------------
;Numeric Location ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ENDIF ELSE IF MrIsA(location, /NUMBER) THEN BEGIN
        nLoc = N_Elements(location)
        
    ;---------------------------------------------------------------------
    ;2-Element Location //////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        IF nLoc EQ 2 THEN BEGIN
            CASE self.direction OF
                'X': BEGIN
                    xloc = location[0]
                    yloc = location[1]
                    zloc = 0
                ENDCASE
                
                'Y': BEGIN
                    xloc = location[0]
                    yloc = location[1]
                    zloc = 0
                ENDCASE
                
                'Z': BEGIN
                    xloc = 0
                    yloc = location[0]
                    zloc = location[1]
                ENDCASE
            ENDCASE
        
    ;---------------------------------------------------------------------
    ;3-Element Location //////////////////////////////////////////////////
    ;---------------------------------------------------------------------
        ENDIF ELSE IF nLoc EQ 3 THEN BEGIN
            xloc = location[0]
            yloc = location[1]
            zloc = location[2]
        
        ;Not {2 | 3} elements?
        ENDIF ELSE Message, 'LOCATION: incorrect number of elements.'
    
    ;Not a {String | Number}?
    ENDIF ELSE Message, 'LOCATION is invalid.'

;---------------------------------------------------------------------
;Set the Location ////////////////////////////////////////////////////
;---------------------------------------------------------------------
    *self.location = StrUpCase(location)
    
    ;If Z is present
    IF N_Elements(zloc) GT 0 THEN BEGIN
        *self.xloc = xloc
        *self.yloc = yloc
        *self.zloc = zloc
        
    ;If Z is not present...
    ENDIF ELSE BEGIN
        *self.xloc = xloc
        *self.yloc = yloc
        Ptr_Free, self.zloc
        self.zloc = Ptr_New(/ALLOCATE_HEAP)
    ENDELSE
    
    ;We are using Normal coordinates.
    self.normal = 1B
    self.data   = 0B
    self.device = 0B
END


;+
;   Set the type of axis to be drawn. This is translated from the DIRECTION and TICKDIR
;   keywords -- see the SetLocation method. Only one of the keywords can be set at a time.
;
; :Private:
;
; :Keywords:
;       XAXIS:          in, optional, type=integer, default=0
;                       Indicates which type of X-axis should be drawn::
;                           0 - Axis below the target with tickmarks directed up.
;                           1 - Axis above the target with tickmarks directed down.
;       YAXIS:          in, optional, type=integer, default=0
;                       Indicates which type of X-axis should be drawn::
;                           0 - Axis to the right of the target with tickmarks directed up.
;                           1 - Axis to the left of the target with tickmarks directed down.
;       ZAXIS:          in, optional, type=integer, default=0
;                       Indicates which type of X-axis should be drawn::
;                           0 - lower (front) right, with tickmarks pointing left
;                           1 - lower (frint) left,  with tickmarks pointing right
;                           2 - upper (back)  left,  with tickmarks pointing right
;                           3 - upper (back)  right, with tickmarks pointing left
;-
PRO MrAxis::SetAxis, $
XAXIS=xaxis, $
YAXIS=yaxis, $
ZAXIS=zaxis
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    nx = N_Elements(xaxis)
    ny = N_Elements(yaxis)
    nz = N_Elements(zaxis)
    
    IF nx + ny + nz EQ 0 THEN BEGIN
        xaxis = self.tickdir
        nx = 1
    ENDIF
    
    IF (nx AND ny GT 0) || (nx AND nz GT 0) || (ny AND nz GT 0) THEN $
        Message, 'The [XYZ]AXIS keywords are mutually exluxive.'

;---------------------------------------------------------------------
;X-Axis //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Make sure the axes do not interfere with one another
    IF nx GT 0 THEN BEGIN
        IF N_Elements(*self.yaxis) NE 0 THEN BEGIN
            Ptr_Free, self.yaxis
            self.yaxis = Ptr_New(/ALLOCATE_HEAP)
        ENDIF
        IF N_Elements(*self.zaxis) NE 0 THEN BEGIN
            Ptr_Free, self.zaxis
            self.zaxis = Ptr_New(/ALLOCATE_HEAP)
        ENDIF
        *self.xaxis = xaxis
    ENDIF
    
;---------------------------------------------------------------------
;Z-Axis //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    IF ny GT 0 THEN BEGIN
        IF N_Elements(*self.xaxis) NE 0 THEN BEGIN
            Ptr_Free, self.xaxis
            self.xaxis = Ptr_New(/ALLOCATE_HEAP)
        ENDIF
        IF N_Elements(*self.zaxis) NE 0 THEN BEGIN
            Ptr_Free, self.zaxis
            self.zaxis = Ptr_New(/ALLOCATE_HEAP)
        ENDIF
        *self.yaxis = yaxis
    ENDIF
    
;---------------------------------------------------------------------
;Z-Axis //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    IF nz GT 0 THEN BEGIN
        IF N_Elements(*self.xaxis) NE 0 THEN BEGIN
            Ptr_Free, self.xaxis
            self.xaxis = Ptr_New(/ALLOCATE_HEAP)
        ENDIF
        IF N_Elements(*self.yaxis) NE 0 THEN BEGIN
            Ptr_Free, self.yaxis
            self.yaxis = Ptr_New(/ALLOCATE_HEAP)
        ENDIF
        *self.zaxis = zaxis
    ENDIF
END


;+
;   Initialization method.
;
;   Note:
;       Keywords below are the same as the regular direct graphics keywords, but are
;       axis-independent (i.e., they do not have X, Y, or Z appended to them). Notable
;       exceptions include::
;           AXIS_RANGE  -   replaces [XYZ]RANGE
;           TICKVALUES  -   replaces [XYZ]TICKV
;
;       For more information about any of the keywords, see `IDL's online help page
;       <http://exelisvis.com/docs/graphkeyw.html>`
;
; :Params:
;       DIRECTION:          in, required, type=string
;                           The type of axis to be made. Choices are::
;                               "X" - An x-axis
;                               "Y" - A y-axis
;                               "Z" - A z-axis 
;       
; :Keywords:
;       AXIS_RANGE:         in, optional, type=dblarr(2)
;                           Data range of the axis.
;       CHARSIZE:           in, optional, type=float
;                           Scale factor for IDL's default character size.
;       CURRENT:            in, optional, type=boolean
;                           If set, the axis will be added to the current MrWindow
;                               graphics window.
;       CHARTHICK:          in, optional, type=integer
;                           Multiple of IDL's default character thickness with which to 
;                               draw axis labels and titles.
;       COLOR:              in, optional, type=string
;                           Color of the axis.
;       DATA:               in, optional, type=boolean
;                           If set, the axis will be locked to the data space and will
;                               move about if the data range of `TARGET` changes. The
;                               default is to set the axis at a fixed location.
;       FONT:               in, optional, type=integer
;                           The type of font desired::
;                               -1  -   Hershey vector-drawn fonts
;                                0  -   Device fonts
;                                1  -   TrueType fonts
;       GRIDSTYLE:          in, optional, type=integer
;                           Linestyle used for plot tickmarks and grids (Set `TICKLEN`=1
;                               to create a grid). Options are::
;                                   0 - Solid
;                                   1 - Dotted
;                                   2 - Dashed
;                                   3 - Dash-Dot
;                                   4 - Dash Dot Dot
;                                   5 - Long Dash
;       LOCATION:           in, optional, type=string/intarr(2)/intarr(3)
;                           Location of the axis with respect to its `TARGET`. If a string
;                               is provided, it must be one of the following::
;                                   "BOTTOM"    -   For an x-axis, place below target
;                                   "LEFT"      -   For a  y-axis, place to the left of target
;                                   "RIGHT"     -   For a  y-axis, place to the right of target
;                                   "TOP"       -   for an x-axis, place above target.
;                               If a 2-element vector is given, it specifies the x- and y-
;                               location of the axis in data coordinates. If a 3-element
;                               vector is given, the 3rd element specifies the z-coordinate.
;       LOG:                in, optional, type=boolean
;                           Log-scale the axis.
;       MINOR:              in, optional, type=integer
;                           Number of minor tickmarks to use. The default is determined
;                               by IDL when the plot is made.
;       OFFSET:             in, optional, type=float
;                           Offset from the `TARGET` graphic, in character units.
;       SUBTITLE:           in, optional, type=string
;                           A subtitle to be placed beneath the title.
;       STYLE:              in, optional, type=integer
;                           Bit-wise axis style options include::
;                                1 - Force exact axis
;                                2 - Extend axis range
;                                4 - Suppress entire axis
;                                8 - Suppress box style axis (draw axis on only one side of plot)
;                               16 - Inhibit setting Y-axis minimum to 0 (Y-axis only)
;       T3D:                in, optional, type=boolean
;                           If set, output will be transformed into 3D using !P.T
;       TARGET:             in, optional, type=object
;                           The graphic with which the axis should be associated. If not
;                               give, the currently selected graphic will be chosen.
;       THICK:              in, optional, type=float
;                           Scale factor controlling the thickness of the axis and its
;                               tickmarks
;       TICKDIR:            in, optional, type=byte
;                           Direction of the tick marks. 0 indicates above or to the right
;                               of the axis, 1 indicates below or to the left.
;       TICKFORMAT:         in, optional, type=string/strarr
;                           Format to be applied to tick labels. See the `online help for
;                               more details.
;       TICKINTERVAL:       in, optional, type=integer
;                           Interval between major tickmarks. The default is determined
;                               by IDL when the plot is created.
;       TICKLAYOUT:         in, optional, type=integer
;                           Tick layout style to be applied to each level of the axis.
;                               Optional are 0, 1, or 2. See the online help for details.
;       TICKLEN:            in, optional, type=float
;                           Length of each tickmark, in normal coordinates.
;       TICKNAME:           in, optional, type=string/strarr
;                           Names to be used for each ticklabel.
;       TICKS:              in, optional, type=integer
;                           Number of major tick mark intervals. There are TICKS+1 tick
;                               labels on the axis.
;       TICKUNITS:          in, optional, type=string/strarr
;                           Units to be used for axis tick labelling. See the online
;                               help for details.
;       TICKVALUES:         in, optional, type=number/numeric array
;                           Values at which major tickmarks should be drawn. If not
;                               provided (an neither is `TICKNAME`), then IDL will
;                               determine them when the plot is created.
;       TITLE:              in, optional, type=string
;                           Title to be placed on the axis. See `TICKDIR`.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrGrLayout superclass is also
;                               accepted for keyword inheritance.
;-
PRO MrAxis::SetProperty, $
CHARSIZE=charsize, $
DIRECTION=direction, $
LOCATION=location, $
OFFSET = offset, $
SAVE=save, $
TARGET=target, $
TICKDIR=tickdir, $

;Direct Graphics Keywords
AXIS_RANGE=axis_range, $
CHARTHICK=charthick, $
COLOR=color, $
DATA=data, $
DEVICE=device, $
FONT=font, $
GRIDSTYLE=gridstyle, $
LOG=log, $
MAJFOR=major, $
MINOR=minor, $
NODATA=noData, $
NORMAL=normal, $
SUBTITLE=subtitle, $
STYLE=style, $
T3D=t3d, $
THICK=thick, $
TICKFORMAT=tickformat, $
TICKINTERVAL=tickinterval, $
TICKLAYOUT=ticklayout, $
TICKLEN=ticklen, $
TICKNAME=tickname, $
TICKS=ticks, $
TICKUNITS=tickunits, $
TICKVALUES=tickvalues, $
TITLE=title, $
XCHARSIZE=xcharsize, $
XMARGIN=xmargin, $
YCHARSIZE=ycharsize, $
YMARGIN=ymargin, $
ZCHARSIZE=zcharsize, $
ZMARGIN=zmargin, $
ZVALUE=zvalue, $
_REF_EXTRA=extra
    Compile_Opt StrictArr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Object Properties
    IF N_Elements(charsize) GT 0 THEN self.charsize = charsize
    IF N_Elements(major)    GT 0 THEN ticks         = major
    IF N_Elements(offset)   GT 0 THEN self.offset   = offset
    IF N_Elements(save)     GT 0 THEN self.save     = keyword_set(save)
    IF N_Elements(xmargin)  GT 0 THEN self.xmargin  = xmargin
    IF N_Elements(ymargin)  GT 0 THEN self.ymargin  = ymargin
    IF N_Elements(zmargin)  GT 0 THEN self.zmargin  = zmargin
    
    ;If a TARGET was given, make sure LOCATION is defined. Default to 'BOTTOM'
    IF N_Elements(target) GT 0 THEN IF Obj_Valid(target) $
        THEN self.target = target $
        ELSE self.target = Obj_New()
    
    ;Set the axis direction. TICKDIR must be checked first.
    IF N_Elements(direction) GT 0 THEN BEGIN
        ;String or integer given?
        IF MrIsA(direction, /SCALAR, 'String') THEN BEGIN
            CASE StrUpCase(direction) OF
                'X': _direction = 0B
                'Y': _direction = 1B
                'Z': _direction = 2B
                else: message, 'Invalid DIRECTION. Must be {"X" | "Y" | "Z"}'
            ENDCASE
        ENDIF ELSE BEGIN
            IF MrIsA(direction, /SCALAR, /INTEGER) $
                THEN _direction = direction $
                ELSE message, 'DIRECTION must be a scalar string or integer.'
        ENDELSE
        
        ;Set the axis
        CASE _direction OF
            0:   self -> SetAxis, XAXIS=self.tickdir
            1:   self -> SetAxis, YAXIS=self.tickdir
            2:   self -> SetAxis, ZAXIS=self.tickdir
            else: message, 'Invalid DIRECTION. Must be {0 | 1 | 2}', /INFORMATIONAL
        ENDCASE
        
        ;Set the property
        self.direction = _direction
    ENDIF
    
    ;Set the tick direction for the proper axis.
    IF N_Elements(tickdir) GT 0 THEN BEGIN
        CASE self.direction OF
            0: self -> SetAxis, XAXIS=tickdir
            1: self -> SetAxis, YAXIS=tickdir
            2: self -> SetAxis, ZAXIS=tickdir
        ENDCASE
        
        self.tickdir = keyword_set(tickdir)
    ENDIF
    
    ;Set the location. DIRECTION must be checked first.
    IF N_Elements(location) GT 0 THEN self -> SetLocation, location

    ;Superclass properties
    self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra
    self -> MrGraphicsKeywords::SetProperty, CHARTHICK=charthick, $
                                             COLOR=color, $
                                             DATA=data, $
                                             DEVICE=device, $
                                             FONT=font, $
                                             XCHARSIZE=xcharsize, $
                                             YCHARSIZE=ycharsize, $
                                             ZCHARSIZE=zcharsize, $
                                             ZVALUE=zvalue

    ;Set other axis properties
    CASE self.direction OF
        0: BEGIN
            IF N_Elements(gridstyle)    GT 0 THEN *self.xgridstyle    = gridstyle
            IF N_Elements(log)          GT 0 THEN  self.xlog          = log
            IF N_Elements(minor)        GT 0 THEN *self.xminor        = minor
            IF N_Elements(axis_range)   GT 0 THEN *self.xrange        = axis_range
            IF N_Elements(style)        GT 0 THEN *self.xstyle        = style
            IF N_Elements(thick)        GT 0 THEN *self.xthick        = thick
            IF N_Elements(tickformat)   GT 0 THEN *self.xtickformat   = tickformat
            IF N_Elements(tickinterval) GT 0 THEN *self.xtickinterval = tickinterval
            IF N_Elements(ticklayout)   GT 0 THEN *self.xticklayout   = ticklayout
            IF N_Elements(ticklen)      GT 0 THEN *self.xticklen      = ticklen
            IF N_Elements(tickname)     GT 0 THEN *self.xtickname     = tickname
            IF N_Elements(ticks)        GT 0 THEN *self.xticks        = ticks
            IF N_Elements(tickunits)    GT 0 THEN *self.xtickunits    = tickunits
            IF N_Elements(tickvalues)   GT 0 THEN *self.xtickv        = tickvalues
            IF N_Elements(title)        GT 0 THEN *self.xtitle        = cgCheckForSymbols(title)
        ENDCASE
        
        1: BEGIN
            IF N_Elements(gridstyle)    GT 0 THEN *self.ygridstyle    = gridstyle
            IF N_Elements(log)          GT 0 THEN  self.ylog          = log
            IF N_Elements(minor)        GT 0 THEN *self.yminor        = minor
            IF N_Elements(axis_range)   GT 0 THEN *self.yrange        = axis_range
            IF N_Elements(style)        GT 0 THEN *self.ystyle        = style
            IF N_Elements(thick)        GT 0 THEN *self.ythick        = thick
            IF N_Elements(tickformat)   GT 0 THEN *self.ytickformat   = tickformat
            IF N_Elements(tickinterval) GT 0 THEN *self.ytickinterval = tickinterval
            IF N_Elements(ticklayout)   GT 0 THEN *self.yticklayout   = ticklayout
            IF N_Elements(ticklen)      GT 0 THEN *self.yticklen      = ticklen
            IF N_Elements(tickname)     GT 0 THEN *self.ytickname     = tickname
            IF N_Elements(ticks)        GT 0 THEN *self.yticks        = ticks
            IF N_Elements(tickunits)    GT 0 THEN *self.ytickunits    = tickunits
            IF N_Elements(tickvalues)   GT 0 THEN *self.ytick         = tickvalues
            IF N_Elements(title)        GT 0 THEN *self.ytitle        = cgCheckForSymbols(title)
        ENDCASE
        
        2: BEGIN
            IF N_Elements(gridstyle)    GT 0 THEN *self.zgridstyle    = gridstyle
            IF N_Elements(log)          GT 0 THEN  self.zlog          = log
            IF N_Elements(minor)        GT 0 THEN *self.zminor        = minor
            IF N_Elements(axis_range)   GT 0 THEN *self.zrange        = axis_range
            IF N_Elements(style)        GT 0 THEN *self.zstyle        = style
            IF N_Elements(thick)        GT 0 THEN *self.zthick        = thick
            IF N_Elements(tickformat)   GT 0 THEN *self.ztickformat   = tickformat
            IF N_Elements(tickinterval) GT 0 THEN *self.ztickinterval = tickinterval
            IF N_Elements(ticklayout)   GT 0 THEN *self.zticklayout   = ticklayout
            IF N_Elements(ticklen)      GT 0 THEN *self.zticklen      = ticklen
            IF N_Elements(tickname)     GT 0 THEN *self.ztickname     = tickname
            IF N_Elements(ticks)        GT 0 THEN *self.zticks        = ticks
            IF N_Elements(tickunits)    GT 0 THEN *self.ztickunits    = tickunits
            IF N_Elements(tickvalues)   GT 0 THEN *self.ztickv        = tickvalues
            IF N_Elements(title)        GT 0 THEN *self.ztitle        = cgCheckForSymbols(title)
        ENDCASE
    ENDCASE
help, *self.location, self.direction, self.tickdir
    ;Draw?
    self.window -> Draw
END


;+
;   The clean-up routine for the object. Destroy pointers, etc.
;-
PRO MrAxis::cleanup
    Compile_Opt StrictArr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Free Pointers
    Ptr_Free, self.location
    Ptr_Free, self.xaxis
    Ptr_Free, self.xloc
    Ptr_Free, self.xmargin
    Ptr_Free, self.yaxis
    Ptr_Free, self.yloc
    Ptr_Free, self.ymargin
    Ptr_Free, self.zaxis
    Ptr_Free, self.zloc
    Ptr_Free, self.zmargin
    
    ;Cleanup the superclasses
    self -> MrGraphicsKeywords::CLEANUP
    self -> MrGrAtom::CLEANUP
END


;+
;   Initialization method.
;
;   Note:
;       Keywords below are the same as the regular direct graphics keywords, but are
;       axis-independent (i.e., they do not have X, Y, or Z appended to them). Notable
;       exceptions include::
;           AXIS_RANGE  -   replaces [XYZ]RANGE
;           TICKVALUES  -   replaces [XYZ]TICKV
;
;       For more information about any of the keywords, see `IDL's online help page
;       <http://exelisvis.com/docs/graphkeyw.html>`
;
; :Params:
;       DIRECTION:          in, required, type=string, default='X'
;                           The type of axis to be made. Choices are::
;                               "X" - An x-axis
;                               "Y" - A y-axis
;                               "Z" - A z-axis 
;       
; :Keywords:
;       AXIS_RANGE:         in, optional, type=dblarr(2)
;                           Data range of the axis.
;       CHARSIZE:           in, optional, type=float, default=1.5
;                           Scale factor for IDL's default character size.
;       CURRENT:            in, optional, type=boolean, default=0
;                           If set, the axis will be added to the current MrWindow
;                               graphics window.
;       CHARTHICK:          in, optional, type=integer, default=1
;                           Multiple of IDL's default character thickness with which to 
;                               draw axis labels and titles.
;       COLOR:              in, optional, type=string, default='Black'
;                           Color of the axis.
;       DATA:               in, optional, type=boolean, default=0
;                           If set, the axis will be locked to the data space and will
;                               move about if the data range of `TARGET` changes. The
;                               default is to set the axis at a fixed location.
;       FONT:               in, optional, type=integer, default=-1
;                           The type of font desired::
;                               -1  -   Hershey vector-drawn fonts
;                                0  -   Device fonts
;                                1  -   TrueType fonts
;       GRIDSTYLE:          in, optional, type=integer, default=0
;                           Linestyle used for plot tickmarks and grids (Set `TICKLEN`=1
;                               to create a grid). Options are::
;                                   0 - Solid
;                                   1 - Dotted
;                                   2 - Dashed
;                                   3 - Dash-Dot
;                                   4 - Dash Dot Dot
;                                   5 - Long Dash
;       LOCATION:           in, optional, type=string/intarr(2)/intarr(3)
;                           Location of the axis with respect to its `TARGET`. If a string
;                               is provided, it must be one of the following::
;                                   "BOTTOM"    -   For an x-axis, place below target
;                                   "LEFT"      -   For a  y-axis, place to the left of target
;                                   "RIGHT"     -   For a  y-axis, place to the right of target
;                                   "TOP"       -   for an x-axis, place above target.
;                               If a 2-element vector is given, it specifies the x- and y-
;                               location of the axis in data coordinates. If a 3-element
;                               vector is given, the 3rd element specifies the z-coordinate.
;       LOG:                in, optional, type=boolean, default=0
;                           Log-scale the axis.
;       MINOR:              in, optional, type=integer
;                           Number of minor tickmarks to use. The default is determined
;                               by IDL when the plot is made.
;       OFFSET:             in, optional, type=float, default=0
;                           Offset from the `TARGET` graphic, in character units.
;       SUBTITLE:           in, optional, type=string, default=""
;                           A subtitle to be placed beneath the title.
;       STYLE:              in, optional, type=integer, default=1
;                           Bit-wise axis style options include::
;                                1 - Force exact axis
;                                2 - Extend axis range
;                                4 - Suppress entire axis
;                                8 - Suppress box style axis (draw axis on only one side of plot)
;                               16 - Inhibit setting Y-axis minimum to 0 (Y-axis only)
;       T3D:                in, optional, type=boolean, default=0
;                           If set, output will be transformed into 3D using !P.T
;       TARGET:             in, optional, type=object, default=current graphic
;                           The graphic with which the axis should be associated. If not
;                               give, the currently selected graphic will be chosen. If
;                               no graphics are selected, the graphic with the highest
;                               ordering will be chosen. If no graphics are found, no
;                               axis will be made.
;       THICK:              in, optional, type=float, default=1.0
;                           Scale factor controlling the thickness of the axis and its
;                               tickmarks
;       TICKDIR:            in, optional, type=byte, default=0
;                           Direction of the tick marks. 0 indicates above or to the right
;                               of the axis, 1 indicates below or to the left. Axis
;                               annotations are drawn on the opposite side of the axis as
;                               the tickmarks.
;       TICKFORMAT:         in, optional, type=string/strarr, default=""
;                           Format to be applied to tick labels. See the `online help for
;                               more details.
;       TICKINTERVAL:       in, optional, type=integer
;                           Interval between major tickmarks. The default is determined
;                               by IDL when the plot is created.
;       TICKLAYOUT:         in, optional, type=integer, default=0
;                           Tick layout style to be applied to each level of the axis.
;                               Optional are 0, 1, or 2. See the online help for details.
;       TICKLEN:            in, optional, type=float, default=0.02
;                           Length of each tickmark, in normal coordinates.
;       TICKNAME:           in, optional, type=string/strarr, default=""
;                           Names to be used for each ticklabel.
;       TICKS:              in, optional, type=integer
;                           Number of major tick mark intervals. There are TICKS+1 tick
;                               labels on the axis.
;       TICKUNITS:          in, optional, type=string/strarr, default=""
;                           Units to be used for axis tick labelling. See the online
;                               help for details.
;       TICKVALUES:         in, optional, type=number/numeric array
;                           Values at which major tickmarks should be drawn. If not
;                               provided (an neither is `TICKNAME`), then IDL will
;                               determine them when the plot is created.
;       TITLE:              in, optional, type=string, default=""
;                           Title to be placed on the axis. See `TICKDIR`.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrGrLayout::SetProperty superclass
;                               method is also accepted for keyword inheritance.
;-
FUNCTION MrAxis::init, direction, $
CHARSIZE=charsize, $
LOCATION=location, $
OFFSET = offset, $
TARGET=target, $
TICKDIR=tickdir, $

;Direct Graphics Keywords
AXIS_RANGE=axis_range, $
CHARTHICK=charthick, $
COLOR=color, $
DATA=data, $
FONT=font, $
GRIDSTYLE=gridstyle, $
LOG=log, $
MAJOR=major, $
MINOR=minor, $
SUBTITLE=subtitle, $
STYLE=sytle, $
T3D=t3d, $
THICK=thick, $
TICKFORMAT=tickformat, $
TICKINTERVAL=tickinterval, $
TICKLAYOUT=ticklayout, $
TICKLEN=ticklen, $
TICKNAME=tickname, $
TICKS=ticks, $
TICKUNITS=tickunits, $
TICKVALUES=tickvalues, $
TITLE=title, $
XCHARSIZE=xcharsize, $
XMARGIN=xmargin, $
YCHARSIZE=ycharsize, $
YMARGIN=ymargin, $
ZCHARSIZE=zcharsize, $
ZMARGIN=zmargin, $
ZVALUE=zvalue, $
_REF_EXTRA=extra



;ANTIALIAS
;CLIP
;COORD_TRANSFORM
;MAJOR
;MINOR
;SHOWTEXT
;SUBGRIDSTYLE
;SUBTICKLEN
;TEXT_COLOR
;TEXT_ORIENTATION
;TEXTPOS
;TICKFONT_NAME
;TICKFONT_SIZE
;TICKFONT_STYLE
;TRANSPARENCY
;UVALUE



    Compile_Opt StrictArr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN, 0
    ENDIF

    ;MAJOR takes precedence over TICKS
    if n_elements(major) gt 0 then ticks = major

;-----------------------------------------------------
;Target, Window, & Superclasses \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Find a single target.
    if n_elements(target) eq 0 then begin
        target = self -> _GetTarget(/ANY, COUNT=nTargets)
        if nTargets eq 0 then message, 'Insert MrAxis failed. No targets available.'
        if nTargets gt 1 then begin
            message, 'More than one target available. Choosing first target.', /INFORMATIONAL
            target = target[0]
        endif
    endif

    ;cgGraphicsKeywords
    IF self -> MrGraphicsKeywords::init() EQ 0 THEN $
        Message, 'Unable to initialize cgGraphicsKeywords.'
        
    ;Superclass
    if self -> MrGrAtom::INIT(TARGET=target, WINREFRESH=refreshIn) eq 0 then $
        message, 'Unable to initialize MrGrAtom'
    
    ;Disable refreshing
    if refreshIn then self.window -> Refresh, /DISABLE

;---------------------------------------------------------------------
;Defaults and Allocate Heap to Pointers //////////////////////////////
;---------------------------------------------------------------------
    
    ;Set Defaults
    current = keyword_set(current)
    log     = keyword_set(log)
    setDefaultValue, charsize, 1.5
    setDefaultValue, direction, 'X'
    setDefaultValue, offset, 0

    ;DIRECTION
    if n_elements(direction) eq 0 then begin
        _direction = 0
        
    endif else if MrIsA(direction, /SCALAR, 'String') then begin
        case strupcase(direction) of
            'X': _direction = 0
            'Y': _direction = 1
            'Z': _direction = 2
            else: message, "DIRECTION must be {'X' | 'Y' | 'Z'}"
        endcase
        
    endif else if MrIsA(direction, /SCALAR, /INTEGER) then begin
        if direction lt 0 || direction gt 3 then message, 'DIRECTION must be {0 | 1 | 2}'
        _direction = direction
        
    endif else begin
         message, 'DIRECTION must be a scalar string or integer.'
    endelse
    
    ;LOCATION
    if n_elements(location) eq 0 then begin
        case _direction of
            0: _location = 'BOTTOM'
            1: _location = 'RIGHT'
            2: _location = ['RIGHT', 'BOTTOM']
            else: message, 'Invalid DIRECTION.'
        endcase
    endif else _location = strupcase(location)
    
    ;TICKDIR
    if n_elements(tickdir) eq 0 then begin
        case _direction of
            0: _tickdir = _location eq 'BOTTOM' ? 0 : 1
            1: _tickdir = _location eq 'LEFT'   ? 0 : 1
            2: _tickdir = 0
            else: message, 'Invalid DIRECTION.'
        endcase
    endif else _tickdir = tickdir
    
    ;Make pointers valid
    self.xloc     = Ptr_New(/ALLOCATE_HEAP)
    self.yloc     = Ptr_New(/ALLOCATE_HEAP)
    self.zloc     = Ptr_New(/ALLOCATE_HEAP)
    self.location = Ptr_New(/ALLOCATE_HEAP)
    self.xaxis    = Ptr_New(/ALLOCATE_HEAP)
    self.xmargin  = Ptr_New(/ALLOCATE_HEAP)
    self.yaxis    = Ptr_New(/ALLOCATE_HEAP)
    self.ymargin  = Ptr_New(/ALLOCATE_HEAP)
    self.zaxis    = Ptr_New(/ALLOCATE_HEAP)
    self.zmargin  = Ptr_New(/ALLOCATE_HEAP)

;---------------------------------------------------------------------
;Set Properties //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    self -> SetProperty, CHARSIZE    = charsize, $
                         DIRECTION   = _direction, $
                         LOCATION    = _location, $
                         OFFSET      = offset, $
                         SAVE        = save, $
                         TARGET      = target, $
                         TICKDIR     = _tickdir, $
                         ;Direct Graphics Keywords
                         AXIS_RANGE   = axis_range, $
                         CHARTHICK    = charthick, $
                         COLOR        = color, $
                         DATA         = data, $
                         DEVICE       = device, $
                         FONT         = font, $
                         GRIDSTYLE    = gridstyle, $
                         LOG          = log, $
                         MINOR        = minor, $
                         NODATA       = noData, $
                         NORMAL       = normal, $
                         SUBTITLE     = subtitle, $
                         STYLE        = sytle, $
                         T3D          = t3d, $
                         THICK        = thick, $
                         TICKFORMAT   = tickformat, $
                         TICKINTERVAL = tickinterval, $
                         TICKLAYOUT   = ticklayout, $
                         TICKLEN      = ticklen, $
                         TICKNAME     = tickname, $
                         TICKS        = ticks, $
                         TICKUNITS    = tickunits, $
                         TICKVALUES   = tickvalues, $
                         TITLE        = title, $
                         XCHARSIZE    = xcharsize, $
                         XMARGIN      = xmargin, $
                         YCHARSIZE    = ycharsize, $
                         YMARGIN      = ymargin, $
                         ZCHARSIZE    = zcharsize, $
                         ZMARGIN      = zmargin, $
                         ZVALUE       = zvalue, $
                        _EXTRA        = extra
    
    ;Refersh the graphics window
    if refreshIn then self -> Refresh

    Return, 1
END


;+
; The class definition module for the object.
; 
; :Params:
;     CLASS:        out, optional, type=struct
;                   The class definition as a structure variable. Occasionally useful.
;-
PRO MrAxis__define, class
    
    class = { MrAxis, $
              inherits MrGrAtom, $
              inherits MrGraphicsKeywords, $
            
              xloc:      Ptr_New(), $
              yloc:      Ptr_New(), $
              zloc:      Ptr_New(), $
              charsize:  0.0, $
              direction: 0B, $
              location:  ptr_new(), $
              offset:    0.0, $
              save:      0B, $
              target:    Obj_New(), $
              tickdir:   0B, $
              xaxis:     Ptr_New(), $
              xlog:      0B, $
              xmargin:   Ptr_New(), $
              ynozero:   0B, $
              yaxis:     Ptr_New(), $
              ylog:      0B, $
              ymargin:   Ptr_New(), $
              zaxis:     Ptr_New(), $
              zlog:      0B $
            }
END