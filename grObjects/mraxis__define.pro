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
;-
;*****************************************************************************************
;+
;
;-
PRO MrAxis::Draw, $
NOERASE=noerase
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF

    IF self.hide THEN RETURN

    ;Make the axis "sticky" so that it moves around with the plot it is associated with.
    ;   This must be done before checking the number of parameters below. If OFFSET=0,
    ;   then the position is nullified and determined automatically by cgAxis.
    IF MrIsA(*self.location, 'STRING') THEN self -> SetLocation
    
    self -> doAxis
    self -> SaveCoords
END


;+
; This method draws the axis object.
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
    nparams = 0
    IF N_Elements(*self.xloc) NE 0 THEN nparams = nparams + 1
    IF N_Elements(*self.yloc) NE 0 THEN nparams = nparams + 1
    IF N_Elements(*self.zloc) NE 0 THEN nparams = nparams + 1

    ;Call cgAxis with the correct number of parameters.
    CASE nparams OF
        0: cgAxis, SAVE  =  self.save, $
                   XAXIS = *self.xaxis, $
                   XLOG  =  self.xlog, $
                   YAXIS = *self.yaxis, $
                   YLOG  =  self.ylog, $
                   ZAXIS = *self.zaxis, $
                   ZLOG  =  self.zlog, $
                   
                   ;cgGraphicsKeywords
;                  AXISCOLOR     = *self.axiscolor, $
;                  BACKGROUND    = *self.background, $
                   CHARSIZE      = *self.charsize, $
                   CHARTHICK     = *self.charthick, $
;                  CLIP          = *self.clip, $
                   COLOR         = *self.color, $
                   DATA          =  self.data, $
                   DEVICE        =  self.device, $
                   NORMAL        =  self.normal, $
                   FONT          = *self.font, $
;                  NOCLIP        = *self.noclip, $
                   NODATA        = *self.nodata, $
                   NOERASE       = *self.noerase, $
;                  POSITION      = *self.position, $
;                  PSYM          = *self.psym, $
                   SUBTITLE      = *self.subtitle, $
;                  SYMSIZE       = *self.symsize, $
                   T3D           = *self.t3d, $
;                  THICK         = *self.thick, $
                   TICKLEN       = *self.ticklen, $
                   TITLE         = *self.title, $
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
        1: cgAxis, *self.xloc, $
                   SAVE  =  self.save, $
                   XAXIS = *self.xaxis, $
                   XLOG  =  self.xlog, $
                   YAXIS = *self.yaxis, $
                   YLOG  =  self.ylog, $
                   ZAXIS = *self.zaxis, $
                   ZLOG  =  self.zlog, $
                   
                   ;cgGraphicsKeywords
                   CHARSIZE      =  self.charsize, $
                   CHARTHICK     = *self.charthick, $
                   COLOR         = *self.color, $
                   DATA          =  self.data, $
                   DEVICE        =  self.device, $
                   NORMAL        =  self.normal, $
                   FONT          = *self.font, $
                   NODATA        = *self.nodata, $
                   NOERASE       = *self.noerase, $
                   SUBTITLE      = *self.subtitle, $
                   T3D           = *self.t3d, $
                   TICKLEN       = *self.ticklen, $
                   TITLE         = *self.title, $
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
                   
        2: cgAxis, *self.xloc, *self.yloc, $
                   SAVE  =  self.save, $
                   XAXIS = *self.xaxis, $
                   XLOG  =  self.xlog, $
                   YAXIS = *self.yaxis, $
                   YLOG  =  self.ylog, $
                   ZAXIS = *self.zaxis, $
                   ZLOG  =  self.zlog, $
                   
                   ;cgGraphicsKeywords
                   CHARSIZE      =  self.charsize, $
                   CHARTHICK     = *self.charthick, $
                   COLOR         = *self.color, $
                   DATA          =  self.data, $
                   DEVICE        =  self.device, $
                   NORMAL        =  self.normal, $
                   FONT          = *self.font, $
                   NODATA        = *self.nodata, $
                   NOERASE       = *self.noerase, $
                   SUBTITLE      = *self.subtitle, $
                   T3D           = *self.t3d, $
                   TICKLEN       = *self.ticklen, $
                   TITLE         = *self.title, $
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
                   
        3: cgAxis, *self.xloc, *self.yloc, *self.zloc, $
                   SAVE  =  self.save, $
                   XAXIS = *self.xaxis, $
                   XLOG  =  self.xlog, $
                   YAXIS = *self.yaxis, $
                   YLOG  =  self.ylog, $
                   ZAXIS = *self.zaxis, $
                   ZLOG  =  self.zlog, $
                   
                   ;cgGraphicsKeywords
                   CHARSIZE      =  self.charsize, $
                   CHARTHICK     = *self.charthick, $
                   COLOR         = *self.color, $
                   DATA          = *self.data, $
                   DEVICE        = *self.device, $
                   NORMAL        = *self.normal, $
                   FONT          = *self.font, $
                   NODATA        = *self.nodata, $
                   NOERASE       = *self.noerase, $
                   SUBTITLE      = *self.subtitle, $
                   T3D           = *self.t3d, $
                   TICKLEN       = *self.ticklen, $
                   TITLE         = *self.title, $
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
;   This method obtains the current properties of the object. 
; 
; :Keywords:
;     xloc: out, optional, type=depends
;         The X location of the axis. 
;     yloc: out, optional, type=depends
;         The Y location of the axis. 
;     zloc: out, optional, type=depends
;         The Z location of the axis. 
;     location, out, optional, type=string
;         The location at which to place the axis.
;     offset, out, optional, type=integer
;         The offset, in character units, from the original axis set up by a previous
;     save: out, optional, type=boolean
;         Set this keyword to save the scaling parameters set by the axis for subsequent use.
;     title: out, optional, type=string
;         The title or annotation that appears on the axis. Equivalent to setting XTITLE
;         wtih `XAXIS`, the YTITLE with `YAXIS`, or ZTITLE with `ZAXIS`.
;     xaxis: out, optional, type=integer
;         If set to 0, the axis is drawn under the plot with the tick marks pointing up; if set 
;         to 1, the axis is drawn on top of the plot with the tick marks pointing down.
;     xlog: out, optional, type=boolean
;         Set this keyword to specify a logarithmic axis type.
;     yaxis: out, optional, type=integer
;         If set to 0, the axis is drawn on the left of the plot with the tick marks pointing 
;         to the right. If set to 1, the axis is drawn on the right of the plot with the tick 
;         marks pointing to the left.
;     ylog: out, optional, type=boolean
;         Set this keyword to specify a logarithmic axis type.
;     zaxis: out, optional, type=integer
;         Set to 0-3 to position the Z axis in various locatons. See the AXIS documentation.
;     zlog: out, optional, type=boolean
;         Set this keyword to specify a logarithmic axis type.
;     _ref_extra: out, optional
;          Any keywords appropriate for the superclass MrGrAtom::GetProperty.
;-
PRO MrAxis::GetProperty, $
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
MINOR=minor, $
NODATA=noData, $
NORMAL=normal, $
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
    Compile_Opt StrictArr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
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
    self -> weGraphicsKeywords::GetProperty, CHARTHICK=charthick, $
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
            IF N_Elements(gridstyle)    GT 0 THEN gridstyle    = *self.xgridstyle
            IF N_Elements(log)          GT 0 THEN log          =  self.xlog
            IF N_Elements(minor)        GT 0 THEN minor        = *self.xminor
            IF N_Elements(axis_range)   GT 0 THEN axis         = *self.xrange_range
            IF N_Elements(style)        GT 0 THEN style        = *self.xstyle
            IF N_Elements(thick)        GT 0 THEN thick        = *self.xthick
            IF N_Elements(tickformat)   GT 0 THEN tickformat   = *self.xtickformat
            IF N_Elements(tickinterval) GT 0 THEN tickinterval = *self.xtickinterval
            IF N_Elements(ticklayout)   GT 0 THEN ticklayout   = *self.xticklayout
            IF N_Elements(ticklen)      GT 0 THEN ticklen      = *self.xticklen
            IF N_Elements(tickname)     GT 0 THEN tickname     = *self.xtickname
            IF N_Elements(ticks)        GT 0 THEN ticks        = *self.xticks
            IF N_Elements(tickunits)    GT 0 THEN tickunits    = *self.xtickunits
            IF N_Elements(tickvalues)   GT 0 THEN tickvalues   = *self.xtickvalues
            IF N_Elements(title)        GT 0 THEN title        = *self.xtitle
        ENDCASE
        
        'Y': BEGIN
            IF N_Elements(gridstyle)    GT 0 THEN gridstyle    = *self.ygridstyle
            IF N_Elements(log)          GT 0 THEN log          =  self.ylog
            IF N_Elements(minor)        GT 0 THEN minor        = *self.yminor
            IF N_Elements(axis_range)   GT 0 THEN axis         = *self.yrange_range
            IF N_Elements(style)        GT 0 THEN style        = *self.ystyle
            IF N_Elements(thick)        GT 0 THEN thick        = *self.ythick
            IF N_Elements(tickformat)   GT 0 THEN tickformat   = *self.ytickformat
            IF N_Elements(tickinterval) GT 0 THEN tickinterval = *self.ytickinterval
            IF N_Elements(ticklayout)   GT 0 THEN ticklayout   = *self.yticklayout
            IF N_Elements(ticklen)      GT 0 THEN ticklen      = *self.yticklen
            IF N_Elements(tickname)     GT 0 THEN tickname     = *self.ytickname
            IF N_Elements(ticks)        GT 0 THEN ticks        = *self.yticks
            IF N_Elements(tickunits)    GT 0 THEN tickunits    = *self.ytickunits
            IF N_Elements(tickvalues)   GT 0 THEN tickvalues   = *self.ytickvalues
            IF N_Elements(title)        GT 0 THEN title        = *self.ytitle
        ENDCASE
        
        'Z': BEGIN
            IF N_Elements(gridstyle)    GT 0 THEN gridstyle    = *self.zgridstyle
            IF N_Elements(log)          GT 0 THEN log          =  self.zlog
            IF N_Elements(minor)        GT 0 THEN minor        = *self.zminor
            IF N_Elements(axis_range)   GT 0 THEN axis         = *self.zrange_range
            IF N_Elements(style)        GT 0 THEN style        = *self.zstyle
            IF N_Elements(thick)        GT 0 THEN thick        = *self.zthick
            IF N_Elements(tickformat)   GT 0 THEN tickformat   = *self.ztickformat
            IF N_Elements(tickinterval) GT 0 THEN tickinterval = *self.ztickinterval
            IF N_Elements(ticklayout)   GT 0 THEN ticklayout   = *self.zticklayout
            IF N_Elements(ticklen)      GT 0 THEN ticklen      = *self.zticklen
            IF N_Elements(tickname)     GT 0 THEN tickname     = *self.ztickname
            IF N_Elements(ticks)        GT 0 THEN ticks        = *self.zticks
            IF N_Elements(tickunits)    GT 0 THEN tickunits    = *self.ztickunits
            IF N_Elements(tickvalues)   GT 0 THEN tickvalues   = *self.ztickvalues
            IF N_Elements(title)        GT 0 THEN title        = *self.ztitle
        ENDCASE
    ENDCASE
END


;+
;   Calculate the axis location
;
; :Private:
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
        IF self.direction EQ 'X' THEN BEGIN
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
        ENDIF ELSE IF self.direction EQ 'Y' THEN BEGIN

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
                    yloc = location[0]
                    zloc = location[1]
                ENDCASE
                
                'Y': BEGIN
                    xloc = location[0]
                    zloc = location[1]
                ENDCASE
                
                'Z': BEGIN
                    xloc = location[0]
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
    self.data = 0B
    self.device = 0B
END


;+
;   Calculate the Axis position
;
; :Private:
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
;   This method sets the properties of the object.
;
; :Keywords:
;     xloc: in, optional, type=depends
;         The X location of the axis. 
;     yloc: in, optional, type=depends
;         The Y location of the axis. 
;     zloc: in, optional, type=depends
;         The Z location of the axis.
;     location, in, optional, type=string
;         The location at which to place the axis.
;     offset, in, optional, type=integer
;         The offset, in character units, from the original axis set up by a previous
;         call to the Plot procedure at which to place new axis.
;     save: in, optional, type=boolean
;         Set this keyword to save the scaling parameters set by the axis for subsequent use.
;     title: in, optional, type=string
;         The title or annotation that appears on the axis. Equivalent to setting XTITLE
;         wtih `XAXIS`, the YTITLE with `YAXIS`, or ZTITLE with `ZAXIS`.
;     xaxis: in, optional, type=integer
;         If set to 0, the axis is drawn under the plot with the tick marks pointing up; if set 
;         to 1, the axis is drawn on top of the plot with the tick marks pointing down.
;     xlog: in, optional, type=boolean
;         Set this keyword to specify a logarithmic axis type.
;     yaxis: in, optional, type=integer
;         If set to 0, the axis is drawn on the left of the plot with the tick marks pointing 
;         to the right. If set to 1, the axis is drawn on the right of the plot with the tick 
;         marks pointing to the left.
;     ylog: in, optional, type=boolean
;         Set this keyword to specify a logarithmic axis type.
;     zaxis: in, optional, type=integer
;         Set to 0-3 to position the Z axis in various locatons. See the AXIS documentation.
;     zlog: in, optional, type=boolean
;         Set this keyword to specify a logarithmic axis type.
;     _ref_extra: in, optional
;          Any keywords appropriate for the MrGrAtom superclass.
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
MINOR=minor, $
NODATA=noData, $
NORMAL=normal, $
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
    IF N_Elements(offset)   GT 0 THEN self.offset   = offset
    IF N_Elements(save)     GT 0 THEN self.save     = keyword_set(save)
    IF N_Elements(tickdir)  GT 0 THEN self.tickdir  = keyword_set(tickdir)
    IF N_Elements(xmargin)  GT 0 THEN self.xmargin  = xmargin
    IF N_Elements(ymargin)  GT 0 THEN self.ymargin  = ymargin
    IF N_Elements(zmargin)  GT 0 THEN self.zmargin  = zmargin
    
    ;If a TARGET was given, make sure LOCATION is defined. Default to 'BOTTOM'
    IF N_Elements(target) GT 0 THEN IF Obj_Valid(target) $
        THEN self.target = target $
        ELSE self.target = Obj_New()
    
    ;Set the tick direction for the proper axis.
    IF N_Elements(tickdir) GT 0 THEN BEGIN
        CASE StrUpCase(direction) OF
            'X': *self.xaxis = tickdir
            'Y': *self.yaxis = tickdir
            'Z': *self.zaxis = tickdir
        ENDCASE
        
        self.tickdir = keyword_set(tickdir)
    ENDIF
    
    ;Set the axis direction. TICKDIR must be checked first.
    IF N_Elements(direction) GT 0 THEN BEGIN
        CASE StrUpCase(direction) OF
            'X': self -> SetAxis, XAXIS=self.tickdir
            'Y': self -> SetAxis, YAXIS=self.tickdir
            'Z': self -> SetAxis, ZAXIS=self.tickdir
        ENDCASE
        
        self.direction = StrUpCase(direction)
    ENDIF
    
    ;Set the location. DIRECTION must be checked first.
    IF N_Elements(location) GT 0 THEN self -> SetLocation, location

    ;Superclass properties
    self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra
    self -> weGraphicsKeywords::SetProperty, CHARTHICK=charthick, $
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
            IF N_Elements(tickvalues)   GT 0 THEN *self.xtickvalues   = tickvalues
            IF N_Elements(title)        GT 0 THEN *self.xtitle        = title
        ENDCASE
        
        'Y': BEGIN
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
            IF N_Elements(tickvalues)   GT 0 THEN *self.ytickvalues   = tickvalues
            IF N_Elements(title)        GT 0 THEN *self.ytitle        = title
        ENDCASE
        
        'Z': BEGIN
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
            IF N_Elements(tickvalues)   GT 0 THEN *self.ztickvalues   = tickvalues
            IF N_Elements(title)        GT 0 THEN *self.ztitle        = title
        ENDCASE
    ENDCASE

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
    self -> weGraphicsKeywords::CLEANUP
    self -> MrGrAtom::CLEANUP
END


;+
; :Params:
;    xloc: in, optional, type=depends
;       The X location of the axis. 
;    yloc: in, optional, type=depends
;       The Y location of the axis. 
;    zloc: in, optional, type=depends
;       The Z location of the axis. 
;       
; :Keywords:
;     data: in, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in data coordinates. Data coordinates
;         are the default, unless DEVICE or NORMAL is set.
;     device: in, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in device coordinates.
;     location, in, optional, type=string, default=''
;         The location at which to place the axis. Options are::
;           "BOTTOM"    -   X-axis at the bottom of the plot.
;           "TOP"       -   X-axis at the top of the plot.
;           "LEFT"      -   Y-axis at the left of the plot.
;           "RIGHT"     -   Y-axis at the right of the plot.
;         Setting this keyword also sets `NORMAL`=1.
;     normal: in, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in normalized coordinates.
;     offset, in, optional, type=integer, default=0
;         The offset, in character units, from the original axis set up by a previous
;         call to the Plot procedure at which to place new axis. Used only with `LOCATION`.
;         An OFFSET of 0 is equivalent to not specifying XLOC, YLOC, or ZLOC.
;     save: in, optional, type=boolean
;         Set this keyword to save the scaling parameters set by the axis for subsequent use.
;     title: in, optional, type=string, default=""
;         The title or annotation that appears on the axis. Equivalent to setting XTITLE
;         wtih `XAXIS`, the YTITLE with `YAXIS`, or ZTITLE with `ZAXIS`.
;     xaxis: in, optional, type=integer, default=0
;         If set to 0, the axis is drawn under the plot with the tick marks pointing up; if set 
;         to 1, the axis is drawn on top of the plot with the tick marks pointing down.
;     xlog: in, optional, type=boolean, default=0
;         Set this keyword to specify a logarithmic axis type.
;     yaxis: in, optional, type=integer, default=0
;         If set to 0, the axis is drawn on the left of the plot with the tick marks pointing 
;         to the right. If set to 1, the axis is drawn on the right of the plot with the tick 
;         marks pointing to the left.
;     ylog: in, optional, type=boolean, default=0
;         Set this keyword to specify a logarithmic axis type.
;     zaxis: in, optional, type=integer, default=0
;         Set to 0-3 to position the Z axis in various locatons. See the AXIS documentation.
;     zlog: in, optional, type=boolean, default=0
;         Set this keyword to specify a logarithmic axis type.
;     _ref_extra: in, optional
;          Any keywords appropriate for the AXIS command or MrAxis.
;-
FUNCTION MrAxis::init, direction, $
CHARSIZE=charsize, $
CURRENT=current, $
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
MINOR=minor, $
NODATA=noData, $
NORMAL=normal, $
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
    Compile_Opt StrictArr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN, 0
    ENDIF

    ;cgGraphicsKeywords
    IF self -> weGraphicsKeywords::init(_EXTRA=extra) EQ 0 THEN $
        Message, 'Unable to initialize cgGraphicsKeywords.'

;---------------------------------------------------------------------
;Defaults and Allocate Heap to Pointers //////////////////////////////
;---------------------------------------------------------------------
    
    ;Set Defaults
    current = keyword_set(current)
    log = keyword_set(log)
    tickdir = keyword_set(tickdir)
    setDefaultValue, charsize, 1.5
    setDefaultValue, direction, 'X'
    setDefaultValue, offset, 0

    ;Default location    
    case strupcase(direction) of
        'X': setDefaultVAlue, location, 'BOTTOM'
        'Y': setDefaultValue, location, 'RIGHT'
        'Z': setDefualtValue, location, ['RIGHT', 'BOTTOM']
        else: message, "DIRECTION must be {'X' | 'Y' | 'Z'}"
    endcase
    
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
    
    ;Objects
    self.target = Obj_New()

;---------------------------------------------------------------------
;Window //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

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

    refresh_in = 1
    if keyword_set(current) then begin
        theWin = GetMrWindows(/CURRENT)
        theWin -> GetProperty, REFRESH=refresh_in
        theWin -> Refresh, /DISABLE
    endif

    ;Graphic Atom
    if self -> MrGrAtom::INIT(CURRENT=current, _EXTRA=extra) eq 0 then $
        message, 'Unable to initialize MrGrAtom.'

;---------------------------------------------------------------------
;Set Properties //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    self -> SetProperty, CHARSIZE=charsize, $
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
                         MINOR=minor, $
                         NODATA=noData, $
                         NORMAL=normal, $
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
                        _EXTRA=extra
    
    ;Refersh the graphics window
    self.window -> Refresh, DISABLE=~refresh_in
                         
    Return, 1
END


;+
; The class definition module for the object.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO MrAxis__define, class
    
    class = { MrAxis, $
              inherits MrGrAtom, $
              inherits weGraphicsKeywords, $
            
              xloc: Ptr_New(), $
              yloc: Ptr_New(), $
              zloc: Ptr_New(), $
              charsize: 0.0, $
              direction: '', $
              location: Ptr_New(), $
              offset: 0, $
              save: 0B, $
              target: Obj_New(), $
              tickdir: 0B, $
              xaxis: Ptr_New(), $
              xlog: 0B, $
              xmargin: Ptr_New(), $
              yaxis: Ptr_New(), $
              ylog: 0B, $
              ymargin: Ptr_New(), $
              zaxis: Ptr_New(), $
              zlog: 0B $
            }
END