; docformat = 'rst'
;
; NAME:
;   weAxis__Define
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
; :Examples:
;   Add a y-axis on the right side of a weZPlot plot::
;       myAxis = obj_new('weAxis', /YAxis, Color='red', YRange=[-500, 500], /Save)
;       myPlot = obj_new('weZPlot', cgDemoData(1), YStyle=8, Position=[0.1, 0.1, 0.85, 0.9], $
;                        AXES=myAxis)
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
;         
;-
;*****************************************************************************************
;+
;   Calculate the Axis position
;-
PRO weAxis::calcAxisPosition, xloc, yloc
    Compile_Opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Return if no location is provided.
    IF self.location EQ "" THEN RETURN
    
    ;If AXOFFSET=0, let cgAxis determine the positions
    IF self.offset EQ 0 THEN BEGIN
        ;Undefine the locations and return.
        Ptr_Free, self.xloc
        Ptr_Free, self.yloc
        Ptr_Free, self.zloc
        self.xloc = Ptr_New(/ALLOCATE_HEAP)
        self.yloc = Ptr_New(/ALLOCATE_HEAP)
        self.zloc = Ptr_New(/ALLOCATE_HEAP)
        RETURN
    ENDIF
    
    ;Get the reference position for placing the axis.
    IF Obj_Valid(self.target) $
        THEN self.target -> GetProperty, POSITION=refPos $
        ELSE refPos = [!X.Window[0], !Y.Window[1], !X.Window[1], !Y.Window[1]]

    ;Get the character sizes    
    xchsize = Double(!D.X_CH_Size) / Double(!D.X_Size) * (*self.charsize)
    ychsize = Double(!D.Y_CH_Size) / Double(!D.Y_Size) * (*self.charsize)

    ;Calculate the axis position
    CASE StrUpCase(self.location) OF
        'RIGHT': xloc = refPos[2] + self.offset*xchsize
        'LEFT':  xloc  = refPos[0] - self.offset*xchsize
        'TOP': BEGIN
            yloc = refPos[3] + self.offset*ychsize
            xloc = refPos[0] ;ignored, but necessary
        ENDCASE
        
        'BOTTOM': BEGIN
            yloc = refPos[1] - self.offset*ychsize
            xloc = refPos[0] ;ignored, but necessary
        ENDCASE
    ENDCASE
END


;+
;
;-
PRO weAxis::Draw
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
    IF self.location NE "" OR Obj_Valid(self.target) THEN BEGIN
        self -> calcAxisPosition, xloc, yloc
        IF N_Elements(xloc) GT 0 THEN *self.xloc = xloc
        IF N_ELEMENTS(yloc) GT 0 THEN *self.yloc = yloc
    ENDIF ELSE BEGIN
        xloc = *self.xloc
        yloc = *self.yloc
        zloc = *self.zloc
    ENDELSE
    
    self -> doAxis
    self -> SaveCoords
END


;+
; This method draws the axis object.
;-
PRO weAxis::doAxis, $
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
        0: cgAxis, SAVE=*self.save, $
                   XAXIS=*self.xaxis, $
                   XLOG=*self.xlog, $
                   YAXIS=*self.yaxis, $
                   YLOG=*self.ylog, $
                   ZAXIS=*self.zaxis, $
                   ZLOG=*self.zlog, $
                   
                   ;cgGraphicsKeywords
;                  AXISCOLOR=*self.axiscolor, $
;                  BACKGROUND=*self.background, $
                   CHARSIZE=*self.charsize, $
                   CHARTHICK=*self.charthick, $
;                  CLIP=*self.clip, $
                   COLOR=*self.color, $
                   DATA=*self.data, $
                   DEVICE=*self.device, $
                   NORMAL=*self.normal, $
                   FONT=*self.font, $
;                  NOCLIP=*self.noclip, $
                   NODATA=*self.nodata, $
                   NOERASE=*self.noerase, $
;                  POSITION=*self.position, $
;                  PSYM=*self.psym, $
                   SUBTITLE=*self.subtitle, $
;                  SYMSIZE=*self.symsize, $
                   T3D=*self.t3d, $
;                  THICK=*self.thick, $
                   TICKLEN=*self.ticklen, $
                   TITLE=*self.title, $
                   XCHARSIZE=*self.xcharsize, $
                   XGRIDSTYLE=*self.xgridstyle, $
                   XMARGIN=*self.xmargin, $
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
                   YMARGIN=*self.ymargin, $
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
                   YTITLE=*self.ytitle, $
                   ZCHARSIZE=*self.zcharsize, $
                   ZGRIDSTYLE=*self.zgridstyle, $
                   ZMARGIN=*self.zmargin, $
                   ZMINOR=*self.zminor, $
                   ZRANGE=*self.zrange, $
                   ZSTYLE=*self.zstyle, $
                   ZTHICK=*self.zthick, $
                   ZTICK_GET=*self.ztick_get, $
                   ZTICKFORMAT=*self.ztickformat, $
                   ZTICKINTERVAL=*self.ztickinterval, $
                   ZTICKLAYOUT=*self.zticklayout, $
                   ZTICKLEN=*self.zticklen, $
                   ZTICKNAME=*self.ztickname, $
                   ZTICKS=*self.zticks, $
                   ZTICKUNITS=*self.ztickunits, $
                   ZTICKV=*self.ztickv, $
                   ZTITLE=*self.ztitle, $
                   ZVALUE=*self.zvalue

        ;Keywords commented out above have been removed
        1: cgAxis, xloc, $
                   SAVE=*self.save, $
                   XAXIS=*self.xaxis, $
                   XLOG=*self.xlog, $
                   YAXIS=*self.yaxis, $
                   YLOG=*self.ylog, $
                   ZAXIS=*self.zaxis, $
                   ZLOG=*self.zlog, $
                   
                   ;cgGraphicsKeywords
                   CHARSIZE=*self.charsize, $
                   CHARTHICK=*self.charthick, $
                   COLOR=*self.color, $
                   DATA=*self.data, $
                   DEVICE=*self.device, $
                   NORMAL=*self.normal, $
                   FONT=*self.font, $
                   NODATA=*self.nodata, $
                   NOERASE=*self.noerase, $
                   SUBTITLE=*self.subtitle, $
                   T3D=*self.t3d, $
                   TICKLEN=*self.ticklen, $
                   TITLE=*self.title, $
                   XCHARSIZE=*self.xcharsize, $
                   XGRIDSTYLE=*self.xgridstyle, $
                   XMARGIN=*self.xmargin, $
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
                   YMARGIN=*self.ymargin, $
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
                   YTITLE=*self.ytitle, $
                   ZCHARSIZE=*self.zcharsize, $
                   ZGRIDSTYLE=*self.zgridstyle, $
                   ZMARGIN=*self.zmargin, $
                   ZMINOR=*self.zminor, $
                   ZRANGE=*self.zrange, $
                   ZSTYLE=*self.zstyle, $
                   ZTHICK=*self.zthick, $
                   ZTICK_GET=*self.ztick_get, $
                   ZTICKFORMAT=*self.ztickformat, $
                   ZTICKINTERVAL=*self.ztickinterval, $
                   ZTICKLAYOUT=*self.zticklayout, $
                   ZTICKLEN=*self.zticklen, $
                   ZTICKNAME=*self.ztickname, $
                   ZTICKS=*self.zticks, $
                   ZTICKUNITS=*self.ztickunits, $
                   ZTICKV=*self.ztickv, $
                   ZTITLE=*self.ztitle, $
                   ZVALUE=*self.zvalue
                   
        2: cgAxis, xloc, yloc, $
                   SAVE=*self.save, $
                   XAXIS=*self.xaxis, $
                   XLOG=*self.xlog, $
                   YAXIS=*self.yaxis, $
                   YLOG=*self.ylog, $
                   ZAXIS=*self.zaxis, $
                   ZLOG=*self.zlog, $
                   
                   ;cgGraphicsKeywords
                   CHARSIZE=*self.charsize, $
                   CHARTHICK=*self.charthick, $
                   COLOR=*self.color, $
                   DATA=*self.data, $
                   DEVICE=*self.device, $
                   NORMAL=*self.normal, $
                   FONT=*self.font, $
                   NODATA=*self.nodata, $
                   NOERASE=*self.noerase, $
                   SUBTITLE=*self.subtitle, $
                   T3D=*self.t3d, $
                   TICKLEN=*self.ticklen, $
                   TITLE=*self.title, $
                   XCHARSIZE=*self.xcharsize, $
                   XGRIDSTYLE=*self.xgridstyle, $
                   XMARGIN=*self.xmargin, $
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
                   YMARGIN=*self.ymargin, $
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
                   YTITLE=*self.ytitle, $
                   ZCHARSIZE=*self.zcharsize, $
                   ZGRIDSTYLE=*self.zgridstyle, $
                   ZMARGIN=*self.zmargin, $
                   ZMINOR=*self.zminor, $
                   ZRANGE=*self.zrange, $
                   ZSTYLE=*self.zstyle, $
                   ZTHICK=*self.zthick, $
                   ZTICK_GET=*self.ztick_get, $
                   ZTICKFORMAT=*self.ztickformat, $
                   ZTICKINTERVAL=*self.ztickinterval, $
                   ZTICKLAYOUT=*self.zticklayout, $
                   ZTICKLEN=*self.zticklen, $
                   ZTICKNAME=*self.ztickname, $
                   ZTICKS=*self.zticks, $
                   ZTICKUNITS=*self.ztickunits, $
                   ZTICKV=*self.ztickv, $
                   ZTITLE=*self.ztitle, $
                   ZVALUE=*self.zvalue
                   
        3: cgAxis, xloc, yloc, zloc, $
                   SAVE=*self.save, $
                   XAXIS=*self.xaxis, $
                   XLOG=*self.xlog, $
                   YAXIS=*self.yaxis, $
                   YLOG=*self.ylog, $
                   ZAXIS=*self.zaxis, $
                   ZLOG=*self.zlog, $
                   
                   ;cgGraphicsKeywords
                   CHARSIZE=*self.charsize, $
                   CHARTHICK=*self.charthick, $
                   COLOR=*self.color, $
                   DATA=*self.data, $
                   DEVICE=*self.device, $
                   NORMAL=*self.normal, $
                   FONT=*self.font, $
                   NODATA=*self.nodata, $
                   NOERASE=*self.noerase, $
                   SUBTITLE=*self.subtitle, $
                   T3D=*self.t3d, $
                   TICKLEN=*self.ticklen, $
                   TITLE=*self.title, $
                   XCHARSIZE=*self.xcharsize, $
                   XGRIDSTYLE=*self.xgridstyle, $
                   XMARGIN=*self.xmargin, $
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
                   YMARGIN=*self.ymargin, $
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
                   YTITLE=*self.ytitle, $
                   ZCHARSIZE=*self.zcharsize, $
                   ZGRIDSTYLE=*self.zgridstyle, $
                   ZMARGIN=*self.zmargin, $
                   ZMINOR=*self.zminor, $
                   ZRANGE=*self.zrange, $
                   ZSTYLE=*self.zstyle, $
                   ZTHICK=*self.zthick, $
                   ZTICK_GET=*self.ztick_get, $
                   ZTICKFORMAT=*self.ztickformat, $
                   ZTICKINTERVAL=*self.ztickinterval, $
                   ZTICKLAYOUT=*self.zticklayout, $
                   ZTICKLEN=*self.zticklen, $
                   ZTICKNAME=*self.ztickname, $
                   ZTICKS=*self.zticks, $
                   ZTICKUNITS=*self.ztickunits, $
                   ZTICKV=*self.ztickv, $
                   ZTITLE=*self.ztitle, $
                   ZVALUE=*self.zvalue
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
;          Any keywords appropriate for the AXIS command or weAxis.
;-
PRO weAxis::GetProperty, $
XLOC=xloc, $
YLOC=yloc, $
ZLOC=zloc, $
CHARSIZE=charsize, $
LABEL=label, $
LOCATION=location, $
OFFSET=offset, $
SAVE=save, $
TARGET=target, $
TITLE=title, $
XAXIS=xaxis, $
XLOG=xlog, $
XMARGIN=xmargin, $
YAXIS=yaxis, $
YLOG=ylog, $
YMARGIN=ymargin, $
ZAXIS=zaxis, $
ZLOG=zlog, $
ZMARGIN=zmargin, $
_REF_EXTRA=extra
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Object Properties
    IF Arg_Present(charsize) GT 0 then charsize = *self.charsize
    IF Arg_Present(label)    NE 0 THEN label = self.label
    IF Arg_Present(location) NE 0 THEN location = self.location
    IF Arg_Present(offset)   NE 0 THEN offset = self.offset
    IF Arg_Present(xloc)     NE 0 AND N_Elements(*self.xloc)  NE 0 THEN xloc = *self.xloc
    IF Arg_Present(yloc)     NE 0 AND N_Elements(*self.yloc)  NE 0 THEN yloc = *self.yloc
    IF Arg_Present(zloc)     NE 0 AND N_Elements(*self.zloc)  NE 0 THEN zloc = *self.zloc
    IF Arg_Present(save)     NE 0 AND N_Elements(*self.save)  NE 0 THEN save = *self.save
    IF Arg_Present(xaxis)    NE 0 AND N_Elements(*self.xaxis) NE 0 THEN xloc = *self.xaxis
    IF Arg_Present(xlog)     NE 0 AND N_Elements(*self.xlog)  NE 0 THEN xlog = *self.xlog
    IF Arg_Present(xmargin)  GT 0 AND N_Elements(*self.xmargin) GT 0 THEN xmargin = *self.xmargin
    IF Arg_Present(yaxis)    NE 0 AND N_Elements(*self.yaxis) NE 0 THEN yaxis = *self.yaxis
    IF Arg_Present(ylog)     NE 0 AND N_Elements(*self.ylog)  NE 0 THEN ylog = *self.ylog
    IF Arg_Present(ymargin)  GT 0 AND N_Elements(*self.ymargin) GT 0 THEN ymargin = *self.ymargin
    IF Arg_Present(zaxis)    NE 0 AND N_Elements(*self.zaxis) NE 0 THEN zaxis = *self.zaxis
    IF Arg_Present(zlog)     NE 0 AND N_Elements(*self.zlog)  NE 0 THEN zlog = *self.zlog
    IF Arg_Present(zmargin)  GT 0 AND N_Elements(*self.zmargin) GT 0 THEN zmargin = *self.zmargin

    IF Arg_Present(target) GT 0 THEN IF Obj_Valid(self.target) GT 0 $
        THEN target = self.target $
        ELSE target = Obj_New()
        
    ;cgGraphicsKeywords
    IF N_Elements(EXTRA) NE 0 THEN BEGIN
        self -> MrGraphicsKeywords::GetProperty, _EXTRA=extra
        self -> MrGrAtom::GetProperty, _EXTRA=extra
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
;          Any keywords appropriate for the AXIS command or weAxis.
;-
PRO weAxis::SetProperty, $
XLOC=xloc, $
YLOC=yloc, $
ZLOC=zloc, $
CHARSIZE=charsize, $
DRAW=draw, $
LABEL=label, $
LOCATION = location, $
OFFSET = offset, $
SAVE=save, $
TARGET=target, $
TITLE=title, $
XAXIS=xaxis, $
XLOG=xlog, $
XMARGIN=xmargin, $
YAXIS=yaxis, $
YLOG=ylog, $
YMARGIN=ymargin, $
ZAXIS=zaxis, $
ZLOG=zlog, $
ZMARGIN=zmargin, $
_REF_EXTRA=extra
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Object Properties
    IF N_Elements(charsize) GT 0 THEN *self.charsize = charsize
    IF N_Elements(label)    GT 0 THEN  self.label    = label
    IF N_Elements(offset)   GT 0 THEN  self.offset   = offset
    IF N_Elements(xloc)     GT 0 THEN *self.xloc     = xloc
    IF N_Elements(yloc)     GT 0 THEN *self.yloc     = yloc
    IF N_Elements(zloc)     GT 0 THEN *self.zloc     = zloc
    IF N_Elements(save)     GT 0 THEN *self.save     = save
    IF N_Elements(xlog)     GT 0 THEN *self.xlog     = xlog
    IF N_Elements(ylog)     GT 0 THEN *self.ylog     = ylog
    IF N_Elements(zlog)     GT 0 THEN *self.zlog     = zlog
    IF N_Elements(xmargin)  GT 0 THEN *self.xmargin  = xmargin
    IF N_Elements(ymargin)  GT 0 THEN *self.ymargin  = ymargin
    IF N_Elements(zmargin)  GT 0 THEN *self.zmargin  = zmargin
    
    ;If a TARGET was given, make sure LOCATION is defined. Default to 'BOTTOM'
    IF N_Elements(target) GT 0 THEN IF Obj_Valid(target) THEN BEGIN
        self.target = target
        IF N_Elements(location) EQ 0 AND self.location EQ '' THEN self.location = 'BOTTOM'
    ENDIF ELSE BEGIN
        self.target = Obj_New()
        self.location = ''
    ENDELSE
    
    ;Check the axis location. Indicate normal coordinates. Default is Data.
    IF N_Elements(location) NE 0 THEN BEGIN
        axloc = StrUpCase(location)
        
        ;Set to Normal Coordinates. cgAxis assumes data.
        IF axloc EQ '' THEN BEGIN
            *self.data = 1
            *self.device = 0
            *self.normal = 0
        ENDIF ELSE BEGIN
            *self.data = 0
            *self.device = 0
            *self.normal = 1
        ENDELSE
        
        ;Undefine [XY]AXIS so they can be defined properly next.
        IF location NE '' THEN BEGIN
            void = temporary(xaxis)
            void = temporary(yaxis)
            void = temporary(zaxis)
        ENDIF

        ;Set the proper axis
        CASE axloc OF
            'RIGHT':  yaxis = 1
            'LEFT':   yaxis = 1
            'TOP':    xaxis = 1
            'BOTTOM': xaxis = 1
            ELSE: ;Do nothing.... Message, 'LOCATION "' + location + '" invalid.', /INFORMATIONAL
        ENDCASE
        
        self.location = axloc
    ENDIF
        
    ;Make sure the axes do not interfere with one another
    IF N_Elements(xaxis) NE 0 THEN BEGIN
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
    
    ;Make sure the axes do not interfere with one another
    IF N_Elements(yaxis) NE 0 THEN BEGIN
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
    
    ;Make sure the axes do not interfere with one another
    IF N_Elements(zaxis) NE 0 THEN BEGIN
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
    
    ;Set the axis title
    IF N_Elements(title) GT 0 THEN BEGIN
        CASE 1 OF
            self.xaxis: *self.xtitle = title
            self.yaxis: *self.ytitle = title
            self.zaxis: *self.ztitle = title
            Else: Message, '[XYZ]Axis not set. Use [XYZ]Title instead of TITLE.', /INFORMATIONAL
        ENDCASE
    ENDIF

    ;Superclass properties
    IF N_Elements(extra) GT 0 THEN BEGIN
        ;MrGrAtom -- Pick out the keywords here to use _STRICT_EXTRA instead of _EXTRA
        atom_kwds = ['HIDE', 'NAME']
        void = IsMember(atom_kwds, extra, iAtom, N_MATCHES=nAtom, NONMEMBER_INDS=IExtra, N_NONMEMBER=nExtra)
        IF nAtom GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra[iAtom]
    
        ;MrGraphicsKeywords Properties
        IF nExtra GT 0 THEN self -> MrGraphicsKeywords::SetProperty, _STRICT_EXTRA=extra[iExtra]
    ENDIF

    ;Draw?
    self.window -> Draw
END


;+
; The clean-up routine for the object. Destroy pointers, etc.
;-
PRO weAxis::cleanup
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Free Pointers
    Ptr_Free, self.xloc
    Ptr_Free, self.yloc
    Ptr_Free, self.zloc
    Ptr_Free, self.charsize
    Ptr_Free, self.save
    Ptr_Free, self.xaxis
    Ptr_Free, self.xlog
    Ptr_Free, self.xmargin
    Ptr_Free, self.yaxis
    Ptr_Free, self.ylog
    Ptr_Free, self.ymargin
    Ptr_Free, self.zaxis
    Ptr_Free, self.zlog
    Ptr_Free, self.zmargin
    
    ;Cleanup the superclasses
    self -> MrGraphicsKeywords::CLEANUP
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
;          Any keywords appropriate for the AXIS command or weAxis.
;-
FUNCTION weAxis::init, xloc, yloc, zloc, $
CHARSIZE=charsize, $
CURRENT=current, $
DATA=data, $
DEVICE=device, $
LOCATION=location, $
NORMAL=normal, $
OFFSET=axoffset, $
SAVE=save, $
TARGET=target, $
TITLE=title, $
XAXIS=xaxis, $
XLOG=xlog, $
XMARGIN=xmargin, $
YAXIS=yaxis, $
YLOG=ylog, $
YMARGIN=ymargin, $
ZAXIS=zaxis, $
ZLOG=zlog, $
ZMARGIN=zmargin, $
_REF_EXTRA=extra
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN, 0
    ENDIF

    ;cgGraphicsKeywords
    IF self -> MrGraphicsKeywords::init(_EXTRA=extra) EQ 0 THEN $
        Message, 'Unable to initialize cgGraphicsKeywords.'

;---------------------------------------------------------------------
;Defaults and Allocate Heap to Pointers //////////////////////////////
;---------------------------------------------------------------------
    
    ;Set Defaults
    current = keyword_set(current)
    setDefaultValue, charsize, 1.5
    setDefaultValue, location, ''
    setDefaultValue, offset, 0

    ;X-axis is the default    
    if n_elements(xaxis) + n_elements(yaxis) + n_elements(zaxis) eq 0 then xaxis = 0
    
    ;Make pointers valid
    self.xloc     = Ptr_New(/ALLOCATE_HEAP)
    self.yloc     = Ptr_New(/ALLOCATE_HEAP)
    self.zloc     = Ptr_New(/ALLOCATE_HEAP)
    self.charsize = Ptr_New(/ALLOCATE_HEAP)
    self.save     = Ptr_New(/ALLOCATE_HEAP)
    self.xaxis    = Ptr_New(/ALLOCATE_HEAP)
    self.xlog     = Ptr_New(/ALLOCATE_HEAP)
    self.xmargin  = Ptr_New(/ALLOCATE_HEAP)
    self.yaxis    = Ptr_New(/ALLOCATE_HEAP)
    self.ylog     = Ptr_New(/ALLOCATE_HEAP)
    self.ymargin  = Ptr_New(/ALLOCATE_HEAP)
    self.zaxis    = Ptr_New(/ALLOCATE_HEAP)
    self.zlog     = Ptr_New(/ALLOCATE_HEAP)
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
                         DATA=data, $
                         DEVICE=device, $
                         LOCATION=location, $
                         NORMAL=normal, $
                         OFFSET=offset, $
                         XLOC=xloc, $
                         YLOC=yloc, $
                         ZLOC=zloc, $
                         SAVE=save, $
                         TARGET=target, $
                         TITLE=title, $
                         XAXIS=xaxis, $
                         XLOG=xlog, $
                         XMARGIN=xmargin, $
                         YAXIS=yaxis, $
                         YLOG=ylog, $
                         YMARGIN=ymargin, $
                         ZAXIS=zaxis, $
                         ZLOG=zlog, $
                         ZMARGIN=zmargin
    
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
PRO weAxis__define, class
    
    class = { weAxis, $
              inherits MrGrAtom, $
              inherits MrGraphicsKeywords, $
            
              xloc: Ptr_New(), $
              yloc: Ptr_New(), $
              zloc: Ptr_New(), $
              charsize: Ptr_New(), $
              location: '', $
              offset: 0, $
              save: Ptr_New(), $
              target: Obj_New(), $
              xaxis: Ptr_New(), $
              xlog: Ptr_New(), $
              xmargin: Ptr_New(), $
              yaxis: Ptr_New(), $
              ylog: Ptr_New(), $
              ymargin: Ptr_New(), $
              zaxis: Ptr_New(), $
              zlog: Ptr_New() $
            }
END