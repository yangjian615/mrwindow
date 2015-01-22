; docformat = 'rst'
;
; NAME:
;   weColorBar__Define
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
;   The purpose of this program is to create a color bar object that can drawn on a
;   data plot.
;
; :Categories:
;    Graphics
;    
; :Examples:
;   Create a simple image and a poorly located color bar::
;       image = dist(256)
;       cgImage, image, /Axes
;       cb = obj_new('weColorBar', /VERTICAL, /DRAW)
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
;       05/09/2013  -   Written by Matthew Argall
;       05/09/2013  -   Added the ::calcColorBarPosition method. - MRA
;       05/10/2013  -   Forgot to pass all of the inherited keywords to cgColorBar. Fixed. - MRA.
;       05/11/2013  -   Forgot to clean up the cgGraphicsKeywords. Fixed. - MRA.
;       06/17/2013  -   Removed CBLOCATION, CBOFFSET, and CBWIDTH properties as well as
;                           the calcColorbarPosition method. - MRA
;       08/01/2013  -   Added the CalcColorbarPosition method and the GRAPHIC, OFFSET,
;                       WIDTH, and CBLOCATION properties. - MRA
;       08/23/2013  -   Added NOERASE keyword to the Draw method. - MRA
;       09/20/2013  -   If GRAPHIC is provided then assume the user wants to put the
;                           colorbar next to it. Default CBLOCATION='RIGHT'. For vertical
;                           colorbars, default to putting ticklables and titles on the
;                           right-side unless CBLOCATION='LEFT'. For horizontal colorbars,
;                           default to putting tick-labels and titles on the top unless
;                           CBLOCATION='BOTTOM'. Also assume that if GRAPHIC is given,
;                           CTINDEX, RANGE, and PALETTE should be synchronized. - MRA
;       09/21/2013  -   MINRANGE and MAXRANGE are no longer object properties, but are
;                           still keyword inputs. RANGE takes precedence over both. - MRA
;       09/23/2013  -   Added the [PXY]_SYSVAR object properties. Added the [XY]RANGE
;                           keywords to the Set/GetProperties methods. - MRA
;       09/25/2013  -   Turned CalcColorbarPosition into a function so that if an error
;                           occurs, the position property will not be altered. - MRA
;       09/27/2013  -   POSITION now handled by MrGraphicAtom__Define. Width and Offset
;                           are now in normalized coordinates, not character units. - MRA
;       09/29/2013  -   WIDTH and OFFSET are back to being defined in character units.
;                           Thusly, the width of the colorbar does not depend on the
;                           width of the window it is displayed in. Furthermore, they
;                           remain undefined until a graphic is provided. If a graphic
;                           if not provided, but CBLocation is given, then the colorbar
;                           will try to fit itself to the last item plotted. - MRA
;       10/02/2013  -   PALETTE, RANGE, and CTINDEX are now synced with GRAPHIC in the
;                           Draw method, just in case the graphic's properties changed. - MRA
;       2013/10/28  -   CTINDEX takes precedence over PALETTE. Needed to undefine CTINDEX
;                           if PALETTE was set. - MRA
;       2013/11/09  -   Renamed GRAPHIC to TARGET to be consistent with IDL 8.0+.
;       2013/11/20  -   Inherit MrGrAtom instead of MrGraphicAtom. Default character size
;                           is 1.5. - MRA
;       2014/01/10  -   CBLOCATION was having trouble with case of letters. Fixed. - MRA
;-
;*****************************************************************************************
;+
;   Calculate the position of the colorbar relative to the graphic by which it is placed.
;
; :Private:
;-
FUNCTION weColorBar::CalcColorbarPosition
    Compile_Opt StrictArr

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN, FltArr(4)
    ENDIF

    ;Is a graphics object present?
    IF Obj_Valid(self.target) THEN BEGIN
        ;Convert from the graphic's coordinate system to normal coordinates.
        self.target -> GetProperty, POSITION=position, DEVICE=gDevice, NORMAL=gNorm
    
        ;The default coordinate system is "normal".
        gNorm = keyword_set(gNorm)
        gDevice = keyword_set(gDevice)
        if gNorm + gDevice eq 0 then gNorm = 1
    
        ;Convert Coordinates
        IF gNorm EQ 0 THEN BEGIN
            position = self.target -> ConvertCoord(position, $
                                                   DEVICE=gDev, $
                                                   NORMAL=gNorm, $
                                                   DATA=gData, $
                                                   /TO_NORMAL)
        ENDIF
    
    ;If not, fit the colorbar to the last thing plotted.
    ENDIF ELSE position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
    
    cbPosition = FltArr(4)

    ;Covnert from character units to normal coordinates.
    xcharsize = Double(!d.x_ch_size) / Double(!d.x_size) * self.charsize
    ycharsize = Double(!d.y_ch_size) / Double(!d.y_size) * self.charsize
    xoffset = xcharsize * *self.offset
    yoffset = ycharsize * *self.offset
    hwidth  = xcharsize * *self.width
    vwidth  = ycharsize * *self.width

    ;Calclulate the position
    CASE self.cbLocation OF
        'RIGHT': BEGIN
            cbPosition[0] = position[2] + xoffset
            cbPosition[1] = position[1]
            cbPosition[2] = position[2] + xoffset + hwidth
            cbPosition[3] = position[3]
        ENDCASE
        
        'LEFT': BEGIN
            cbPosition[0] = position[0] - xoffset - hwidth
            cbPosition[1] = position[1]
            cbPosition[2] = position[0] - xoffset
            cbPosition[3] = position[3]
        ENDCASE
        
        'TOP': BEGIN
            cbPosition[0] = position[0]
            cbPosition[1] = position[3] + yoffset
            cbPosition[2] = position[2]
            cbPosition[3] = position[3] + yoffset + vwidth
        ENDCASE
        
        'BOTTOM': BEGIN
            cbPosition[0] = position[0]
            cbPosition[1] = position[1] - yoffset - vwidth
            cbPosition[2] = position[2]
            cbPosition[3] = position[1] - yoffset
        ENDCASE
    ENDCASE

    ;Make sure the position fits within the window
    IF cbPosition[0] LT 0 OR cbPosition[2] GT 1 THEN Message, 'The colorbar does not fit within the window.', /INFORMATIONAL
    IF cbPosition[1] LT 0 OR cbPosition[3] GT 1 THEN Message, 'The colorbar does not fit within the window.', /INFORMATIONAL
    
    ;Return the position
    RETURN, cbPosition
END


;+
; This method draws the color bar object. 
;-
PRO weColorBar::Draw, $
NOERASE=noerase
    Compile_Opt StrictArr

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    IF N_Elements(noerase) EQ 0 THEN noerase = *self.noerase

    ;If a graphic or a cbLocation was given...
    IF Obj_Valid(self.target) || (self.cblocation NE '') THEN BEGIN
        
        ;Calculate a the colorbar position
        position = self -> CalcColorbarPosition()
        
        ;If an error occured, then return
        IF Array_Equal(position, [0.0, 0.0, 0.0, 0.0]) $
            THEN RETURN $
            ELSE self.position = position
        
        ;The position is caluclated in normal coordinates.    
        self.normal = 1
        self.data   = 0
        self.device = 0
        
        ;Make sure RANGE, CTINDEX, PALETTE are the same
        IF MrIsA(self.target, 'MrImage') $
            THEN self.target -> GetProperty, RANGE=gRange, PALETTE=gPalette, LOG=log, CTINDEX=gCTIndex $
            ELSE self.target -> GetProperty, RANGE=gRange, PALETTE=gPalette

        ;Calling the SetProperty method causes an infinite loop.
        ;Set the properties directly.
        if self.vertical $
            then self.ylog = Keyword_Set(log) $
            else self.xlog = Keyword_Set(log)
        if n_elements(range)    gt 0 then self.range    = gRange
        if n_elements(gCTIndex) gt 0 then *self.ctindex = gCTIndex
        if n_elements(gPalette) gt 0 then *self.palette = gPalette
    ;Otherwise, use the given position
    ENDIF
    
    ;Adjust postscript output.
    if !d.name eq 'PS' then begin
        tcharsize = MrPS_Rescale(*self.tcharsize, /CHARSIZE)
        textthick = MrPS_Rescale(self.textthick,  /THICK)
        charsize  = MrPS_Rescale(self.charsize,   /CHARSIZE)
        charthick = MrPS_Rescale(*self.charthick, /CHARTHICK)
        thick     = MrPS_Rescale(*self.thick,     /THICK)
    endif else begin
        tcharsize = *self.tcharsize
        textthick =  self.textthick
        charsize  =  self.charsize
        charthick = *self.charthick
        thick     = *self.thick
    endelse

    ;Draw the Color Bar
    cgColorBar, ANNOTATECOLOR =  self.annotatecolor, $
                BOTTOM        =  self.bottom, $
                BREWER        =  self.brewer, $
                CHARPERCENT   =  self.charpercent, $
                CLAMP         = *self.clamp, $
                CTINDEX       = *self.ctindex, $
                DISCRETE      =  self.discrete, $
                DIVISIONS     =  self.divisions, $
                FIT           =  self.fit, $
                FORMAT        =  self.format, $
                INVERTCOLORS  =  self.invertcolors, $
                MINOR         =  self.minor, $
                NCOLORS       =  self.ncolors, $
                NEUTRALINDEX  = *self.neutralIndex, $
                OOB_FACTOR    =  self.oob_factor, $
                OOB_HIGH      = *self.oob_high, $
                OOB_LOW       = *self.oob_low, $
                PALETTE       = *self.palette, $
                RANGE         =  self.range, $
                REVERSE       =  self.reverse, $
                RIGHT         =  self.right, $
                TCHARSIZE     =       tcharsize, $
                TEXTTHICK     =       textthick, $
                TLOCATION     = *self.tlocation, $
                TICKINTERVAL  = *self.tickinterval, $
                TICKNAMES     = *self.ticknames, $
                TOP           =  self.top, $
                VERTICAL      =  self.vertical, $
                XLOG          =  self.xlog, $
                YLOG          =  self.ylog, $
               
                ;MrGraphicAtom Keywords
                POSITION      =  self.position, $
               
               
                ;MrGraphicsKeywords
;                AXISCOLOR    = *self.axiscolor, $
;                BACKGROUND   = cgColor(*self.background), $
                CHARSIZE      =       charsize, $
                CHARTHICK     =       charthick, $
;                CLIP         = *self.clip, $
                COLOR         = *self.color, $
                DATA          =  self.data, $
                DEVICE        =  self.device, $
                NORMAL        =  self.normal, $
                FONT          = *self.font, $
;                NOCLIP       = *self.noclip, $
                NODATA        = *self.nodata, $
                NOERASE       =       noerase, $
;                PSYM         = *self.psym, $
                SUBTITLE      = *self.subtitle, $
;                SYMSIZE      = *self.symsize, $
                T3D           = *self.t3d, $
;                THICK        = *self.thick, $
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
;                ZTICK_GET    = *self.ztick_get, $
;                ZTICKFORMAT  = *self.ztickformat, $
;                ZTICKINTERVAL= *self.ztickinterval, $
;                ZTICKLAYOUT  = *self.zticklayout, $
;                ZTICKLEN     = *self.zticklen, $
;                ZTICKNAME    = *self.ztickname, $
;                ZTICKS       = *self.zticks, $
;                ZTICKUNITS   = *self.ztickunits, $
;                ZTICKV       = *self.ztickv, $
;                ZTITLE       = *self.ztitle, $
                ZVALUE        = *self.zvalue

    self -> SaveCoords
END


;+
; The clean-up routine for the object. Destroy pointers, etc.
;-
PRO weColorBar::cleanup
    Compile_Opt StrictArr

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Free Pointers
    Ptr_Free, self.clamp
    Ptr_Free, self.ctIndex
    Ptr_Free, self.neutralIndex
    Ptr_Free, self.offset
    Ptr_Free, self.oob_high
    Ptr_Free, self.oob_low
    Ptr_Free, self.palette
    Ptr_Free, self.tcharsize
    Ptr_Free, self.tickinterval
    Ptr_Free, self.ticknames
    Ptr_Free, self.tlocation
    Ptr_Free, self.width
    
    ;Cleanup the superclasses
    self -> MrGraphicsKeywords::CLEANUP
    self -> MrGrAtom::Cleanup
END


;+
; This method obtains the current properties of the object. 
; 
; :Keywords:
;-
PRO weColorBar::GetProperty, $
ANNOTATECOLOR=annotatecolor, $
BOTTOM=bottom, $
BREWER=brewer, $
CBLOCATION=cblocation, $
CHARPERCENT=charpercent, $
CHARSIZE=charsize, $
CLAMP=clamp, $
CTINDEX=ctindex, $
DISCRETE=discrete, $
DIVISIONS=divisions, $
FIT=fit, $
FORMAT=format, $
INVERTCOLORS=invertcolors, $
MAXRANGE=maxrange, $
MINOR=minor, $
MINRANGE=minrange, $
NAME=name, $
NCOLORS=ncolors, $
NEUTRALINDEX=neutralIndex, $
OFFSET=offset, $
OOB_FACTOR=oob_factor, $
OOB_HIGH=oob_high, $
OOB_LOW=oob_low, $
PALETTE=palette, $
POSITION=position, $
RANGE=range, $
REVERSE=reverse, $
RIGHT=right, $
TARGET=target, $
TCHARSIZE=tcharsize, $
TEXTTHICK=textthick, $
TLOCATION=tlocation, $
TICKINTERVAL=tickinterval, $
TICKLEN=ticklen, $
TICKNAMES=ticknames, $
TOP=top, $
VERTICAL=vertical, $
WIDTH=width, $
XLOG=xlog, $
XRANGE=xrange, $
YLOG=ylog, $
YRANGE=yrange, $
_REF_EXTRA=extra
    Compile_Opt StrictArr

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;weGraphics Keywords
    IF N_Elements(extra) GT 0 THEN BEGIN
        self -> MrGraphicsKeywords::GetProperty, _EXTRA=extra
        self -> MrGrAtom::GetProperty, _EXTRA=extra
    ENDIF
    
    ;Keywords
    IF Arg_Present(annotatecolor) THEN annotatecolor = self.annotatecolor
    IF Arg_Present(bottom)        THEN bottom        = self.bottom
    IF Arg_Present(brewer)        THEN brewer        = self.brewer
    IF Arg_Present(cblocation)    THEN cblocation    = self.cblocation
    IF Arg_Present(charpercent)   THEN charpercent   = self.charpercent
    IF Arg_Present(charsize)      THEN charsize      = self.charsize
    IF Arg_Present(discrete)      THEN discrete      = self.discrete
    IF Arg_Present(divisions)     THEN divisions     = self.divisions
    IF Arg_Present(fit)           THEN fit           = self.fit
    IF Arg_Present(format)        THEN format        = self.format
    IF Arg_Present(invertcolors)  THEN invertcolors  = self.invertcolors
    IF Arg_Present(minor)         THEN minor         = self.minor
    IF Arg_Present(maxrange)      THEN maxrange      = self.range[1]
    IF Arg_Present(minrange)      THEN minrange      = self.range[0]
    IF Arg_Present(name)          THEN name          = self.name
    IF Arg_Present(ncolors)       THEN ncolors       = self.ncolors
    IF Arg_Present(neutralIndex)  THEN neutralIndex  = self.neutralIndex
    IF Arg_Present(oob_factor)    THEN oob_factor    = self.oob_factor
    IF Arg_Present(position)      THEN position      = self.position
    IF Arg_Present(range)         THEN range         = self.range
    IF Arg_Present(reverse)       THEN reverse       = self.reverse
    IF Arg_Present(right)         THEN right         = self.right
    IF Arg_Present(textthick)     THEN texttick      = self.texthick
    IF Arg_Present(ticklen)       THEN ticklen       = self.ticklen
    IF Arg_Present(top)           THEN top           = self.top
    IF Arg_Present(vertical)      THEN vertical      = self.vertical
    IF Arg_Present(xlog)          THEN xlog          = self.xlog
    IF Arg_Present(ylog)          THEN ylog          = self.ylog
    
    ;Pointers
    IF Arg_Present(ctindex)      AND N_Elements(*self.ctindex)      NE 0 THEN ctindex      = *self.ctindex
    IF Arg_Present(neutralIndex) AND N_Elements(*self.neutralIndex) NE 0 THEN neutralIndex = *self.neutralIndex
    IF Arg_Present(offset)       AND N_Elements(*self.offset)       NE 0 THEN offset       = *self.offset
    IF Arg_Present(oob_high)     AND N_Elements(*self.oob_high)     NE 0 THEN oob_high     = *self.oob_high
    IF Arg_Present(oob_low)      AND N_Elements(*self.oob_low)      NE 0 THEN oob_low      = *self.oob_low
    IF Arg_Present(palette)      AND N_Elements(*self.palette)      NE 0 THEN palette      = *self.palette
    IF Arg_Present(tcharsize)    AND N_Elements(*self.tcharsize)    NE 0 THEN tcharsize    = *self.tcharsize
    IF Arg_Present(tlocation)    AND N_Elements(*self.tlocation)    NE 0 THEN tlocation    = *self.tlocation
    IF Arg_Present(tickinterval) AND N_Elements(*self.tickinterval) NE 0 THEN tickinterval = *self.tickinterval
    IF Arg_Present(ticknames)    AND N_Elements(*self.ticknames)    NE 0 THEN ticknames    = *self.ticknames
    IF Arg_Present(width)        AND N_Elements(*self.width)        NE 0 THEN width        = *self.width
    
    ;[XY]Range -- If the colorbar is vertical (horizontal) set X(Y)Range to
    ;             [0.0, 0.0] to indicate that the range is invalid. This also 
    ;             would indicate that the range is determined automatically by IDL.
    IF Arg_Present(xrange) and self.vertical eq 0 then xrange = self.range else xrange = fltarr(2)
    IF Arg_Present(yrange) and self.vertical eq 1 then yrange = self.range else yrange = fltarr(2)
    
    ;Objects
    IF Arg_Present(target) THEN BEGIN
        IF Obj_Valid(self.target) $
            THEN target = self.target $
            ELSE target = Obj_New()
    ENDIF
END


;+
; This method sets the properties of the object.
;
; :Keywords:
;-
pro weColorBar::SetProperty, $
ANNOTATECOLOR=annotatecolor, $
BOTTOM=bottom, $
BREWER=brewer, $
CBLOCATION=cblocation, $
CHARSIZE=charsize, $
CHARPERCENT=charpercent, $
CLAMP=clamp, $
CTINDEX=ctindex, $
DISCRETE=discrete, $
DIVISIONS=divisions, $
DRAW = draw, $
FIT=fit, $
FORMAT=format, $
INVERTCOLORS=invertcolors, $
MAXRANGE=maxrange, $
MINOR=minor, $
MINRANGE=minrange, $
NCOLORS=ncolors, $
NEUTRALINDEX=neutralIndex, $
OFFSET=offset, $
OOB_FACTOR=oob_factor, $
OOB_HIGH=oob_high, $
OOB_LOW=oob_low, $
PALETTE=palette, $
POSITION=position, $
RANGE=range, $
REVERSE=reverse, $
RIGHT=right, $
TARGET=target, $
TCHARSIZE=tcharsize, $
TEXTTHICK=textthick, $
TLOCATION=tlocation, $
TICKINTERVAL=tickinterval, $
TICKLEN=ticklen, $
TICKNAMES=ticknames, $
TOP=top, $
VERTICAL=vertical, $
WIDTH=width, $
XLOG=xlog, $
XRANGE=xrange, $
YLOG=ylog, $
YRANGE=yrange, $
_REF_EXTRA=extra
    Compile_Opt StrictArr

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    
;-----------------------------------------------------
;Sync Colorbar with Graphic \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;If a graphic was provided, it was so that the colorbar could be placed next to it.
    ;Make sure CBLOCATION is not '', default to 'RIGHT'. Also, synchronize CTINDEX,
    ;PALETTE, and RANGE.
    IF N_Elements(target) NE 0 THEN IF Obj_Valid(target) THEN BEGIN
        self.target = target
        IF MrIsA(target, 'MrImage') $
            THEN target -> GetProperty, RANGE=gRange, PALETTE=gPalette, CTINDEX=gCTIndex $
            ELSE target -> GetProperty, RANGE=gRange, PALETTE=gPalette
        
        ;Sync the colorbar properties with those of GRAPHIC. They will be set
        ;as object properties below.
        IF N_Elements(gRange)   NE 0  THEN range = gRange
        IF N_Elements(gCTIndex) NE 0  THEN ctindex = gCTIndex
        IF N_Elements(gPalette) NE 0  THEN palette = gPalette
        
        ;Choose a location for the graphic
        IF N_Elements(cbLocation) EQ 0 AND self.cbLocation EQ '' THEN cbLocation = 'RIGHT'
    ENDIF ELSE BEGIN
        self.target = Obj_New()
        cbLocation = ''
    ENDELSE
    
;-----------------------------------------------------
;Fit Colorbar Nicely \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;If CBLOCATION is not '', then VERTICAL needs to be set properly. Also, put tick-
    ;labels on and titles on the same side of the colorbar.
    IF N_Elements(cbLocation) NE 0 THEN BEGIN
        CASE StrUpCase(cbLocation) OF
            '': ;Do nothing
        
            'RIGHT': BEGIN
                vertical = 1
                IF N_Elements(tLocation) EQ 0 THEN tLocation = 'RIGHT'
                IF N_Elements(right)     EQ 0 THEN right     = 1
            ENDCASE
        
            'LEFT': BEGIN
                vertical = 1
                IF N_Elements(tLocation) EQ 0 THEN tLocation = 'LEFT'
                IF N_Elements(right)     EQ 0 THEN right     = 0
            ENDCASE
        
            'BOTTOM': BEGIN
                vertical = 0
                IF N_Elements(tLocation) EQ 0 THEN tLocation = 'BOTTOM'
                IF N_Elements(top)       EQ 0 THEN top       = 0
            ENDCASE
        
            'TOP': BEGIN
                vertical = 0
                IF N_Elements(tLocation) EQ 0 THEN tLocation = 'TOP'
                IF N_Elements(top)       EQ 0 THEN top       = 1
            ENDCASE
        ENDCASE
        
        ;Set a width and offset for the colorbar
        IF N_Elements(offset) EQ 0 THEN offset = 1
        IF N_Elements(width)  EQ 0 THEN width = 3
    ENDIF
    
;-----------------------------------------------------
;Object Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;[XY]Range. Set the color range, but MINRANGE, MAXRANGE and RANGE take precedence.
    IF N_Elements(xrange) NE 0 AND self.vertical EQ 0 THEN self.range = xrange
    IF N_Elements(yrange) NE 0 AND self.vertical EQ 1 THEN self.range = yrange

    ;Other Keywords
    IF N_Elements(annotatecolor) NE 0 THEN  self.annotatecolor = annotatecolor
    IF N_Elements(bottom)        NE 0 THEN  self.bottom = bottom
    IF N_Elements(brewer)        NE 0 THEN  self.brewer = brewer
    IF N_Elements(cbLocation)    NE 0 THEN  self.cbLocation = StrUpCase(cbLocation)
    IF N_Elements(charpercent)   NE 0 THEN  self.charpercent = charpercent
    IF N_Elements(charsize)      GT 0 THEN  self.charsize = charsize
    IF N_Elements(clamp)         NE 0 THEN *self.clamp = clamp
    IF N_Elements(ctindex)       NE 0 THEN *self.ctindex = ctindex
    IF N_Elements(discrete)      NE 0 THEN  self.discrete = discrete
    IF N_Elements(divisions)     NE 0 THEN  self.divisions = divisions
    IF N_Elements(fit)           NE 0 THEN  self.fit = fit
    IF N_Elements(format)        NE 0 THEN  self.format = format
    IF N_Elements(invertcolors)  NE 0 THEN  self.invertcolors = invertcolors
    IF N_Elements(name)          NE 0 THEN  self.name = name
    IF N_Elements(maxrange)      NE 0 THEN  self.range[1] = maxrange
    IF N_Elements(minor)         NE 0 THEN  self.minor = minor
    IF N_Elements(minrange)      NE 0 THEN  self.range[0] = minrange
    IF N_Elements(ncolors)       NE 0 THEN  self.ncolors = ncolors
    IF N_Elements(neutralIndex)  NE 0 THEN *self.neutralIndex = neutralIndex
    IF N_Elements(offset)        NE 0 THEN *self.offset = offset
    IF N_Elements(oob_factor)    NE 0 THEN  self.oob_factor = oob_factor
    IF N_Elements(oob_high)      NE 0 THEN *self.oob_high = oob_high
    IF N_Elements(oob_low)       NE 0 THEN *self.oob_low = oob_low
    IF N_Elements(position)      GT 0 THEN  self.position = position
    IF N_Elements(range)         NE 0 THEN  self.range = range
    IF N_Elements(reverse)       NE 0 THEN  self.reverse = reverse
    IF N_Elements(right)         NE 0 THEN  self.right = right
    IF N_Elements(tcharsize)     NE 0 THEN *self.tcharsize = tcharsize
    IF N_Elements(textthick)     NE 0 THEN  self.textthick = textthick
    IF N_Elements(tlocation)     NE 0 THEN *self.tlocation = tlocation
    IF N_Elements(tickinterval)  NE 0 THEN *self.tickinterval = tickinterval
    IF N_Elements(ticknames)     NE 0 THEN *self.ticknames = ticknames
    IF N_Elements(top)           NE 0 THEN  self.top = top
    IF N_Elements(vertical)      NE 0 THEN  self.vertical = vertical
    IF N_Elements(width)         NE 0 THEN *self.width = width
    IF N_Elements(xlog)          NE 0 THEN  self.xlog = xlog
    IF N_Elements(ylog)          NE 0 THEN  self.ylog = ylog
    
    IF N_Elements(palette) NE 0 THEN BEGIN
        *self.palette = palette
        Ptr_Free, self.ctindex
        self.ctindex = Ptr_New(/ALLOCATE_HEAP)
    ENDIF
    
    ;Superclass properties
    IF N_Elements(extra) GT 0 THEN BEGIN
        ;MrGrAtom -- Pick out the keywords here to use _STRICT_EXTRA instead of _EXTRA
        atom_kwds = ['HIDE', 'NAME']
        void = MrIsMember(atom_kwds, extra, iAtom, COUNT=nAtom, COMPLEMENT=IExtra, NCOMPLEMENT=nExtra)
        IF nAtom GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra[iAtom]
    
        ;MrGraphicsKeywords Properties
        IF nExtra GT 0 THEN self -> MrGraphicsKeywords::SetProperty, _STRICT_EXTRA=extra[iExtra]
    ENDIF
    
    self.window -> Draw
END


;+
;   The initialization method of the object. Called automatically when the object
;   is created. Note that several [X, Y, Z] Graphics Keywords are ignored in favor
;   of their axis-neutral names (sans [X, Y, Z]). See keyword descriptions below for
;   details.
;    
; :Keywords:
;    annotatecolor: in, optional, type=string, default="opposite"
;       The name of the "annotation color" to use. The names are those for
;       cgCOLOR. If this keyword is used, the annotation color is loaded after
;       the color bar is displayed. This keyword is provided to maintain backward 
;       compatibility, but also to solve the potential problem of an extra line showing up
;       in the color bar when the COLOR keyword is used in indexed color mode. In other words,
;       use ANNOTATECOLOR in place of COLOR for complete color model independent results.
;    bottom: in, optional, type=integer, default=0
;       The lowest color index of the colors to be loaded in the color bar.
;    brewer: in, optional, type=boolean, default=0
;         This keyword is used only if the `CTIndex` keyword is used to select a color table number.
;         Setting this keyword allows Brewer color tables to be used.
;    charpercent: in, optional, type=float, default=0.85                 
;       A value from 0.0 go 1.0 that is multiplied by the CHARSIZE to produce
;       the character size for the color bar. This value is only used if CHARSIZE is 
;       undefined. This keyword is primarily useful for using color bars in resizeable 
;       graphics windows (cgWindow).
;    charsize: in, optional, type=float
;       The character size of the color bar annotations. Default is cgDefCharsize()*charPercent.
;    clamp: in, optional, type=float
;        A two-element array in data units. The color bar is clamped to these
;        two values. This is mostly of interest if you are "window-leveling"
;        an image. The clamp is set to the "window" of the color bar.
;        Normally, when you are doing this, you would like the colors outside
;        the "window" to be set to a neutral color. Use the NEUTRALINDEX keyword
;        to set the netural color index in the color bar. (See the Examples section
;        for more information.)
;    color: in, optional, type=string
;        The name of the color to use for color bar annotations. Ignored unless passed 
;        the name of a cgColor color. The default value is to use the ANNOTATECOLOR.
;    ctindex: in, optional, type=integer
;         The index number of a color table. The `Brewer` and `Reverse` keywords will be checked
;         to see how to load the color table into the `Palette` keyword. This keyword will take
;         precidence over any colors that are loaded with the `Palette` keyword. 
;    discrete: in, optional, type=boolean, default=0
;         Set this keyword to configure certain properties of the color bar to make
;         discrete color blocks for the color bar. This works best if you are using
;         a handful of colors in the color bar (e.g, 8-16).
;    divisions: in, optional, type=integer, default=0
;         The number of divisions to divide the bar into. There will be (divisions + 1) annotations. 
;         When set to 0 (the default), the IDL Plot command detemines the number of divisions used.
;    draw: in, optional, type=boolean, default=0
;         Draw the color bar immediately.
;    fit: in, optional, type=boolean, default=0
;       If this keyword is set, the colorbar "fits" itself to the normalized
;       coordinates of the last graphics command executed. In other words, for
;       a horizontal color bar, postition[[0,2]] = !X.Window, and for a vertical
;       color bar, position[[1,3]] = !Y.Window. Other positions are adjusted
;       to put the colorbar "reasonably" close to the plot. The fit many not always
;       be accurate. If you are fitting to an image, be sure to set the SAVE keyword
;       on cgImage to establish a data coordinate system.
;    format: in, optional, type=string, default=""
;       The format of the color bar annotations. The default is to let the IDL Plot command 
;       determine how the color bar labels are formatted.
;    TARGET:            in, optional, type=object
;                       The graphic that the colorbar describes. If no target is given,
;                           all currently selected targets with a color palette will
;                           be given a colorbar. If no graphics are selected, and neither
;                           `CTINDEX` or `PALETTE` are given, no object will be created.
;                           If either of the color palette keywords are present, the
;                           colorbar will be placed in the current window.
;    invertcolors: in, optional, type=boolean, default=0
;       Setting this keyword inverts the colors in the color bar.
;    maxrange: in, optional
;       The maximum data value for the color bar annotation. Default is NCOLORS.
;    minrange: in, optional, type=float, default=0.0
;       The minimum data value for the bar annotation. 
;    minor: in, optional, type=integer, default=2
;       The number of minor tick divisions. 
;    ncolors: in, optional, type=integer, default=256
;       This is the number of colors in the color bar.
;    neutralindex: in, optional, type=integer   
;       This is the color index to use for color bar values outside the
;       clamping range when clamping the color bar with the CLAMP keyword.
;       If this keyword is absent, the highest color table value is used
;       for low range values and the lowest color table value is used
;       for high range values, in order to provide contrast with the
;       clamped region. (See the Examples section for more information.)
;    oob_factor: in, optional, type=float, default=1.0
;       The default is to make the length of the out-of-bounds triangle the
;       same distance as the height (or width, in the case of a vertical
;       color bar) of the color bar. If you would prefer a shorted triangle length, 
;       set this keyword to a value less than zero (e.g., 0.5). If you prefer a 
;       longer length, set this keyword to a value greater than zero. The "standard"
;       length will be multiplied by this value.
;    oob_high: in, optional, type=string
;       The name of an out-of-bounds high color. This color will be represented
;       by a triangle on the right or top of the color bar. If the color is
;       a string byte value (e.g., "215"), then this color in the current color
;       table is used. The color can also be a three-element color triple 
;       (e.g., [240, 200, 65]). Note, you can CANNOT use a long integer as
;       a color table index number with this keyword. If you want to use a 
;       color table index number, be sure the number is a short integer, byte
;       value, or a string (e.g, OOB_HIGH=200S, OOB_HIGH=200B, or OOB_HIGH='200').
;    oob_low: in, optional, type=string
;       The name of an out-of-bounds low color. This color will be represented
;       by a triangle on the left or bottom of the color bar. If the color is
;       a string byte value (e.g., "215"), then this color in the current color
;       table is used. The color can also be a three-element color triple 
;       (e.g., [240, 200, 65]). Note, you can CANNOT use a long integer as
;       a color table index number with this keyword. If you want to use a 
;       color table index number, be sure the number is a short integer, byte
;       value, or a string (e.g, OOB_HIGH=200S, OOB_HIGH=200B, or OOB_HIGH='200').
;    offset: in, optional, type=int/float, default=0.04
;       The offset, in normalized units, of the colorbar from the `graphic` by which
;       it is placed. Use only with the `graphic`.
;    palette: in, optional, type=byte
;       A color palette containing the RGB color vectors to use for the color
;       bar. The program will sample NCOLORS from the color palette. 
;    range: in, optional, type=float
;       A two-element vector of the form [min, max]. Provides an alternative 
;       and faster way way of setting the MINRANGE and MAXRANGE keywords.
;    reverse: in, optional, type=boolean, default=0
;       An alternative keyword name (one I can actually remember!) for the INVERTCOLORS keyword.
;       It reverses the colors in the color bar.
;    right: in, optional, type=boolean, default=0   
;       This puts the labels on the right-hand side of a vertical color bar. It applies 
;       only to vertical color bars.
;    tickinterval: in, optional, type=float
;       Set this keyword to the interval spacing of major tick marks. Use this keyword in
;       place of XTickInterval or YTickInterval keywords.
;    ticknames: in, optional, type=string                 
;       A string array of names or values for the color bar tick marks. There should be
;       `divisions` + 1 tick names in the array.
;    tcharsize: in, optional, type=float
;       The title size. By default, the same as `Charsize`. Note that this keyword is
;       ignored for vertical color bars unless the title location (`TLocation`) is on
;       the opposite side of the color bar from the color bar labels. This is a consequence
;       of being upable to determine the length of color bar labels programmatically in this
;       orientation.
;    textthick: in, optional, type=float, default=1.0
;        Sets the thickness of the textual annotations on the color bar.
;    tlocation: in, optional, type=string
;       The title location, which allows the user to set the title location independently 
;       of the colorbar labels. May be "TOP" or "BOTTOM" for horizontal color bars, and
;       "LEFT" or "RIGHT" for vertical color bars.
;    top: in, optional, type=boolean, default=0
;       This puts the labels on top of the bar rather than under it. The keyword only 
;       applies if a horizontal color bar is rendered.
;    vertical: in, optional, type=boolean, default=0
;       Setting this keyword give a vertical color bar. The default is a horizontal color bar.
;    width: in, optional, type=int/float, default=0.08
;       Width of the colorbar in normalized units. Used only with the `graphic` keyword.
;    xlog: in, optional, type=boolean, default=0
;       Set this keyword to use logarithmic scaling for the colorbar data range.
;    ylog: in, optional, type=boolean, default=0
;       Set this keyword to use logarithmic scaling for the colorbar data range.
;    _ref_extra: in, optional
;         Any keyword appropriate for the cgColorBar command is also accepted by keyword
;         inheritance.
FUNCTION weColorBar::INIT, $
ANNOTATECOLOR = annotatecolor, $
BOTTOM = bottom, $
BREWER = brewer, $
CBLOCATION = cbLocation, $
CHARPERCENT = charpercent, $
CHARSIZE = charsize, $
CLAMP = clamp, $
COLOR = color, $
CTINDEX = ctindex, $
CURRENT = current, $
DISCRETE = discrete, $
DIVISIONS = divisions, $
DRAW = draw, $
FIT = fit, $
FORMAT = format, $
HIDE = hide, $
INVERTCOLORS = invertcolors, $
NAME = name, $
MAXRANGE = maxrange, $
MINOR = minor, $
MINRANGE = minrange, $
NCOLORS = ncolors, $
NEUTRALINDEX = neutralIndex, $
NODISPLAY = nodisplay, $ ; Obsolete, Ignored.
OFFSET = offset, $
OOB_FACTOR = oob_factor, $
OOB_HIGH = oob_high, $
OOB_LOW = oob_low, $
PALETTE = palette, $
POSITION = position, $
RANGE = range, $
REVERSE = reverse, $
RIGHT = right, $
TARGET = target, $
TCHARSIZE = tcharsize, $
TEXTTHICK = textthick, $
TLOCATION = tlocation, $
TICKINTERVAL = tickinterval, $
TICKNAMES = ticknames, $
TOP = top, $
VERTICAL = vertical, $
WIDTH = width, $
XLOG = xlog, $
YLOG = ylog, $
_REF_EXTRA = extra
    Compile_Opt StrictArr

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN, 0
    ENDIF

;-----------------------------------------------------
;Target \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;If a target was not given, a palette must be given.
    if n_elements(palette) eq 0 && n_elements(ctindex) eq 0 then begin
        if n_elements(target) eq 0 then begin
            target = self -> _GetTarget(/ALL, /ANY, COUNT=nTargets)
            if nTargets eq 0 then message, 'Insert MrColorbar failed. No targets available.'
        endif
    endif

;-----------------------------------------------------
;Superclasses \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;MrGraphicsKeywords
    IF self -> MrGraphicsKeywords::INIT(_STRICT_EXTRA=extra) EQ 0 THEN $
        Message, 'Unable to initialize MrGraphicsKeywords.'

;-----------------------------------------------------
;Validate Pointers \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    self.clamp        = Ptr_New(/ALLOCATE_HEAP)
    self.ctIndex      = Ptr_New(/ALLOCATE_HEAP)
    self.neutralIndex = Ptr_New(/ALLOCATE_HEAP)
    self.offset       = Ptr_New(/ALLOCATE_HEAP)
    self.oob_high     = Ptr_New(/ALLOCATE_HEAP)
    self.oob_low      = Ptr_New(/ALLOCATE_HEAP)
    self.palette      = Ptr_New(/ALLOCATE_HEAP)
    self.tcharsize    = Ptr_New(/ALLOCATE_HEAP)
    self.tickinterval = Ptr_New(/ALLOCATE_HEAP)
    self.ticknames    = Ptr_New(/ALLOCATE_HEAP)
    self.tlocation    = Ptr_New(/ALLOCATE_HEAP)
    self.width        = Ptr_New(/ALLOCATE_HEAP)

;---------------------------------------------------------------------
;Window //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Superclass. Use the target's window. If no target was given, get the current window.
    success = self -> MrGrAtom::INIT(TARGET=target, /CURRENT, NAME=name, HIDE=hide, WINREFRESH=refreshIn)
    if success eq 0 then message, 'Unable to initialize MrGrAtom'

;-----------------------------------------------------
;TARGET And CBLOCATION \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;
    ;If an object is provided, then its RANGE, CTINDEX, and PALETTE will
    ;be adopted by the colorbar within the SetProperty method. Furthermore, it will
    ;be assumed that you want the colorbar placed next to the graphic, so CBLOCATION
    ;will default to 'RIGHT'
    ;
    ;In the case that CBLOCATION is not the empty string (''), e.g. when GRAPHIC is
    ;provided, the colorbar will be positioned next to the last item displayed (or GRAPHIC,
    ;if present). As such, RIGHT, TOP, TLOCATION, and VERTICAL must remain as-is (event
    ;if they are undefined) until the SetProperty method is called below. Within SetProperty,
    ;default values will be chosen for these quantities.
    ;
    
    ;Graphic object
    self.target = Obj_New()
    
    ;CBLOCATION must also remain as-is if a graphic was provided.
    IF (N_Elements(target) EQ 0) || (Obj_Valid(target) EQ 0) THEN BEGIN
        SetDefaultValue, cbLocation, ''
        SetDefaultValue, top, 1, /BOOLEAN
        SetDefaultValue, vertical, 0, /BOOLEAN
        SetDefaultValue, right, 0, /BOOLEAN
        SetDefaultValue, tlocation, 'TOP'
    ENDIF
    
;-----------------------------------------------------
;Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Set Default Values
    annotatecolor = cgDefaultColor(annotatecolor, DEFAULT="opposite")
    setDefaultValue, bottom, 0
    setDefaultValue, brewer, 0
    setDefaultValue, charpercent, 0.85
    setDefaultValue, charsize, 1.5
    setDefaultValue, color, annotatecolor
    discrete=Keyword_Set(discrete)
    setDefaultValue, divisions, 0
    fit = Keyword_Set(fit)
    setDefaultValue, format, ''
    invertcolors = Keyword_Set(invertcolors)
    setDefaultValue, ncolors, 256
    setDefaultValue, minor, 2
    setDefaultValue, oob_factor, 1.0
    reverse = Keyword_Set(reverse)
    setDefaultValue, title, ''
    setDefaultValue, ticklen, 0.25
    setDefaultValue, tcharsize, charsize
    setDefaultValue, textthick, 1.0
    xlog = Keyword_Set(xlog)
    ylog = Keyword_Set(ylog)
    
    ;RANGE takes precedence over MINRANGE and MAXRANGE
    IF N_Elements(range) EQ 0 THEN BEGIN
        range = fltarr(2)
        IF N_Elements(minrange) EQ 0 THEN range[0] = 0       ELSE range[0] = minrange
        IF N_Elements(maxrange) EQ 0 THEN range[1] = ncolors ELSE range[1] = maxrange
    ENDIF

    
;-----------------------------------------------------
;Set Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    self -> SetProperty, ANNOTATECOLOR=annotatecolor, $
                         BOTTOM=bottom, $
                         BREWER=brewer, $
                         CBLOCATION=cbLocation, $
                         CHARPERCENT=charpercent, $
                         CHARSIZE=charsize, $
                         CLAMP=clamp, $
                         COLOR=color, $
                         CTINDEX=ctindex, $
                         DISCRETE=discrete, $
                         DIVISIONS=divisions, $
                         FIT=fit, $
                         FORMAT=format, $
                         INVERTCOLORS=invertcolors, $
                         MINOR=minor, $
                         NCOLORS=ncolors, $
                         NEUTRALINDEX=neutralIndex, $
                         OFFSET=offset, $
                         OOB_FACTOR=oob_factor, $
                         OOB_HIGH=oob_high, $
                         OOB_LOW=oob_low, $
                         POSITION=position, $
                         PALETTE=palette, $
                         RANGE=range, $
                         REVERSE=reverse, $
                         RIGHT=right, $
                         TARGET=target, $
                         TCHARSIZE=tcharsize, $
                         TEXTTHICK=textthick, $
                         TLOCATION=tlocation, $
                         TICKINTERVAL=tickinterval, $
                         TICKLEN=ticklen, $
                         TICKNAMES=ticknames, $
                         TOP=top, $
                         VERTICAL=vertical, $
                         WIDTH=width, $
                         XLOG=xlog, $
                         YLOG=ylog, $
                         _STRICT_EXTRA=extra

    ;Turn refresh back on?
    if n_elements(target) eq 0 $
        then self.window -> Refresh $
        else if refreshIn then self.window -> Refresh
    
    RETURN, 1
end


;+
; The class definition module for the object.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO weColorBar__define, class
    
    class = { weColorbar, $
              inherits MrGraphicsKeywords, $
              inherits MrGrAtom, $
             
              ;weColorbar Properties
              target: Obj_New(), $
              position: fltarr(4), $
             
              ;cgColorbar Properties
              annotatecolor: '', $
              bottom: 0, $
              brewer: 0, $
              cblocation: '', $
              charpercent: 0.0, $
              charsize: 0.0, $
              clamp: Ptr_New(), $
              ctindex: Ptr_New(), $
              discrete: 0, $
              divisions: 0, $
              fit: 0, $
              format: '', $
              invertcolors: 0, $
              minor: 0.0, $
              ncolors: 0, $
              neutralindex: Ptr_New(), $
              offset: Ptr_New(), $
              oob_factor: 0.0, $
              oob_high: Ptr_New(), $
              oob_low: Ptr_New(), $
              palette: Ptr_New(), $
              range: fltarr(2), $
              reverse: 0, $
              right: 0, $
              tcharsize: Ptr_New(), $
              textthick: 0.0, $
              tickinterval: Ptr_New(), $
              ticknames: Ptr_New(), $
              tlocation: Ptr_New(), $
              top: 0, $
              vertical: 0, $
              width: Ptr_New(), $
              xlog: 0, $
              ylog: 0 $
            }
end