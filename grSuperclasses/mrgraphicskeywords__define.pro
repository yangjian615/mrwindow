; docformat = 'rst'
;
; NAME:
;   MrGraphicsKeywords__Define
;
; PURPOSE:
;   Provides an object interface to handle IDL direct graphics plotting keywords. Basically,
;   any graphics keyword that is common to IDL plotting routines (e.g. Plot, Contour, 
;   Surface, etc.) is supported here.
;
;   Modifications::
;       Does not contain CHARSIZE, POSITION, XMARGIN or YMARGIN keywords. - MRA
;       2014/01/21  -   DATA, NORMAL, and DEVICE are no longer pointers. - MRA
;       2014/01/23  -   Added the _OverloadPrint method. Renamed to MrGraphicsKeywords__Define - MRA
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2012, by Fanning Software Consulting, Inc. All rights reserved.           ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
;+
;
;-
FUNCTION MrGraphicsKeywords::_OverloadPrint
    compile_opt strictarr
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = cgErrorMsg()
        RETURN, 0
    ENDIF

    ;Strings that will be used a lot.    
    joinStr = '   '
    undefined = '<IDL Default>'
    
    ;Prefix for the property values
    AxisColor  = string('AxisColor',  '=', FORMAT='(a-26, a-2)')
    Background = string('Background', '=', FORMAT='(a-26, a-2)')
    CharThick  = string('CharThick',  '=', FORMAT='(a-26, a-2)')
    Clip       = string('Clip',       '=', FORMAT='(a-26, a-2)')
    Color      = string('Color',      '=', FORMAT='(a-26, a-2)')
    Data       = string('Data',       '=', FORMAT='(a-26, a-2)')
    Device     = string('Device',     '=', FORMAT='(a-26, a-2)')
    Normal     = string('Normal',     '=', FORMAT='(a-26, a-2)')
    Font       = string('Font',       '=', FORMAT='(a-26, a-2)')
    LineStyle  = string('LineStyle',  '=', FORMAT='(a-26, a-2)')
    NoClip     = string('NoClip',     '=', FORMAT='(a-26, a-2)')
    NoData     = string('NoData',     '=', FORMAT='(a-26, a-2)')
    NoErase    = string('NoErase',    '=', FORMAT='(a-26, a-2)')
    PSym       = string('PSym',       '=', FORMAT='(a-26, a-2)')
    SubTitle   = string('SubTitle',   '=', FORMAT='(a-26, a-2)')
    SymSize    = string('SymSize',    '=', FORMAT='(a-26, a-2)')
    T3D        = string('T3D',        '=', FORMAT='(a-26, a-2)')
    Thick      = string('Thick',      '=', FORMAT='(a-26, a-2)')
    TickLen    = string('TickLen',    '=', FORMAT='(a-26, a-2)')
    Title      = string('Title',      '=', FORMAT='(a-26, a-2)')
    
    XCharSize     = string('XCharSize',     '=', FORMAT='(a-26, a-2)')
    XGridStyle    = string('XGridStyle',    '=', FORMAT='(a-26, a-2)')
    XMinor        = string('XMinor',        '=', FORMAT='(a-26, a-2)')
    XRange        = string('XRange',        '=', FORMAT='(a-26, a-2)')
    XStyle        = string('XStyle',        '=', FORMAT='(a-26, a-2)')
    XThick        = string('XThick',        '=', FORMAT='(a-26, a-2)')
    XTick_Get     = string('XTick_Get',     '=', FORMAT='(a-26, a-2)')
    XTickFormat   = string('XTickFormat',   '=', FORMAT='(a-26, a-2)')
    XTickInterval = string('XTickInterval', '=', FORMAT='(a-26, a-2)')
    XTickLayout   = string('XTickLayout',   '=', FORMAT='(a-26, a-2)')
    XTickLen      = string('XTickLen',      '=', FORMAT='(a-26, a-2)')
    XTickName     = string('XTickName',     '=', FORMAT='(a-26, a-2)')
    XTicks        = string('XTicks',        '=', FORMAT='(a-26, a-2)')
    XTickUnits    = string('XTickUnits',    '=', FORMAT='(a-26, a-2)')
    XTickV        = string('XTickV',        '=', FORMAT='(a-26, a-2)')
    XTitle        = string('XTitle',        '=', FORMAT='(a-26, a-2)')
    
    YCharSize     = string('YCharSize',     '=', FORMAT='(a-26, a-2)')
    YGridStyle    = string('YGridStyle',    '=', FORMAT='(a-26, a-2)')
    YMinor        = string('YMinor',        '=', FORMAT='(a-26, a-2)')
    YRange        = string('YRange',        '=', FORMAT='(a-26, a-2)')
    YStyle        = string('YStyle',        '=', FORMAT='(a-26, a-2)')
    YThick        = string('YThick',        '=', FORMAT='(a-26, a-2)')
    YTick_Get     = string('YTick_Get',     '=', FORMAT='(a-26, a-2)')
    YTickFormat   = string('YTickFormat',   '=', FORMAT='(a-26, a-2)')
    YTickInterval = string('YTickInterval', '=', FORMAT='(a-26, a-2)')
    YTickLayout   = string('YTickLayout',   '=', FORMAT='(a-26, a-2)')
    YTickLen      = string('YTickLen',      '=', FORMAT='(a-26, a-2)')
    YTickName     = string('YTickName',     '=', FORMAT='(a-26, a-2)')
    YTicks        = string('YTicks',        '=', FORMAT='(a-26, a-2)')
    YTickUnits    = string('YTickUnits',    '=', FORMAT='(a-26, a-2)')
    YTickV        = string('YTickV',        '=', FORMAT='(a-26, a-2)')
    YTitle        = string('YTitle',        '=', FORMAT='(a-26, a-2)')
    
    ZCharSize     = string('ZCharSize',     '=', FORMAT='(a-26, a-2)')
    ZGridStyle    = string('ZGridStyle',    '=', FORMAT='(a-26, a-2)')
    ZMinor        = string('ZMinor',        '=', FORMAT='(a-26, a-2)')
    ZRange        = string('ZRange',        '=', FORMAT='(a-26, a-2)')
    ZStyle        = string('ZStyle',        '=', FORMAT='(a-26, a-2)')
    ZThick        = string('ZThick',        '=', FORMAT='(a-26, a-2)')
    ZTick_Get     = string('ZTick_Get',     '=', FORMAT='(a-26, a-2)')
    ZTickFormat   = string('ZTickFormat',   '=', FORMAT='(a-26, a-2)')
    ZTickInterval = string('ZTickInterval', '=', FORMAT='(a-26, a-2)')
    ZTickLayout   = string('ZTickLayout',   '=', FORMAT='(a-26, a-2)')
    ZTickLen      = string('ZTickLen',      '=', FORMAT='(a-26, a-2)')
    ZTickName     = string('ZTickName',     '=', FORMAT='(a-26, a-2)')
    ZTicks        = string('ZTicks',        '=', FORMAT='(a-26, a-2)')
    ZTickUnits    = string('ZTickUnits',    '=', FORMAT='(a-26, a-2)')
    ZTickV        = string('ZTickV',        '=', FORMAT='(a-26, a-2)')
    ZTitle        = string('ZTitle',        '=', FORMAT='(a-26, a-2)')
    
    ;Append the property values
    IF N_Elements(*self.axiscolor)  EQ 0 THEN axiscolor  += undefined ELSE axiscolor  += "'" + String(*self.axiscolor,  FORMAT='(a0)') + "'"
    IF N_Elements(*self.background) EQ 0 THEN background += undefined ELSE background += "'" + String(*self.background, FORMAT='(a0)') + "'"
    IF N_Elements(*self.charthick)  EQ 0 THEN charthick  += undefined ELSE charthick  += String(*self.charthick,  FORMAT='(f0)')
    IF N_Elements(*self.clip)       EQ 0 THEN clip       += undefined ELSE clip       += String(*self.clip,       FORMAT='(4(f0, 3x))')
    IF N_Elements(*self.color)      EQ 0 THEN color      += undefined ELSE color      += StrJoin("'" + String(*self.color, FORMAT='(a0)') + "'", joinStr)
    IF N_Elements( self.data)       EQ 0 THEN data       += undefined ELSE data       += String( self.data,       FORMAT='(i0)')
    IF N_Elements( self.device)     EQ 0 THEN device     += undefined ELSE device     += String( self.device,     FORMAT='(i0)')
    IF N_Elements( self.normal)     EQ 0 THEN normal     += undefined ELSE normal     += String( self.normal,     FORMAT='(i0)')
    IF N_Elements(*self.font)       EQ 0 THEN font       += undefined ELSE font       += String(*self.font,       FORMAT='(i0)')
    IF N_Elements(*self.linestyle)  EQ 0 THEN linestyle  += undefined ELSE linestyle  += StrJoin(String(*self.linestyle, FORMAT='(i0)'), joinStr)
    IF N_Elements(*self.noclip)     EQ 0 THEN noclip     += undefined ELSE noclip     += String(*self.noclip,     FORMAT='(i0)')
    IF N_Elements(*self.nodata)     EQ 0 THEN nodata     += undefined ELSE nodata     += String(*self.nodata,     FORMAT='(i0)')
    IF N_Elements(*self.noerase)    EQ 0 THEN noerase    += undefined ELSE noerase    += String(*self.noerase,    FORMAT='(i0)')
    IF N_Elements(*self.psym)       EQ 0 THEN psym       += undefined ELSE psym       += StrJoin(String(*self.psym, FORMAT='(i0)'), joinStr)
    IF N_Elements(*self.subtitle)   EQ 0 THEN subtitle   += undefined ELSE subtitle   += String(*self.subtitle,   FORMAT='(a0)')
    IF N_Elements(*self.symsize)    EQ 0 THEN symsize    += undefined ELSE symsize    += StrJoin(String(*self.symsize, FORMAT='(i0)'), joinStr)
    IF N_Elements(*self.t3d)        EQ 0 THEN t3d        += undefined ELSE t3d        += String(*self.t3d,        FORMAT='(i0)')
    IF N_Elements(*self.thick)      EQ 0 THEN thick      += undefined ELSE thick      += String(*self.thick,      FORMAT='(f0)')
    IF N_Elements(*self.ticklen)    EQ 0 THEN ticklen    += undefined ELSE ticklen    += String(*self.ticklen,    FORMAT='(i0)')
    IF N_Elements(*self.title)      EQ 0 THEN title      += undefined ELSE title      += "'" + String(*self.title,      FORMAT='(a0)') + "'"

    IF N_Elements(*self.xcharsize)     EQ 0 THEN xcharsize     += undefined ELSE xcharsize     += String(*self.xcharsize,     FORMAT='(f0)')
    IF N_Elements(*self.xgridstyle)    EQ 0 THEN xgridstyle    += undefined ELSE xgridstyle    += String(*self.xgridstyle,    FORMAT='(i0)')
    IF N_Elements(*self.xminor)        EQ 0 THEN xminor        += undefined ELSE xminor        += String(*self.xminor,        FORMAT='(i0)')
    IF N_Elements(*self.xrange)        EQ 0 THEN xrange        += undefined ELSE xrange        += String(*self.xrange,        FORMAT='(2(f0, 3x))')
    IF N_Elements(*self.xstyle)        EQ 0 THEN xstyle        += undefined ELSE xstyle        += String(*self.xstyle,        FORMAT='(i0)')
    IF N_Elements(*self.xthick)        EQ 0 THEN xthick        += undefined ELSE xthick        += String(*self.xthick,        FORMAT='(f0)')
    IF N_Elements(*self.xtick_get)     EQ 0 THEN xtick_get     += undefined ELSE xtick_get     += StrJoin(String(*self.xtick_get, FORMAT='(f0)'), joinStr)
    IF N_Elements(*self.xtickformat)   EQ 0 THEN xtickformat   += undefined ELSE xtickformat   += "'" + String(*self.xtickformat,   FORMAT='(a0)') + "'"
    IF N_Elements(*self.xtickinterval) EQ 0 THEN xtickinterval += undefined ELSE xtickinterval += String(*self.xtickinterval, FORMAT='(f0)')
    IF N_Elements(*self.xticklayout)   EQ 0 THEN xticklayout   += undefined ELSE xticklayout   += String(*self.xticklayout,   FORMAT='(i0)')
    IF N_Elements(*self.xticklen)      EQ 0 THEN xticklen      += undefined ELSE xticklen      += String(*self.xticklen,      FORMAT='(f0)')
    IF N_Elements(*self.xtickname)     EQ 0 THEN xtickname     += undefined ELSE xtickname     += StrJoin("'" + String(*self.xtickname, FORMAT='(a0)') + "'", joinStr)
    IF N_Elements(*self.xticks)        EQ 0 THEN xticks        += undefined ELSE xticks        += String(*self.xticks,        FORMAT='(i0)')
    IF N_Elements(*self.xtickunits)    EQ 0 THEN xtickunits    += undefined ELSE xtickunits    += StrJoin("'" + String(*self.xtickunits, FORMAT='(a0)' + "'"), joinStr)
    IF N_Elements(*self.xtickv)        EQ 0 THEN xtickv        += undefined ELSE xtickv        += StrJoin(String(*self.xtickv, FORMAT='(f0)'), joinStr)
    IF N_Elements(*self.xtitle)        EQ 0 THEN xtitle        += undefined ELSE xtitle        += "'" + String(*self.xtitle,        FORMAT='(a0)') + "'"

    IF N_Elements(*self.ycharsize)     EQ 0 THEN ycharsize     += undefined ELSE ycharsize     += String(*self.ycharsize,     FORMAT='(f0)')
    IF N_Elements(*self.ygridstyle)    EQ 0 THEN ygridstyle    += undefined ELSE ygridstyle    += String(*self.ygridstyle,    FORMAT='(i0)')
    IF N_Elements(*self.yminor)        EQ 0 THEN yminor        += undefined ELSE yminor        += String(*self.yminor,        FORMAT='(i0)')
    IF N_Elements(*self.yrange)        EQ 0 THEN yrange        += undefined ELSE yrange        += String(*self.yrange,        FORMAT='(2(f0, 3x))')
    IF N_Elements(*self.ystyle)        EQ 0 THEN ystyle        += undefined ELSE ystyle        += String(*self.ystyle,        FORMAT='(i0)')
    IF N_Elements(*self.ythick)        EQ 0 THEN ythick        += undefined ELSE ythick        += String(*self.ythick,        FORMAT='(f0)')
    IF N_Elements(*self.ytick_get)     EQ 0 THEN ytick_get     += undefined ELSE ytick_get     += StrJoin(String(*self.ytick_get, FORMAT='(f0)'), joinStr)
    IF N_Elements(*self.ytickformat)   EQ 0 THEN ytickformat   += undefined ELSE ytickformat   += "'" + String(*self.ytickformat,   FORMAT='(a0)') + "'"
    IF N_Elements(*self.ytickinterval) EQ 0 THEN ytickinterval += undefined ELSE ytickinterval += String(*self.ytickinterval, FORMAT='(f0)')
    IF N_Elements(*self.yticklayout)   EQ 0 THEN yticklayout   += undefined ELSE yticklayout   += String(*self.yticklayout,   FORMAT='(i0)')
    IF N_Elements(*self.yticklen)      EQ 0 THEN yticklen      += undefined ELSE yticklen      += String(*self.yticklen,      FORMAT='(f0)')
    IF N_Elements(*self.ytickname)     EQ 0 THEN ytickname     += undefined ELSE ytickname     += StrJoin("'" + String(*self.ytickname, FORMAT='(a0)') + "'", joinStr)
    IF N_Elements(*self.yticks)        EQ 0 THEN yticks        += undefined ELSE yticks        += String(*self.yticks,        FORMAT='(i0)')
    IF N_Elements(*self.ytickunits)    EQ 0 THEN ytickunits    += undefined ELSE ytickunits    += StrJoin("'" + String(*self.ytickunits, FORMAT='(a0)' + "'"), joinStr)
    IF N_Elements(*self.ytickv)        EQ 0 THEN ytickv        += undefined ELSE ytickv        += StrJoin(String(*self.ytickv, FORMAT='(f0)'), joinStr)
    IF N_Elements(*self.ytitle)        EQ 0 THEN ytitle        += undefined ELSE ytitle        += "'" + String(*self.ytitle,        FORMAT='(a0)') + "'"

    IF N_Elements(*self.zcharsize)     EQ 0 THEN zcharsize     += undefined ELSE zcharsize     += String(*self.zcharsize,     FORMAT='(f0)')
    IF N_Elements(*self.zgridstyle)    EQ 0 THEN zgridstyle    += undefined ELSE zgridstyle    += String(*self.zgridstyle,    FORMAT='(i0)')
    IF N_Elements(*self.zminor)        EQ 0 THEN zminor        += undefined ELSE zminor        += String(*self.zminor,        FORMAT='(i0)')
    IF N_Elements(*self.zrange)        EQ 0 THEN zrange        += undefined ELSE zrange        += String(*self.zrange,        FORMAT='(2(f0, 3x))')
    IF N_Elements(*self.zstyle)        EQ 0 THEN zstyle        += undefined ELSE zstyle        += String(*self.zstyle,        FORMAT='(i0)')
    IF N_Elements(*self.zthick)        EQ 0 THEN zthick        += undefined ELSE zthick        += String(*self.zthick,        FORMAT='(f0)')
    IF N_Elements(*self.ztick_get)     EQ 0 THEN ztick_get     += undefined ELSE ztick_get     += StrJoin(String(*self.ztick_get, FORMAT='(f0)'), joinStr)
    IF N_Elements(*self.ztickformat)   EQ 0 THEN ztickformat   += undefined ELSE ztickformat   += "'" + String(*self.ztickformat,   FORMAT='(a0)') + "'"
    IF N_Elements(*self.ztickinterval) EQ 0 THEN ztickinterval += undefined ELSE ztickinterval += String(*self.ztickinterval, FORMAT='(f0)')
    IF N_Elements(*self.zticklayout)   EQ 0 THEN zticklayout   += undefined ELSE zticklayout   += String(*self.zticklayout,   FORMAT='(i0)')
    IF N_Elements(*self.zticklen)      EQ 0 THEN zticklen      += undefined ELSE zticklen      += String(*self.zticklen,      FORMAT='(f0)')
    IF N_Elements(*self.ztickname)     EQ 0 THEN ztickname     += undefined ELSE ztickname     += StrJoin("'" + String(*self.ztickname, FORMAT='(a0)') + "'", joinStr)
    IF N_Elements(*self.zticks)        EQ 0 THEN zticks        += undefined ELSE zticks        += String(*self.zticks,        FORMAT='(i0)')
    IF N_Elements(*self.ztickunits)    EQ 0 THEN ztickunits    += undefined ELSE ztickunits    += StrJoin("'" + String(*self.ztickunits, FORMAT='(a0)' + "'"), joinStr)
    IF N_Elements(*self.ztickv)        EQ 0 THEN ztickv        += undefined ELSE ztickv        += StrJoin(String(*self.ztickv, FORMAT='(f0)'), joinStr)
    IF N_Elements(*self.ztitle)        EQ 0 THEN ztitle        += undefined ELSE ztitle        += "'" + String(*self.ztitle,        FORMAT='(a0)') + "'"

    outStr = [ AxisColor, $
               Background, $
               CharThick, $
               Clip, $
               Color, $
               Data, $
               Device, $
               Normal, $
               Font, $
               LineStyle, $
               NoClip, $
               NoData, $
               NoErase, $
               PSym, $
               SubTitle, $
               SymSize, $
               T3D, $
               Thick, $
               TickLen, $
               Title, $
               XCharSize, $
               XGridStyle, $
               XMinor, $
               XRange, $
               XStyle, $
               XThick, $
               XTick_Get, $
               XTickFormat, $
               XTickInterval, $
               XTickLayout, $
               XTickLen, $
               XTickName, $
               XTicks, $
               XTickUnits, $
               XTickV, $
               XTitle, $
               YCharSize, $
               YGridStyle, $
               YMinor, $
               YRange, $
               YStyle, $
               YThick, $
               YTick_Get, $
               YTickFormat, $
               YTickInterval, $
               YTickLayout, $
               YTickLen, $
               YTickName, $
               YTicks, $
               YTickUnits, $
               YTickV, $
               YTitle, $
               ZCharSize, $
               ZGridStyle, $
               ZMinor, $
               ZRange, $
               ZStyle, $
               ZThick, $
               ZTick_Get, $
               ZTickFormat, $
               ZTickInterval, $
               ZTickLayout, $
               ZTickLen, $
               ZTickName, $
               ZTicks, $
               ZTickUnits, $
               ZTickV, $
               ZTitle $
             ]

    return, transpose(outStr)
END


;+
; Provides an object interface to handle IDL direct graphics plotting keywords. Basically,
; any graphics keyword that is common to IDL plotting routines (e.g. Plot, Contour, 
; Surface, etc.) is supported here. See the IDL documentation for "Graphics Keywords for
; a complete list.
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;           
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 16 May 2012, by David W. Fanning.
;        Added missing LINESTYLE keyword. 22 May 2012. DWF.
;        BIG problem in the way I was handling the PSYM keyword solved! 18 July 2012. DWF.
;        04/26/2013 -   Removed XMARGIN and YMARGIN keywords because they were conflicting
;                           causing duplicate definitions in a class of mine. - Matthew R Argall
;        09/27/2013 -   Removed the POSITION keyword. - MRA
;        2013/11/23 -   Removed the CHARSIZE keyword. - MRA
;        2014/01/21 -   DATA, NORMAL, and DEVICE are no longer pointers. - MRA
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-

;+
; This method initializes the object. Any "graphics keyword" that falls under
; the IDL definition is allowed. Plus, there are a few "Coyote Graphics" specific
; keywords (e.g., AXISCOLOR) that are not allowed in normal IDL plotting routines.
; Colors are handled in the Coyote Graphics way, as color names, primarily.
;-
FUNCTION MrGraphicsKeywords::INIT, $
    AXISCOLOR=axiscolor, $
    BACKGROUND=background, $
;    CHARSIZE=charsize, $
    CHARTHICK=charthick, $
    CLIP=clip, $
    COLOR=color, $
    DATA=data, $
    DEVICE=device, $
    LINESTYLE=linestyle, $
    NORMAL=normal, $
    FONT=font, $
    NOCLIP=noclip, $
    NODATA=nodata, $
    NOERASE=noerase, $
;    POSITION=position, $
    PSYM=psym, $
    SUBTITLE=subtitle, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
    TICKLEN=ticklen, $
    TITLE=title, $
    
    XCHARSIZE=xcharsize, $
    XGRIDSTYLE=xgridstyle, $
;    XMARGIN=xmargin, $
    XMINOR=xminor, $
    XRANGE=xrange, $
    XSTYLE=xstyle, $
    XTHICK=xthick, $
    XTICK_GET=xtick_get, $
    XTICKFORMAT=xtickformat, $
    XTICKINTERVAL=xtickinterval, $
    XTICKLAYOUT=xticklayout, $
    XTICKLEN=xticklen, $
    XTICKNAME=xtickname, $
    XTICKS=xticks, $
    XTICKUNITS=xtickunits, $
    XTICKV=xtickv, $
    XTITLE=xtitle, $
    
    YCHARSIZE=ycharsize, $
    YGRIDSTYLE=ygridstyle, $
;    YMARGIN=ymargin, $
    YMINOR=yminor, $
    YRANGE=yrange, $
    YSTYLE=ystyle, $
    YTHICK=ythick, $
    YTICK_GET=ytick_get, $
    YTICKFORMAT=ytickformat, $
    YTICKINTERVAL=ytickinterval, $
    YTICKLAYOUT=yticklayout, $
    YTICKLEN=yticklen, $
    YTICKNAME=ytickname, $
    YTICKS=yticks, $
    YTICKUNITS=ytickunits, $
    YTICKV=ytickv, $
    YTITLE=ytitle, $
   
    ZCHARSIZE=zcharsize, $
    ZGRIDSTYLE=zgridstyle, $
    ZMARGIN=zmargin, $
    ZMINOR=zminor, $
    ZRANGE=zrange, $
    ZSTYLE=zstyle, $
    ZTHICK=zthick, $
    ZTICK_GET=ztick_get, $
    ZTICKFORMAT=ztickformat, $
    ZTICKINTERVAL=ztickinterval, $
    ZTICKLAYOUT=zticklayout, $
    ZTICKLEN=zticklen, $
    ZTICKNAME=ztickname, $
    ZTICKS=zticks, $
    ZTICKUNITS=ztickunits, $
    ZTICKV=ztickv, $
    ZTITLE=ztitle, $
    ZVALUE=zvalue
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = cgErrorMsg()
        RETURN, 0
    ENDIF
    
;---------------------------------------------------------------------
;Default Values //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    IF N_Elements(background) EQ 0 THEN background = 'white'
    IF N_Elements(axiscolor) EQ 0 THEN axiscolor = 'opposite' 
    IF N_Elements(color) EQ 0 THEN color = 'opposite'
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=!P.Font)
    device = keyword_set(device)
    normal = keyword_set(normal)
    data = keyword_set(data)
    
    ;For all direct graphics types, DATA is either ignored (in which case NORMAL is the
    ;default) or it is the default coordinate system.
    if data + normal + device eq 0 then data = 1
    
;---------------------------------------------------------------------
;Allocate Heap ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    self.axiscolor = Ptr_New(/Allocate_Heap)
    self.background = Ptr_New(/Allocate_Heap)
;    self.charsize = Ptr_New(/Allocate_Heap)
    self.charthick = Ptr_New(/Allocate_Heap)
    self.clip = Ptr_New(/Allocate_Heap)
    self.color = Ptr_New(/Allocate_Heap)
;    self.data = Ptr_New(/Allocate_Heap)
;    self.device = Ptr_New(/Allocate_Heap)
    self.font = Ptr_New(/Allocate_Heap)
    self.linestyle = Ptr_New(/Allocate_Heap)
;    self.normal = Ptr_New(/Allocate_Heap)
    self.noclip = Ptr_New(/Allocate_Heap)
    self.nodata = Ptr_New(/Allocate_Heap)
    self.noerase = Ptr_New(/Allocate_Heap)
;    self.position = Ptr_New(/Allocate_Heap)
    self.psym = Ptr_New(/Allocate_Heap)
    self.subtitle = Ptr_New(/Allocate_Heap)
    self.symsize = Ptr_New(/Allocate_Heap)
    self.t3d = Ptr_New(/Allocate_Heap)
    self.thick = Ptr_New(/Allocate_Heap)
    self.ticklen = Ptr_New(/Allocate_Heap)
    self.title = Ptr_New(/Allocate_Heap)
    
    self.xcharsize = Ptr_New(/Allocate_Heap)
    self.xgridstyle = Ptr_New(/Allocate_Heap)
;    self.xmargin = Ptr_New(/Allocate_Heap)
    self.xminor = Ptr_New(/Allocate_Heap)
    self.xrange = Ptr_New(/Allocate_Heap)
    self.xstyle = Ptr_New(/Allocate_Heap)
    self.xthick = Ptr_New(/Allocate_Heap)
    self.xtick_get = Ptr_New(/Allocate_Heap)
    self.xtickformat = Ptr_New(/Allocate_Heap)
    self.xtickinterval = Ptr_New(/Allocate_Heap)
    self.xticklayout = Ptr_New(/Allocate_Heap)
    self.xticklen = Ptr_New(/Allocate_Heap)
    self.xtickname = Ptr_New(/Allocate_Heap)
    self.xticks = Ptr_New(/Allocate_Heap)
    self.xtickunits = Ptr_New(/Allocate_Heap)
    self.xtickv = Ptr_New(/Allocate_Heap)
    self.xtitle = Ptr_New(/Allocate_Heap)
    
    self.ycharsize = Ptr_New(/Allocate_Heap)
    self.ygridstyle = Ptr_New(/Allocate_Heap)
;    self.ymargin = Ptr_New(/Allocate_Heap)
    self.yminor = Ptr_New(/Allocate_Heap)
    self.yrange = Ptr_New(/Allocate_Heap)
    self.ystyle = Ptr_New(/Allocate_Heap)
    self.ythick = Ptr_New(/Allocate_Heap)
    self.ytick_get = Ptr_New(/Allocate_Heap)
    self.ytickformat = Ptr_New(/Allocate_Heap)
    self.ytickinterval = Ptr_New(/Allocate_Heap)
    self.yticklayout = Ptr_New(/Allocate_Heap)
    self.yticklen = Ptr_New(/Allocate_Heap)
    self.ytickname = Ptr_New(/Allocate_Heap)
    self.yticks = Ptr_New(/Allocate_Heap)
    self.ytickunits = Ptr_New(/Allocate_Heap)
    self.ytickv = Ptr_New(/Allocate_Heap)
    self.ytitle = Ptr_New(/Allocate_Heap)
       
    self.zcharsize = Ptr_New(/Allocate_Heap)
    self.zgridstyle = Ptr_New(/Allocate_Heap)
    self.zmargin = Ptr_New(/Allocate_Heap)
    self.zminor = Ptr_New(/Allocate_Heap)
    self.zrange = Ptr_New(/Allocate_Heap)
    self.zstyle = Ptr_New(/Allocate_Heap)
    self.zthick = Ptr_New(/Allocate_Heap)
    self.ztick_get = Ptr_New(/Allocate_Heap)
    self.ztickformat = Ptr_New(/Allocate_Heap)
    self.ztickinterval = Ptr_New(/Allocate_Heap)
    self.zticklayout = Ptr_New(/Allocate_Heap)
    self.zticklen = Ptr_New(/Allocate_Heap)
    self.ztickname = Ptr_New(/Allocate_Heap)
    self.zticks = Ptr_New(/Allocate_Heap)
    self.ztickunits = Ptr_New(/Allocate_Heap)
    self.ztickv = Ptr_New(/Allocate_Heap)
    self.ztitle = Ptr_New(/Allocate_Heap)
    self.zvalue = Ptr_New(/Allocate_Heap)
    
        
;---------------------------------------------------------------------
;Set Properties //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    self -> MrGraphicsKeywords::SetProperty, AXISCOLOR=axiscolor, $
                                             BACKGROUND=background, $
;                                             CHARSIZE=charsize, $
                                             CHARTHICK=charthick, $
                                             CLIP=clip, $
                                             COLOR=color, $
                                             DATA=data, $
                                             DEVICE=device, $
                                             FONT=font, $
                                             LINESTYLE=linestyle, $
                                             NORMAL=normal, $
                                             NOCLIP=noclip, $
                                             NODATA=nodata, $
                                             NOERASE=noerase, $
;                                             POSITION=position, $
                                             PSYM=psym, $
                                             SUBTITLE=subtitle, $
                                             SYMSIZE=symsize, $
                                             T3D=t3d, $
                                             THICK=thick, $
                                             TICKLEN=ticklen, $
                                             TITLE=title, $
     
                                             XCHARSIZE=xcharsize, $
                                             XGRIDSTYLE=xgridstyle, $
;                                             XMARGIN=xmargin, $
                                             XMINOR=xminor, $
                                             XRANGE=xrange, $
                                             XSTYLE=xstyle, $
                                             XTHICK=xthick, $
                                             XTICK_GET=xtick_get, $
                                             XTICKFORMAT=xtickformat, $
                                             XTICKINTERVAL=xtickinterval, $
                                             XTICKLAYOUT=xticklayout, $
                                             XTICKLEN=xticklen, $
                                             XTICKNAME=xtickname, $
                                             XTICKS=xticks, $
                                             XTICKUNITS=xtickunits, $
                                             XTICKV=xtickv, $
                                             XTITLE=xtitle, $
     
                                             YCHARSIZE=ycharsize, $
                                             YGRIDSTYLE=ygridstyle, $
;                                             YMARGIN=ymargin, $
                                             YMINOR=yminor, $
                                             YRANGE=yrange, $
                                             YSTYLE=ystyle, $
                                             YTHICK=ythick, $
                                             YTICK_GET=ytick_get, $
                                             YTICKFORMAT=ytickformat, $
                                             YTICKINTERVAL=ytickinterval, $
                                             YTICKLAYOUT=yticklayout, $
                                             YTICKLEN=yticklen, $
                                             YTICKNAME=ytickname, $
                                             YTICKS=yticks, $
                                             YTICKUNITS=ytickunits, $
                                             YTICKV=ytickv, $
                                             YTITLE=ytitle, $
    
                                             ZCHARSIZE=zcharsize, $
                                             ZGRIDSTYLE=zgridstyle, $
                                             ZMARGIN=zmargin, $
                                             ZMINOR=zminor, $
                                             ZRANGE=zrange, $
                                             ZSTYLE=zstyle, $
                                             ZTHICK=zthick, $
                                             ZTICK_GET=ztick_get, $
                                             ZTICKFORMAT=ztickformat, $
                                             ZTICKINTERVAL=ztickinterval, $
                                             ZTICKLAYOUT=zticklayout, $
                                             ZTICKLEN=zticklen, $
                                             ZTICKNAME=ztickname, $
                                             ZTICKS=zticks, $
                                             ZTICKUNITS=ztickunits, $
                                             ZTICKV=ztickv, $
                                             ZTITLE=ztitle, $
     
                                             ZVALUE=zvalue
    
    RETURN, 1
END 

;+
; The clean-up method for the object. Nearly all keywords are stored as pointers
; that must be cleaned up here.
;-
PRO MrGraphicsKeywords::CLEANUP

    Ptr_Free, self.axiscolor
    Ptr_Free, self.background
;    Ptr_Free, self.charsize
    Ptr_Free, self.charthick
    Ptr_Free, self.clip
    Ptr_Free, self.color
;    Ptr_Free, self.data
;    Ptr_Free, self.device
    Ptr_Free, self.font
    Ptr_Free, self.linestyle
;    Ptr_Free, self.normal
    Ptr_Free, self.noclip
    Ptr_Free, self.nodata
    Ptr_Free, self.noerase
;    Ptr_Free, self.position
    Ptr_Free, self.psym
    Ptr_Free, self.subtitle
    Ptr_Free, self.symsize
    Ptr_Free, self.t3d
    Ptr_Free, self.thick
    Ptr_Free, self.ticklen
    Ptr_Free, self.title
    
    Ptr_Free, self.xcharsize
    Ptr_Free, self.xgridstyle
;    Ptr_Free, self.xmargin
    Ptr_Free, self.xminor
    Ptr_Free, self.xrange
    Ptr_Free, self.xstyle
    Ptr_Free, self.xthick
    Ptr_Free, self.xtick_get
    Ptr_Free, self.xtickformat
    Ptr_Free, self.xtickinterval
    Ptr_Free, self.xticklayout
    Ptr_Free, self.xticklen
    Ptr_Free, self.xtickname
    Ptr_Free, self.xticks
    Ptr_Free, self.xtickunits
    Ptr_Free, self.xtickv
    Ptr_Free, self.xtitle
    
    Ptr_Free, self.ycharsize
    Ptr_Free, self.ygridstyle
;    Ptr_Free, self.ymargin
    Ptr_Free, self.yminor
    Ptr_Free, self.yrange
    Ptr_Free, self.ystyle
    Ptr_Free, self.ythick
    Ptr_Free, self.ytick_get
    Ptr_Free, self.ytickformat
    Ptr_Free, self.ytickinterval
    Ptr_Free, self.yticklayout
    Ptr_Free, self.yticklen
    Ptr_Free, self.ytickname
    Ptr_Free, self.yticks
    Ptr_Free, self.ytickunits
    Ptr_Free, self.ytickv
    Ptr_Free, self.ytitle
       
    Ptr_Free, self.zcharsize
    Ptr_Free, self.zgridstyle
    Ptr_Free, self.zmargin
    Ptr_Free, self.zminor
    Ptr_Free, self.zrange
    Ptr_Free, self.zstyle
    Ptr_Free, self.zthick
    Ptr_Free, self.ztick_get
    Ptr_Free, self.ztickformat
    Ptr_Free, self.ztickinterval
    Ptr_Free, self.zticklayout
    Ptr_Free, self.zticklen
    Ptr_Free, self.ztickname
    Ptr_Free, self.zticks
    Ptr_Free, self.ztickunits
    Ptr_Free, self.ztickv
    Ptr_Free, self.ztitle
    Ptr_Free, self.zvalue
END

;+
; The GetProperty method is the way graphics routines obtain the keyword values for
; the graphics keywords.
;-
PRO MrGraphicsKeywords::GetProperty, $
    AXISCOLOR=axiscolor, $
    BACKGROUND=background, $
;    CHARSIZE=charsize, $
    CHARTHICK=charthick, $
    CLIP=clip, $
    COLOR=color, $
    DATA=data, $
    DEVICE=device, $
    FONT=font, $
    LINESTYLE=linestyle, $
    NORMAL=normal, $
    NOCLIP=noclip, $
    NODATA=nodata, $
    NOERASE=noerase, $
;    POSITION=position, $
    PSYM=psym, $
    SUBTITLE=subtitle, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
    TICKLEN=ticklen, $
    TITLE=title, $
    
    XCHARSIZE=xcharsize, $
    XGRIDSTYLE=xgridstyle, $
;    XMARGIN=xmargin, $
    XMINOR=xminor, $
    XRANGE=xrange, $
    XSTYLE=xstyle, $
    XTHICK=xthick, $
    XTICK_GET=xtick_get, $
    XTICKFORMAT=xtickformat, $
    XTICKINTERVAL=xtickinterval, $
    XTICKLAYOUT=xticklayout, $
    XTICKLEN=xticklen, $
    XTICKNAME=xtickname, $
    XTICKS=xticks, $
    XTICKUNITS=xtickunits, $
    XTICKV=xtickv, $
    XTITLE=xtitle, $
    
    YCHARSIZE=ycharsize, $
    YGRIDSTYLE=ygridstyle, $
;    YMARGIN=ymargin, $
    YMINOR=yminor, $
    YRANGE=yrange, $
    YSTYLE=ystyle, $
    YTHICK=ythick, $
    YTICK_GET=ytick_get, $
    YTICKFORMAT=ytickformat, $
    YTICKINTERVAL=ytickinterval, $
    YTICKLAYOUT=yticklayout, $
    YTICKLEN=yticklen, $
    YTICKNAME=ytickname, $
    YTICKS=yticks, $
    YTICKUNITS=ytickunits, $
    YTICKV=ytickv, $
    YTITLE=ytitle, $
   
    ZCHARSIZE=zcharsize, $
    ZGRIDSTYLE=zgridstyle, $
    ZMARGIN=zmargin, $
    ZMINOR=zminor, $
    ZRANGE=zrange, $
    ZSTYLE=zstyle, $
    ZTHICK=zthick, $
    ZTICK_GET=ztick_get, $
    ZTICKFORMAT=ztickformat, $
    ZTICKINTERVAL=ztickinterval, $
    ZTICKLAYOUT=zticklayout, $
    ZTICKLEN=zticklen, $
    ZTICKNAME=ztickname, $
    ZTICKS=zticks, $
    ZTICKUNITS=ztickunits, $
    ZTICKV=ztickv, $
    ZTITLE=ztitle, $
    ZVALUE=zvalue
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ; Get the properties of the object.
    IF Arg_Present(data)       THEN data   = self.data 
    IF Arg_Present(device)     THEN device = self.device
    IF Arg_Present(normal)     THEN normal = self.normal
    
    IF Arg_Present(axiscolor)  THEN IF N_Elements(*self.axiscolor)  NE 0 THEN axiscolor  = *self.axiscolor
    IF Arg_Present(background) THEN IF N_Elements(*self.background) NE 0 THEN background = *self.background
;    IF Arg_Present(charsize)  THEN IF N_Elements(*self.charsize)   NE 0 THEN charsize   = *self.charsize 
    IF Arg_Present(charthick)  THEN IF N_Elements(*self.charthick)  NE 0 THEN charthick  = *self.charthick 
    IF Arg_Present(clip)       THEN IF N_Elements(*self.clip)       NE 0 THEN clip       = *self.clip 
    IF Arg_Present(color)      THEN IF N_Elements(*self.color)      NE 0 THEN color      = *self.color
    IF Arg_Present(font)       THEN IF N_Elements(*self.font)       NE 0 THEN font       = *self.font
    IF Arg_Present(linestyle)  THEN IF N_Elements(*self.linestyle)  NE 0 THEN linestyle  = *self.linestyle
    IF Arg_Present(noclip)     THEN IF N_Elements(*self.noclip)     NE 0 THEN noclip     = *self.noclip
    IF Arg_Present(nodata)     THEN IF N_Elements(*self.nodata)     NE 0 THEN nodata     = *self.nodata 
    IF Arg_Present(noerase)    THEN IF N_Elements(*self.noerase)    NE 0 THEN noerase    = *self.noerase
;    IF Arg_Present(position)  THEN IF N_Elements(*self.position)   NE 0 THEN position   = *self.position
    IF Arg_Present(psym)       THEN IF N_Elements(*self.psym)       NE 0 THEN psym       = *self.psym
    IF Arg_Present(subtitle)   THEN IF N_Elements(*self.subtitle)   NE 0 THEN subtitle   = *self.subtitle
    IF Arg_Present(symsize)    THEN IF N_Elements(*self.symsize)    NE 0 THEN symsize    = *self.symsize
    IF Arg_Present(t3d)        THEN IF N_Elements(*self.t3d)        NE 0 THEN t3d        = *self.t3d 
    IF Arg_Present(thick)      THEN IF N_Elements(*self.thick)      NE 0 THEN thick      = *self.thick
    IF Arg_Present(ticklen)    THEN IF N_Elements(*self.ticklen)    NE 0 THEN ticklen    = *self.ticklen
    IF Arg_Present(title)      THEN IF N_Elements(*self.title)      NE 0 THEN title      = *self.title

    IF Arg_Present(xcharsize)     THEN IF N_Elements(*self.xcharsize)     NE 0 THEN xcharsize     = *self.xcharsize
    IF Arg_Present(xgridstyle)    THEN IF N_Elements(*self.xgridstyle)    NE 0 THEN xgridstyle    = *self.xgridstyle
    IF Arg_Present(xmargin)       THEN IF N_Elements(*self.xmargin)       NE 0 THEN xmargin       = *self.xmargin
;    IF Arg_Present(xminor)       THEN IF N_Elements(*self.xminor)        NE 0 THEN xminor        = *self.xminor
    IF Arg_Present(xrange)        THEN IF N_Elements(*self.xrange)        NE 0 THEN xrange        = *self.xrange
    IF Arg_Present(xstyle)        THEN IF N_Elements(*self.xstyle)        NE 0 THEN xstyle        = *self.xstyle
    IF Arg_Present(xthick)        THEN IF N_Elements(*self.xthick)        NE 0 THEN xthick        = *self.xthick
    IF Arg_Present(xtick_get)     THEN IF N_Elements(*self.xtick_get)     NE 0 THEN xtick_get     = *self.xtick_get
    IF Arg_Present(xtickformat)   THEN IF N_Elements(*self.xtickformat)   NE 0 THEN xtickformat   = *self.xtickformat
    IF Arg_Present(xtickinterval) THEN IF N_Elements(*self.xtickinterval) NE 0 THEN xtickinterval = *self.xtickinterval
    IF Arg_Present(xticklayout)   THEN IF N_Elements(*self.xticklayout)   NE 0 THEN xticklayout   = *self.xticklayout 
    IF Arg_Present(xticklen)      THEN IF N_Elements(*self.xticklen)      NE 0 THEN xticklen      = *self.xticklen 
    IF Arg_Present(xtickname)     THEN IF N_Elements(*self.xtickname)     NE 0 THEN xtickname     = *self.xtickname
    IF Arg_Present(xticks)        THEN IF N_Elements(*self.xticks)        NE 0 THEN xticks        = *self.xticks
    IF Arg_Present(xtickunits)    THEN IF N_Elements(*self.xtickunits)    NE 0 THEN xtickunits    = *self.xtickunits
    IF Arg_Present(xtickv)        THEN IF N_Elements(*self.xtickv)        NE 0 THEN xtickv        = *self.xtickv
    IF Arg_Present(xtitle)        THEN IF N_Elements(*self.xtitle)        NE 0 THEN xtitle        = *self.xtitle
    
    IF Arg_Present(ycharsize)     THEN IF N_Elements(*self.ycharsize)     NE 0 THEN ycharsize     = *self.ycharsize
    IF Arg_Present(ygridstyle)    THEN IF N_Elements(*self.ygridstyle)    NE 0 THEN ygridstyle    = *self.ygridstyle
;    IF Arg_Present(ymargin)      THEN IF N_Elements(*self.ymargin)       NE 0 THEN ymargin       = *self.ymargin
    IF Arg_Present(yminor)        THEN IF N_Elements(*self.yminor)        NE 0 THEN yminor        = *self.yminor 
    IF Arg_Present(yrange)        THEN IF N_Elements(*self.yrange)        NE 0 THEN yrange        = *self.yrange
    IF Arg_Present(ystyle)        THEN IF N_Elements(*self.ystyle)        NE 0 THEN ystyle        = *self.ystyle
    IF Arg_Present(ythick)        THEN IF N_Elements(*self.ythick)        NE 0 THEN ythick        = *self.ythick
    IF Arg_Present(ytick_get)     THEN IF N_Elements(*self.ytick_get)     NE 0 THEN ytick_get     = *self.ytick_get
    IF Arg_Present(ytickformat)   THEN IF N_Elements(*self.ytickformat)   NE 0 THEN ytickformat   = *self.ytickformat
    IF Arg_Present(ytickinterval) THEN IF N_Elements(*self.ytickinterval) NE 0 THEN ytickinterval = *self.ytickinterval
    IF Arg_Present(yticklayout)   THEN IF N_Elements(*self.yticklayout)   NE 0 THEN yticklayout   = *self.yticklayout 
    IF Arg_Present(yticklen)      THEN IF N_Elements(*self.yticklen)      NE 0 THEN yticklen      = *self.yticklen 
    IF Arg_Present(ytickname)     THEN IF N_Elements(*self.ytickname)     NE 0 THEN ytickname     = *self.ytickname
    IF Arg_Present(yticks)        THEN IF N_Elements(*self.yticks)        NE 0 THEN yticks        = *self.yticks
    IF Arg_Present(ytickunits)    THEN IF N_Elements(*self.ytickunits)    NE 0 THEN ytickunits    = *self.ytickunits
    IF Arg_Present(ytickv)        THEN IF N_Elements(*self.ytickv)        NE 0 THEN ytickv        = *self.ytickv
    IF Arg_Present(ytitle)        THEN IF N_Elements(*self.ytitle)        NE 0 THEN ytitle        = *self.ytitle

    IF Arg_Present(zcharsize)     THEN IF N_Elements(*self.zcharsize)     NE 0 THEN zcharsize     = *self.zcharsize
    IF Arg_Present(zgridstyle)    THEN IF N_Elements(*self.zgridstyle)    NE 0 THEN zgridstyle    = *self.zgridstyle
    IF Arg_Present(zmargin)       THEN IF N_Elements(*self.zmargin)       NE 0 THEN zmargin       = *self.zmargin
    IF Arg_Present(zminor)        THEN IF N_Elements(*self.zminor)        NE 0 THEN zminor        = *self.zminor 
    IF Arg_Present(zrange)        THEN IF N_Elements(*self.zrange)        NE 0 THEN zrange        = *self.zrange
    IF Arg_Present(zstyle)        THEN IF N_Elements(*self.zstyle)        NE 0 THEN zstyle        = *self.zstyle
    IF Arg_Present(zthick)        THEN IF N_Elements(*self.ztick_get)     NE 0 THEN zthick        = *self.ztick_get
    IF Arg_Present(ztick_get)     THEN IF N_Elements(*self.ztick_get)     NE 0 THEN ztick_get     = *self.ztick_get
    IF Arg_Present(ztickformat)   THEN IF N_Elements(*self.ztickformat)   NE 0 THEN ztickformat   = *self.ztickformat
    IF Arg_Present(ztickinterval) THEN IF N_Elements(*self.ztickinterval) NE 0 THEN ztickinterval = *self.ztickinterval
    IF Arg_Present(zticklayout)   THEN IF N_Elements(*self.zticklayout)   NE 0 THEN zticklayout   = *self.zticklayout 
    IF Arg_Present(zticklen)      THEN IF N_Elements(*self.zticklen)      NE 0 THEN zticklen      = *self.zticklen 
    IF Arg_Present(ztickname)     THEN IF N_Elements(*self.ztickname)     NE 0 THEN ztickname     = *self.ztickname
    IF Arg_Present(zticks)        THEN IF N_Elements(*self.zticks)        NE 0 THEN zticks        = *self.zticks
    IF Arg_Present(ztickunits)    THEN IF N_Elements(*self.ztickunits)    NE 0 THEN ztickunits    = *self.ztickunits
    IF Arg_Present(ztickv)        THEN IF N_Elements(*self.ztickv)        NE 0 THEN ztickv        = *self.ztickv
    IF Arg_Present(ztitle)        THEN IF N_Elements(*self.ztitle)        NE 0 THEN ztitle        = *self.ztitle
    IF Arg_Present(zvalue)        THEN IF N_Elements(*self.zvalue)        NE 0 THEN zvalue        = *self.zvalue

END

;+
; The SetProperty method is how these keyword values are set for the plotting routine.
;-
PRO MrGraphicsKeywords::SetProperty, $
    AXISCOLOR=axiscolor, $
    BACKGROUND=background, $
;    CHARSIZE=charsize, $
    CHARTHICK=charthick, $
    CLIP=clip, $
    COLOR=color, $
    DATA=data, $
    DEVICE=device, $
    NORMAL=normal, $
    FONT=font, $
    LINESTYLE=linestyle, $
    NOCLIP=noclip, $
    NODATA=nodata, $
    NOERASE=noerase, $
;    POSITION=position, $
    PSYM=psym, $
    SUBTITLE=subtitle, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
    TICKLEN=ticklen, $
    TITLE=title, $
    
    XCHARSIZE=xcharsize, $
    XGRIDSTYLE=xgridstyle, $
;    XMARGIN=xmargin, $
    XMINOR=xminor, $
    XRANGE=xrange, $
    XSTYLE=xstyle, $
    XTHICK=xthick, $
    XTICK_GET=xtick_get, $
    XTICKFORMAT=xtickformat, $
    XTICKINTERVAL=xtickinterval, $
    XTICKLAYOUT=xticklayout, $
    XTICKLEN=xticklen, $
    XTICKNAME=xtickname, $
    XTICKS=xticks, $
    XTICKUNITS=xtickunits, $
    XTICKV=xtickv, $
    XTITLE=xtitle, $
    
    YCHARSIZE=ycharsize, $
    YGRIDSTYLE=ygridstyle, $
;    YMARGIN=ymargin, $
    YMINOR=yminor, $
    YRANGE=yrange, $
    YSTYLE=ystyle, $
    YTHICK=ythick, $
    YTICK_GET=ytick_get, $
    YTICKFORMAT=ytickformat, $
    YTICKINTERVAL=ytickinterval, $
    YTICKLAYOUT=yticklayout, $
    YTICKLEN=yticklen, $
    YTICKNAME=ytickname, $
    YTICKS=yticks, $
    YTICKUNITS=ytickunits, $
    YTICKV=ytickv, $
    YTITLE=ytitle, $
   
    ZCHARSIZE=zcharsize, $
    ZGRIDSTYLE=zgridstyle, $
    ZMARGIN=zmargin, $
    ZMINOR=zminor, $
    ZRANGE=zrange, $
    ZSTYLE=zstyle, $
    ZTHICK=zthick, $
    ZTICK_GET=ztick_get, $
    ZTICKFORMAT=ztickformat, $
    ZTICKINTERVAL=ztickinterval, $
    ZTICKLAYOUT=zticklayout, $
    ZTICKLEN=zticklen, $
    ZTICKNAME=ztickname, $
    ZTICKS=zticks, $
    ZTICKUNITS=ztickunits, $
    ZTICKV=ztickv, $
    ZTITLE=ztitle, $
    ZVALUE=zvalue

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = cgErrorMsg()
        RETURN
    ENDIF
    
;---------------------------------------------------------------------
;General /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    IF N_Elements(axiscolor) NE 0 THEN *self.axiscolor = axiscolor
    IF N_Elements(background) NE 0 THEN *self.background = background
;    IF N_Elements(charsize) NE 0 THEN *self.charsize = charsize
    IF N_Elements(charthick) NE 0 THEN *self.charthick = charthick
    IF N_Elements(clip) NE 0 THEN *self.clip = clip 
    IF N_Elements(color) NE 0 THEN *self.color = color
    IF N_Elements(font) NE 0 THEN *self.font = font 
    IF N_Elements(linestyle) NE 0 THEN *self.linestyle = linestyle 
    IF N_Elements(noclip) NE 0 THEN *self.noclip = Keyword_Set(noclip)
    IF N_Elements(nodata) NE 0 THEN *self.nodata = Keyword_Set(nodata)
    IF N_Elements(noerase) NE 0 THEN *self.noerase = Keyword_Set(noerase)
;    IF N_Elements(position) NE 0 THEN *self.position = position 
    IF N_Elements(psym) NE 0 THEN *self.psym = psym
    IF N_Elements(subtitle) NE 0 THEN *self.subtitle = subtitle
    IF N_Elements(symsize) NE 0 THEN *self.symsize = symsize
    IF N_Elements(t3d) NE 0 THEN *self.t3d = Keyword_Set(t3d)
    IF N_Elements(thick) NE 0 THEN *self.thick = thick
    IF N_Elements(ticklen) NE 0 THEN *self.ticklen = ticklen
    IF N_Elements(title) NE 0 THEN *self.title = title
    
    IF N_Elements(data) GT 0 THEN BEGIN
        self.data = Keyword_Set(data)
        IF self.data EQ 1B THEN BEGIN
            self.device = 0
            self.normal = 0
        ENDIF
    ENDIF
    
    IF N_Elements(device) GT 0 THEN BEGIN
        self.device = Keyword_Set(device)
        IF self.device EQ 1B THEN BEGIN
            self.data   = B
            self.normal = 0B
        ENDIF
    ENDIF
    
    IF N_Elements(normal) GT 0 THEN BEGIN
        self.normal = Keyword_Set(normal)
        IF self.normal EQ 1B THEN BEGIN
            self.data   = 0B
            self.device = 0B
        ENDIF
    ENDIF
    
;---------------------------------------------------------------------
;X ///////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    IF N_Elements(xcharsize)     NE 0 THEN *self.xcharsize     = xcharsize
    IF N_Elements(xgridstyle)    NE 0 THEN *self.xgridstyle    = xgridstyle
;    IF N_Elements(xmargin)      NE 0 THEN *self.xmargin       = xmargin
    IF N_Elements(xminor)        NE 0 THEN *self.xminor        = xminor
    IF N_Elements(xrange)        NE 0 THEN *self.xrange        = xrange
    IF N_Elements(xstyle)        NE 0 THEN *self.xstyle        = xstyle
    IF N_Elements(xthick)        NE 0 THEN *self.xthick        = xthick
    IF N_Elements(xtickformat)   NE 0 THEN *self.xtickformat   = xtickformat
    IF N_Elements(xtickinterval) NE 0 THEN *self.xtickinterval = xtickinterval
    IF N_Elements(xticklayout)   NE 0 THEN *self.xticklayout   = xticklayout
    IF N_Elements(xticklen)      NE 0 THEN *self.xticklen      = xticklen
    IF N_Elements(xtickname)     NE 0 THEN *self.xtickname     = xtickname 
    IF N_Elements(xticks)        NE 0 THEN *self.xticks        = xticks
    IF N_Elements(xtickunits)    NE 0 THEN *self.xtickunits    = xtickunits
    IF N_Elements(xtickv)        NE 0 THEN *self.xtickv        = xtickv 
    IF N_Elements(xtitle)        NE 0 THEN *self.xtitle        = xtitle
    
;---------------------------------------------------------------------
;Y ///////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    IF N_Elements(ycharsize)     NE 0 THEN *self.ycharsize     = ycharsize
    IF N_Elements(ygridstyle)    NE 0 THEN *self.ygridstyle    = ygridstyle
;    IF N_Elements(ymargin)      NE 0 THEN *self.ymargin       = ymargin
    IF N_Elements(yminor)        NE 0 THEN *self.yminor        = yminor
    IF N_Elements(yrange)        NE 0 THEN *self.yrange        = yrange
    IF N_Elements(ystyle)        NE 0 THEN *self.ystyle        = ystyle
    IF N_Elements(ythick)        NE 0 THEN *self.ythick        = ythick
    IF N_Elements(ytickformat)   NE 0 THEN *self.ytickformat   = ytickformat
    IF N_Elements(ytickinterval) NE 0 THEN *self.ytickinterval = ytickinterval
    IF N_Elements(yticklayout)   NE 0 THEN *self.yticklayout   = yticklayout
    IF N_Elements(yticklen)      NE 0 THEN *self.yticklen      = yticklen
    IF N_Elements(ytickname)     NE 0 THEN *self.ytickname     = ytickname 
    IF N_Elements(yticks)        NE 0 THEN *self.yticks        = yticks
    IF N_Elements(ytickunits)    NE 0 THEN *self.ytickunits    = ytickunits
    IF N_Elements(ytickv)        NE 0 THEN *self.ytickv        = ytickv 
    IF N_Elements(ytitle)        NE 0 THEN *self.ytitle        = ytitle
    
;---------------------------------------------------------------------
;Z ///////////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    IF N_Elements(zcharsize)     NE 0 THEN *self.zcharsize     = zcharsize
    IF N_Elements(zgridstyle)    NE 0 THEN *self.zgridstyle    = zgridstyle
    IF N_Elements(zmargin)      NE 0 THEN *self.zmargin       = zmargin
    IF N_Elements(zminor)        NE 0 THEN *self.zminor        = zminor
    IF N_Elements(zrange)        NE 0 THEN *self.zrange        = zrange
    IF N_Elements(zstyle)        NE 0 THEN *self.zstyle        = zstyle
    IF N_Elements(zthick)        NE 0 THEN *self.zthick        = zthick
    IF N_Elements(ztickformat)   NE 0 THEN *self.ztickformat   = ztickformat
    IF N_Elements(ztickinterval) NE 0 THEN *self.ztickinterval = ztickinterval
    IF N_Elements(zticklayout)   NE 0 THEN *self.zticklayout   = zticklayout
    IF N_Elements(zticklen)      NE 0 THEN *self.zticklen      = zticklen
    IF N_Elements(ztickname)     NE 0 THEN *self.ztickname     = ztickname 
    IF N_Elements(zticks)        NE 0 THEN *self.zticks        = zticks
    IF N_Elements(ztickunits)    NE 0 THEN *self.ztickunits    = ztickunits
    IF N_Elements(ztickv)        NE 0 THEN *self.ztickv        = ztickv 
    IF N_Elements(ztitle)        NE 0 THEN *self.ztitle        = ztitle
END

;+
; This is the object class definition for the MrGraphicsKeywords object class.
; Normally, this class serves as the superclass for Coyote Graphics graphics 
; objects that need graphics keyword support.
;-
PRO MrGraphicsKeywords__Define, class

   class = {MrGraphicsKeywords, $
              AXISCOLOR:     Ptr_New(), $
              BACKGROUND:    Ptr_New(), $
;              CHARSIZE:     Ptr_New(), $
              CHARTHICK:     Ptr_New(), $
              CLIP:          Ptr_New(), $
              COLOR:         Ptr_New(), $
              DATA:          0B, $
              DEVICE:        0B, $
              NORMAL:        0B, $
              FONT:          Ptr_New(), $
              LINESTYLE:     Ptr_New(), $
              NOCLIP:        Ptr_New(), $
              NODATA:        Ptr_New(), $
              NOERASE:       Ptr_New(), $
;              POSITION:     Ptr_New(), $
              PSYM:          Ptr_New(), $
              SUBTITLE:      Ptr_New(), $
              SYMSIZE:       Ptr_New(), $
              T3D:           Ptr_New(), $
              THICK:         Ptr_New(), $
              TICKLEN:       Ptr_New(), $
              TITLE:         Ptr_New(), $
              XCHARSIZE:     Ptr_New(), $
              XGRIDSTYLE:    Ptr_New(), $
;              XMARGIN:      Ptr_New(), $
              XMINOR:        Ptr_New(), $
              XRANGE:        Ptr_New(), $
              XSTYLE:        Ptr_New(), $
              XTHICK:        Ptr_New(), $
              XTICK_GET:     Ptr_New(), $
              XTICKFORMAT:   Ptr_New(), $
              XTICKINTERVAL: Ptr_New(), $
              XTICKLAYOUT:   Ptr_New(), $
              XTICKLEN:      Ptr_New(), $
              XTICKNAME:     Ptr_New(), $
              XTICKS:        Ptr_New(), $
              XTICKUNITS:    Ptr_New(), $
              XTICKV:        Ptr_New(), $
              XTITLE:        Ptr_New(), $

              YCHARSIZE:     Ptr_New(), $
              YGRIDSTYLE:    Ptr_New(), $
;              YMARGIN:      Ptr_New(), $
              YMINOR:        Ptr_New(), $
              YRANGE:        Ptr_New(), $
              YSTYLE:        Ptr_New(), $
              YTHICK:        Ptr_New(), $
              YTICK_GET:     Ptr_New(), $
              YTICKFORMAT:   Ptr_New(), $
              YTICKINTERVAL: Ptr_New(), $
              YTICKLAYOUT:   Ptr_New(), $
              YTICKLEN:      Ptr_New(), $
              YTICKNAME:     Ptr_New(), $
              YTICKS:        Ptr_New(), $
              YTICKUNITS:    Ptr_New(), $
              YTICKV:        Ptr_New(), $
              YTITLE:        Ptr_New(), $
              
              ZCHARSIZE:     Ptr_New(), $
              ZGRIDSTYLE:    Ptr_New(), $
              ZMARGIN:       Ptr_New(), $
              ZMINOR:        Ptr_New(), $
              ZRANGE:        Ptr_New(), $
              ZSTYLE:        Ptr_New(), $
              ZTHICK:        Ptr_New(), $
              ZTICK_GET:     Ptr_New(), $
              ZTICKFORMAT:   Ptr_New(), $
              ZTICKINTERVAL: Ptr_New(), $
              ZTICKLAYOUT:   Ptr_New(), $
              ZTICKLEN:      Ptr_New(), $
              ZTICKNAME:     Ptr_New(), $
              ZTICKS:        Ptr_New(), $
              ZTICKUNITS:    Ptr_New(), $
              ZTICKV:        Ptr_New(), $
              ZTITLE:        Ptr_New(), $
              
              ZVALUE:        Ptr_New() $
              }
END