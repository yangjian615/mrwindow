; docformat = 'rst'
;
; NAME:
;   weLegendItem__Define
;
; PURPOSE:
;   This is a "wrapper" for the cgLegendItem__Define class. Additions::
;       o Determine the location of the legend automatically
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
;   The purpose of this program is to create a simple legend object that can be drawn on
;   a data plot. Perhaps later such objects can be collected into a more sophisticated
;   "legend" object.
;
;   Note that GRAPHIC is meant to exist independently of the "weLegendItem" object. As
;   such, the GRAPHIC object is not destroyed when the particular weLegendItem is destroyed.
;        
; :Categories:
;    Graphics
;    
; :Examples:
;    A plot with a simple legend::
;       cgDisplay, 800, 450
;       legendItem1 = Obj_New('weLegendItem', SymColor='red7', PSym=6, Symsize=1.5, $
;           Location=[0.825, 0.875], Title='May 27', /Hardware, Length=0.05)
;       legendItem2 = Obj_New('weLegendItem', SymColor='blu7', PSym=15, Symsize=1.5, $
;           Location=[0.825, 0.835], Title='June 27', /Hardware, Length=0.05)
;       cgPlot, cgDemoData(17), PSym=-6, SymColor='red7', Position=[0.15, 0.15, 0.8, 0.9], $
;          Legends=[legendItem1,legendItem2], Label='First Experiment'
;       cgOPlot, cgDemoData(17), PSym=-15, SymColor='blu7'
;       Obj_Destroy, [legendItem1,legendItem2]
;
;    Same example, but with a single legend object and with a centered symbol::
;       cgDisplay, 800, 450
;       legendItem = Obj_New('weLegendItem', SymColor=['red7', 'blu7'], $
;           PSym=[6,15], Symsize=1.5, Location=[0.825, 0.875], Title=['May 27', 'Jun 27'], $
;           /CENTER_SYM, /Hardware, Length=0.05)
;       cgPlot, cgDemoData(17), PSym=-6, SymColor='red7', Position=[0.15, 0.15, 0.8, 0.9], $
;          Legends=legendItem, Label='First Experiment'
;       cgOPlot, cgDemoData(17), PSym=-15, SymColor='blu7'
;       Obj_Destroy, legendItem
;
;   Same example, but with a "general", "sticky" legend position::
;       cgDisplay, 800, 450
;       legendItem = Obj_New('weLegendItem', SymColor=['red7', 'blu7'], $
;           PSym=[6,15], Symsize=1.5, Title=['May 27', 'Jun 27'], GenLocation=7, $
;           /CENTER_SYM, /Hardware, Length=0.05)
;       cgPlot, cgDemoData(17), PSym=-6, SymColor='red7', Position=[0.15, 0.15, 0.8, 0.9], $
;          Legends=legendItem, Label='First Experiment'
;       cgOPlot, cgDemoData(17), PSym=-15, SymColor='blu7'
;       Obj_Destroy, legendItem
;           
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;     
; :History:
;     Change History::
;        Written 18 July 2012. DWF.
;        Adapted to accept multiple legend elements. Legend elements are stacked vertically. 
;           Each legend element can be given its own title, color, symbol symbol color, and 
;           linestyle. Legend elements are offset  by 1.3*!D.Y_CH_Size/!D.YSize.  A single 
;           symbol can now be drawn in the center of the line instead of one at each end 
;           point with the CENTER_SYM keyword. 04/25/2013, Matthew Argall.
;        Added the capability to specify a general location. Said location is "sticky"
;           in that it will move as the plot is resized. 10 May 2013. Matthew Argall
;        Added the DRAW keyword. Fixed bug in default settings for LOCATION and
;           GENLOCATION. 12 May 2013. Matthew Argall.
;        Added the GRAPHIC keyword. Changed VISIBLE to HIDE. Combined the functionality
;           of GENLOCATION and LOCATION. Re-designed the INIT method. 08 Sept 2013. Matthew Argall
;        Added the POSITION keyword to the GetProperty method. Created the LegendSize
;           IsInside, and GetPosition methods. 09 Sept 2013. Matthew Argall
;        Inherit MrGrAtom. Rename GRAPHIC to TARGET. 20 November 2013. Matthew Argall
;        Added the DATA property. 05 March 2013. Matthew Argall
;       2014/03/10  -   Removed the CURRENT keyword. Model function graphics behavior
;                           by using the current graphics selection as target if no
;                           targets are given. - MRA
;-
;****************************************************************************************
;+
;   Calculate the position of the legend based on a general location.
;
;   A means of providing a general, "sticky" location for the legend. The legend
;   will stay in the same relative position to the graph even if the graph moves.
;   Legend options postage stamp the top left and right corners::
;           1         2                  5         6
;                ------------------------------
;           3    |    4                  7    |    8
;                |                            |
;
;   NOTE::
;       This method assumes that the !D, !X, and !Y system variables are set to
;       the plot by which the colorbar will be placed.
;-
function weLegendItem::calcLegendLocation
    compile_opt idl2
    
    ;Catch errors
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN, -1
    ENDIF
    
    dimension = self -> LegendSize(YOFFSET=yoffset, XCHSIZE=xchsize, YCHSIZE=ychsize)
    length = dimension[0]
    height  = dimension[1]

    ;Get the reference position    
    IF Obj_Valid(self.target) EQ 0 $
        THEN ref_position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]] $
        ELSE self.target -> GetProperty, POSITION=ref_position

    ;Calculate the location
    CASE *self.location OF
        1: location = [ref_position[0]-length-5*xchsize,  ref_position[3]+height+ychsize] ;space between line and title
        2: location = [ref_position[0]+xchsize,           ref_position[3]+height+ychsize]
        3: location = [ref_position[0]-length-10*xchsize, ref_position[3]]        ;make room for x-axis labels
        4: location = [ref_position[0]+xchsize,           ref_position[3]-ychsize]
        5: location = [ref_position[2]-length-5*xchsize,  ref_position[3]+height];+ychsize]
        6: location = [ref_position[2]+xchsize,           ref_position[3]+height+ychsize]
        7: location = [ref_position[2]-length-5*xchsize,  ref_position[3]-ychsize]        ;space between line and title
        8: location = [ref_position[2]+xchsize,           ref_position[3]]
        ELSE: Message, 'GENLOCATION is not a valid location.'
    ENDCASE

    RETURN, location
END


;+
;   Calculate the dimensions of the legend.
;
; :Keywords:
;       XCHSIZE:        out, optional, type=float
;                       Average width of a character, in normal coordinates.
;       YCHSIZE:        out, optional, type=float
;                       Average height of a character, in normal coordinates.
;       YOFFSET:        out, optional, type=float
;                       The offset between legend items, in normal coordinates.
;
; :Returns:
;       DIMENSIONS:     The [length, width] of the legend.
;-
FUNCTION weLegendItem::LegendSize, $
XCHSIZE = xchsize, $
YCHSIZE = ychsize, $
YOFFSET = yoffset
    Compile_Opt idl2
    
    ;Catch errors
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN, [0,0]
    ENDIF
    
    ;Get the height and length of the legend. Offset from the corner by a character unit
    xchsize = Double(!D.X_CH_Size)/Double(!D.X_Size) * self.charsize
    ychsize = Double(!D.Y_CH_Size)/Double(!D.Y_Size) * self.charsize

    ;General size of the aggregate legend text
    nlegends = N_Elements(*self.title)
    length = self.length + Max(StrLen(*self.title))*xchsize
    width = (nlegends*ychsize)
    
    yoffset = 1.3*ychsize  ;same as in ::Draw
    
    RETURN, [length, width]
END


;+
; This method draws the legend item in a graphics window.
;-
PRO weLegendItem::Draw, $
NOERASE=noerase

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        IF N_Elements(thisFont) NE 0 THEN !P.Font = thisFont
        RETURN
    ENDIF
    
    ; If this is not visible, return now.
    IF self.hide EQ 1 THEN RETURN

    ;Generate a location?
    IF Obj_Valid(self.target) THEN BEGIN
        CASE N_Elements(*self.location) OF
            1: location = self -> calcLegendLocation()
            2: if self.data eq 1 $
                then location = self.target -> ConvertCoord(*self.location, /DATA, /TO_NORMAL) $
                else location = *self.location
            ELSE: ;Do nothing
        ENDCASE
    ENDIF ELSE BEGIN
        location = *self.location
    ENDELSE

    ;Return if the location could not be calculated.
    IF N_Elements(location) EQ 1 && LOCATION EQ -1 THEN RETURN

    x0 = location[0]
    x1 = x0 + self.length
    y = location[1]
    void = self -> LegendSize(XCHSIZE=xchsize, YCHSIZE=ychsize, YOFFSET=y_offset)

    ;For each legend element
    FOR j = 0, N_Elements(*self.title) - 1 DO BEGIN
        ; Need to draw a line?
        IF x0 NE x1 THEN BEGIN
           cgPlotS, [x0,x1], [y,y]-(j*y_offset), COLOR=(*self.color)[j], $
                    LINESTYLE=(*self.lineStyle)[j], THICK=self.thick, /NORMAL
        ENDIF
    
        ; Need to draw symbols?
        IF (*self.psym)[j] NE 0 THEN BEGIN
            ; Center the symbol on the line?
            IF self.center_sym THEN BEGIN
                x2 = x0 + (x1 - x0) / 2.0
               cgPlotS, [x2,x2], [y,y]-(j*y_offset), PSYM=(*self.psym)[j], $
                        SYMSIZE=self.symsize, SYMCOLOR=(*self.symColor)[j], /NORMAL
            ENDIF ELSE BEGIN
               cgPlotS, [x0,x1], [y,y]-(j*y_offset), PSYM=(*self.psym)[j], $
                        SYMSIZE=self.symsize, SYMCOLOR=(*self.symColor)[j], /NORMAL
            ENDELSE
        ENDIF
    
        ; Draw the title.
        IF self.hardware THEN BEGIN
            thisFont = !P.Font
            !P.Font = (!D.Name EQ 'PS') ? 1 : 0
        ENDIF
        cgText, x1+(2.0*xchsize), y-(0.5*ychsize)-(j*y_offset),$
            /NORMAL, ALIGNMENT=0.0, (*self.title)[j], COLOR=(*self.tcolor)[j], $
            TT_FONT=*self.tt_font, CHARSIZE=self.charsize, FONT=!P.Font, $
            CHARTHICK=self.charthick
        IF self.hardware THEN !P.Font = thisFont
        
    ENDFOR
    
END


;+
;   Get the position of the legend in normal coordinates: [x0, y0, x1, y1]. (x0, y0) are
;   the coordinates of the lower left-corner of the legend, while (x1, y1) are those of
;   the upper-right corner.
;-
FUNCTION weLegendItem::GetPosition
    Compile_Opt idl2
    
    ;Catch errors
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN, [0,0]
    ENDIF
    
    ;Calculate the length and width of the legend
    dimensions = self -> LegendSize()

    ;Determine the position based on the location of the upper-left corner.
    position = fltarr(4)
    
    ;Do we need to calculate the location?
    if n_elements(*self.location) eq 1 $
        then location = self -> calcLegendLocation() $
        else location = *self.location
        
    position[[0,3]] = location
    position[1] = location[0] - dimensions[0]
    position[2] = location[1] + dimensions[1]
        
    RETURN, POSITION
END    


;+
; This method obtains properties from the object.
;
; :Keywords:
;     center_sym: out, optional, type=boolean
;        This keyword is set if symbols are placed in the center of the line.
;     charsize: out, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     color: out, optional, type=string/strarr
;        The name of the data color. This is the color of each data line.
;     hardware: out, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: out, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyle: out, optional, type=integer/intarr
;        The line style for drawing each line.
;     location: out, optional, type=int/fltarr
;        The location of the upper-left corner of the legend item, or an integer
;        specifying the location with respect to the upper-left/right corner of the
;        previously drawn graphic.
;     psym: out, optional, type=integer/intarr
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: out, optional, type=string/strarr
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: out, optional, type=float, default=1.0
;        The symbol size.
;     symthick: out, optional, type=float, default=1.0
;        The thickness of the symbol.
;     target: out, optional, type=object, default=0
;        The graphic by which the legend was placed.
;     tcolor: out, optional, type=string/strarr
;        The `Title` color. Set by default to `Color`.
;     thick: out, optional, type=float, default=1.0
;        The thickness of the line.
;     title: out, optional, type=string/strarr, default='Plot Item'
;        The "title" or text for each legend item.
;     tt_font: out, optional, type=string
;        The name of a true-type font to use for the legend text.
;-
PRO weLegendItem::GetProperty, $
   CENTER_SYM=center_sym, $
   CHARSIZE=charsize, $
   CHARTHICK=charthick, $
   COLOR=color, $
   DATA=data, $
   HARDWARE=hardware, $
   GENLOCATION=genlocation, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   LOCATION=location, $
   POSITION=position, $
   PSYM=psym, $
   SYMCOLOR=symcolor, $
   SYMSIZE=symsize, $
   SYMTHICK=symthick, $
   TARGET=target, $
   TCOLOR=tcolor, $
   THICK=thick, $
   TITLE=title, $
   TT_FONT=tt_font, $
  _REF_EXTRA=extra

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN
    ENDIF

    IF Arg_Present(center_sym) THEN center_sym =  self.center_sym
    IF Arg_Present(charsize)   THEN charsize   =  self.charsize
    IF Arg_Present(charthick)  THEN charthick  =  self.charthick
    IF Arg_Present(color)      THEN color      = *self.color
    IF Arg_Present(data)       THEN data       =  self.data
    IF Arg_Present(device)     THEN device     =  self.device
    IF Arg_Present(hardware)   THEN hardware   =  self.hardware
    IF Arg_Present(length)     THEN length     =  self.length
    IF Arg_Present(linestyle)  THEN linestyle  = *self.linestyle
    IF Arg_Present(location)   THEN location   = *self.location
    IF Arg_Present(normal)     THEN normal     =  self.normal
    IF Arg_Present(psym)       THEN psym       = *self.psym
    IF Arg_Present(symcolor)   THEN symcolor   = *self.symcolor
    IF Arg_Present(symsize)    THEN symsize    =  self.symsize
    IF Arg_Present(symthick)   THEN symthick   =  self.symthick
    IF Arg_Present(thick)      THEN thick      =  self.thick
    IF Arg_Present(tcolor)     THEN tcolor     = *self.tcolor
    IF Arg_Present(title)      THEN title      = *self.title
    IF Arg_Present(tt_font)    THEN tt_font    = *self.tt_font

    IF Arg_Present(target) THEN IF Obj_Valid(self.target) $
        THEN target = *self.target $
        ELSE target = obj_new()
    
    IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::GetProperty, _STRICT_EXTRA=extra
    
    ;Calculate the position of the legend [x0,y0,x1,y1]
    IF Arg_Present(position) THEN position = self -> GetPosition()
END


;+
;   Determine if the coordate made by X and Y lies within the object.
;
; :Params:
;       X:              in, required, type=numeric scalar
;                       X coordinate 
;       Y:              in, optional, type=numeric scalar/array
;                       Y coordinate
;       POSITION:       in, optional, type=fltarr(4), default=*self.position
;                       Determine if [`X`,`Y`] is inside this POSITION.
;
; :Keywords:
;       _REF_EXTRA:     in, out, optional, type=any
;                       Any keyword accepted by MrGraphicAtom::IsInside is also accepted
;                           for keyword inheritance.
;
; :Returns:
;       TF_INSIDE:      Returns true (1) if [x,y] lies within POSITION. False (0) otherwise.
;-
FUNCTION weLegendItem::IsInside, x, y, position, $
NORMAL = normal, $
_REF_EXTRA=extra
    Compile_Opt idl2
    
    ;Catch errors
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN, [0,0]
    ENDIF
    
    ;Get the position
    if n_elements(position) eq 0 then begin
        position = self -> GetPosition()
        normal = 1
    endif

    ;Call the superclass
    tf_inside = self -> MrGrAtom::IsInside(x, y, position, $
                                           NORMAL=normal, $
                                           _STRICT_EXTRA=extra)
    
    RETURN, tf_inside
END


;+
; This method sets properties of the object.
; 
; :Keywords:
;     center_sym: in, optional, type=boolean
;        Set this keyword to place a single symbol in the center of the line.
;     charsize: in, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     color: in, optional, type=string/strarr
;        The name of the data color. This is the color of each data line.
;     hardware: in, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: in, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyle: in, optional, type=integer/intarr
;        The line style for drawing each line.
;     location: in, optional, type=int/fltarr
;        The location of the upper-left corner of the legend item, or an integer
;        specifying the location with respect to the upper-left/right corner of the
;        previously drawn graphic.
;     psym: in, optional, type=integer/intarr
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: in, optional, type=string/strarr
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     symthick: in, optional, type=float, default=1.0
;        The thickness of the symbol.
;     target: in, optional, type=object, default=0
;        The graphic by which to place the legend.
;     tcolor: in, optional, type=string/strarr
;        The `Title` color. Set by default to `Color`.
;     thick: in, optional, type=float, default=1.0
;        The thickness of the line.
;     title: in, optional, type=string/strarr, default='Plot Item'
;        The "title" or text for each legend item.
;     tt_font: in, optional, type=string
;        The name of a true-type font to use for the legend text.
;-
PRO weLegendItem::SetProperty, $
   CENTER_SYM=center_sym, $
   CHARSIZE=charsize, $
   CHARTHICK=charthick, $
   COLOR=color, $
   DATA=data, $
   HARDWARE=hardware, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   LOCATION=location, $
   PSYM=psym, $
   SYMCOLOR=symcolor, $
   SYMSIZE=symsize, $
   SYMTHICK=symthick, $
   TARGET=target, $
   TCOLOR=tcolor, $
   THICK=thick, $
   TITLE=title, $
   TT_FONT=tt_font, $
  _REF_EXTRA=extra

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN
    ENDIF

    IF N_Elements(center_sym) NE 0 THEN  self.center_sym = center_sym
    IF N_Elements(charsize)   NE 0 THEN  self.charsize = charsize
    IF N_Elements(charthick)  NE 0 THEN  self.charthick = charthick
    IF N_Elements(color)      NE 0 THEN *self.color = color
    if n_elements(data)       GT 0 THEN  self.data = data
    IF N_Elements(hardware)   NE 0 THEN  self.hardware = hardware
    IF N_Elements(length)     NE 0 THEN  self.length = length
    IF N_Elements(linestyle)  NE 0 THEN *self.linestyle = linestyle
    IF N_Elements(location)   NE 0 THEN *self.location = location
    IF N_Elements(psym)       NE 0 THEN *self.psym = psym
    IF N_Elements(symcolor)   NE 0 THEN *self.symcolor = symcolor
    IF N_Elements(symsize)    NE 0 THEN  self.symsize = symsize
    IF N_Elements(symthick)   NE 0 THEN  self.symthick = symthick
    IF N_Elements(thick)      NE 0 THEN  self.thick = thick
    IF N_Elements(tcolor)     NE 0 THEN *self.tcolor = tcolor
    IF N_Elements(title)      NE 0 THEN *self.title = title
    IF N_Elements(tt_font)    NE 0 THEN *self.tt_font = tt_font    
    
    IF N_Elements(target) NE 0 THEN IF Obj_Valid(target) THEN self.target = target
    
    IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra
    
    self.window -> Draw
END


;+
; This method destroys anything the object uses that retains memory space.
;-
PRO weLegendItem::CLEANUP

   Ptr_Free, self.color
   Ptr_Free, self.location
   Ptr_Free, self.linestyle
   Ptr_Free, self.psym
   Ptr_Free, self.symcolor
   Ptr_Free, self.tcolor
   Ptr_Free, self.title
   Ptr_Free, self.tt_font
   
   self -> MrGrAtom::CleanUp

END


;+
; This method creates an instance of the object.
;
; :Keywords:
;     center_sym: in, optional, type=boolean
;        Set this keyword to place a single symbol in the center of the line. The default
;        is to draw a symbol at each endpoint of the line.
;     charsize: in, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     color: in, optional, type=string/strarr
;        The name of the data color. This is the color of each data line.
;     hardware: in, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: in, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyle: in, optional, type=integer/intarr
;        The line style for drawing each line.
;     location: in, optional, type=int/fltarr
;        The location of the upper-left corner of the legend item,
;        in normalized coordinates (0 to 1 in the graphics window).
;        The default is [0.1, 0.95]. If LOCATION is an integer, then it specifies a
;        general location with respect the corner of a graphic::
;           1       2                    5         6
;                ------------------------------
;           3    |  4                    7    |    8
;                |                            |
;     psym: in, optional, type=integer/intarr
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: in, optional, type=string/strarr
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     symthick: in, optional, type=float, default=1.0
;        The thickness of the symbol.
;       TARGET:         in, optional, type=object
;                       The graphic that the legend describes. If no target is given,
;                           all currently selected graphics will be used. If no graphics
;                           are selected, all 
;       TITLE:          in, optional, type=string/strarr, default='Plot Item'
;                       The "title" or text for each legend item. If no title is given,
;                           the names of each `TARGET` will be used.
;     tcolor: in, optional, type=string/strarr
;        The `Title` color. Set by default to `Color`.
;     thick: in, optional, type=float, default=1.0
;        The thickness of the line.
;     title: in, optional, type=string/strarr, default='Plot Item'
;        The "title" or text for each legend item.
;     tt_font: in, optional, type=string
;        The name of a true-type font to use for the legend text.
;-
FUNCTION weLegendItem::INIT, $
   CENTER_SYM=center_sym, $
   CHARSIZE=charsize, $
   CHARTHICK=charthick, $
   COLOR=color, $
   DATA=data, $
   HARDWARE=hardware, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   LOCATION=location, $
   PSYM=psym, $
   SYMCOLOR=symcolor, $
   SYMSIZE=symsize, $
   SYMTHICK=symthick, $
   TARGET=target, $
   TCOLOR=tcolor, $
   THICK=thick, $
   TITLE=title, $
   TT_FONT=tt_font, $
  _REF_EXTRA=extra
    Compile_Opt strictarr
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN, 0
    ENDIF

;-----------------------------------------------------
;Target & Title \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if n_elements(target) eq 0 then begin
        target = self -> _GetTarget(/ALL, /ANY, COUNT=nTargets)
        if nTargets eq 0 then message, 'Insert MrLegend failed. No targets available.'
    endif

    ;Get the title from the targets?
    if n_elements(title) eq 0 then begin
        title = strarr(nTargets)
        for i = 0, nTargets-1 do title[i] = target -> GetName()
    endif
    nLegends = n_elements(title)

;-----------------------------------------------------
;Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Default colors
    default_colors = ['opposite', 'Blue', 'Forest_Green', 'Red', 'Magenta', 'Orange']
    if nLegends gt 5 then default_colors = [default_colors, replicate('opposite', nLegends-5)]
    if nLegends eq 1 then d_color = default_colors[0] else d_color = default_colors[1:nLegends]
    
    ;Set Defaults
    SetDefaultValue, center_sym, /BOOLEAN
    SetDefaultValue, charsize, 1.5
    SetDefaultValue, color, d_color
    SetDefaultValue, hardware, 0
    SetDefaultValue, length, 0.075
    SetDefaultValue, linestyle, Replicate(0, nLegends)
    SetDefaultValue, location, 7
    SetDefaultValue, psym, Replicate(0, nLegends)
    SetDefaultValue, symcolor, color
    SetDefaultValue, symsize, 1.0
    SetDefaultValue, symthick, 1.0
    SetDefaultValue, thick, 1.0
    SetDefaultValue, tcolor, color
    
    ;Validate Pointers
    self.color     = Ptr_New(/ALLOCATE_HEAP)
    self.linestyle = Ptr_New(/ALLOCATE_HEAP)
    self.location  = Ptr_New(/ALLOCATE_HEAP)
    self.psym      = Ptr_New(/ALLOCATE_HEAP)
    self.symcolor  = Ptr_New(/ALLOCATE_HEAP)
    self.tcolor    = Ptr_New(/ALLOCATE_HEAP)
    self.title     = Ptr_New(/ALLOCATE_HEAP)
    self.tt_font   = Ptr_New(/ALLOCATE_HEAP)
    self.location  = Ptr_New(/ALLOCATE_HEAP)

;---------------------------------------------------------------------
;Window //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Superclass
    if self -> MrGrAtom::INIT(TARGET=target, _STRICT_EXTRA=extra) eq 0 then $
        message, 'Unable to initialize MrGrAtom'
    
    ;Refresh the window?
    self.window -> GetProperty, REFRESH=refreshIn
    if refreshIn then self.window -> Refresh, /DISABLE
    
;---------------------------------------------------------------------
;Set Properties //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    self -> SetProperty, CENTER_SYM = center_sym, $
                         CHARTHICK  = charthick, $
                         CHARSIZE   = charsize, $
                         COLOR      = color, $
                         DATA       = data, $
                         TARGET     = target, $
                         HARDWARE   = hardware, $
                         LENGTH     = length, $
                         LINESTYLE  = linestyle, $
                         LOCATION   = location, $
                         PSYM       = psym, $
                         SYMCOLOR   = symcolor, $
                         SYMSIZE    = symsize, $
                         SYMTHICK   = symthick, $
                         TCOLOR     = tcolor, $
                         THICK      = thick, $
                         TITLE      = title, $
                         TT_FONT    = tt_font
    
    ;Turn refresh back on?
    if refreshIn then self.window -> Refresh

    RETURN, 1
END


;+
; The class definition module for the object.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO weLegendItem__Define, class
    Compile_Opt strictarr

    class = { weLegendItem, $
              INHERITS MrGrAtom, $
              
              alignment: 0.0, $             ;Text alignement
              charsize: 0.0, $              ;Character size of legend elements.
              charthick: 0.0, $             ;Character thickness.
              center_sym: 0, $              ;Center the symbol on the line
              clip: 0B, $                   ;
              color: Ptr_New(), $           ;Color of each legend item
              data: 0B, $                   ;Data coordinates
              target: Obj_New(), $          ;Graphic around which to place the legend
              hardware: 0, $                ;Hardware fonts?
              length: 0.0, $                ;Length of the line for each legend element
              linestyle: Ptr_New(), $       ;Linestyle for each legend element
              location: Ptr_New(), $        ;Location of the legend
              noclip: 0B, $                 ;
              orientation: 0.0, $           ;
              psym: Ptr_New(), $            ;Symbol to use for each legend element
              symcolor: Ptr_New(), $        ;Color of the symbol to use for each legend element
              symsize: 0.0, $               ;Size of the symbol for each legend element
              symthick: 0.0, $              ;Thickness of the symbol for each legend element
              t3d: 0B, $                    ;
              tcolor: Ptr_New(), $          ;Color of the title
              text_axes: 0B, $              ;Plane of vector drawn text in 3D plotting
              thick: 0.0, $                 ;Thickness of the legend
              title: Ptr_New(), $           ;Title or text for each legend element
              tt_font: Ptr_New(), $         ;True-type font
              width: 0.0, $                 ;Width of the text string, in normal coordinates.
              z: 0.0 $
            }
END