; docformat = 'rst'
;
; NAME:
;       MrLegend__Define
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
;   Create a legend.
;
; :Uses:
;   Uses the following external programs::
;       cgDemoData.pro (Coyote Graphics)
;       cgErrorMsg.pro (Coyote Graphics)
;       setDefaultValue.pro (Coyote Graphics)
;       cgLegendItem.pro
;       MrGrDataAtom__define.pro
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
;	Modification History::
;       2014/06/10  -   Written by Matthew Argall
;       2014/11/24  -   Shift the legend down by half a character size when a box is not
;                           being drawn so that LOCATION indicates the top of the first
;                           legend item, not its middle. - MRA
;-
;*****************************************************************************************
;+
;   Calculate the location of the legend.
;-
function MrLegend::CalculateLocation, $
ALIGNMENT=alignment
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        IF N_Elements(thisFont) NE 0 THEN !P.Font = thisFont
        RETURN, -1
    ENDIF
    
    ;Get the position of the target
    ;   - For direct graphics, use the last plotted location
    IF N_Elements(*self.target) GT 0 && Obj_Valid((*self.target)[0]) $
        THEN (*self.target)[0] -> GetProperty, POSITION=position $
        ELSE position = [!x.window[0], !y.window[0], !x.window[1], !y.window[1]]

    ;Where to position?
    IF self._location LE 4 $
        THEN location = position[[0,3]] $
        ELSE location = position[[2,3]]
    
    ;Set the alignement
    CASE self._location OF
        1: alignment = 2
        2: alignment = 3
        3: alignment = 1
        4: alignment = 0
        5: alignment = 2
        6: alignment = 3
        7: alignment = 1
        8: alignment = 0
        ELSE: message, strtrim(self._location, 2) + ' is not a valid location.'    
    ENDCASE
    
    RETURN, location
END


;+
;   This method draws the legend item in a graphics window.
;-
PRO MrLegend::Draw, $
NOERASE=noerase

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        IF N_Elements(thisFont) NE 0 THEN !P.Font = thisFont
        RETURN
    ENDIF
    
    ; Hide?.
    IF self.hide EQ 1 THEN RETURN

    ;Generate a location?
    self.cgLegend -> GetProperty, DATA=data, LOCATION=location, BOX=box
    IF self._location NE 0B $
        THEN new_location = self -> CalculateLocation(ALIGNMENT=alignment) $
        ELSE new_location = location
    
    ;Convert to normal coordinates?
    ;   - cgLegendItem will use the ConvertCoord() function to convert from
    ;       data to normal coodinates. Therefore, the target must be the current graphic.
    IF data && N_Elements(*self.target) GT 0 && Obj_Valid((*self.target)[0]) $
        THEN (*self.target)[0] -> SetCurrent
    
    ;If a box is not being drawn, shift the legend down have a character size
    ;   - LOCATION specifies the end point of the legend line
    ;   - I want it to specify the location of the top of the first text item
    if ~box then new_location -= [0.0, 0.55 * float(!d.y_ch_size) / !d.y_size]

    ;Size for postscript output
    IF !d.name EQ 'PS' THEN BEGIN
        self.cgLegend -> GetProperty, CHARSIZE=charsize, CHARTHICK=charchick, THICK=thick, VSPACE=vspace
        self.cgLegend -> SetProperty, CHARSIZE  = MrPS_Rescale(charsize,  /CHARSIZE), $
                                      CHARTHICK = MrPS_Rescale(charchick, /CHARTHICK), $
                                      THICK     = MrPS_Rescale(thick,     /THICK), $
                                      VSPACE    = 1.0
    ENDIF

    ;Set the location
    self.cgLegend -> SetProperty, DATA=data, LOCATION=new_location, ALIGNMENT=alignment

    ;Draw
    self.cgLegend -> Draw
    
    ;Reset the sizes and thicknesses
    IF !d.name EQ 'PS' $
        THEN self.cgLegend -> SetProperty, CHARSIZE=charsize, CHARTHICK=charchick, THICK=thick, VSPACE=vspace
END


;+
;   Get the position of the legend in normal coordinates: [x0, y0, x1, y1]. (x0, y0) are
;   the coordinates of the lower left-corner of the legend, while (x1, y1) are those of
;   the upper-right corner.
;-
FUNCTION MrLegend::GetPosition
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
PRO MrLegend::GetProperty, $
 HIDE=hide, $
 NAME=name, $
 POSITION=position, $
 TARGET=target, $
 WINDOW=window, $
_REF_EXTRA=extra
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN
    ENDIF

    ;Keywords for MrGrAtom
    IF Arg_Present(name)   THEN name   = self.name
    IF Arg_Present(hide)   THEN hide   = self.hide
    IF Arg_Present(window) THEN window = self.window
    
    ;Target
    IF Arg_Present(target) THEN IF Max(Obj_Valid(*self.target)) $
        THEN target = *self.target $
        ELSE target = obj_new()

    ;Superclass
    IF N_Elements(extra) GT 0 THEN self.cgLegend -> GetProperty, _STRICT_EXTRA=extra
    
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
FUNCTION MrLegend::IsInside, x, y, position, $
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
PRO MrLegend::SetProperty, $
 ALIGNMENT=alignment, $
 DATA=data, $
 TARGET=target, $
 HIDE=hide, $
 LOCATION=location, $
 NAME=name, $
 VISIBLE=visible, $
_REF_EXTRA=extra
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN
    ENDIF

    ;Let VISIBLE substitude for HIDE
    IF N_Elements(name)    GT 0 THEN self.name = name
    IF N_Elements(visible) GT 0 THEN self.hide = ~Keyword_Set(visible)
    IF N_Elements(hide)    GT 0 THEN self.hide = Keyword_Set(hide)
    
    ;Set the location?
    CASE N_Elements(location) OF
        0: ;Do nothing
        1: BEGIN
            ;If location is a scalar, it must be between 1 and 8
            self._location = (1 > location) < 8
        
            ;Calculate the location.
            ;   - It is returned in normal coordinates.
            new_location = self -> CalculateLocation(ALIGNMENT=alignment)
            data = 0
        ENDCASE
        
        2: BEGIN
            ;The location is NOT relative
            self._location = 0
            new_location = location
        ENDCASE
        
        ELSE: ;Do nothing
    ENDCASE
        
    ;Set legend properties
    self.cgLegend -> SetProperty, ALIGNMENT    = alignment, $
                                  DATA         = data, $
                                  LOCATION     = new_location, $
                                  VISIBLE      = ~self.hide, $
                                 _STRICT_EXTRA = extra
    
    ;Set the target
    ;   - Make sure that all targets are valid.
    IF N_Elements(target) GT 0 THEN $
        IF Min(Obj_Valid(target) EQ 1) THEN *self.target = target
    
    ;MrGrAtom
    
    self.window -> Draw
END


;+
; This method destroys anything the object uses that retains memory space.
;-
PRO MrLegend::CLEANUP
   obj_destroy, self.cgLegend
   self -> MrGrAtom::Cleanup
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
FUNCTION MrLegend::INIT, $
 HIDE=hide, $
 TARGET=target, $

 ;cgLegendItem Keywords
 ADDCMD=addcmd, $
 ALIGNMENT=alignment, $
 BACKGROUND=background, $
 BG_COLOR=bg_color, $
 BOX=box, $
 BX_COLOR=bx_color, $
 BX_THICK=bx_thick, $
 CENTER_SYM=center_sym, $
 CHARSIZE=charsize, $
 COLORS=colors, $
 DATA=data, $
 DRAW=draw, $
 HARDWARE=hardware, $
 LENGTH=length, $
 LINESTYLES=linestyles, $
 LOCATION=location, $
 PSYMS=psyms, $
 SYMCOLORS=symcolors, $
 SYMSIZE=symsize, $
 SYMTHICK=symthick, $
 TCOLORS=tcolors, $
 THICK=thick, $
 TITLES=titles, $
 TT_FONT=tt_font, $
 VISIBLE=visible, $
 VSPACE=vspace, $
 WINDOW=window, $
_REF_EXTRA=extra
    Compile_Opt strictarr
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN, 0
    ENDIF
    
    ;Let VISIBLE be a substitude for HIDE
    if n_elements(visible) gt 0 $
        then hide = ~keyword_set(visible) $
        else hide = keyword_set(hide)
    visible = ~hide

;-----------------------------------------------------
;Target & Title \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    nTargets = n_elements(target)
    if nTargets eq 0 then begin
        target = self -> _GetTarget(/ALL, /ANY, COUNT=nTargets)
        if nTargets eq 0 then message, 'Insert MrLegend failed. No targets available.'
    endif

    ;Get the title from the targets?
    if n_elements(titles) eq 0 then begin
        titles = strarr(nTargets)
        for i = 0, nTargets-1 do titles[i] = target -> GetName()
    endif
    nLegends = n_elements(titles)

;-----------------------------------------------------
;Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Default colors
    default_colors = ['opposite', 'Blue', 'Forest_Green', 'Red', 'Magenta', 'Orange']
    if nLegends gt 5 then default_colors = [default_colors, replicate('opposite', nLegends-5)]
    if nLegends eq 1 $
        then d_colors = default_colors[0] $
        else d_color = default_colors[1:nLegends]
    
    ;Set Defaults
    SetDefaultValue, colors, d_colors
    SetDefaultValue, charsize, 1.5

;---------------------------------------------------------------------
;Window //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Superclass
    ;   - Takes the window from the target, but does not set the target.
    if self -> MrGrAtom::INIT(HIDE=hide, TARGET=target, _STRICT_EXTRA=extra) eq 0 then $
        message, 'Unable to initialize MrGrAtom'
    
    ;Refresh the window?
    self.window -> GetProperty, REFRESH=refreshIn
    if refreshIn then self.window -> Refresh, /DISABLE
    
;---------------------------------------------------------------------
; Create the Legend //////////////////////////////////////////////////
;---------------------------------------------------------------------
    self.target = ptr_new(/ALLOCATE_HEAP)

    theLegend = obj_new('cgLegendItem', ALIGNMENT=alignment, $ $
                                        BACKGROUND=background, $
                                        BG_COLOR=bg_color, $
                                        BOX=box, $
                                        BX_COLOR=bx_color, $
                                        BX_THICK=bx_thick, $
                                        CENTER_SYM=center_sym, $
                                        CHARSIZE=charsize, $
                                        COLORS=colors, $
                                        DATA=data, $
                                        HARDWARE=hardware, $
                                        LENGTH=length, $
                                        LINESTYLES=linestyles, $
                                        PSYMS=psyms, $
                                        SYMCOLORS=symcolors, $
                                        SYMSIZE=symsize, $
                                        SYMTHICK=symthick, $
                                        TCOLORS=tcolors, $
                                        THICK=thick, $
                                        TITLES=titles, $
                                        TT_FONT=tt_font, $
                                        VISIBLE=visible, $
                                        VSPACE=vspace)
    if obj_valid(theLegend) eq 0 then return, 0
    self.cgLegend = theLegend

    ;Set object properties
    self -> SetProperty, TARGET=target, LOCATION=location

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
PRO MrLegend__Define, class
    Compile_Opt strictarr

    class = { MrLegend, $
              INHERITS MrGrAtom, $
              cgLegend:  obj_new(), $
              target:    ptr_new(), $
              _location: 0B $
            }
END