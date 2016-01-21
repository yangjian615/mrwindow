; docformat = 'rst'
;
; NAME:
;   MrText__Define
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
;   The purpose of this program is to create a text object object that can drawn on a
;   data plot.
;
; :Categories:
;    Graphics
;    
; :Examples:
;   The following commands will be used for all examples::
;       x = findgen(256)/256
;       sinx = sin(2*!pi*x)
;       cgPlot, x, sinx, TITLE='A Simple Sine Wave', XTITLE='Time', YTITLE='Amplitude'
;
;  Speceify where the text goes, retrieve the text width::
;       myText = obj_new('MrText', 0.5, 0.5, 'Look at my Text!', Color='Blue', $
;                        /NORMAL, /DRAW, WIDTH=width)
;       obj_destroy, myText
;
;  Click to place the text::
;       myText = obj_new('MrText', 'Look at my Text!', Color='Blue', /DRAW, /PLACE)
;       obj_destroy, myText
;
;  Change and move the existing text via the SetProperty method::
;       myText = obj_new('MrText', 0.5, 0.5, 'Look at my Text!', Color='Blue', /NORMAL, /DRAW)
;       myText -> SetProperty, STRING='Put it Over Here!', /PLACE, OUTLOC=outloc, WIDTH=width
;       obj_destroy, myText
;
;  Change and move the existing text via the draw method, retrieve OUTLOC and WIDTH::
;       myText = obj_new('MrText', 0.5, 0.5, 'Look at my Text!', Color='Blue', /NORMAL, /DRAW)
;       myText -> Draw, 'Now Here!', /PLACE, OUTLOC=outloc, WIDTH=width
;       obj_destroy, myText
;
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
;       05/14/2013  -   Written by Matthew Argall
;       05/28/2013  -   Made separate conditions for the Draw and Place in the SetProperty
;                           method. - MRA
;       07/31/2013  -   Added the graphic property for use with DATA coordinates.
;       2013/11/20  -   Inherit MrGrAtom. Rename GRAPHIC to TARGET. - MRA
;       2014/02/03  -   Renamed from weText to MrText. Added the Place method. - MRA
;       2014/08/29  -   Added the RELATIVE property. - MRA
;-
;*****************************************************************************************
;+
;   Print information about the text object.
;-
FUNCTION MrText::_OverloadPrint
    
    ;Error handling
    Catch, the_error
    IF the_error NE 0 THEN BEGIN
        Catch, /cancel
        MrPrintF, 'LogErr'
        RETURN, "''"
    ENDIF
    
    undefined = '<undefined>'
    undefObj  = '<NullObject>'
    default   = '<IDL_Default>'
    joinStr   = '   '
    
    ;First, get the results from the superclasses
    atomKeys = self -> MrGrAtom::_OverloadPrint()

    ;Text Color
    case n_elements(*self.color) of
        0: color = default
        1: color = size(*self.color, /TNAME) eq 'STRING' ? *self.color : string(*self.color, FORMAT='(i0)')
        3: color = '[' + strjoin(string(color, FORMAT='(i3)'), ', ') + ']'
    endcase

    ;Fill Color
    case n_elements(*self.color) of
        0: fill_color = default
        1: fill_color = size(*self.fill_color, /TNAME) eq 'STRING' ? *self.fill_color : string(*self.fill_color, FORMAT='(i0)')
        3: fill_color = '[' + strjoin(string(fill_color, FORMAT='(i3)'), ', ') + ']'
    endcase

    ;Border Color
    case n_elements(*self.box_color) of
        0: box_color = default
        1: box_color = size(*self.box_color, /TNAME) eq 'STRING' ? *self.box_color : string(*self.box_color, FORMAT='(i0)')
        3: box_color = '[' + strjoin(string(box_color, FORMAT='(i3)'), ', ') + ']'
    endcase
    
    ;Box LineStyle
    IF Size(*self.box_linestyle, /TNAME) EQ 'STRING' $
        THEN box_linestyle = *self.box_linestyle $
        ELSE box_linestyle = String(*self.box_linestyle, FORMAT='(i1)')

    ;Class Properties
    alignment     = string('Alignment',          '=', self.alignment,     FORMAT='(a-26, a-2, f0)')
    baseline      = string('BaseLine',           '=', self.baseline,      FORMAT='(a-26, a-2, 3(f0.4, 3x))')
    box_linestyle = string('Box_LineStyle',      '=',      box_linestyle, FORMAT='(a-26, a-2, a0)')
    box_color     = string('Box_Color',          '=',      box_color,     FORMAT='(a-26, a-2, a0)')
    bx_pos        = string('BX_Pos',             '=', self.bx_pos,        FORMAT='(a-26, a-2, 4(f0.4, 3x))')
    charsize      = string('CharSize',           '=', self.charsize,      FORMAT='(a-26, a-2, f0)')
    charthick     = string('CharThick',          '=', self.charthick,     FORMAT='(a-26, a-2, f0)')
;    clip          = string('Clip',               '=', self.clip,          FORMAT='(a-26, a-2, i1)')
    color         = string('Color',              '=',      color,         FORMAT='(a-26, a-2, a0)')
    data          = string('Data',               '=', self.data,          FORMAT='(a-26, a-2, i1)')
    device        = string('Device',             '=', self.device,        FORMAT='(a-26, a-2, i1)')
    fill_back     = string('Fill_Background',    '=', self.fill_background, FORMAT='(a-26, a-2, i1)')
    fill_color    = string('Fill_Color',         '=',      fill_color,    FORMAT='(a-26, a-2, a0)')
    margins       = string('Margins',            '=', self.margins,       FORMAT='(a-26, a-2, 4(f0.4, 3x))')
    noclip        = string('NoClip',             '=', self.noclip,        FORMAT='(a-26, a-2, i1)')
    normal        = string('Normal',             '=', self.normal,        FORMAT='(a-26, a-2, i1)')
    onglass       = string('OnGlass',            '=', self.onglass,       FORMAT='(a-26, a-2, i1)')
    orientation   = string('Orientation',        '=', self.orientation,   FORMAT='(a-26, a-2, f0)')
    relative      = string('Relative',           '=', self.relative,      FORMAT='(a-26, a-2, i1)')
    t3d           = string('T3D',                '=', self.t3d,           FORMAT='(a-26, a-2, i1)')
    text_axes     = string('Text_Axes',          '=', self.text_axes,     FORMAT='(a-26, a-2, i1)')
;    tt_font       = string('TT_Font',            '=', self.tt_font,       FORMAT='(a-26, a-2, f0)')
    updir         = string('UpDir',              '=', self.updir,         FORMAT='(a-26, a-2, 3(f0.4, 3x))')
    valign        = string('Vertical_Alignment', '=', self.vertical_alignment, FORMAT='(a-26, a-2, f0)')
    width         = string('Width',              '=', self.width,         FORMAT='(a-26, a-2, f0)')

    ;Put MrPlot properties together
    selfStr = obj_class(self) + '  <' + strtrim(obj_valid(self, /GET_HEAP_IDENTIFIER), 2) + '>'
    textKeys = [ [alignment], $
                 [baseline], $
                 [box_linestyle], $
                 [box_color], $
                 [bx_pos], $
                 [charsize], $
                 [charthick], $
;                 [clip], $
                 [color], $
                 [data], $
                 [device], $
                 [fill_back], $
                 [fill_color], $
                 [margins], $
                 [noclip], $
                 [normal], $
                 [onglass], $
                 [orientation], $
                 [relative], $
                 [t3d], $
                 [text_axes], $
;                 [tt_font], $
                 [updir], $
                 [valign] $
               ]

    ;Group everything in alphabetical order
    result = [[atomKeys], ['  ' + textKeys]]
    result = [[selfStr],  [result[0, sort(result)]]]
    
    return, result
END


;+
;   The purpose of this method is to calculate the corners of a text-boxes outlined by
;   the character height, text width, and angle of orientation.
;
;          <----LPercent----->
;       (x3,y3)         (xloc,yloc)                      (x1,y1)
;          .-----------------^------------------------------.      _
;          |                 x   The Text                   |      | !d.y_ch_Size
;          .------------------------------------------------.      _
;       (x0,y0)                                          (x2,y2)
;                            <----------RPercent------------>
;          <--------------------Width----------------------->
;-
FUNCTION MrText::GetCorners
    compile_opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN, !Null
    ENDIF
    
    ;Percent of the text to the left of [xloc, yloc]
    lpercent = (*self.width) * (      *self.alignment)
    rpercent = (*self.width) * (1.0 - *self.alignment)
    
    ;Height of characters in normal coordinates
    ychsize = double(!D.Y_CH_Size) / double(!D.Y_Size)
    
    ;Find (x0, y0)
    ;  If the text is oriented at some angle, "width" is the hypotenuse. Get the
    ;  length of each side of the triangle
    x0 = *self.xpos - lpercent * Cos(*self.orientation)
    y0 = *self.ypos - lpercent * Sin(*self.orientation)
    
    ;Find (x2, y2)
    x2 = *self.xpos + rpercent * Cos(*self.orientation)
    y2 = *self.ypos + rpercent * Sin(*self.orientation)
    
    ;Find (x1, y1)
    x1 = x2 - !D.Y_CH_Size * Sin(*self.orientation)
    y1 = y2 + !D.Y_CH_Size * Cos(*self.orientation)
    
    ;Find (x3, y3)
    x3 = x0 - ychsize * Sin(*self.orientation)
    y3 = y0 + ychsize * Cos(*self.orientation)
    
    RETURN, [x0, y0, x2, y2, x1, y1, x3, y3]
END


;+
;   Determine if the coorinate formed by the points x and y lie within the text box
;   outlined by the GetCorners method.
;-
FUNCTION MrText::IsInside, x, y, $
DATA = data, $
DELTA = delta, $
NORMAL = normal
    compile_opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        if !Except eq 0 then !Except = 1
        MrPrintF, 'LogErr'
        RETURN, 0
    ENDIF
    
    ;Get the position
    corners = self -> GetCorners()
    xy0 = corners[0:1]
    xy1 = corners[4:5]
    
    ;Is the graphic rotated?
    IF N_Elements(*self.orientation) GT 0 THEN BEGIN
        ;We want to rotate the graphic back to an orientation of 0 degrees,
        ;so take the negative of the orientation angle
        theta = -(*self.orientation)
        
        ;Form a 2D rotation matrix to rotate back
        rotmat = Transpose([[Cos(theta), -Sin(theta)], $
                            [Sin(theta),  Cos(theta)]])
                            
        ;Rotate. Make sure to centered everything on the origin
        center = [*self.xloc, *self.yloc]
        xy0_prime = (rotmat # (xy0 - center)) + center
        xy1_prime = (rotmat # (xy1 - center)) + center
        
        ;Now for the given coordinates
        xy_prime = (rotmat # ([x,y] - center)) + center
        
    ;Not rotated.
    ENDIF ELSE BEGIN
        xy0_prime = xy0
        xy1_prime = xy1
        xy_prime  = [x,y]
    ENDELSE
    
    ;Determine if (x,y) lies within the area outlined by CORNERS
    IF xy_prime[0] GE xy0_prime[0] && xy_prime[0] LE xy1_prime[0] && $
       xy_prime[1] GE xy0_prime[1] && xy_prime[1] LE xy1_prime[1] $
        THEN tf_inside = 1 $
        ELSE tf_inside = 0
    
    RETURN, tf_inside
END


;+
; This method places text interactively in the display window.
;
; :Params:
;       TEXT:           in, optional, type=string, default=current text
;                       The text to output.
;
; :Keywords:
;       OUTLOC:         out, optional, type=various
;                       Only used if PLACE is set, this is a two-element array containing
;                           the xloc and yloc of the cursor position in the window.
;       WIDTH:          out, optional, type=float
;                       Set this keyword to a named variable in which to return the width
;                           of the text string, in normalized coordinate units. Note that
;                           output keyword values cannot be returned from the routine if
;                           the command is being executed in a cgWindow.
;-
PRO MrText::Place, text, $
OUTLOC=outloc, $
WIDTH=width
    compile_opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        self.hide = 0
        IF n_elements(refresh_in) GT 0 THEN self.window -> Refresh, DISABLE=~refresh_in
        RETURN
    ENDIF
    
    ;Text to be placed
    IF N_Elements(text) EQ 0 $
        THEN textStr = *self.text $
        ELSE textStr = text
        
    ;Redraw everything, but without the text
    self.hide = 1
    self.window -> GetProperty, REFRESH=refresh_in
    self -> Refresh
    if refresh_in eq 0 then self -> Refresh, /DISABLE
    
    ;Stop hiding
    self.hide = 0

    ;Wait for click, then draw text
    cgText, textStr, /PLACE, $
            ALIGNMENT   =  self.alignment, $
            CHARSIZE    =  self.charsize, $
            CHARTHICK   =  self.charthick, $
            CLIP        = *self.clip, $
            COLOR       =  self.color, $
            DATA        =  self.data, $
            DEVICE      =  self.device, $
            FONT        =  self.font, $
            MAP_OBJECT  =  self.map_object, $
            NOCLIP      =  self.noclip, $
            NORMAL      =  self.normal, $
            ORIENTATION = *self.orientation, $
            OUTLOC      =       outloc, $
            T3D         =  self.t3d, $
            TEXT_AXES   =  self.text_axes, $
            TT_FONT     = *self.tt_font, $
            WIDTH       =       width, $
            Z           = *self.z
        
    ;Store the clicked location.
    *self.xloc = outloc[0]
    *self.yloc = outloc[1]
    self.outloc = outloc
    if n_elements(width) gt 0 then self.width = width
END


;+
;   This method draws the axis object.
;
; :Keywords:
;       NOERASE:        in, optional, type=boolean, default=0
;                       If set, the device will not be erased before drawing. The default
;                           is to clear the display before drawing the graphic.
;-
PRO MrText::Draw, $
NOERASE=noerase
    compile_opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        IF color_state EQ 0 then cgSetColorState, 0
        MrPrintF, 'LogErr'
        RETURN
    ENDIF
    
    ;Do not draw anything
    IF self.hide THEN RETURN
    
    ;Draw in decomposed color mode
    color_state = cgGetColorState()
    IF color_state EQ 0 THEN cgSetColorState, 1
    
    ;Draw invisible text first
    self -> DoText, HEIGHT=height, WIDTH=width
    
    ;Draw text in 3D?
    is3D = N_Elements(*self.zloc) GT 0

;-----------------------------------------------------
; Location of Text \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Convert normal coordinates.
    CASE 1 OF
        ;Data coordinates
        self.data: BEGIN
            IF is3D $
                THEN pos = self.target -> ConvertCoord(*self.xloc, *self.yloc, *self.zloc, /DATA, /TO_NORMAL) $
                ELSE pos = self.target -> ConvertCoord(*self.xloc, *self.yloc,             /DATA, /TO_NORMAL)
        ENDCASE
        
        ;Device coordinates
        self.device: BEGIN
            IF is3D $
                THEN pos = self.target -> ConvertCoord(*self.xloc, *self.yloc, *self.zloc, /DEVICE, /TO_NORMAL) $
                ELSE pos = self.target -> ConvertCoord(*self.xloc, *self.yloc,             /DEVICE, /TO_NORMAL)
        ENDCASE
    
        ;Relative coordinates
        self.relative: BEGIN
            ;Restore the target's coordinate system.
            self.target -> RestoreCoords
        
            ;X Position
            IF !x.crange[0] LT !x.crange[1] $
                THEN xloc = (!x.crange[1] - !x.crange[0]) *        *self.xloc  + !x.crange[0] $
                ELSE xloc = (!x.crange[0] - !x.crange[1]) * (1.0 - *self.xloc) + !x.crange[1]
        
            ;Y Position
            IF !y.crange[0] LT !y.crange[1] $
                THEN yloc = (!y.crange[1] - !y.crange[0]) *        *self.yloc  + !y.crange[0] $
                ELSE yloc = (!y.crange[0] - !y.crange[1]) * (1.0 - *self.yloc) + !y.crange[1]
        
            ;Draw in 3D? -- Determine position of text in normal coordinates
            IF is3D THEN BEGIN
                ;Z Position
                IF !z.crange[0] LT !z.crange[1] $
                    THEN zloc = (!z.crange[1] - !y.crange[0]) *        *self.zloc  + !z.crange[0] $
                    ELSE zloc = (!z.crange[0] - !y.crange[1]) * (1.0 - *self.zloc) + !z.crange[1]
                
                ;Convert to normal coordinates
                pos = self.target -> ConvertCoord(xloc, yloc, zloc, /DATA, /TO_NORMAL)
            ENDIF ELSE BEGIN
                pos = self.target -> ConvertCoord(xloc, yloc, /DATA, /TO_NORMAL)
            ENDELSE
        ENDCASE
        
        ;Normal coordinates
        self.normal: BEGIN
            IF is3D $
                THEN pos = [*self.xloc, *self.yloc, *self.zloc] $
                ELSE pos = [*self.xloc, *self.yloc]
        ENDCASE
    ENDCASE
    
    xchar = Float(!D.X_Ch_Size) / Float(!D.X_Size)
    ychar = Float(!D.Y_Ch_Size) / Float(!D.Y_Size)

    ;Determine location of box
    ;   - POS is the original location of the lower-left corner of the first line of text
    bx0         = pos[0] - width * self.alignment
    by0         = pos[1] - (N_Elements(*self.text)-1) * ychar * self.charsize
    bx1         = bx0 + width
    by1         = by0 + height
    
    ;Adjust vertical alignemnt
    by0 -= height * self.vertical_alignment
    by1 -= height * self.vertical_alignment
    self.bx_pos = [bx0, by0, bx1, by1]

    ;Location of text
    ;   - POS is the location of the lower-left corner of the first line of text
    ;   - POS[1] needs to be adjusted for VERTICAL_ALIGNMENT. XYOutS will handle ALIGNMENT automatically.
    txt_pos = [pos[0], by1 - ychar*self.charsize]

    ;Apply margins
    self.bx_pos[0] -= self.margins[0] * xchar * self.charsize
    self.bx_pos[1] -= self.margins[1] * ychar * self.charsize
    self.bx_pos[2] += self.margins[2] * xchar * self.charsize
    self.bx_pos[3] += self.margins[3] * ychar * self.charsize

;-----------------------------------------------------
; Draw Box and Text \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Fill the box with color
    IF self.fill_background THEN BEGIN
        box_color = cgColor(*self.fill_color)
        PolyFill, self.bx_pos[[0,2,2,0,0]], self.bx_pos[[1,1,3,3,1]], COLOR=box_color, /NORMAL
    ENDIF

    ;Outline the box
    box_linestyle = MrLineStyle(*self.box_linestyle)
    box_color     = cgColor(*self.box_color)
    IF box_linestyle NE 6 THEN BEGIN
        PlotS, self.bx_pos[[0,2,2,0,0]], self.bx_pos[[1,1,3,3,1]], $
               COLOR=box_color, LINESTYLE=box_linestyle, /NORMAL
    ENDIF
            
    ;Draw the text
    ;   - Horizontal position is determined automatically by XYOutS and ALIGNMENT
    ;   - Vertical position needs to be adjusted according to VERTICAL_ALIGNMENT
    self -> DoText, *self.text, txt_pos[0], txt_pos[1]
    
    ;Return to the original color state
    IF color_state EQ 0 THEN cgSetColorState, 0
END


;+
;   Draw the text to the screen.
;-
PRO MrText::DoText, theText, x, y, z, $
HEIGHT=height, $
WIDTH=width
    Compile_Opt strictarr
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        IF N_Elements(pixID) GT 0 THEN BEGIN
            WDelete, pixID
            WSet, currentID
        ENDIF
        MrPrintF, 'LogErr'
        RETURN
    ENDIF

;-----------------------------------------------------
; Are we Determining Text Width? \\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;If the position needs to be returned
    IF N_Params() EQ 0 THEN BEGIN
        theText = *self.text
    
        ; In order to figure out how wide the box is, we first need to draw the text.
        ;   - If windows are not supported, we have to draw outside the visible area.
        ;   - For other devices, create a pixmap window and draw in the middle.
        IF ((!D.Flags AND 256) EQ 0) THEN BEGIN
            x = 0.1
            y = 1.25
        ENDIF ELSE BEGIN
            ;Current window
            currentID = !D.Window
    
            ;Open an invisible window
            Window, XSIZE=!D.X_Size, YSIZE=!D.Y_Size, /PIXMAP, /FREE
            pixID = !D.Window

            ;Draw in the middle of the window
            x = 0.5
            y = 0.5
        ENDELSE
    ENDIF

;-----------------------------------------------------
; Rescale for PostScript Output \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Rescale for suitable postscript output
    IF !D.Name eq 'PS' THEN BEGIN
        charsize  = MrPS_Rescale(self.charsize,  /CHARSIZE)
        charthick = MrPS_Rescale(self.charthick, /CHARTHICK)
    ENDIF ELSE BEGIN
        charsize  = self.charsize
        charthick = self.charthick
        font      = self.font
    ENDELSE
    color = cgColor(*self.color)

;-----------------------------------------------------
; Draw the Text \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Keep the text on a single line
    IF N_Elements(theText) GT 1 $
        THEN textStr = StrJoin(theText, '!C') $
        ELSE textStr = theText[0]

    ;Draw the text
    textStr = TexToIDL(textStr)
    XYOutS, x, y, textStr, $
            /NORMAL, $
            ALIGNMENT   =  self.alignment, $
            CHARSIZE    =       charsize, $
            CHARTHICK   =       charthick, $
            CLIP        = *self.clip, $
            COLOR       =       color, $
            FONT        =       font, $
            NOCLIP      =  self.noclip, $
            ORIENTATION =  self.orientation, $
            T3D         =  self.t3d, $
            TEXT_AXES   =  self.text_axes, $
            WIDTH       =       width, $
            Z           =       z

    ;Determine the text height    
    height = N_Elements(*self.text) * (Float(!D.Y_Ch_Size) / Float(!D.Y_Size)) * self.charsize

    ;Set the window
    IF N_Elements(pixID) NE 0 THEN BEGIN
        WDelete, pixID
        WSet, currentID
    ENDIF
END

;+
;   This method obtains the current properties of the object. 
; 
; :Keywords:
;       XLOC:           out, optional, type=depends
;                       The X location of the text. 
;       YLOC:           out, optional, type=depends
;                       The Y location of the text. 
;       TEXT:           out, optional, type=string
;                       The text to output. By default, the calling sequence of the program.
;       ALIGNMENT:      out, optional, type=integer
;                       Set this keyword to indicate the alignment of the text with respect
;                           to the x and y location. 0 is left aligned, 0.5 is centered,
;                           and 1.0 is right aligned. The alignment is set to 0.5 if PLACE
;                           is set and ALIGNMENT is unspecified. Otherwise, the default is 0.
;       CHARSIZE:       out, optional, type=float
;                       The character size for axes annotations. Uses cgDefCharSize to
;                           select default character size, unless !P.Charsize is set, in
;                           which case !P.Charsize is always used.
;       CHARTHICK:      out, optional, type=float
;                       Thickness of vector-drawn fonts.
;       CLIP:           out, optional, type=fltarr(4)
;                       Coordinates of a rectangle used to clip the graphics output. Units
;                           are specified by `DATA`, `DEVICE`, and `NORMAL`.
;       COLOR:          out, optional, type=string/integer/long
;                       The color of the text. Color names are those used with cgColor.
;       CURRENT:        out, optional, type=boolean
;                       If set, the text will be added to the current MrWindow widget.
;       DATA:           out, optional, type=boolean
;                       Set this keyword to indicate xloc and yloc are in data coordinates.
;                           Data coordinates are the default, unless DEVICE or NORMAL is set.
;       DEVICE:         out, optional, type=boolean
;                       Set this keyword to indicate xloc and yloc are in device coordinates.
;       FONT:           out, optional, type=integer
;                       The type of font desired::
;                           -1  -   Hershey vector-drawn fonts
;                            0  -   Device fonts
;                            1  -   TrueType fonts
;       MAP_OBJECT:     out, optional, type=object
;                       If you are drawing on a map projection set up with Map_Proj_Init
;                           and using projected meter space, rather than lat/lon space,
;                           then you can use this keyword to provide a cgMap object that
;                           will allow you to convert the `x` and `y` parameters from
;                           longitude and latitude, respectively, to projected meter space
;                           before drawing.
;       NOCLIP:         out, optional, type=boolean
;                       If set, graphics output will not be clipped.
;       NORMAL:         out, optional, type=boolean
;                       Set this keyword to indicate xloc and yloc are in normalized coordinates.
;       ORIENTATION:    out, optional, type=float
;                       Use this keyword to specify the counterclockwise angle of rotation
;                           of the text in degrees from the horizontal.
;       OUTLOC:         out, optional, type=various
;                       Only used if PLACE is set, this is a two-element array containing
;                           the xloc and yloc of the cursor position in the window.
;       PLACE:          out, optional, type=boolean
;                       Set this keyword if you wish to click the cursor in the graphics
;                           window to place the text. If this keyword is set, you do not
;                           need to specify the `xloc` and `yloc` positional parameters.
;                           The first positional parameter is assumed to be the text. The
;                           clicked location will be returned in the `OutLoc` variable. If
;                           the `Alignment` keyword is not set, it will be set to 0.5 to
;                           set "center" as the default placement alignment. This has been
;                           modified to allow this keyword to work in a resizeable graphics
;                           window as well. Clicking once in the window will set the
;                           parameters so you don't have to click every time the window is
;                           resized.
;       RELATIVE:       out, optional, type=boolean, default=0
;                       If set, then `XLOC` and `YLOC` specify the location with respect
;                           to the normalized data coordinates of `TARGET`.
;       T3D:            out, optional, type=boolean
;                       If set, output will be transformed into 3D using !P.T
;       TARGET:         out, optional, type=object
;                       If `DATA` is set, then the graphics object whose data coordinates
;                           are to be used in converting to and from data coordinates. The
;                           graphics object must have a ConvertCoord method. Note that the
;                           graphics object will not be destroyed when the text object is
;                           destroyed.
;       TEXT_AXES:      out, optional, type=int
;                       Plane of vector drawn text when 3D plotting is enabled::
;                           0   -   XY-Plane
;                           1   -   XZ-Plane
;                           2   -   YZ-Plane
;                           3   -   YX-Plane
;                           4   -   ZX-Plane
;                           5   -   ZY-Plane
;       TT_FONT:        out, optional, type=string
;                       The true-type font to use for the text. Only used if FONT=1.
;       WIDTH:          out, optional, type=float
;                       Set this keyword to a named variable in which to return the width
;                           of the text string, in normalized coordinate units. Note that
;                           output keyword values cannot be returned from the routine if
;                           the command is being executed in a cgWindow.
;       Z:              out, optional, type=number
;                       A Z-coordinate. Used if `T3D` is set.
;       _REF_EXTRA:     out, optional, type=any
;                       Any keywords appropriate for the MrGrAtom::GetProperty method is
;                           also accepted by keyword inheritance
;-
PRO MrText::GetProperty, $
XLOC=xloc, $
YLOC=yloc, $
STRING=text, $
ALIGNMENT=alignment, $
CHARSIZE=charsize, $
CHARTHICK=charthick, $
CLIP=clip, $
COLOR=color, $
CURRENT=current, $
DATA=data, $
DEVICE=device, $
FONT=font, $
MARGINS=margins, $
MAP_OBJECT=map_object, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
OUTLOC=outloc, $
PLACE=place, $
RELATIVE=relative, $
T3D=t3d, $
TARGET=target, $
TEXT_AXES=text_axes, $
TT_FONT=tt_font, $
WIDTH=width, $
Z=z, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN
    ENDIF
    
    IF Arg_Present(xloc)        GT 0 THEN xloc        = *self.xloc
    IF Arg_Present(yloc)        GT 0 THEN yloc        = *self.yloc
    IF Arg_Present(text)        GT 0 THEN text        = *self.text
    IF Arg_Present(alignment)   GT 0 THEN alignment   =  self.alignment
    IF Arg_Present(charsize)    GT 0 THEN charsize    =  self.charsize
    IF Arg_Present(charthick)   GT 0 THEN charthick   =  self.charthick
    IF Arg_Present(clip)        GT 0 THEN clip        = *self.clip
    IF Arg_Present(color)       GT 0 THEN color       =  self.color
    IF Arg_Present(data)        GT 0 THEN data        =  self.data
    IF Arg_Present(device)      GT 0 THEN device      =  self.device
    IF Arg_Present(font)        GT 0 THEN font        =  self.font
    IF Arg_Present(noclip)      GT 0 THEN noclip      =  self.noclip
    IF Arg_Present(normal)      GT 0 THEN normal      =  self.normal
    IF Arg_Present(orientation) GT 0 THEN orientation = *self.orientation
    IF Arg_Present(outloc)      GT 0 THEN outloc      =  self.outloc
    IF Arg_Present(relative)    GT 0 THEN relative    =  self.relative
    IF Arg_Present(t3d)         GT 0 THEN t3d         =  self.t3d
    IF Arg_Present(text_axes)   GT 0 THEN text_axes   =  self.text_axes
    IF Arg_Present(tt_font)     GT 0 THEN tt_font     = *self.tt_font
    IF Arg_Present(width)       GT 0 THEN width       =  self.width
    IF Arg_Present(z)           GT 0 THEN z           = *self.z

    IF Arg_Present(map_obj) THEN BEGIN
        IF Obj_Valid(self.map_object) $
            THEN map_object = self.map_object $
            ELSE map_obj = Obj_New()
    ENDIF
    
    IF Arg_Present(target) THEN BEGIN
        IF Obj_Valid(self.target) $
            THEN target = self.target $
            ELSE target = Obj_New()
    ENDIF
    
    IF Arg_Present(position) THEN position = self -> GetPosition()
    
    IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::GetProperty, _STRICT_EXTRA=extra
END


;+
;   This method sets the properties of the object.
;
; :Keywords:
;       XLOC:           in, optional, type=depends
;                       The X location of the text. 
;       YLOC:           in, optional, type=depends
;                       The Y location of the text. 
;       TEXT:           in, optional, type=string
;                       The text to output. By default, the calling sequence of the program.
;       ALIGNMENT:      in, optional, type=integer
;                       Set this keyword to indicate the alignment of the text with respect
;                           to the x and y location. 0 is left aligned, 0.5 is centered,
;                           and 1.0 is right aligned. The alignment is set to 0.5 if PLACE
;                           is set and ALIGNMENT is unspecified. Otherwise, the default is 0.
;       CHARSIZE:       in, optional, type=float
;                       The character size for axes annotations. Uses cgDefCharSize to
;                           select default character size, unless !P.Charsize is set, in
;                           which case !P.Charsize is always used.
;       CHARTHICK:      in, optional, type=float
;                       Thickness of vector-drawn fonts.
;       CLIP:           in, optional, type=fltarr(4)
;                       Coordinates of a rectangle used to clip the graphics output. Units
;                           are specified by `DATA`, `DEVICE`, and `NORMAL`.
;       COLOR:          in, optional, type=string/integer/long
;                       The color of the text. Color names are those used with cgColor.
;       CURRENT:        in, optional, type=boolean
;                       If set, the text will be added to the current MrWindow widget.
;       DATA:           in, optional, type=boolean
;                       Set this keyword to indicate xloc and yloc are in data coordinates.
;                           Data coordinates are the default, unless DEVICE or NORMAL is set.
;       DEVICE:         in, optional, type=boolean
;                       Set this keyword to indicate xloc and yloc are in device coordinates.
;       FONT:           in, optional, type=integer
;                       The type of font desired::
;                           -1  -   Hershey vector-drawn fonts
;                            0  -   Device fonts
;                            1  -   TrueType fonts
;       MAP_OBJECT:     in, optional, type=object
;                       If you are drawing on a map projection set up with Map_Proj_Init
;                           and using projected meter space, rather than lat/lon space,
;                           then you can use this keyword to provide a cgMap object that
;                           will allow you to convert the `x` and `y` parameters from
;                           longitude and latitude, respectively, to projected meter space
;                           before drawing.
;       NOCLIP:         in, optional, type=boolean
;                       If set, graphics output will not be clipped.
;       NORMAL:         in, optional, type=boolean
;                       Set this keyword to indicate xloc and yloc are in normalized coordinates.
;       ORIENTATION:    in, optional, type=float
;                       Use this keyword to specify the counterclockwise angle of rotation
;                           of the text in degrees from the horizontal.
;       OUTLOC:         out, optional, type=various
;                       Only used if PLACE is set, this is a two-element array containing
;                           the xloc and yloc of the cursor position in the window.
;       PLACE:          in, optional, type=boolean
;                       Set this keyword if you wish to click the cursor in the graphics
;                           window to place the text. If this keyword is set, you do not
;                           need to specify the `xloc` and `yloc` positional parameters.
;                           The first positional parameter is assumed to be the text. The
;                           clicked location will be returned in the `OutLoc` variable. If
;                           the `Alignment` keyword is not set, it will be set to 0.5 to
;                           set "center" as the default placement alignment. This has been
;                           modified to allow this keyword to work in a resizeable graphics
;                           window as well. Clicking once in the window will set the
;                           parameters so you don't have to click every time the window is
;                           resized.
;       RELATIVE:       in, optional, type=boolean, default=0
;                       If set, then `XLOC` and `YLOC` specify the location with respect
;                           to the normalized data coordinates of `TARGET`.
;       T3D:            in, optional, type=boolean
;                       If set, output will be transformed into 3D using !P.T
;       TARGET:         in, optional, type=object
;                       If `DATA` is set, then the graphics object whose data coordinates
;                           are to be used in converting to and from data coordinates. The
;                           graphics object must have a ConvertCoord method. Note that the
;                           graphics object will not be destroyed when the text object is
;                           destroyed.
;       TEXT_AXES:      in, optional, type=int
;                       Plane of vector drawn text when 3D plotting is enabled::
;                           0   -   XY-Plane
;                           1   -   XZ-Plane
;                           2   -   YZ-Plane
;                           3   -   YX-Plane
;                           4   -   ZX-Plane
;                           5   -   ZY-Plane
;       TT_FONT:        in, optional, type=string
;                       The true-type font to use for the text. Only used if FONT=1.
;       WIDTH:          out, optional, type=float
;                       Set this keyword to a named variable in which to return the width
;                           of the text string, in normalized coordinate units. Note that
;                           output keyword values cannot be returned from the routine if
;                           the command is being executed in a cgWindow.
;       Z:              in, optional, type=number
;                       A Z-coordinate. Used if `T3D` is set.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keywords appropriate for the MrGrAtom::SetProperty method is
;                           also accepted by keyword inheritance
;-
PRO MrText::SetProperty, $
XLOC=xloc, $
YLOC=yloc, $
ZLOC=zloc, $
STRING=text, $
ALIGNMENT=alignment, $
BASELINE=baseline, $
BOX_LINESTYLE=box_linestyle, $
BOX_COLOR=box_color, $
CHARSIZE=charsize, $
CHARTHICK=charthick, $
CLIP=clip, $
COLOR=color, $
CURRENT=current, $
DATA=data, $
DEVICE=device, $
FILL_BACKGROUND=fill_background, $
FILL_COLOR=fill_color, $
FONT=font, $
MAP_OBJECT=map_object, $
MARGINS=margins, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
PLACE=place, $
RELATIVE=relative, $
T3D=t3d, $
TARGET=target, $
TEXT_AXES=text_axes, $
TT_FONT=tt_font, $
UPDIR=updir, $
VERTICAL_ALIGNMENT=vertical_alignment, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN
    ENDIF
    
    IF N_Elements(xloc)               GT 0 THEN *self.xloc               = xloc
    IF N_Elements(yloc)               GT 0 THEN *self.yloc               = yloc
    IF N_Elements(zloc)               GT 0 THEN *self.zloc               = zloc
    IF N_Elements(text)               GT 0 THEN *self.text               = text
    IF N_Elements(alignment)          GT 0 THEN  self.alignment          = alignment
    IF N_Elements(box_color)          GT 0 THEN *self.box_color          = box_color
    IF N_Elements(box_linestyle)      GT 0 THEN *self.box_linestyle      = box_linestyle
    IF N_Elements(charsize)           GT 0 THEN  self.charsize           = charsize
    IF N_Elements(charthick)          GT 0 THEN  self.charthick          = charthick
    IF N_Elements(clip)               GT 0 THEN *self.clip               = clip
    IF N_Elements(color)              GT 0 THEN *self.color              = color
    IF N_Elements(fill_background)    GT 0 THEN  self.fill_background    = fill_background
    IF N_Elements(fill_color)         GT 0 THEN *self.fill_color         = fill_color
    IF N_Elements(font)               GT 0 THEN  self.font               = font
    IF N_Elements(noclip)             GT 0 THEN  self.noclip             = Keyword_Set(noclip)
    IF N_Elements(onglass)            GT 0 THEN  self.onglass            = Keyword_Set(onglass)
    IF N_Elements(t3d)                GT 0 THEN  self.t3d                = Keyword_Set(t3d)
    IF N_Elements(tt_font)            GT 0 THEN *self.tt_font            = tt_font
    IF N_Elements(vertical_alignment) GT 0 THEN  self.vertical_alignment = vertical_alignment
    
    ;Margins
    IF N_Elements(margins) GT 0 THEN BEGIN
        CASE N_Elements(self.margins) OF
            1: self.margins = margins
            2: BEGIN
                self.margins[[0,2]] = margins[0]
                self.margins[[1,3]] = margins[1]
            ENDCASE
            4: self.margins = margins
            ELSE: Message, 'MARGINS must have 1, 2, or 4 elements.', /INFORMATIONAL
        ENDCASE
    ENDIF
    
    ;Orientation
    ;   - Rotate counter-clockwise from X-axis (same as rotation about z-axis)
    ;   - Must come before ORIENTATION, ONGLASS, BASELINE, and UPDIR
    IF N_Elements(orientation) GT 0 THEN BEGIN
        onglass = 1
        theta   = orientation * !DtoR
        
        ;Rotate Text
        ;              Baseline      UpDir   Depth
        ;
        ;          |  cos(theta)  sin(theta)   0 |
        ; matrix = | -sin(theta)  cos(theta)   0 |
        ;          |       0           0       1 |
        ;
        baseline = [ cos(theta), sin(theta), 0]
        updir    = [-sin(theta), cos(theta), 0]
        self.orientation = orientation
    ENDIF
    
    ;OnGlass?
    ;   - Must come after ORIENTATION and before BASELINE and UPDIR
    IF N_Elements(onglass) EQ 1 THEN BEGIN
        ;Set OnGlass
        ;   - Turn 3D transformations off
        self.onglass = Keyword_Set(onglass)
        IF self.onglass EQ 0 THEN self.t3d = 0B
    
        ;Project text onto the glass
        ;   - Set Z = 0 to put text up front
        ;   - Do not rotate text toward the Z-direction
        *self.zloc   = 0
        updir        = [self.updir[0:1], 0.0]
        baseline     = [self.baseline[0:1], 0.0]
    ENDIF
    
    ;Text Axes determines
    IF N_Elements(text_axes) GT 0 THEN BEGIN
        CASE text_axes OF
            ;XY-Plane
            0: BEGIN
                baseline = [1.0, 0.0, 0.0]
                updir    = [0.0, 1.0, 0.0]
            ENDCASE
            
            ;XZ-Plane
            1: BEGIN
                baseline = [1.0, 0.0, 0.0]
                updir    = [0.0, 0.0, 1.0]
            ENDCASE
            
            ;YZ-Plane
            2: BEGIN
                baseline = [0.0, 0.0, 1.0]
                updir    = [0.0, 1.0, 0.0]
            ENDCASE
            
            ;YX-Plane
            3: BEGIN
                baseline = [0.0, 1.0, 0.0]
                updir    = [1.0, 0.0, 0.0]
            ENDCASE
            
            ;ZX-Plane
            4: BEGIN
                baseline = [0.0, 0.0, 1.0]
                updir    = [1.0, 0.0, 0.0]
            ENDCASE
            
            ;ZY-Plane
            5: BEGIN
                baseline = [0.0, 0.0, 1.0]
                updir    = [0.0, 1.0, 0.0]
            ENDCASE
            
            ELSE: Message, 'TEXT_AXES must be an integer 0-5.', /INFORMATIONAL
        ENDCASE
    ENDIF
    
    
    ;Base line direction
    IF N_Elements(baseline) GT 0 THEN BEGIN
        ;Normalize the vector
        base_norm     = FltArr(3)
        base_norm[0]  = baseline / Sqrt(Total(baseline^2))
        self.baseline = base_norm
        
        ;Set the first column of the tranformation matrix
;        self.tmatrix[*,0] = basenorm
    ENDIF
    
    ;Up direction
    IF N_Elements(updir) GT 0 THEN BEGIN
        ;Normalize
        up_norm    = FltArr(3)
        up_norm[0] = updir / Sqrt(Total(updir^2))
        
        ;Find the direction perpendicular to UPDIR and BASELINE
        z_dir = FltArr(3)
        z_dir[0] = up_norm[1] * self.baseline[2] - up_norm[2] * self.baseline[1]
        z_dir[1] = up_norm[2] * self.baseline[0] - up_norm[0] * self.baseline[2]
        z_dir[2] = up_norm[0] * self.baseline[1] - up_norm[1] * self.baseline[0]

        ;UPDIR and BASELINE cannot be parallel
        IF Sqrt(Total(z_dir^2)) EQ 0 THEN BEGIN
            Message, 'BASELINE and UPDIR vectors are coincident.', /INFORMATIONAL
        ENDIF ELSE BEGIN
            ;Ensure UPDIR is perpendicular to BASELINE by taking Z_DIR x BASELINE
            up_perp = FltArr(3)
            up_perp[0] = z_dir[1] * self.baseline[2] - z_dir[2] * self.baseline[1]
            up_perp[1] = z_dir[2] * self.baseline[0] - z_dir[0] * self.baseline[2]
            up_perp[2] = z_dir[0] * self.baseline[1] - z_dir[1] * self.baseline[0]
        
            ;Normalize again
            up_perp = up_perp / Sqrt(Total(up_perp^2))
        
            ;Save the up-direction
            self.updir = up_perp
        ENDELSE
    ENDIF
    
    
    
    ;DATA, DEVICE, NORMAL, and RELATIVE depend on each other.
    ;   - DATA takes precendence, so set last.
    ;   - RELATIVE automatically sets DATA = 1
    IF N_Elements(device) GT 0 THEN BEGIN
        self.device = Keyword_Set(device)
        IF self.device THEN BEGIN
            self.data     = 0B
            self.normal   = 0B
            self.relative = 0B
        ENDIF
    ENDIF
    
    IF N_Elements(normal) GT 0 THEN BEGIN
        self.normal = Keyword_Set(normal)
        IF self.normal THEN BEGIN
            self.data     = 0B
            self.device   = 0B
            self.relative = 0B
        ENDIF
    ENDIF

    ;A relative position is with respect to the dataspace.
    IF N_Elements(relative) GT 0 THEN BEGIN
        self.relative = Keyword_Set(relative)
        IF self.relative THEN BEGIN
            self.data   = 0B
            self.device = 0B
            self.normal = 0B
        ENDIF
    ENDIF
    
    IF N_Elements(data) GT 0 THEN BEGIN
        self.data = Keyword_Set(data)
        IF self.data THEN BEGIN
            self.device   = 0B
            self.normal   = 0B
            self.relative = 0B
        ENDIF
    ENDIF
        
    
    IF N_Elements(map_obj) GT 0 THEN IF Obj_Valid(map_object) $
        THEN self.map_object = map_object $
        ELSE self.map_object = Obj_New()
    
    IF N_Elements(target) GT 0 THEN IF Obj_Valid(target) $
        THEN self.target = target $
        ELSE self.target = Obj_New()

    IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra

    ;Draw the text?
    IF Keyword_Set(Place) THEN self -> Draw, text, PLACE=place, OUTLOC=outloc, WIDTH=width

    self.window -> Draw
END


;+
; The clean-up routine for the object. Destroy pointers, etc.
;-
PRO MrText::cleanup
    compile_opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN
    ENDIF
    
    ;Free Pointers
    Ptr_Free, self.xloc
    Ptr_Free, self.yloc
    Ptr_Free, self.zloc
    Ptr_Free, self.text
    Ptr_Free, self.box_linestyle
    Ptr_Free, self.box_color
    Ptr_Free, self.clip
    Ptr_Free, self.fill_color
    Ptr_Free, self.tt_font
    
    self -> MrGrAtom::CleanUp
END


;+
;   Provides a device-independent and color-model-independent way to write text into
;   a graphics window. It is a wrapper to the XYOUTS command.
;
; :Params:
;       XLOC:           in, required, type=depends
;                       The X location of the text.
;       YLOC:           in, required, type=depends
;                       The Y location of the text.
;       TEXT:           in, optional, type=string
;                       The text to output. By default, the calling sequence of the program.
;
; :Keywords:
;       ALIGNMENT:      in, optional, type=integer, default=0
;                       Set this keyword to indicate the alignment of the text with respect
;                           to the x and y location. 0 is left aligned, 0.5 is centered,
;                           and 1.0 is right aligned. The alignment is set to 0.5 if PLACE
;                           is set and ALIGNMENT is unspecified. Otherwise, the default is 0.
;       CHARSIZE:       in, optional, type=float, default=cgDefCharSize()
;                       The character size for axes annotations. Uses cgDefCharSize to
;                           select default character size, unless !P.Charsize is set, in
;                           which case !P.Charsize is always used.
;       CHARTHICK:      in, optional, type=float, default=1.0
;                       Thickness of vector-drawn fonts.
;       CLIP:           in, optional, type=fltarr(4)
;                       Coordinates of a rectangle used to clip the graphics output. Units
;                           are specified by `DATA`, `DEVICE`, and `NORMAL`.
;       COLOR:          in, optional, type=string/integer/long, default="opposite"
;                       The color of the text. Color names are those used with cgColor.
;       CURRENT:        in, optional, type=boolean, default=0
;                       If set, the text will be added to the current MrWindow widget.
;       DATA:           in, optional, type=boolean
;                       Set this keyword to indicate xloc and yloc are in data coordinates.
;                           Data coordinates are the default, unless DEVICE or NORMAL is set.
;       DEVICE:         in, optional, type=boolean
;                       Set this keyword to indicate xloc and yloc are in device coordinates.
;       FONT:           in, optional, type=integer, default=-1
;                       The type of font desired::
;                           -1  -   Hershey vector-drawn fonts
;                            0  -   Device fonts
;                            1  -   TrueType fonts
;       MAP_OBJECT:     in, optional, type=object, default=obj_new()
;                       If you are drawing on a map projection set up with Map_Proj_Init
;                           and using projected meter space, rather than lat/lon space,
;                           then you can use this keyword to provide a cgMap object that
;                           will allow you to convert the `x` and `y` parameters from
;                           longitude and latitude, respectively, to projected meter space
;                           before drawing.
;       NOCLIP:         in, optional, type=boolean, default=0
;                       If set, graphics output will not be clipped.
;       NORMAL:         in, optional, type=boolean
;                       Set this keyword to indicate xloc and yloc are in normalized coordinates.
;       ORIENTATION:    in, optional, type=float, default=0.0
;                       Use this keyword to specify the counterclockwise angle of rotation
;                           of the text in degrees from the horizontal.
;       RELATIVE:       in, optional, type=boolean, default=0
;                       If set, then `XLOC` and `YLOC` specify the location with respect
;                           to the normalized data coordinates of `TARGET`.
;       T3D:            in, optional, type=boolean, default=0
;                       If set, output will be transformed into 3D using !P.T
;       TARGET:         in, optional, type=object, default=obj_new()
;                       If `DATA` is set, then the graphics object whose data coordinates
;                           are to be used in converting to and from data coordinates. The
;                           graphics object must have a ConvertCoord method. If no target
;                           is provided, the text will be placed in the current window. If
;                           no window is available, one will be created.
;       TEXT_AXES:      in, optional, type=int, default=0
;                       Plane of vector drawn text when 3D plotting is enabled::
;                           0   -   XY-Plane
;                           1   -   XZ-Plane
;                           2   -   YZ-Plane
;                           3   -   YX-Plane
;                           4   -   ZX-Plane
;                           5   -   ZY-Plane
;       TT_FONT:        in, optional, type=string
;                       The true-type font to use for the text. Only used if FONT=1.
;       Z:              in, optional, type=number
;                       A Z-coordinate. Used if `T3D` is set.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keywords appropriate for the MrGrAtom::SetProperty method is
;                           also accepted by keyword inheritance
;-
FUNCTION MrText::init, x, y, z, text, $
ALIGNMENT=alignment, $
BASELINE=baseline, $
BOX_COLOR=box_color, $
BOX_LINESTYLE=box_linestyle, $
CHARSIZE=charsize, $
CHARTHICK=charthick, $
CLIP=clip, $
COLOR=color, $
CURRENT=current, $
DATA=data, $
DEVICE=device, $
FILL_BACKGROUND=fill_background, $
FILL_COLOR=fill_color, $
FONT=font, $
MAP_OBJECT=map_object, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
RELATIVE=relative, $
T3D=t3d, $
TARGET=target, $
TEXT_AXES=text_axes, $
TT_FONT=tt_font, $
UPDIR=updir, $
VERTICAL_ALIGNMENT=vertical_alignment, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN, 0
    ENDIF

;---------------------------------------------------------------------
;Superclass & Window /////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Window is obtained by MrGrAtom
    if self -> MrGrAtom::INIT(TARGET=target, /CURRENT, WINREFRESH=winRefresh) eq 0 $
        then message, 'Unable to initialize MrGrAtom'

;---------------------------------------------------------------------
;Location ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;If PLACE is set, then TEXT is the first parameter.
    IF Keyword_Set(place) THEN BEGIN
        textStr = xloc
    
    ENDIF ELSE BEGIN
        ;Number of parameters given
        nparams = N_Elements(x)      EQ 0 ? 0 : $
                    N_Elements(y)    EQ 0 ? 1 : $
                    N_Elements(z)    EQ 0 ? 2 : $
                    N_Elements(text) EQ 0 ? 3 : $
                    4
        IF nparams LT 3 THEN Message, 'Incorrect number of parameters.'
        
        ;If X, Y, and TEXT were given
        IF NPARAMS EQ 3 THEN BEGIN
            ; If the text is specified as the first parameter, move things around.
            IF Size(xloc, /TNAME) EQ 'STRING' THEN BEGIN
                xloc    = y
                yloc    = z
                theText = x
            ENDIF ELSE BEGIN
                xloc    = x
                yloc    = y
                theText = z
            ENDELSE
            
        ;IF Z was also given
        ENDIF ELSE BEGIN
            ; If the text is specified as the first parameter, move things around.
            IF Size(xloc, /TNAME) EQ 'STRING' THEN BEGIN
                xloc    = y
                yloc    = z
                zloc    = text
                theText = x
            ENDIF ELSE BEGIN
                xloc    = x
                yloc    = y
                zloc    = z
                theText = text
            ENDELSE
        ENDELSE
    ENDELSE

;---------------------------------------------------------------------
;Object Properties ///////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Set defaults
    place           = keyword_set(place)
    fill_background = keyword_set(fill_background)
    data            = keyword_set(data)
    device          = keyword_set(device)
    normal          = keyword_set(normal)
    relative        = keyword_set(relative)
    t3d             = keyword_set(t3d)
    IF normal + data + device + relative EQ 0 THEN normal = 1
    IF normal + data + device + relative NE 1 $
        THEN Message, 'Exactly one of DATA, DEVICE, NORMAL, and RELATIVE must be set.'
    
    if n_elements(alignment)          eq 0 then alignment          = 0.0
    if n_elements(baseline)           eq 0 then baseline           = [1.0, 0.0, 0.0]
    if n_elements(box_color)          eq 0 then box_color          = 'Black'
    if n_elements(box_linestyle)      eq 0 then box_linestyle      = 'None'
    if n_elements(charsize)           eq 0 then charsize           = 1.5
    if n_elements(charthick)          eq 0 then charthick          = 1.0
    if n_elements(color)              eq 0 then color              = 'Black'
    if n_elements(background_color)   eq 0 then background_color   = 'White'
    if n_elements(noclip)             eq 0 then noclip             = 1B
    if n_elements(font)               eq 0 then font               = -1
    if n_elements(margins)            eq 0 then margins            = [0.5, 0.5, 0.5, 0.25]
    if n_elements(text_axes)          eq 0 then text_axes          = 0
    if n_elements(updir)              eq 0 then updir              = [0.0, 1.0, 0.0]
    if n_elements(vertical_alignment) eq 0 then vertical_alignment = 0.0

    ;Make pointers valid
    self.xloc          = Ptr_New(/ALLOCATE_HEAP)
    self.yloc          = Ptr_New(/ALLOCATE_HEAP)
    self.text          = Ptr_New(/ALLOCATE_HEAP)
    self.box_color     = Ptr_New(/ALLOCATE_HEAP)
    self.box_linestyle = Ptr_New(/ALLOCATE_HEAP)
    self.clip          = Ptr_New(/ALLOCATE_HEAP)
    self.color         = Ptr_New(/ALLOCATE_HEAP)
    self.fill_color    = Ptr_New(/ALLOCATE_HEAP)
    self.tt_font       = Ptr_New(/ALLOCATE_HEAP)
    self.zloc          = Ptr_New(/ALLOCATE_HEAP)
    
    ;Objects
    self.target     = Obj_New()
    self.map_object = Obj_New()

    ;Set object properties
    self -> SetProperty, XLOC               = xloc, $
                         YLOC               = yloc, $
                         ZLOC               = zloc, $
                         STRING             = theText, $
                         ALIGNMENT          = alignment, $
                         BASELINE           = baseline, $
                         BOX_LINESTYLE      = box_linestyle, $
                         BOX_COLOR          = box_color, $
                         CHARSIZE           = charsize, $
                         CHARTHICK          = charthick, $
                         CLIP               = clip, $
                         COLOR              = color, $
                         CURRENT            = current, $
                         FILL_BACKGROUND    = fill_background, $
                         FILL_COLOR         = fill_color, $
                         FONT               = font, $
                         MAP_OBJECT         = map_object, $
                         MARGINS            = margins, $
                         NOCLIP             = noclip, $
                         ORIENTATION        = orientation, $
                         T3D                = onglass, $
                         TARGET             = target, $
                         TEXT_AXES          = text_axes, $
                         TT_FONT            = tt_font, $
                         UPDIR              = updir, $
                         VERTICAL_ALIGNMENT = vertical_alignment, $
                        _EXTRA              = extra

    ;Set coordinate keywords here
    ;   - They depend on each other, so one sets all in the SetProperty method.
    ;   - Setting three to 0 and one to 1 causes problems.
    self.data     = data
    self.device   = device
    self.normal   = normal
    self.relative = relative

    ;Refresh the graphics?
    if winRefresh then self -> Refresh
                         
    Return, 1
END


;+
; The class definition module for the object.
;
; :Fields:
;       xloc:           x-coordinate of where to place the text
;       yloc:           y-coordinate of where to place the text
;       text:           text string to be written to the display
;       graphic:        The graphic on which to place the text (if DATA coordinates provided).
;       alignment:      Alignment of text with respect to [xloc,yloc] (right/left/center justified)
;       charsize:       Size of the characters within the text
;       charthick:      Thickness of the characters within the text.
;       clip:       
;       color:          Color of the text.
;       data:           [xloc, yloc] are specified in data coordinates.
;       device:         [xloc, yloc] are specified in device coordinates.
;       font:           Type of font desired.
;       map_object:     Map on which to place the text.
;       noclip:
;       normal:         [xloc, yloc] are specified in normal coordinates.
;       orientation:    Counter-clockwise angle of orientation of the text.
;       outloc:         If text is placed interactively, the location.
;       relative:       Location is given in terms of a normalized dataspace.
;       t3d:            Transform to 3D space.
;       text_axes:      
;       tt_font:        TrueType font to use.
;       z:              Z-coordinate of the text when drawn in 3D
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO MrText__define, class
    
    class = { MrText, $
              inherits MrGrAtom, $          ;For dot-referencing in IDL>=8.0
              xloc:               Ptr_New(), $
              yloc:               Ptr_New(), $
              zloc:               Ptr_New(), $
              text:               Ptr_New(), $
              alignment:          0.0, $
              baseline:           [0.0, 0.0, 0.0], $
              box_linestyle:      Ptr_New(), $
              box_color:          Ptr_New(), $
              bx_pos:             [0.0, 0.0, 0.0, 0.0], $
              charsize:           0.0, $
              charthick:          0S, $
              clip:               Ptr_New(), $
              color:              Ptr_New(), $
              data:               0B, $
              device:             0B, $
              fill_background:    0B, $
              fill_color:         Ptr_New(), $
              font:               0, $
              map_object:         Obj_New(), $
              margins:            [0.0, 0.0, 0.0, 0.0], $
              noclip:             0B, $
              normal:             0B, $
              onglass:            0B, $
              orientation:        0.0, $
              relative:           0B, $
              t3d:                0B, $
              target:             Obj_New(), $
              text_axes:          0, $
              tt_font:            Ptr_New(), $
              updir:              [0.0, 0.0, 0.0], $
              vertical_alignment: 0.0, $
              width:              0.0 $
            }
END