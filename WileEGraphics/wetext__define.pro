; docformat = 'rst'
;
; NAME:
;   weText__Define
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
;       myText = obj_new('weText', 0.5, 0.5, 'Look at my Text!', Color='Blue', $
;                        /NORMAL, /DRAW, WIDTH=width)
;       obj_destroy, myText
;
;  Click to place the text::
;       myText = obj_new('weText', 'Look at my Text!', Color='Blue', /DRAW, /PLACE)
;       obj_destroy, myText
;
;  Change and move the existing text via the SetProperty method::
;       myText = obj_new('weText', 0.5, 0.5, 'Look at my Text!', Color='Blue', /NORMAL, /DRAW)
;       myText -> SetProperty, STRING='Put it Over Here!', /PLACE, OUTLOC=outloc, WIDTH=width
;       obj_destroy, myText
;
;  Change and move the existing text via the draw method, retrieve OUTLOC and WIDTH::
;       myText = obj_new('weText', 0.5, 0.5, 'Look at my Text!', Color='Blue', /NORMAL, /DRAW)
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
;-
;*****************************************************************************************
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
FUNCTION weText::GetCorners
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
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
FUNCTION weText::IsInside, x, y, $
DATA = data, $
DELTA = delta, $
NORMAL = normal
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        if !Except eq 0 then !Except = 1
        void = cgErrorMsg()
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
; This method draws the axis object.
;-
PRO weText::Draw, text, $
NOERASE=noerase, $
OUTLOC=outloc, $
PLACE=place, $
WIDTH=width
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    IF self.hide THEN RETURN
    
    ;Let user click where text is to be placed
    IF Keyword_Set(place) THEN BEGIN
        IF N_Elements(text) EQ 0 $
            THEN textStr = *self.text $
            ELSE textStr = text

        ;Wait for click, then draw text
        cgText, textStr, $
                ALIGNMENT=*self.alignment, $
                CHARSIZE=*self.charsize, $
                CHARTHICK=*self.charthick, $
                CLIP=*self.clip, $
                COLOR=*self.color, $
                DATA=*self.data, $
                DEVICE=*self.device, $
                FONT=*self.font, $
                MAP_OBJECT=self.map_object, $
                NOCLIP=*self.noclip, $
                NORMAL=*self.normal, $
                ORIENTATION=*self.orientation, $
                OUTLOC=outloc, $
                PLACE=place, $
                T3D=*self.t3d, $
                TEXT_AXES=*self.text_axes, $
                TT_FONT=*self.tt_font, $
                WIDTH=width, $
                Z=*self.z
        
        ;Store the clicked location.
        *self.xloc = outloc[0]
        *self.yloc = outloc[1]
        *self.outloc = outloc
    
    ;Position was provided...
    ENDIF ELSE BEGIN
    
        ;Convert from data to normal coordinates if DATA is set and a graphic was given.
        ;In this way the text does not have to be drawn immediately after the plot is
        ;drawn.
        IF *self.data EQ 1 AND Obj_Valid(self.target) THEN BEGIN
            coords = self.target -> ConvertCoord(*self.xloc, *self.yloc, $
                                                 /DATA, /TO_NORMAL)
            xloc = coords[0]
            yloc = coords[1]
            normal = 1
            data = 0
            device = 0
        ENDIF ELSE BEGIN
            xloc = *self.xloc
            yloc = *self.yloc
            normal = *self.normal
            data = *self.data
            device = *self.device
        ENDELSE

        ;Draw the text
        cgText, xloc, yloc, *self.text, $
                ALIGNMENT=*self.alignment, $
                CHARSIZE=*self.charsize, $
                CHARTHICK=*self.charthick, $
                CLIP=*self.clip, $
                COLOR=*self.color, $
                DATA=data, $
                DEVICE=device, $
                FONT=*self.font, $
                MAP_OBJECT=self.map_object, $
                NOCLIP=*self.noclip, $
                NORMAL=normal, $
                ORIENTATION=*self.orientation, $
                T3D=*self.t3d, $
                TEXT_AXES=*self.text_axes, $
                TT_FONT=*self.tt_font, $
                WIDTH=width, $
                Z=*self.z
    ENDELSE
    
    IF N_Elements(width) NE 0 THEN *self.width = width
END


;+
;   This method obtains the current properties of the object. 
; 
; :Keywords:
;     xloc: out, required, type=depends
;       The X location of the text. 
;     yloc: out, required, type=depends
;       The Y location of the text. 
;     string: out, optional, type=string
;        The text to output. By default, the calling sequence of the program.
;     alignment: out, optional, type=integer, 
;         Set this keyword to indicate the alignment of the text with respect to the
;         x and y location. 0 is left aligned, 0.5 is centered, and 1.0 is right aligned.
;         The alignment is set to 0.5 if PLACE is set and ALIGNMENT is unspecified. 
;         Otherwise, the default is 0.
;     charsize: out, optional, type=float, 
;         The character size for axes annotations. Uses cgDefCharSize to select default
;         character size, unless !P.Charsize is set, in which case !P.Charsize is always used.
;     charthick: out, optional, type=float
;         The line thickness of the vector drawn font characters, from 0.0-1.0
;     clip: out, optional, type=fltarr(4)
;         The coordinates of a rectangle used to clip the graphics output.
;     color: out, optional, type=string/integer/long, 
;         The color of the text. Color names are those used with cgColor. 
;     data: out, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in data coordinates. Data coordinates
;         are the default, unless DEVICE or NORMAL is set.
;     device: out, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in device coordinates.
;     font: out, optional, type=integer, 
;         The type of font desired. By default, !P.Font.
;     graphic: in, optional, type=object
;          The graphics object whose data coordinates are to be used in converting to and
;          from data coordinates.
;     map_object: out, optional, type=object
;        If you are drawing on a map projection set up with Map_Proj_Init
;        and using projected meter space, rather than lat/lon space, then you can use this
;        keyword to provide a cgMap object that will allow you to convert the `x` and `y`
;        parameters from longitude and latitude, respectively, to projected meter space
;        before drawing.
;     noclip: out, optional, type=boolean
;        Set this keyword to suppress clipping of the plot.
;     normal: out, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in normalized coordinates.
;     orientation: out, optional, type=float, 
;         Use this keyword to specify the counterclockwise angle of rotation of the text
;         in degrees from the horizontal.
;     outloc: out, optional, type=various
;         Only used if PLACE was set, this is a two-element array containing the xloc and yloc
;         of the cursor position in the window.
;     t3d: out, optional, type=boolean
;          Set this keyword to indicate that the generalized transformation matrix in
;          !P.T is to be used.
;     text_axes: out, optional, type=integer
;          This keyword specifies the plane of vector drawn text when three-dimensional
;          plotting is enabled.
;     tt_font: out, optional, type=string
;         The true-type font to use for the text. Only used if FONT=1.
;     width: out, optional, type=float
;         Set this keyword to a named variable in which to return the width of the text string, 
;         in normalized coordinate units. Note that output keyword values cannot be returned
;         from the routine if the command is being executed in a cgWindow. Returned only
;         if `STRING` and `PLACE` are used.
;     z: out, optional, type=float
;         The Z coordinate if a Z parameter is not present in the call.
;-
PRO weText::GetProperty, $
XLOC=xloc, $
YLOC=yloc, $
STRING=text, $
ALIGNMENT=alignment, $
CHARSIZE=charsize, $
CHARTHICK=charthick, $
CLIP=clip, $
COLOR=color, $
DATA=data, $
DEVICE=device, $
FONT=font, $
GRAPHIC=graphic, $
MAP_OBJECT=map_object, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
OUTLOC=outloc, $
POSITION=position, $
T3D=t3d, $
TEXT_AXES=text_axes, $
TT_FONT=tt_font, $
WIDTH=width, $
Z=z, $
_REF_EXTRA=extra
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    IF Arg_Present(xloc)        NE 0 AND N_Elements(*self.xloc)        NE 0 THEN xloc = *self.xloc
    IF Arg_Present(yloc)        NE 0 AND N_Elements(*self.yloc)        NE 0 THEN yloc = *self.yloc
    IF Arg_Present(text)        NE 0 AND N_Elements(*self.text)        NE 0 THEN text = *self.text
    IF Arg_Present(alignment)   NE 0 AND N_Elements(*self.alignment)   NE 0 THEN alignment = *self.alignment
    IF Arg_Present(charsize)    NE 0 AND N_Elements(*self.charsize)    NE 0 THEN charsize = *self.charsize
    IF Arg_Present(charthick)   NE 0 AND N_Elements(*self.charthick)   NE 0 THEN charthick = *self.charthick
    IF Arg_Present(clip)        NE 0 AND N_Elements(*self.clip)        NE 0 THEN clip = *self.clip
    IF Arg_Present(color)       NE 0 AND N_Elements(*self.color)       NE 0 THEN color = *self.color
    IF Arg_Present(data)        NE 0 AND N_Elements(*self.data)        NE 0 THEN data = *self.data
    IF Arg_Present(device)      NE 0 AND N_Elements(*self.device)      NE 0 THEN device = *self.device
    IF Arg_Present(font)        NE 0 AND N_Elements(*self.font)        NE 0 THEN font = *self.font
    IF Arg_Present(noclip)      NE 0 AND N_Elements(*self.noclip)      NE 0 THEN noclip = *self.noclip
    IF Arg_Present(normal)      NE 0 AND N_Elements(*self.normal)      NE 0 THEN normal = *self.normal
    IF Arg_Present(orientation) NE 0 AND N_Elements(*self.orientation) NE 0 THEN orientation = *self.orientation
    IF Arg_Present(outloc)      NE 0 AND N_Elements(*self.outloc)      NE 0 THEN outloc = *self.outloc
    IF Arg_Present(t3d)         NE 0 AND N_Elements(*self.t3d)         NE 0 THEN t3d = *self.t3d
    IF Arg_Present(text_axes)   NE 0 AND N_Elements(*self.text_axes)   NE 0 THEN text_axes = *self.text_axes
    IF Arg_Present(tt_font)     NE 0 AND N_Elements(*self.tt_font)     NE 0 THEN tt_font = *self.tt_font
    IF Arg_Present(width)       NE 0 AND N_Elements(*self.width)       NE 0 THEN width = *self.width
    IF Arg_Present(z)           NE 0 AND N_Elements(*self.z)           NE 0 THEN z = *self.z

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
    
    IF N_Elements(extra) GT 0 THEN self -> GetProperty, _STRICT_EXTRA=extra
END


;+
;   This method sets the properties of the object.
;
; :Keywords:
;     xloc: in, required, type=depends
;          The X location of the text. 
;     yloc: in, required, type=depends
;          The Y location of the text. 
;     string: in, optional, type=string
;          The text to output. By default, the calling sequence of the program.
;     alignment: in, optional, type=integer, 
;          Set this keyword to indicate the alignment of the text with respect to the
;          x and y location. 0 is left aligned, 0.5 is centered, and 1.0 is right aligned.
;          The alignment is set to 0.5 if PLACE is set and ALIGNMENT is unspecified. 
;          Otherwise, the default is 0.
;     charsize: in, optional, type=float, 
;          The character size for axes annotations. Uses cgDefCharSize to select default
;          character size, unless !P.Charsize is set, in which case !P.Charsize is always used.
;     charthick: in, optional, type=float
;          The line thickness of the vector drawn font characters, from 0.0-1.0
;     clip: in, optional, type=fltarr(4)
;          The coordinates of a rectangle used to clip the graphics output.
;     color: in, optional, type=string/integer/long, 
;          The color of the text. Color names are those used with cgColor. 
;     data: in, optional, type=boolean
;          Set this keyword to indicate xloc and yloc are in data coordinates. Data coordinates
;          are the default, unless DEVICE or NORMAL is set.
;     device: in, optional, type=boolean
;          Set this keyword to indicate xloc and yloc are in device coordinates.
;     draw: in, optional, type=integer, 
;          Draw the text immediately.
;     font: in, optional, type=integer, 
;          The type of font desired. By default, !P.Font.
;     target: in, optional, type=object
;          The graphics object whose data coordinates are to be used in converting to and
;          from data coordinates.
;     map_object: in, optional, type=object
;          If you are drawing on a map projection set up with Map_Proj_Init
;          and using projected meter space, rather than lat/lon space, then you can use this
;          keyword to provide a cgMap object that will allow you to convert the `x` and `y`
;          parameters from longitude and latitude, respectively, to projected meter space
;          before drawing.
;     noclip: in, optional, type=boolean
;          Set this keyword to suppress clipping of the plot.
;     normal: in, optional, type=boolean
;          Set this keyword to indicate xloc and yloc are in normalized coordinates.
;     orientation: in, optional, type=float, 
;          Use this keyword to specify the counterclockwise angle of rotation of the text
;          in degrees from the horizontal.
;     outloc: out, optional, type=various
;          Only used if STRING and PLACE are set, this is a two-element array containing
;          the xloc and yloc of the cursor position in the window.
;     place: in, optional, type=boolean
;          Use this keyword to reposition the text. If `STRING` is also in use, the
;          text displayed with change accordingly.
;     t3d: in, optional, type=boolean
;          Set this keyword to indicate that the generalized transformation matrix in
;          !P.T is to be used.
;     text_axes: in, optional, type=integer
;          This keyword specifies the plane of vector drawn text when three-dimensional
;          plotting is enabled.
;     tt_font: in, optional, type=string
;         The true-type font to use for the text. Only used if FONT=1.
;     width: out, optional, type=float
;         Set this keyword to a named variable in which to return the width of the text string, 
;         in normalized coordinate units. Note that output keyword values cannot be returned
;         from the routine if the command is being executed in a cgWindow. Returned only
;         if `STRING` and `PLACE` are used.
;     z: in, optional, type=float
;         The Z coordinate if a Z parameter is not present in the call.
;-
PRO weText::SetProperty, $
XLOC=xloc, $
YLOC=yloc, $
STRING=text, $
ALIGNMENT=alignment, $
CHARSIZE=charsize, $
CHARTHICK=charthick, $
CLIP=clip, $
COLOR=color, $
DATA=data, $
DEVICE=device, $
DRAW=draw, $
FONT=font, $
TARGET=target, $
MAP_OBJECT=map_object, $
NOCLIP=noclip, $
NORMAL=normal, $
ORIENTATION=orientation, $
OUTLOC=outloc, $
PLACE=place, $
T3D=t3d, $
TEXT_AXES=text_axes, $
TT_FONT=tt_font, $
WIDTH=width, $
Z=z, $
_REF_EXTRA=extra
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    IF N_Elements(xloc)        NE 0 THEN *self.xloc = xloc
    IF N_Elements(yloc)        NE 0 THEN *self.yloc = yloc
    IF N_Elements(text)        NE 0 THEN *self.text = text
    IF N_Elements(alignment)   NE 0 THEN *self.alignment = alignment
    IF N_Elements(charsize)    NE 0 THEN *self.charsize = charsize
    IF N_Elements(charthick)   NE 0 THEN *self.charthick = charthick
    IF N_Elements(clip)        NE 0 THEN *self.clip = clip
    IF N_Elements(color)       NE 0 THEN *self.color = color
    IF N_Elements(data)        NE 0 THEN *self.data = data
    IF N_Elements(device)      NE 0 THEN *self.device = device
    IF N_Elements(font)        NE 0 THEN *self.font = font
    IF N_Elements(noclip)      NE 0 THEN *self.noclip = noclip
    IF N_Elements(normal)      NE 0 THEN *self.normal = normal
    IF N_Elements(orientation) NE 0 THEN *self.orientation = orientation
    IF N_Elements(t3d)         NE 0 THEN *self.t3d = t3d
    IF N_Elements(text_axes)   NE 0 THEN *self.text_axes = text_axes
    IF N_Elements(tt_font)     NE 0 THEN *self.tt_font = tt_font
    IF N_Elements(z)           NE 0 THEN *self.z = z
    
    IF N_Elements(map_obj) NE 0 THEN IF Obj_Valid(map_object) $
        THEN self.map_object = map_object $
        ELSE self.map_object = Obj_New()
    
    IF N_Elements(target) NE 0 THEN IF Obj_Valid(target) $
        THEN self.target = target $
        ELSE self.target = Obj_New()

    IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra

    ;Draw the text?
    IF Keyword_Set(Place) THEN self -> Draw, text, PLACE=place, OUTLOC=outloc, WIDTH=width
    IF Keyword_Set(draw) and ~Keyword_Set(Place) then self -> Draw
END


;+
; The clean-up routine for the object. Destroy pointers, etc.
;-
PRO weText::cleanup
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
    Ptr_Free, self.text
    Ptr_Free, self.alignment
    Ptr_Free, self.charsize
    Ptr_Free, self.charthick
    Ptr_Free, self.text_axes
    Ptr_Free, self.width
    Ptr_Free, self.clip
    Ptr_Free, self.color
    Ptr_Free, self.data
    Ptr_Free, self.device
    Ptr_Free, self.normal
    Ptr_Free, self.font
    Ptr_Free, self.orientation
    Ptr_Free, self.outloc
    Ptr_Free, self.noclip
    Ptr_Free, self.t3d
    Ptr_Free, self.tt_font
    Ptr_Free, self.width
    Ptr_Free, self.z
    
    ;Destroy Objects
    IF Obj_Valid(self.map_object) THEN Obj_Destroy, self.map_object
    
    self -> MrGrAtom::CleanUp
END


;+
;   Provides a device-independent and color-model-independent way to write text into
;   a graphics window. It is a wrapper to the XYOUTS command.
;
; :Params:
;    xloc: in, required, type=depends
;       The X location of the text. 
;    yloc: in, required, type=depends
;       The Y location of the text. 
;    text: in, optional, type=string
;        The text to output. By default, the calling sequence of the program.
;
; :Keywords:
;     alignment: in, optional, type=integer, default=0
;         Set this keyword to indicate the alignment of the text with respect to the
;         x and y location. 0 is left aligned, 0.5 is centered, and 1.0 is right aligned.
;         The alignment is set to 0.5 if PLACE is set and ALIGNMENT is unspecified. 
;         Otherwise, the default is 0.
;     charsize: in, optional, type=float, default=cgDefCharSize()
;         The character size for axes annotations. Uses cgDefCharSize to select default
;         character size, unless !P.Charsize is set, in which case !P.Charsize is always used.
;     color: in, optional, type=string/integer/long, default="opposite"
;         The color of the text. Color names are those used with cgColor. 
;     data: in, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in data coordinates. Data coordinates
;         are the default, unless DEVICE or NORMAL is set.
;     device: in, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in device coordinates.
;     font: in, optional, type=integer, default=!P.Font
;         The type of font desired. By default, !P.Font.
;     target: in, optional, type=object, default=obj_new()
;         If `data` is set, then the graphics object whose data coordinates are to be
;         used in converting to and from data coordinates. The graphics object must have a
;         ConvertCoord method. Note that the graphics object will not be destroyed when
;         the text object is destroyed.
;     draw: in, optional, type=integer, default=0
;         Draw the text immediately.
;     map_object: in, optional, type=object, default=obj_new()
;        If you are drawing on a map projection set up with Map_Proj_Init
;        and using projected meter space, rather than lat/lon space, then you can use this
;        keyword to provide a cgMap object that will allow you to convert the `x` and `y`
;        parameters from longitude and latitude, respectively, to projected meter space
;        before drawing.
;     normal: in, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in normalized coordinates.
;     orientation: in, optional, type=float, default=0.0
;         Use this keyword to specify the counterclockwise angle of rotation of the text
;         in degrees from the horizontal.
;     outloc: out, optional, type=various
;         Only used if PLACE is set, this is a two-element array containing the xloc and yloc
;         of the cursor position in the window.
;     place: in, optional, type=boolean
;          Set this keyword if you wish to click the cursor in the graphics window to place
;          the text. If this keyword is set, you do not need to specify the `xloc` and `yloc`
;          positional parameters. The first positional parameter is assumed to be the text.
;          The clicked location will be returned in the `OutLoc` variable. If the `Alignment`
;          keyword is not set, it will be set to 0.5 to set "center" as the default placement
;          alignment. This has been modified to allow this keyword to work in a resizeable
;          graphics window as well. Clicking once in the window will set the parameters so 
;          you don't have to click every time the window is resized.
;     tt_font: in, optional, type=string
;         The true-type font to use for the text. Only used if FONT=1.
;     width: out, optional, type=float
;         Set this keyword to a named variable in which to return the width of the text string, 
;         in normalized coordinate units. Note that output keyword values cannot be returned
;         from the routine if the command is being executed in a cgWindow.
;     _ref_extra: in, optional
;         Any keywords appropriate for the XYOutS command or cgText is also accepted
;         by keyword inheritance
;-
FUNCTION weText::init, xloc, yloc, text, $
ALIGNMENT=alignment, $
CHARSIZE=charsize, $
COLOR=color, $
CURRENT=current, $
DATA=data, $
DEVICE=device, $
DRAW=draw, $
FONT=font, $
TARGET=target, $
MAP_OBJECT=map_object, $
NORMAL=normal, $
ORIENTATION=orientation, $
OUTLOC=outloc, $
PLACE=place, $
TT_FONT=tt_font, $
WIDTH=width, $
_REF_EXTRA=extra
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN, 0
    ENDIF
    
    ;If PLACE is set, then TEXT is the first parameter.
    IF Keyword_Set(place) THEN BEGIN
        textStr = xloc
    
    ENDIF ELSE BEGIN
        ; If the text is specified as the first parameter, move things around.
        IF Size(xloc, /TNAME) EQ 'STRING' THEN BEGIN
            temp = xloc
            x = yloc
            y = text
            textStr = temp
        ENDIF ELSE BEGIN
            x = xloc
            y = yloc
            textStr = text
        ENDELSE
    ENDELSE
    
    ;Set defaults
    SetDefaultValue, place, 0, /BOOLEAN
    SetDefaultValue, draw, place, /BOOLEAN

    ;Make pointers valid
    self.xloc = Ptr_New(/ALLOCATE_HEAP)
    self.yloc = Ptr_New(/ALLOCATE_HEAP)
    self.text = Ptr_New(/ALLOCATE_HEAP)
    self.alignment = Ptr_New(/ALLOCATE_HEAP)
    self.charsize = Ptr_New(/ALLOCATE_HEAP)
    self.charthick = Ptr_New(/ALLOCATE_HEAP)
    self.clip = Ptr_New(/ALLOCATE_HEAP)
    self.color = Ptr_New(/ALLOCATE_HEAP)
    self.data = Ptr_New(/ALLOCATE_HEAP)
    self.device = Ptr_New(/ALLOCATE_HEAP)
    self.font = Ptr_New(/ALLOCATE_HEAP)
    self.target = Obj_New()
    self.map_object = Obj_New()
    self.noclip = Ptr_New(/ALLOCATE_HEAP)
    self.normal = Ptr_New(/ALLOCATE_HEAP)
    self.orientation = Ptr_New(/ALLOCATE_HEAP)
    self.outloc = Ptr_New(/ALLOCATE_HEAP)
    self.t3d = Ptr_New(/ALLOCATE_HEAP)
    self.text_axes = Ptr_New(/ALLOCATE_HEAP)
    self.tt_font = Ptr_New(/ALLOCATE_HEAP)
    self.width = Ptr_New(/ALLOCATE_HEAP)
    self.z = Ptr_New(/ALLOCATE_HEAP)

    ;If REFRESH=1 three things happen: If the call to MrGrAtom is
    ;   1. before here, none of the pointers are valid and calls to SetProperty by MrGrAtom
    ;      cause "Invalid pointer" errors.
    ;   2. here, then, when MrGrAtom::_SetWindow creates a window, MrPlotManager will call
    ;      the SetProperty method, which in turn calls the self.window -> Draw method.
    ;      Since the data properties have not yet been set, an error will occur when trying
    ;      to display it.
    ;   3. after the call to SetProperty so that all of the data is loaded, the initial
    ;      call to SetProperty will not have a valid self.window property. This is a
    ;      problem because at the end of SetProperty, self.window -> Draw is called.
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
    
    ;Set object properties
    self -> SetProperty, XLOC=x, $
                         YLOC=y, $
                         STRING=textStr, $
                         ALIGNMENT=alignment, $
                         CHARSIZE=charsize, $
                         COLOR=color, $
                         DATA=data, $
                         DEVICE=device, $
                         FONT=font, $
                         TARGET=target, $
                         MAP_OBJECT=map_object, $
                         NORMAL=normal, $
                         ORIENTATION=orientation, $
                         TT_FONT=tt_font, $
                         _STRICT_EXTRA=extra

    ;Refresh the graphics?
    if keyword_set(current) $
        then theWin -> Refresh, DISABLE=~init_refresh $
        else self -> Refresh
                         
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
;       t3d:            Transform to 3D space.
;       text_axes:      
;       tt_font:        TrueType font to use.
;       z:              
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO weText__define, class
    
    class = { weText, $
              inherits MrGrAtom, $          ;For dot-referencing in IDL>=8.0
              
              xloc: Ptr_New(), $
              yloc: Ptr_New(), $
              text: Ptr_New(), $
              target: obj_new(), $
              alignment: Ptr_New(), $
              charsize: Ptr_New(), $
              charthick: Ptr_New(), $
              clip: Ptr_New(), $
              color: Ptr_New(), $
              data: Ptr_New(), $
              device: Ptr_New(), $
              font: Ptr_New(), $
              map_object: Obj_New(), $
              noclip: Ptr_New(), $
              normal: Ptr_New(), $
              orientation: Ptr_New(), $
              outloc: Ptr_New(), $
              t3d: Ptr_New(), $
              text_axes: Ptr_New(), $
              tt_font: Ptr_New(), $
              width: Ptr_New(), $
              z: Ptr_New() $
            }
END