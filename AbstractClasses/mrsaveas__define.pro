; docformat = 'rst'
;
; NAME:
;       MrSaveAs__Define
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
;+
;   The purpose of this class is to save graphics written to the current graphics device.
;   Output types include PNG, JPEG, TIFF, GIF, PS, and PDF. An interface with the
;   various save methods can easily be made with the SaveAs_Menu. Events are handled
;   internally via the SaveAs_Events and SaveAs methods.
;
;   Note it may be necessary to have ImageMagick and GhostScript installed on your computer.
;
;   RULES FOR INHERITING:
;       This class can be inherited; however, it requires the subclass to have a Draw
;       method in order to create postscript files. This includes postscript intermediate
;       files created by ImageMagick when producing raster images (see the internal
;       Draw method below).
;
;       To create raster images, provide the window ID of the display window from which
;       the image will be read. If not provided, the window indicated by !D.Window will
;       be used (see the SetDisplayWindow method below).
;
;   RULES FOR INDEPENDENT USE:
;       For postscript output, a window object with a draw method must be provided. For
;       raster output, make sure the window you want read is the current window, or
;       supply the window ID via the INIT method or the SetProperty method.
;
;       Dialog boxes are created to help the user pick where she wants to save the file.
;       The GROUP_LEADER property allows the dialog boxes to be associated with a widget.
;
;   INCORPORATING INTO WIDGET PROGRAMS
;       To incorporate into a widget program, use the Notify_Realize event handler
;       for the Widget_Draw object. In the callback procedure, pass the draw widget's
;       widget ID to the SaveAs object via the SetProperty method.
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
;	Modification History::
;       05/17/2013  -   Written by Matthew Argall. Many methods were adapted from
;                           cgCmdWindow__Define, from the Coyote Graphics library.
;       05/20/2013  -   In the AutoRasterFile method, if winID is not currently open, 
;                           then do not use WSet to make the window current. This prevents
;                           a new, blank window from being created. - MRA
;       09/26/2013  -   Renamed from MrAbstractSaveAs to MrSaveAs. - MRA
;       2014/03/08  -   Button UValues were causing ImageMagick plots to be created in
;                           the wrong format. Fixed. - MRA
;       2014/03/13  -   Properties can now be set via the INIT method. Added the _SaveWinID
;                           and _SaveWindow properties so that the class does not have
;                           to be inherited. Added the Draw and SetDisplayWindow methods.
;                           Added more control to how buttons are made in Create_SaveAs_Menu.- MRA
;       2014/03/18  -   Added the _Group_Leader property. - MRA
;-
;*****************************************************************************************
;+
; All widget events come here and are dispatched to the proper object method.
;
; :Params:
;     event: in, required, type=structure
;         An event structure.
;-
PRO MrSaveAs_SaveAs_Events, event
    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ;Get the user value and call the appropriate event handling method.
    Widget_Control, event.id, GET_VALUE=button_name, GET_UVALUE=event_handler
    
    ;Call the proper method to handle events
    call_method, event_handler.method, event_handler.object, event
END


;+
; Provides a programmatic way to create a PostScript file from the window.
; Call by setting the CREATE_PS keyword with cgControl.
;
; :Params:
;     filename:  in, required, type=string
;         The name of the PostScript file to generate.
;-
PRO MrSaveAs::AutoPostScriptFile, filename

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        
        ; Close the PostScript file.
        cgPS_Close, /NoFix     

        ; Set the window index number back.
        IF N_Elements(currentWindow) NE 0 THEN BEGIN
            IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1
        ENDIF
        
        RETURN
    ENDIF
    
    ; Make this window the current graphics windows.
    self -> SetDisplayWindow, currentWindow

    ; Pick a file name
    IF N_Elements(filename) EQ 0 THEN filename='cgwindow.ps'

    ; Allow the user to configure the PostScript file.
    cgPS_Open, GUI=0, $
               FILENAME=filename, $
               DECOMPOSED=self.ps_decomposed, $
               EUROPEAN=self.ps_metric, $
               ENCAPSULATED=self.ps_encapsulated, $
               GROUP_LEADER=*self._group_leader, $
               SCALE_FACTOR=self.ps_scale_factor, $
               CHARSIZE=self.ps_charsize, $
               FONT=self.ps_font, $
               QUIET=self.ps_quiet, $
               TT_FONT=self.ps_tt_font
    
    ; Draw the graphics to the display window.
    self -> Draw
    
    ; Clean up.
    cgPS_Close, NOMESSAGE=self.ps_quiet

    ; Set the window index number back.
    IF currentWindow NE -1 THEN WSet, currentWindow ELSE WSet, -1

END


;+
; Provides a programmatic way to create a raster file from the window.
; Call by setting the create_png, etc. keyword with cgControl.
;
; :Params:
;     filetype:  in, required, type=string
;         The type of raster file (e.g., PNG, JPEG, etc.).
;     filename:  in, required, type=string
;         The name of the output file.
;-
PRO MrSaveAs::AutoRasterFile, filetype, filename

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        
        ; Close the PostScript file.
        cgPS_Close, /NoFix     

        ; Set the window index number back.
        IF N_Elements(currentWindow) NE 0 THEN BEGIN
            IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1
        ENDIF
        
        RETURN
    ENDIF
    
    ; Make this window the current graphics windows.
    self -> SetDisplayWindow, currentWindow
    if currentWindow eq -1 then return

    IF N_Elements(filetype) EQ 0 then filetype = 'PNG'
    IF N_Elements(filename) EQ 0 THEN filename = 'cgWindow.' + StrLowCase(filetype)
    IF StrUpCase(filetype) EQ 'PDF' THEN rastertype = -1 ELSE rastertype = self.im_raster
    
    ; Strip the extension off the filename.
    outname = cgRootName(filename, DIRECTORY=dirName)
    
    ; Put it back together without an extension.
    outputFilename = Filepath(ROOT_DIR=dirName, outname)

    ; What kind of raster file.
    CASE rasterType OF
    
        ; PDF File.
       -1: BEGIN
       
           thisname = outputFilename + '.ps'
           outname = outputFilename + '.pdf'
           cgPS_Open, $
                DECOMPOSED=self.ps_decomposed, $
                FILENAME=thisname, $
                GROUP_LEADER=*self._group_leader, $
                METRIC=self.ps_metric, $
                KEYWORDS=keywords, $ ; Returned cgPS_Config keywords.
                SCALE_FACTOR=self.ps_scale_factor, $
                CHARSIZE=self.ps_charsize, $
                FONT=self.ps_font, $
                QUIET=1, $
                TT_FONT=self.ps_tt_font
                           
           ; Draw the graphics.
           self -> Draw
           
           ; Close the file and make a PDF file.
           cgPS_Close
           cgPS2PDF, thisname, outname, DELETE_PS=self.ps_delete, /SILENT, SUCCESS=success, $
              UNIX_CONVERT_CMD=self.pdf_unix_convert_cmd, GS_PATH=self.pdf_path
           IF ~success THEN BEGIN
              Message, 'Unable to create PDF file. See cgPS2PDF documentation.'
           ENDIF ELSE BEGIN
              IF ~self.ps_quiet THEN Print, 'PDF output will be created here: ' + outname
           ENDELSE
           ENDCASE
    
        ; Normal raster.
        0: BEGIN

           void = cgSnapshot(TYPE=fileType, FILENAME=outputFilename, /NODIALOG)
           Print, 'Output file is located here: ' + outputFilename 
           ENDCASE
           
        ; Raster via ImageMagick.
        1: BEGIN
        
           ; Create a PostScript file first.
           thisname = outputFilename + '.ps'
           cgPS_Open, $
                DECOMPOSED=self.ps_decomposed, $
                FILENAME=thisname, $
                GROUP_LEADER=*self._group_leader, $
                METRIC=self.ps_metric, $
                KEYWORDS=keywords, $ ; Returned cgPS_Config keywords.
                SCALE_FACTOR=self.ps_scale_factor, $
                CHARSIZE=self.ps_charsize, $
                FONT=self.ps_font, $
                QUIET=1, $
                TT_FONT=self.ps_tt_font
                
           ; Cannot successfully convert encapsulated landscape file to raster.
           ; Limitation of ImageMagick, and specifically, GhostScript, which does
           ; the conversion.
           IF keywords.encapsulated && keywords.landscape THEN BEGIN
                Message, 'ImageMagick cannot successfully convert an encapsulated ' + $
                         'PostScript file in landscape mode to a raster file. Returning...'
           ENDIF
           
           ; Draw the graphics.
           self -> Draw

           ; Close the file and convert to proper file type.
            CASE filetype OF
                'BMP':  cgPS_Close, /BMP, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width, NOMESSAGE=self.ps_quiet
                'GIF':  cgPS_Close, /GIF, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width, NOMESSAGE=self.ps_quiet
                'JPEG': cgPS_Close, /JPEG, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width, NOMESSAGE=self.ps_quiet
                'PNG':  cgPS_Close, /PNG,  DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width, NOMESSAGE=self.ps_quiet
                'TIFF': cgPS_Close, /TIFF, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width, NOMESSAGE=self.ps_quiet
           ENDCASE
           ;IF ~self.ps_quiet THEN Print, 'Output file is located here: ' + outfilename
           ENDCASE
    
    ENDCASE

    ; Set the window index number back.
    IF currentWindow NE -1 THEN WSet, currentWindow ELSE WSet, -1
END


;+
;   Create buttons of various save options. If the MENU keyword is not set, all of the
;   buttons will have `PARENT` as the parent widget. If the MENU keyword is selected, the
;   'Save As' menu will look like::
;
;       [Save As]
;           JPEG
;           TIFF
;           PNG
;           GIF
;           [cgSaveAs]
;               PostScript File
;               PDF File
;               [Raster Image File]
;                   cg JPEG
;                   cg TIFF
;                   cg PNG
;                   cg GIF
;               [Raster Image File via ImageMagick]
;                   IM JPEG
;                   IM TIFF
;                   IM PNG
;                   IM GIF
;
; :Params:
;       PARENT:             in, required, type=integer
;                           The widget ID of the parent widget for the new SaveAs Menu.
;
; :Keywords:
;       MENU:               in, optional, type=boolean, default=0
;                           If set, all buttons will be placed under a "SaveAs" submenu.
;       PS_PDF:             in, optional, type=boolean, default=0
;                           If set, PS and PDF buttons will be created. Saving is performed
;                               with the use of cgPS_Open and cgPS_Close.
;       MrRaster:           in, optional, type=boolean, default=0
;                           If set, JPEG, PNG, TIFF, and GIF buttons will be created.
;                               Saving is performed with MrScreenCapture.
;       cgRaster:           in, optional, type=boolean, default=0
;                           If set, JPEG, PNG, TIFF, and GIF buttons will be created.
;                               Saving is performed either with cgSnapShot or via
;                               ImageMagick and post-script intermediary files (see PS_PDF).
;       imRaster:           in, optional, type=boolean, default=0
;                           If set, JPEG, PNG, TIFF, and GIF buttons will be created.
;                               Saving is performed via ImageMagick and post-script
;                               intermediary files (see PS_PDF).
;-
pro MrSaveAs::Create_SaveAs_Menu, parent, $
MENU=menu, $
PS_PDF=ps_pdf, $
MRRASTER=MrRaster, $
CGRASTER=cgRaster, $
IMRASTER=imRaster
	Compile_Opt strictarr
	
	catch, theError
	IF theError NE 0 THEN BEGIN
	    catch, /cancel
	    void = cgErrorMsg()
	    RETURN
	ENDIF
	
	;Defaults
	menu = keyword_set(menu)
	MrRaster = keyword_set(MrRaster)
	cgRaster = keyword_set(cgRaster)
	imRaster = keyword_set(imRaster)
	ps_pdf   = keyword_set(ps_pdf)
	
	IF MrRaster + cgRaster + imRaster + ps_pdf EQ 0 THEN BEGIN
	    MrRaster = 1
	    cgRaster = 1
	    imRaster = 1
	    ps_pdf   = 1
	ENDIF

;---------------------------------------------------------------------
;MrRaster ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Create a "SaveAs" menu bar button?
    IF menu $
        THEN saveID = widget_button(parent, VALUE='SaveAs', /MENU) $
        ELSE saveID = parent
	
	;MrSaveAs options
    IF MrRaster EQ 1 THEN BEGIN
        button = widget_button(saveID, VALUE='JPEG', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'Screen_Capture'})
        button = widget_button(saveID, VALUE='TIFF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'Screen_Capture'})
        button = widget_button(saveID, VALUE='PNG',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'Screen_Capture'})
        button = widget_button(saveID, VALUE='GIF',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'Screen_Capture'})
    ENDIF

;---------------------------------------------------------------------
;PS & PDF ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Create the "cgSaveAs" menu button
    IF menu AND ps_pdf + cgRaster + imRaster GT 0 $
        THEN cgSaveAsID = widget_button(saveID, VALUE='cgSaveAs', MENU=menu) $
        ELSE cgSaveAsID = saveID

    ;Put in the PS and PDF options.
    IF ps_pdf THEN BEGIN
        button = Widget_Button(cgSaveAsID, Value='PostScript File', UNAME='POSTSCRIPT', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'CreatePostscriptFile'})
        button = Widget_Button(cgSaveAsID, Value='PDF File',        UNAME='PDF',        EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsRaster'})
    ENDIF
    
;---------------------------------------------------------------------
;cgRaster ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    IF cgRaster THEN BEGIN
        ;cgRaster submenu
        IF menu $
            THEN cgRasterID = Widget_Button(cgSaveAsID, Value='Raster File', MENU=menu) $
            ELSE cgRasterID = cgSaveAsID
    
        ;cgRaster options
        button = Widget_Button(cgRasterID, Value='cg BMP',  UNAME='RASTER_BMP',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsRaster'})
        button = Widget_Button(cgRasterID, Value='cg GIF',  UNAME='RASTER_GIF',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsRaster'})
        button = Widget_Button(cgRasterID, Value='cg JPEG', UNAME='RASTER_JPEG', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsRaster'})
        button = Widget_Button(cgRasterID, Value='cg PNG',  UNAME='RASTER_PNG',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsRaster'})
        button = Widget_Button(cgRasterID, Value='cg TIFF', UNAME='RASTER_TIFF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsRaster'})
    ENDIF ELSE cgRasterID = cgSaveAsID
        
;---------------------------------------------------------------------
;imRaster ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ; If you can find ImageMagick on this machine, you can convert to better
    ; looking raster files.
    IF imRaster and cgHasImageMagick() EQ 1 THEN BEGIN
        ;ImageMagick submenu
        IF menu $
            THEN imRasterID = Widget_Button(cgSaveAsID, Value='Raster File via ImageMagick', MENU=menu) $
            ELSE imRasterID = cgSaveAsID

        ;ImageMagick options
        button = Widget_Button(imRasterID, Value='IM BMP',  UNAME='IMAGEMAGICK_BMP',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsRaster'})
        button = Widget_Button(imRasterID, Value='IM GIF',  UNAME='IMAGEMAGICK_GIF',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsRaster'})
        button = Widget_Button(imRasterID, Value='IM JPEG', UNAME='IMAGEMAGICK_JPEG', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsRaster'})
        button = Widget_Button(imRasterID, Value='IM PNG',  UNAME='IMAGEMAGICK_PNG',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsRaster'})
        button = Widget_Button(imRasterID, Value='IM TIFF', UNAME='IMAGEMAGICK_TIFF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsRaster'})
    ENDIF
end


;+
; Sends the window commands to a PostScript file.
;
; :Params:
;     event: in, required, type=structure
;         An event structure.
;-
PRO MrSaveAs::CreatePostScriptFile, event
    Compile_Opt idl2
    
    ; Error handling during postscript output.
    Catch, PS_Error
    IF PS_Error NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        
        ; Close the PostScript file.
        cgPS_Close, /NoFix     

        ; Set the window index number back.
        IF N_Elements(currentWindow) NE 0 THEN BEGIN
            IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1
        ENDIF
        RETURN
    ENDIF
    
    ; Make this window the current graphics windows.
    self -> SetDisplayWindow, currentWindow
    IF currentWindow EQ -1 THEN RETURN
    
    ; Construct a file name, if you have one.
    ext = self.ps_encapsulated ? '.eps' : '.ps'
    IF self.saveFile NE "" THEN BEGIN
        filename = Filepath(ROOT_DIR=self.saveDir, self.saveFile + ext)
    ENDIF ELSE BEGIN
         CD, CURRENT=thisDir
         filename = Filepath(ROOT_DIR=thisDir, 'MrDrawWindow' + ext)
    ENDELSE

    ; Allow the user to configure the PostScript file.
    cgPS_Open, /GUI, $
               CANCEL=cancelled, $
               CHARSIZE=self.ps_charsize, $
               DECOMPOSED=self.ps_decomposed, $
               EUROPEAN=self.ps_metric, $
               ENCAPSULATED=self.ps_encapsulated, $
               FILENAME=filename, $
               FONT=self.ps_font, $
               GROUP_LEADER=*self._group_leader, $
               KEYWORDS=keywords, $
               SCALE_FACTOR=self.ps_scale_factor, $
               QUIET=self.ps_quiet, $
               TT_FONT=self.ps_tt_font
    IF cancelled THEN RETURN
    
    ; Save the name of the last output file.
    self.saveDir = File_DirName(keywords.filename)
    self.saveFile = cgRootName(keywords.filename)
    
    ; Execute the graphics commands.
    self -> Draw
    
    ; Clean up.
    cgPS_Close
    
    ; Set the window index number back.
    IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1

END


;+
;   Some methods of saving simply reqd the image from the display window. Others, such
;   as the postscript device, require the graphics to be re-drawn. In those cases, this
;   method determines how that drawing should be undertaken.
;-
pro MrSaveAs::Draw
    compile_opt strictarr
    
    ;If the class has been inherited, there is no reason to supply a window. Interal
    ;calls to the draw method will be directed to the superclass's draw method. If
    ;it has not been inherited, we need to call the window's Draw method.
    self._SaveWindow -> Draw
end


;+
; This method retrieves properties from the object.
; 
; :Keywords:
;     ADJUSTSIZE:               out, optional, type=boolean
;                               Set this keyword to adjust default character size to the
;                                   display window size.
;     IM_DENSITY:               out, optional, type=integer, default=300
;                               Set this keyword to the sampling density when ImageMagick
;                                   creates raster image file from PostScript outout.
;     IM_OPTIONS:               out, optional, type=string, default=""
;                               Set this keyword to any ImageMagick options you would like
;                                   to pass along to the ImageMagick convert command when
;                                   creating raster image files from PostScript output.
;     IM_RESIZE:                out, optional, type=integer, default=25
;                               Set this keyword to percentage that the raster image file
;                                   created my ImageMagick from PostScript output should
;                                   be resized.
;     IM_RASTER:                out, optional, type=boolean, default=1
;                               Set this keyword to zero to create raster files using the
;                                   create_png etc. keywords directly, instead of via
;                                   ImageMagick.
;     IM_TRANSPARENT:           out, optional, type=boolean, default=0
;                               Set this keyword to allow ImageMagick to create transparent
;                                   backgrounds when it makes raster image files from
;                                   PostScript output.
;     PDF_PATH:                 out, optional, type=string
;                               Set this keyword to the name of the path to the Ghostscript
;                                   command for converting PS to PDF.
;     PDF_UNIX_CONVERT_CMD:     out, optional, type=string
;                               Set this keyword to the name of an alternative UNIX command
;                                   to convert PostScript to PDF.
;     PS_CHARSIZE:              out, optional, type=float
;                               The PostScript character size.
;     PS_DECOMPOSED:            out, optional, type=boolean, default=0
;                               Set this keyword to zero to set the PostScript color mode
;                                   to indexed color and to one to set the PostScript color
;                                   mode to decomposed color.
;     PS_DELETE:                out, optional, type=boolean, default=1
;                               Set this keyword to zero if you want to keep the PostScript
;                                   output ImageMagick creates when making raster file
;                                   output.
;     PS_ENCAPSULATED:          out, optional, type=boolean, default=0
;                               Set this keyword to configure PSCONFIG to produce
;                                   encapsulated PostScript output by default.
;     PS_FONT:                  out, optional, type=integer
;                               Set this keyword to the type of font you want to use in
;                                   PostScript output. It sets the FONT keyword on the
;                                   PSConfig command. Normally, 0 (hardware fonts) or
;                                   1 (true-type fonts).
;     PS_METRIC:                out, optional, type=boolean, default=0
;                               Set this keyword to configure PSCONFIG to use metric values
;                                   and A4 page size in its interface.
;     PS_QUIET:                 out, optional, type=boolean, default=0
;                               Set this keyword to set the QUIET keyword on PS_Start.
;     PS_SCALE_FACTOR:          out, optional, type=float
;                               Set his keyword to the PostScript scale factor you wish to
;                                   use in creating PostScript output.
;     PS_TT_FONT:               out, optional, type=string
;                               Set this keyword to the name of a true-type font to use in
;                                   creating PostScript output.
;     WINID:                    out, optional, type=long
;                               ID of the display window that will be read when raster
;                                   output is generated.
;     WINDOW:                   out, optional, type=object
;                               A display window with a Draw method.
;-
PRO MrSaveAs::GetProperty, $
ADJUSTSIZE=adjustsize, $
IM_DENSITY=im_density, $                      ; Sets the density parameter on ImageMagick convert command.
IM_RESIZE=im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
IM_OPTIONS=im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
IM_RASTER=im_raster, $                        ; Sets whether to generate raster files via ImageMagick
IM_TRANSPARENT=im_transparent, $  ; Sets the "alpha" keyword on ImageMagick convert command.
PDF_PATH=pdf_path, $                          ; The path to the Ghostscript conversion command.
PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $  ; Command to convert PS to PDF.
PS_CHARSIZE=ps_charsize, $                    ; Select the character size for PostScript output.
PS_DECOMPOSED=ps_decomposed, $
PS_DELETE=ps_delete, $
PS_ENCAPSULATED=ps_encapsulated, $
PS_FONT=ps_font, $                            ; Select the font for PostScript output.
PS_METRIC=ps_metric, $
PS_SCALE_FACTOR=ps_scale_factor, $            ; Select the scale factor for PostScript output.
PS_QUIET=ps_quiet, $
PS_TT_FONT=ps_tt_font, $                      ; Select the true-type font to use for PostScript output.
WINID=winID, $
WINDOW=theWindow
    Compile_Opt strictarr
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF

    IF Arg_Present(winID)     THEN winID     = self._saveWinID
    IF Arg_Present(theWindow) THEN theWindow = self._saveWindow

    ; Window properties.
    IF Arg_Present(adjustsize) THEN adjustsize = self.adjustsize
    
    ; PDF properties.
    IF Arg_Present(pdf_unix_convert_cmd) THEN pdf_unix_convert_cmd = self.pdf_unix_convert_cmd
    IF Arg_Present(pdf_path)             THEN pdf_path             = self.pdf_path
   
    ; PostScript properties.
    IF Arg_Present(ps_charsize)     THEN ps_charsize     = self.ps_charsize
    IF Arg_Present(ps_decomposed)   THEN ps_decomposed   = self.ps_decomposed
    IF Arg_Present(ps_delete)       THEN ps_delete       = self.ps_delete
    IF Arg_Present(ps_encapsulated) THEN ps_encapsulated = self.ps_encapsulated
    IF Arg_Present(ps_metric)       THEN ps_metric       = self.ps_metric
    IF Arg_Present(ps_font)         THEN ps_font         = self.ps_font
    IF Arg_Present(ps_quiet)        THEN ps_quiet        = self.ps_quiet
    IF Arg_Present(ps_scale_factor) THEN ps_scale_factor = self.ps_scale_factor
    IF Arg_Present(ps_tt_font)      THEN ps_tt_font      = self.ps_tt_font
    
    ; ImageMagick properties.
    IF Arg_Present(im_transparent)  THEN im_transparent = self.im_transparent
    IF Arg_Present(im_density)      THEN im_density     = self.im_density
    IF Arg_Present(im_options)      THEN im_options     = self.im_options
    IF Arg_Present(im_resize)       THEN im_resize      = self.im_resize
    IF Arg_Present(im_raster)       THEN im_raster      = self.im_raster
END


;+
; This method creates PostScript, PDF, BMP, GIF, JPEG, PNG, and TIFF file output
; from the pixmap window contents. The method assumes ImageMagick and Ghostscript
; are installed correctly.
;
; :Params:
;    filename: in, optional, type=string, default='idl.ps'
;        The name of the output file. The type of file is determined from the
;        file name extension.
;-
PRO MrSaveAs::Output, filename
    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ; Need a filename?
    IF N_Elements(filename) EQ 0 THEN filename = 'MrWindow.ps'

    ; The type of file is determined by the filename extension.
    rootname = cgRootName(filename, DIRECTORY=dir, EXTENSION=ext)
    CASE StrUpCase(ext) OF
       'PS':   self -> AutoPostScriptFile, filename
       'EPS':  self -> AutoPostScriptFile, filename
       'PDF':  self -> AutoRasterFile, 'PDF', filename
       'BMP':  self -> AutoRasterFile, 'BMP', filename
       'GIF':  self -> AutoRasterFile, 'GIF', filename
       'JPG':  self -> AutoRasterFile, 'JPEG', filename
       'JPEG': self -> AutoRasterFile, 'JPEG', filename
       'PNG':  self -> AutoRasterFile, 'PNG', filename
       'TIF':  self -> AutoRasterFile, 'TIFF', filename
       'TIFF': self -> AutoRasterFile, 'TIFF', filename
       ELSE: Message, 'Unknown file type: ' + StrUpCase(ext) + '.'
    ENDCASE
END


;+
; This event handler method saves the graphics window as a raster image file.
; PDF files also pass through here.
; 
; :Params:
;     event: in, required, type=structure
;        The event structure.
;-
PRO MrSaveAs::SaveAsRaster, event
    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        
        ; Close the PostScript file.
        cgPS_Close, /NoFix     

        ; Set the window index number back.
        IF N_Elements(currentWindow) NE 0 THEN BEGIN
            IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1
        ENDIF
        
        RETURN
    ENDIF

    ; Only going in here for down event.
    IF event.select NE 1 THEN RETURN
    
    buttonValue = Widget_Info(event.id, /UNAME)
    
    ; Is this a PDF file or a raster file?
    IF buttonValue EQ 'PDF' THEN BEGIN
       rasterType = -1
       filetype = 'PDF'
    ENDIF ELSE BEGIN
    
        ; Determine if this is normal raster (0) or ImageMagick raster (1).
        IF StrMid(buttonValue, 0, 6) EQ 'RASTER' THEN BEGIN
            fileType = StrMid(buttonValue, 7)
            rasterType = 0 
        ENDIF ELSE BEGIN
            filetype = StrMid(buttonValue, 12)
            rasterType = 1
        ENDELSE
        
    ENDELSE
    
    ; Make this window the current graphics windows.
    self -> SetDisplayWindow, currentWindow
    IF currentWindow EQ -1 THEN RETURN
    
    ; Construct a file name, if you have one.
    CASE filetype OF
       'BMP':  ext = '.bmp'
       'GIF':  ext = '.gif'
       'JPEG': ext = '.jpg'
       'PDF':  ext = '.pdf'
       'PNG':  ext = '.png'
       'TIFF': ext = '.tif'
    ENDCASE
    IF self.saveFile NE "" THEN BEGIN
        filename = Filepath(ROOT_DIR=self.saveDir, self.saveFile + ext)
    ENDIF ELSE BEGIN
         CD, CURRENT=thisDir
         filename = Filepath(ROOT_DIR=thisDir, 'MrDrawWindow' + ext)
    ENDELSE
    
    ; Get a filename from the user.
    CASE filetype OF
       'BMP':  filename = cgPickfile(FILE=filename, /WRITE, TITLE='Select an Output File...')
       'GIF':  filename = cgPickfile(FILE=filename, /WRITE, TITLE='Select an Output File...')
       'JPEG': filename = cgPickfile(FILE=filename, /WRITE, TITLE='Select an Output File...')
       'PDF':  filename = cgPickfile(FILE=filename, /WRITE, TITLE='Select an Output File...')
       'PNG':  filename = cgPickfile(FILE=filename, /WRITE, TITLE='Select an Output File...')
       'TIFF': filename = cgPickfile(FILE=filename, /WRITE, TITLE='Select an Output File...')
    ENDCASE
    IF filename EQ "" THEN RETURN
    
    ; Parset the name.
    root_name = cgRootName(filename, DIRECTORY=dirName)
    outname = Filepath(ROOT_DIR=dirname, root_name)
    
    ; Save this name.
    self.saveFile = root_name
    self.saveDir = dirName

    ; What kind of raster file.
    CASE rasterType OF
    
        ; PDF File.
       -1: BEGIN
       
           thisname = outname + '.ps'
           outname = outname + '.pdf'
           cgPS_Open, $
                DECOMPOSED=self.ps_decomposed, $
                FILENAME=thisname, $
                GROUP_LEADER=*self._group_leader, $
                METRIC=self.ps_metric, $
                KEYWORDS=keywords, $ ; Returned PSConfig keywords.
                SCALE_FACTOR=self.ps_scale_factor, $
                CHARSIZE=self.ps_charsize, $
                FONT=self.ps_font, $
                QUIET=self.ps_quiet, $
                TT_FONT=self.ps_tt_font
                           
           ; Draw the graphics.
           self -> Draw
           
           ; Close the file and make a PDF file.
           cgPS_Close
           cgPS2PDF, thisname, outname, DELETE_PS=self.ps_delete, /SILENT, SUCCESS=success, $
              UNIX_CONVERT_CMD=self.pdf_unix_convert_cmd, GS_PATH=self.pdf_path
           IF ~success THEN BEGIN
              Message, 'Unable to create PDF file. See cgPS2PDF documentation.'
           ENDIF ELSE BEGIN
              IF ~self.ps_quiet THEN Print, 'PDF output will be created here: ' + outname
           ENDELSE
           ENDCASE
    
        ; Normal raster.
        0: BEGIN
           void = cgSnapshot(TYPE=fileType, FILENAME=outname, /NODIALOG)
           IF ~self.ps_quiet THEN Print, 'Output file located here: ' + outname 
           ENDCASE
           
        ; Raster via ImageMagick.
        1: BEGIN
        
           ; Create a PostScript file first.
           thisname = outname + '.ps'
           cgPS_Open, $
                DECOMPOSED=self.ps_decomposed, $
                FILENAME=thisname, $
                GROUP_LEADER=*self._group_leader, $
                METRIC=self.ps_metric, $
                KEYWORDS=keywords, $ ; Returned PSConfig keywords.
                SCALE_FACTOR=self.ps_scale_factor, $
                CHARSIZE=self.ps_charsize, $
                FONT=self.ps_font, $
                QUIET=1, $
                TT_FONT=self.ps_tt_font
                
           ; Cannot successfully convert encapsulated landscape file to raster.
           ; Limitation of ImageMagick, and specifically, GhostScript, which does
           ; the conversion.
           IF keywords.encapsulated && keywords.landscape THEN BEGIN
                Message, 'ImageMagick cannot successfully convert an encapsulated ' + $
                         'PostScript file in landscape mode to a raster file. Returning...'
           ENDIF
           
           ; Draw the graphics.
           self -> Draw

           ; Close the file and convert to proper file type.
           CASE filetype OF
                'BMP':  cgPS_Close, /BMP, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width
                'GIF':  cgPS_Close, /GIF, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width
                'JPEG': cgPS_Close, /JPEG, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width
                'PNG':  cgPS_Close, /PNG,  DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width
                'TIFF': cgPS_Close, /TIFF, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width
           ENDCASE
           IF ~self.ps_quiet THEN Print, 'Output will be created here: ' + outfilename
           ENDCASE
    
    ENDCASE
    
    ; Set the window index number back.
    IF currentWindow NE -1 THEN WSet, currentWindow ELSE WSet, -1
END


;+
;       Save the currently plotted data to a file. The file type is determined
;       from the file extension. If the extension is not recognized, a PNG file
;       will be made. Extensions recognized are::
;
;           'PNG'
;           'JPEG'
;           'GIF'
;           'TIFF'
;
; :Params:
;       FILENAME:       in, optional, type=String/structure
;                       Name of file in which image is to be saved. This can also be
;                           an event structure returned by the windows manager.
;-
pro MrSaveAs::Screen_Capture, filename, $
DIRECTORY=directory
	compile_opt strictarr
	
	catch, theError
	if theError ne 0 then begin
	    catch, /cancel
	    void = cgErrorMsg()
	    return
	endif

;---------------------------------------------------------------------
;Check Inputs ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;If an event was passed, undefine the filename so that a dialog
    ;is generated to ask for the file name
	if size(filename, /TNAME) eq 'STRUCT' then begin
	    event = temporary(filename)
	    widget_control, event.id, GET_VALUE=file_type
    endif else file_type = ''

    ;If a file name was given, figure out how to save from the extension
	if n_elements(filename) ne 0 then begin
	    void = cgRootName(filename, EXTENSION=file_type, DIRECTORY=saveDir)
	
	;If no file name was given, ask for one
	endif else begin
        ;Open to the previously chosen directory or the present working directory
        if n_elements(directory) eq 0 then $
            if self.saveDir eq '' $
                then void = cgRootName(DIRECTORY=directory) $
                else directory = self.saveDir
        
        ;Initial file name
        if self.saveFile eq '' $
            then file = 'MrWindow' $
            else file = self.saveFile
        
        ;Ask for a file name
        filename = dialog_pickfile(DIALOG_PARENT=*self._group_leader, $
                                   FILE=file, PATH=directory, GET_PATH=saveDir, $
                                   TITLE='Save Image As:', /WRITE)
        
        ;Return if cancelled
        if filename eq '' then return
        
        ;Get the file extension so we know how to save. If the extension was given, do
        ;not append another to the file name.
        void = cgRootName(filename, EXTENSION=ext)
	    if ext ne file_type and file_type ne '' then filename += '.' + strlowcase(file_type)
    endelse
    
    ;set the save directory so we can open to the same place
    self.saveDir = saveDir
    self.saveFile = file_basename(filename)

;---------------------------------------------------------------------
;Save the Dsplay /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;set the window to the current window
    self -> SetDisplayWindow, currentWindow
    if currentWindow eq -1 then return
    
    ;get the current color table and read the screen. Read a 24-bit color if the write
    ;routine accepts 24-bit images. Otherwise, read an 8-bit image using the current color
    ;table
    if keyword_set(gif) then begin
        tvlct, r, g, b, /get
        theImage = tvrd()
    endif else theImage = tvrd(TRUE=1)
	
	;write the image to a file
	case strupcase(file_type) of
	    'JPEG': write_jpeg, filename, theImage, TRUE=1
	    
	    'PNG': write_png, filename, theImage
	    
	    'TIFF': begin
            ;tvrd() scans from bottome to top. Tiff readers read top to bottom. Must reverse
            ;the vertical dimension of the image.
            theImage = reverse(theImage, 3)
            write_tiff, filename, theImage
        endcase
        
        'GIF': write_gif, filename, theImage, r, g, b
        
        ;If the extension is not known, save to a JPEG
        else: write_jpeg, filename, theImage, TRUE=1
    endcase
end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;
; :Params:
;       CURRENTWINDOW:          out, optional, type=long
;                               Window ID of the window active before calling
;                                   SetDisplayWindow.
;-
PRO MrSaveAs::SetDisplayWindow, currentWindow
    Compile_Opt strictarr
    
    ;Set the window
    ;   - Check the winID property
    ;   - See if a window is open
    IF WindowAvailable(self._saveWinID) THEN BEGIN
        currentWin = !D.Window
        wset, self._saveWinID
    ENDIF ELSE IF WindowAvailable(!D.Window) THEN BEGIN
        currentWindow = !D.Window
        ;Window is already set. Do nothing
    ENDIF ELSE BEGIN
        currentWindow = -1
        message, 'No windows are available to read.'
    ENDELSE
end


;+
; This method sets properties of the window object. 
; 
; :Keywords:
;     ADJUSTSIZE:               in, optional, type=boolean
;                               Set this keyword to adjust default character size to the
;                                   display window size.
;     GROUP_LEADER:             in, optional, type=long
;                               ID of a widget that will serve as a group leader for
;                                   any dialog pickfile boxes that appear.
;     IM_DENSITY:               in, optional, type=integer, default=300
;                               Set this keyword to the sampling density when ImageMagick
;                                   creates raster image file from PostScript outout.
;     IM_OPTIONS:               in, optional, type=string, default=""
;                               Set this keyword to any ImageMagick options you would like
;                                   to pass along to the ImageMagick convert command when
;                                   creating raster image files from PostScript output.
;     IM_RESIZE:                in, optional, type=integer, default=25
;                               Set this keyword to percentage that the raster image file
;                                   created my ImageMagick from PostScript output should
;                                   be resized.
;     IM_RASTER:                in, optional, type=boolean, default=1
;                               Set this keyword to zero to create raster files using the
;                                   create_png etc. keywords directly, instead of via
;                                   ImageMagick.
;     IM_TRANSPARENT:           in, optional, type=boolean, default=0
;                               Set this keyword to allow ImageMagick to create transparent
;                                   backgrounds when it makes raster image files from
;                                   PostScript output.
;     PDF_PATH:                 out, optional, type=string
;                               Set this keyword to the name of the path to the Ghostscript
;                                   command for converting PS to PDF.
;     PDF_UNIX_CONVERT_CMD:     out, optional, type=string
;                               Set this keyword to the name of an alternative UNIX command
;                                   to convert PostScript to PDF.
;     PS_CHARSIZE:              in, optional, type=float
;                               The PostScript character size.
;     PS_DECOMPOSED:            in, optional, type=boolean, default=0
;                               Set this keyword to zero to set the PostScript color mode
;                                   to indexed color and to one to set the PostScript color
;                                   mode to decomposed color.
;     PS_DELETE:                in, optional, type=boolean, default=1
;                               Set this keyword to zero if you want to keep the PostScript
;                                   output ImageMagick creates when making raster file
;                                   output.
;     PS_ENCAPSULATED:          in, optional, type=boolean, default=0
;                               Set this keyword to configure PSCONFIG to produce
;                                   encapsulated PostScript output by default.
;     PS_FONT:                  in, optional, type=integer
;                               Set this keyword to the type of font you want to use in
;                                   PostScript output. It sets the FONT keyword on the
;                                   PSConfig command. Normally, 0 (hardware fonts) or
;                                   1 (true-type fonts).
;     PS_METRIC:                in, optional, type=boolean, default=0
;                               Set this keyword to configure PSCONFIG to use metric values
;                                   and A4 page size in its interface.
;     PS_QUIET:                 in, optional, type=boolean, default=0
;                               Set this keyword to set the QUIET keyword on PS_Start.
;     PS_SCALE_FACTOR:          in, optional, type=float
;                               Set his keyword to the PostScript scale factor you wish to
;                                   use in creating PostScript output.
;     PS_TT_FONT:               in, optional, type=string
;                               Set this keyword to the name of a true-type font to use in
;                                   creating PostScript output.
;     WINID:                    in, optional, type=long
;                               ID of the display window that will be read when raster
;                                   output is generated. If not given, the window indicated
;                                   by !D.Window will be read.
;     WINDOW:                   in, optional, type=object
;                               A display window with a Draw method. Required for
;                                   post-script output.
;-
PRO MrSaveAs::SetProperty, $
ADJUSTSIZE=adjustsize, $       ; Adjust the default charsize to match display size.
GROUP_LEADER=group_leader, $                  ; ID of a widget to serve as the group leader
IM_DENSITY=im_density, $                      ; Sets the density parameter on ImageMagick convert command.
IM_OPTIONS=im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
IM_RASTER=im_raster, $                        ; Sets whether to use ImageMagick to create raster files.
IM_RESIZE=im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
IM_TRANSPARENT=im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
IM_WIDTH = im_width, $                        ; Sets the final width of the raster files create with ImageMagick.
PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $  ; Command to convert PS to PDF.
PDF_PATH=pdf_path, $                          ; The path to the Ghostscript conversion command.
PS_CHARSIZE=ps_charsize, $                    ; Select the character size for PostScript output.
PS_DECOMPOSED=ps_decomposed, $                ; Sets the PostScript color mode.
PS_DELETE=ps_delete, $                        ; Delete the PostScript file when making IM raster files.
PS_ENCAPSULATED=ps_encapsulated, $            ; Select encapusulated PostScript output.
PS_FONT=ps_font, $                            ; Select the font for PostScript output.
PS_METRIC=ps_metric, $                        ; Select metric measurements in PostScript output.
PS_QUIET=ps_quiet, $                          ; Select the QUIET keyword for PS_Start.
PS_SCALE_FACTOR=ps_scale_factor, $            ; Select the scale factor for PostScript output.
PS_TT_FONT=ps_tt_font, $                      ; Select the true-type font to use for PostScript output.
WINID=winID, $
WINDOW=theWindow
    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    IF N_Elements(winID)                GT 0 THEN  self._saveWinID    = winID
    IF N_Elements(theWindow)            GT 0 THEN  self._saveWindow   = theWindow
    IF N_Elements(group_leader)         GT 0 THEN *self._group_leader = group_leader
    
    IF N_Elements(adjustsize)           GT 0 THEN self.adjustsize = Keyword_Set(adjustsize)

    IF N_Elements(im_transparent)       GT 0 THEN self.im_transparent       = im_transparent
    IF N_Elements(im_density)           GT 0 THEN self.im_density           = im_density
    IF N_Elements(im_resize)            GT 0 THEN self.im_resize            = im_resize
    IF N_Elements(im_options)           GT 0 THEN self.im_options           = im_options
    IF N_Elements(im_raster)            GT 0 then self.im_raster            = im_raster
    IF N_Elements(im_width)             GT 0 then self.im_width             = im_width
    IF N_Elements(pdf_unix_convert_cmd) GT 0 THEN self.pdf_unix_convert_cmd = pdf_unix_convert_cmd
    IF N_Elements(pdf_path)             GT 0 THEN self.pdf_path             = pdf_path
    IF N_Elements(ps_decomposed)        GT 0 THEN self.ps_decomposed        = ps_decomposed
    IF N_Elements(ps_delete)            GT 0 THEN self.ps_delete            = ps_delete
    IF N_Elements(ps_metric)            GT 0 THEN self.ps_metric            = ps_metric
    IF N_Elements(ps_encapsulated)      GT 0 THEN self.ps_encapsulated      = ps_encapsulated
    IF N_Elements(ps_charsize)          GT 0 THEN self.ps_charsize          = ps_charsize
    IF N_Elements(ps_font)              GT 0 THEN self.ps_font              = ps_font
    IF N_Elements(ps_quiet)             GT 0 THEN self.ps_quiet             = ps_quiet
    IF N_Elements(ps_scale_factor)      GT 0 THEN self.ps_scale_factor      = ps_scale_factor
    IF N_Elements(ps_tt_font)           GT 0 THEN self.ps_tt_font           = ps_tt_font    
END


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrSaveAs::cleanup
    ;Do nothing
end


;+
;   The initialization method.
;
; :Params:
;     WINID:                    in, optional, type=long
;                               ID of the display window that will be read when raster
;                                   output is generated. If not given, the window indicated
;                                   by !D.Window will be read.
;     THEWINDOW:                in, optional, type=object
;                               A display window with a Draw method. Required for
;                                   post-script output.
; 
; :Keywords:
;     ADJUSTSIZE:               in, optional, type=boolean
;                               Set this keyword to adjust default character size to the
;                                   display window size.
;     GROUP_LEADER:             in, optional, type=long
;                               ID of a widget that will serve as a group leader for
;                                   any dialog pickfile boxes that appear.
;     IM_DENSITY:               in, optional, type=integer, default=300
;                               Set this keyword to the sampling density when ImageMagick
;                                   creates raster image file from PostScript outout.
;     IM_OPTIONS:               in, optional, type=string, default=""
;                               Set this keyword to any ImageMagick options you would like
;                                   to pass along to the ImageMagick convert command when
;                                   creating raster image files from PostScript output.
;     IM_RESIZE:                in, optional, type=integer, default=25
;                               Set this keyword to percentage that the raster image file
;                                   created my ImageMagick from PostScript output should
;                                   be resized.
;     IM_RASTER:                in, optional, type=boolean, default=1
;                               Set this keyword to zero to create raster files using the
;                                   create_png etc. keywords directly, instead of via
;                                   ImageMagick.
;     IM_TRANSPARENT:           in, optional, type=boolean, default=0
;                               Set this keyword to allow ImageMagick to create transparent
;                                   backgrounds when it makes raster image files from
;                                   PostScript output.
;     PDF_PATH:                 out, optional, type=string
;                               Set this keyword to the name of the path to the Ghostscript
;                                   command for converting PS to PDF.
;     PDF_UNIX_CONVERT_CMD:     out, optional, type=string
;                               Set this keyword to the name of an alternative UNIX command
;                                   to convert PostScript to PDF.
;     PS_CHARSIZE:              in, optional, type=float
;                               The PostScript character size.
;     PS_DECOMPOSED:            in, optional, type=boolean, default=0
;                               Set this keyword to zero to set the PostScript color mode
;                                   to indexed color and to one to set the PostScript color
;                                   mode to decomposed color.
;     PS_DELETE:                in, optional, type=boolean, default=1
;                               Set this keyword to zero if you want to keep the PostScript
;                                   output ImageMagick creates when making raster file
;                                   output.
;     PS_ENCAPSULATED:          in, optional, type=boolean, default=0
;                               Set this keyword to configure PSCONFIG to produce
;                                   encapsulated PostScript output by default.
;     PS_FONT:                  in, optional, type=integer
;                               Set this keyword to the type of font you want to use in
;                                   PostScript output. It sets the FONT keyword on the
;                                   PSConfig command. Normally, 0 (hardware fonts) or
;                                   1 (true-type fonts).
;     PS_METRIC:                in, optional, type=boolean, default=0
;                               Set this keyword to configure PSCONFIG to use metric values
;                                   and A4 page size in its interface.
;     PS_QUIET:                 in, optional, type=boolean, default=0
;                               Set this keyword to set the QUIET keyword on PS_Start.
;     PS_SCALE_FACTOR:          in, optional, type=float
;                               Set his keyword to the PostScript scale factor you wish to
;                                   use in creating PostScript output.
;     PS_TT_FONT:               in, optional, type=string
;                               Set this keyword to the name of a true-type font to use in
;                                   creating PostScript output.
;-
function MrSaveAs::init, winID, theWindow, $
ADJUSTSIZE=adjustsize, $                      ; Adjust the default charsize to match display size.
GROUP_LEADER=group_leader, $                  ; ID of a widget to serve as the group leader
IM_DENSITY=im_density, $                      ; Sets the density parameter on ImageMagick convert command.
IM_OPTIONS=im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
IM_RASTER=im_raster, $                        ; Sets whether to use ImageMagick to create raster files.
IM_RESIZE=im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
IM_TRANSPARENT=im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
IM_WIDTH = im_width, $                        ; Sets the final width of the raster files create with ImageMagick.
PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $  ; Command to convert PS to PDF.
PDF_PATH=pdf_path, $                          ; The path to the Ghostscript conversion command.
PS_CHARSIZE=ps_charsize, $                    ; Select the character size for PostScript output.
PS_DECOMPOSED=ps_decomposed, $                ; Sets the PostScript color mode.
PS_DELETE=ps_delete, $                        ; Delete the PostScript file when making IM raster files.
PS_ENCAPSULATED=ps_encapsulated, $            ; Select encapusulated PostScript output.
PS_FONT=ps_font, $                            ; Select the font for PostScript output.
PS_METRIC=ps_metric, $                        ; Select metric measurements in PostScript output.
PS_QUIET=ps_quiet, $                          ; Select the QUIET keyword for PS_Start.
PS_SCALE_FACTOR=ps_scale_factor, $            ; Select the scale factor for PostScript output.
PS_TT_FONT=ps_tt_font                         ; Select the true-type font to use for PostScript output.
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
     
;---------------------------------------------------------------------
;Get Default Values //////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ; Get the global defaults.
    cgWindow_GetDefs, $
;       AdjustSize = d_adjustsize, $                 ; Adjust charsize to window size.
       
       ; PDF properties.
       PDF_Unix_Convert_Cmd = d_pdf_unix_convert_cmd, $ ; Command to convert PS to PDF.
       PDF_Path             = d_pdf_path, $             ; The path to the Ghostscript conversion command.
   
       ; ImageMagick Properties.
       IM_Transparent = d_im_transparent, $         ; Sets the "alpha" keyword on ImageMagick convert command.
       IM_Density     = d_im_density, $             ; Sets the density parameter on ImageMagick convert command.
       IM_Raster      = d_im_raster, $              ; Create raster files via ImageMagick.
       IM_Resize      = d_im_resize, $              ; Sets the resize parameter on ImageMagick convert command.
       IM_Options     = d_im_options, $             ; Sets extra ImageMagick options on the ImageMagick convert command.
       IM_Width       = d_im_width, $               ; The width of the raster file output.
        
       ; PostScript properties.
       PS_Decomposed   = d_ps_decomposed, $         ; Sets the PostScript color mode.
       PS_Delete       = d_ps_delete, $             ; Delete PS file when making IM raster.
       PS_Metric       = d_ps_metric, $             ; Select metric measurements in PostScript output.
       PS_Encapsulated = d_ps_encapsulated, $       ; Create Encapsulated PostScript output.    
       PS_FONT         = d_ps_font, $               ; Select the font for PostScript output.
       PS_CHARSIZE     = d_ps_charsize, $           ; Select the character size for PostScript output.
       PS_QUIET        = d_ps_quiet, $              ; Select the QUIET keyword for PS_Start.
       PS_SCALE_FACTOR = d_ps_scale_factor, $       ; Select the scale factor for PostScript output.
       PS_TT_FONT      = d_ps_tt_font               ; Select the true-type font to use for PostScript output.
   
     
;---------------------------------------------------------------------
;Set Object Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(winID)     gt 0 then self._saveWinID  = winID
    if n_elements(theWindow) gt 0 then self._saveWindow = theWindow
    
    ;Heap Variables
    self._group_leader = ptr_new(/ALLOCATE_HEAP)
    if n_elements(group_leader) gt 0 then *self._group_leader = group_leader

;    self.adjustsize = d_adjustsize
    self.im_transparent = n_elements(im_transparent) gt 0 ? im_transparent : d_im_transparent
    self.im_density     = n_elements(im_density)     gt 0 ? im_density     : d_im_density
    self.im_options     = n_elements(im_options)     gt 0 ? im_options     : d_im_options
    self.im_raster      = n_elements(im_raster)      gt 0 ? im_raster      : d_im_raster
    self.im_resize      = n_elements(im_resize)      gt 0 ? im_resize      : d_im_resize
    self.im_width       = n_elements(im_width)       gt 0 ? im_width       : d_im_width

    self.pdf_unix_convert_cmd = n_elements(pdf_unix_convert_cmd) gt 0 ? pdf_unix_convert_cmd : d_pdf_unix_convert_cmd
    self.pdf_path             = n_elements(pdf_path)             gt 0 ? pdf_path             : d_pdf_path

    self.ps_decomposed   = n_elements(ps_decomposed)   gt 0 ? ps_decomposed   : d_ps_decomposed
    self.ps_delete       = n_elements(ps_delete)       gt 0 ? ps_delete       : d_ps_delete
    self.ps_encapsulated = n_elements(ps_encapsulated) gt 0 ? ps_encapsulated : d_ps_encapsulated
    self.ps_metric       = n_elements(ps_metric)       gt 0 ? ps_metric       : d_ps_metric
    self.ps_charsize     = n_elements(ps_charsize)     gt 0 ? ps_charsize     : d_ps_charsize
    self.ps_font         = n_elements(ps_font)         gt 0 ? ps_font         : d_ps_font
    self.ps_quiet        = n_elements(ps_quiet)        gt 0 ? ps_quiet        : d_ps_quiet
    self.ps_scale_factor = n_elements(ps_scale_factor) gt 0 ? ps_scale_factor : d_ps_scale_factor
    self.ps_tt_font      = n_elements(ps_tt_font)      gt 0 ? ps_tt_font      : d_ps_tt_font
    
    return, 1
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrSaveAs__define, class
    compile_opt strictarr

    class = {MrSaveAs, $
    
             ;Display window
             _SaveWindow:   obj_new(), $   ; Window in which graphics are displayed.
             _SaveWinID:    0L, $          ; ID of the window from which the image will be read.
             _group_leader: ptr_new(), $   ; ID of a widget to serve as a group leader.
              
             ; PostScript options.
             ps_decomposed: 0L, $          ; Sets the PostScript color mode.
             ps_delete: 0L, $              ; Delete the PS file when making IM image file.
             ps_encapsulated: 0L, $        ; Encapsulated PostScript
             ps_metric: 0L, $              ; Metric measurements in PostScript.
             ps_charsize: 0.0, $           ; The character size to use for PostScript output.
             ps_font: 0, $                 ; The PostScript font to use.
             ps_quiet: 0, $                ; Select the QUIET keyword for PS_Start.
             ps_scale_factor: 0, $         ; The PostScript scale factor.
             ps_tt_font: "", $             ; The name of a true-type font to use for PostScript output.
             
             ; PDF options.
             pdf_unix_convert_cmd: "", $   ; The name of an alternative UNIX command to convert PS to PDF.
             pdf_path: "", $               ; The name of the path to a Ghostscript conversion command.

             ; ImageMagick output parameters.
             im_transparent: 0B, $         ; Sets the "alpha" keyword on ImageMagick convert command.
             im_density: 0L, $             ; Sets the density parameter on ImageMagick convert command.
             im_resize: 0L, $              ; Sets the resize parameter on ImageMagick convert command.
             im_options: "", $             ; Sets extra ImageMagick options on the ImageMagick convert command.
             im_raster: 0L, $              ; Create raster files via ImageMagick
             im_width: 0L, $               ; Sets the width of the final raster output with ImageMagick
              
             saveFile: '', $               ;Last file saved.
             saveDir: ''}                  ;Directory in which the last file was saved. 
end