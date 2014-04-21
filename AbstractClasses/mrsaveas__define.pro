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
;   FONTS
;       `Device Fonts    <http://www.exelisvis.com/docs/Using_Device_Fonts.html>`
;       `Embedded Format <http://exelisvis.com/docs/Embedded_Formatting_Comm.html>`
;       `Hershey Fonts   <http://exelisvis.com/docs/Using_Hershey_Vector_Fon.html>`
;       `True-Type Fonts <http://www.exelisvis.com/docs/Using_TrueType_Fonts.html>`
;       `PS_Show_Fonts   <http://exelisvis.com/docs/PS_SHOW_FONTS.html>`
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       cgPickFile.pro
;       cgPS_Open.pro
;       cgPS_Close.pro
;       cgPS2PDF.pro
;       cgRootName.pro
;       cgSnapshot.pro
;       cgWindow_GetDefs.pro
;       FSC_PSConfig__Define.pro
;       isMember.pro
;       remove_tags.pro
;       WindowAvailable.pro (Coyote Graphics)
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
;       2014/04/11  -   Combine all output methods into a single Save method. Removed
;                           "cg" and "im" prefixes on buttons. Added the SaveAsEvents
;                           method. Removed the AutoPostScriptFile, AutoRasterFile, 
;                           CreatePostScriptFile, Output, and SaveAsRaster methods. Added
;                           the PS_CONFIG property for configuring the postscript device. - MRA
;       2014/04/16  -   Check the window object for a SetCurrent method when setting the
;                           display window. Check for the !cgPickFile_* system variables
;                           to point a new object to the last saved location. Double
;                           directory structures are no longer added to postscript
;                           filenames. - MRA
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
        button = Widget_Button(cgSaveAsID, Value='PostScript File', UNAME='PS',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
        button = Widget_Button(cgSaveAsID, Value='PDF File',        UNAME='PDF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
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
        button = Widget_Button(cgRasterID, Value='BMP',  UNAME='RASTER_BMP',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
        button = Widget_Button(cgRasterID, Value='GIF',  UNAME='RASTER_GIF',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
        button = Widget_Button(cgRasterID, Value='JPEG', UNAME='RASTER_JPEG', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
        button = Widget_Button(cgRasterID, Value='PNG',  UNAME='RASTER_PNG',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
        button = Widget_Button(cgRasterID, Value='TIFF', UNAME='RASTER_TIFF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
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
        button = Widget_Button(imRasterID, Value='BMP',  UNAME='IMAGEMAGICK_BMP',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
        button = Widget_Button(imRasterID, Value='GIF',  UNAME='IMAGEMAGICK_GIF',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
        button = Widget_Button(imRasterID, Value='JPEG', UNAME='IMAGEMAGICK_JPEG', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
        button = Widget_Button(imRasterID, Value='PNG',  UNAME='IMAGEMAGICK_PNG',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
        button = Widget_Button(imRasterID, Value='TIFF', UNAME='IMAGEMAGICK_TIFF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
    ENDIF
end


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
;       ADJUSTSIZE:             out, optional, type=boolean
;                               Set this keyword to adjust default character size to the
;                                   display window size.
;       IM_DENSITY:             out, optional, type=integer, default=300
;                               Set this keyword to the sampling density when ImageMagick
;                                   creates raster image file from PostScript outout.
;       IM_OPTIONS:             out, optional, type=string, default=""
;                               Set this keyword to any ImageMagick options you would like
;                                   to pass along to the ImageMagick convert command when
;                                   creating raster image files from PostScript output.
;       IM_RESIZE:              out, optional, type=integer, default=25
;                               Set this keyword to percentage that the raster image file
;                                   created my ImageMagick from PostScript output should
;                                   be resized.
;       IM_RASTER:              out, optional, type=boolean, default=1
;                               Set this keyword to zero to create raster files using the
;                                   create_png etc. keywords directly, instead of via
;                                   ImageMagick.
;       IM_TRANSPARENT:         out, optional, type=boolean, default=0
;                               Set this keyword to allow ImageMagick to create transparent
;                                   backgrounds when it makes raster image files from
;                                   PostScript output.
;       PDF_PATH:               out, optional, type=string
;                               Set this keyword to the name of the path to the Ghostscript
;                                   command for converting PS to PDF.
;       PDF_UNIX_CONVERT_CMD:   out, optional, type=string
;                               Set this keyword to the name of an alternative UNIX command
;                                   to convert PostScript to PDF.
;       PS_CHARSIZE:            out, optional, type=float
;                               The PostScript character size.
;       PS_DECOMPOSED:          out, optional, type=boolean, default=0
;                               Set this keyword to zero to set the PostScript color mode
;                                   to indexed color and to one to set the PostScript color
;                                   mode to decomposed color.
;       PS_DELETE:              out, optional, type=boolean, default=1
;                               Set this keyword to zero if you want to keep the PostScript
;                                   output ImageMagick creates when making raster file
;                                   output.
;       PS_ENCAPSULATED:        out, optional, type=boolean, default=0
;                               Set this keyword to configure PSCONFIG to produce
;                                   encapsulated PostScript output by default.
;       PS_FONT:                out, optional, type=integer
;                               Set this keyword to the type of font you want to use in
;                                   PostScript output. It sets the FONT keyword on the
;                                   PSConfig command. Normally, 0 (hardware fonts) or
;                                   1 (true-type fonts).
;       PS_KEYWORDS:            out, optional, type=structure
;                               The current set of keywords in use by cgPS_Config.
;       PS_METRIC:              out, optional, type=boolean, default=0
;                               Set this keyword to configure PSCONFIG to use metric values
;                                   and A4 page size in its interface.
;       PS_QUIET:               out, optional, type=boolean, default=0
;                               Set this keyword to set the QUIET keyword on PS_Start.
;       PS_SCALE_FACTOR:        out, optional, type=float
;                               Set his keyword to the PostScript scale factor you wish to
;                                   use in creating PostScript output.
;       PS_TT_FONT:             out, optional, type=string
;                               Set this keyword to the name of a true-type font to use in
;                                   creating PostScript output.
;       WINID:                  out, optional, type=long
;                               ID of the display window that will be read when raster
;                                   output is generated.
;       WINDOW:                 out, optional, type=object
;                               A display window with a Draw method.
;-
PRO MrSaveAs::GetProperty, $
ADJUSTSIZE=adjustsize, $
IM_DENSITY=im_density, $
IM_RESIZE=im_resize, $
IM_OPTIONS=im_options, $
IM_RASTER=im_raster, $
IM_TRANSPARENT=im_transparent, $
PDF_PATH=pdf_path, $
PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $
PS_CHARSIZE=ps_charsize, $
PS_DECOMPOSED=ps_decomposed, $
PS_DELETE=ps_delete, $
PS_ENCAPSULATED=ps_encapsulated, $
PS_FONT=ps_font, $
PS_KEYWORDS=ps_keywords, $
PS_METRIC=ps_metric, $
PS_SCALE_FACTOR=ps_scale_factor, $
PS_QUIET=ps_quiet, $
PS_TT_FONT=ps_tt_font, $
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
    
    IF Arg_Present(ps_keywords)     THEN ps_keywords = self.ps_config -> GetKeywords()
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
PRO MrSaveAs::Save, filename, $
FILETYPE=fileType, $
MATCH=match, $
PICK_FILE=pick_file
    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        
        ; Close the PostScript file.
        cgPS_Close, /NoFix     

        ; Set the window index number back.
        IF N_Elements(currentWindow) GT 0 THEN BEGIN
            IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1
        ENDIF
        
        RETURN
    ENDIF
    
    ;Defaults
    pick_file = Keyword_Set(pick_file)
    match     = (N_Elements(match) EQ 0) ? 1 : keyword_set(match)
    IF N_Elements(filename) EQ 0 THEN filename = ''
    IF N_Elements(fileType) EQ 0 THEN fileType = ''
    
    ;Set the current graphics windows.
    self -> SetDisplayWindow, currentWindow
    IF currentWindow EQ -1 THEN RETURN

;---------------------------------------------------------------------
; Automatic Output? //////////////////////////////////////////////////
;---------------------------------------------------------------------
    IF pick_file EQ 0 THEN BEGIN
        IF filename EQ '' THEN filename = FilePath(self.saveFile, ROOT_DIR=self.saveDir)
        outName = cgRootName(filename, DIRECTORY=outDir, EXTENSION=ext)
        IF fileType EQ '' $
            THEN typeOut = StrUpCase(ext) $
            ELSE typeOut = StrUpCase(fileType)

;---------------------------------------------------------------------
; Pick a File ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ENDIF ELSE BEGIN
        IF fileType EQ '' THEN void = cgRootName(self.saveFile, EXTENSION=fileType)
        typeOut = StrUpCase(fileType)
        
        ;Open the PS Config GUI
        IF typeOut EQ 'PDF' OR typeOut EQ 'PS' THEN BEGIN
            self.ps_config -> GUIFont, CANCEL=cancel, GROUP_LEADER=*self._group_leader, $
                              FONTTYPE=self.ps_font
            IF cancel THEN RETURN
        
            ;Get the device keywords.
            psKeywords = self.ps_config -> GetKeywords()
            filename = psKeywords.filename
        
        ;Standard file-selection GUI
        ENDIF ELSE BEGIN
            filename = cgPickfile(FILE=self.saveFile, /WRITE, TITLE='Select an Output File...')
            IF filename EQ '' THEN RETURN
        ENDELSE
        
        ;Parse the file name
        baseName = cgRootName(filename, DIRECTORY=outDir)
    ENDELSE

;---------------------------------------------------------------------
; Save to File ///////////////////////////////////////////////////////
;---------------------------------------------------------------------  
    ;Make sure a valid file type was given
    CASE typeOut OF
       'PS':   raster = 0B
       'EPS':  raster = 0B
       'PDF':  raster = 0B
       'BMP':  raster = 1B
       'GIF':  raster = 1B
       'JPG':  raster = 1B
       'JPEG': raster = 1B
       'PNG':  raster = 1B
       'TIF':  raster = 1B
       'TIFF': raster = 1B
       ELSE: Message, 'Unknown file type: "' + typeOut + '".'
    ENDCASE

    ;Save the file name and directory
    self.saveFile = baseName + '.' + StrLowCase(typeOut)
    self.saveDir  = outDir
    self.ps_config -> SetProperty, DIRECTORY=outDir, FILENAME=baseName + '.' + StrLowCase(typeOut)
    
    ;Create the file name without an extension
    outName = FilePath(baseName, ROOT_DIR=outDir)

;---------------------------------------------------------------------
; Postscript Device? /////////////////////////////////////////////////
;---------------------------------------------------------------------
    IF (typeOut EQ 'PDF') OR (typeOut EQ 'PS') OR self.im_raster THEN BEGIN

    ;---------------------------------------------------------------------
    ; Open Postscript File ///////////////////////////////////////////////
    ;---------------------------------------------------------------------
        ;Create a postscript intermediary file. If a raster file extension
        ;is given to cgPS_Open, the intermediary file is deleted automatically
        ;when the file is closed. I want to control this behavior, so I ask to
        ;create a postscript file first.
        fname_temp = baseName + '.ps'
        
        ;Get the configuration keywords
        ps_keys  = self.ps_config -> GetKeywords()
        isolatin = ps_keys.isolatin1
        fontsize = ps_keys.font_size
        ps_keys  = remove_tags(ps_keys, ['language_level', 'font_size', 'portrait', $
                                         'tt_font',        'isolatin1'])
        ps_keys  = create_struct(ps_keys, 'fontsize', fontsize, 'isolatin', isolatin)

        ;Open the file.
        cgPS_Open, CHARSIZE     =  self.ps_charsize, $
                   FILENAME     =       fname_temp, $
                   GROUP_LEADER = *self._group_leader, $
                   KEYWORDS     =  ps_keywords, $
                   MATCH        =  match, $
                   QUIET        =  1B, $
                   SCALE_FACTOR =  self.ps_scale_factor, $
                   TT_FONT      =  self.ps_tt_font, $
                  _EXTRA        =  ps_keys

        ;Set the device
        ps_keywords = remove_tags(ps_keywords, ['pagetype', 'fonttype'])
        device, _STRICT_EXTRA=ps_keywords

        ;Update the page size from the MATCH keyword.
        ps_keywords = remove_tags(ps_keywords, ['language_level', 'font_size', 'portrait', $
                                                'tt_font',        'isolatin1'])

        ;Remove the directory from the file name
        ps_keywords.filename = file_basename(ps_keywords.filename)
        self.ps_config -> SetProperty, _STRICT_EXTRA=ps_keywords
    ;---------------------------------------------------------------------
    ; Close Postscript Device ////////////////////////////////////////////
    ;---------------------------------------------------------------------
        CASE typeOut OF
        ;---------------------------------------------------------------------
        ; PS /////////////////////////////////////////////////////////////////
        ;---------------------------------------------------------------------
            'PS': BEGIN
                ;Write to the file.
                self -> Draw
    
                ;Close the file.
                cgPS_Close, OUTFILENAME=fOut
                outName = n_elements(fOut) gt 0 ? fOut : outName + '.ps'
            ENDCASE
            
        ;---------------------------------------------------------------------
        ; PDF ////////////////////////////////////////////////////////////////
        ;---------------------------------------------------------------------
            'PDF': BEGIN
                ;Write to the file
                self -> Draw
                
                ;Append the extension to the output file
                outName = outName + '.pdf'
                
                ; Close the file and make a PDF file.
                cgPS_Close
                cgPS2PDF, fname_temp, outName, $
                          DELETE_PS=self.ps_delete, $
                          /SILENT, $
                          SUCCESS=success, $
                          UNIX_CONVERT_CMD=self.pdf_unix_convert_cmd, $
                          GS_PATH=self.pdf_path
                
                ;Successful?
                IF ~success THEN $
                   Message, 'Unable to create PDF file. See cgPS2PDF documentation.'
            ENDCASE
            
        ;---------------------------------------------------------------------
        ; ImageMagic Raster //////////////////////////////////////////////////
        ;---------------------------------------------------------------------
            ELSE: BEGIN
                ; Cannot successfully convert encapsulated landscape file to raster.
                ; Limitation of ImageMagick, and specifically, GhostScript, which does
                ; the conversion.
                IF ps_keywords.encapsulated && ps_keywords.landscape THEN BEGIN
                    Message, 'ImageMagick cannot successfully convert an encapsulated ' + $
                             'PostScript file in landscape mode to a raster file. Returning...'
                ENDIF
                
                ;Write the graphics to the file.
                self -> Draw

                ;Close the file. The intermediate postscript file will be deleted.
                cgPS_Close, ALLOW_TRANSPARENT =  self.im_transparent, $
                            DELETE_PS         =  self.ps_delete, $
                            DENSITY           =  self.im_density, $
                            FILETYPE          =       typeOut, $
                            IM_OPTIONS        =  self.im_options, $
                            OUTFILENAME       =       fnameOut, $
                            RESIZE            =  self.im_resize, $
                            WIDTH             = *self.im_width
                
                outName = fnameOut
            ENDCASE
        ENDCASE

;---------------------------------------------------------------------
; Screen Shot ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ENDIF ELSE BEGIN
       void = cgSnapshot(TYPE=typeOut, FILENAME=outName + '.' + StrLowCase(typeOut), /NODIALOG)
    ENDELSE
    
    ;Indicate where the file was saved.
    IF ~self.ps_quiet THEN Print, typeOut + ' file located here: ' + outName
    
    ;Set the window index number back.
    IF currentWindow NE -1 THEN WSet, currentWindow ELSE WSet, -1
END


;+
; This event handler method saves the graphics window as a raster image file.
; PDF files also pass through here.
; 
; :Params:
;     event: in, required, type=structure
;        The event structure.
;-
PRO MrSaveAs::SaveAsEvents, event
    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF

    ; Only going in here for down event.
    IF event.select NE 1 THEN RETURN
    
    ;The button's user value determines the type of file to create. 
    Widget_Control, event.id, GET_VALUE=buttonValue
    uName = Widget_Info(event.id, /UNAME)
    
    ;Extract the file type from the user name
    fileType = StregEx(uName, '(RASTER_|IMAGEMAGICK_)?([A-Z]+)', /SUBEXP, /EXTRACT)
    fileType = fileType[2]

    ;Save the file
    self -> Save, FILETYPE=fileType, /PICK_FILE
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
    on_error, 2

    ;Set the window
    ;   - Check the winID property
    ;   - See if a window is open
    IF obj_valid(self._saveWindow) && obj_hasmethod(self._saveWindow, 'SetCurrent') THEN BEGIN
        currentWindow = !D.Window
        self._saveWindow -> SetCurrent
    ENDIF ELSE IF WindowAvailable(self._saveWinID) THEN BEGIN
        currentWin = !D.Window
        wset, self._saveWinID
    ENDIF ELSE IF WindowAvailable(!D.Window) THEN BEGIN
        currentWindow = !D.Window
        ;Window is already set. Do nothing
    ENDIF ELSE BEGIN
        currentWindow = -1
        message, 'No windows are available to read.'
    ENDELSE
END


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
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by cgPS_Config.pro is also accepted via
;                               keyword inheritance.
;-
PRO MrSaveAs::SetProperty, $
ADJUSTSIZE=adjustsize, $
GROUP_LEADER=group_leader, $
IM_DENSITY=im_density, $
IM_OPTIONS=im_options, $
IM_RASTER=im_raster, $
IM_RESIZE=im_resize, $
IM_TRANSPARENT=im_transparent, $
IM_WIDTH = im_width, $
PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $
PDF_PATH=pdf_path, $
PS_CHARSIZE=ps_charsize, $
PS_DECOMPOSED=ps_decomposed, $
PS_DELETE=ps_delete, $
PS_ENCAPSULATED=ps_encapsulated, $
PS_FONT=ps_font, $
PS_METRIC=ps_metric, $
PS_QUIET=ps_quiet, $
PS_SCALE_FACTOR=ps_scale_factor, $
PS_TT_FONT=ps_tt_font, $
WINID=winID, $
WINDOW=theWindow, $
_REF_EXTRA=extra
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
    
    IF N_Elements(adjustsize)           GT 0 THEN  self.adjustsize = Keyword_Set(adjustsize)

    IF N_Elements(im_transparent)       GT 0 THEN  self.im_transparent       = im_transparent
    IF N_Elements(im_density)           GT 0 THEN  self.im_density           = im_density
    IF N_Elements(im_resize)            GT 0 THEN  self.im_resize            = im_resize
    IF N_Elements(im_options)           GT 0 THEN  self.im_options           = im_options
    IF N_Elements(im_raster)            GT 0 then  self.im_raster            = im_raster
    IF N_Elements(im_width)             GT 0 then *self.im_width             = im_width
    IF N_Elements(pdf_unix_convert_cmd) GT 0 THEN  self.pdf_unix_convert_cmd = pdf_unix_convert_cmd
    IF N_Elements(pdf_path)             GT 0 THEN  self.pdf_path             = pdf_path
    
    IF N_Elements(ps_delete)            GT 0 THEN  self.ps_delete            = ps_delete
    IF N_Elements(ps_charsize)          GT 0 THEN  self.ps_charsize          = ps_charsize
    IF N_Elements(ps_quiet)             GT 0 THEN  self.ps_quiet             = ps_quiet
    IF N_Elements(ps_scale_factor)      GT 0 THEN  self.ps_scale_factor      = ps_scale_factor
    IF N_Elements(ps_tt_font)           GT 0 THEN  self.ps_tt_font           = ps_tt_font
;    IF N_Elements(ps_metric)            GT 0 THEN  self.ps_metric            = ps_metric
;    IF N_Elements(ps_font)              GT 0 THEN  self.ps_font              = ps_font
;    IF N_Elements(ps_decomposed)        GT 0 THEN  self.ps_decomposed        = ps_decomposed
;    IF N_Elements(ps_encapsulated)      GT 0 THEN  self.ps_encapsulated      = ps_encapsulated

    ;Remove duplicate keywords from the EXTRA array, if they are present
    IF N_Elements(extra) GT 0 THEN BEGIN
        tf_member = isMember(['FONTTYPE', 'TRUETYPE'], extra, /FOLD_CASE, $
                             NONMEMBER_INDS=iExtra, N_NONMEMBERS=nExtra)
        IF Max(tf_member) GT 0 THEN BEGIN
            message, 'Cannot set FONTTYPE or TRUETYPE keywords. Use PS_FONT and PS_TT_FONT instead.', /INFORMATIONAL
            IF nExtra GT 0 $
                THEN extra = extra[iExtra] $
                ELSE void  = temporary(extra)
        ENDIF
    ENDIF

    
    self.ps_config -> SetProperty, DECOMPOSED   = ps_decomposed, $
                                   ENCAPSULATED = ps_encapsulated, $
                                   FONTTYPE     = ps_font, $
                                   METRIC       = ps_metric, $
                                  _STRICT_EXTRA = extra
END


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrSaveAs::cleanup
    ptr_free, self.im_width
    obj_destroy, self.ps_config
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
ADJUSTSIZE=adjustsize, $
GROUP_LEADER=group_leader, $
IM_DENSITY=im_density, $
IM_OPTIONS=im_options, $
IM_RASTER=im_raster, $
IM_RESIZE=im_resize, $
IM_TRANSPARENT=im_transparent, $
IM_WIDTH = im_width, $
PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $
PDF_PATH=pdf_path, $
PS_CHARSIZE=ps_charsize, $
PS_DECOMPOSED=ps_decomposed, $
PS_DELETE=ps_delete, $
PS_ENCAPSULATED=ps_encapsulated, $
PS_FONT=ps_font, $
PS_METRIC=ps_metric, $
PS_QUIET=ps_quiet, $
PS_SCALE_FACTOR=ps_scale_factor, $
PS_TT_FONT=ps_tt_font
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
    cgWindow_GetDefs, AdjustSize = d_adjustsize, $
                      ; PDF properties.
                      PDF_Unix_Convert_Cmd = d_pdf_unix_convert_cmd, $
                      PDF_Path             = d_pdf_path, $
                      ; ImageMagick Properties.
                      IM_Transparent = d_im_transparent, $
                      IM_Density     = d_im_density, $
                      IM_Raster      = d_im_raster, $
                      IM_Resize      = d_im_resize, $
                      IM_Options     = d_im_options, $
                      IM_Width       = d_im_width, $
                      ; PostScript properties.
                      PS_Decomposed   = d_ps_decomposed, $
                      PS_Delete       = d_ps_delete, $
                      PS_Metric       = d_ps_metric, $
                      PS_Encapsulated = d_ps_encapsulated, $
                      PS_FONT         = d_ps_font, $
                      PS_CHARSIZE     = d_ps_charsize, $
                      PS_QUIET        = d_ps_quiet, $
                      PS_SCALE_FACTOR = d_ps_scale_factor, $
                      PS_TT_FONT      = d_ps_tt_font
     
;---------------------------------------------------------------------
;Set Object Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
    if n_elements(winID)     gt 0 then self._saveWinID  = winID
    if n_elements(theWindow) gt 0 then self._saveWindow = theWindow
    
    ;Heap Variables
    self._group_leader = ptr_new(/ALLOCATE_HEAP)
    if n_elements(group_leader) gt 0 then *self._group_leader = group_leader
    
    ;Objects
    self.ps_config = obj_new('FSC_PSConfig')

    ;Defaults
    self.adjustsize     = n_elements(adjust_size)    gt 0 ? adjustsize        : d_adjustsize
    self.im_transparent = n_elements(im_transparent) gt 0 ? im_transparent    : d_im_transparent
    self.im_density     = n_elements(im_density)     gt 0 ? im_density        : d_im_density
    self.im_options     = n_elements(im_options)     gt 0 ? im_options        : d_im_options
    self.im_raster      = n_elements(im_raster)      gt 0 ? im_raster         : d_im_raster
    self.im_resize      = n_elements(im_resize)      gt 0 ? im_resize         : d_im_resize
    self.im_width       = n_elements(im_width)       gt 0 ? ptr_new(im_width) : ptr_new(d_im_width)

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
    
    ;Default save file location
    CD, CURRENT=current
    defsysv, '!cgPickFile_LastDir', EXISTS=exists
    self.saveDir = exists ? !cgPickFile_LastDir : current + path_sep()
    
    ;Default save file name
    defsysv, '!cgPickfile_LastFile', EXISTS=exists
    self.saveFile = exists ? !cgPickFile_LastFile : 'MrWindow.png'
    
    ;PS file locations
    ps_filename = cgRootName(self.saveFile) + '.ps'
    self.ps_config -> SetProperty, DIRECTORY=self.saveDir, FILENAME='MrWindow.ps'
    
    return, 1
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;
; :Fields:
;       _SAVEWINDOW:            Window in which graphics are displayed.
;       _SAVEWINID:             ID of the window from which the image will be read.
;       _GROUP_LEADER:          ID of a widget to serve as a group leader.
;       PS_CONFIG:              A FSC_PSCONFIG object.
;       PS_DECOMPOSED:          Sets the PostScript color mode.
;       PS_DELETE:              Delete the PS file when making IM image file.
;       PS_ENCAPSULATED:        Encapsulated PostScript
;       PS_METRIC:              Metric measurements in PostScript.
;       PS_CHARSIZE:            The character size to use for PostScript output.
;       PS_FONT:                The PostScript font to use.
;       PS_QUIET:               Select the QUIET keyword for PS_Start.
;       PS_SCALE_FACTOR:        The PostScript scale factor.
;       PS_TT_FONT:             The name of a true-type font to use for PostScript output.
;       PDF_UNIX_CONVERT_CMD:   The name of an alternative UNIX command to convert PS to PDF.
;       PDF_PATH:               The name of the path to a Ghostscript conversion command.
;       IM_TRANSPARENT:         Sets the "alpha" keyword on ImageMagick convert command.
;       IM_DENSITY:             Sets the density parameter on ImageMagick convert command.
;       IM_RESIZE:              Sets the resize parameter on ImageMagick convert command.
;       IM_OPTIONS:             Sets extra ImageMagick options on the ImageMagick convert command.
;       IM_RASTER:              Create raster files via ImageMagick
;       IM_WIDTH:               Sets the width of the final raster output with ImageMagick
;       SAVEFILE:               Last file saved.
;       SAVEDIR:                Directory in which the last file was saved.
;-
pro MrSaveAs__define, class
    compile_opt strictarr

    class = {MrSaveAs, $
    
             ;Display window
             _SaveWindow:   obj_new(), $   ; Window in which graphics are displayed.
             _SaveWinID:    0L, $          ; ID of the window from which the image will be read.
             _group_leader: ptr_new(), $   ; ID of a widget to serve as a group leader.
              
             ; PostScript options.
             adjustsize:      0B, $
             ps_config:       obj_new(), $
             ps_decomposed:   0B, $
             ps_delete:       0B, $
             ps_encapsulated: 0B, $
             ps_metric:       0B, $
             ps_charsize:     0.0, $
             ps_font:         0, $
             ps_quiet:        0B, $
             ps_scale_factor: 0.0, $
             ps_tt_font:      "", $
             
             ; PDF options.
             pdf_unix_convert_cmd: "", $
             pdf_path:             "", $

             ; ImageMagick output parameters.
             im_transparent: 0B, $
             im_density:     0L, $
             im_resize:      0L, $
             im_options:     "", $
             im_raster:      0B, $
             im_width:       Ptr_New(), $
              
             saveFile:       '', $
             saveDir:        ''}
end