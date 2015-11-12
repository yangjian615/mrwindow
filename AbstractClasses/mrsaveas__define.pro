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
;       2014/05/16  -   Choosing non-ImageMagick options from the SaveAs menu now uses
;                           cgSnapShot instead of ImageMagick when IM is installed. - MRA
;       2014/08/18  -   File extension '.eps' now generates and encapsulated postscript
;                           file without having to set keywords beforehand. - MRA
;       2014/11/15  -   Added the GIF_*, PS_*, and GetFilename methods. Simplified the
;                           Save method. - MRA
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
;       BITMAP:             in, optional, type=boolean, default=0
;                           If set, `VALUE` will be interpreted as a bitmap.
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
;                               Saving is performed with cgSnapShot.
;       imRaster:           in, optional, type=boolean, default=0
;                           If set, JPEG, PNG, TIFF, and GIF buttons will be created.
;                               Saving is performed via ImageMagick and post-script
;                               intermediary files (see PS_PDF).
;       VALUE:              in, optional, type=string/bytarr, default='SaveAs'
;                           A .bmp file, an image array, or the name of the primary save
;                               button. In the former two cases, `BITMAP` must be set.
;                               This keyword is ignored if `MENU`=0.
;-
pro MrSaveAs::Create_SaveAs_Menu, parent, $
BITMAP=bitmap, $
MENU=menu, $
PS_PDF=ps_pdf, $
MRRASTER=MrRaster, $
CGRASTER=cgRaster, $
IMRASTER=imRaster, $
VALUE=value
	Compile_Opt strictarr

	catch, theError
	IF theError NE 0 THEN BEGIN
		catch, /cancel
		void = cgErrorMsg()
		RETURN
	ENDIF

	;Defaults
	menu     = keyword_set(menu)
	MrRaster = keyword_set(MrRaster)
	cgRaster = keyword_set(cgRaster)
	imRaster = keyword_set(imRaster)
	ps_pdf   = keyword_set(ps_pdf)
	if n_elements(value) eq 0 then begin
		value = 'SaveAs'
		bitmap = 0
	endif

	;If nothing was set, set everything.
	IF MrRaster + cgRaster + imRaster + ps_pdf EQ 0 THEN BEGIN
		MrRaster = 1
		cgRaster = 1
		imRaster = 1
		ps_pdf   = 1
	ENDIF

	;Check if ImageMagick is present.
	has_IM = cgHasImageMagick()

;---------------------------------------------------------------------
; Menu ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	IF menu THEN BEGIN
		;Create a SaveAs button
		saveID = widget_button(parent, VALUE=value, /MENU, BITMAP=bitmap)

		;Create MrRaster buttons
		IF MrRaster THEN BEGIN
			button = widget_button(saveID, VALUE='JPEG', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'Screen_Capture'})
			button = widget_button(saveID, VALUE='TIFF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'Screen_Capture'})
			button = widget_button(saveID, VALUE='PNG',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'Screen_Capture'})
			button = widget_button(saveID, VALUE='GIF',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'Screen_Capture'})
		ENDIF
	
		;cgSaveAs button
		IF ps_pdf + cgRaster + (imRaster AND has_IM) GT 0 THEN BEGIN
			cgSaveAsID = widget_button(saveID, VALUE='cgSaveAs', MENU=menu)
	
			;PS/PDF
			IF ps_pdf THEN BEGIN
				button = Widget_Button(cgSaveAsID, Value='PostScript File', UNAME='PS',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
				button = Widget_Button(cgSaveAsID, Value='PDF File',        UNAME='PDF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
			ENDIF
	
			;cgRaster
			IF cgRaster THEN BEGIN
				cgRasterID = Widget_Button(cgSaveAsID, Value='Raster File', MENU=menu)
				button = Widget_Button(cgRasterID, Value='BMP',  UNAME='RASTER_BMP',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
				button = Widget_Button(cgRasterID, Value='GIF',  UNAME='RASTER_GIF',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
				button = Widget_Button(cgRasterID, Value='JPEG', UNAME='RASTER_JPEG', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
				button = Widget_Button(cgRasterID, Value='PNG',  UNAME='RASTER_PNG',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
				button = Widget_Button(cgRasterID, Value='TIFF', UNAME='RASTER_TIFF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
			ENDIF

			;imRaster
			IF imRaster AND has_IM THEN BEGIN
				imRasterID = Widget_Button(cgSaveAsID, Value='Raster File via ImageMagick', MENU=menu)
				button = Widget_Button(imRasterID, Value='BMP',  UNAME='IMAGEMAGICK_BMP',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
				button = Widget_Button(imRasterID, Value='GIF',  UNAME='IMAGEMAGICK_GIF',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
				button = Widget_Button(imRasterID, Value='JPEG', UNAME='IMAGEMAGICK_JPEG', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
				button = Widget_Button(imRasterID, Value='PNG',  UNAME='IMAGEMAGICK_PNG',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
				button = Widget_Button(imRasterID, Value='TIFF', UNAME='IMAGEMAGICK_TIFF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
			ENDIF
		ENDIF
	
;---------------------------------------------------------------------
; Base ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	ENDIF ELSE BEGIN

		;MrRaster
		IF MrRaster EQ 1 THEN BEGIN
			button = widget_button(saveID, VALUE='JPEG', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'Screen_Capture'})
			button = widget_button(saveID, VALUE='TIFF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'Screen_Capture'})
			button = widget_button(saveID, VALUE='PNG',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'Screen_Capture'})
			button = widget_button(saveID, VALUE='GIF',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'Screen_Capture'})
		ENDIF

		;PS/PSF.
		IF ps_pdf THEN BEGIN
			button = Widget_Button(saveID, Value='PostScript File', UNAME='PS',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
			button = Widget_Button(saveID, Value='PDF File',        UNAME='PDF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
		ENDIF

		;cgRaster
		IF cgRaster THEN BEGIN
			button = Widget_Button(cgRasterID, Value='BMP',  UNAME='RASTER_BMP',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
			button = Widget_Button(cgRasterID, Value='GIF',  UNAME='RASTER_GIF',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
			button = Widget_Button(cgRasterID, Value='JPEG', UNAME='RASTER_JPEG', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
			button = Widget_Button(cgRasterID, Value='PNG',  UNAME='RASTER_PNG',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
			button = Widget_Button(cgRasterID, Value='TIFF', UNAME='RASTER_TIFF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
		ENDIF
	
		;ImageMagick
		IF imRaster and has_IM EQ 1 THEN BEGIN
			button = Widget_Button(imRasterID, Value='BMP',  UNAME='IMAGEMAGICK_BMP',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
			button = Widget_Button(imRasterID, Value='GIF',  UNAME='IMAGEMAGICK_GIF',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
			button = Widget_Button(imRasterID, Value='JPEG', UNAME='IMAGEMAGICK_JPEG', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
			button = Widget_Button(imRasterID, Value='PNG',  UNAME='IMAGEMAGICK_PNG',  EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
			button = Widget_Button(imRasterID, Value='TIFF', UNAME='IMAGEMAGICK_TIFF', EVENT_PRO='MrSaveAs_SaveAs_Events', UVALUE={object: self, method: 'SaveAsEvents'})
		ENDIF
	ENDELSE
end


;+
;   Some methods of saving simply reqd the image from the display window. Others, such
;   as the postscript device, require the graphics to be re-drawn. In those cases, this
;   method determines how that drawing should be undertaken.
;-
PRO MrSaveAs::Draw
	Compile_Opt strictarr

	;If the class has been inherited, there is no reason to supply a window. Interal
	;calls to the draw method will be directed to the superclass's draw method. If
	;it has not been inherited, we need to call the window's Draw method.
	IF Obj_Valid(self._SaveWindow) THEN BEGIN
		IF self._SaveWindow -> GetRefresh() $
			THEN self._SaveWindow -> Draw $
			ELSE self._SaveWindow -> Refresh
	ENDIF
END


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;
; :Params:
;       CURRENTWINDOW:          out, optional, type=long
;                               Window ID of the window active before calling
;                                   SetDisplayWindow.
;-
FUNCTION MrSaveAs::GetFilename, filename, $
BASENAME=basename, $
DIRECTORY=directory, $
EXTENSION=extension, $
PICK_FILE=pick_file, $
TYPE=type
	Compile_Opt strictarr
	on_error, 2

	;GUI to pick a file?
	pick_file = Keyword_Set(pick_file)
	
	;If a file name was given, get the directory, extension, and basename
	IF N_Elements(filename) GT 0 THEN BEGIN
		basename = cgRootName(filename, DIRECTORY=directory, EXTENSION=extension)
		IF N_Elements(type)      EQ 0 THEN type = extension
		IF N_Elements(directory) EQ 0 THEN cd, CURRENT=directory
		
	;If not filename was given, create directory, extension, and basename
	ENDIF ELSE BEGIN
		basename = cgRootName(self.saveFile, EXTENSION=extension)
		IF N_Elements(type)      EQ 0 THEN type      = extension
		IF N_Elements(directory) EQ 0 THEN directory = self.saveDir
	ENDELSE
	extension = type

	;Form a tentative output file
	output_file = type eq '' ? FilePath(ROOT_DIR=directory, basename) $
	                         : FilePath(ROOT_DIR=directory, basename + '.' + StrLowCase(type))

;---------------------------------------------------------------------
; Pick a File ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	IF pick_file THEN BEGIN
		;Default file type
		typeOut = StrUpCase(type)
	
		;Open the PS_Config GUI
		IF typeOut EQ 'PDF' || typeOut EQ 'PS' || typeOut EQ 'EPS' THEN BEGIN
			self.ps_config -> GUIFont, CANCEL       =       cancel, $
			                           GROUP_LEADER = *self._group_leader, $
			                           FONTTYPE     =  self.ps_font

			;Get the filename.    
			IF cancel THEN BEGIN
				output_file = ''
			ENDIF ELSE BEGIN
				psKeywords  = self.ps_config -> GetKeywords()
				output_file = psKeywords.filename
			ENDELSE
	
		;Standard file-selection GUI
		ENDIF ELSE BEGIN
			output_file = cgPickfile(FILE=output_file, /WRITE, TITLE='Select an Output File...')
		ENDELSE
	
		;Parse the file name
		basename = cgRootName(output_file, DIRECTORY=directory, EXTENSION=extension)
	ENDIF
	
	RETURN, output_file
END


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
IM_HEIGHT=im_height, $
IM_OPTIONS=im_options, $
IM_PNG8=im_png8, $
IM_RASTER=im_raster, $
IM_RESIZE=im_resize, $
IM_TIFF_DEPTH=im_tiff_depth, $
IM_TRANSPARENT=im_transparent, $
IM_WIDTH=im_width, $
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

	; PDF properties.
	IF Arg_Present(pdf_unix_convert_cmd) THEN pdf_unix_convert_cmd = self.pdf_unix_convert_cmd
	IF Arg_Present(pdf_path)             THEN pdf_path             = self.pdf_path

	; PostScript properties.
	IF Arg_Present(adjustsize)      THEN adjustsize      = self.adjustsize
	IF Arg_Present(ps_charsize)     THEN ps_charsize     = self.ps_charsize
	IF Arg_Present(ps_decomposed)   THEN ps_decomposed   = self.ps_decomposed
	IF Arg_Present(ps_delete)       THEN ps_delete       = self.ps_delete
	IF Arg_Present(ps_encapsulated) THEN ps_encapsulated = self.ps_encapsulated
	IF Arg_Present(ps_metric)       THEN ps_metric       = self.ps_metric
	IF Arg_Present(ps_pagetype)     THEN ps_pagetype     = self.ps_pagetype
	IF Arg_Present(ps_font)         THEN ps_font         = self.ps_font
	IF Arg_Present(ps_quiet)        THEN ps_quiet        = self.ps_quiet
	IF Arg_Present(ps_scale_factor) THEN ps_scale_factor = self.ps_scale_factor
	IF Arg_Present(ps_tt_font)      THEN ps_tt_font      = self.ps_tt_font

	; ImageMagick properties.
	IF Arg_Present(im_density)      THEN im_density     =  self.im_density
	IF Arg_Present(im_height)       THEN im_height      = *self.im_height
	IF Arg_Present(im_options)      THEN im_options     =  self.im_options
	IF Arg_Present(im_png8)         THEN im_png8        =  self.im_png8
	IF Arg_Present(im_tiff_depth)   THEN im_tiff_depth  =  self.im_tiff_depth
	IF Arg_Present(im_transparent)  THEN im_transparent =  self.im_transparent
	IF Arg_Present(im_resize)       THEN im_resize      =  self.im_resize
	IF Arg_Present(im_raster)       THEN im_raster      =  self.im_raster
	IF Arg_Present(im_width)        THEN im_width      = *self.im_width

	IF Arg_Present(ps_keywords)     THEN ps_keywords = self.ps_config -> GetKeywords()
END


;+
;   Open a multi-image GIF file.
;
; :Params:
;       FILENAME:           in, required, type=string
;                           Name of the file to which the GIF will be written.
;
; :Keywords:
;       BACKGROUND_COLOR:   in, optional, type=byte, default=0B
;                           Color table index of the color to be used as the background
;                               color of the image.
;       REPEAT_COUNT:       in, optional, type=long, default=0L
;                           Number of times the GIF should repeat when planed. 0 will
;                               produce an infinite loop.
;-
PRO MrSaveAs::GIF_Open, filename, $
BACKGROUND_COLOR=background_color, $
REPEAT_COUNT=repeat_count
	Compile_Opt strictarr
	on_error, 2
	
	;Is a GIF file already open?
	DefSysV, '!MrSaveAs_GIF', EXISTS=tf_exists
	IF tf_exists THEN BEGIN
		IF !MrSaveAs_GIF.filename NE '' THEN $
			Message, 'A GIF file is already open for writing. Use MrSaveAs::GIF_Close.'
	ENDIF ELSE BEGIN
		DefSysV, '!MrSaveAs_GIF', { MrSaveAs_GIF, $
		                            background:   0B, $
		                            count:        0L, $
		                            dimensions:   LonArr(2), $
		                            filename:     '', $
		                            frames:       Ptr_New(/ALLOCATE_HEAP), $
		                            repeat_count: 0 $
		                          }
	ENDELSE
	
	;Set the file name
	!MrSaveAs_GIF.filename     = filename
	!MrSaveAs_GIF.background   = N_Elements(background_color) EQ 0 ? 0 : background_color
	!MrSaveAs_GIF.repeat_count = N_Elements(repeat_count)     EQ 0 ? 0 : repeat_count
END


;+
;   Close the multi-image GIF file.
;-
PRO MrSaveAs::GIF_Close
	Compile_Opt strictarr
	on_error, 2
	
	;Close the GIF file
	Write_GIF, !MrSaveAs_GIF.filename, /CLOSE

	;Is a GIF file already open?
	DefSysV, '!MrSaveAs_GIF', EXISTS=tf_exists
	IF tf_exists THEN BEGIN
		Ptr_Free, !MrSaveAs_GIF.frames
		!MrSaveAs_GIF.background   = 0B
		!MrSaveAs_GIF.filename     = ''
		!MrSaveAs_GIF.count        = 0L
		!MrSaveAs_GIF.dimensions   = [0L, 0L]
		!MrSaveAs_GIF.repeat_count = 0L
		!MrSaveAs_GIF.frames       = Ptr_New(/ALLOCATE_HEAP)
	ENDIF
END


;+
;   Write to a multi-frame GIF file.
;
; :Keywords:
;       CUBE:               in, optional, type=byte
;                           Set to a number between 2 and 6 to specify the cubic method of
;                               color quantization to use. Used with 24-bit displays.
;       DITHER:             in, optional, type=boolean, default=0
;                           If set, the color quantized image will be dithered. Used only
;                               with 24-bit displays.
;       DELAY_TIME:         in, optional, type=long, default=50 or same as previous frame.
;                           Time delay between successive GIF frames. Can be different for
;                               each frame.
;       DISPOSAL_METHOD:    in, optional, type=long, default=2 or same as previous frame.
;                           Manner in which the current frame should exit when the next
;                               frame appears. Options are:
;                                   0   -   Do nothing.
;                                   1   -   Leave as is.
;                                   2   -   Erase with background color.
;                                   3   -   Undo. Restore contents from before current frame.
;       TRANSPARENT:        in, optional, type=integer, default=-1
;                           Color table index of the color to be made transparent. -1
;                               indicates no transparency.
;       USER_INPUT:         in, optional, type=boolean, default=0
;                           If set, the frame will not advance until the user tells it to
;                               (via mouse click, enter key, etc.). If `DELAY_TIME` is
;                               used, it will aslo trigger the next frame.
;-
PRO MrSaveAs::GIF_Write, $
CUBE = cube, $
DITHER = dither, $
DELAY_TIME = delay_time, $
DISPOSAL_METHOD = disposal_method, $
TRANSPARENT = transparent, $
USER_INPUT = user_input
	Compile_Opt strictarr
	on_error, 2

	;GIF must be open.
	DefSysV, '!MrSaveAs_GIF', EXISTS=tf_exists
	IF ~tf_exists THEN $
		Message, 'GIF file is not open. Use MrSaveAs::GIF_Open first.'
	IF !MrSaveAs_GIF.filename EQ '' THEN $
		Message, 'GIF file is not open. Use MrSaveAs::GIF_Open first.'
	
	;Capture the image
	theImage = cgSnapShot()
	
	;True color image?
	IF Size(theImage, /N_DIMENSIONS) EQ 3 $
		THEN truecolor = Where(Size(theImage, /DIMENSIONS) EQ 3) + 1 $
		ELSE truecolor = 0

	;True color images must be quantized to 8-bits
	IF truecolor THEN BEGIN
		CASE Keyword_Set(cube) OF
			0: image2D = Color_Quan(theImage, 1, r, g, b, Colors=colors, Dither=dither)
			1: image2D = Color_Quan(theImage, 1, r, g, b, Cube=2 > cube < 6)
		ENDCASE
	ENDIF ELSE BEGIN
		TVLCT, r, g, b, /Get
		image2D = theImage
	ENDELSE
	
	;Save the color table
	;   - Pad unused elements with 0's
	rgb_table    = BytArr(3,256)
	rgb_table[0] = Transpose([[r], [g], [b]])
	
	;First save
	count = !MrSaveAs_GIF.count
	IF count EQ 0 THEN BEGIN
		;All frames must have the same dimensions
		!MrSaveAs_GIF.dimensions = Size(image2D, /DIMENSIONS)
		
		;Frame Information
		IF N_Elements(delay_time)      EQ 0 THEN delay_time       = 50
		IF N_Elements(disposal_method) EQ 0 THEN disposal_method  = 50
		IF N_Elements(transparent)     EQ 0 THEN transparent      = -1
		if N_Elements(user_input)      EQ 0 THEN user_input       = 0B
		
		;Global Information
		background_color = !MrSaveAs_GIF.background
		repeat_count     = !MrSaveAs_GIF.repeat_count
		
	;Subsequent saves
	ENDIF ELSE BEGIN
		;Take settings from the previous frame
		IF N_Elements(delay_time)      EQ 0 THEN delay_time      = (*!MrSaveAs_GIF.frames)[count-1].delay_time
		if N_Elements(disposal_method) EQ 0 THEN disposal_method = (*!MrSaveAs_GIF.frames)[count-1].disposal_method
		IF N_Elements(transparent)     EQ 0 THEN transparent     = (*!MrSaveAs_GIF.frames)[count-1].transparent
		if N_Elements(user_input)      EQ 0 THEN user_input      = (*!MrSaveAs_GIF.frames)[count-1].user_input
		
		;Resize the image
		;	- All frames must be the same size.
		IF ~Array_Equal(!MrSaveAs_GIF.dimensions, Size(image2D, /DIMENSIONS)) THEN BEGIN
			image2D = cgImageResize(image2D, !MrSaveAs_GIF.dimensions[0], !MrSaveAs_GIF.dimensions[1])
		ENDIF
	ENDELSE
	
	;Save the frame information
	theFrame = { delay_time:      delay_time, $
	             disposal_method: disposal_method, $
	             rgb_table:       rgb_table, $
	             transparent:     transparent, $
	             user_input:      user_input  $
	           }
	
	;Append the frame to the list.
	IF !MrSaveAs_GIF.count EQ 0 $
		THEN *!MrSaveAs_GIF.frames = theFrame $
		ELSE *!MrSaveAs_GIF.frames = [*!MrSaveAs_GIF.frames, theFrame]
	
	;Write the image
	Write_GIF, !MrSaveAs_GIF.filename, image2D, r, g, b, $
	           BACKGROUND_COLOR = background_color, $
	           DELAY_TIME       = delay_time, $
	           DISPOSAL_METHOD  = disposal_method, $
	           MULTIPLE         = 1, $
	           REPEAT_COUNT     = repeat_count, $
	           TRANSPARENT      = transparent, $
	           USER_INPUT       = user_input
	
	;Increase the frame count
	!MrSaveAs_GIF.count += 1
END


;+
;   Open a PostScript file for printing
;    
; :Params:
;     FILENAME:            in, optional, type=string, default='idl.ps'
;                          The name of the PostScript file to be created. This can also be
;                              the name of a raster file (e.g., PNG, JPEG, TIFF, PDF, etc.)
;                              that you would like to have created from a PostScript 
;                              intermediate file. This requires that ImageMagick is
;                              installed correctly on your machine. If you choose this
;                              kind of filename, the intermediate PostScript file is
;                              automatically deleted.
;    
; :KEYWORDS:
;     CANCEL:               out, optional, type=boolean, default=0
;                           An output keyword that is set to 1 if the user cancelled from
;                           PS_Config. Otherwise, set to 0.
;     CHARSIZE:             in, optional, type=float
;                           If this keyword is set, the !P.Charsize variable is set to
;                               this value until cgPS_Close is called.
;     DEFAULT_THICKNESS:    in, optional, type=integer, default=3
;                           Sets the following system variables to this value while
;                               creating PostScript OUTPUT: !P.Thick, !P.CharThick,
;                               !X.Thick, !Y.Thick, !Z.Thick. These variables are
;                               returned to their original values by `cgPS_Close`. A
;                               system variable is set to this value only if it currently
;                               contains the IDL default value of 0.0. If it is set to
;                               anything else, this default thickness value is ignored.
;     DEJAVUSANS:           in, optional, type=boolean, default=0
;                           Set this keyword to select the DejaVuSans true-type font for
;                               PostScript output. This option is ONLY available in IDL
;                               8.2 or higher and/or you have installed the DejaVuSans
;                               true-type font in your font directory.
;     FONT:                 in, optional, type=integer, default=0
;                           Set this to the type of font you want. A -1 selects Hershey
;                               fonts, a 0 selects hardware fonts (Helvetica, normally),
;                               and a 1 selects a True-Type font. Set to 0 by default.
;     ENCAPSULATED:         in, optional, type=boolean, default=0
;                           Set this keyword to produce encapsulated PostScript output.
;     GUI:                  in, optional, type=boolean, default=0
;                           The default behavior is to use cgPS_Config to configure the
;                               PostScript device silently. If you wish to allow the user
;                               to interatively configure the PostScript device, set this
;                               keyword.
;     KEYWORDS:             out, optional, type=structure
;                           This output keyword contains the keyword structure returned
;                               from PS_Config.
;     LANDSCAPE:            in, optional, type=boolean, default=0
;                           Set this keyword to produce landscape PostScript output.
;     NOMATCH:              in, optional, type=boolean, default=0
;                           Normally, cgPS_Open will try to "match" the aspect ratio of
;                               the PostScript file "window" to the current display window.
;                               If this keyword is set, then this doesn't occur, giving 
;                               the user the option of specifying the size and offsets of
;                               the PostScript window directly though appropriate keywords.
;     QUIET:                in, optional, type=boolean, default=0
;                           If set, informational messages are not set.
;     SCALE_FACTOR:         in, optional, type=float, default=1.0
;                           Set this to the PostScript scale factor. By DEFAULT: 1.
;     TT_FONT:              in, optional, type=string
;                           The name of a true-type font to use. Using this keyword sets
;                               `Font` to 1.
;     _REF_EXTRA:           in, optional
;                           Any keyword appropriate for the PostScript configuration
;                               program cgPS_Config, from the Coyote Library can be used
;                               with cgPS_Open.
;-
PRO MrSaveAs::PS_Open, filename, $
CANCEL=cancelled, $
CHARSIZE=charsize, $
DEFAULT_THICKNESS=default_thickness, $
DEJAVUSANS=dejavusans, $
FONT=font , $
ENCAPSULATED=encapsulated, $
GUI=gui, $
KEYWORDS=keywords, $
LANDSCAPE=landscape, $
NOMATCH=nomatch, $
PAGETYPE=pagetype, $
QUIET=quiet, $
SET_FONT=set_font, $
SCALE_FACTOR=scale_factor, $
TT_FONT=tt_font, $
_REF_EXTRA=extra
	Compile_Opt strictarr
	on_error, 2
	
	;
	; TODO: Rethink how inputs are implemented. Specifically
	;           CHARSIZE, DEFAULT_THICKNESS, DEJAVUSANS FONT
	;           SET_FONT, TT_FONT
	;
	;       In cgPS_Open, they set system variable properties
	;       that affect the default fonts while in postscript
	;       mode. For MrGraphics, all of these properties have
	;       are provided via the graphics routines. As a
	;       result, the system variables are always over-
	;       ridden and have no effect.
	;

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	gui        =  Keyword_Set(gui)
;	dejavusans =  Keyword_Set(dejavusans)
	landscape  =  Keyword_Set(landscape)
	tf_match   = ~Keyword_Set(nomatch)
	IF N_Elements(filename)          EQ 0 THEN pick_file         =      1
	IF N_Elements(quiet)             EQ 0 THEN quiet             = self.ps_quiet
;	IF N_Elements(font)              EQ 0 THEN font              = self.ps_font
	IF N_Elements(encapsulated)      EQ 0 THEN encapsulated      = self.ps_encapsulated
;	IF N_Elements(default_thickness) EQ 0 THEN default_thickness = 3
	IF N_Elements(pagetype)          EQ 0 THEN pagetype          = self.ps_pagetype
	IF N_Elements(scale_factor)      EQ 0 THEN scale_factor      = self.ps_scale_factor
	
	;Encapsulate cannot handle landscape
	IF encapsulated THEN landscape = 0

	;
	; FONT
	;
	
	; Save the current True-Type font before entering the PostScript device.
	; Necessary for restoring it later.
;	self._ps_restore_tt_font_old = self.ps_tt_font

	; Need DejaVuSans fonts?
;	IF dejavusans && (Float(!Version.Release) GE 8.2) THEN BEGIN
;		tt_font = 'DejaVuSans'
;		font = 1
;	ENDIF
	
	;
	; I did a bad thing and made the keyword TT_FONT specify the name of a true-type font. This is
	; inconsistent with other software for setting up the PostScript device (e.g., FSC_PSConfig__Define
	; and cgPS_Config. Here I try to rectify the situation.
	;
	;    DEVICE accepts the following keywords:
	;        SET_FONT: scalar string specifying the name of the font used with TT or Hardward fonts
	;        TT_FONT:  The font set via the SET_FONT keyword is a TrueType font.
	;
	; In this method, TT_FONT is used as SET_FONT. Unmix them here.
	;
;	IF Size(tt_font, /TNAME) EQ 'STRING' THEN BEGIN
;		setfont = tt_font
;		tt_font = 1
;	ENDIF

	; We can pick TT fonts either by name with TT_FONT or by class with FONT
	;   - Make sure that both are defined consistently.
;	IF (N_Elements(setfont) EQ 0) AND (font EQ 1) THEN setfont = self.ps_tt_font
;	IF N_Elements(setfont) GT 0 THEN BEGIN
;		self._ps_restore_tt_font = setfont
;		font = 1
;	ENDIF
;	self._ps_restore_font = font

	;If no file name was given, have the user select it.
	;   - Retrieve the extension to determine if we will convert to raster.
	;   - PS_FILENAME is the intermediate PostScript file and is kept in self._ps_restore_filename
	;   - FILENAME is the real filename, and it extension is kept in self._ps_restore_rastertype
	ps_filename = self -> GetFilename( filename, $
	                                   BASENAME  = basename, $
	                                   DIRECTORY = directory, $
	                                   EXTENSION = extension, $
	                                   PICK_FILE = pick_file )
	self.saveFile = extension eq '' ? basename : basename + '.' + extension
	self.saveDir  = directory

	print_ps_location = 1
	CASE StrUpCase(extension) OF
		'PS':  print_ps_location = 0
		'EPS': print_ps_location = 0
		'':    ps_filename = Filepath(ROOT_DIR=directory, basename + '.ps')
		ELSE: BEGIN
			ps_filename = Filepath(ROOT_DIR=directory, basename + '.ps')

			; If ImageMagick is installed, then we can create the raster file directly,
			; and we can delete the intermediate PostScript file.
			IF self._has_im THEN BEGIN
				self._ps_restore_rastertype = extension
				print_ps_location           = ~self.ps_delete
			ENDIF
		ENDCASE
	ENDCASE

;-----------------------------------------------------
; SetUp \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Close the old postscript
	IF self._ps_restore_setup EQ 1 THEN self -> PS_Close, /NOFIX, /NOMESSAGE
	
	;Save setup
	self._ps_restore_setup  = 1
	self._ps_restore_device = !D.Name
;	self._ps_restore_p      = !P
;	self._ps_restore_x      = !X
;	self._ps_restore_y      = !Y
;	self._ps_restore_z      = !Z

	; Change any parameters you feel like changing.
;	IF self._ps_restore_p.thick     EQ 0 THEN !P.Thick     = default_thickness
;	IF self._ps_restore_p.charthick EQ 0 THEN !P.Charthick = default_thickness
;	IF self._ps_restore_x.thick     EQ 0 THEN !X.Thick     = default_thickness
;	IF self._ps_restore_y.thick     EQ 0 THEN !Y.Thick     = default_thickness
;	IF self._ps_restore_z.thick     EQ 0 THEN !Z.Thick     = default_thickness
	
	; Set the true-type font.
	IF self._SaveWinID EQ -1 AND ((!D.Flags AND 256) NE 0) THEN BEGIN
		Window, /FREE, /PIXMAP
		pixmap = !D.Window
	ENDIF
;	!P.Font = self._ps_restore_font
	IF N_Elements(pixmap) NE 0 THEN WDelete, pixmap

;-----------------------------------------------------
; Configure PostScript Device \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	cancelled = 0
	IF tf_match THEN BEGIN
		;Try landscape if the window is wider than it is tall.
		IF !D.X_Size GT !D.Y_Size THEN landscape = 1 ELSE landscape = 0
		IF encapsulated           THEN landscape = 0
		
		;Create a PS Window with the same dimensions as the device window
		sizes = cgPSWindow(_Extra=extra, LANDSCAPE=landscape, /SANE_OFFSETS)
		
		;Obtain a set of keywords for configuring the PS device
		keywords = cgPS_Config( _Strict_Extra=extra, $
		                        INCHES       = sizes.inches, $
		                        XSIZE        = sizes.xsize, $
		                        YSIZE        = sizes.ysize, $
		                        XOFFSET      = sizes.xoffset, $
		                        YOFFSET      = sizes.yoffset, $
		                        CANCEL       = cancelled, $
		                        NOGUI        = (~gui), $
		                        LANDSCAPE    = sizes.landscape, $
		                        ENCAPSULATED = encapsulated, $
		                        FILENAME     = file_basename(ps_filename), $
		                        DIRECTORY    = directory)

	ENDIF ELSE BEGIN
		keywords = cgPS_Config(_Strict_Extra = extra, $
		                       ENCAPSULATED  = encapsulated, $
		                       LANDSCAPE     = landscape, $
		                       CANCEL        = cancelled, $
		                       NOGUI         = (~gui), $
		                       FILENAME      = file_basename(ps_filename), $
		                       DIRECTORY     = directory)
	ENDELSE
	IF cancelled THEN BEGIN
		self -> PS_Close, /NOFIX, /NOMESSAGE
		RETURN
	ENDIF

	; Let them know where the output will be.
	IF ~quiet THEN BEGIN
		IF print_ps_location THEN Print, 'PostScript output will be created here: ', keywords.filename
	ENDIF

	;Change to the PostScrip device and configure it
	Set_Plot, 'PS'
	Device, _EXTRA=keywords, SCALE_FACTOR=scale_factor
;	IF N_Elements(setfont) NE 0 THEN Device, SET_FONT=setfont, /TT_Font

	; Determine the character size.
;	IF self._ps_restore_p.charsize EQ 0 THEN BEGIN
;		IF N_Elements(charsize) EQ 0 THEN BEGIN
;			!P.Charsize = cgDefCharsize(FONT=font)
;		ENDIF ELSE !P.Charsize = charsize
;	ENDIF ELSE BEGIN
;		IF N_Elements(charsize) NE 0 THEN !P.Charsize = charsize
;	ENDELSE

	; Store filename and other pertinent information.
	self._ps_restore_filename  = keywords.filename
	self._ps_restore_eps       = keywords.encapsulated
	self._ps_restore_landscape = Fix(keywords.landscape)
	self._ps_restore_pagetype  = pagetype
	self._ps_restore_quiet     = Fix(quiet)
END


;+
;   Close the Postscript File & Device.
;-
PRO MrSaveAs::PS_Close, $
ALLOW_TRANSPARENT=allow_transparent, $
BMP=bmp, $
DELETE_PS=delete_ps, $
DENSITY=density, $
FILETYPE=filetype, $
GIF=gif, $
GS_PATH=gs_path, $
HEIGHT=height, $
IM_OPTIONS=im_options, $
JPEG=jpeg, $
NOFIX=nofix, $
NOMESSAGE=nomessage, $
OUTFILENAME=outfilename, $
PDF=pdf, $
PNG=png, $
RESIZE=resize, $
SHOWCMD=showcmd, $
TIFF=tiff, $
UNIX_CONVERT_CMD=unix_convert_cmd, $
WIDTH=width
	

	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL

		; Issue an error message, unless messages are turned off.
		IF ~Keyword_Set(nomessage) THEN void = cgErrorMsg()

		; Clean up.
		IF self._ps_restore_device NE "" THEN Set_Plot, self._ps_restore_device
;		!P = self._ps_restore_p
;		!X = self._ps_restore_x
;		!Y = self._ps_restore_y
;		!Z = self._ps_restore_z
		self._ps_restore_setup      = 0
		self._ps_restore_device     = ""
		self._ps_restore_filename   = ""
		self._ps_restore_rastertype = ""

		RETURN
	ENDIF

;-----------------------------------------------------
; Close PS Device \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	; Close the PostScript file, if this is PostScript device.
	IF !D.Name EQ 'PS' THEN Device, /CLOSE_FILE
	xsize       = !D.X_Size
	ysize       = !D.Y_Size
	ps_filename = self._ps_restore_filename

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Defaults
	tf_pdf            =  Keyword_Set(pdf)
	allow_transparent =  Keyword_Set(allow_transparent)
	tf_fix            = ~Keyword_Set(nofix)
	showcmd           =  Keyword_Set(showcmd)
	IF N_Elements(delete_ps) EQ 0 THEN delete_ps = self.ps_delete
	IF N_Elements(filetype)  EQ 0 THEN filetype  = self._ps_restore_rastertype
	IF N_Elements(density)   EQ 0 THEN density   = self.im_density
	IF N_Elements(resize)    EQ 0 THEN resize    = self.im_resize

	; Need to convert the PostScript to a raster file?
	needRaster = 0
	CASE StrUpCase(filetype) OF
		'BMP':  bmp  = 1
		'GIF':  gif  = 1
		'PDF':  pdf  = 1
		'PNG':  png  = 1
		'JPEG': jpeg = 1
		'JPG':  jpeg = 1
		'TIFF': tiff = 1
		'TIF':  tiff = 1
		"": 
		ELSE: void = Dialog_Message('File type ' + StrUpCase(filetype) + ' invalid. No raster created.')
	ENDCASE
	IF Keyword_Set(bmp)  THEN needRaster = 1
	IF Keyword_Set(gif)  THEN needRaster = 1
	IF Keyword_Set(pdf)  THEN needRaster = 1
	IF Keyword_Set(png)  THEN needRaster = 1
	IF Keyword_Set(jpeg) THEN needRaster = 1
	IF Keyword_Set(tiff) THEN needRaster = 1
	IF (N_Elements(width) NE 0) && (N_Elements(height) NE 0) $
		THEN Message, 'Cannot specify both HEIGHT and WIDTH at the same time.'

	; If the file is in landscape mode, then fix it so that the plot
	; is right-side up.
	IF self._ps_restore_landscape THEN BEGIN
		IF tf_fix THEN BEGIN
			cgFixPS, ps_filename, $
			         PAGETYPE = self._ps_restore_pagetype, $
			         SUCCESS  = success, $
			         QUIET    = 1
			IF success EQ 0 THEN Print, 'Encountered problem fixing landscape PostScript file. Proceeding...'
		ENDIF
		portrait = 0
	ENDIF ELSE portrait = 1

;-----------------------------------------------------
; Convert PS to Raster \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF needRaster THEN BEGIN
		;
		; Requirements to convert to raster:
		;    PDF    - Must have either the UNIX_CONVERT_CMD or ImageMagick
		;    RASTER - Must have ImageMagick
		;
		IF (~tf_pdf && ~self._has_im) || (tf_pdf && N_Elements(unix_convert_cmd) EQ 0 && ~self._has_im) THEN BEGIN
			Print, ''
			Print, 'Message from the cgPS_Close Program:'
			Print, '   The requested PDF operation cannot be completed unless ImageMagick is installed.'
			delete_ps = 0
			Print, '   The requested PostScript file has been saved: ' + ps_filename + '.'
			Print, '   Please see http://www.idlcoyote.com/graphics_tips/weboutput.php for details'
			Print, '   about converting PostScript intermediate files to PDF files via ImageMagick.'
			void = Dialog_Message('cgPS_Close: ImageMagick must be installed to complete raster operation.')
		ENDIF

		;PDF
		IF tf_pdf THEN BEGIN
			cgPS2PDF, ps_filename, raster_filename, $
			          DELETE_PS        = delete_ps, $
			          GS_PATH          = gs_path, $
			          PAGETYPE         = self._ps_restore_pagetype, $
			          SHOWCMD          = showcmd, $
			          SILENT           = silent, $
			          SUCCESS          = success, $
			          UNIX_CONVERT_CMD = unix_convert_cmd
		
		;RASTER
		ENDIF ELSE BEGIN
			cgPS2Raster, ps_filename, raster_filename, $
			             ALLOW_TRANSPARENT = allow_transparent, $
			             BMP               = bmp, $
			             DELETE_PS         = delete_ps, $
			             DENSITY           = density, $
			             IM_OPTIONS        = im_options, $
			             FILETYPE          = filetype, $
			             GIF               = gif, $
			             JPEG              = jpeg, $
			             HEIGHT            = height, $
			             OUTFILENAME       = outfilename, $
			             PDF               = pdf, $
			             PNG               = png, $
			             PORTRAIT          = portrait, $
			             RESIZE            = resize, $
			             SHOWCMD           = showcmd, $
			             SILENT            = self._ps_restore_quiet, $
			             SUCCESS           = success, $
			             TIFF              = tiff, $
			             WIDTH             = width
		ENDELSE
	ENDIF

;-----------------------------------------------------
; Clean Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	; Restore the previous True-Type font state for the PostScript device.
;	IF !D.Name EQ 'PS' THEN Device, SET_FONT=self._ps_restore_tt_font_old, /TT_Font

	; Clean up.
	IF self._ps_restore_device NE "" THEN Set_Plot, self._ps_restore_device
;	!P = self._ps_restore_p
;	!X = self._ps_restore_x
;	!Y = self._ps_restore_y
;	!Z = self._ps_restore_z
	self._ps_restore_setup      = 0
	self._ps_restore_device     = ""
	self._ps_restore_filename   = ""
	self._ps_restore_rastertype = ""
END


;+
;   Get PostScript and ImageMagick default values.
;
; :Keywords:
;     ADJUSTSIZE:               out, optional, type=boolean
;                               Adjust default character size to the display window size.
;     IM_DENSITY:               out, optional, type=integer
;                               Set this keyword to the sampling density when ImageMagick
;                                   creates raster image file from PostScript outout.
;     IM_PNG8:                  out, optional, type=boolean
;                               If set, create 8-bit instead of 24-bit PNG files.
;     IM_OPTIONS:               out, optional, type=string
;                               Set this keyword to any ImageMagick options you would like
;                                   to pass along to the ImageMagick convert command when
;                                   creating raster image files from PostScript output.
;     IM_RASTER:                out, optional, type=boolean
;                               If set this, raster files will be produced by ImageMagick
;                                   via an intermediate PostScript file. This is the
;                                   default if you have ImageMagick installed.
;     IM_RESIZE:                out, optional, type=integer
;                               Set this keyword to percentage that the raster image file
;                                   created my ImageMagick from PostScript output should
;                                   be resized.
;     IM_TIFF_DEPTH:            out, optional, type=integer
;                               Channel depth of TIFF files on ImageMagick convert command.
;     IM_TRANSPARENT:           out, optional, type=boolean
;                               Set this keyword to allow ImageMagick to create transparent
;                                   backgrounds when it makes raster image files from
;                                   PostScript output.
;     IM_WIDTH:                 out, optional, type=integer, defualt=0
;                               Sets the width of raster output on raster files created
;                                   with ImageMagick.
;     PDF_PATH:                 out, optional, type=string
;                               Set this keyword to the name of the path to the Ghostscript
;                                   command for converting PS to PDF.
;     PDF_UNIX_CONVERT_CMD:     out, optional, type=string
;                               Set this keyword to the name of an alternative UNIX command
;                                   to convert PostScript to PDF.
;     PS_CHARSIZE:              out, optional, type=float
;                               The PostScript character size.
;     PS_DECOMPOSED:            out, optional, type=boolean
;                               Set this keyword to zero to set the PostScript color mode
;                                   to indexed color and to one to set the PostScript color
;                                   mode to decomposed color.
;     PS_DELETE:                out, optional, type=boolean
;                               Set this keyword to zero if you want to keep the PostScript
;                                   output ImageMagick creates when making raster file
;                                   output.
;     PS_ENCAPSULATED:          out, optional, type=boolean
;                               Set this keyword to configure PSCONFIG to produce
;                                   encapsulated PostScript output by default.
;     PS_FONT:                  out, optional, type=integer
;                               Set this keyword to the type of font you want to use in
;                                   PostScript output. It sets the FONT keyword on the
;                                   PSConfig command. Normally, 0 (hardware fonts) or
;                                   1 (true-type fonts).
;     PS_METRIC:                out, optional, type=boolean
;                               Set this keyword to configure PSCONFIG to use metric values
;                                   and A4 page size in its interface.
;     PS_QUIET:                 out, optional, type=boolean
;                               Set this keyword to set the QUIET keyword on PS_Start.
;     PS_SCALE_FACTOR:          out, optional, type=float
;                               Set his keyword to the PostScript scale factor you wish to
;                                   use in creating PostScript output.
;     PS_TT_FONT:               out, optional, type=string
;                               Set this keyword to the name of a true-type font to use in
;                                   creating PostScript output.
;     RESET:                    out, optional, type=boolean
;                               If set, reset default values.
;-
PRO MrSaveAs::GetDefaults, $
ADJUSTSIZE=adjustsize, $
IM_DENSITY=im_density, $
IM_PNG8=im_png8, $
IM_OPTIONS=im_options, $
IM_RASTER=im_raster, $
IM_RESIZE=im_resize, $
IM_TIFF_DEPTH=im_tiff_depth, $
IM_TRANSPARENT=im_transparent, $
IM_WIDHT=im_width, $
PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $
PDF_PATH=pdf_path, $
PS_DECOMPOSED=ps_decomposed, $
PS_DELETE=ps_delete, $
PS_METRIC=ps_metric, $
PS_ENCAPSULATED=ps_encapsulated, $
PS_CHARSIZE=ps_charsize, $
PS_FONT=ps_font, $
PS_PAGETYPE=ps_pagetype, $
PS_QUIET=ps_quiet, $
PS_SCALE_FACTOR = ps_scale_factor, $
PS_TT_FONT=ps_tt_font

	Compile_Opt strictarr
	on_error, 2

	;Check if the system variable exists
	DefSysV, '!MRSAVEAS_DEFAULTS', EXISTS=exists
	IF ~exists THEN self -> SetDefaults
	
	; If the user asked for the default, give it to them.
	IF Arg_Present(adjustsize)           THEN adjustsize          = !MrSaveAs_Defaults.adjustsize
	IF Arg_Present(im_density)           THEN im_density          = !MrSaveAs_Defaults.im_density
	IF Arg_Present(im_png8)              THEN im_png8             = !MrSaveAs_Defaults.im_png8
	IF Arg_Present(im_options)           THEN im_options          = !MrSaveAs_Defaults.im_options
	IF Arg_Present(im_raster)            THEN im_raster           = !MrSaveAs_Defaults.im_raster
	IF Arg_Present(im_resize)            THEN im_resize           = !MrSaveAs_Defaults.im_resize
	IF Arg_Present(im_tiff_depth)        THEN im_tiff_depth       = !MrSaveAs_Defaults.im_tiff_depth
	IF Arg_Present(im_transparent)       THEN im_transparent      = !MrSaveAs_Defaults.im_transparent
	IF Arg_Present(im_width) THEN BEGIN
		IF !MrSaveAs_Defaults.im_width NE 0 THEN im_width = !MrSaveAs_Defaults.im_width
	ENDIF
	IF Arg_Present(pdf_unix_convert_cmd) THEN pdf_unix_convert_cmd = !MrSaveAs_Defaults.pdf_unix_convert_cmd
	IF Arg_Present(pdf_path)             THEN pdf_path             = !MrSaveAs_Defaults.pdf_path
	IF Arg_Present(ps_decomposed)        THEN ps_decomposed        = !MrSaveAs_Defaults.ps_decomposed
	IF Arg_Present(ps_delete)            THEN ps_delete            = !MrSaveAs_Defaults.ps_delete
	IF Arg_Present(ps_metric)            THEN ps_metric            = !MrSaveAs_Defaults.ps_metric
	IF Arg_Present(ps_encapsulated)      THEN ps_encapsulated      = !MrSaveAs_Defaults.ps_encapsulated
	IF Arg_Present(ps_charsize)          THEN ps_charsize          = !MrSaveAs_Defaults.ps_charsize
	IF Arg_Present(ps_font)              THEN ps_font              = !MrSaveAs_Defaults.ps_font
	IF Arg_Present(ps_quiet)             THEN ps_quiet             = !MrSaveAs_Defaults.ps_quiet
	IF Arg_Present(ps_pagetype)          THEN ps_pagetype          = !MrSaveAs_Defaults.ps_pagetype
	IF Arg_Present(ps_scale_factor)      THEN ps_scale_factor      = !MrSaveAs_Defaults.ps_scale_factor
	IF Arg_Present(ps_tt_font)           THEN ps_tt_font           = !MrSaveAs_Defaults.ps_tt_font
END


;+
;   Set defaults for PostScript and ImageMagick output. A system variable is created
;   so that defaults can be remembered throughout an IDL session.
;
; :Keywords:
;     ADJUSTSIZE:               in, optional, type=boolean, default=0
;                               Adjust default character size to the display window size.
;     IM_DENSITY:               in, optional, type=integer, default=300
;                               Set this keyword to the sampling density when ImageMagick
;                                   creates raster image file from PostScript outout.
;     IM_PNG8:                  in, optional, type=boolean, default=0
;                               If set, create 8-bit instead of 24-bit PNG files.
;     IM_OPTIONS:               in, optional, type=string, default=""
;                               Set this keyword to any ImageMagick options you would like
;                                   to pass along to the ImageMagick convert command when
;                                   creating raster image files from PostScript output.
;     IM_RASTER:                in, optional, type=boolean
;                               If set this, raster files will be produced by ImageMagick
;                                   via an intermediate PostScript file. This is the
;                                   default if you have ImageMagick installed.
;     IM_RESIZE:                in, optional, type=integer, default=25
;                               Set this keyword to percentage that the raster image file
;                                   created my ImageMagick from PostScript output should
;                                   be resized.
;     IM_TIFF_DEPTH:            in, optional, type=integer, default=8
;                               Channel depth of TIFF files on ImageMagick convert command.
;     IM_TRANSPARENT:           in, optional, type=boolean, default=0
;                               Set this keyword to allow ImageMagick to create transparent
;                                   backgrounds when it makes raster image files from
;                                   PostScript output.
;     IM_WIDTH:                 in, optional, type=integer, defualt=0
;                               Sets the width of raster output on raster files created
;                                   with ImageMagick.
;     PDF_PATH:                 in, optional, type=string, default=''
;                               Set this keyword to the name of the path to the Ghostscript
;                                   command for converting PS to PDF.
;     PDF_UNIX_CONVERT_CMD:     in, optional, type=string, default=''
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
;     RESET:                    in, optional, type=boolean, default=0
;                               If set, reset default values.
;-
PRO MrSaveAs::SetDefaults, $
ADJUSTSIZE=adjustsize, $
IM_DENSITY=im_density, $
IM_OPTIONS=im_options, $
IM_PNG8=im_png8, $
IM_RASTER=im_raster, $
IM_RESIZE=im_resize, $
IM_TIFF_DEPTH=im_tiff_depth, $
IM_TRANSPARENT=im_transparent, $
IM_WIDTH=im_width, $
PDF_PATH=pdf_path, $
PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $
PS_CHARSIZE=ps_charsize, $
PS_DECOMPOSED=ps_decomposed, $
PS_DELETE=ps_delete, $
PS_ENCAPSULATED=ps_encapsulated, $
PS_FONT=ps_font, $
PS_METRIC=ps_metric, $
PS_PAGETYPE=ps_pagetype, $
PS_QUIET=ps_quiet, $
PS_SCALE_FACTOR = ps_scale_factor, $
PS_TT_FONT=ps_tt_font, $
RESET=reset
	Compile_Opt strictarr
	on_error, 2

	;Check if the system variable exists
	DefSysV, '!MRSAVEAS_DEFAULTS', EXISTS=exists
	
	;If it does not exist, or if we are resetting all defaults, create it
	IF ~exists || Keyword_Set(reset) THEN BEGIN
		; Check the various keywords. Create defaults.
		adjustsize = Keyword_Set(adjustsize)
		IF N_Elements(im_density)           EQ 0 THEN im_density           = 300
		im_png8 = Keyword_Set(im_png8)
		IF N_Elements(im_options)           EQ 0 THEN im_options           = ""
		IF N_Elements(im_raster)            EQ 0 THEN BEGIN
			IF self._has_im THEN im_raster = 1 ELSE im_raster = 0
		ENDIF
		IF N_Elements(im_resize)            EQ 0 THEN im_resize            = 25
		IF N_Elements(im_tiff_depth)        EQ 0 THEN im_tiff_depth        = 8
		IF N_Elements(im_transparent)       EQ 0 THEN im_transparent       = 0
		IF N_Elements(im_width)             EQ 0 THEN im_width             = 0
		IF N_Elements(pdf_unix_convert_cmd) EQ 0 THEN pdf_unix_convert_cmd = ""
		IF N_Elements(pdf_path)             EQ 0 THEN pdf_path             = ""
		IF N_Elements(ps_charsize)          EQ 0 THEN ps_charsize          = 0.0
		IF N_Elements(ps_encapsulated)      EQ 0 THEN ps_encapsulated      = 0
		IF N_Elements(ps_font)              EQ 0 THEN ps_font              = 0
		IF N_Elements(ps_decomposed)        EQ 0 THEN ps_decomposed        = 0 ; Index mode by default.
		IF N_Elements(ps_delete)            EQ 0 THEN ps_delete            = 1
		IF N_Elements(ps_metric)            EQ 0 THEN ps_metric            = 0
		IF N_Elements(ps_pagetype)          EQ 0 THEN ps_pagetype          = 'LETTER'
		IF N_Elements(ps_quiet)             EQ 0 THEN ps_quiet             = 0
		IF N_Elements(ps_scale_factor)      EQ 0 THEN ps_scale_factor      = 1.0
		IF N_Elements(ps_tt_font)           EQ 0 THEN ps_tt_font           = 'Helvetica'
		
		; Define the default structure.
		mrsaveas_defaults = { _MRSAVEAS_DEFAULTS, $
		                      adjustsize:                 adjustsize, $
		                      IM_Density:                 im_density, $           ; Sets the density parameter on ImageMagick convert command.
		                      IM_PNG8:                    im_png8, $              ; If set create 8-bit rather than 24-bit PNG files.
		                      IM_Options:                 im_options, $           ; Sets extra ImageMagick options on the ImageMagick convert command.
		                      IM_Raster:                  im_raster, $            ; Sets the raster via ImageMagick setting.
		                      IM_Resize:                  im_resize, $            ; Sets the resize parameter on ImageMagick convert command.
		                      IM_TIFF_Depth:              im_tiff_depth, $        ; Sets the channel depth of TIFF files on ImageMagick convert command.
		                      IM_Transparent:             im_transparent, $       ; Sets the "alpha" keyword on ImageMagick convert command.
		                      IM_Width:                   im_width, $             ; Sets the width of raster output on raster files created with ImageMagick.
		                      PDF_UNIX_Convert_Cmd:       pdf_unix_convert_cmd, $ ; Sets the PDF alternative conversion command.
		                      PDF_Path:                   pdf_path, $             ; Set the path to the PDF conversion command.
		                      PS_Charsize:                ps_charsize, $          ; PostScript character size.
		                      PS_Decomposed:              ps_decomposed, $        ; Sets the PostScript color mode.
		                      PS_Delete:                  ps_delete, $            ; Delete the PostScript file when making IM files.
		                      PS_Encapsulated:            ps_encapsulated, $      ; Create Encapsulated PostScript output.
		                      PS_Font:                    ps_font, $              ; PostScript font to use.
		                      PS_Metric:                  ps_metric, $            ; Select metric measurements in PostScript output.
		                      PS_PageType:                ps_pagetype, $
		                      PS_Quiet:                   ps_quiet, $             ; PostScript QUIET keyword on cgPS_Open.
		                      PS_Scale_Factor:            ps_scale_factor, $      ; PostScript scale_factor
		                      PS_TT_Font:                 ps_tt_font $            ; PostScript true-type font.
		                    }
		
		;Create or reset the sytem variable
		IF exists $
			THEN !MrSaveAs_Defaults = mrsaveas_defaults $
			ELSE DefSysV, '!MrSaveAs_Defaults', mrsaveas_defaults

	;Set defaults
	ENDIF ELSE BEGIN
		IF N_Elements(adjustsize)           GT 0 THEN !MRSAVEAS_DEFAULTS.adjustsize           = adjustsize
		IF N_Elements(im_density)           GT 0 THEN !MRSAVEAS_DEFAULTS.im_density           = im_density
		IF N_Elements(im_png8)              GT 0 THEN !MRSAVEAS_DEFAULTS.im_png8              = im_png8
		IF N_Elements(im_options)           GT 0 THEN !MRSAVEAS_DEFAULTS.im_options           = im_options
		IF N_Elements(im_raster)            GT 0 THEN !MRSAVEAS_DEFAULTS.im_raster            = im_raster
		IF N_Elements(im_resize)            GT 0 THEN !MRSAVEAS_DEFAULTS.im_resize            = im_resize
		IF N_Elements(im_tiff_depth)        GT 0 THEN !MRSAVEAS_DEFAULTS.im_tiff_depth        = im_tiff_depth
		IF N_Elements(im_transparent)       GT 0 THEN !MRSAVEAS_DEFAULTS.im_transparent       = Keyword_Set(im_transparent)
		IF N_Elements(im_width)             GT 0 THEN !MRSAVEAS_DEFAULTS.im_width             = im_width
		IF N_Elements(raster_im)            GT 0 then !MRSAVEAS_DEFAULTS.raster_im            = raster_im
		IF N_Elements(pdf_unix_convert_cmd) GT 0 THEN !MRSAVEAS_DEFAULTS.pdf_unix_convert_cmd = pdf_unix_convert_cmd
		IF N_Elements(pdf_path)             GT 0 THEN !MRSAVEAS_DEFAULTS.pdf_path             = pdf_path
		IF N_Elements(ps_decomposed)        GT 0 THEN !MRSAVEAS_DEFAULTS.ps_decomposed        = Keyword_Set(ps_decomposed)
		IF N_Elements(ps_delete)            GT 0 THEN !MRSAVEAS_DEFAULTS.ps_delete            = Keyword_Set(ps_delete)
		IF N_Elements(ps_metric)            GT 0 THEN !MRSAVEAS_DEFAULTS.ps_metric            = Keyword_Set(ps_metric)
		IF N_Elements(ps_encapsulated)      GT 0 THEN !MRSAVEAS_DEFAULTS.ps_encapsulated      = Keyword_Set(ps_encapsulated)
		IF N_Elements(ps_charsize)          GT 0 THEN !MRSAVEAS_DEFAULTS.ps_charsize          = ps_charsize
		IF N_Elements(ps_font)              GT 0 THEN !MRSAVEAS_DEFAULTS.ps_font              = ps_font
		IF N_Elements(ps_quiet)             GT 0 THEN !MRSAVEAS_DEFAULTS.ps_quiet             = ps_quiet
		IF N_Elements(ps_pagetype)          GT 0 THEN !MRSAVEAS_DEFAULTS.ps_pagetype          = ps_pagetype
		IF N_Elements(ps_scale_factor)      GT 0 THEN !MRSAVEAS_DEFAULTS.ps_scale_factor      = ps_scale_factor
		IF N_Elements(ps_tt_font)           GT 0 THEN !MRSAVEAS_DEFAULTS.ps_tt_font           = ps_tt_font
	ENDELSE
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
PRO MrSaveAs::Save, filename, win, $
FILETYPE=fileType, $
MATCH=match, $
PICK_FILE=pick_file, $
RESIZE=resize
	Compile_Opt idl2

	; Error handling.
	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL
		void = cgErrorMsg()

		; Close the PostScript file.
		self -> PS_Close, /NOFIX, /NOMESSAGE

		; Set the window index number back.
		IF N_Elements(currentWindow) GT 0 THEN BEGIN
			IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1
		ENDIF

		RETURN
	ENDIF

	;Defaults
	pick_file = Keyword_Set(pick_file)
	match     = N_Elements(match) EQ 0 ? 1 : keyword_set(match)
	
	;Set the current window
	self -> SetWindow, win, CURRENT=currentWindow

;---------------------------------------------------------------------
; Automatic Output? //////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Get a file name to use.
	out_file = self -> GetFilename(filename, PICK_FILE=pick_file, TYPE=filetype, $
	                               DIRECTORY=directory, BASENAME=basename, EXTENSION=typeOut)
	typeOut  = StrUpCase(typeOut)

	;Save the file name and directory
	self.saveFile   = basename + '.' + StrLowCase(typeOut)
	self.saveDir    = directory
	self.ps_config -> SetProperty, DIRECTORY=self.saveDir, FILENAME=self.saveFile
	oFilename       = FilePath(self.saveFile, ROOT_DIR=self.saveDir)

;---------------------------------------------------------------------
; Raster File? ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Make sure a valid file type was given
	CASE typeOut OF
		'BMP':  raster = 1B
		'GIF':  raster = 1B
		'JPG':  raster = 1B
		'JPEG': raster = 1B
		'PNG':  raster = 1B
		'TIF':  raster = 1B
		'TIFF': raster = 1B
		'PS':   raster = 0B
		'PDF':  raster = 0B
		'EPS':  raster = 0B
		ELSE: Message, 'Unknown file type: "' + typeOut + '".'
	ENDCASE

;---------------------------------------------------------------------
; Postscript Device? /////////////////////////////////////////////////
;---------------------------------------------------------------------
	IF ~raster OR self.im_raster THEN BEGIN
		;We must have a window object in order to automatically produce PostScript output.
		IF ~Obj_Valid(self._SaveWindow) THEN $
			Message, 'PS, EPS, and ImageMagick raster files require a window object. Use PS_Open/PS_Close instead.'
	
		;Open the postscript file
		self -> PS_Open, oFilename, MATCH=match

		;Draw
		self -> Draw
		
		;Close the PS file
		self -> PS_Close

;---------------------------------------------------------------------
; Screen Shot ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	ENDIF ELSE BEGIN
		void = cgSnapshot(TYPE=typeOut, FILENAME=oFilename, /NODIALOG)
	ENDELSE

	;Indicate where the file was saved.
	IF ~self.ps_quiet THEN Print, typeOut + ' file located here: ' + oFilename

	;Reset the window ID.
	IF ((!d.flags and 256) ne 0) THEN BEGIN
		IF currentWindow NE -1 THEN WSet, currentWindow ELSE WSet, -1
	ENDIF
END


;+
; This event handler method saves the graphics window as a raster image file.
; PDF files also pass through here.
; 
; :Params:
;     EVENT:            in, required, type=structure
;                       The event structure.
;-
PRO MrSaveAs::SaveAsEvents, event
    Compile_Opt idl2

	; Error handling.
	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL
		if n_elements(thisIM) gt 0 then self.im_raster = thisIM
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

	;ImageMagick or cgSnapshot for raster files?
	IF StregEx(uName, 'IMAGEMAGICK', /BOOLEAN) $
		THEN tf_imagemagick = 1 $
		ELSE tf_imagemagick = 0

	;Toggle IM?
	thisIM         = self.im_raster
	self.im_raster = tf_imagemagick

	;Save the file
	self -> Save, FILETYPE=fileType, /PICK_FILE

	;Toggle IM back.
	self.im_raster = thisIM
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
	if ((!d.flags and 256) eq 0) && currentWindow eq -1 then return

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
;   Set the current window.
;
; :Params:
;       CURRENTWINDOW:          out, optional, type=long
;                               Window ID of the window active before calling
;                                   SetDisplayWindow.
;-
PRO MrSaveAs::SetWindow, win, $
CURRENT=currentWindow
	Compile_Opt strictarr
	on_error, 2

	;A window was given
	IF N_Elements(win) GT 0 THEN BEGIN
		;Window Object
		IF Size(win, /TNAME) EQ 'OBJREF' && Obj_HasMethod(win, 'SetCurrent') THEN BEGIN
			current          =  !D.Window
			win              -> SetCurrent
			self._saveWindow =  win
		
		;WindowID
		ENDIF ELSE IF WindowAvailable(win) || win EQ -1 THEN BEGIN
			current = !D.Window
			WSet, win
			self._saveWinID = win
		
		;Other
		ENDIF ELSE BEGIN
			message, 'A window object or window ID must be given.'
		ENDELSE
	
	;Use object properties
	ENDIF ELSE BEGIN

		;Do we have a window object?
		IF obj_valid(self._saveWindow) && Obj_HasMethod(self._saveWindow, 'SetCurrent') THEN BEGIN
			currentWindow    =  !D.Window
			self._saveWindow -> SetCurrent
			
		;Widnow ID
		ENDIF ELSE BEGIN
			;RETURN if the device does not support windows.
			IF (!D.Flags AND 256) EQ 0 THEN RETURN

			;Is with WindowID Valid?
			IF WindowAvailable(self._saveWinID) THEN BEGIN
				currentWindow = !D.Window
				WSet, self._saveWinID
			
			;Is the current window valid?
			ENDIF ELSE IF WindowAvailable(!D.Window) THEN BEGIN
				currentWindow = !D.Window
			
			;No valid windows.
			ENDIF ELSE BEGIN
				currentWindow = -1
				message, 'No windows are available.'
			ENDELSE
		ENDELSE
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
IM_PNG8=im_png8, $
IM_RASTER=im_raster, $
IM_RESIZE=im_resize, $
IM_TIFF_DEPTH=im_tiff_depth, $
IM_TRANSPARENT=im_transparent, $
IM_HEIGHT = im_height, $
IM_WIDTH = im_width, $
PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $
PDF_PATH=pdf_path, $
PS_CHARSIZE=ps_charsize, $
PS_DECOMPOSED=ps_decomposed, $
PS_DELETE=ps_delete, $
PS_ENCAPSULATED=ps_encapsulated, $
PS_FONT=ps_font, $
PS_METRIC=ps_metric, $
PS_PAGETYPE=ps_pagetype, $
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


	;ImageMagick Properties
	IF N_Elements(im_density)           GT 0 THEN  self.im_density     = im_density
	IF N_Elements(im_options)           GT 0 THEN  self.im_options     = im_options
	IF N_Elements(im_png8)              GT 0 THEN  self.im_png8        = Keyword_Set(im_png8)
	IF N_Elements(im_raster)            GT 0 then  self.im_raster      = Keyword_Set(im_raster)
	IF N_Elements(im_resize)            GT 0 THEN  self.im_resize      = im_resize
	IF N_Elements(im_tiff_depth)        GT 0 THEN  self.im_tiff_depth  = im_tiff_depth
	IF N_Elements(im_transparent)       GT 0 THEN  self.im_transparent = Keyword_Set(im_transparent)
	
	;PDF Properties
	IF N_Elements(pdf_unix_convert_cmd) GT 0 THEN  self.pdf_unix_convert_cmd = pdf_unix_convert_cmd
	IF N_Elements(pdf_path)             GT 0 THEN  self.pdf_path             = pdf_path

	;PS Properties
	IF N_Elements(adjustsize)           GT 0 THEN  self.adjustsize      = Keyword_Set(adjustsize)
	IF N_Elements(ps_charsize)          GT 0 THEN  self.ps_charsize     = ps_charsize
	IF N_Elements(ps_decomposed)        GT 0 THEN  self.ps_decomposed   = Keyword_Set(ps_decomposed)
	IF N_Elements(ps_delete)            GT 0 THEN  self.ps_delete       = Keyword_Set(ps_delete)
	IF N_Elements(ps_encapsulated)      GT 0 THEN  self.ps_encapsulated = Keyword_Set(ps_encapsulated)
	IF N_Elements(ps_font)              GT 0 THEN  self.ps_font         = ps_font
	IF N_Elements(ps_metric)            GT 0 THEN  self.ps_metric       = Keyword_Set(ps_metric)
	IF N_Elements(ps_pagetype)          GT 0 THEN  self.ps_pagetype     = ps_pagetype
	IF N_Elements(ps_quiet)             GT 0 THEN  self.ps_quiet        = Keyword_Set(ps_quiet)
	IF N_Elements(ps_scale_factor)      GT 0 THEN  self.ps_scale_factor = ps_scale_factor
	IF N_Elements(ps_tt_font)           GT 0 THEN  self.ps_tt_font      = ps_tt_font

	IF N_Elements(im_height) GT 0 THEN BEGIN
		Ptr_Free, self.im_width
		self.im_width = Ptr_New(/ALLOCATE_HEAP)
		*self.im_height = im_height
	ENDIF

	IF N_Elements(im_width) GT 0 THEN BEGIN
		Ptr_Free, self.im_height
		self.im_height = Ptr_New(/ALLOCATE_HEAP)
		*self.im_width = im_width
	ENDIF

	;Remove duplicate keywords from the EXTRA array, if they are present
	IF N_Elements(extra) GT 0 THEN BEGIN
		tf_member = MrIsMember(['FONTTYPE', 'TRUETYPE'], extra, /FOLD_CASE, $
		                       COMPLEMENT=iExtra, NCOMPLEMENT=nExtra)
		IF Max(tf_member) GT 0 THEN BEGIN
			message, 'Cannot set FONTTYPE or TRUETYPE keywords. Use PS_FONT and PS_TT_FONT instead.', /INFORMATIONAL
			IF nExtra GT 0 $
				THEN extra = extra[iExtra] $
				ELSE void  = temporary(extra)
		ENDIF
	ENDIF


;	self.ps_config -> SetProperty, DECOMPOSED   = ps_decomposed, $
;	                               ENCAPSULATED = ps_encapsulated, $
;	                               FONTTYPE     = ps_font, $
;	                               METRIC       = ps_metric, $
;	                               PAGETYPE     = ps_pagetype, $
;	                              _STRICT_EXTRA = extra
END


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrSaveAs::cleanup
	ptr_free,    self._group_leader
	ptr_free,    self.im_width
	ptr_free,    self.im_height
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
IM_HEIGHT=im_height, $
IM_OPTIONS=im_options, $
IM_PNG8=im_png8, $
IM_RASTER=im_raster, $
IM_RESIZE=im_resize, $
IM_TIFF_DEPTH=im_tiff_depth, $
IM_TRANSPARENT=im_transparent, $
IM_WIDTH = im_width, $
PDF_PATH=pdf_path, $
PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $
PS_CHARSIZE=ps_charsize, $
PS_DECOMPOSED=ps_decomposed, $
PS_DELETE=ps_delete, $
PS_ENCAPSULATED=ps_encapsulated, $
PS_FONT=ps_font, $
PS_METRIC=ps_metric, $
PS_PAGETYPE=ps_pagetype, $
PS_QUIET=ps_quiet, $
PS_SCALE_FACTOR=ps_scale_factor, $
PS_TT_FONT=ps_tt_font, $
_REF_EXTRA=extra
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return, 0
	endif
 
;---------------------------------------------------------------------
; Allocate Heap //////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Objects & Heap
	self.ps_config     = Obj_New('FSC_PSConfig')
	self._group_leader = Ptr_New(/ALLOCATE_HEAP)
	self.im_height     = Ptr_New(/ALLOCATE_HEAP)
	self.im_width      = Ptr_New(/ALLOCATE_HEAP)
 
;---------------------------------------------------------------------
;Get Default Values //////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Do we have ImageMagick?
	;   - Must be known before setting default properties
	self._has_im = cgHasImageMagick()

	;Establish default values
	;   - Call SetDefaults to initialize system variable if need be.
	self -> SetDefaults
	self -> SetProperty, _STRICT_EXTRA=!MrSaveAs_Defaults
 
;---------------------------------------------------------------------
;Set Object Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------

	;Windows and group leaders
	if n_elements(winID)        gt 0 then  self._saveWinID    = winID
	if n_elements(theWindow)    gt 0 then  self._saveWindow   = theWindow
	if n_elements(group_leader) gt 0 then *self._group_leader = group_leader

	;Set User Properties
	self -> SetProperty, ADJUSTSIZE           = adjustsize, $
	                     GROUP_LEADER         = group_leader, $
	                     IM_DENSITY           = im_density, $
	                     IM_OPTIONS           = im_options, $
	                     IM_PNG8              = im_png8, $
	                     IM_RASTER            = im_raster, $
	                     IM_RESIZE            = im_resize, $
	                     IM_TIFF_DEPTH        = im_tiff_depth, $
	                     IM_TRANSPARENT       = im_transparent, $
	                     IM_HEIGHT            = im_height, $
	                     IM_WIDTH             = im_width, $
	                     PDF_UNIX_CONVERT_CMD = pdf_unix_convert_cmd, $
	                     PDF_PATH             = pdf_path, $
	                     PS_CHARSIZE          = ps_charsize, $
	                     PS_DECOMPOSED        = ps_decomposed, $
	                     PS_DELETE            = ps_delete, $
	                     PS_ENCAPSULATED      = ps_encapsulated, $
	                     PS_FONT              = ps_font, $
	                     PS_METRIC            = ps_metric, $
	                     PS_PAGETYPE          = ps_pagetype, $
	                     PS_QUIET             = ps_quiet, $
	                     PS_SCALE_FACTOR      = ps_scale_factor, $
	                     PS_TT_FONT           = ps_tt_font, $
	                     WINID                = winID, $
	                     WINDOW               = theWindow, $
	                     _REF_EXTRA           = extra

	;Default save file location
	CD, CURRENT=current
	defsysv, '!cgPickFile_LastDir', EXISTS=exists
	self.saveDir = exists ? !cgPickFile_LastDir : current

	;Default save file name
	defsysv, '!cgPickfile_LastFile', EXISTS=exists
	self.saveFile = exists ? !cgPickFile_LastFile : 'MrWindow.png'

	;PS file locations
	ps_filename = cgRootName(self.saveFile) + '.ps'
	self.ps_config -> SetProperty, DIRECTORY=self.saveDir, FILENAME='MrWindow.ps'
	IF N_Elements(extra) GT 0 THEN self.ps_config -> SetProperty, _STRICT_EXTRA=extra

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
;       _HAS_IM:                Flag indicating that the user has image magic
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

	class = { MrSaveAs, $
	
	          ;Display window
	          _SaveWindow:   obj_new(), $   ; Window in which graphics are displayed.
	          _SaveWinID:    0L, $          ; ID of the window from which the image will be read.
	          _group_leader: ptr_new(), $   ; ID of a widget to serve as a group leader.
	          _has_im:       0B, $
	          
	          ; PostScript options.
	          adjustsize:      0B, $
	          ps_charsize:     0.0, $
	          ps_config:       obj_new(), $
	          ps_decomposed:   0B, $
	          ps_delete:       0B, $
	          ps_encapsulated: 0B, $
	          ps_font:         0, $
	          ps_metric:       0B, $
	          ps_pagetype:     '', $
	          ps_quiet:        0B, $
	          ps_scale_factor: 0.0, $
	          ps_tt_font:      '', $
	         
	          ;Restore PS options
	          _ps_restore_filename:    '', $
	          _ps_restore_setup:       0B, $
	          _ps_restore_device:      '', $
	          _ps_restore_landscape:   0B, $
	          _ps_restore_eps:         0B, $
	          _ps_restore_quiet:       0B, $
	          _ps_restore_pagetype:    '', $
	          _ps_restore_tt_font:     '', $
	          _ps_restore_tt_font_old: '', $
	          _ps_restore_font:        0,  $
	          _ps_restore_rastertype:  '', $
	          _ps_restore_p:           !P, $
	          _ps_restore_x:           !X, $
	          _ps_restore_y:           !Y, $
	          _ps_restore_z:           !Z, $
	          
	          ; PDF options.
	          pdf_path:             '', $
	          pdf_unix_convert_cmd: '', $
	          
	          ; ImageMagick output parameters.
	          im_density:     0L, $
	          im_height:      Ptr_New(), $
	          im_options:     '', $
	          im_png8:        0B, $
	          im_resize:      0L, $
	          im_raster:      0B, $
	          im_tiff_depth:  0L, $
	          im_transparent: 0B, $
	          im_width:       Ptr_New(), $
	          
	          saveFile:       '', $
	          saveDir:        '' $
	        }
end