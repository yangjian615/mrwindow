; docformat = 'rst'
;
; NAME:
;       MrImage_Examples
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
;   Examples of how to use MrImage__Define.
;
; :Examples:
;   See MrImage_Examples.pro for a series of examples::
;       IDL> void = MrImage_Examples()
;       IDL> win  = MrImage_Examples(14)
;
; :Params:
;       EXAMPLE:        in, required, type=int
;                       Index number of the example to be excecuted.
;
; :Returns:
;       WIN:            A MrImage object reference.
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
;       2014/09/09  -   Written by Matthew Argall
;-
function MrImage_Examples, example
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if obj_valid(img) then img -> Close
        void = cgErrorMSG()
        return, obj_new()
    endif
    
    ;Print a description of each example
    if n_elements(example) eq 0 then begin
        print, [['EXAMPLE    DESCRIPTION'], $
                ['   1       Display a la TV Procedure '], $
                ['   2       4x4 Tile a la TV Procedure'], $
                ['   3       Position a la TV Procedure'], $
                ['   4       Image Positioned via MrWindow'], $
                ['   5       Image with Coordinate Axes'], $
                ['   6       Image with NaNs and Missing Values'], $
                ['   7       Provide Coordinates for Y-Axis'], $
                ['   8       Provide Coordinates for X- and Y-Axis'], $
                ['   9       DATA_POS Keyword'], $
                ['  10       Log-Scale the Axes'], $
                ['  11       Center Each Pixel'], $
                ['  12       Position Each Pixel with Lower-Right and Upper-Left Corners'], $
                ['  13       Position Each Pixel with Pixel Centers and Delta +/-'], $
                ['  14       Polar Image'], $
                ['  15       Polar Image, Pixel Deltas, Log-Scale'], $
                ['  16       Polar Image, Polar axes & tickmarks']]
        return, -1
    endif

;---------------------------------------------------------------------
; Display a la TV Procedure //////////////////////////////////////////
;---------------------------------------------------------------------
    case example of
        1: begin
            ;Create the data
            data = cgDemoData(12)
            
            ;Create the image
            img  = MrImage(data, /TV)
            win  = img.window
        endcase

;---------------------------------------------------------------------
; 4x4 Tile a la TV Procedure /////////////////////////////////////////
;---------------------------------------------------------------------
        2: begin
            ;Create the data
            data = cgDemoData(12)
            dims = size(data, /DIMENSIONS)
            
            ;Make a window
            win  = MrWindow(XSIZE=dims[0]*2, YSIZE=dims[1]*2, REFRESH=0)
            
            ;Draw the images. Put the images in the current window.
            for i = 0, 3 do img = MrImage(data, i, /CURRENT, /TV, /NOERASE)
            win -> Refresh
        endcase
        

;---------------------------------------------------------------------
; Position a la TV Procedure /////////////////////////////////////////
;---------------------------------------------------------------------
        ;Position an image as the TV procedure would.
        3: begin
            data = cgDemoData(12)
            img  = MrImage(data, 30, 40, /TV)
            win  = img.window
        endcase
        
;---------------------------------------------------------------------
; Image Positioned via MrWindow //////////////////////////////////////
;---------------------------------------------------------------------
        ;Display the image centered in the window.
        4: begin
            data  = cgDemoData(12)
            theIm = MrImage(data)
            win   = theIm.window
        endcase
        
;---------------------------------------------------------------------
; Image with Coordinate Axes /////////////////////////////////////////
;---------------------------------------------------------------------
        5: begin
            data = cgDemoData(4)
            theIm = MrImage(data, CTINDEX=24, /AXES, TITLE='M51 Whirlpool Galaxy', $
                             YTITLE='Light Years', XTITLE='Distance (1000km)')
            win   = theIm.window
        endcase
        
;---------------------------------------------------------------------
; Image with NaNs and Missing Values /////////////////////////////////
;---------------------------------------------------------------------
        6: begin
            bad_image                  = float(cgDemoData(21))
            bad_image[0:10,0:10]       = !values.f_nan
            bad_image[245:255,155:245] = 300
            dims = size(bad_image, /DIMENSIONS)
            x    = indgen(dims[0])
            y    = indgen(dims[1])
            img = MrImage(bad_image, x, y, /AXES, /SCALE, CTINDEX=11, MISSING_VALUE=300, $
                                            MISSING_COLOR='Green', /NAN)
            win = img.window
        endcase

;---------------------------------------------------------------------
; Provide Coordinates for Y-Axis /////////////////////////////////////
;---------------------------------------------------------------------
        7: begin
            message, 'This example is not yet complete.'
;            data       = cgDemoData(12)
;            dims       = size(data, /DIMENSIONS)
;            lightYears = findgen(dims[1]) / (dims[1]-1)
;            img        = MrImage(data, lightYears, CTINDEX=24, /AXES, $
;                                  TITLE='M51 Whirlpool Galaxy', $
;                                  YTITLE='Light Years', XTITLE='Distance (1000km)')
;            win        = img.window
        endcase

;---------------------------------------------------------------------
; Provide Coordinates for X- and Y-Axis //////////////////////////////
;---------------------------------------------------------------------
        8: begin
            data   = cgDemoData(16)
            dims   = size(data, /DIMENSIONS)
            length = findgen(dims[0]) / (dims[0]-1)*9.46
            width  = findgen(dims[1]) / (dims[1]-1)
            img    = MrImage(data, length, width, CTINDEX=24, /AXES, $
                              TITLE='Muscle', $
                              YTITLE='nm', XTITLE='nm')
            win    = img.window
        endcase

;---------------------------------------------------------------------
; DATA_POS Keyword ///////////////////////////////////////////////////
;---------------------------------------------------------------------
        9: begin
            data = cgDemoData(10)
            dims = size(data, /DIMENSIONS)
            x    = indgen(dims[0])
            y    = indgen(dims[1])
            ymin = min(y)
            ymax = max(y)
            img  = MrImage(data[*,*,0], x, y, DATA_POS=[0, ymin, dims[0]/2, ymax], $
                            /SCALE, CTINDEX=33, /AXES)
            img2 = MrImage(data[*,*,1], x, y, DATA_POS=[dims[0]/2, ymin, dims[0]-1, ymax], $
                            /CURRENT, /SCALE, CTINDEX=33, /AXES, OVERPLOT=img)
            win  = img.window
        endcase

;---------------------------------------------------------------------
; Log-Scale the Axes /////////////////////////////////////////////////
;---------------------------------------------------------------------
        10: begin
            data       = cgDemoData(12)
            dims       = size(data, /DIMENSIONS)
            distance   = findgen(dims[0]) / (dims[0]-1)*9.46
            lightYears = findgen(dims[1]) / (dims[1]-1)
            img        = MrImage(data, distance, lightYears, CTINDEX=24, /AXES, $
                                  TITLE='M51 Whirlpool Galaxy', $
                                  YTITLE='Light Years', XTITLE='Distance (10$\up12$km)', $
                                  /XLOG, /YLOG, XRANGE=[0.1, 9.46], YRANGE=[0.1, 1])
            win        = img.window
        endcase

;---------------------------------------------------------------------
; Center Each Pixel //////////////////////////////////////////////////
;---------------------------------------------------------------------
        11: begin
            data = dist(20)
            dims = size(data, /DIMENSIONS)
            x_center = findgen(dims[0]) + 1
            y_center = findgen(dims[0]) + 1
            
            ;Note that the pixels are centered on the tickmarks
            img      = MrImage(data, x_center, y_center, /AXES, CTINDEX=22, $
                                XRANGE=[0,21], YRANGE=[0,21], /CENTER, /SCALE)
            win      = img.window
        endcase

;---------------------------------------------------------------------
; Position Each Pixel with Lower-Right and Upper-Left Corners ////////
;---------------------------------------------------------------------
        12: begin
            data = cgDemoData(8)
            data = data[*,*,0]
            dims = size(data, /DIMENSIONS)
            x_ll = findgen(dims[0]) + 0.25
            x_ur = findgen(dims[0]) + 0.75
            y_ll = findgen(dims[1]) + 0.1
            y_ur = findgen(dims[1]) + 1.1
            
            ;Note the gaps between verticle stripes.
            ;   - Each pixel extends from n+[0.25, 0.75] so that there is
            ;       a 0.5 unit gap between pixels
            img  = MrImage(data, x_ll, y_ll, x_ur, y_ur, $
                            /AXES, /SCALE, CTINDEX=11, BACKGROUND='Grey')
            win  = img.window
        endcase

;---------------------------------------------------------------------
; Position Each Pixel with Pixel Centers and Delta +/- ///////////////
;---------------------------------------------------------------------
        13: begin
            data = cgDemoData(3)
            dims = size(data, /DIMENSIONS)
            x_center = findgen(dims[0]) + 0.5
            y_center = findgen(dims[1]) + 0.5
            x_dminus = 0.5
            y_dminus = 0.5
            x_dplus  = linspace(0, 0.5, dims[0])
            y_dplus  = linspace(0, 0.5, dims[1])
            img      = MrImage(data, x_center, y_center, x_dminus, y_dminus, x_dplus, y_dplus, $
                                /AXES, /SCALE, CTINDEX=2)
            win      = img.window
        endcase

;---------------------------------------------------------------------
; Polar Image ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
        14: begin
            ;Create the grid spacing
            L    = linspace(  0,  9, 0.5, /INTERVAL)
            MLT  = linspace(  0, 24, 1.0, /INTERVAL)
            MLat = linspace(-45, 45, 1.0, /INTERVAL)

            ;Redefine MLT to be between 0 and 360 degrees
            MLT = MLT * (2*!pi / 24D)
        
            ;Create an image
            nBins_L   = n_elements(L)
            nBins_MLT = n_elements(MLT)
            theImage  = bytscl(dist(nBins_L, nBins_MLT))
        
            ;Display the image
            position = MrLayout([1,1], 1, /SQUARE)
            img      = MrImage(theImage, L, MLT, /POLAR, /AXES, CTINDEX=13, XRANGE=[-10,10], $
                                YRANGE=[-10,10], POSITION=position)
            win      = img.window
        endcase

;---------------------------------------------------------------------
; Polar Image, Pixel Deltas, Log-Scale ///////////////////////////////
;---------------------------------------------------------------------
        15: begin
            ;An image with 36 energy channels and 11 pitch angle channels
            theImage  = dist(36, 11)
            
            ;Logarithmically spaced energy channels with +/-
            Energy    = logspace(2, 4.4, 36)
            E_DPlus   = [(Energy[1:*] - Energy[0:-2]) / 2, (Energy[-1] - Energy[-2]) / 2]
            E_DMinus  = [Energy[0]/2.0, (Energy[1:*] - Energy[0:-2]) / 2]
            
            ;Pitch angle sectors with +/-
            PA        = [4.5, linspace(18, 162, 9), 175.5] * !dtor
            PA_DPlus  = [4.5, replicate(9, 9), 4.5] * !dtor
            PA_DMinus = [4.5, replicate(9, 9), 4.5] * !dtor

            ;Display the image
            position = MrLayout([1,1], 1, /SQUARE)
            img      = MrImage(theImage, Energy, PA, E_DMinus, PA_DMinus, E_DPlus, PA_DPlus, $
                                /POLAR, /RLOG, /SCALE, /AXES, CTINDEX=13, POSITION=position)
            win      = img.window
        endcase

;---------------------------------------------------------------------
; Polar Image, Polar Axes & Tickmarks ////////////////////////////////
;---------------------------------------------------------------------
        16: begin
            ;An image with 36 energy channels and 11 pitch angle channels
            theImage  = dist(36, 11)
            dims      = size(theImage, /DIMENSIONS)
            
            ;Logarithmically spaced energy channels with +/-
            Energy    = logspace(2, 4.4, 36)
            E_DPlus   = [(Energy[1:*] - Energy[0:-2]) / 2, (Energy[-1] - Energy[-2]) / 2]
            E_DMinus  = [Energy[0]/2.0, (Energy[1:*] - Energy[0:-2]) / 2]
            
            ;Pitch angle sectors with +/-
            PA        = [4.5, linspace(18, 162, 9), 175.5] * !dtor
            PA_DPlus  = [4.5, replicate(9, 9), 4.5] * !dtor
            PA_DMinus = [4.5, replicate(9, 9), 4.5] * !dtor
            
            ;Display the image
            position = MrLayout([1,1], 1, /SQUARE)
            img      = MrImage(theImage, Energy, PA, E_DMinus, PA_DMinus, E_DPlus, PA_DPlus, $
                                /POLAR, /XLOG, /SCALE, /AXES, CTINDEX=13, POSITION=position, $
                                POL_RLINESTYLE=2, POL_AXSTYLE=6, $
                                XTITLE='Energy (eV)', YTITLE='Energy (eV)')
            win      = img.window
        endcase

;---------------------------------------------------------------------
; Overplot Two Polar Images //////////////////////////////////////////
;---------------------------------------------------------------------
        17: begin
            ;An image with 36 energy channels and 11 pitch angle channels
            theImage  = dist(36, 11)
            dims      = size(theImage, /DIMENSIONS)
            
            ;Logarithmically spaced energy channels with +/-
            Energy    = logspace(2, 4.4, 36)
            E_DPlus   = [(Energy[1:*] - Energy[0:-2]) / 2, (Energy[-1] - Energy[-2]) / 2]
            E_DMinus  = [Energy[0]/2.0, (Energy[1:*] - Energy[0:-2]) / 2]
            
            ;Pitch angle sectors with +/-
            PA        = [4.5, linspace(18, 162, 9), 175.5] * !dtor
            PA_DPlus  = [4.5, replicate(9, 9), 4.5] * !dtor
            PA_DMinus = [4.5, replicate(9, 9), 4.5] * !dtor
            
            ;Display the image
            position = MrLayout([1,1], 1, /SQUARE)
            img1     = MrImage(theImage, Energy, PA, E_DMinus, PA_DMinus, E_DPlus, PA_DPlus, $
                                /POLAR, /XLOG, /SCALE, /AXES, CTINDEX=13, POSITION=position, $
                                POL_RLINESTYLE=2, POL_AXSTYLE=1, $
                                XRANGE=[-max(Energy), max(Energy)], YRANGE=[0, 2.0 * !pi], $
                                XTITLE='Energy (eV)', YTITLE='Energy (eV)')
            
            ;Turn refresh off
            img1 -> Refresh, /DISABLE
;            img1.xrange = [-(img1.xrange)[1], (img1.xrange)[1]]
            img1.yrange = [0, 2.0 * !pi]
            
            ;Overplot a reflection of the first image
            PA        = 2 * !pi - reverse(PA)
            PA_DPlus  = reverse(PA_DPlus)
            PA_DMinus = reverse(PA_DMinus)
            img2      = MrImage(theImage, Energy, PA, E_DMinus, PA_DMinus, E_DPlus, PA_DPlus, $
                                OVERPLOT=img1, /POLAR, /XLOG, /SCALE)
            
            ;Refresh and return the window
            img1 -> Refresh
            win = img1.window
        endcase

;---------------------------------------------------------------------
; No More Examples ///////////////////////////////////////////////////
;---------------------------------------------------------------------
        else: message, 'EXAMPLE must be between 1 and 17.'
    endcase
    
    return, win
end
