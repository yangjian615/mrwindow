#!/bin/bash

#Destination directories
dest_MrWindow=/Users/argall/Documents/Work/Programs/MrWindow #MrWindow
dest_CDF=$dest_MrWindow/CDFReader                            #CDF Reader
dest_Utilities=$dest_MrWindow/utilities                      #Utilities
dest_WileE=$dest_MrWindow/WileE                              #Wile E. Graphics

#Source directories
src_CDF=/Users/argall/Documents/Work/Programs/cdf-reader    #CDF Reader
src_Lib=/Users/argall/Documents/Work/Programs/MyLibraryIDL  #My Personal Library
src_WileE=/Users/argall/Documents/Work/Programs/WileEGraphics       #Wile E. Graphics
src_Array=$src_Lib/array_utils                              #Array Utilities
src_GUI=$src_Lib/guis                                       #GUIs
src_Index=$src_Lib/index_utils                              #Index Utilities
src_Plot=$src_Lib/plot_utils                                #Plot Utilities
src_Struct=$src_Lib/struct_utils                            #Structure Utilities
src_Sys=$src_Lib/sys_utils                                  #System Utilities
src_Time=$src_Lib/time_utils                                #Time Utilities
src_Type=$src_Lib/type_utils                                #Type Utilities

#Sources: CDF Reader
CDFFiles[0]=$src_CDF/cdf_info__define.pro
CDFFiles[1]=$src_CDF/cdf_read__define.pro
CDFFiles[2]=$src_CDF/cdf_read.pro

CDFUtils[0]=$src_Time/convert_time.pro
CDFUtils[1]=$src_Time/datetime_to_epoch.pro
CDFUtils[2]=$src_Time/dissectdatetime.pro
CDFUtils[3]=$src_Time/dissectdate.pro
CDFUtils[4]=$src_Time/dissecttime.pro
CDFUtils[5]=$src_Struct/has_tag.pro
CDFUtils[6]=$src_Type/isnumeric.pro
CDFUtils[7]=$src_Type/type_to_format_code.pro

#Sources: Wile E. Graphics utils
WileEFiles[0]=$src_WileE/wearrow__define.pro
WileEFiles[1]=$src_WileE/weaxis__define.pro
WileEFiles[2]=$src_WileE/wecolorbar__define.pro
WileEFiles[3]=$src_WileE/wegraphicskeywords__define.pro
WileEFiles[4]=$src_WileE/welegend.pro
WileEFiles[5]=$src_WileE/welegenditem__define.pro
WileEFiles[6]=$src_WileE/weoplot.pro
WileEFiles[7]=$src_WileE/weoverplot__define.pro
WileEFiles[8]=$src_WileE/weoverplot.pro
WileEFiles[9]=$src_WileE/weplot.pro
WileEFiles[10]=$src_WileE/wetext__define.pro

#Sources: Array utils
UtilityFiles[0]=$src_Array/ismember.pro

#Sources: Index utils
UtilityFiles[1]=$src_Index/twod_to_oned_index.pro

#Source: GUIs
UtilityFiles[2]=$src_GUI/text_gui.pro

#Sources: Plot utils
UtilityFiles[3]=$src_Plot/mrgetwindow.pro
UtilityFiles[4]=$src_Plot/mrplotlayout.pro
UtilityFiles[5]=$src_Plot/image_plots.pro
UtilityFiles[6]=$src_Plot/load_color.pro
UtilityFiles[7]=$src_Plot/color_trip.pro
UtilityFiles[8]=$src_Plot/time_labels.pro

#Source: System utils
UtilityFiles[9]=$src_Sys/pwd.pro
UtilityFiles[10]=$src_Sys/file_extension.pro

#Source: Time utils
UtilityFiles[11]=$src_Time/epoch_to_ssm.pro

#------------------------------------
# MAKE THE LINKS ////////////////////
#------------------------------------
ln -fs ${CDFFiles[@]} $dest_CDF
ln -fs ${CDFUtils[@]} $dest_CDF/utilities
ln -fs ${UtilityFiles[@]} $dest_Utilities
ln -fs ${WileEFiles[@]} $dest_WileE