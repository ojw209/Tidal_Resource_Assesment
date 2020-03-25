#setwd('/media/oliver/Mass Storage/Marine Renewables/R_Assesment')
setwd('K:/Marine Renewables/R_Assesment')

library(raster)
library(tidyverse)
library(RColorBrewer)
library(zeallot)
library(ncdf4)


browseOnce <- function() {
  old <- getOption("error")
  function() {
    options(error = old)
    browser()
  }
}


options(error = browseOnce())

#Function to read in NETCDF files
NC_READ_IN <- function(M_Start, M_END,READ_START,READ_COUNT,DEPTH) {
  
  for (month in M_START:M_END){
    cat("---------- \n")
    cat("Month:",month,"\n"  )
    cat("----------\n")
    DInM = c(31,28,31,30,31,30,31,31,30,31,30,31)
    folder_path = "MARS3D/"
    ncname_gen = "MARC_F1-MARS3D-ARMOR_2018"
    ALD_EXTENT =extent(READ_START[1],READ_START[1]+READ_COUNT[1],READ_START[2],READ_START[2]+READ_COUNT[2]);
    i = 1
    if (month < 10){
      m_pre = as.character(month)
      m_post = paste("0", m_pre, sep = "")
    } else {
      m_post = as.character(month)
    }
    for (day in 1:DInM[month]){
      print(day)
      if (day < 10){
        d_pre = as.character(day)
        d_post = paste("0", d_pre, sep = "")
      } else {
        d_post = as.character(day)
      }
      for (hour in 0:23){
        if (hour < 10){
          h_pre = as.character(hour)
          h_post = paste("0", h_pre,"00", sep = "")
        } else {
          h_pre = as.character(hour)
          h_post = paste(h_pre,"00", sep = "")
        }
        FILE_N = paste(folder_path,ncname_gen,m_post,d_post,"T",h_post,"Z.nc",sep = "")
        
        err_test = try(nc_open(FILE_N))
        if (inherits(err_test, "try-error") == FALSE){
          Vel_U_Rast = FILE_N %>%
            raster(varname = "UZ",layer = DEPTH) %>%
            crop(y = ALD_EXTENT)%>% shift(.,dy = -0.5) %>%{if( i > 1) stack(.,Vel_U_Rast) else .}
          Vel_V_Rast = FILE_N %>%
            raster(varname = "VZ",layer = DEPTH) %>%
            crop(y = ALD_EXTENT) %>% shift(.,dx = -0.5) %>%{if( i > 1) stack(.,Vel_V_Rast) else .}
          Depth_Rast = FILE_N %>%
            raster(varname = "XE",layer = DEPTH) %>%
            crop(y = ALD_EXTENT) %>%{if( i > 1) stack(.,Depth_Rast) else .}} 
        else {
          cat("Error at:",FILE_N, "\n")
        }
        
        
        i = i+ 1
      }
    }
  }
  Long_grid_Rast = FILE_N %>% raster(.,varname = "longitude") %>%
    crop(y = ALD_EXTENT)
  Lat_grid_Rast =  FILE_N %>% raster(.,varname = "latitude") %>%
    crop(y = ALD_EXTENT)
  return = list(Long_grid_Rast,Lat_grid_Rast,Vel_U_Rast,Vel_V_Rast,Depth_Rast)
}

#NETCDF-Temporal Length
M_START = 1
M_END = 12

#NETCDF-Spatial Length
READ_START = c(240,240)
READ_COUNT = c(70,70)
DEPTH = 40
        
#Call File_Read In Function.
c(Long_grid_Rast,Lat_grid_Rast,Vel_U_Rast,Vel_V_Rast,Depth_Rast) %<-% NC_READ_IN(M_START,M_END,READ_START,READ_COUNT,DEPTH)

c(Hs_Rast,W_Dir_Rast,fp_pk_fre_Rast,u_wind_Rast,v_wind_Rast,time_tot) %<-% NC_READ_IN_2(M_START,M_END,READ_START,READ_COUNT)

Speed_Rast = sqrt((Vel_U_Rast*Vel_U_Rast)+(Vel_V_Rast*Vel_V_Rast))

plot(subset(Speed_Rast,1))

