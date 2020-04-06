##############################################
#Code to calculate energy diff, between Yaw  #
#And Bi                                      #
#Author: Oliver West                         #
#Date: 06/04/2020                            #
##############################################

#Raster Analysis
library(sf)
library(raster)
library(ggplot2)
library(rgdal)
library(rgeos)
library(rasterVis) 
library(sf)


#Turn Rasters into matrix for ease of proscesing
C_Speed_Matrix = as.matrix(C_Speed)
C_Angle_Matrix = as.matrix(C_Angle)

#Create Storage Matrices.
C_Bi_Energy_Yield = matrix(0,nrow = 4900,ncol = 180)
C_Yaw_Energy_Yield = matrix(nrow = 4900,ncol = 1)
C_Bi_Opt_Energy_Yield = matrix(0,nrow = 4900,ncol = 1)
C_Opt_Angle = matrix(0,nrow = 4900,ncol = 1)
plot_mat = matrix(0,nrow = time_tot,ncol = 180)
C_Max_Flow = matrix(0,nrow = time_tot,ncol = 1)


#Calculate energy yield for a course of a year - at each alpha. [This may take some time.]
for (alpha in 1:180){
  cat('Alpha',alpha, '\n')
  for (i in 1:4900){
    if (i %% 1000 == 0){
      cat('Site', i, '\n')
    }
    E_temp_Bi_Drive = 0
    for (t in 1:time_tot){
      V_Axis = C_Speed_Matrix[i,t]*cos(deg2rad(alpha) - C_Angle_Matrix[i,t])
      E_temp_Bi_Drive = E_temp_Bi_Drive + 0.5*0.434*pi*100*V_Axis^3
    }
    C_Bi_Energy_Yield[i,alpha] = E_temp_Bi_Drive
    
  }
}

#Calculate Angle at which optimimumn energy yield occurs. 
for (i in 1:4900){
 if (length(which.max(C_Bi_Energy_Yield[i,])) != 0){
 cat()
 C_Opt_Angle[i] = which.max(C_Bi_Energy_Yield[i,])
 C_Bi_Opt_Energy_Yield[i] = C_Bi_Energy_Yield[i,C_Opt_Angle[i]]
 C_Max_Flow [i] = max(C_Speed_Matrix[i,])
 }
}

#Calculate expected energy yield for the yaw drive turbine. 
for (i in 1:4900){
  E_Temp_Yaw_Drive = 0
  for (t in 1:time_tot){
    E_Temp_Yaw_Drive = E_Temp_Yaw_Drive + 0.5*0.434*pi*100*C_Speed_Matrix[i,t]^3
  }
  C_Yaw_Energy_Yield[i] = E_Temp_Yaw_Drive
}

#Turn Vectors back into raster.
C_Speed_Matrix =  C_Speed_Matrix %>% matrix(.,nrow = 70, ncol = 70)%>% t() %>% raster()
C_Max_Flow=  C_Max_Flow %>% matrix(.,nrow = 70, ncol = 70)%>% t() %>% raster()
C_Yaw_Energy_Yield =  C_Yaw_Energy_Yield %>% matrix(.,nrow = 70, ncol = 70)%>% t() %>% raster()
C_Opt_Angle =  C_Opt_Angle  %>% matrix(.,nrow = 70, ncol = 70)%>% t() %>% raster()
C_Bi_Opt_Energy_Yield = C_Bi_Opt_Energy_Yield %>% matrix(.,nrow = 70, ncol = 70)%>% t() %>% raster()
crs(C_Speed_Matrix) = crs(Lat_grid_Rast)
extent(C_Speed_Matrix) = extent(Lat_grid_Rast)
crs(C_Max_Flow) = crs(Lat_grid_Rast)
extent(C_Max_Flow) = extent(Lat_grid_Rast)
crs(C_Yaw_Energy_Yield) = crs(C_Speed)
extent(C_Yaw_Energy_Yield) = extent(C_Speed)
crs(C_Opt_Angle) = crs(C_Speed)
extent(C_Opt_Angle) = extent(C_Speed)
crs(C_Bi_Opt_Energy_Yield) = crs(C_Speed)
extent(C_Bi_Opt_Energy_Yield) = extent(C_Speed)
E_Diff = C_Yaw_Energy_Yield - C_Bi_Opt_Energy_Yield

#Find Energy Difference
C_E_Diff = C_Bi_Opt_Energy_Yield - C_Yaw_Energy_Yield

#Use levelplot(DF,contour = TRUE) to plot rasters.
