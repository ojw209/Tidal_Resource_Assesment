setwd('/media/oliver/Mass Storage/Marine Renewables/R_Assesment')
setwd('K:/Marine Renewables/R_Assesment')
#sudo wget --convert-links --no-parent -nd -nc -r -N -nH ftp://ftp.ifremer.fr/ifremer/sextant_data/MANGAE2500-AGRIF/MARC_F1-MARS3D-ARMOR/best_estimate/2018/

library(raster)
library(tidyverse)
library(RColorBrewer)
library(zeallot)
library(ncdf4)
library(lubridate)
library(extRemes)

#NETCDF-Temporal Length
M_START = 7
M_END = 8

#NETCDF-Spatial Length
READ_START = c(240, 240)
READ_COUNT = c(70, 70)
DEPTH = 40

#Extent = [-2.304063, -1.836719, 49.56459, 49.86526] - +proj=longlat +datum=WGS84
#Res_x = 0.03338172
#Res_y = 0.03340757
site_list = list(c(4, 4), c(6, 4), c(6, 5), c(10, 6))
#NOTE:Needs justification of alignment with site_list 1
site_list_2 = list(c(10, 30), c(20, 30), c(25, 35), c(35, 40))


#Call File_Read In Function.
c(Long_grid_Rast,Lat_grid_Rast,Vel_U_Rast,Vel_V_Rast,Depth_Rast) %<-% NC_READ_IN(M_START, M_END, READ_START, READ_COUNT, DEPTH)

#Create Data Frame for candidate sites.
DF_MET = data.frame("time" = seq(
  as.POSIXct("2018-01-01 00:00:00"),
  by = "hour",
  len = time_tot
))

for (i in 1:length(site_list_2)) {
  print(i)
  for (t in 1:time_tot) {
    D_temp[[i]] = Depth_Rast %>% subset(., t) %>% getValuesBlock(.,row = site_list_2[[i]][2],nrows = 1,col = site_list_2[[i]][1],ncols = 1)%>% {
      if (t > 1)
        append(.,D_temp[[i]])
      else
        .
    }
    C_U_temp[[i]] = Depth_Rast %>% subset(., t) %>% getValuesBlock(.,row = site_list_2[[i]][2],nrows = 1,col = site_list_2[[i]][1],ncols = 1)%>% {
      if (t > 1)
        append(.,C_U_temp[[i]])
      else
        .
    }
    C_V_temp[[i]] = Depth_Rast %>% subset(., t) %>% getValuesBlock(.,row = site_list_2[[i]][2],nrows = 1,col = site_list_2[[i]][1],ncols = 1)%>% {
      if (t > 1)
        append(.,C_V_temp[[i]])
      else
        .
    }
    
  }
  DF_MET[, paste0("C_Speed", i)] = sqrt(C_U_temp[[i]] ^ 2 + C_V_temp[[i]] ^2)
  DF_MET[, paste0("Depth", i)] = D_temp[[i]]
}

c(Hs_Rast,W_Dir_Rast,fp_pk_fre_Rast,u_wind_Rast,v_wind_Rast,time_tot) %<-% NC_READ_IN_2(M_START, M_END, READ_START, READ_COUNT)

#Tidy Workspace
rm(READ_START, READ_COUNT, DEPTH, M_START, M_END)



#Temp listfor Storing Time series
U_Site_List_temp = rep(list(0), length(site_list))
V_Site_List_temp = rep(list(0), length(site_list))
Hs_Site_List_temp = rep(list(0), length(site_list))
W_Dir_Site_List_temp = rep(list(0), length(site_list))
fp_pk_fre_Site_List_temp = rep(list(0), length(site_list))
C_U_temp = rep(list(0), length(site_list))
C_V_temp = rep(list(0), length(site_list))
D_temp = rep(list(0), length(site_list))


for (i in 1:length(site_list)) {
  print(i)
  for (t in 1:time_tot) {
    U_Site_List_temp[[i]] = u_wind_Rast %>% subset(., t) %>% getValuesBlock(
      .,
      row = site_list[[i]][2],
      nrows = 1,
      col = site_list[[i]][1],
      ncols = 1
    ) %>% {
      if (t > 1)
        append(., U_Site_List_temp[[i]])
      else
        .
    }
    V_Site_List_temp[[i]] = v_wind_Rast %>% subset(., t) %>% getValuesBlock(
      .,
      row = site_list[[i]][2],
      nrows = 1,
      col = site_list[[i]][1],
      ncols = 1
    ) %>% {
      if (t > 1)
        append(., V_Site_List_temp[[i]])
      else
        .
    }
    Hs_Site_List_temp[[i]] = Hs_Rast %>% subset(., t) %>% getValuesBlock(
      .,
      row = site_list[[i]][2],
      nrows = 1,
      col = site_list[[i]][1],
      ncols = 1
    ) %>% {
      if (t > 1)
        append(., Hs_Site_List_temp[[i]])
      else
        .
    }
    W_Dir_Site_List_temp[[i]] = W_Dir_Rast %>% subset(., t) %>% getValuesBlock(
      .,
      row = site_list[[i]][2],
      nrows = 1,
      col = site_list[[i]][1],
      ncols = 1
    ) %>% {
      if (t > 1)
        append(., W_Dir_Site_List_temp[[i]])
      else
        .
    }
    fp_pk_fre_Site_List_temp[[i]] = fp_pk_fre_Rast %>% subset(., t) %>% getValuesBlock(
      .,
      row = site_list[[i]][2],
      nrows = 1,
      col = site_list[[i]][1],
      ncols = 1
    ) %>% {
      if (t > 1)
        append(., fp_pk_fre_Site_List_temp[[i]])
      else
        .
    }
  }
  DF_MET[, paste0("Wi_Speed", i)] = sqrt(U_Site_List_temp[[i]] ^ 2 + V_Site_List_temp[[i]] ^
                                           2)
  DF_MET[, paste0("Wi_Angle", i)] = rad2deg(atan2(V_Site_List_temp[[i]], U_Site_List_temp[[i]]))
  DF_MET[, paste0("Hs", i)] = Hs_Site_List_temp[[i]]
  DF_MET[, paste0("Wa_Angle", i)] = W_Dir_Site_List_temp[[i]]
  DF_MET[, paste0("fp_pk_fre", i)] = fp_pk_fre_Site_List_temp[[i]]
}

#Tidy Workspace
#rm(U_Site_List_temp,V_Site_List_temp,Hs_Site_List_temp,W_Dir_Site_List_temp,fp_pk_fre_Site_List_temp,C_U_temp, C_V_temp,site_list,site_list_2)
#rm(Vel_U_Rast,Vel_V_Rast,Depth_Rast)
#rm(Hs_Rast,W_Dir_Rast,fp_pk_fre_Rast,u_wind_Rast,v_wind_Rast,time_tot)

#################
####Sum Stats####
#################

#Tidal_Vel_DF = as.data.frame(DF_MET$Wi_Speed)

Wi_Angle_1_Wind_Rose <-
  ggplot(DF_MET, aes(x = Wi_Angle1)) + geom_histogram(bins = 20) + coord_polar()

Wa_Angle_1_Wind_Rose <-
  ggplot(DF_MET, aes(x = Wa_Angle1)) + geom_histogram(bins = 20) + coord_polar()

Wi_Speed_1_ts <-
  ggplot(DF_MET, aes(x = time, y = Wi_Speed1)) + geom_line() + xlab("")

Hs_1_ts <-
  ggplot(DF_MET, aes(x = time, y = Hs1)) + geom_line() + xlab("")

figure_1 <-
  multiplot(Wi_Angle_1_Wind_Rose,
            Wa_Angle_1_Wind_Rose,
            Wi_Speed_1_ts,
            Hs_1_ts,
            cols = 2)

#Extreme Analysis
Extreme_Hs = fevd(Depth1, DF_MET, method = "Bayesian",time.units = "hours")
ci(Extreme_Hs, return.period = c(50*24,100*24))



