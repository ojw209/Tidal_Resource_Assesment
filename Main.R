##############################################
#Marine Project Code                         #
#Author: Oliver West                         #
#Date: 06/04/2020                            #
##############################################

#Set Working Directory to your working directory
setwd('/media/oliver/Mass Storage/Marine Renewables/R_Assesment')
setwd('K:/Marine Renewables/R_Assesment')
#sudo wget --convert-links --no-parent -nd -nc -r -N -nH ftp://ftp.ifremer.fr/ifremer/sextant_data/MANGAE2500-AGRIF/MARC_F1-MARS3D-ARMOR/best_estimate/2018/

##Load Library Functions
library(raster)
library(tidyverse)
library(RColorBrewer)
library(zeallot)
library(ncdf4)
library(lubridate)
library(extRemes)

#NETCDF-Temporal Length
M_START = 11
M_END = 12

#NETCDF-Spatial Length
READ_START = c(240, 240)
READ_COUNT = c(70, 70)
DEPTH = 40

#Extent = [-2.304063, -1.836719, 49.56459, 49.86526] - +proj=longlat +datum=WGS84
#Res_x = 0.03338172
#Res_y = 0.03340757
site_list = list(c(4, 4), c(6, 4), c(6, 5), c(10, 6),c(11,6))
#NOTE:Needs justification of alignment with site_list 1
site_list_2 = list(c(10, 30), c(20, 30), c(25, 35), c(35, 40),c(50,55))


#Call File_Read In Function - Files for velocity field anylsis.
c(Long_grid_Rast,Lat_grid_Rast,Vel_U_Rast,Vel_V_Rast,Depth_Rast) %<-% NC_READ_IN(M_START, M_END, READ_START, READ_COUNT, DEPTH)

#Time Length Paramater
time_tot = 8758

#Create Data Frame for candidate sites.
DF_MET = data.frame("time" = seq(
  as.POSIXct("2018-01-01 00:00:00"),
  by = "hour",
  len = time_tot
))

#Temp listfor Storing Time series
U_Site_List_temp = rep(list(0), length(site_list))
V_Site_List_temp = rep(list(0), length(site_list))
Hs_Site_List_temp = rep(list(0), length(site_list))
W_Dir_Site_List_temp = rep(list(0), length(site_list))
fp_pk_fre_Site_List_temp = rep(list(0), length(site_list))
C_U_temp = rep(list(0), length(site_list_2))
C_V_temp = rep(list(0), length(site_list_2))
D_temp = rep(list(0), length(site_list_2))

#Extract Raster Data
for (i in 1:length(site_list_2)) {
  cat(i)
  for (t in 1:time_tot) {
    C_U_temp[[i]] = Vel_U_Rast %>% subset(., t) %>% getValuesBlock(
      .,
      row = site_list_2[[i]][2],
      nrows = 1,
      col = site_list_2[[i]][1],
      ncols = 1
    ) %>% {
      if (t > 1)
        append(., C_U_temp[[i]])
      else        .
    }
    C_V_temp[[i]] = Vel_V_Rast %>% subset(., t) %>% getValuesBlock(
      .,
      row = site_list_2[[i]][2],
      nrows = 1,
      col = site_list_2[[i]][1],
      ncols = 1
    ) %>% {
      if (t > 1)
        append(., C_V_temp[[i]])
      else        .
    }
    D_temp[[i]] = Depth_Rast_1_12 %>% subset(., t) %>% getValuesBlock(
      .,
      row = site_list_2[[i]][2],
      nrows = 1,
      col = site_list_2[[i]][1],
      ncols = 1
    ) %>% {
      if (t > 1)
        append(., D_temp[[i]])
      else        .
    }
    
    
  }
  DF_MET[, paste0("C_Speed", i)] = sqrt(C_U_temp[[i]] ^ 2 + C_V_temp[[i]] ^
                                          2)
  DF_MET[, paste0("C_Angle", i)] = rad2deg(atan2(C_V_temp[[i]], C_U_temp[[i]])) 

  DF_MET[, paste0("Depth", i)] = D_temp[[i]]
}


#Call File_Read In Function - Files for Met Ocean anylsis.
c(Hs_Rast,W_Dir_Rast,fp_pk_fre_Rast,u_wind_Rast,v_wind_Rast,time_tot) %<-% NC_READ_IN_2(M_START, M_END, READ_START, READ_COUNT)

#Tidy Workspace
rm(READ_START, READ_COUNT, DEPTH, M_START, M_END)

#Extract Raster Data
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
  DF_MET[, paste0("Wi_Angle", i)] = rad2degfrom(atan2(U_Site_List_temp[[i]], V_Site_List_temp[[i]]))
  DF_MET[, paste0("Hs", i)] = Hs_Site_List_temp[[i]]
  DF_MET[, paste0("Wa_Angle", i)] = W_Dir_Site_List_temp[[i]]
  DF_MET[, paste0("fp_pk_fre", i)] = fp_pk_fre_Site_List_temp[[i]]
}

#Tidy Workspace
rm(U_Site_List_temp,V_Site_List_temp,Hs_Site_List_temp,W_Dir_Site_List_temp,fp_pk_fre_Site_List_temp,C_U_temp, C_V_temp,site_list,site_list_2)
rm(Vel_U_Rast,Vel_V_Rast,Depth_Rast)
rm(Hs_Rast,W_Dir_Rast,fp_pk_fre_Rast,u_wind_Rast,v_wind_Rast,time_tot)

#################
####Sum Stats####
#################
##Create energy density dataframes, for each angle.
DF_Energy_Density = data.frame("Angle" = seq(
  10,
  by = 10,
  len = 36
))
#Calculate energy density at each angle,
for (i in 1:5){
  for (j in 1:36){
    DF_Energy_Density[j,i+1] = tryCatch(DF_MET %>% filter(.,eval(parse(text =paste0("C_Angle",i))) >= 10*(j-1) & eval(parse(text =paste0("C_Angle",i))) < 10*j) %>% subset(.,select = paste0("C_Speed",i)) %>% sum(), silent = TRUE, error = function(e) {DF_Energy_Density[j,i+1] =1})
    }
}
#Turn into dataframe.
colnames(DF_Energy_Density) = c('Angle','Site1','Site2','Site3','Site4','Site5')
DF_Energy_Density = gather(DF_Energy_Density,key = "Loc",value = "E_Density",Site1,Site2,Site3,Site4,Site5)
DF_Energy_Density$Raw_Energy = 0.5*0.434*100*DF_Energy_Density$E_Density

#Create Dataframe for all sites. [MARS3D]
MET_plot_DF_t = DF_MET %>%rename(.,Site1 = C_Speed1, Site2 = C_Speed2, Site3 = C_Speed3, Site4= C_Speed4, Site5= C_Speed5) %>% gather(.,key = "Loc",value = "C_Speed",Site1,Site2,Site3,Site4,Site5)
MET_plot_DF = MET_plot_DF_t[c(1,12:13)]
MET_plot_DF_t = DF_MET %>%rename(.,Site1 = C_Angle1, Site2 = C_Angle2, Site3 = C_Angle3, Site4= C_Angle4, Site5= C_Angle5) %>% gather(.,key = "Loc",value = "C_Angle",Site1,Site2,Site3,Site4,Site5)
MET_plot_DF = MET_plot_DF %>% cbind(.,MET_plot_DF_t[13])
MET_plot_DF_t = DF_MET %>%rename(.,Site1 = Depth1, Site2 = Depth2, Site3 = Depth3, Site4= Depth4, Site5= Depth5) %>% gather(.,key = "Loc",value = "Depth",Site1,Site2,Site3,Site4,Site5)
MET_plot_DF = MET_plot_DF %>% cbind(.,MET_plot_DF_t[13])


#Create Dataframe for all sites. [NORGAS]
MET_plot_DF = DF_MET %>%rename(.,Site1 = Wi_Speed1, Site2 = Wi_Speed2, Site3 = Wi_Speed3, Site4= Wi_Speed4, Site5= Wi_Speed5) %>% gather(.,key = "Loc",value = "Wi_Speed",Site1,Site2,Site3,Site4,Site5)
MET_plot_DF = MET_plot_DF[c(1,22:23)]
MET_plot_DF_t = DF_MET %>%rename(.,Site1 = Wi_Angle1, Site2 = Wi_Angle2, Site3 = Wi_Angle3, Site4= Wi_Angle4,Site5= Wi_Angle5) %>% gather(.,key = "Loc",value = "Wi_Angle",Site1,Site2,Site3,Site4,Site5)
MET_plot_DF = MET_plot_DF %>% cbind(.,MET_plot_DF_t[23])
MET_plot_DF_t = DF_MET %>%rename(.,Site1 = Hs1, Site2 = Hs2, Site3 = Hs3, Site4= Hs4, Site5= Hs5) %>% gather(.,key = "Loc",value = "Hs",Site1,Site2,Site3,Site4,Site5)
MET_plot_DF = MET_plot_DF %>% cbind(.,MET_plot_DF_t[23])
MET_plot_DF_t = DF_MET %>%rename(.,Site1 = Wa_Angle1, Site2 = Wa_Angle2, Site3 = Wa_Angle3, Site4= Wa_Angle4,Site5= Wa_Angle5 ) %>% gather(.,key = "Loc",value = "Wa_Angle",Site1,Site2,Site3,Site4,Site5)
MET_plot_DF = MET_plot_DF %>% cbind(.,MET_plot_DF_t[23])
MET_plot_DF_t = DF_MET %>%rename(.,Site1 = fp_pk_fre1, Site2 = fp_pk_fre2, Site3 = fp_pk_fre3, Site4= fp_pk_fre4 ,Site5= fp_pk_fre5 ) %>% gather(.,key = "Loc",value = "fp_pk_fre",Site1,Site2,Site3,Site4,Site5)
MET_plot_DF = MET_plot_DF %>% cbind(.,MET_plot_DF_t[23])

#Create sub Dataframe for angle of site 1, 2 and 3.
DF_Temp = DF_MET %>% rename(.,'Site1' = 'C_Angle1', 'Site3' = 'C_Angle3','Site5' = 'C_Angle5') %>% gather(.,key = 'Site',value = 'C_Angle',Site1,Site3,Site5)
DF_Comparison = DF_Temp[,c(14,15)]

#Energy Density at each angle.
ggplot(DF_Energy_Density,aes(x= Angle,y = Raw_Energy)) + geom_bar(stat = "identity", colour="black", fill="cyan") + 
  geom_density(stat = "identity",alpha=.2, fill="#FF6666") + facet_wrap(~ Loc, nrow = 2) + 
  ylab('Cumulative Energy KW') + 
  ggtitle("Cumulitive Yearly Energy Production For Each Site",subtitle = "Statified by Flow Angle")

#Energy Density at each Angle, polar coordinates
ggplot(DF_Comparison, aes(x=C_Angle)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="cyan") + coord_polar(start= -pi/30) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ facet_wrap(~ Site) +
  scale_x_continuous(breaks = round(seq(0, 360, by = 20),1)) +
  ggtitle("Distribution of flow Angle's over the course of 2018",subtitle = "Data for Site 1,2 and 3") +
  xlab('Note, the bi-modal nature of the direction of flooding on Site 5.')

#Polar Plot for the energy distributions at site 1,3 and 5.
ggplot(MET_plot_DF, aes(x=C_Speed)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="cyan")+
  geom_density(alpha=.2, fill="#FF6666") + facet_wrap(~ Loc) +
  ggtitle("Distribution of flow Angle's over the course of 2018",subtitle = "Data for Site 1,2 and 3") +
  xlab('Note, the bi-modal nature of the direction of flooding on Site 5.')


#Tidal_Vel_DF = as.data.frame(DF_MET$Wi_Speed)
ggplot(MET_plot_DF, aes(x=Depth)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="cyan")+
  geom_density(alpha=.2, fill="#FF6666") + facet_wrap(~ Loc)

#Tidal_Vel_DF = as.data.frame(DF_MET$Wi_Speed)
ggplot(MET_plot_DF, aes(x=C_Speed)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="cyan")+
  geom_density(alpha=.2, fill="#FF6666") + facet_wrap(~ Loc) + xlab(bquote('Stream Velocity' (m~s^-1))) 

#Flow Direction Graph
ggplot(MET_plot_DF, aes(y = C_Speed,x = C_Angle)) + geom_point(colour = 'black',alpha = 0.05, size = 0.1, stroke = 0.1) + facet_wrap(~ Loc) +
  coord_polar(start= -pi/30) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_x_continuous(breaks = round(seq(0, 360, by = 20),1))


#Wind Rose Plots
wr_plot_site = plot.windrose(data = MET_plot_DF,spd = "Wi_Speed",dir = "Wi_Angle", spdmin = 0, spdmax = 20)
wr_plot_site_2 = wr_plot_site +  facet_wrap(~ Loc,ncol = 3)

#Wave Rose Plots
wr_plot_site = plot.windrose(data = MET_plot_DF,spd = "Hs",dir = "Wa_Angle", spdmin = 0, spdmax = 20)
wr_plot_site_2 = wr_plot_site +  facet_wrap(~ Loc,ncol = 3)

colnames(Depth) = c('Site1','Depth1','Site2','Depth2','Site3','Depth3','Site4','Depth4','Site5','Depth5')
Depth_Vel = Depth[,c(1,3,5,7,9)]
Depth_Dep = Depth[,-c(1,3,5,7,9)]
colnames(Depth_Dep) = c('Site1','Site2','Site3','Site4','Site5')
Depth_Vel_Df = gather(Depth_Vel,key = "Loc",value = 'Velocity', Site1,Site2,Site3,Site4,Site5)
Depth_Dep_Df = gather(Depth_Dep,key = "Loc",value = 'Depth', Site1,Site2,Site3,Site4,Site5)

Depth_Vel_Df$Depth = Depth_Dep_Df$Depth

#Plot Depth
ggplot(Depth_Vel_Df, aes(x = Depth,y = Velocity, colour = Loc)) + geom_line(size = 1.5) +
  labs(x = bquote('Depth'~(m)), y = bquote('Velocity'~(m / s^-1)))
  

#Extreme Analysis
for (i in 1:5){
  cat("\n",i)
  Seasonal_Effect = abs(sin(seq(pi,2*pi,length.out = nrow(DF_MET))))
  Extreme_Hs = fevd(eval(parse(text = paste0("DF_MET$Wi_Speed",i))),span = 1, DF_MET, method = "Bayesian",time.units = "720/year",period = "years", priorParams=list(v=c(1000,1000,0.1),m= c(0,0,0.5)))
  cat(" \n 1Y:")
  cat(max(return.level(Extreme_Hs, return.period = 720)))
  cat(" \n 20Y:")
  cat(max(return.level(Extreme_Hs, return.period = 20*720)))
  cat(" \n 50Y:")
  cat(max(return.level(Extreme_Hs, return.period = 50*720)))
  cat(" \n 100Y:")
  cat(max(return.level(Extreme_Hs, return.period = 100*720)))
  cat(" \n Summary:")
  cat(summary(eval(parse(text = paste0("DF_MET$Wi_Speed",i)))))
  summary(Extreme_Hs)
}
plot(Extreme_Hs,rperiods = c( 2,50,100))

