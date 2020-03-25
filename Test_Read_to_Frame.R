C_U_temp = rep(list(0), length(site_list_2))
C_V_temp = rep(list(0), length(site_list_2))
D_temp = rep(list(0), length(site_list_2))


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
