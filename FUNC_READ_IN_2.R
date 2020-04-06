##############################################
#Read in Function 1 (NORGAS)                 #
#And Bi                                      #
#Author: Oliver West                         #
#Date: 06/04/2020                            #
##############################################


NC_READ_IN_2 <- function(M_Start, M_END, READ_START, READ_COUNT) {
  i = 1
  folder_path = "NORGAS/"
  ncname_gen = "MARC_WW3-NORGAS-2M_2018"
  ALD_EXTENT = as(extent(-2.304, -1.842, 49.554, 49.864), 'SpatialPolygons')
  DInM = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  for (month in M_START:M_END) {
    cat("---------- \n")
    cat("Month:", month, "\n")
    cat("----------\n")
    if (month < 10) {
      m_pre = as.character(month)
      m_post = paste("0", m_pre, sep = "")
    } else {
      m_post = as.character(month)
    }
    for (day in 1:DInM[month]) {
      print(day)
      if (day < 10) {
        d_pre = as.character(day)
        d_post = paste("0", d_pre, sep = "")
      } else {
        d_post = as.character(day)
      }
      for (hour in 0:23){
        if (hour < 10) {
          h_pre = as.character(hour)
          h_post = paste("0", h_pre, sep = "")
        } else {
          h_pre = as.character(hour)
          h_post = paste(h_pre, sep = "")
        }
        
        FILE_N = paste(folder_path,ncname_gen,m_post,d_post,"T",h_post,"Z.nc",sep = "")
        
        err_test = try(nc_open(FILE_N))
        if (inherits(err_test, "try-error") == FALSE) {
          Hs_Rast = FILE_N %>% raster(varname = "hs") %>% crop(y = ALD_EXTENT) %>% {
              if (i > 1)
                stack(., Hs_Rast)
              else
                .
            }
          W_Dir_Rast = FILE_N %>% raster(varname = "dir") %>% crop(y = ALD_EXTENT)  %>% {
              if (i > 1)
                stack(., W_Dir_Rast)
              else
                .
            }
          fp_pk_fre_Rast = FILE_N %>% raster(varname = "fp") %>% crop(y = ALD_EXTENT) %>% {
              if (i > 1)
                stack(., fp_pk_fre_Rast)
              else
                .
            }
          u_wind_Rast = FILE_N %>% raster(varname = "uwnd") %>% crop(y = ALD_EXTENT) %>% {
              if (i > 1)
                stack(., u_wind_Rast)
              else
                .
            }
          v_wind_Rast = FILE_N %>% raster(varname = "vwnd") %>% crop(y = ALD_EXTENT) %>% {
            if (i > 1)
                stack(., v_wind_Rast)
              else
                .
            }
        }
        else {
          cat("Error at:", FILE_N)
          closeAllConnections()
        }
        i = i + 1
      }
    }
  }
  return = list(Hs_Rast,W_Dir_Rast,fp_pk_fre_Rast,u_wind_Rast,v_wind_Rast,i - 1)
}