

modelos_rx2 = function(nombre, r){
  library(ncdf4)
  
  
  if(r<28){
    ruta = "/datos/osman/nmme/monthly"
    mask=as.matrix(read.table("mascara.txt"))
    t = list.files("/datos/osman/nmme/monthly", pattern = paste("tref_Amon_", nombre, sep = ""))
    
    tref = nc_open(paste(ruta, t[1], sep = "/"))
    temp = ncvar_get(tref, names(tref$var)[1])
    
    lat = ncvar_get(tref, "Y")
    lon = ncvar_get(tref, "X")
    
    lon2 = lon[which(lon==275):which(lon==330)]
    lat2 = lat[which(lat==-60):which(lat==15)]
    
    #anios = as.character(seq(from = 1982, to = 2010, by = 1))
    anios= seq(from = 1982, to = 2010, by = 1)
    #meses = as.character(c("11_r", "02_r", "05_r", "08_r"))
    
    v2 = array(NA, dim = c(length(lon2), length(lat2), 3, r, length(anios), 4))
    
    init_cond=c(2,5,8,11)
    
    for(m in 1:4){
      final_month = init_cond[m] - 1
      
      ic_format=formatC(init_cond[m], width = 2, format = "d", flag = "0")
      
      for(i in 1:length(anios)){
        
        file_pattern = paste('tref_Amon_',nombre,"_",anios[i] , ic_format,"_r*_", as.character(anios[i]),ic_format, '-' , 
                             as.character(anios[i] + 1), ic_format=formatC(final_month, width = 2, format = "d", flag = "0") ,'.nc',sep="")
        
        t = Sys.glob(paste("/datos/osman/nmme/monthly", file_pattern, sep = "/"))
        
        for(j in 1:length(t)){
          
          v = nc_open(t[j])
          
          v2[,,,j,i,m] = ncvar_get(v, "tref")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
          
          nc_close(v)
        }               
      }
    }
    
    v2[which(v2<=0)]=NA 
    
    londim = ncdim_def("lon", "grados_este", as.double(lon2))
    latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
    timedim = ncdim_def("time", "Years", as.double(anios))
    mesesdim = ncdim_def("meses", "meses", as.double(1:3))
    rdim = ncdim_def("r","miembros", as.double(1:r))
    tempdim = ncdim_def("temp", "°C", as.double(v2))
    seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))
  
    
    fillvalue = NA
    dlname = "temperatura"
    temp_def = ncvar_def("temp", "Kelvin", list(londim, latdim, mesesdim, rdim, timedim, seasondim ), fillvalue, dlname, prec="single")
    
    ncfname = paste(nombre, "-temp", ".nc", sep = "")
    
    ncout = nc_create(ncfname, list(temp_def), force_v4=T)
    
    ncvar_put(ncout, temp_def, v2)
    
    #ncatt_put(ncout, "lon", "lat", "meses", "r", "anios", "season") #esto nse que hace pero no anda
    
    nc_close(ncout) #verificar donde guarda los nc
    
    
    #T1 =  array(NA, dim = c(length(lon2), length(lat2), 4)) 
    #for(i in 1:4){
    #  T1[,,i] = apply(v2[,,,,,i], c(1,2), mean, na.rm = TRUE)*mask  # poner TRUE xq T lo toma como si fuera el array...
    #}
    
    # SD
    
    #T2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4))
    #for(i in 1:4){
    #  T2[,,,i] =  apply(v2[,,,,,i], c(1,2,5), mean, na.rm = TRUE)
    #}
    
    #sd_t = array(NA, dim = c(length(lon2), length(lat2),4)) 
    #for(i in 1:4){
    #  sd_t[,,i] = apply(T2[,,,i], c(1,2), sd, na.rm = TRUE)*mask
    #}
    
    
    ### PP ###
    
    v2 = array(NA, dim = c(length(lon2), length(lat2), 3, r, length(anios), 4))
    
    for(m in 1:4){
      final_month = init_cond[m] - 1
      
      ic_format=formatC(init_cond[m], width = 2, format = "d", flag = "0")
      
      for(i in 1:length(anios)){
        
        file_pattern = paste('prec_Amon_',nombre,"_",anios[i] , ic_format,"_r*_", as.character(anios[i]),ic_format, '-' , 
                             as.character(anios[i] + 1), ic_format=formatC(final_month, width = 2, format = "d", flag = "0") ,'.nc',sep="")
        
        t = Sys.glob(paste("/datos/osman/nmme/monthly", file_pattern, sep = "/"))
        
        for(j in 1:length(t)){
          
          v = nc_open(t[j])
          
          v2[,,,j,i,m] = ncvar_get(v, "prec")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]*30
          
          nc_close(v)
        }               
      }
    }
    
    v2[which(v2<0)]=NA
    
    londim = ncdim_def("lon", "grados_este", as.double(lon2))
    latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
    timedim = ncdim_def("time", "Years", as.double(anios))
    mesesdim = ncdim_def("meses", "meses", as.double(1:3))
    rdim = ncdim_def("r","miembros", as.double(1:r))
    tempdim = ncdim_def("PP", "mm x day", as.double(v2))
    seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))
    
    
    fillvalue = NA
    dlname = "precipitacion"
    temp_def = ncvar_def("pp", "mm x dia", list(londim, latdim, mesesdim, rdim, timedim, seasondim ), fillvalue, dlname, prec="single")
    
    ncfname = paste(nombre, "-pp", ".nc", sep = "")
    
    ncout = nc_create(ncfname, list(temp_def), force_v4=T)
    
    ncvar_put(ncout, temp_def, v2)
    
    #ncatt_put(ncout, "lon", "lat", "meses", "r", "anios", "season") #esto nse que hace pero no anda
    
    nc_close(ncout) #verificar donde guarda los nc
    
    
    # ACA GENERAR NCFD PARA GUARDAR!! 
    
    #PP =  array(NA, dim = c(length(lon2), length(lat2), 4)) 
    #for(i in 1:4){
    #  PP[,,i] = apply(v2[,,,,,i], c(1,2), mean, na.rm = TRUE)*mask 
    #}
    
    #SD
    
    #PP2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4)) 
    #for(i in 1:4){
    #  PP2[,,,i] =  apply(v2[,,,,,i], c(1,2,5), mean, na.rm = TRUE)
    #}
    
    #sd_pp = array(NA, dim = c(length(lon2), length(lat2),4)) 
    #for(i in 1:4){
    #  sd_pp[,,i] = apply(PP2[,,,i], c(1,2), sd, na.rm = TRUE)*mask
    #}
    
    
    #T_PP = list()
    #T_PP[[1]] = T1
    #T_PP[[2]] = sd_t
    #T_PP[[3]] = PP
    #T_PP[[4]] = sd_pp
    #return(T_PP)
    
  } else {
    ruta = "/datos/osman/nmme/monthly"
    mask=as.matrix(read.table("mascara.txt"))
    t = list.files("/datos/osman/nmme/monthly", pattern = paste("tref_Amon_", nombre, sep = ""))
    
    tref = nc_open(paste(ruta, t[1], sep = "/"))
    temp = ncvar_get(tref, names(tref$var)[1])
    
    lat = ncvar_get(tref, "Y")
    lon = ncvar_get(tref, "X")
    
    lon2 = lon[which(lon==275):which(lon==330)]
    lat2 = lat[which(lat==-60):which(lat==15)]
    
    #anios = as.character(seq(from = 1982, to = 2010, by = 1))
    anios= seq(from = 1982, to = 2010, by = 1)
    #meses = as.character(c("11_r", "02_r", "05_r", "08_r"))
    
    v2 = array(NA, dim = c(length(lon2), length(lat2), 3, r, length(anios), 4))
    
    init_cond=c(2,5,8,11)
    
    for(m in 1:4){
      final_month = init_cond[m] - 1
      
      ic_format=formatC(init_cond[m], width = 2, format = "d", flag = "0")
      
      for(i in 1:length(anios)){
        
        file_pattern = paste('tref_Amon_',nombre,"_",anios[i] , ic_format,"_r*_", as.character(anios[i]),ic_format, '-' , 
                             as.character(anios[i] + 1), ic_format=formatC(final_month, width = 2, format = "d", flag = "0") ,'.nc',sep="")
        
        t = Sys.glob(paste("/datos/osman/nmme/monthly", file_pattern, sep = "/"))
        
        for(j in 1:(length(t)-4)){
          
           if(j>=18){ 
            v = nc_open(t[j+4]) # saltea r25 a r28 (en el orden que quedan en t. quedan igual para todos los modelos q llegan hasta 28)
            v2[,,,j,i,m] = ncvar_get(v, "tref")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
          } else {
            v = nc_open(t[j])
            v2[,,,j,i,m] = ncvar_get(v, "tref")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
          }
          
          nc_close(v)
        }               
      }
    }
    
    v2[which(v2<=0)]=NA 
    
    londim = ncdim_def("lon", "grados_este", as.double(lon2))
    latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
    timedim = ncdim_def("time", "Years", as.double(anios))
    mesesdim = ncdim_def("meses", "meses", as.double(1:3))
    rdim = ncdim_def("r","miembros", as.double(1:r))
    tempdim = ncdim_def("temp", "kelvin", as.double(v2))
    seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))
    
    
    fillvalue = NA
    dlname = "temperatura"
    temp_def = ncvar_def("temp", "Kelvin", list(londim, latdim, mesesdim, rdim, timedim, seasondim ), fillvalue, dlname, prec="single")
    
    ncfname = paste(nombre, "-temp", ".nc", sep = "")
    
    ncout = nc_create(ncfname, list(temp_def), force_v4=T)
    
    ncvar_put(ncout, temp_def, v2)
    
    #ncatt_put(ncout, "lon", "lat", "meses", "r", "anios", "season") #esto nse que hace pero no anda
    
    nc_close(ncout) #verificar donde guarda los nc
    
    ### seria bueno convertir todo lo anterior en funcion ###
    
    #T1 =  array(NA, dim = c(length(lon2), length(lat2), 4)) 
    #for(i in 1:4){
    #  T1[,,i] = apply(v2[,,,,,i], c(1,2), mean, na.rm = TRUE)*mask  
    #}
  
    #  
    # SD
    
    #T2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4))
    #for(i in 1:4){
    #  T2[,,,i] =  apply(v2[,,,,,i], c(1,2,5), mean, na.rm = TRUE)
    #}
    
    #sd_t = array(NA, dim = c(length(lon2), length(lat2),4)) 
    #for(i in 1:4){
    #  sd_t[,,i] = apply(T2[,,,i], c(1,2), sd, na.rm = TRUE)*mask
    #}
    
    
    ### PP ###
    
    v2 = array(NA, dim = c(length(lon2), length(lat2), 3, r, length(anios), 4))
    
    for(m in 1:4){
      final_month = init_cond[m] - 1
      
      ic_format=formatC(init_cond[m], width = 2, format = "d", flag = "0")
      
      for(i in 1:length(anios)){
        
        file_pattern = paste('prec_Amon_',nombre,"_",anios[i] , ic_format,"_r*_", as.character(anios[i]),ic_format, '-' , 
                             as.character(anios[i] + 1), ic_format=formatC(final_month, width = 2, format = "d", flag = "0") ,'.nc',sep="")
        
        t = Sys.glob(paste("/datos/osman/nmme/monthly", file_pattern, sep = "/"))
        
        for(j in 1:(length(t)-4)){
          
          if(j>=18){
            v = nc_open(t[j+4])
            v2[,,,j,i,m] = ncvar_get(v, "prec")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
          } else {
            v = nc_open(t[j])
            v2[,,,j,i,m] = ncvar_get(v, "prec")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]*30
          }
          nc_close(v)
        }               
      }
    }
    
    v2[which(v2<0)]=NA
    
    
    londim = ncdim_def("lon", "grados_este", as.double(lon2))
    latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
    timedim = ncdim_def("time", "Years", as.double(anios))
    mesesdim = ncdim_def("meses", "meses", as.double(1:3))
    rdim = ncdim_def("r","miembros", as.double(1:r))
    tempdim = ncdim_def("PP", "mm x day", as.double(v2))
    seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))
    
    
    fillvalue = NA
    dlname = "precipitacion"
    temp_def = ncvar_def("pp", "mm x dia", list(londim, latdim, mesesdim, rdim, timedim, seasondim ), fillvalue, dlname, prec="single")
    
    ncfname = paste(nombre, "-pp", ".nc", sep = "")
    
    ncout = nc_create(ncfname, list(temp_def), force_v4=T)
    
    ncvar_put(ncout, temp_def, v2)
    
    nc_close(ncout) #verificar donde guarda los nc
    
    
    #PP =  array(NA, dim = c(length(lon2), length(lat2), 4)) 
    #for(i in 1:4){
    #  PP[,,i] = apply(v2[,,,,,i], c(1,2), mean, na.rm = TRUE)*mask 
    #}
    
    #SD
    
    #PP2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4)) 
    #for(i in 1:4){
    #  PP2[,,,i] =  apply(v2[,,,,,i], c(1,2,5), mean, na.rm = TRUE)
    #}
    
    #sd_pp = array(NA, dim = c(length(lon2), length(lat2),4)) 
    #for(i in 1:4){
    #  sd_pp[,,i] = apply(PP2[,,,i], c(1,2), sd, na.rm = TRUE)*mask
    #}
    
    
    #T_PP = list()
    #T_PP[[1]] = T1
    #T_PP[[2]] = sd_t
    #T_PP[[3]] = PP
    #T_PP[[4]] = sd_pp
    #return(T_PP)
  }
}




mean_sd = function(nombre){
  
  ruta =  "/home/luciano.andrian/tesis/"
  
  mask=as.matrix(read.table("mascara.txt"))
  
  anios= seq(from = 1982, to = 2010, by = 1)
  ### TEMP ###
  
  nc = nc_open(paste(ruta, nombre, "-temp.nc", sep = ""))
  
  temp = ncvar_get(nc, "temp")
  lon = ncvar_get(nc, "lon")
  lat = ncvar_get(nc, "lat")
  T1 =  array(NA, dim = c(length(lon2), length(lat2), 4)) 
  for(i in 1:4){
    T1[,,i] = apply(temp[,,,,,i], c(1,2), mean, na.rm = TRUE)*mask  
  }
 
  # SD
 
  T2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4))
  for(i in 1:4){
    T2[,,,i] =  apply(temp[,,,,,i], c(1,2,5), mean, na.rm = TRUE)
  }
  
  sd_t = array(NA, dim = c(length(lon2), length(lat2),4)) 
  for(i in 1:4){
    sd_t[,,i] = apply(T2[,,,i], c(1,2), sd, na.rm = TRUE)*mask
  }
  
  
  ### PP ###
  
  nc = nc_open(paste(ruta, nombre, "-pp.nc", sep = ""))
  
  PP = ncvar_get(nc, "pp")
  lon = ncvar_get(nc, "lon")
  lat = ncvar_get(nc, "lat")
  PP1 =  array(NA, dim = c(length(lon2), length(lat2), 4)) 
  for(i in 1:4){
    PP1[,,i] = apply(PP[,,,,,i], c(1,2), mean, na.rm = TRUE)*mask  
  }
  
  # SD
  
  PP2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4))
  for(i in 1:4){
    PP2[,,,i] =  apply(PP[,,,,,i], c(1,2,5), mean, na.rm = TRUE)
  }
  
  sd_pp = array(NA, dim = c(length(lon2), length(lat2),4)) 
  for(i in 1:4){
    sd_pp[,,i] = apply(PP2[,,,i], c(1,2), sd, na.rm = TRUE)*mask
  }
  
  T_PP = list()
  T_PP[[1]] = T1
  T_PP[[2]] = sd_t
  T_PP[[3]] = PP1
  T_PP[[4]] = sd_pp
  return(T_PP)
  
}
