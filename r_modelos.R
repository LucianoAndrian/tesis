# lectura salidas de modelos.
# promedios ensambles corridas del mismo modelo, cada estacion pronosticada un mes antes.
# falta desvio


modelos_r10 = function(nombre, lon2, lat2){
  library(ncdf4)
  
  ruta = "/datos/osman/nmme/monthly"
  mask=as.matrix(read.table("mascara.txt"))
  t = list.files("/datos/osman/nmme/monthly", pattern = paste("tref_Amon_", nombre, sep = ""))
  
  tref = nc_open(paste(ruta, t[1], sep = "/"))
  temp = ncvar_get(tref, names(tref$var)[1])
  
  lat = ncvar_get(tref, "Y")
  lon = ncvar_get(tref, "X")
  
  lon2 = lon[which(lon==275):which(lon==330)]
  lat2 = lat[which(lat==-60):which(lat==15)]
  
  anios = as.character(seq(from = 1982, to = 2010, by = 1))
  meses = as.character(c("11_r", "02_r", "05_r", "08_r"))
  
  v2 = array(NA, dim = c(length(lon2), length(lat2), 3, 10, length(anios), 4))

  for(m in 1:4){
      for(i in 1:length(anios)){
        t = list.files("/datos/osman/nmme/monthly", pattern = paste("tref_Amon_", nombre,"_", anios[i], meses[m], sep = ""))
        for(j in 1:length(t)){
          v = nc_open(paste(ruta, t[j], sep = "/"))
          v2[,,,j,i,m] = ncvar_get(v, "tref")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
          nc_close(v)
        }               
      }
  }
  
  T =  array(NA, dim = c(length(lon2), length(lat2), 4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    T[,,i] = apply(v2[,,,,,i], c(1,2), mean)*mask 
  }
  
  # SD
  
  T2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
  T2[,,,i] =  apply(v2[,,,,,i], c(1,2,5), mean)
  }
  
  sd_t = array(NA, dim = c(length(lon2), length(lat2),4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    sd_t[,,i] = apply(T2[,,,i], c(1,2), sd)*mask
  }
  
  ### PP ###
  
  pp = list.files("/datos/osman/nmme/monthly", pattern = paste("prec_Amon_", nombre, sep = ""))
  
  pref = nc_open(paste(ruta, pp[1], sep = "/"))
  prec = ncvar_get(pref, names(pref$var)[1])
  
  v2 = array(NA, dim = c(length(lon2), length(lat2), 3, 10, length(anios), 4))
  
    for(m in 1:4){
    for(i in 1:length(anios)){
      pp = list.files("/datos/osman/nmme/monthly", pattern = paste("prec_Amon_", nombre,"_", anios[i], meses[m], sep = ""))
      for(j in 1:length(pp)){
        v = nc_open(paste(ruta, pp[j], sep = "/"))
        v2[,,,j,i,m] = ncvar_get(v, "prec")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]*30
        nc_close(v)
      }               
    }
  }
  
  PP =  array(NA, dim = c(length(lon2), length(lat2), 4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    PP[,,i] = apply(v2[,,,,,i], c(1,2), mean)*mask 
  }
  
  #SD
  
  PP2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    PP2[,,,i] =  apply(v2[,,,,,i], c(1,2,5), mean)
  }
  
  sd_pp = array(NA, dim = c(length(lon2), length(lat2),4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    sd_pp[,,i] = apply(PP2[,,,i], c(1,2), sd)*mask
  }
  
  
  T_PP = list()
  T_PP[[1]] = T
  T_PP[[2]] = sd_t
  T_PP[[3]] = PP
  T_PP[[4]] = sd_pp
  return(T_PP)
}








modelos_rx = function(nombre, lon2, lat2, r){
  library(ncdf4)
  
  ruta = "/datos/osman/nmme/monthly"
  mask=as.matrix(read.table("mascara.txt"))
  t = list.files("/datos/osman/nmme/monthly", pattern = paste("tref_Amon_", nombre, sep = ""))
  
  tref = nc_open(paste(ruta, t[1], sep = "/"))
  temp = ncvar_get(tref, names(tref$var)[1])
  
  lat = ncvar_get(tref, "Y")
  lon = ncvar_get(tref, "X")
  
  lon2 = lon[which(lon==275):which(lon==330)]
  lat2 = lat[which(lat==-60):which(lat==15)]
  
  anios = as.character(seq(from = 1982, to = 2010, by = 1))
  meses = as.character(c("11_r", "02_r", "05_r", "08_r"))
  
  v2 = array(NA, dim = c(length(lon2), length(lat2), 3, r, length(anios), 4))
  
  for(m in 1:4){
    for(i in 1:length(anios)){
      t = list.files("/datos/osman/nmme/monthly", pattern = paste("tref_Amon_", nombre,"_", anios[i], meses[m], sep = ""))
      for(j in 1:length(t)){
        v = nc_open(paste(ruta, t[j], sep = "/"))
        v2[,,,j,i,m] = ncvar_get(v, "tref")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
        nc_close(v)
      }               
    }
  }
  
  T =  array(NA, dim = c(length(lon2), length(lat2), 4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    T[,,i] = apply(v2[,,,,,i], c(1,2), mean)*mask 
  }
  
  # SD
  
  T2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    T2[,,,i] =  apply(v2[,,,,,i], c(1,2,5), mean)
  }
  
  sd_t = array(NA, dim = c(length(lon2), length(lat2),4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    sd_t[,,i] = apply(T2[,,,i], c(1,2), sd)*mask
  }
  
  ### PP ###
  
  pp = list.files("/datos/osman/nmme/monthly", pattern = paste("prec_Amon_", nombre, sep = ""))
  
  pref = nc_open(paste(ruta, pp[1], sep = "/"))
  prec = ncvar_get(pref, names(pref$var)[1])
  
  v2 = array(NA, dim = c(length(lon2), length(lat2), 3, r, length(anios), 4))
  
  for(m in 1:4){
    for(i in 1:length(anios)){
      pp = list.files("/datos/osman/nmme/monthly", pattern = paste("prec_Amon_", nombre,"_", anios[i], meses[m], sep = ""))
      for(j in 1:length(pp)){
        v = nc_open(paste(ruta, pp[j], sep = "/"))
        v2[,,,j,i,m] = ncvar_get(v, "prec")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]*30
        nc_close(v)
      }               
    }
  }
  
  PP =  array(NA, dim = c(length(lon2), length(lat2), 4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    PP[,,i] = apply(v2[,,,,,i], c(1,2), mean)*mask 
  }
  
  #SD
  
  PP2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    PP2[,,,i] =  apply(v2[,,,,,i], c(1,2,5), mean)
  }
  
  sd_pp = array(NA, dim = c(length(lon2), length(lat2),4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    sd_pp[,,i] = apply(PP2[,,,i], c(1,2), sd)*mask
  }
  
  
  T_PP = list()
  T_PP[[1]] = T
  T_PP[[2]] = sd_t
  T_PP[[3]] = PP
  T_PP[[4]] = sd_pp
  return(T_PP)
}
