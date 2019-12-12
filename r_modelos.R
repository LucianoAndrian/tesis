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
  
  v2 = array(NA, dim = c(length(lon2), length(lat2), 3, 10, 37, 4))
  
  anios = as.character(seq(from = 1982, to = 2018, by = 1))
  meses = as.character(c("11_r", "02_r", "05_r", "08_r"))
  

  #esto anda, pero tarda como 10min...
  
  
  for(m in 1:4){
      for(i in 1:37){
        t = list.files("/datos/osman/nmme/monthly", pattern = paste("tref_Amon_", nombre,"_", anios[i], meses[m], sep = ""))
        for(j in 1:length(t)){
          v = nc_open(paste(ruta, t[j], sep = "/"))
          v2[,,,j,i,m] = ncvar_get(v, "tref")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
        }               
      }
  }
  
  T =  array(NA, dim = c(length(lon2), length(lat2), 4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    T[,,i] = apply(v2[,,,,,i], c(1,2), mean)*mask 
  }
                 
  ### PP ###
  
  pp = list.files("/datos/osman/nmme/monthly", pattern = paste("prec_Amon_", nombre, sep = ""))
  
  pref = nc_open(paste(ruta, pp[1], sep = "/"))
  prec = ncvar_get(pref, names(pref$var)[1])
  
  lat = ncvar_get(pref, "Y")
  lon = ncvar_get(pref, "X")
  
  lon2 = lon[which(lon==275):which(lon==330)]
  lat2 = lat[which(lat==-60):which(lat==15)]
  
  v2 = array(NA, dim = c(length(lon2), length(lat2), 3, 10, 37, 4))
  
  
  for(m in 1:4){
    for(i in 1:37){
      pp = list.files("/datos/osman/nmme/monthly", pattern = paste("prec_Amon_", nombre,"_", anios[i], meses[m], sep = ""))
      for(j in 1:length(pp)){
        v = nc_open(paste(ruta, pp[j], sep = "/"))
        v2[,,,j,i,m] = ncvar_get(v, "prec")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
      }               
    }
  }
  
  PP =  array(NA, dim = c(length(lon2), length(lat2), 4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    PP[,,i] = apply(v2[,,,,,i], c(1,2), mean)*mask 
  }
  
  
  T_PP = list()
  T_PP[[1]]=T
  T_PP[[2]]=PP
  
  return(T_PP)
}
