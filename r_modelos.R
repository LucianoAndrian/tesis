# lectura salidas de modelos.
# promedios ensambles corridas del mismo modelo, cada estacion pronosticada un mes antes.
# falta desvio


modelos_r10 = function(nombre, lon2, lat2){
  library(ncdf4)
  
  ruta = "/datos/osman/nmme/monthly"
  mask=as.matrix(read.table("mascara.txt"))
  t = list.files("/datos/osman/nmme/monthly", pattern = paste("tref_Amon_", nombre, sep = ""))
  
                 
  tref = nc_open(paste(ruta,t[1], sep = "/"))
   
  # para obtener lon2 y lat2 desde el mismo modelo              
  temp = ncvar_get(tref, names(tref$var)[1])
                 
  lat = ncvar_get(tref, "Y")
  lon = ncvar_get(tref, "X")
                 
  lon2 = lon[which(lon==275):which(lon==330)]
  lat2 = lat[which(lat==-60):which(lat==15)]
                 
  # 3era dimension leads               
  v = array(NA, dim = c(length(lon2), length(lat2), 12))
  
  # 3era dim meses estaciones, 4ta dim runs, 5ta dim estaciones                
  v2 = array(NA, dim = c(length(lon2), length(lat2), 3, 10, 4))
                 
  for(i in 0:3){ # i pasa de en los meses previos a las estaciones nov, feb, may, ago
    for(j in 0:9){ # j las corridas
      v = nc_open(paste(ruta, t[(101+j+30*i)], sep = "/"))
      temp = ncvar_get(v, "tref")
      temp = temp[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),] #****
      v2[,,,j+1,i+1] = temp[,,2:4]
    }
  }
                 

  T =  array(NA, dim = c(length(lon2), length(lat2), 4)) #****   si hay diferencia va tirar error
  for(i in 1:4){
    T[,,i] = apply(v2[,,,,i], c(1,2), mean)*mask 
  }
                 
  
  ### PP ###
  
  pp = list.files("/datos/osman/nmme/monthly", pattern = paste("prec_Amon_", nombre, sep=""))
                 
  v = array(NA, dim = c(length(lon2), length(lat2), 12))
                 
  v2 = array(NA, dim = c(length(lon2), length(lat2), 3, 10, 4))
                 
  for(i in 0:3){
    for(j in 0:9){
      v = nc_open(paste(ruta, pp[(101+j+30*i)], sep = "/"))
      prec = ncvar_get(v, "prec")
      prec = prec[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),] 
      v2[,,,j+1,i+1] = prec[,,2:4]
    }
  }
                 
                 
  PP =  array(NA, dim = c(length(lon2), length(lat2), 4))
  for(i in 1:4){
    PP[,,i] = apply(v2[,,,,i], c(1,2), mean)*mask
  }
  
  T_PP = list()
  T_PP[[1]]=T
  T_PP[[2]]=PP
  
  return(T_PP)
}
