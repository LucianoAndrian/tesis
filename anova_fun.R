anova_fun = function(variable){
  variable = as.character(variable)
  ensemble_total = readline("Todos los modelos?(si, no): ")

  if(ensemble_total == "si"){

    nc = nc_open(paste("/home/luciano.andrian/tesis/ncfiles/pre_anova-",variable,".nc", sep = ""))
    v_seasons = ncvar_get(nc, variable) 
    nc_close(nc)
    
    mask = as.matrix(read.table("mascara.txt"))
    
    # v_mean, v_y, etc, son los X00, Xy00 etc.
    v_mean = array(NA, dim = c(length(lon2), length(lat2), 4))
    v_y = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4))
    v_m = array(NA, dim = c(length(lon2), length(lat2), 4, 8)) 
    v_ym = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8)) 
    
    for(i in 1:4){
      v_mean[,,i] = apply(v_seasons[,,,,i,],c(1,2), mean, na.rm = T)         #ver
      v_y[,,,i] = apply(v_seasons[,,,,i,],c(1,2,4), mean, na.rm = T)
      v_m[,,i,] = apply(v_seasons[,,,,i,], c(1,2,5), mean, na.rm = T) 
      v_ym[,,,i,] = apply(v_seasons[,,,,i,],c(1,2,4,5), mean, na.rm = T)
    }
    
    # calculo de los estimadores SS's
    
    ## SSa ##
    
    aux = array(NA, dim = c(length(lon2), length(lat2), length(anios),4))
    for(i in 1:length(anios)){
      aux[,,i,] = (v_y[,,i,]-v_mean)**2
    }
    
    SSa = apply(aux, c(1,2,4), sum)
    
    
    ## SSb ##
    
    aux = array(NA, dim = c(length(lon2), length(lat2), 4, 8))
    for(i in 1:8){
      aux[,,,i] = (v_m[,,,i]-v_mean)**2
    }
    
    SSb = apply(aux, c(1,2,3), sum)
    
    
    ## SSe ##
    
    aux = array(NA, dim = c(length(lon2), length(lat2), 28, length(anios), 4, 8))
    for(i in 1:28){
      aux[,, i,,,] = (v_seasons[,,i,,,]-v_ym)**2
    }
    
    SSe = apply(aux, c(1,2,5), sum, na.rm = T)  # los NA que reemplazan los errores en los archivos de FLOR-A06, son omitidos en los calulos
    # pero se notan los valores diferentes del promedio con menos variables. 
    # vuelven aparecer las lineas en JJA (SSe[,,2]) ver image.plot(SSe[,,2])
    
    
    ## SSg ## 
    
    aux = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:8){
      aux[,,,,i] = v_ym[,,,,i] - v_y
    }
    
    aux2 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:length(anios)){
      aux2[,,i,,] = aux[,,i,,] - v_m
    }
    
    aux3 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:length(anios)){
      for(j in 1:8){
        aux3[,,i,,j] = (aux2[,,i,,j] + v_mean)**2
      }
    }
    
    SSg = apply(aux3, c(1,2,4), sum, na.rm = T)   # revisar calculos, las distintas dimensiones obligan a hacerlos por separado
    
    
    # estimadores insesgados
    
    TSS = SSa + SSb + SSg + SSe
    
    SS = list()
    SS[[1]] = SSa
    SS[[2]] = SSb
    SS[[3]] = SSg
    SS[[4]] = SSe
    SS[[5]] = TSS 
    # agregar testeos de estimadores insesgados a esta lista.
    return(SS)
    
  } else {
    nomodel = as.numeric(readline("Modelo a eliminar del ensamble. (1)COLA-CCSM4, (2)GFDL-CM2p1, (3)GFDL-FLOR-A06, (4)GFDL-FLOR-B01, (5)NASA-GEOS5, (6)NCEP-CFSv2, (7)CMC-CanCM4i, (8)CMC-CanSIPSv2: " ))
    
    nc = nc_open("/home/luciano.andrian/tesis/ncfiles/pre_anova-temp.nc")
    v_seasons = ncvar_get(nc, "temp") 
    nc_close(nc)
    
    v_seasons[,,,,,nomodel] = NA
    # crear funcion con todo esto, tiene q tener temp y pp y seleccion de un modelo para sacar del ensamble.
    
    mask = as.matrix(read.table("mascara.txt"))
    
    # temp_mean, temp_y, etc, son los X00, Xy00 etc.
    v_mean = array(NA, dim = c(length(lon2), length(lat2), 4))
    v_y = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4))
    v_m = array(NA, dim = c(length(lon2), length(lat2), 4, 8)) 
    v_ym = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8)) 
    
    for(i in 1:4){
      v_mean[,,i] = apply(v_seasons[,,,,i,],c(1,2), mean, na.rm = T)         #ver
      v_y[,,,i] = apply(v_seasons[,,,,i,],c(1,2,4), mean, na.rm = T)
      v_m[,,i,] = apply(v_seasons[,,,,i,], c(1,2,5), mean, na.rm = T) 
      v_ym[,,,i,] = apply(v_seasons[,,,,i,],c(1,2,4,5), mean, na.rm = T)
    }
    
    # calculo de los estimadores SS's
    
    ## SSa ##
    
    aux = array(NA, dim = c(length(lon2), length(lat2), length(anios),4))
    for(i in 1:length(anios)){
      aux[,,i,] = (v_y[,,i,]-v_mean)**2
    }
    
    SSa = apply(aux, c(1,2,4), sum)
    
    
    ## SSb ##
    
    aux = array(NA, dim = c(length(lon2), length(lat2), 4, 8))
    for(i in 1:8){
      aux[,,,i] = (v_m[,,,i]-v_mean)**2
    }
    
    SSb = apply(aux, c(1,2,3), sum)
    
    
    ## SSe ##
    
    aux = array(NA, dim = c(length(lon2), length(lat2), 28, length(anios), 4, 8))
    for(i in 1:28){
      aux[,, i,,,] = (v_seasons[,,i,,,]-v_ym)**2
    }
    
    SSe = apply(aux, c(1,2,5), sum, na.rm = T)  # los NA que reemplazan los errores en los archivos de FLOR-A06, son omitidos en los calulos
    # pero se notan los valores diferentes del promedio con menos variables. 
    # vuelven aparecer las lineas en JJA (SSe[,,2]) ver image.plot(SSe[,,2])
    
    
    ## SSg ## 
    
    aux = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:8){
      aux[,,,,i] = v_ym[,,,,i] - v_y
    }
    
    aux2 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:length(anios)){
      aux2[,,i,,] = aux[,,i,,] - v_m
    }
    
    aux3 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:length(anios)){
      for(j in 1:8){
        aux3[,,i,,j] = (aux2[,,i,,j] + v_mean)**2
      }
    }
    
    SSg = apply(aux3, c(1,2,4), sum, na.rm = T)   # revisar calculos, las distintas dimensiones obligan a hacerlos por separado
    
    
    # estimadores insesgados
    
    TSS = SSa + SSb + SSg + SSe
    
    SS = list()
    SS[[1]] = SSa
    SS[[2]] = SSb
    SS[[3]] = SSg
    SS[[4]] = SSe
    SS[[5]] = TSS 
    # agregar testeos de estimadores insesgados a esta lista.
    
    return(SS)
  } 
}