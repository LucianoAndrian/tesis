anova_fun = function(){
  variable = as.character(readline("Variable (pp), (temp): "))
  ensemble_total = readline("Todos los modelos?(si, no): ")
  lon2 = read.table("lon2.txt")[,1]
  lat2 = read.table("lat2.txt")[,1]
  anios = seq(from = 1982, to = 2010, by = 1)
  library(ncdf4)

  if(ensemble_total == "si"){

    nc = nc_open(paste("/home/luciano.andrian/tesis/ncfiles/pre_anova-",variable,".nc", sep = ""))
    v_seasons = ncvar_get(nc, variable) # lon lat members years seasons models 
    nc_close(nc)
    
    mask = as.matrix(read.table("mascara.txt"))
    
    x000 = array(NA, dim = c(length(lon2), length(lat2), 4))
    xt00 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4))
    x0m0 = array(NA, dim = c(length(lon2), length(lat2), 4, 8))
    xtm0 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    
    for(i in 1:4){
      x000[,,i] = apply(v_seasons[,,,,i,],c(1,2), mean, na.rm = T)         
      xt00[,,,i] = apply(v_seasons[,,,,i,],c(1,2,4), mean, na.rm = T)
      x0m0[,,i,] = apply(v_seasons[,,,,i,], c(1,2,5), mean, na.rm = T) 
      xtm0[,,,i,] = apply(v_seasons[,,,,i,],c(1,2,4,5), mean, na.rm = T)
    }
    k = c(10, 10, 12, 12, 4, 28, 10, 20) #miembros de cada modelo
    t = 29 #anios
    m = 8 #modelos
    
    # calculo de los estimadores SS's
    
    ########################################### SSa ###########################################
    
    aux = array(NA, dim = c(length(lon2), length(lat2), length(anios),4,8))
    for(i in 1:length(anios)){
      aux[,,i,,] = (xt00[,,i,]-x000)**2
    }
    
    SSa = apply(sum(k)*aux, c(1,2,4), sum)
    
    
    ########################################### SSb ###########################################
    
    aux = array(NA, dim = c(length(lon2), length(lat2), 4, 8))
    for(i in 1:8){
      aux[,,,i] = (x0m0[,,,i]-x000)**2
    }
    
    for(i in 1:8){ 
      aux[,,,i] = aux[,,,i]*k[i]*t
    }
    
    SSb = apply(aux, c(1,2,3), sum)
    
    
    ########################################### SSe ###########################################
    
    aux = array(NA, dim = c(length(lon2), length(lat2), 28, length(anios), 4, 8))
    for(i in 1:28){
      aux[,, i,,,] = (v_seasons[,,i,,,]-xtm0)**2
    }
    
    SSe = apply(aux, c(1,2,5), sum, na.rm = T)  
    
    
    ########################################### SSg ########################################### 
    
    aux = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:8){
      aux[,,,,i] = xtm0[,,,,i] - xt00
    }
    
    aux2 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:length(anios)){
      aux2[,,i,,] = aux[,,i,,] - x0m0
    }
    
    aux3 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:length(anios)){
      for(j in 1:8){
        aux3[,,i,,j] = (aux2[,,i,,j] + x000)**2
      }
    }
    
    for(i in 1:8){ 
      aux3[,,,,i] = aux3[,,,,i]*k[i]    # ERROR! ESTO ESTABA MULTIPLICADO POR t. 
    }
    
    SSg = apply(aux3, c(1,2,4), sum, na.rm = T)   
    
    
    TSS = SSa + SSb + SSg + SSe
    
    SS = list()
    SS[[1]] = SSa
    SS[[2]] = SSb
    SS[[3]] = SSg
    SS[[4]] = SSe
    SS[[5]] = TSS 
    
    #cocientes
    
    c_b = (SSb - (m-1)/(t*105)*SSe)/TSS   #fraccion de TSS explicada por SSb
    
    c_a = (SSa - (t-1)/(t*105)*SSe)/TSS   #fraccion de TSS explicada por SSa 
    
    c_g = (SSg - ((t-1)*(m-1))/(t*105)*SSe)/TSS  #fraccion de TSS explicada por SSg   
    
    c_e = SSe/TSS #fraccion de ÇTSS explicada por SSe
    
    SS[[6]] = c_a
    SS[[7]] = c_b
    SS[[8]] = c_g
    SS[[9]] = c_e
    
    
    return(SS)
    
  } else {
    nomodel = as.numeric(readline("Modelo a eliminar del ensamble. (1)COLA-CCSM4, (2)GFDL-CM2p1, (3)GFDL-FLOR-A06, (4)GFDL-FLOR-B01, (5)NASA-GEOS5, (6)NCEP-CFSv2, (7)CMC-CanCM4i, (8)CMC-CanSIPSv2: " ))
    
    nc = nc_open(paste("/home/luciano.andrian/tesis/ncfiles/pre_anova-",variable,".nc", sep = ""))
    v_seasons = ncvar_get(nc, "temp") 
    nc_close(nc)
    
    v_seasons[,,,,,nomodel] = NA
    
    mask = as.matrix(read.table("mascara.txt"))
    
    x000 = array(NA, dim = c(length(lon2), length(lat2), 4))
    xt00 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4))
    x0m0 = array(NA, dim = c(length(lon2), length(lat2), 4, 8))
    xtm0 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    
    for(i in 1:4){
      x000[,,i] = apply(v_seasons[,,,,i,],c(1,2), mean, na.rm = T)         
      xt00[,,,i] = apply(v_seasons[,,,,i,],c(1,2,4), mean, na.rm = T)
      x0m0[,,i,] = apply(v_seasons[,,,,i,], c(1,2,5), mean, na.rm = T) 
      xtm0[,,,i,] = apply(v_seasons[,,,,i,],c(1,2,4,5), mean, na.rm = T)
    }
    k = c(10, 10, 12, 12, 4, 28, 10, 20) #miembros de cada modelo
    t = 29 #anios
    m = 8 #modelos
    
    k[nomodel] = NA
    
    # calculo de los estimadores SS's
    
    ########################################### SSa ########################################### 
    
    aux = array(NA, dim = c(length(lon2), length(lat2), length(anios),4,8))
    for(i in 1:length(anios)){
      aux[,,i,,] = (xt00[,,i,]-x000)**2
    }
    
    SSa = apply(sum(k, na.rm = T)*aux, c(1,2,4), sum)
    
    
    ########################################### SSb ########################################### 
    
    aux = array(NA, dim = c(length(lon2), length(lat2), 4, 8))
    for(i in 1:8){
      aux[,,,i] = (x0m0[,,,i]-x000)**2
    }
    
    for(i in 1:8){ 
      aux[,,,i] = aux[,,,i]*k[i]*t # ver si esto funca.. NAxNA
    }
    
    SSb = apply(aux, c(1,2,3), sum)
    
    
    ########################################### SSe ########################################### 
    
    aux = array(NA, dim = c(length(lon2), length(lat2), 28, length(anios), 4, 8))
    for(i in 1:28){
      aux[,, i,,,] = (v_seasons[,,i,,,]-xtm0)**2
    }
    
    SSe = apply(aux, c(1,2,5), sum, na.rm = T)  
    
    
    ########################################### SSg ########################################### 
    
    aux = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:8){
      aux[,,,,i] = xtm0[,,,,i] - xt00
    }
    
    aux2 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:length(anios)){
      aux2[,,i,,] = aux[,,i,,] - x0m0
    }
    
    aux3 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:length(anios)){
      for(j in 1:8){
        aux3[,,i,,j] = (aux2[,,i,,j] + x000)**2
      }
    }
    
    for(i in 1:8){ 
      aux3[,,,,i] = aux3[,,,,i]*k[i]*t
    }
    
    SSg = apply(aux3, c(1,2,4), sum, na.rm = T)
    
    TSS = SSa + SSb + SSg + SSe
    
    SS = list()
    SS[[1]] = SSa
    SS[[2]] = SSb
    SS[[3]] = SSg
    SS[[4]] = SSe
    SS[[5]] = TSS 
    
    #cocientes
    
    c_b = (SSb - (m-1)/(t*106)*SSe)/TSS   #fraccion de TSS explicada por SSb
    
    c_a = (SSa - (t-1)/(t*106)*SSe)/TSS   #fraccion de TSS explicada por SSa 
    
    c_g = (SSg - (t*m-1)/(t*106)*SSe)/TSS  #fraccion de TSS explicada por SSg   
    
    c_e = SSe/TSS #fraccion de ÇTSS explicada por SSe
    
    SS[[6]] = c_a
    SS[[7]] = c_b
    SS[[8]] = c_g
    SS[[9]] = c_e
    
    
    return(SS)
  } 
}