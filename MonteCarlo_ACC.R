# MonteCarlo ACC
source('funciones_MC.R')
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]

# Anomalías observadas computadas una sola vez
Obs = preObsCV()

# Anomalías del MME random

# Siguiendo el funcionamiento de aux.desemp2.R...

RACC = function(n, Op=Obs){
  
  #----------------------------------------------------------------------------#
  ###    Modelos. F(j,m) j años, m estaciones.    ###
  #----------------------------------------------------------------------------#
  modelos = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01",
              "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-GEM-NEMO") 
  modelos_random = sample(modelos, replace = T)
  
  # esto es F 
  t.mods = array(data = NA, dim = c(56, 76, 29, 4, 8)) 
  pp.mods = array(data = NA, dim = c(56, 76, 29, 4, 8))
  for(i in 1:length(modelos_random)){
    aux = mean_sd(modelos_random[i])
    t.mods[,,,,i] = aux[[5]]
    pp.mods[,,,,i] = aux[[6]]
  }  
  #--------------------------Cross Validation modelos --------------------------#
  aux = diag(29)
  aux[which(aux == 1)] = NA ; aux[which(aux == 0)] = 1
  aux2 = array(data = 1, dim = c(56, 76, 29, 4, 8, 29))
  aux3 = array(data = 1, dim = c(56, 76, 29, 4, 8, 29)) # T
  aux4 = array(data = 1, dim = c(56, 76, 29, 4, 8, 29)) # PP
  aux5 = array(data = NA, dim = c(56, 76, 29, 4, 8))
  aux6 = array(data = NA, dim = c(56, 76, 29, 4, 8))
  
  for(i in 1:29){
    # una especie de matriz identidad inversa con NA y 1 pero en 4 dim.
    aux2[,,i,,,i] = aux2[,,i,,,i]*aux[i,i]  
    aux3[,,,,,i] = aux2[,,,,,i]*t.mods
    aux4[,,,,,i] = aux2[,,,,,i]*pp.mods
    
    # promedio sacando cada anio
    aux5[,,i,,] = apply(aux3[,,,,,i], c(1, 2, 4, 5), mean, na.rm = T)   
    aux6[,,i,,] = apply(aux4[,,,,,i], c(1, 2, 4, 5), mean, na.rm = T)
  }
  #----------------------------------------------------------------------------#
  # ACC -----------------------------------------------------------------------#
  # Actual ACC
  t.Fp = t.mods - aux5
  t.Fp_ens = apply(t.Fp, c(1,2,3,4), mean, na.rm = T)
  
  pp.Fp = pp.mods - aux6
  pp.Fp_ens = apply(pp.Fp, c(1,2,3,4), mean, na.rm = T)
  
  # Temp
  aux = Op[,,1:29,,1]*t.Fp_ens#[,,,,i] 
  num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
  den = ((apply(Op[,,,,1]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(t.Fp_ens**2, c(1, 2, 4), sum, na.rm = T))/29) 
  t.ACC = num/sqrt(den)
  
  # PP
  pp.ACC = array(data = NA, dim = c(56, 76, 4, 3))
  for(j in 2:4){ # las 3 bases de datos
    
    aux = Op[,,1:29,,j]*pp.Fp_ens#[,,,,i]
    num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
    
    den = ((apply(Op[,,,,j]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(pp.Fp_ens**2, c(1, 2, 4), sum, na.rm = T))/29)
    
    pp.ACC[,,,j-1] = num/sqrt(den)
    
  }
  
  rc = qt(p = 0.95,df = 29-1)/sqrt((29-1)+qt(p = 0.95,df = 29-1))
  
  resultados = list()
  resultados[[1]] = t.ACC
  resultados[[2]] = pp.ACC
  resultados[[3]] = rc
  
  #----------------------------------------------------------------------------#
  # ACC teorico
  
  ##### Ensamble sacando modelos ####
  
  t.Fp_oneless = array(data = NA, dim = c(length(lon2), length(lat2), 29, 4, 8))
  pp.Fp_oneless = array(data = NA, dim = c(length(lon2), length(lat2), 29, 4, 8))
  
  for(i in 1:8){
    aux = t.Fp
    aux[,,,,i] = NA
    t.Fp_oneless[,,,,i] = apply(aux, c(1,2,3,4), mean, na.rm = T)  # obtengo 8 ensambles en los que falta un modelo
    
    aux2 = pp.Fp
    aux2[,,,,i] = NA
    pp.Fp_oneless[,,,,i] = apply(aux2, c(1,2,3,4), mean, na.rm = T)
  }  
  
  #
  t.ACC_ens_vs_mods = array(data = NA, c(length(lon2), length(lat2), 4, 8))
  pp.ACC_ens_vs_mods = array(data = NA, c(length(lon2), length(lat2), 4, 8))
  
  for(i in 1:8){
    aux = t.Fp_oneless[,,,,i]*t.Fp[,,,,i]
    num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
    den = ((apply(t.Fp[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(t.Fp_oneless[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) 
    t.ACC_ens_vs_mods[,,,i] = num/sqrt(den)
    
    aux = pp.Fp_oneless[,,,,i]*pp.Fp[,,,,i]
    num = (apply(aux, c(1, 2, 4), sum, na.rm = T))/29 
    den = ((apply(pp.Fp[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) * ((apply(pp.Fp_oneless[,,,,i]**2, c(1, 2, 4), sum, na.rm = T))/29) 
    pp.ACC_ens_vs_mods[,,,i] = num/sqrt(den)
  }
  
  #--- promedio de los ACC teo ----#
  t.ACC_teo_prom = apply(t.ACC_ens_vs_mods, c(1,2,3), mean, na.rm = T)
  pp.ACC_teo_prom = apply(pp.ACC_ens_vs_mods, c(1,2,3), mean, na.rm = T)
  
  resultados[[6]] = t.ACC_teo_prom
  resultados[[7]] = pp.ACC_teo_prom
  
  ##############################################################################
  obs_minus_teo = array(data = NA, c(length(lon2), length(lat2), 4,2))
  
  obs_minus_teo[,,,1] = resultados[[1]] - resultados[[6]]
  obs_minus_teo[,,,2] = resultados[[2]][,,,3] - resultados[[7]]
  
  WriteNC_ACC(obs_minus_teo, paste('diffACC', n, sep = ""))
  ##############################################################################
}

require(parallel)

j=1
while(j<10000){
  x = seq(j,(j+9))
  j = j + 10
  
  h = format(Sys.time(), "%H")
  d = format(Sys.time(), "%d") 
  if (d < 18){ # solo valido para julio 2022 XD
    cores = 10
  } else {
    if (as.integer(h) >= 19 | as.integer(h) <= 8){
      cores = 10
    } else {
      cores = 5
    }
  }
  # VEER no ocupa tanto ram como ANOVA
  # 
  mclapply(x,RACC, mc.cores = cores, mc.preschedule = F)
}