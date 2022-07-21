# Funciones similares a las de funciones.R, con random_esemble y random_k como
# argumentos y sin opciones  sin_mod
################################################################################
anova_fun_MC = function(variable, random_ensemble, random_k){
  library(ncdf4)
  # k = c(10, 10, 12, 12, 4, 28, 10, 12)
  # t = 29 #anios
  lon2 = read.table("lon2.txt")[,1]
  lat2 = read.table("lat2.txt")[,1]
  
  anios = seq(from = 1982, to = 2010, by = 1)
  

  m = length(random_ensemble)
  if (m!=8){
    break
  }
  
  k = random_k
  t = length(anios)
  
  nc = nc_open(paste("/home/luciano.andrian/tesis/ncfiles/pre_anova-",variable,".nc", sep = ""))
  v_seasons = ncvar_get(nc, variable) # lon lat members years seasons models 
  nc_close(nc)
  
  random_v_seasons = array(NA, dim = c(dim(v_seasons)))
  for (i in 1:8){
    model = random_ensemble[i]
    random_v_seasons[,,,,,i] = v_seasons[,,,,,model]
  }
  
  v_seasons = random_v_seasons
  
  # dim v_seasons[lon, lat, max_miembros*, anios, seasons, modelos]
  # *los que tienen menos r dejan el resto en NA
  
  mask = as.matrix(read.table("mascara.txt"))
  
  # Hodson - Sutton
  
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
  
  c_b = (SSb - (m-1)/(t*sum(k-1))*SSe)/TSS   #fraccion de TSS explicada por SSb
  
  c_a = (SSa - (t-1)/(t*sum(k-1))*SSe)/TSS   #fraccion de TSS explicada por SSa 
  
  c_g = (SSg - ((t-1)*(m-1))/(t*sum(k-1))*SSe)/TSS  #fraccion de TSS explicada por SSg   
  
  c_e = SSe/TSS #fraccion de ÇTSS explicada por SSe
  
  SS[[6]] = c_a
  SS[[7]] = c_b
  SS[[8]] = c_g
  SS[[9]] = c_e
  
  
  return(SS)
}  
################################################################################
test_cos_MC = function(SS, random_ensemble, random_k){
  
  
  #### testeos ####
  
  # crea una mascara para graficar los cocientes SS
  
  #SS[[1]] = SSa
  #SS[[2]] = SSb
  #SS[[3]] = SSg
  #SS[[4]] = SSe
  #SS[[5]] = TSS 
  
  #SS[[6]] = c_a
  #SS[[7]] = c_b
  #SS[[8]] = c_g
  #SS[[9]] = c_e
  
  mask = as.matrix(read.table("mascara.txt"))
  mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
  for(i in 1:4){
    mask_arr[,,i] = mask
  }
  
  anios = seq(from = 1982, to = 2010, by = 1)
  
  m = length(random_ensemble)
  if (m!=8){
    break
  }
  
  k = random_k
  t = length(anios)
  
  
  alpha_f = qf(0.95,t-1,t*(sum(k-1)))
  beta_f = qf(0.95, m-1, t*(sum(k-1)))
  gamma_f = qf(0.95, (m-1)*(t-1), t*(sum(k-1))) 
  
  sigma_alpha_2 = SS[[1]]/((t-1)) 
  
  sigma_beta_2 = SS[[2]]/(m-1) 
  
  sigma_gamma_2 = SS[[3]]/((t-1)*(m-1))  
  
  sigma_epsilon_2 = SS[[4]]/(t*(sum(k-1)))     
  
  alpha = (sigma_alpha_2/sigma_epsilon_2)*mask_arr
  beta = (sigma_beta_2/sigma_epsilon_2)*mask_arr
  gamma = (sigma_gamma_2/sigma_epsilon_2)*mask_arr
  
  # esta OK.
  alpha[which(alpha<alpha_f)] = NA
  alpha[which(!is.na(alpha))] = 1
  
  beta[which(beta<beta_f)] = NA
  beta[which(!is.na(beta))] = 1
  
  gamma[which(gamma<gamma_f)] = NA
  gamma[which(!is.na(gamma))] = 1
  
  sig = list()
  sig[[1]] = alpha
  sig[[2]] = beta
  sig[[3]] = gamma
  sig[[4]] = array(data = 1, dim = c(56, 76, 4))*mask_arr
  
  sig[[5]] = sigma_alpha_2
  sig[[6]] = sigma_beta_2
  sig[[7]] = sigma_gamma_2
  sig[[8]] = sigma_epsilon_2
  sig[[9]] = gamma_f
  return(sig)
}
################################################################################
pp_test_MC = function(ss_v, random_ensemble, random_k){
  
  mask=as.matrix(read.table("mascara.txt"))
  mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
  for(i in 1:4){
    mask_arr[,,i] = mask
  }
  
  anios = seq(from = 1982, to = 2010, by = 1)
  
  m = length(random_ensemble)
  if (m!=8){
    break
  }
  
  k = random_k
  t = length(anios)
  
  
  f = qf(0.95, t-1, t*sum(k-1))
  pp_f = 1/(1+(sum(k)/(f-1)))
  
  
  aux_pp_v = (ss_v[[1]]/ss_v[[4]])*((t*sum(k-1))/(t-1))
  pp_v = 1/(1+((sum(k))/(aux_pp_v-1)))
  
  pp_v_sig = pp_v
  
  # testear y mascara
  pp_v[which(pp_v<pp_f)] = NA # saco los que no son significativos
  
  pp_v_sig[which(pp_v_sig<pp_f)] = NA
  pp_v_sig[which(!is.na(pp_v))] = 1 
  
  pp_v = pp_v*mask_arr #agrego mascara de continente a todas las estaciones
  pp_v_sig = pp_v_sig*mask_arr 
  
  
  pp = list()
  pp[[1]] = pp_v
  pp[[2]] = pp_v_sig
  return (pp)
}

################################################################################
#Escribe las salidas de SS y PPot como nc
################################################################################
WriteNC = function(tosave, paste_name){
  
  if (is.list(tosave)){
    ss_arr = array(NA, dim = c(length(lon2), length(lat2), 4,9))
    for (i in 1:length(tosave)){
      ss_arr[,,,i] = tosave[[i]]
    }
  } else {
    ss_arr = tosave
  }
  
  
  londim = ncdim_def("lon", "grados_este", as.double(lon2))
  latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
  seasondim = ncdim_def("seasons", "season", as.double(1:4))
  termdim = ncdim_def("term", "term", as.double(1:9))
  #ssdim = ncdim_def("SS", "SS", as.double(ss_arr))
  
  fillvalue = NA
  dlname = "SSterms"
  v_def = ncvar_def("SSterms", "", list(londim, latdim, seasondim, termdim), fillvalue, dlname, prec="single")
  ncfname = paste("/pikachu/datos/luciano.andrian/aux_nmme_quantiles/", paste_name, ".nc", sep = "")
  
  ncout = nc_create(ncfname, list(v_def), force_v4=T)
  
  ncvar_put(ncout, v_def, ss_arr)
  
  nc_close(ncout)
}
################################################################################
################################################################################
################################################################################
# Funciones ACC. Similar a aux.desemp2.R
################################################################################
preObsCV = function(){
  library(ncdf4)
  library(fields)
  source("funciones.R")
  #mask = as.matrix(read.table("mascara.txt"))
  
  # Temp - CPC ----------------------------------------------------------------#
  ruta = "/pikachu/datos/osman/nmme/monthly"
  
  tref = nc_open(paste(ruta,"tref_monthly_nmme_ghcn_cams.nc", sep = "/"))
  names(tref$var)
  temp = ncvar_get(tref, "tref")
  lat = ncvar_get(tref, "Y")
  lon = ncvar_get(tref, "X")
  nc_close(tref)
  
  temp = temp[which(lon==275):which(lon==330), which(lat==-60):which(lat==15), 3:371] 
  
  lon2 = lon[which(lon==275):which(lon==330)]  # se usan las mismas en PP
  lat2 = lat[which(lat==-60):which(lat==15)]   #
  
  temp_estaciones = array(NA, dim = c(length(lon2), length(lat2), 30, 12))
  
  for(j in 1:12){
    for (i in 0:29){
      temp_estaciones[,,1+i,j] = temp[ , , j+12*i]
    }
  }
  
  # Estaciones
  
  prom_est_cpc_t = array(NA, dim = c(length(lon2), length(lat2), 30, 4))
  i=1
  while(i<=4){
    prom_est_cpc_t[,,,i] = apply(temp_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
    i = i + 1
  }
  # PP - CMAP -----------------------------------------------------------------#
  aux = nc_open("/home/luciano.andrian/tesis/ncfiles/X190.191.242.210.56.5.48.49.nc")
  #aux2 = ncvar_get(aux, "precip")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),]
  lon = ncvar_get(aux, "lon")
  lat = ncvar_get(aux, "lat")
  aux2 = ncvar_get(aux, "precip")[,,27:386]
  nc_close(aux)
  
  lon2 = lon
  lat2 = lat
  
  pp3_int = array(NA, dim = c(58, 78, 360)) # esta quedo con mayor latitud y longitud ya que sino queda mas chico debido a la grilla 2.5x2.5
  
  #Interpolado
  for(i in 1:360){  
    mod = list(x = lon2, y = lat2, z = aux2[,,i])
    grid = list(x=seq(min(lon2), max(lon2), by = 1), y = seq(min(lat2), max(lat2), by = 1))
    pp_aux = interp.surface.grid(obj = mod, grid.list = grid)
    pp3_int[,,i] = pp_aux$z  
  }
  
  
  pp3_estaciones = array(NA, dim = c(58, 78, 30, 12))
  
  for(j in 1:12){
    for (i in 0:29){
      pp3_estaciones[,,1+i,j] = pp3_int[1:58 , 1:78, j+12*i]
    }
  }
  
  prom_est_cmap_pp = array(NA, dim = c(58, 78, 30, 4))
  i=1
  while(i<=4){
    prom_est_cmap_pp[,,,i] = apply(pp3_estaciones[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)*30 # esta en mm/day
    i = i + 1
  }
  
  #----------------------------------------------------------------------------#
  # Mantengo las 4 dimenciones finales para no modificar lo que ya funciona
  
  # uso misma cantidad de años que los modelos
  datos.obs = array(data = NA, dim = c(56, 76, 29, 4, 4)) 
  datos.obs[,,,,1] = prom_est_cpc_t[,,1:29,] 
  # datos.obs[,,,,2] = prom_est_cpc_pp[,,1:29,]
  # datos.obs[,,,,3] = prom_est_gpcc_pp[,,1:29,]
  # este tenia + lats y lons por el grillado
  datos.obs[,,,,4] = prom_est_cmap_pp[2:57,2:77,1:29,]  
  
  
  # Cross Validation  datos.obs -----------------------------------------------#
  # 
  # para cada año tengo q tener promedio de todos los años menos ese año.
  
  aux = diag(29)
  aux[which(aux == 1)] = NA ; aux[which(aux == 0)] = 1
  
  aux2 = array(data = 1, dim = c(56, 76, 29, 4, 29, 4))  
  aux2.obs = array(data = 1, dim = c(56, 76, 29, 4, 29, 4))
  
  # para las 4 base de datos, la 1era temp y las otras pp
  cv.obs = array(data = NA, dim = c(56, 76, 29, 4, 4)) 
  
  for(i in 1:29){
    
    # como matriz identidad inversa con NA en la diagonal y 1 pero en 4 dimenciones.
    aux2[,,i,,i,] = aux2[,,i,,i,]*aux[i,i] 
    aux2.obs[,,,,i,] = aux2[,,,,i,]*datos.obs
    
    # promedio sacando cada año.
    cv.obs[,,i,,] = apply(aux2.obs[,,,,i,], c(1,2,4,5), mean, na.rm = T)
  }
  
  # O'-------------------------------------------------------------------------# 
  Op = datos.obs - cv.obs
  return (Op)
}
################################################################################
WriteNC_ACC = function(tosave, paste_name){
  ss_arr = tosave
  
  londim = ncdim_def("lon", "grados_este", as.double(lon2))
  latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
  seasondim = ncdim_def("seasons", "season", as.double(1:4))
  vdim = ncdim_def("v", "v", as.double(1:2))
  
  
  fillvalue = NA
  dlname = "ACCv"
  v_def = ncvar_def("ACCv", "", list(londim, latdim, seasondim, vdim), fillvalue, dlname, prec="single")
  ncfname = paste("/pikachu/datos/luciano.andrian/ACC_aux_nmme_quantiles/", paste_name, ".nc", sep = "")
  
  ncout = nc_create(ncfname, list(v_def), force_v4=T)
  
  ncvar_put(ncout, v_def, ss_arr)
  
  nc_close(ncout)
}

################################################################################
WriteNC_diff = function(tosave, paste_name){
  ss_arr = tosave
  londim = ncdim_def("lon", "grados_este", as.double(lon2))
  latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
  seasondim = ncdim_def("seasons", "season", as.double(1:4))
  vdim = ncdim_def("v", "v", as.double(1))
  fillvalue = NA
  dlname = "dif"
  v_def = ncvar_def("dif", "", list(londim, latdim, seasondim, vdim), fillvalue, dlname, prec="single")
  ncfname = paste("/pikachu/datos/luciano.andrian/aux_nmme_quantiles/", paste_name, '.nc', sep = "")
  ncout = nc_create(ncfname, list(v_def), force_v4=T)
  ncvar_put(ncout, v_def, ss_arr)
  nc_close(ncout)
}

