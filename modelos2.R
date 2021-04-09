# prueba sacar modelos
source("funciones.R")

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
letras = c(as.character("\u03b1"), as.character("\u03B2"), as.character("\u194"), as.character("\u03B5"))




#"Modelo a eliminar del ensamble. (1)COLA-CCSM4, (2)GFDL-CM2p1, (3)GFDL-FLOR-A06, (4)GFDL-FLOR-B01, (5)NASA-GEOS5, (6)NCEP-CFSv2, (7)CMC-CanCM4i, (8)CMC-CanSIPSv2: " 

ss_T = anova_fun(variable = "temp", ensemble_total = "no")    # ver funcion. ssgama muy alto.

ss_PP = anova_fun(variable = "pp", ensemble_total = "no")


#como guardar esto? como un Rdata. # parece que anda.
save(ss_T, file="ss_T.RData")
save(ss_PP, file = "ss_PP.RData")


sig_temp = list()
for(i in 1:8){
  sig_temp[[i]] = test_cos(ss_T[[i]], ensemble_total = "no", nomodel_selec = "no", no_model = i )
}

sig_pp = list()
for(i in 1:8){
  sig_pp[[i]] = test_cos(ss_PP[[i]], ensemble_total = "no", nomodel_selec = "no", no_model = i )
}


mask=as.matrix(read.table("mascara.txt"))
mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}

#ss_[[modelo que se saco]][[componentes de TSS y cocients]][,,estacion]
#sig_[[modelo que saco]][[sigmas]][,,estacion]

nombres2 = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "ECCC-GEM-NEMO") 

for(m in 1:8){
  
  for(i in 6:9){  
    

    
    mapa_topo3(variable = ss_T[[m]][[i]]*mask_arr, variable.sig = sig_temp[[m]][[i-5]], colorbar = "YlOrRd", revert = F, escala = seq(0, 1, by = 0.1)
               , titulo =  paste("T - Fraction of total variance determined by", letras[i-5], " sin ", nombres2[m] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
               , salida = "/salidas/ensemble/anova/sin_mod/sin_mod1/", nombre.fig = paste("temp_ss", letras[i-5], "_no_", nombres2[m], sep = ""), na.fill = -1000
               , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
               , lon = lon2, lat = lat2, type.sig = "point", letter.size = 16)
    
    mapa_topo3(variable = ss_PP[[m]][[i]]*mask_arr, variable.sig = sig_pp[[m]][[i-5]], colorbar = "PuBuGn", revert = F, escala = seq(0, 1, by = 0.1)
               , titulo =  paste("PP - Fraction of total variance determined by", letras[i-5], " sin ", nombres2[m]  , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
               , salida = "/salidas/ensemble/anova/sin_mod/sin_mod1/", nombre.fig = paste("pp_ss", letras[i-5], "_no_", nombres2[m], sep = ""), na.fill = -1000
               , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
               , lon = lon2, lat = lat2, type.sig = "point", letter.size = 16)
    
  }
}

# hacer otra escala para SSg

for(m in 1:8){

  mapa_topo3(variable = ss_T[[m]][[8]]*mask_arr, variable.sig = sig_temp[[m]][[8-5]], colorbar = "YlOrRd", revert = F, escala = seq(0, 0.1, by = 0.01)
             , titulo = paste("T - Fraction of total variance determined by", letras[8-5] ," sin ", nombres2[m], by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/ensemble/anova/sin_mod/sin_mod1/", nombre.fig = paste("esc_temp_ss", letras[8-5], "_no_", nombres2[m], sep = ""), na.fill = -1000
             , sig = T, color.vsig = "black", alpha.vsig = 0.4, r = 4, estaciones = T, altura.topo = 1500, size.point = 0.8
             , lon = lon2, lat = lat2, type.sig = "point", letter.size = 15)
  
  
  
  mapa_topo3(variable = ss_PP[[m]][[8]]*mask_arr, variable.sig = sig_pp[[m]][[8-5]], colorbar = "PuBuGn", revert = F, escala = seq(0, 0.1, by = 0.01)
             , titulo = paste("PP - Fraction of total variance determined by", letras[8-5], " without ", nombres2[m] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/ensemble/anova/sin_mod/sin_mod1/", nombre.fig = paste("esc_pp_ss", letras[8-5], "_no_", nombres2[m], sep = ""), na.fill = -1000
             , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
             , lon = lon2, lat = lat2, type.sig = "point", letter.size = 15)

}


pp=pp_test(ss_temp = ss_T, ss_pp = ss_PP, ensemble_total = "no")

for(m in 1:8){
  
  
  mapa_topo3(variable = pp[[m]][[1]]*mask_arr, variable.sig = pp[[m]][[2]], colorbar = "YlOrRd", revert = F, escala = seq(0, 1, by = 0.1)
             , titulo =  paste("Predictability of Temp.", " without ", nombres2[m], by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/ensemble/anova/sin_mod/pred_sinmod/", nombre.fig = paste("pred_temp", "_no_", nombres2[m], sep = ""), na.fill = -1000
             , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
             , lon = lon2, lat = lat2, type.sig = "point")
  
  
  mapa_topo3(variable = pp[[m]][[3]]*mask_arr, variable.sig = pp[[m]][[4]], colorbar = "PuBuGn", revert = F, escala = seq(0, 0.4, by = 0.05)
             , titulo =  paste("Predictibility of Precip.", " without ", nombres2[m], by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/ensemble/anova/sin_mod/pred_sinmod/", nombre.fig = paste("pred_pp", "_no_", nombres2[m], sep = ""), na.fill = -1000
             , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
             , lon = lon2, lat = lat2, type.sig = "point")
  
  
  
}
