#rm(list=ls())


library(ncdf4)

source("funciones.R")
#source("mapa_obs.R")
#source("r2_modelos.R")

#---------------------------------------------------------------#
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
mask=as.matrix(read.table("mascara.txt"))
mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}
load("topo.RData")
#---------------------------------------------------------------#

nombres = c("COLA-CCSM4", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2")

r = c(10, 12, 12, 4, 28, 10, 20) # NCEP es tiene 24 miembros pero aparecen 28 ...?, para que la funcion no tenga problemas es necesario que r = 28
                                 # GDFL-CM2P1 tiene 28 miembros para pp, poniendo r 28 no afecta a los calculos de de temp con 10 miembros (NA)    

##--------------------------------------------------------SELECCION Y GUARDADO DE LOS MODELOS--------------------------------------------------------##
##### NO CORRER SI NO ES NECESARIO!!! #####
for( i in 1:length(nombres)){
  modelos_rx2(nombres[i], r[i])
}

cm2p1()

#####
##-----------------------------------------------------------GRAFICADO DE MEDIAS Y DESVIOS-----------------------------------------------------------##

nombres2 = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2") # ACA ESTAN TODOS. INCLUYENDO CM2P1

for(i in 1:length(nombres2)){
  v = mean_sd(nombres2[i])
  
  # imagenes
  # T
  mapa_topo3(variable = v[[1]], colorbar = "Spectral", revert = T, escala = seq(-5, 40, by = 5)
             , titulo =  paste("Promedio Temperatura 1982 - 2010 - ", nombres2[i], sep = ""), label.escala = "ºC", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/ensemble/", nombre.fig = paste("temp_",nombres2[i], sep = ""), na.fill = -1000
             , r = 4, estaciones = T, altura.topo = 1500, resta = 273
             , lon = lon2, lat = lat2)
  
  
  # SD - T
  mapa_topo3(variable = v[[2]], colorbar = "YlOrRd", revert = F, escala = seq(0, 1, by = 0.1)
             , titulo =  paste("SD Temperatura 1982 - 2010 - ", nombres2[i], sep = ""), label.escala = "ºC", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/ensemble/", nombre.fig = paste("SD-temp_",nombres2[i], sep = ""), na.fill = -1000
             , r = 4, estaciones = T, altura.topo = 1500
             , lon = lon2, lat = lat2)
  

  # PP
  
  mapa_topo3(variable = v[[3]], colorbar = "YlGnBu", revert = F, escala = seq(0, 400, by = 50)
             , titulo =  paste("Promedio Precipitacíón 1982 - 2010 - ", nombres2[i], sep = ""), label.escala = "mm", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/ensemble/", nombre.fig = paste("pp_",nombres2[i], sep = ""), na.fill = -1000
             , r = 4, estaciones = T, altura.topo = 1500
             , lon = lon2, lat = lat2)

  # SD
  mapa_topo3(variable = v[[4]], colorbar = "YlGnBu", revert = F, escala = seq(0, 100, by = 10)
             , titulo =  paste("SD Precipitacióm 1982 - 2010 - ", nombres2[i], sep = ""), label.escala = "mm", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/ensemble/", nombre.fig = paste("SD-pp_",nombres2[i], sep = ""), na.fill = -1000
             , r = 4, estaciones = T, altura.topo = 1500
             , lon = lon2, lat = lat2)
 
}


##-------------------------------------------------------------------------ANOVA---------------------------------------------------------------------##
#source("anova_fun.R")

# Anova y cocientes
ss_temp = anova_fun()  
ss_pp = anova_fun()

#SS[[1]] = SSa      SS[[6]] = c_a
#SS[[2]] = SSb      SS[[7]] = c_b
#SS[[3]] = SSg      SS[[8]] = c_g
#SS[[4]] = SSe      SS[[9]] = c_e
#SS[[5]] = TSS 


#alpha = "\u03b1"       beta = "\u03B2"       gamma = "\u194"       epsilon = "\u03B5"

letras = c(as.character("\u03b1"), as.character("\u03B2"), as.character("\u194"), as.character("\u03B5"))
  
# testeos cocientes #

#source("tests.R")

sig_temp = test_cos(ss_temp, ensemble_total = "si", nomodel_selec = "no", no_model = "no")
sig_pp = test_cos(ss_pp, ensemble_total = "si", nomodel_selec = "no", no_model = "no")


# esta OK.


#source("mapa_sig.R")
for(i in 6:9){  
  
  mapa_topo3(variable = ss_temp[[i]]*mask_arr, variable.sig = sig_temp[[i-5]], colorbar = "YlOrRd", revert = F, escala = seq(0, 1, by = 0.1)
             , titulo =  paste("T - Fraccion de TSS representada por SS", letras[i-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/ensemble/anova/anova1/", nombre.fig = paste("temp_ss", letras[i-5], sep = ""), na.fill = -1000
             , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
             , lon = lon2, lat = lat2, type.sig = "point")
  
  
  mapa_topo3(variable = ss_pp[[i]]*mask_arr, variable.sig = sig_pp[[i-5]], colorbar = "PuBuGn", revert = F, escala = seq(0, 1, by = 0.1)
             , titulo =  paste("PP - Fraccion de TSS representada por SS", letras[i-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
             , salida = "/salidas/ensemble/anova/anova1/", nombre.fig = paste("pp_ss", letras[i-5], sep = ""), na.fill = -1000
             , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
             , lon = lon2, lat = lat2, type.sig = "point")
  
}


# ssg como otra escala # 0--> 0.2 de a 0.04

mapa_topo3(variable = ss_temp[[8]]*mask_arr, variable.sig = sig_temp[[8-5]], colorbar = "YlOrRd", revert = F, escala = seq(0, 0.1, by = 0.01)
           , titulo = paste("T - Fraccion de TSS representada por SS", letras[8-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/ensemble/anova/anova1/", nombre.fig = paste("esc_temp_ss", letras[8-5], sep = ""), na.fill = -1000
           , sig = T, color.vsig = "black", alpha.vsig = 0.4, r = 4, estaciones = T, altura.topo = 1500, size.point = 0.8
           , lon = lon2, lat = lat2, type.sig = "point")


mapa_topo3(variable = ss_pp[[8]]*mask_arr, variable.sig = sig_pp[[8-5]], colorbar = "PuBuGn", revert = F, escala = seq(0, 0.1, by = 0.01)
           , titulo = paste("PP - Fraccion de TSS representada por SS", letras[8-5] , by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/ensemble/anova/anova1/", nombre.fig = paste("esc_pp_ss", letras[8-5], sep = ""), na.fill = -1000
           , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
           , lon = lon2, lat = lat2, type.sig = "point")

##-------------------------------------------------------------------------PREDICTIBILIDAD----------------------------------------------------------------##

#funcion pp_test
#pp[[1]] = pp_temp
#pp[[2]] = pp_temp_sig
#pp[[3]] = pp_pp
#pp[[4]] = pp_pp_sig

pp = pp_test(ss_temp, ss_pp)

mapa_topo3(variable = pp[[1]]*mask_arr, variable.sig = pp[[2]], colorbar = "YlOrRd", revert = F, escala = seq(0, 1, by = 0.1)
           , titulo =  paste("Predictibilidad - T",  by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/ensemble/anova/anova1/", nombre.fig = "pred_temp", na.fill = -1000
           , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
           , lon = lon2, lat = lat2, type.sig = "point")


#precip con otra escala, 0 --> 0.4


mapa_topo3(variable = pp[[3]]*mask_arr, variable.sig = pp[[4]], colorbar = "PuBuGn", revert = F, escala = seq(0, 0.4, by = 0.05)
           , titulo =  paste("Predictibilidad - PP",  by = ""), label.escala = "", mapa = "SA", width = 20, height = 20
           , salida = "/salidas/ensemble/anova/anova1/", nombre.fig = "pred_pp", na.fill = -1000
           , sig = T, color.vsig = "black", alpha.vsig = 0.5, r = 4, estaciones = T, altura.topo = 1500, size.point = 1
           , lon = lon2, lat = lat2, type.sig = "point")


