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
  mapa(lista = v[[1]], titulo = paste("Promedio Temperatura 1982 - 2010 - ", nombres2[i], sep = ""), nombre_fig = paste("temp_",nombres2[i], sep = ""), escala = c(-5,40 ) 
       , label_escala = "°C", resta = 273.15, brewer = "Spectral", revert = "si", niveles = 11
       , contour = "si", lon2, lat2, seq(-5, 40, by = 5), seq(-5, 40, by = 5), "/salidas/ensemble/")
  
  # SD - T
  mapa(lista = v[[2]], titulo = paste(" SD Temperatura 1982 - 2010 - ", nombres2[i], sep = ""), nombre_fig = paste("SD-temp_",nombres2[i], sep = ""), escala = c(0,3) 
       , label_escala = "°C", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9 
       , contour = "si", lon2, lat2, seq(0, 3, by = 0.5), seq(0, 3, by = 0.5), seq(0, 3, by = 0.5), "/salidas/ensemble/")
  
  # PP
  
  mapa(lista = v[[3]], titulo = paste("Promedio Precipitaciòn 1982 - 2010 - ", nombres2[i], sep = ""), nombre_fig = paste("pp_",nombres2[i], sep = ""), escala = c(0, 500) 
       , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
       , contour = "si", lon2, lat2, seq(0, 500, by = 50), seq(0, 500, by = 50),"/salidas/ensemble/")
  
  # SD
  
  mapa(lista = v[[4]], titulo = paste("SD Precipitaciòn 1982 - 2010 - ", nombres2[i], sep = ""), nombre_fig = paste("SD-pp_",nombres2[i], sep = ""), escala = c(0, 150) 
       , label_escala = "mm", resta = 0, brewer = "YlGn", revert = "no", niveles = 9
       , contour = "si", lon2, lat2, seq(0, 150, by = 25), seq(0, 150, by = 25), "/salidas/ensemble/")
 
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
sig_temp = test_cos(ss_temp)
sig_pp = test_cos(ss_pp)


#source("mapa_sig.R")
for(i in 6:9){  
  
  mapa_sig(lista = ss_temp[[i]]*sig_temp[[i-5]], lista2 = sig_temp[[i-5]], titulo = paste("T - Fraccion de TSS representada por SS", letras[i-5] , by = "")  , nombre_fig = paste("temp_ss", letras[i-5], sep = ""), escala = c(0,1) 
       ,label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 5, contour = "si", lon2, lat2, seq(0, 1, by = 0.2),"/salidas/ensemble/anova/")
  
  
  mapa_sig(lista = ss_pp[[i]]*sig_pp[[i-5]], lista2 = sig_pp[[i-5]], titulo = paste("PP - Fraccion de TSS representada por SS", letras[i-5] , by = "")  , nombre_fig = paste("pp_ss", letras[i-5], sep = ""), escala = c(0,1) 
       ,label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 5, contour = "si", lon2, lat2, seq(0, 1, by = 0.2),"/salidas/ensemble/anova/")
  
}


# ssg como otra escala # 0--> 0.2 de a 0.04

mapa_sig(lista = ss_temp[[8]]*sig_temp[[8-5]], lista2 = sig_temp[[8-5]], titulo = paste("T - Fraccion de TSS representada por SS", letras[8-5] , by = "")  , nombre_fig = paste("esc_temp_ss", letras[8-5], sep = ""), escala = c(0,0.2) 
     ,label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 5, contour = "si", lon2, lat2, seq(0, 0.2, by = 0.04),"/salidas/ensemble/anova/")

mapa_sig(lista = ss_pp[[8]]*sig_pp[[8-5]], lista2 = sig_pp[[8-5]], titulo = paste("PP - Fraccion de TSS representada por SS", letras[8-5] , by = "")  , nombre_fig = paste("esc_pp_ss", letras[8-5], sep = ""), escala = c(0,0.2) 
     ,label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 5, contour = "si", lon2, lat2, seq(0, 0.2, by = 0.04),"/salidas/ensemble/anova/")


##-------------------------------------------------------------------------PREDICTIBILIDAD----------------------------------------------------------------##

#funcion pp_test
#pp[[1]] = pp_temp
#pp[[2]] = pp_temp_sig
#pp[[3]] = pp_pp
#pp[[4]] = pp_pp_sig

pp = pp_test(ss_temp, ss_pp)


mapa_sig(lista = pp[[1]]*pp[[2]], lista2 = pp[[2]], titulo = paste("Predictibilidad - T",  by = "")  , nombre_fig = paste("pred_temp"), escala = c(0,1) 
         ,label_escala = "", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 6, contour = "si", lon2, lat2, seq(0, 1, by = 0.2),"/salidas/ensemble/anova/pred/")


mapa_sig(lista = pp[[3]]*pp[[4]], lista2 = pp[[4]], titulo = paste("Predictibilidad - PP",  by = "")  , nombre_fig = paste("pred_pp_01"), escala = c(0,1) 
         ,label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 6, contour = "si", lon2, lat2, seq(0, 1, by = 0.2),"/salidas/ensemble/anova/pred/")

#precip con otra escala, 0 --> 0.4

mapa_sig(lista = pp[[3]]*pp[[4]], lista2 = pp[[4]], titulo = paste("Predictibilidad - PP",  by = "")  , nombre_fig = paste("pred_pp"), escala = c(0,0.5) 
         ,label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 5, contour = "si", lon2, lat2, seq(0, 0.4, by = 0.1),"/salidas/ensemble/anova/pred/")

