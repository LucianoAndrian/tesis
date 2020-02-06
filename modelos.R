
rm(list=ls())
require(here)
library(ncdf4)
source("mapa_obs.R")
source("r2_modelos.R")
lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]


nombres = c("COLA-CCSM4", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2")
r = c(10, 12, 12, 4, 28, 10, 20) # NCEP es tiene 24 miembros pero aparecen 28 ...?, para que la funcion no tenga problemas es necesario que r = 28
                                 # GDFL-CM2P1 tiene 28 miembros para pp, poniendo r 28 no afecta a los calculos de de temp con 10 miembros (NA)    

# seleccion y guardado de datos NC en carpeta ncfiles 
#####
for( i in 1:length(nombres)){
  modelos_rx2(nombres[i], r[i])
}

cm2p1()

#####

nombres2 = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2")

for(i in 1:length(nombres2)){
  v = mean_sd(nombres2[i])
  
  # imagenes
  # T
  mapa(lista = v[[1]], titulo = paste("Promedio Temperatura 1982 - 2010 - ", nombres2[i], sep = ""), nombre_fig = paste("temp_",nombres2[i], sep = ""), escala = c(-5,40 ) 
       ,label_escala = "°C", resta = 273.15, brewer = "Spectral", revert = "si", niveles = 11, contour = "si", lon2, lat2, c(-5,0,5,10,15,20,25,30,35,40),"/salidas/ensemble/")
  
  # SD - T
  mapa(lista = v[[2]], titulo = paste(" SD Temperatura 1982 - 2010 - ", nombres2[i], sep = ""), nombre_fig = paste("SD-temp_",nombres2[i], sep = ""), escala = c(0,3) 
       ,label_escala = "°C", resta = 0, brewer = "RdYlBu", revert = "si", niveles = 9, contour = "si", lon2, lat2, c(0,0.5,1,1.5,2,2.5,3),"/salidas/ensemble/")
  
  # PP
  
  mapa(lista = v[[3]], titulo = paste("Promedio Precipitaciòn 1982 - 2010 - ", nombres2[i], sep = ""), nombre_fig = paste("pp_",nombres2[i], sep = ""), escala = c(0, 500) 
       ,label_escala = "mm", resta = 0, brewer = "BrBG", revert = "no", niveles = 9, contour = "si", lon2, lat2, c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500),"/salidas/ensemble/")
  
  # SD
  
  mapa(lista = v[[4]], titulo = paste("SD Precipitaciòn 1982 - 2010 - ", nombres2[i], sep = ""), nombre_fig = paste("SD-pp_",nombres2[i], sep = ""), escala = c(0, 150) 
       ,label_escala = "mm", resta = 0, brewer = "Spectral", revert = "si", niveles = 9, contour = "si", lon2, lat2, c(0, 25, 50, 75, 100, 125, 150 ),"/salidas/ensemble/")
 
}


source("anova_fun.R")
mask=as.matrix(read.table("mascara.txt"))
ss_temp = anova_fun()  
ss_pp = anova_fun()

mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
for(i in 1:4){
  mask_arr[,,i] = mask
}

#SS[[1]] = SSa
#SS[[2]] = SSb
#SS[[3]] = SSg
#SS[[4]] = SSe
#SS[[5]] = TSS 

#SS[[6]] = c_a
#SS[[7]] = c_b
#SS[[8]] = c_g
#SS[[9]] = c_e


#alpha = as.character("\u03b1")
#beta = as.character("\u03B2")
#gamma = as.character("\u194")
#epsilon = as.character("\u03B5")

letras = c(as.character("\u03b1"), as.character("\u03B2"), as.character("\u194"), as.character("\u03B5"))
  
source("mapa_sig.R")  #mapa con significancia

###### testeos cocientes #####

source("tests.R")
sig_temp = test_cos(ss_temp)
sig_pp = test_cos(ss_pp)

#graficar
source("mapa_obs.R")



for(i in 6:9){  # GRAFICAR CON O SIN SIGNIFICANCIA ??
  
  mapa(lista = ss_temp[[i]]*sig_temp[[i-5]], titulo = paste("T - Fraccion de TSS representada por SS", letras[i-5] , by = "")  , nombre_fig = paste("temp_ss", letras[i-5], sep = ""), escala = c(0,1) 
       ,label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 5, contour = "si", lon2, lat2, seq(0, 1, by = 0.2),"/salidas/ensemble/anova/")
  
  
  mapa(lista = ss_pp[[i]]*sig_pp[[i-5]], titulo = paste("PP - Fraccion de TSS representada por SS", letras[i-5] , by = "")  , nombre_fig = paste("pp_ss", letras[i-5], sep = ""), escala = c(0,1) 
       ,label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 5, contour = "si", lon2, lat2, seq(0, 1, by = 0.2),"/salidas/ensemble/anova/")
  
}


# ssg como otra escala


mapa(lista = ss_temp[[8]]*sig[[8-5]], titulo = paste("T - Fraccion de TSS representada por SS", letras[8-5] , by = "")  , nombre_fig = paste("esc_temp_ss", letras[8-5], sep = ""), escala = c(0,0.2) 
     ,label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 5, contour = "si", lon2, lat2, seq(0, 0.2, by = 0.04),"/salidas/ensemble/anova/")

mapa(lista = ss_pp[[8]]*sig_pp[[8-5]], titulo = paste("PP - Fraccion de TSS representada por SS", letras[8-5] , by = "")  , nombre_fig = paste("esc_pp_ss", letras[8-5], sep = ""), escala = c(0,0.2) 
     ,label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 5, contour = "si", lon2, lat2, seq(0, 0.2, by = 0.04),"/salidas/ensemble/anova/")


#  predictibilidad.  ### VER ahora todo es significativo... ### OK

sigma_alpha_2 = SS[[1]]/((t-1)) 

sigma_beta_2 = SS[[2]]/(m-1) 

sigma_gamma_2 = SS[[3]]/((t-1)*(m-1))  

sigma_epsilon_2 = SS[[4]]/(t*(sum(k)-1))


# pred = sigmas.. ver estimador insesgado para cada una de ellas
anios = seq(from = 1982, to = 2010, by = 1)
#sigma_alfa = ss[[1]]/(length(anios)-1) # esto esta MAAAL
#sigma_error = ss[[5]]/(length(anios)-1)

pred = sigma_alpha_2/(sigma_alpha_2+sigma_epsilon_2)*mask_arr #ARREGLAR


miembros = c(10, 10, 12, 12, 4, 28, 10, 20)

r = sum(miembros)
m = 8
t = 29

aux_pp = (ss[[1]]/ss[[4]])*((t*105)/(t-1))
pp = 1/(1+((m*r)/(aux_pp-1)))

pp_f = 1/(1+(8*r)/qf(0.95, t-1, t*106)-1)

pp[which(pp<pp_f)]=NA
pp = pp*mask_arr

image.plot(pp[,,4])


# ver si se puede mejorar el grafico con significancia, ver geom_points o stat subset

sig_temp[[3]][which((sig_temp[[3]]==1))] = 2
sig_temp[[3]][which(is.na(sig_temp[[3]]))] = 10
sig_temp[[3]][which((sig_temp[[3]]==2))] = NA
source("mapa_sig.R")
mapa_sig(lista = ss_temp[[8]], lista2 = sig_temp[[3]], titulo = "Fraccion de TSS representada por SS()", nombre_fig = paste("prueba", sep = ""), escala = c(0,0.2) 
         ,label_escala = "no se", resta = 0, brewer = "Spectral", revert = "no", niveles = 5, contour = "si", lon2, lat2, seq(0,0.2, by = 0.04),"/salidas/ensemble/anova/")

















#extra

#GFDL FLOR A06 vs B01

gfdl = c("GFDL-FLOR-A06", "GFDL-FLOR-B01")

A06 = mean_sd(gfdl[1])
B01 = mean_sd(gfdl[2])

dif_t = A06[[1]] - B01[[1]]

dif_pp = A06[[3]] - B01[[3]]


# imagenes
# T

mapa(lista = v[[1]], titulo = paste("Promedio Temperatura 1982 - 2010 - GFDL-FLOR-A06", sep = ""), nombre_fig = paste("TEMP_GFDL_FLOR_A06", sep = ""), escala = c(-5,40 ) 
     ,label_escala = "°C", resta = 273.15, brewer = "Spectral", revert = "si", niveles = 11, contour = "si", lon2, lat2, c(-5,0,5,10,15,20,25,30,35,40),"/salidas/ensemble/GFDL/")

mapa(lista = v2[[1]], titulo = paste("Promedio Temperatura 1982 - 2010 - GFDL-FLOR-B01", sep = ""), nombre_fig = paste("TEMP_GFDL_FLOR_B01", sep = ""), escala = c(-5,40 ) 
     ,label_escala = "°C", resta = 273.15, brewer = "Spectral", revert = "si", niveles = 11, contour = "si", lon2, lat2, c(-5,0,5,10,15,20,25,30,35,40),"/salidas/ensemble/GFDL/")



mapa(lista = dif_t, titulo = paste("Diferencia Promedio Temperatura GFDL-FLOR A06 - B01", sep = ""), nombre_fig = paste("dif_temp_gfdl", sep = ""), escala = c(-0.3, 0.4) 
     ,label_escala = "°C", resta = 0, brewer = "RdBu", revert = "si", niveles = 11, contour = "si", lon2, lat2, c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3),"/salidas/ensemble/GFDL/")
  
# PP
  
mapa(lista = dif_pp, titulo = paste("Diferencia Promedio PP GFDL-FLOR A06 - B01 - ", sep = ""), nombre_fig = paste("dif_pp_gfdl", sep = ""), escala = c(-15, 15) 
     ,label_escala = "mm", resta = 0, brewer = "BrBG", revert = "no", niveles = 11, contour = "si", lon2, lat2, c(-15, -10, -5, 0, 5, 10, 15),"/salidas/ensemble/GFDL/")


mapa(lista = v[[3]], titulo = paste("Promedio Temperatura 1982 - 2010 - GFDL-FLOR-A06", sep = ""), nombre_fig = paste("PP_GFDL_FLOR_A06", sep = ""), escala = c(0, 500) 
     ,label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si", lon2, lat2, c(0, 50, 100, 150, 200, 250, 300,350, 400, 450, 500),"/salidas/ensemble/GFDL/")


mapa(lista = v2[[3]], titulo = paste("Promedio Temperatura 1982 - 2010 - GFDL-FLOR-B01", sep = ""), nombre_fig = paste("PP_GFDL_FLOR_B01", sep = ""), escala = c(0, 500) 
     ,label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si", lon2, lat2, c(0, 50, 100, 150, 200, 250, 300,350, 400, 450, 500),"/salidas/ensemble/GFDL/")



### tests ### no queda claro como testear
#f1 = (ss[[2]]/ss[[4]])*(y*m*(r-1)/(m-1))
#f1_f = qf(0.95, m-1, y*m*(r-1) )


#aux_f2 = (m-1)/(y*m*(r-1))
#f2 = (ss[[1]] - aux_f2*ss[[4]])/(ss[[5]]) 
#f2_f =  qf(.95, m-1, y*m*(r-1))
