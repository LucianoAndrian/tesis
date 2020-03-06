# prueba sacar modelos
source("funciones.R")

lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]
letras = c(as.character("\u03b1"), as.character("\u03B2"), as.character("\u194"), as.character("\u03B5"))




#"Modelo a eliminar del ensamble. (1)COLA-CCSM4, (2)GFDL-CM2p1, (3)GFDL-FLOR-A06, (4)GFDL-FLOR-B01, (5)NASA-GEOS5, (6)NCEP-CFSv2, (7)CMC-CanCM4i, (8)CMC-CanSIPSv2: " 

ss_T = anova_fun()    # ver funcion. ssgama muy alto.

ss_PP = anova_fun()


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


#ss_[[modelo que se saco]][[componentes de TSS y cocients]][,,estacion]
#sig_[[modelo que saco]][[sigmas]][,,estacion]

nombres2 = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-CanSIPSv2") 

for(m in 1:8){
  
  for(i in 6:9){  
    
    mapa_sig(lista = ss_T[[m]][[i]]*sig_temp[[m]][[i-5]], lista2 = sig_temp[[m]][[i-5]], titulo = paste("T - SS", letras[i-5], " sin ", nombres2[m] , by = "")  , nombre_fig = paste("temp_ss", letras[i-5], "_no_", nombres2[m], sep = ""), escala = c(0,1) 
             ,label_escala = "", resta = 0, brewer = "OrRd", revert = "no", niveles = 5, contour = "si", lon2, lat2, seq(0, 1, by = 0.2),"/salidas/ensemble/anova/sin_mod/")
    
    
    mapa_sig(lista = ss_PP[[m]][[i]]*sig_pp[[m]][[i-5]], lista2 = sig_pp[[m]][[i-5]], titulo = paste("PP - SS", letras[i-5], " sin ", nombres2[m]  , by = "")  , nombre_fig = paste("pp_ss", letras[i-5], "_no_", nombres2[m], sep = ""), escala = c(0,1) 
             ,label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 5, contour = "si", lon2, lat2, seq(0, 1, by = 0.2),"/salidas/ensemble/anova/sin_mod/")
    
  }
  
  
}

# hacer otra escala para SSg


pp=pp_test(ss_T,ss_PP)

for(m in 1:8){
  
  mapa_sig(lista = pp[[m]][[1]]*pp[[m]][[2]], lista2 = pp[[m]][[2]], titulo = paste("Predictibilidad - T", " sin ", nombres2[m], by = "")  , nombre_fig = paste("pred_temp", "_no_", nombres2[m], sep = ""), escala = c(0,1) 
           ,label_escala = "", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 6, contour = "si", lon2, lat2, seq(0, 1, by = 0.2),"/salidas/ensemble/anova/sin_mod/")
  
  
  mapa_sig(lista = pp[[m]][[3]]*pp[[m]][[4]], lista2 = pp[[m]][[4]], titulo = paste("Predictibilidad - PP", " sin ", nombres2[m], by = "")  , nombre_fig = paste("pred_pp", "_no_", nombres2[m], sep = ""), escala = c(0,1) 
           ,label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 6, contour = "si", lon2, lat2, seq(0, 1, by = 0.2),"/salidas/ensemble/anova/sin_mod/")
  
  
}


# ok. 
# de a 1. seria mejor automatizar todo???