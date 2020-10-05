# ACC "espacial"?


#### Apertura base de datos ####
#-------------------------------------------------#
### Observaciones. O(j,m) j años, m estaciones. ###
#-------------------------------------------------#
# necesito "estaciones_p_a_t" de datos_obs.R  (ahora se va a llamar prom_est)
# los años y latitudes se mantienen igual que en datos_obs.R

library(ncdf4)
source("funciones.R")
mask = as.matrix(read.table("mascara.txt"))


# O == prom_est-...
# O' == O - c_v_....

##------------------------ CPC ------------------------ ## 
#sin mascara

# Temp

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





# PP
## ------------------------ CMAP ------------------------ ## # sin mascara
# solo pp
library(fields)
aux = nc_open("/home/luciano.andrian/tesis/X190.191.242.210.56.5.48.49.nc")
#aux2 = ncvar_get(aux, "precip")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),]
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
aux2 = ncvar_get(aux, "precip")[,,27:386]
nc_close(aux)

lon2 = lon
lat2 = lat

pp3_int = array(NA, dim = c(58, 78, 360)) # esta quedo con mayor latitud y longitud ya que sino queda mas chico debido a la grilla 2.5x2.5

for(i in 1:360){  #interpolado
  
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

# O
datos.obs = array(data = NA, dim = c(56, 76, 29, 4, 2)) # uso misma cantidad de años que los modelos
datos.obs[,,,,1] = prom_est_cpc_t[,,1:29,] 

datos.obs[,,,,2] = prom_est_cmap_pp[2:57,2:77,1:29,]  # este tenia + lats y lons por el grillado



########################## Cross Validation  datos.obs ##########################
# 
# para cada año tengo q tener promedio de todos los años menos ese año.

aux = diag(29)
aux[which(aux == 1)] = NA ; aux[which(aux == 0)] = 1

aux2 = array(data = 1, dim = c(56, 76, 29, 4, 29, 2))  
aux2.obs = array(data = 1, dim = c(56, 76, 29, 4, 29, 2))

cv.obs = array(data = NA, dim = c(56, 76, 29, 4, 2)) # para las 4 base de datos, la 1era temp y las otras pp

for(i in 1:29){
  
  aux2[,,i,,i,] = aux2[,,i,,i,]*aux[i,i] # como matriz identidad inversa con NA en la diagonal y 1 pero en 4 dimenciones.
  
  aux2.obs[,,,,i,] = aux2[,,,,i,]*datos.obs
  
  # promedio sacando cada año.
  
  cv.obs[,,i,,] = apply(aux2.obs[,,,,i,], c(1,2,4,5), mean, na.rm = T)
  
}



### O' 
Op = datos.obs - cv.obs


#### Apertura de los modelos ####
#-------------------------------------------------#
###    Modelos. F(j,m) j años, m estaciones.    ###
#-------------------------------------------------#

# necesito el array intermedio para crear sd que tiene la funcion mean_sd. 
# modificada la funcion, devuelve lista que en las dim [[5]] = se encuetnra la temp y  [[6]] la pp. h
# ESTAS LISTAS SON EL ENSAMBLE DE LOS MIEMBROS DE CADA MODELO --> OK


lon2 = read.table("lon2.txt")[,1]
lat2 = read.table("lat2.txt")[,1]

modelos = c("COLA-CCSM4", "GFDL-CM2p1", "GFDL-FLOR-A06", "GFDL-FLOR-B01", "NASA-GEOS5", "NCEP-CFSv2", "CMC-CanCM4i", "CMC-GEM-NEMO") 

# uso misma denominacion que para las obserbaciones.
# esto es F 
t.mods = array(data = NA, dim = c(56, 76, 29, 4, 8)) # recordar, los modelos 1982-2010 (29 años)
pp.mods = array(data = NA, dim = c(56, 76, 29, 4, 8))
for(i in 1:length(modelos)){
  aux = mean_sd(modelos[i])
  t.mods[,,,,i] = aux[[5]]
  pp.mods[,,,,i] = aux[[6]]
}  

########################## Cross Validation modelos ##########################

aux = diag(29)
aux[which(aux == 1)] = NA ; aux[which(aux == 0)] = 1

aux2 = array(data = 1, dim = c(56, 76, 29, 4, 8, 29))

aux3 = array(data = 1, dim = c(56, 76, 29, 4, 8, 29)) # T
aux4 = array(data = 1, dim = c(56, 76, 29, 4, 8, 29)) # PP

aux5 = array(data = NA, dim = c(56, 76, 29, 4, 8))
aux6 = array(data = NA, dim = c(56, 76, 29, 4, 8))

for(i in 1:29){
  aux2[,,i,,,i] = aux2[,,i,,,i]*aux[i,i]  # una especie de matriz identidad inversa con NA y 1 pero en 4 dim.
  
  aux3[,,,,,i] = aux2[,,,,,i]*t.mods
  aux4[,,,,,i] = aux2[,,,,,i]*pp.mods
  
  # promedio sacando cada anio
  # 
  aux5[,,i,,] = apply(aux3[,,,,,i], c(1, 2, 4, 5), mean, na.rm = T)   
  aux6[,,i,,] = apply(aux4[,,,,,i], c(1, 2, 4, 5), mean, na.rm = T)
  
  
}

t.Fp = t.mods - aux5
pp.Fp = pp.mods - aux6

#### AREAS ####
#----falta alguna? -----#
lats = list()
lats[[1]] =  seq(which(lat2 == -13), which(lat2 == 2), by = 1); lats[[2]] = seq(which(lat2 == -16), which(lat2 == 4), by = 1)
lats[[3]] = seq(which(lat2 == -16), which(lat2 == 2), by = 1); lats[[4]] = seq(which(lat2 == -26), which(lat2 == -17), by = 1)
lats[[5]] = seq(which(lat2 == -39), which(lat2 == -24), by = 1)

lons = list()
lons[[1]] =  seq(which(lon2 == 291), which(lon2 == 304), by = 1); lons[[2]] = seq(which(lon2 == 301), which(lon2 == 316), by = 1)
lons[[3]] = seq(which(lon2 == 313), which(lon2 == 326), by = 1); lons[[4]] = seq(which(lon2 == 308), which(lon2 == 321), by = 1)
lons[[5]] = seq(which(lon2 == 296), which(lon2 == 309), by = 1)


#### ACC ####

# haciendo igual q en desempmods...

t.Fp_ens = apply(t.Fp, c(1,2,3,4), mean, na.rm = T)
pp.Fp_ens = apply(pp.Fp, c(1,2,3,4), mean, na.rm = T)
acc_ens = array(data = NA, dim = c(29,4,5,2))
V = list()
V[[1]] = t.Fp_ens
V[[2]] = pp.Fp_ens
for(v in 1:2){
  
    for(z in 1:5){
      
      xp = Op[lons[[z]], lats[[z]],,,v]
      xp_sp = apply(xp, c(3,4), mean, na.rm = T)
      
      fp = V[[v]][lons[[z]], lats[[z]],,] # cada modelo
      fp_sp = apply(fp, c(3,4), mean, na.rm = T)
      
      aux.o = array(data = NA, dim = c(dim(xp),5,2))
      aux.m = array(data = NA, dim = c(dim(fp),5,2))
      
      for(a in 1:29){
        
        aux.o[,,a,,z,v] =  xp[,,a,] - xp_sp[a,]
        aux.m[,,a,,z,v] =  fp[,,a,] - fp_sp[a,]
        
        n = length(xp[,1,1,1])*length(xp[1,,1,1])
        
        num = apply(aux.o*aux.m, c(3,4), sum, na.rm = T)
        den = n*sqrt((apply(aux.o**2, c(3,4), sum, na.rm = T)/n)*(apply(aux.m**2, c(3,4), sum, na.rm = T)/n))
        
        acc_ens[,,z,v] = num/den
        
      }
   }
}

#prueba grafico
library(ggplot2)
rc = qt(p = 0.95,df = 29-1)/sqrt((29-1)+qt(p = 0.95,df = 29-1))

region = c("Amazonia", "South American Monsoon", "North-estern Brazil", "SACZ", "La Plata Basin")
region.fig = c("Am", "SAM", "NeB", "SACZ")
var.title = c("Temperatura", "Precipitación")
var = c("t", "pp")

for(v in 1:2){
  for(z in 1:5){
    
    aux = as.data.frame(acc_ens[,,z,v])
    
    aux=cbind(aux, seq(1982, 2010))
    colnames(aux) = c("MAM", "JJA", "SON", "DJF", "Años")
    
    g = ggplot(aux, aes(x = Años))+theme_minimal()+
      geom_line(aes(y = MAM, colour = "MAM"), size = 1) +
      geom_line(aes(y = JJA, colour = "JJA"), size = 1) +
      geom_line(aes(y = SON, colour = "SON"), size = 1) +
      geom_line(aes(y = DJF, colour = "DJF"), size = 1) +
      
      scale_colour_manual("", 
                          breaks = c("MAM", "JJA", "SON", "DJF"),
                          values = c("yellow2", "royalblue", "green3", "orange2")) +
      geom_hline(yintercept = rc, color = "grey", size = 1, alpha = 1) +
      
      
      ggtitle(paste("ACC ", var.title[v], " - ", region[z], sep = "")) +
      scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2)) + 
      scale_x_continuous(limits = c(1982, 2010), breaks = seq(1982, 2010, by = 2)) +
      
      theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
            panel.border = element_rect(colour = "black", fill = NA, size = 1),
            panel.ontop = F,
            plot.title = element_text(hjust = 0.5, size = 18),
            legend.position = "right", legend.key.width = unit(1, "cm"), legend.key.height = unit(2, "cm"), legend.text = element_text(size = 15)) 
    
    ggsave(paste("/home/luciano.andrian/tesis/salidas/desemp_mods/ACC2/", var[v], ".ACC2_", region[z],".jpg",sep =""), plot = g, width = 30, height = 15  , units = "cm")
    
    
    
  }
}
