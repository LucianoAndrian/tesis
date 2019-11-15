rm(list=ls()) 
require(here)
#setwd("/home/auri/Facultad/")
PATH <- "/home/luciano/V"
setwd(PATH)
library(ncdf4)
path<-"/home/auri/Facultad/Tesis/"

tref = nc_open(paste(path, "tref_monthly_nmme_ghcn_cams.nc", sep = "/"))
names(tref$var)
temp = ncvar_get(tref, "tref")

# serie desde enero '82 -> noviembre 2012 
# NO FALTAN MESES, SOLO LO LEE RARO NCVIEW ---> USAR NCDUMP POR TERMINAL
# TOMAR DESDE MARZO DEL '82 HASTA NOV DEL 2011?? ---> uso feb del 2012 asi me quedan misma cantidad de estaciones.  ES IMPORTANTE O DA IGUAL??
lat = ncvar_get(tref, "Y")
lon = ncvar_get(tref, "X")

temp = temp[which(lon==275):which(lon==330), which(lat==-60):which(lat==15), 3:371] 

lon2 = lon[which(lon==275):which(lon==330)]
lat2 = lat[which(lat==-60):which(lat==15)]

temp_estaciones = array(NA, dim = c(length(lon2), length(lat2), 30, 12)) # CON ESTA DIMENCION YA CUMPLE LO DE ARRIBA.

for(j in 1:12){
  for (i in 0:29){
    temp_estaciones[,,1+i,j] = temp[ , , j+12*i]
  }
}


estaciones_p_a = array(NA, dim = c(length(lon2), length(lat2), 30, 4))
for(i in 1:9){
  estaciones_p_a[,,,i-ceiling(i/2)] = apply(temp_estaciones[,,,1:i+3], c(1,2,3), mean)
  i = i + 3
}

estaciones_prom = array(NA, dim = c(length(lon2), length(lat2), 4))

for( i in 1:4){
  estaciones_prom[,,i] = apply(estaciones_p_a[,,,i], c(1,2), mean)
}
 
# probar graficos
library(ncdf4)
library(maps)
library(ncdf4)
require(fields)
require(mapdata)
library(ggplot2)
library(metR)
library(RColorBrewer)
library(mapproj)


lon = 23
lat = 30

value = array(estaciones_prom, dim = length(lon2)*length(lat2))
data = matrix(data = NA, nrow = length(lon2)*length(lat2), ncol = 3)

l=0
while(l<length(lon2)*length(lat2)){
  data[seq(l:l+length(lon2)),1]<-lon2
  l=l+length(lon2)
}

for(j in 1:length(lat2)){
  lat_v = array(lat2[j],dim=length(lon2))
  data[(length(lon2)*j-(length(lon2)-1)):(j*length(lon2)),2]<-lat_v
} 

value[which(is.na(value))] = 0
data[,3]<-value-273.15
error<-as.data.frame(data)

colnames(error)<-c("lon", "lat", "temp")

error[which(error$lon>180),][,1]<-error[which(error$lon>180),][,1]-360  



mapa <- map_data("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
                                      "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua"), 
                 colour = "black")
# veeeeeeer
library(scico)
g <- ggplot() + theme_minimal()+
  xlab("Longitud") + ylab("Latitud") + 
  theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
  geom_contour_fill2(data=error,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = 0)+
  #geom_tile(data=error,aes(x = lon, y= lat,fill = temp),alpha=1, na.rm = T)+
  scale_fill_gradient2(name="°C",low = "blue", mid = "white", high = "red",midpoint = 0)+#colours=rev(brewer.pal(n=11,"Spectral")),na.value = "white")+
  #scale_fill_distiller(type = "div")+
  geom_polygon(data=mapa, aes(x=long,y=lat, group =group),fill = NA, color = "black") + #coord_map("stereographic", orientation = c(-35, -56, 0))+
  ggtitle(paste("prueba", " - " , "prueba", sep = ""))+
  scale_x_continuous(limits = c(-85, -33))+
  theme(axis.text.y   = element_text(size=14), axis.text.x   = element_text(size=14), axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=3),
        panel.ontop = TRUE,
        plot.title = element_text(hjust=0.5))



