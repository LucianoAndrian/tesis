library(ncdf4)
library(maps)
library(ncdf4)
require(fields)
require(mapdata)
library(ggplot2)
library(RColorBrewer)
library(mapproj)
library(metR)
ruta = getwd()
mask=read.table("mascara.txt")

est=c("MAM", "JJA", "SON", "DJF")
g = list()

value = array(lista[,,i], dim = length(lon)*length(lat))
data = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)

l=0
while(l<length(lon)*length(lat)){
  data[seq(l:l+length(lon)),1]<-lon
  l=l+length(lon)
}

for(j in 1:length(lat)){
  lat_v = array(lat[j],dim=length(lon))
  data[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
} 


data[,3]<-value-resta
data<-as.data.frame(data)

colnames(data)<-c("lon", "lat", "temp")

data[which(data$lon>180),][,1]<-data[which(data$lon>180),][,1]-360  


mapa <- map_data("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
                                      "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua", "falkland islands",
                                      "Martinique"), 
                 colour = "black")

if(revert == "si"){
  if(contour == "si"){
    g = ggplot() + theme_minimal()+
      xlab("Longitud") + ylab("Latitud") + 
      theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
      
      #geom_tile(data = data, aes(x = lon, y = lat, fill = temp), alpha=0.9, na.rm = T) +
      
      #geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp),alpha = 1, na.fill = -10000, breaks = breaks_c_f) +
      
      #scale_fill_gradientn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", guide = "legend", breaks = escala_dis) +
      
      #guides(fill = guide_legend(reverse = TRUE)) +
      
      geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
      ggtitle(paste(titulo, " - " , est[i], sep = ""))+
      scale_x_continuous(limits = c(-85, -33))+
      theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
            axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill = NA, size = 3),
            panel.ontop = TRUE,
            plot.title = element_text(hjust = 0.5))
    
    
    
    
    ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg", sep = ""), plot = g, width = 15, height = 15  , units = "cm")
    
    
    
    aconcagua <- metR::GetTopography(-85 + 360, -30 + 360,
                                     15,  -60, 
                                     resolution = 1/10)
  
    arg = aconcagua
    arg[which(arg$h<800)]=NA 
    
    ggplot(arg, aes(lon, lat)) +
      theme_minimal()+
      xlab("Longitud") + ylab("Latitud") + 
      
      theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
      
      geom_contour_fill(data = data, aes(x = lon +360, y = lat, z = temp),alpha = 1, na.fill = -10000, breaks = breaks_c_f) +
      
      geom_tile(aes(fill = h ), na.rm = T, alpha = 0.5, color = "black")+
      
      scale_fill_gradientn(limits = c(-5, 40), name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "black", breaks = breaks_c_f,
                           guide = guide_colorbar(nbin = 11,  barwidth = 23, barheight = 2, title.position = "right", title.hjust = 0.5, 
                                                          raster = FALSE, ticks = FALSE)) +
      
      geom_polygon(data = mapa, aes(x = long+360,y = lat, group =group),fill = NA, color = "black") +

      coord_fixed(expand = FALSE)+
      ggtitle(paste(titulo, " - " , est[i], sep = ""))+
      scale_x_continuous(limits = c(-85+360, -33+360))+
      theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
            axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill = NA, size = 3),
            panel.ontop = TRUE,
            plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
            
  