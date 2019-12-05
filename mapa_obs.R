mapa_obs = function(lista, titulo, nombre_fig, escala, label_escala, resta, brewer, revert, niveles, contour ){
  
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
  
  
  est=c("MAM", "JJA", "SON", "DJF")
  g = list()
  for(i in 1:4){
    value = array(lista[,,i], dim = length(lon2)*length(lat2))
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
    
    
    data[,3]<-value-resta
    data<-as.data.frame(data)
    
    colnames(data)<-c("lon", "lat", "temp")
    
    data[which(data$lon>180),][,1]<-data[which(data$lon>180),][,1]-360  
    
    
    mapa <- map_data("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
                                          "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua"), 
                     colour = "black")
    if(revert == "si"){
      if(contour == "si"){
        g = ggplot() + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon, y= lat,fill = temp),alpha=0.9, na.rm = T)+
          
          geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          scale_fill_gradientn(limits=escala,name=label_escala,colours=rev(brewer.pal(n=niveles,brewer)),na.value = "white")+
          
          geom_polygon(data=mapa, aes(x=long,y=lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85, -33))+
          theme(axis.text.y   = element_text(size=14), axis.text.x   = element_text(size=14), axis.title.y  = element_text(size=14),
                axis.title.x  = element_text(size=14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill=NA, size=3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust=0.5))
        
        
        ggsave(paste(ruta, "/salidas/", nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
      } else {
        g = ggplot() + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon, y= lat,fill = temp),alpha=0.9, na.rm = T)+
          
          #geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          scale_fill_gradientn(limits=escala,name=label_escala,colours=rev(brewer.pal(n=niveles,brewer)),na.value = "white")+
          
          geom_polygon(data=mapa, aes(x=long,y=lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85, -33))+
          theme(axis.text.y   = element_text(size=14), axis.text.x   = element_text(size=14), axis.title.y  = element_text(size=14),
                axis.title.x  = element_text(size=14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill=NA, size=3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust=0.5))
        
        ggsave(paste(ruta, "/salidas/", nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
        
      }
      
    } else {
      if(contour == "si"){
        
        g = ggplot() + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon, y= lat,fill = temp),alpha=0.9, na.rm = T)+
          
          geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          scale_fill_gradientn(limits=escala,name=label_escala,colours=(brewer.pal(n=niveles,brewer)),na.value = "white")+
          
          geom_polygon(data=mapa, aes(x=long,y=lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85, -33))+
          theme(axis.text.y   = element_text(size=14), axis.text.x   = element_text(size=14), axis.title.y  = element_text(size=14),
                axis.title.x  = element_text(size=14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill=NA, size=3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust=0.5))
        
        ggsave(paste(ruta, "/salidas/", nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
      } else {
        g = ggplot() + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon, y= lat,fill = temp),alpha=0.9, na.rm = T)+
          
          #geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          scale_fill_gradientn(limits=escala,name=label_escala,colours=(brewer.pal(n=niveles,brewer)),na.value = "white")+
          
          geom_polygon(data=mapa, aes(x=long,y=lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85, -33))+
          theme(axis.text.y   = element_text(size=14), axis.text.x   = element_text(size=14), axis.title.y  = element_text(size=14),
                axis.title.x  = element_text(size=14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill=NA, size=3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust=0.5))
        
        ggsave(paste(ruta, "/salidas/", nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
        
      }
      
    }
  }
  
}
