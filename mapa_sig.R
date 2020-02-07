mapa_sig = function(lista,lista2, titulo, nombre_fig, escala, label_escala, resta, brewer, revert, niveles, contour, lon, lat, escala_dis, salida){
  
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
  
  sig = lista2 # mascara significativa
  
  sig[which((sig==1))] = 2
  sig[which(is.na(sig))] = 1
  sig[which((sig==2))] = NA
  sig=sig*mask_arr
  
  # desarmado de lita sig_
  
  est=c("MAM", "JJA", "SON", "DJF")
  g = list()
  for(i in 1:4){
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
    
    # data2
  
    # pasando a puntos
    
    # solo puntos significativos. los NA no funcan. 
    
    puntos= which(!is.na(sig[,,i]), arr.ind = T)
      
    puntos2 <- data.frame(lon=lon2[puntos[,1]], lat=lat2[puntos[,2]])
    puntos2[which(puntos2$lon>180),][,1]<-puntos2[which(puntos2$lon>180),][,1]-360 

    
    mapa <- map_data("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
                                          "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua", "falkland islands",
                                          "Martinique"), 
                     colour = "black")
    
    if(revert == "si"){
      if(contour == "si"){
        g = ggplot() + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon, y= lat,fill = temp),alpha=0.9, na.rm = T)+
          
          geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          geom_point(data=puntos2, aes(x = lon, y = lat), col="black",size=0.5)+
          
          scale_fill_gradientn(limits=escala,name=label_escala,colours=rev(brewer.pal(n=niveles,brewer)),na.value = "white", guide = "legend",breaks = escala_dis)+
          
          guides(fill = guide_legend(reverse = TRUE))+
          
          geom_polygon(data=mapa, aes(x=long,y=lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85, -33))+
          theme(axis.text.y   = element_text(size=14), axis.text.x   = element_text(size=14), axis.title.y  = element_text(size=14),
                axis.title.x  = element_text(size=14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill=NA, size=3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust=0.5))
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
    
      } else {
        g = ggplot() + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon, y= lat,fill = temp),alpha=0.9, na.rm = T)+
          
          #geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          geom_point(data=puntos2, aes(x = lon, y = lat), col="black",size=0.5)+
          
          scale_fill_gradientn(limits=escala,name=label_escala,colours=rev(brewer.pal(n=niveles,brewer)),na.value = "white", guide = "legend",breaks = escala_dis) +
          
          guides(fill = guide_legend(reverse = TRUE))+
          
          geom_polygon(data=mapa, aes(x=long,y=lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85, -33))+
          theme(axis.text.y   = element_text(size=14), axis.text.x   = element_text(size=14), axis.title.y  = element_text(size=14),
                axis.title.x  = element_text(size=14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill=NA, size=3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust=0.5)) +
          
          geom_contour(data = data2, aes(x = lon, y= lat, z = temp), alpha = 1)
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
        
      }
      
    } else {
      if(contour == "si"){
        
        g = ggplot() + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data=data,aes(x = lon, y= lat,fill = temp),alpha = 0.9, na.rm = T) +
          
          geom_contour_fill(data=data,aes(x = lon, y = lat, z = temp),alpha=1, na.fill = -10000) +
          
          geom_point(data=puntos2, aes(x = lon, y = lat), col="black",size=0.5)+
          
          scale_fill_gradientn(limits=escala,name=label_escala,colours = brewer.pal(n=niveles, brewer), na.value = "white", guide = "legend", breaks = escala_dis)+
          
          guides(fill = guide_legend(reverse = TRUE)) +
          
          geom_polygon(data=mapa, aes(x=long,y=lat, group = group), fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = "")) +
          scale_x_continuous(limits = c(-85, -33)) +
          theme(axis.text.y   = element_text(size=14), axis.text.x   = element_text(size=14), axis.title.y  = element_text(size=14),
                axis.title.x  = element_text(size=14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill=NA, size=3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust=0.5))
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
      } else {
        g = ggplot() + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_polygon(data=mapa, aes(x=long,y=lat, group =group),fill = "black", color = "black") +
          
          geom_tile(data=data2,aes(x = lon, y= lat,fill = temp, colour = temp),alpha=0.9, na.rm = F) +
          
          #geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          geom_point(data=puntos2, aes(x = lon, y = lat), col="black",size=0.5)+
          
          scale_fill_gradientn(limits=escala,name=label_escala,colours= brewer.pal(n=niveles,brewer),na.value = "white", guide = "legend",breaks = escala_dis)+
          
          guides(fill = guide_legend(reverse = TRUE))+
          
          geom_polygon(data=mapa, aes(x=long,y=lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85, -33))+
          theme(axis.text.y   = element_text(size=14), axis.text.x   = element_text(size=14), axis.title.y  = element_text(size=14),
                axis.title.x  = element_text(size=14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill=NA, size=3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust=0.5)) 
          
          #geom_contour(data = data2, aes(x = lon, y= lat, z = temp), alpha = 1)
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
        
      }
      
    }
  }

}

