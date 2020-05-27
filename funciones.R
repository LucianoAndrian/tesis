#### Funciones ####

### MAPA ####
mapa = function(lista, titulo, nombre_fig, escala, label_escala, resta, brewer, revert, niveles, contour, lon, lat, escala_dis, breaks_c_f, topo2, salida){
  
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
  
  
  #topo = metR::GetTopography(-85 + 359.5, -29 + 359.5, 15.5,  -60.5, resolution = 1/res) # mapa topografia
  #topo2 = topo #
  #topo2[which(topo2$h<altura)]=NA
                            
  
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
    
    
    mapa <- map_data("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
                                          "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua", "falkland islands",
                                          "Martinique"), 
                     colour = "black")
    
    if(revert == "si"){
      if(contour == "si"){
        g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data = data, aes(x = lon + 360, y = lat, fill = temp), alpha=0.9, na.rm = T) +
          
          geom_contour_fill(data = data, aes(x = lon + 360, y = lat, z = temp),alpha = 1, na.fill = -10000, breaks = breaks_c_f) +
          
          geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          #guides(fill = guide_legend(reverse = TRUE)) +
          
          geom_polygon(data = mapa, aes(x = long + 360, y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85+360, -33+360))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg", sep = ""), plot = g, width = 15, height = 15  , units = "cm")
      } else {
        g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon + 360, y = lat,fill = temp),alpha=0.9, na.rm = T) +
          
          geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
          
          #geom_contour_fill(data=data, aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000, breaks = breaks_c_f)
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          
          
          #guides(fill = guide_legend(reverse = TRUE)) +
          
          geom_polygon(data = mapa, aes(x = long + 360, y = lat, group = group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = "")) +
          scale_x_continuous(limits = c(-85+360, -33+360))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
        
      }
      
    } else {
      if(contour == "si"){
        
        g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data = data,aes(x = lon + 360, y = lat, fill = temp), alpha = 0.9, na.rm = T) +
          
          geom_contour_fill(data = data, aes(x = lon + 360, y = lat, z = temp), alpha = 1, na.fill = -10000, breaks = breaks_c_f) +
          
          geom_tile(aes(fill = h ), na.rm = T, alpha = 0., color = "black") +
          
          #stat_subset(data=data, aes(x = lon, y = lat, z = temp, subset = temp <= rc), shape = 20, size = 1, color = "black", alpha = 0.3, geom = "point")+
          #geom_contour(data = data, aes(x = lon, y = lat, z = temp), color = "blue", size = 0.666, breaks = rc )+
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          #guides(fill = guide_legend(reverse = TRUE)) +
          
          geom_polygon(data = mapa, aes(x = long + 360, y = lat, group = group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85+360, -33+360))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
      } else {
        g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data = data,aes(x = lon + 360, y = lat,fill = temp),alpha = 0.7, na.rm = T) +
          
          geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
          
          #geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          #guides(fill = guide_legend(reverse = TRUE))+
          
          geom_polygon(data = mapa, aes(x=long + 360,y = lat, group = group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85+360, -33+360))+
          theme(axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14), axis.title.y = element_text(size = 14),
                axis.title.x  = element_text( size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
        
      }
      
    }
  }
  
}



#### MODELOS_RX2 ####
# seleccion y guardado de los ensambles en nc.
# todos los modelos menos GFDL-CM2p1 



modelos_rx2 = function(nombre, r){
  library(ncdf4)
  
  
  if(r<28){
    
    ruta = "/datos/osman/nmme/monthly"
    mask=as.matrix(read.table("mascara.txt"))
    t = list.files("/datos/osman/nmme/monthly", pattern = paste("tref_Amon_", nombre, sep = ""))
    
    tref = nc_open(paste(ruta, t[1], sep = "/"))
    temp = ncvar_get(tref, names(tref$var)[1])
    
    lat = ncvar_get(tref, "Y")
    lon = ncvar_get(tref, "X")
    
    lon2 = lon[which(lon==275):which(lon==330)]
    lat2 = lat[which(lat==-60):which(lat==15)]
    
    #anios = as.character(seq(from = 1982, to = 2010, by = 1))
    anios= seq(from = 1982, to = 2010, by = 1)
    #meses = as.character(c("11_r", "02_r", "05_r", "08_r"))
    
    v2 = array(NA, dim = c(length(lon2), length(lat2), 3, r, length(anios), 4))
    
    init_cond=c(2,5,8,11)
    
    for(m in 1:4){
      final_month = init_cond[m] - 1
      
      ic_format=formatC(init_cond[m], width = 2, format = "d", flag = "0")
      
      for(i in 1:length(anios)){
        
        file_pattern = paste('tref_Amon_',nombre,"_",anios[i] , ic_format,"_r*_", as.character(anios[i]),ic_format, '-' , 
                             as.character(anios[i] + 1), ic_format=formatC(final_month, width = 2, format = "d", flag = "0") ,'.nc',sep="")
        
        t = Sys.glob(paste("/datos/osman/nmme/monthly", file_pattern, sep = "/"))
        
        for(j in 1:length(t)){
          
          v = nc_open(t[j])
          
          v2[,,,j,i,m] = ncvar_get(v, "tref")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
          
          nc_close(v)
        }               
      }
    }
    
    v2[which(v2<=0)]=NA 
    
    londim = ncdim_def("lon", "grados_este", as.double(lon2))
    latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
    timedim = ncdim_def("time", "Years", as.double(anios))
    mesesdim = ncdim_def("meses", "meses", as.double(1:3))
    rdim = ncdim_def("r","miembros", as.double(1:r))
    tempdim = ncdim_def("temp", "°C", as.double(v2))
    seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))
    
    
    fillvalue = NA
    dlname = "temperatura"
    temp_def = ncvar_def("temp", "Kelvin", list(londim, latdim, mesesdim, rdim, timedim, seasondim ), fillvalue, dlname, prec="single")
    
    ncfname = paste(nombre, "-temp", ".nc", sep = "")
    
    ncout = nc_create(ncfname, list(temp_def), force_v4=T)
    
    ncvar_put(ncout, temp_def, v2)
    
    nc_close(ncout) #verificar donde guarda los nc
    
    
    ### PP ###
    
    v2 = array(NA, dim = c(length(lon2), length(lat2), 3, r, length(anios), 4))
    
    for(m in 1:4){
      final_month = init_cond[m] - 1
      
      ic_format=formatC(init_cond[m], width = 2, format = "d", flag = "0")
      
      for(i in 1:length(anios)){
        
        file_pattern = paste('prec_Amon_',nombre,"_",anios[i] , ic_format,"_r*_", as.character(anios[i]),ic_format, '-' , 
                             as.character(anios[i] + 1), ic_format=formatC(final_month, width = 2, format = "d", flag = "0") ,'.nc',sep="")
        
        t = Sys.glob(paste("/datos/osman/nmme/monthly", file_pattern, sep = "/"))
        
        for(j in 1:length(t)){
          
          v = nc_open(t[j])
          
          v2[,,,j,i,m] = ncvar_get(v, "prec")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]*30
          
          nc_close(v)
        }               
      }
    }
    
    v2[which(v2<0)]=NA
    
    londim = ncdim_def("lon", "grados_este", as.double(lon2))
    latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
    timedim = ncdim_def("time", "Years", as.double(anios))
    mesesdim = ncdim_def("meses", "meses", as.double(1:3))
    rdim = ncdim_def("r","miembros", as.double(1:r))
    tempdim = ncdim_def("PP", "mm x day", as.double(v2))
    seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))
    
    
    fillvalue = NA
    dlname = "precipitacion"
    temp_def = ncvar_def("pp", "mm x dia", list(londim, latdim, mesesdim, rdim, timedim, seasondim ), fillvalue, dlname, prec="single")
    
    ncfname = paste(nombre, "-pp", ".nc", sep = "")
    
    ncout = nc_create(ncfname, list(temp_def), force_v4=T)
    
    ncvar_put(ncout, temp_def, v2)
    
    nc_close(ncout) #verificar donde guarda los nc
    
  } else {
    ruta = "/datos/osman/nmme/monthly"
    mask=as.matrix(read.table("mascara.txt"))
    t = list.files("/datos/osman/nmme/monthly", pattern = paste("tref_Amon_", nombre, sep = ""))
    
    tref = nc_open(paste(ruta, t[1], sep = "/"))
    temp = ncvar_get(tref, names(tref$var)[1])
    
    lat = ncvar_get(tref, "Y")
    lon = ncvar_get(tref, "X")
    
    lon2 = lon[which(lon==275):which(lon==330)]
    lat2 = lat[which(lat==-60):which(lat==15)]
    
    anios= seq(from = 1982, to = 2010, by = 1)
    
    v2 = array(NA, dim = c(length(lon2), length(lat2), 3, r, length(anios), 4))
    
    init_cond=c(2,5,8,11)
    
    for(m in 1:4){
      final_month = init_cond[m] - 1
      
      ic_format=formatC(init_cond[m], width = 2, format = "d", flag = "0")
      
      for(i in 1:length(anios)){
        
        file_pattern = paste('tref_Amon_',nombre,"_",anios[i] , ic_format,"_r*_", as.character(anios[i]),ic_format, '-' , 
                             as.character(anios[i] + 1), ic_format=formatC(final_month, width = 2, format = "d", flag = "0") ,'.nc',sep="")
        
        t = Sys.glob(paste("/datos/osman/nmme/monthly", file_pattern, sep = "/"))
        
        for(j in 1:(length(t)-4)){
          
          if(j>=18){ 
            v = nc_open(t[j+4]) # saltea r25 a r28 (en el orden que quedan en t. quedan igual para todos los modelos q llegan hasta 28)
            v2[,,,j,i,m] = ncvar_get(v, "tref")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
          } else {
            v = nc_open(t[j])
            v2[,,,j,i,m] = ncvar_get(v, "tref")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
          }
          
          nc_close(v)
        }               
      }
    }
    
    v2[which(v2<=0)]=NA 
    
    londim = ncdim_def("lon", "grados_este", as.double(lon2))
    latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
    timedim = ncdim_def("time", "Years", as.double(anios))
    mesesdim = ncdim_def("meses", "meses", as.double(1:3))
    rdim = ncdim_def("r","miembros", as.double(1:r))
    tempdim = ncdim_def("temp", "kelvin", as.double(v2))
    seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))
    
    
    fillvalue = NA
    dlname = "temperatura"
    temp_def = ncvar_def("temp", "Kelvin", list(londim, latdim, mesesdim, rdim, timedim, seasondim ), fillvalue, dlname, prec="single")
    
    ncfname = paste(nombre, "-temp", ".nc", sep = "")
    
    ncout = nc_create(ncfname, list(temp_def), force_v4=T)
    
    ncvar_put(ncout, temp_def, v2)
    
    nc_close(ncout) #verificar donde guarda los nc
    
    
    ### PP ###
    
    v2 = array(NA, dim = c(length(lon2), length(lat2), 3, r, length(anios), 4))
    
    for(m in 1:4){
      final_month = init_cond[m] - 1
      
      ic_format=formatC(init_cond[m], width = 2, format = "d", flag = "0")
      
      for(i in 1:length(anios)){
        
        file_pattern = paste('prec_Amon_',nombre,"_",anios[i] , ic_format,"_r*_", as.character(anios[i]),ic_format, '-' , 
                             as.character(anios[i] + 1), ic_format=formatC(final_month, width = 2, format = "d", flag = "0") ,'.nc',sep="")
        
        t = Sys.glob(paste("/datos/osman/nmme/monthly", file_pattern, sep = "/"))
        
        for(j in 1:(length(t)-4)){
          
          if(j>=18){
            v = nc_open(t[j+4])
            v2[,,,j,i,m] = ncvar_get(v, "prec")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
          } else {
            v = nc_open(t[j])
            v2[,,,j,i,m] = ncvar_get(v, "prec")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]*30
          }
          nc_close(v)
        }               
      }
    }
    
    v2[which(v2<0)]=NA
    
    
    londim = ncdim_def("lon", "grados_este", as.double(lon2))
    latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
    timedim = ncdim_def("time", "Years", as.double(anios))
    mesesdim = ncdim_def("meses", "meses", as.double(1:3))
    rdim = ncdim_def("r","miembros", as.double(1:r))
    tempdim = ncdim_def("PP", "mm x day", as.double(v2))
    seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))
    
    
    fillvalue = NA
    dlname = "precipitacion"
    temp_def = ncvar_def("pp", "mm x dia", list(londim, latdim, mesesdim, rdim, timedim, seasondim ), fillvalue, dlname, prec="single")
    
    ncfname = paste(nombre, "-pp", ".nc", sep = "")
    
    ncout = nc_create(ncfname, list(temp_def), force_v4=T)
    
    ncvar_put(ncout, temp_def, v2)
    
    nc_close(ncout) #verificar donde guarda los nc
    
  }
}


##### CM2p1 #### 
#guardado y seleccion para GFDL-CM2p1 
cm2p1 = function(){
  
  r = 10
  
  ruta = "/datos/osman/nmme/monthly"
  mask=as.matrix(read.table("mascara.txt"))
  t = list.files("/datos/osman/nmme/monthly", pattern = "tref_Amon_GFDL-CM2p1")
  
  tref = nc_open(paste(ruta, t[1], sep = "/"))
  temp = ncvar_get(tref, names(tref$var)[1])
  
  lat = ncvar_get(tref, "Y")
  lon = ncvar_get(tref, "X")
  
  lon2 = lon[which(lon==275):which(lon==330)]
  lat2 = lat[which(lat==-60):which(lat==15)]
  
  
  anios= seq(from = 1982, to = 2010, by = 1)
  
  v2 = array(NA, dim = c(length(lon2), length(lat2), 3, r, length(anios), 4))
  
  init_cond=c(2,5,8,11)
  
  for(m in 1:4){
    final_month = init_cond[m] - 1
    
    ic_format=formatC(init_cond[m], width = 2, format = "d", flag = "0")
    
    for(i in 1:length(anios)){
      
      file_pattern = paste('tref_Amon_GFDL-CM2p1',"_",anios[i] , ic_format,"_r*_", as.character(anios[i]),ic_format, '-' , 
                           as.character(anios[i] + 1), ic_format=formatC(final_month, width = 2, format = "d", flag = "0") ,'.nc',sep="")
      
      t = Sys.glob(paste("/datos/osman/nmme/monthly", file_pattern, sep = "/"))
      
      for(j in 1:length(t)){
        
        v = nc_open(t[j])
        
        v2[,,,j,i,m] = ncvar_get(v, "tref")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]
        
        nc_close(v)
      }               
    }
  }
  
  v2[which(v2<=0)]=NA 
  
  londim = ncdim_def("lon", "grados_este", as.double(lon2))
  latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
  timedim = ncdim_def("time", "Years", as.double(anios))
  mesesdim = ncdim_def("meses", "meses", as.double(1:3))
  rdim = ncdim_def("r","miembros", as.double(1:r))
  tempdim = ncdim_def("temp", "°C", as.double(v2))
  seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))
  
  
  fillvalue = NA
  dlname = "temperatura"
  temp_def = ncvar_def("temp", "Kelvin", list(londim, latdim, mesesdim, rdim, timedim, seasondim ), fillvalue, dlname, prec="single")
  
  ncfname = paste("GFDL-CM2p1", "-temp", ".nc", sep = "")
  
  ncout = nc_create(ncfname, list(temp_def), force_v4=T)
  
  ncvar_put(ncout, temp_def, v2)
  
  nc_close(ncout) #verificar donde guarda los nc
  
  
  ### PP ###
  
  v2 = array(NA, dim = c(length(lon2), length(lat2), 3, r, length(anios), 4))
  
  for(m in 1:4){
    final_month = init_cond[m] - 1
    
    ic_format=formatC(init_cond[m], width = 2, format = "d", flag = "0")
    
    for(i in 1:length(anios)){
      
      r_10 = c(1:10)
      
      file_pattern = paste('prec_Amon_GFDL-CM2p1',"_",anios[i] , ic_format, "_r",r_10,"_", as.character(anios[i]),ic_format, '-' , 
                           as.character(anios[i] + 1), ic_format=formatC(final_month, width = 2, format = "d", flag = "0") ,'.nc',sep="")
      
      t = Sys.glob(paste("/datos/osman/nmme/monthly", file_pattern, sep = "/"))
      
      for(j in 1:length(t)){
        
        v = nc_open(t[j])
        
        v2[,,,j,i,m] = ncvar_get(v, "prec")[which(lon==275):which(lon==330), which(lat==-60):which(lat==15),2:4]*30
        
        nc_close(v)
      }               
    }
  }
  
  v2[which(v2<0)]=NA
  
  londim = ncdim_def("lon", "grados_este", as.double(lon2))
  latdim = ncdim_def("lat", "grados_norte", as.double(lat2))
  timedim = ncdim_def("time", "Years", as.double(anios))
  mesesdim = ncdim_def("meses", "meses", as.double(1:3))
  rdim = ncdim_def("r","miembros", as.double(1:r))
  tempdim = ncdim_def("PP", "mm x day", as.double(v2))
  seasondim = ncdim_def("estaciones", "estaciones", as.double(1:4))
  
  
  fillvalue = NA
  dlname = "precipitacion"
  temp_def = ncvar_def("pp", "mm x dia", list(londim, latdim, mesesdim, rdim, timedim, seasondim ), fillvalue, dlname, prec="single")
  
  ncfname = paste("GFDL-CM2p1", "-pp", ".nc", sep = "")
  
  ncout = nc_create(ncfname, list(temp_def), force_v4=T)
  
  ncvar_put(ncout, temp_def, v2)
  
  nc_close(ncout) #verificar donde guarda los nc
  
}


#### MEAN_SD ####
# media y desvio de los archivos guardados por modelos_rx2 y cm2p1

mean_sd = function(nombre){
  
  ruta =  "/home/luciano.andrian/tesis/ncfiles/"
  
  mask=as.matrix(read.table("mascara.txt"))
  
  anios= seq(from = 1982, to = 2010, by = 1)
  ### TEMP ###
  
  nc = nc_open(paste(ruta, nombre, "-temp.nc", sep = ""))
  
  temp = ncvar_get(nc, "temp")
  lon = ncvar_get(nc, "lon")
  lat = ncvar_get(nc, "lat")
  T1 =  array(NA, dim = c(length(lon2), length(lat2), 4)) 
  for(i in 1:4){
    T1[,,i] = apply(temp[,,,,,i], c(1,2), mean, na.rm = TRUE)*mask  
  }
  
  # SD # error ensamble
  
  # la correccion seria:
  # promedio meses en estaciones
  #sd_t = array(NA, dim = c(length(lon2), length(lat2),4)) 
  #for(i in 1:4){
  #  
  #  aux =  apply(apply(temp[,,,,,i], c(1,2,4,5), mean, na.rm = TRUE), c(1,2,3), sd, na.rm = T)
  #  sd_t[,,i] = apply(aux, c(1,2), mean, na.rm = T) #ensamble
    
  #}
  
  
  T2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4))
  for(i in 1:4){
    T2[,,,i] =  apply(temp[,,,,,i], c(1,2,5), mean, na.rm = TRUE)
  }
  
  sd_t = array(NA, dim = c(length(lon2), length(lat2),4)) 
  for(i in 1:4){
    sd_t[,,i] = apply(T2[,,,i], c(1,2), sd, na.rm = TRUE)*mask
  }
  
  
  ### PP ###
  
  nc = nc_open(paste(ruta, nombre, "-pp.nc", sep = ""))
  
  PP = ncvar_get(nc, "pp")
  lon = ncvar_get(nc, "lon")
  lat = ncvar_get(nc, "lat")
  PP1 =  array(NA, dim = c(length(lon2), length(lat2), 4)) 
  for(i in 1:4){
    PP1[,,i] = apply(PP[,,,,,i], c(1,2), mean, na.rm = TRUE)*mask  
  }
  
  # SD # error ensamble
  
  # la correccion seria:
  # promedio meses en estaciones
  #sd_p = array(NA, dim = c(length(lon2), length(lat2),4)) 
  #for(i in 1:4){
  #  
  #  aux =  apply(apply(PP[,,,,,i], c(1,2,4,5), mean, na.rm = TRUE), c(1,2,3), sd, na.rm = T)
  #  sd_p[,,i] = apply(aux, c(1,2), mean, na.rm = T) #ensamble
  
  #}
  
  PP2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4))
  for(i in 1:4){
    PP2[,,,i] =  apply(PP[,,,,,i], c(1,2,5), mean, na.rm = TRUE)
  }
  
  sd_pp = array(NA, dim = c(length(lon2), length(lat2),4)) 
  for(i in 1:4){
    sd_pp[,,i] = apply(PP2[,,,i], c(1,2), sd, na.rm = TRUE)*mask
  }
  
  # anularia T2 y PP2
  T_PP = list()
  T_PP[[1]] = T1
  T_PP[[2]] = sd_t
  T_PP[[3]] = PP1
  T_PP[[4]] = sd_pp
  T_PP[[5]] = T2 
  T_PP[[6]] = PP2
  return(T_PP)
  
  nc_close(nc)
  
}






#### ANOVA_FUN ####
# anova, ss y cocientes (no tests)
anova_fun = function(){
  variable = as.character(readline("Variable (pp), (temp): "))
  ensemble_total = readline("Todos los modelos?(si, no): ")
  
  k = c(10, 10, 12, 12, 4, 28, 10, 20)
  t = 29 #anios
  lon2 = read.table("lon2.txt")[,1]
  lat2 = read.table("lat2.txt")[,1]
  
  anios = seq(from = 1982, to = 2010, by = 1)
  
  library(ncdf4)
  
  if(ensemble_total == "si"){
    
    m = 8 #modelos
    
    nc = nc_open(paste("/home/luciano.andrian/tesis/ncfiles/pre_anova-",variable,".nc", sep = ""))
    v_seasons = ncvar_get(nc, variable) # lon lat members years seasons models 
    nc_close(nc)
    
    # dim v_seasons[lon, lat, max_miembros*, anios, seasons, modelos]
    # *los que tienen menos r dejan el resto en NA
    
    mask = as.matrix(read.table("mascara.txt"))
    
    # Hodson - Sutton
    
    x000 = array(NA, dim = c(length(lon2), length(lat2), 4))
    xt00 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4))
    x0m0 = array(NA, dim = c(length(lon2), length(lat2), 4, 8))
    xtm0 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    
    for(i in 1:4){
      x000[,,i] = apply(v_seasons[,,,,i,],c(1,2), mean, na.rm = T)         
      xt00[,,,i] = apply(v_seasons[,,,,i,],c(1,2,4), mean, na.rm = T)
      x0m0[,,i,] = apply(v_seasons[,,,,i,], c(1,2,5), mean, na.rm = T) 
      xtm0[,,,i,] = apply(v_seasons[,,,,i,],c(1,2,4,5), mean, na.rm = T)
    }
    
    
    
    # calculo de los estimadores SS's
    
    ########################################### SSa ###########################################
    
    aux = array(NA, dim = c(length(lon2), length(lat2), length(anios),4,8))
    for(i in 1:length(anios)){
      aux[,,i,,] = (xt00[,,i,]-x000)**2
    }
    
    SSa = apply(sum(k)*aux, c(1,2,4), sum)
    
    
    ########################################### SSb ###########################################
    
    aux = array(NA, dim = c(length(lon2), length(lat2), 4, 8))
    for(i in 1:8){
      aux[,,,i] = (x0m0[,,,i]-x000)**2
    }
    
    for(i in 1:8){ 
      aux[,,,i] = aux[,,,i]*k[i]*t
    }
    
    SSb = apply(aux, c(1,2,3), sum)
    
    
    ########################################### SSe ###########################################
    
    aux = array(NA, dim = c(length(lon2), length(lat2), 28, length(anios), 4, 8))
    for(i in 1:28){
      aux[,, i,,,] = (v_seasons[,,i,,,]-xtm0)**2
    }
    
    SSe = apply(aux, c(1,2,5), sum, na.rm = T)  
    
    
    ########################################### SSg ########################################### 
    
    aux = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:8){
      aux[,,,,i] = xtm0[,,,,i] - xt00
    }
    
    aux2 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:length(anios)){
      aux2[,,i,,] = aux[,,i,,] - x0m0
    }
    
    aux3 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 8))
    for(i in 1:length(anios)){
      for(j in 1:8){
        aux3[,,i,,j] = (aux2[,,i,,j] + x000)**2
      }
    }
    
    for(i in 1:8){ 
      aux3[,,,,i] = aux3[,,,,i]*k[i]    # ERROR! ESTO ESTABA MULTIPLICADO POR t. 
    }
    
    SSg = apply(aux3, c(1,2,4), sum, na.rm = T)   
    
    
    TSS = SSa + SSb + SSg + SSe
    
    SS = list()
    SS[[1]] = SSa
    SS[[2]] = SSb
    SS[[3]] = SSg
    SS[[4]] = SSe
    SS[[5]] = TSS 
    
    #cocientes
    
    c_b = (SSb - (m-1)/(t*105)*SSe)/TSS   #fraccion de TSS explicada por SSb
    
    c_a = (SSa - (t-1)/(t*105)*SSe)/TSS   #fraccion de TSS explicada por SSa 
    
    c_g = (SSg - ((t-1)*(m-1))/(t*105)*SSe)/TSS  #fraccion de TSS explicada por SSg   
    
    c_e = SSe/TSS #fraccion de ÇTSS explicada por SSe
    
    SS[[6]] = c_a
    SS[[7]] = c_b
    SS[[8]] = c_g
    SS[[9]] = c_e
    
    
    return(SS)
    
  } else {
    
    todos = as.numeric(readline("Sacar todos los modelos (1) o elegir uno (2): "))
    if(todos == 1){
      
      mask = as.matrix(read.table("mascara.txt"))
      
      m = 7 #modelos
      
      nc = nc_open(paste("/home/luciano.andrian/tesis/ncfiles/pre_anova-",variable,".nc", sep = ""))
      v_seasons = ncvar_get(nc, variable) # lon lat members years seasons models 
      nc_close(nc)
      
      ss_v = list() # lista para guardar cada SS al omitir cada modelo
      
      for(f in 1:8){
        
        nomodel = seq(1:8)
        nomodel = nomodel[nomodel!=f]  #esto para eliminar la dimencion correspondiente al modelo en v_seasons ya que traer errores en apply
        
        v_seasons2 = v_seasons[,,,,,nomodel]   #ok
        
        
        #igual que antes. pero ahora con 7 modelos
        x000 = array(NA, dim = c(length(lon2), length(lat2), 4))
        xt00 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4))
        x0m0 = array(NA, dim = c(length(lon2), length(lat2), 4, 7))
        xtm0 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 7))
        
        for(i in 1:4){
          x000[,,i] = apply(v_seasons2[,,,,i,],c(1,2), mean, na.rm = T)         
          xt00[,,,i] = apply(v_seasons2[,,,,i,],c(1,2,4), mean, na.rm = T)
          x0m0[,,i,] = apply(v_seasons2[,,,,i,], c(1,2,5), mean, na.rm = T) 
          xtm0[,,,i,] = apply(v_seasons2[,,,,i,],c(1,2,4,5), mean, na.rm = T)
        }
        # para lo proximo es necesario un nuevo k, que no tenga la cantidad de miembros que le corresponden al modelo omitido
        
        k = c(10, 10, 12, 12, 4, 28, 10, 20) # es necesario q se defina para cada j
        k[f] = NA # eliminando el correspondiente al modelo omitido
        k = k[!is.na(k)] # lenght = 7
        
        
        # calculo de los estimadores SS's
        
        ########################################### SSa ###########################################
        
        aux = array(NA, dim = c(length(lon2), length(lat2), length(anios),4,7))  #7 en lugar de 8
        for(i in 1:length(anios)){
          aux[,,i,,] = (xt00[,,i,]-x000)**2
        }
        
        SSa = apply(sum(k)*aux, c(1,2,4), sum)
        
        
        ########################################### SSb ###########################################
        
        aux = array(NA, dim = c(length(lon2), length(lat2), 4, 7)) #7 en lugar de 8
        for(i in 1:7){
          aux[,,,i] = (x0m0[,,,i]-x000)**2
        }
        
        for(i in 1:7){ 
          aux[,,,i] = aux[,,,i]*k[i]*t   ## aca va k. y por esto es necesario cambiarlo
        }
        
        SSb = apply(aux, c(1,2,3), sum)
        
        
        ########################################### SSe ###########################################
        
        aux = array(NA, dim = c(length(lon2), length(lat2), 28, length(anios), 4, 7)) #7 en lugar de 8
        for(i in 1:28){
          aux[,, i,,,] = (v_seasons2[,,i,,,]-xtm0)**2
        }
        
        SSe = apply(aux, c(1,2,5), sum, na.rm = T)  
        
        
        ########################################### SSg ########################################### 
        
        aux = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 7))
        for(i in 1:7){
          aux[,,,,i] = xtm0[,,,,i] - xt00
        }
        
        aux2 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 7))
        for(i in 1:length(anios)){
          aux2[,,i,,] = aux[,,i,,] - x0m0
        }
        
        aux3 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 7))
        for(i in 1:length(anios)){
          for(j in 1:7){
            aux3[,,i,,j] = (aux2[,,i,,j] + x000)**2
          }
        }
        
        for(i in 1:7){ 
          aux3[,,,,i] = aux3[,,,,i]*k[i]    
        }
        
        SSg = apply(aux3, c(1,2,4), sum, na.rm = T)   
        
        
        TSS = SSa + SSb + SSg + SSe
        
        SS = list()
        SS[[1]] = SSa
        SS[[2]] = SSb
        SS[[3]] = SSg
        SS[[4]] = SSe
        SS[[5]] = TSS 
        
        #cocientes
        
        c_b = (SSb - (m-1)/(t*(sum(k)-1))*SSe)/TSS   #fraccion de TSS explicada por SSb
        
        c_a = (SSa - (t-1)/(t*(sum(k)-1))*SSe)/TSS   #fraccion de TSS explicada por SSa 
        
        c_g = (SSg - ((t-1)*(m-1))/(t*(sum(k)-1))*SSe)/TSS  #fraccion de TSS explicada por SSg   
        
        c_e = SSe/TSS #fraccion de ÇTSS explicada por SSe
        
        SS[[6]] = c_a
        SS[[7]] = c_b
        SS[[8]] = c_g
        SS[[9]] = c_e
        
        ss_v[[f]] = SS
        
        rm(SS)
        
      }
      
      return(ss_v)
      
    } else {
      
      f = as.numeric(readline("Modelo a eliminar del ensamble. (1)COLA-CCSM4, (2)GFDL-CM2p1, (3)GFDL-FLOR-A06, (4)GFDL-FLOR-B01, (5)NASA-GEOS5, (6)NCEP-CFSv2, (7)CMC-CanCM4i, (8)CMC-CanSIPSv2: " ))
      
      mask = as.matrix(read.table("mascara.txt"))
      
      m = 7 #modelos
      
      nc = nc_open(paste("/home/luciano.andrian/tesis/ncfiles/pre_anova-",variable,".nc", sep = ""))
      v_seasons = ncvar_get(nc, variable) # lon lat members years seasons models 
      nc_close(nc)
      
      
      nomodel = seq(1:8)
      nomodel = nomodel[nomodel!=f]  #esto para eliminar la dimencion correspondiente al modelo en v_seasons ya que traer errores en apply
      v_seasons2 = v_seasons[,,,,,nomodel]   #ok
      
      k[f] = NA # eliminando el correspondiente al modelo omitido
      k = k[!is.na(k)] # lenght = 7
      
      #igual que antes. pero ahora con 7 modelos
      x000 = array(NA, dim = c(length(lon2), length(lat2), 4))
      xt00 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4))
      x0m0 = array(NA, dim = c(length(lon2), length(lat2), 4, 7))
      xtm0 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 7))
      
      for(i in 1:4){
        x000[,,i] = apply(v_seasons2[,,,,i,],c(1,2), mean, na.rm = T)         
        xt00[,,,i] = apply(v_seasons2[,,,,i,],c(1,2,4), mean, na.rm = T)
        x0m0[,,i,] = apply(v_seasons2[,,,,i,], c(1,2,5), mean, na.rm = T) 
        xtm0[,,,i,] = apply(v_seasons2[,,,,i,],c(1,2,4,5), mean, na.rm = T)
      }  
      
      # calculo de los estimadores SS's
      
      ########################################### SSa ###########################################
      
      aux = array(NA, dim = c(length(lon2), length(lat2), length(anios),4,7))  #7 en lugar de 8
      for(i in 1:length(anios)){
        aux[,,i,,] = (xt00[,,i,]-x000)**2
      }
      
      SSa = apply(sum(k)*aux, c(1,2,4), sum)
      
      
      ########################################### SSb ###########################################
      
      aux = array(NA, dim = c(length(lon2), length(lat2), 4, 7)) #7 en lugar de 8
      for(i in 1:7){
        aux[,,,i] = (x0m0[,,,i]-x000)**2
      }
      
      for(i in 1:7){ 
        aux[,,,i] = aux[,,,i]*k[i]*t   ## aca va k. y por esto es necesario cambiarlo
      }
      
      SSb = apply(aux, c(1,2,3), sum)
      
      
      ########################################### SSe ###########################################
      
      aux = array(NA, dim = c(length(lon2), length(lat2), 28, length(anios), 4, 7)) #7 en lugar de 8
      for(i in 1:28){
        aux[,, i,,,] = (v_seasons2[,,i,,,]-xtm0)**2
      }
      
      SSe = apply(aux, c(1,2,5), sum, na.rm = T)  
      
      
      ########################################### SSg ########################################### 
      
      aux = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 7))
      for(i in 1:7){
        aux[,,,,i] = xtm0[,,,,i] - xt00
      }
      
      aux2 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 7))
      for(i in 1:length(anios)){
        aux2[,,i,,] = aux[,,i,,] - x0m0
      }
      
      aux3 = array(NA, dim = c(length(lon2), length(lat2), length(anios), 4, 7))
      for(i in 1:length(anios)){
        for(j in 1:7){
          aux3[,,i,,j] = (aux2[,,i,,j] + x000)**2
        }
      }
      
      for(i in 1:7){ 
        aux3[,,,,i] = aux3[,,,,i]*k[i]    
      }
      
      SSg = apply(aux3, c(1,2,4), sum, na.rm = T)   
      
      
      TSS = SSa + SSb + SSg + SSe
      
      SS = list()
      SS[[1]] = SSa
      SS[[2]] = SSb
      SS[[3]] = SSg
      SS[[4]] = SSe
      SS[[5]] = TSS 
      
      #cocientes
      
      c_b = (SSb - (m-1)/(t*(sum(k)-1))*SSe)/TSS   #fraccion de TSS explicada por SSb
      
      c_a = (SSa - (t-1)/(t*(sum(k)-1))*SSe)/TSS   #fraccion de TSS explicada por SSa 
      
      c_g = (SSg - ((t-1)*(m-1))/(t*(sum(k)-1))*SSe)/TSS  #fraccion de TSS explicada por SSg   
      
      c_e = SSe/TSS #fraccion de ÇTSS explicada por SSe
      
      SS[[6]] = c_a
      SS[[7]] = c_b
      SS[[8]] = c_g
      SS[[9]] = c_e
      
      return(SS)
      
    }
  }  
} 

#### TEST_COS ####
#test cocientes de anova_fun
test_cos = function(SS, ensemble_total, nomodel_selec, no_model){

  
  #### testeos ####
  
  # crea una mascara para graficar los cocientes SS
  
  #SS[[1]] = SSa
  #SS[[2]] = SSb
  #SS[[3]] = SSg
  #SS[[4]] = SSe
  #SS[[5]] = TSS 
  
  #SS[[6]] = c_a
  #SS[[7]] = c_b
  #SS[[8]] = c_g
  #SS[[9]] = c_e
  
  if( ensemble_total == "si"){
    
    mask = as.matrix(read.table("mascara.txt"))
    mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
    for(i in 1:4){
      mask_arr[,,i] = mask
    }
    # 
    
    k = c(10, 10, 12, 12, 4, 28, 10, 20) #miembros de cada modelo
    t = 29 #anios
    m = 8 #modelos
    
    alpha_f = qf(0.95,t-1,t*sum(k))
    beta_f = qf(0.95, m-1, t*sum(k))
    gamma_f = qf(0.95, (m-1)*(t-1), t*sum(k)) 
    
    sigma_alpha_2 = SS[[1]]/((t-1)) 
    
    sigma_beta_2 = SS[[2]]/(m-1) 
    
    sigma_gamma_2 = SS[[3]]/((t-1)*(m-1))  
    
    sigma_epsilon_2 = SS[[4]]/(t*(sum(k)-1))     
    
    alpha = (sigma_alpha_2/sigma_epsilon_2)*mask_arr
    beta = (sigma_beta_2/sigma_epsilon_2)*mask_arr
    gamma = (sigma_gamma_2/sigma_epsilon_2)*mask_arr
    
    # esta OK.
    

    
    alpha[which(alpha<alpha_f)] = NA
    alpha[which(!is.na(alpha))] = 1
    
    beta[which(beta<beta_f)] = NA
    beta[which(!is.na(beta))] = 1
    
    gamma[which(gamma<gamma_f)] = NA
    gamma[which(!is.na(gamma))] = 1
    
    sig = list()
    sig[[1]] = alpha
    sig[[2]] = beta
    sig[[3]] = gamma
    sig[[4]] = array(data = 1, dim = c(56, 76, 4))*mask_arr
    
    sig[[5]] = sigma_alpha_2
    sig[[6]] = sigma_beta_2
    sig[[7]] = sigma_gamma_2
    sig[[8]] = sigma_epsilon_2
    return(sig)
  
  } else {
    
    todos = nomodel_selec
    if(todos == "si"){
    
    nomodel = as.numeric(readline("Modelo a eliminar del ensamble. (1)COLA-CCSM4, (2)GFDL-CM2p1, (3)GFDL-FLOR-A06, (4)GFDL-FLOR-B01, (5)NASA-GEOS5, (6)NCEP-CFSv2, (7)CMC-CanCM4i, (8)CMC-CanSIPSv2: " ))
    
    mask = as.matrix(read.table("mascara.txt"))
    mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
    for(i in 1:4){
      mask_arr[,,i] = mask
    }
     
    k = c(10, 10, 12, 12, 4, 28, 10, 20) #miembros de cada modelo
    t = 29 #anios
    k[nomodel] = NA
    m = 7 #modelos
    
    alpha_f = qf(0.95,t-1,t*sum(k, na.rm = T))
    beta_f = qf(0.95, m-1, t*sum(k, na.rm = T))
    gamma_f = qf(0.95, (m-1)*(t-1), t*sum(k, na.rm = T)) 
    
    sigma_alpha_2 = SS[[1]]/((t-1)) 
    
    sigma_beta_2 = SS[[2]]/(m-1) 
    
    sigma_gamma_2 = SS[[3]]/((t-1)*(m-1))  
    
    sigma_epsilon_2 = SS[[4]]/(t*(sum(k, na.rm = T)-1))     
    
    alpha = (sigma_alpha_2/sigma_epsilon_2)*mask_arr
    beta = (sigma_beta_2/sigma_epsilon_2)*mask_arr
    gamma = (sigma_gamma_2/sigma_epsilon_2)*mask_arr
    
    # esta OK.
    
    alpha[which(alpha<alpha_f)] = NA
    alpha[which(!is.na(alpha))] = 1
    
    beta[which(beta<beta_f)] = NA
    beta[which(!is.na(beta))] = 1
    
    gamma[which(gamma<gamma_f)] = NA
    gamma[which(!is.na(gamma))] = 1
    
    sig = list()
    sig[[1]] = alpha
    sig[[2]] = beta
    sig[[3]] = gamma
    sig[[4]] = array(data = 1, dim = c(56, 76, 4))*mask_arr
    
    sig[[5]] = sigma_alpha_2
    sig[[6]] = sigma_beta_2
    sig[[7]] = sigma_gamma_2
    sig[[8]] = sigma_epsilon_2
    return(sig)
    
    } else {
      
      nomodel = no_model
      
      mask = as.matrix(read.table("mascara.txt"))
      mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
      for(i in 1:4){
        mask_arr[,,i] = mask
      }
      
      k = c(10, 10, 12, 12, 4, 28, 10, 20) #miembros de cada modelo
      t = 29 #anios
      k[nomodel] = NA
      m = 7 #modelos
      
      alpha_f = qf(0.95,t-1,t*sum(k, na.rm = T))
      beta_f = qf(0.95, m-1, t*sum(k, na.rm = T))
      gamma_f = qf(0.95, (m-1)*(t-1), t*sum(k, na.rm = T)) 
      
      sigma_alpha_2 = SS[[1]]/((t-1)) 
      
      sigma_beta_2 = SS[[2]]/(m-1) 
      
      sigma_gamma_2 = SS[[3]]/((t-1)*(m-1))  
      
      sigma_epsilon_2 = SS[[4]]/(t*(sum(k, na.rm = T)-1))     
      
      alpha = (sigma_alpha_2/sigma_epsilon_2)*mask_arr
      beta = (sigma_beta_2/sigma_epsilon_2)*mask_arr
      gamma = (sigma_gamma_2/sigma_epsilon_2)*mask_arr
      
      # esta OK.
      
      alpha[which(alpha<alpha_f)] = NA
      alpha[which(!is.na(alpha))] = 1
      
      beta[which(beta<beta_f)] = NA
      beta[which(!is.na(beta))] = 1
      
      gamma[which(gamma<gamma_f)] = NA
      gamma[which(!is.na(gamma))] = 1
      
      sig = list()
      sig[[1]] = alpha
      sig[[2]] = beta
      sig[[3]] = gamma
      sig[[4]] = array(data = 1, dim = c(56, 76, 4))*mask_arr
      
      sig[[5]] = sigma_alpha_2
      sig[[6]] = sigma_beta_2
      sig[[7]] = sigma_gamma_2
      sig[[8]] = sigma_epsilon_2
      return(sig)
      
    }
  }
}
#### MAPA_SIG ####
#mapas con zonas no significativas marcadas con puntos
#se complementa con anova_fun y test_cos
mapa_sig = function(lista,lista2, titulo, nombre_fig, escala, label_escala, resta, brewer, revert, niveles, contour, lon, lat
                    , escala_dis, breaks_c_f, alpha, topo2, altura, salida){
  
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
  mask = as.matrix(read.table("mascara.txt"))                  
  
  mask_arr = array(NA, dim = c(length(lon), length(lat), 4))
  for(i in 1:4){
    mask_arr[,,i] <- mask
  }
  
  #topo = metR::GetTopography(-85 + 359.5, -29 + 359.5, 15.5,  -60.5, resolution = 1/res) # mapa topografia
  topo2 = topo #
  topo2[which(topo2$h<altura)]=NA
  
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
        g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon + 360, y= lat,fill = temp),alpha=0.9, na.rm = T) +
          
          geom_contour_fill(data=data,aes(x = lon + 360, y= lat, z = temp),alpha=1, na.fill = -10000, breaks =  breaks_c_f)+
          
          geom_point(data=puntos2, aes(x = lon + 360, y = lat), col="black",size=0.5, alpha = alpha)+
          
          geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          #guides(fill = guide_legend(reverse = TRUE))+
          
          geom_polygon(data=mapa, aes(x=long + 360, y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85 + 360, -33 + 360))+
          theme(axis.text.y   = element_text(size=14), axis.text.x   = element_text(size=14), axis.title.y  = element_text(size=14),
                axis.title.x  = element_text(size=14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill=NA, size=3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust=0.5))
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
        
      } else {
        g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon + 360, y= lat,fill = temp),alpha=0.9, na.rm = T)+
          
          #geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          geom_point(data=puntos2, aes(x = lon + 360, y = lat), col="black",size=0.5, alpha = alpha)+
          
          geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          #guides(fill = guide_legend(reverse = TRUE))+
          
          geom_polygon(data=mapa, aes(x=long + 360, y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85 + 360, -33 + 360))+
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
        
        g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data=data,aes(x = lon + 360, y= lat,fill = temp),alpha = 0.7, na.rm = T) +
          
          geom_contour_fill(data=data,aes(x = lon + 360, y = lat, z = temp),alpha=1, na.fill = -10000, breaks =  breaks_c_f) +
          
          geom_point(data=puntos2, aes(x = lon + 360, y = lat), col="black",size=0.5, alpha = alpha)+
          
          geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          #guides(fill = guide_legend(reverse = TRUE)) +
          
          geom_polygon(data=mapa, aes(x=long + 360, y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85 + 360, -33 + 360))+
          theme(axis.text.y   = element_text(size=14), axis.text.x   = element_text(size=14), axis.title.y  = element_text(size=14),
                axis.title.x  = element_text(size=14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill=NA, size=3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust=0.5))
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
      } else {
        g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          #geom_polygon(data=mapa, aes(x=long + 360,y=lat, group =group),fill = "black", color = "black") +
          
          geom_tile(data=data2,aes(x = lon + 360, y= lat,fill = temp, colour = temp),alpha=0.9, na.rm = F) +
          
          #geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          geom_point(data=puntos2, aes(x = lon + 360, y = lat), col="black",size=0.5, alpha = alpha)+
          
          geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
          
          scale_fill_stepsn(limits = escala, name = label_escala,colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          #guides(fill = guide_legend(reverse = TRUE))+
          
          geom_polygon(data=mapa, aes(x=long + 360, y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85 + 360, -33 + 360)) +
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


#### PP_TEST ####
pp_test = function(ss_temp, ss_pp){
  
  anios = seq(from = 1982, to = 2010, by = 1)
  
  mask=as.matrix(read.table("mascara.txt"))
  mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
  for(i in 1:4){
    mask_arr[,,i] = mask
  }
  
  
  ensemble_total = readline("Todos los modelos?(si, no): ")
  
  if(ensemble_total == "si"){
    k = c(10, 10, 12, 12, 4, 28, 10, 20) #miembros de cada modelo
    t = 29 #anios
    m = 8 #modelos
    
    f = qf(0.95, t-1, t*106)
    pp_f = 1/(1+m*sum(k)*(f-1))
    
    # hodson - sutton. segun zwiers PP  # esto lo hago solo para testear y crear una mascara para el mapa
    
    aux_pp_temp = (ss_temp[[1]]/ss_temp[[4]])*((t*105)/(t-1))
    pp_temp = 1/(1+((m*sum(k))/(aux_pp_temp-1)))
    
    pp_temp_sig = pp_temp
    
    # testear y mascara
    pp_temp[which(pp_temp<pp_f)] = NA # saco los que no son significativos
    
    pp_temp_sig[which(pp_temp_sig<pp_f)] = NA
    pp_temp_sig[which(!is.na(pp_temp))] = 1 
    
    pp_temp = pp_temp*mask_arr #agrego mascara de continente a todas las estaciones
    pp_temp_sig = pp_temp_sig*mask_arr 
    
    #ideam pp
    
    aux_pp_pp = (ss_pp[[1]]/ss_pp[[4]])*((t*105)/(t-1))
    pp_pp = 1/(1+((m*sum(k))/(aux_pp_pp-1)))
    
    pp_pp_sig = pp_pp
    
    pp_pp[which(pp_pp<pp_f)] = NA
    
    pp_pp_sig[which(pp_pp_sig<pp_f)] = NA
    pp_pp_sig[which(!is.na(pp_pp_sig))] = 1
    
    pp_pp = pp_pp*mask_arr
    pp_pp_sig = pp_pp_sig*mask_arr
    
    pp = list()
    pp[[1]] = pp_temp
    pp[[2]] = pp_temp_sig
    pp[[3]] = pp_pp
    pp[[4]] = pp_pp_sig
    
    return(pp)
  } else {
    
    todos = as.numeric(readline("Sacar todos los modelos (1) o elegir uno (2): "))
    if(todos == 2){
      
      k = c(10, 10, 12, 12, 4, 28, 10, 20) #miembros de cada modelo
      nomodel = as.numeric(readline("Modelo a eliminar del ensamble. (1)COLA-CCSM4, (2)GFDL-CM2p1, (3)GFDL-FLOR-A06, (4)GFDL-FLOR-B01, (5)NASA-GEOS5, (6)NCEP-CFSv2, (7)CMC-CanCM4i, (8)CMC-CanSIPSv2: " ))
      
      k[nomodel] = NA
      t = 29 #anios
      m = 7 #modelos
      
      f = qf(0.95, t-1, t*106)
      pp_f = 1/(1+m*sum(k, na.rm = T)*(f-1))
      
      # hodson - sutton. segun zwiers PP  # esto lo hago solo para testear y crear una mascara para el mapa
      
      aux_pp_temp = (ss_temp[[1]]/ss_temp[[4]])*((t*105)/(t-1))
      pp_temp = 1/(1+((m*sum(k, na.rm = T))/(aux_pp_temp-1)))
      
      pp_temp_sig = pp_temp
      
      # testear y mascara
      pp_temp[which(pp_temp<pp_f)] = NA # saco los que no son significativos
      
      pp_temp_sig[which(pp_temp_sig<pp_f)] = NA
      pp_temp_sig[which(!is.na(pp_temp))] = 1 
      
      pp_temp = pp_temp*mask_arr #agrego mascara de continente a todas las estaciones
      pp_temp_sig = pp_temp_sig*mask_arr 
      
      #ideam pp
      
      aux_pp_pp = (ss_pp[[1]]/ss_pp[[4]])*((t*105)/(t-1))
      pp_pp = 1/(1+((m*sum(k, na.rm = T))/(aux_pp_pp-1)))
      
      pp_pp_sig = pp_pp
      
      pp_pp[which(pp_pp<pp_f)] = NA
      
      pp_pp_sig[which(pp_pp_sig<pp_f)] = NA
      pp_pp_sig[which(!is.na(pp_pp_sig))] = 1
      
      pp_pp = pp_pp*mask_arr
      pp_pp_sig = pp_pp_sig*mask_arr
      
      pp = list()
      pp[[1]] = pp_temp
      pp[[2]] = pp_temp_sig
      pp[[3]] = pp_pp
      pp[[4]] = pp_pp_sig
      
      return(pp)
    } else {
      
      t = 29 #anios  
      m = 7 #modelos
      
      ss_v = list()
      
      for(l in 1:8){
        
        k = c(10, 10, 12, 12, 4, 28, 10, 20) # es necesario q se defina para cada j
        k[l] = NA # eliminando el correspondiente al modelo omitido
        k = k[!is.na(k)] # lenght = 7
        
        f = qf(0.95, t-1, t*sum(k)-1)
        pp_f = 1/(1+m*sum(k)*(f-1))
        
        aux_pp_temp = (ss_temp[[l]][[1]]/ss_temp[[l]][[4]])*((t*sum(k)-1)/(t-1))
        pp_temp = 1/(1+((m*sum(k))/(aux_pp_temp-1)))   
        
        pp_temp_sig = pp_temp
        
        # testear y mascara
        pp_temp[which(pp_temp<pp_f)] = NA # saco los que no son significativos
        
        pp_temp_sig[which(pp_temp_sig<pp_f)] = NA
        pp_temp_sig[which(!is.na(pp_temp))] = 1 
        
        pp_temp = pp_temp*mask_arr 
        pp_temp_sig = pp_temp_sig*mask_arr 
        
        
        #ideam pp
        
        aux_pp_pp = (ss_pp[[l]][[1]]/ss_pp[[l]][[4]])*((t*sum(k))/(t-1))
        pp_pp = 1/(1+((m*sum(k))/(aux_pp_pp-1)))
        
        pp_pp_sig = pp_pp
        
        pp_pp[which(pp_pp<pp_f)] = NA
        
        pp_pp_sig[which(pp_pp_sig<pp_f)] = NA
        pp_pp_sig[which(!is.na(pp_pp_sig))] = 1
        
        pp_pp = pp_pp*mask_arr
        pp_pp_sig = pp_pp_sig*mask_arr
        
        pp = list()
        pp[[1]] = pp_temp
        pp[[2]] = pp_temp_sig
        pp[[3]] = pp_pp
        pp[[4]] = pp_pp_sig
        
        
        ss_v[[l]] = pp
        
        rm(pp)
        
      }
      
      return(ss_v)
      
      # hodson - sutton. segun zwiers PP  # esto lo hago solo para testear y crear una mascara para el mapa
    }
  }
}  
#### MAPA_SIG2.0 ####
# la funcion es igual mapa, con opciones de puntos sobre el sombreados para marcar niveles significativo.
# NO reemplaza a mapa_sig.

mapa_sig2 = function(lista, titulo, nombre_fig, escala, label_escala, resta, brewer, revert, niveles, contour, lon, lat
                     , escala_dis, breaks_c_f, alpha, size, color, v, topo2 ,salida){
  
  
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
  
  #topo = metR::GetTopography(-85 + 359.5, -29 + 359.5, 15.5,  -60.5, resolution = 1/res) # mapa topografia
  #topo2 = topo #
  #topo2[which(topo2$h<altura)]=NA
  
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
    
    
    mapa <- map_data("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
                                          "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua", "falkland islands",
                                          "Martinique"), 
                     colour = "black")
    
    if(revert == "si"){
      if(contour == "si"){
        g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data = data, aes(x = lon + 360, y = lat, fill = temp), alpha=0.9, na.rm = T) +
          
          geom_contour_fill(data = data, aes(x = lon + 360, y = lat, z = temp),alpha = 1, na.fill = -10000, breaks = breaks_c_f) +
          
          stat_subset(data=data, aes(x = lon + 360, y = lat, z = temp, subset = temp <= v), shape = 20, size = size, color = color, alpha = alpha, geom = "point")+
          
          geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          #guides(fill = guide_legend(reverse = TRUE)) +
          
          geom_polygon(data=mapa, aes(x=long + 360, y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85 + 360, -33 + 360))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg", sep = ""), plot = g, width = 15, height = 15  , units = "cm")
      } else {
        g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon + 360, y = lat,fill = temp),alpha=0.9, na.rm = T) +
          
          stat_subset(data=data, aes(x = lon + 360, y = lat, z = temp, subset = temp <= v), shape = 20, size = size, color = color, alpha = alpha, geom = "point")+
          
          #geom_contour_fill(data=data, aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000, breaks = breaks_c_f)
          
          geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          #guides(fill = guide_legend(reverse = TRUE)) +
          
          geom_polygon(data=mapa, aes(x=long + 360, y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85 + 360, -33 + 360))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
        
      }
      
    } else {
      if(contour == "si"){
        
        g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data = data,aes(x = lon + 360, y = lat, fill = temp), alpha = 0.7, na.rm = T) +
          
          geom_contour_fill(data = data, aes(x = lon + 360, y = lat, z = temp), alpha = 1, na.fill = -10000, breaks = breaks_c_f) +
          geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
          
          
          stat_subset(data=data, aes(x = lon + 360, y = lat, z = temp, subset = temp <= v), shape = 20, size = size, color = color, alpha = alpha, geom = "point")+
          #geom_contour(data = data, aes(x = lon, y = lat, z = temp), color = "blue", size = 0.666, breaks = rc )+
          
         
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          #guides(fill = guide_legend(reverse = TRUE)) +
          
          geom_polygon(data=mapa, aes(x=long + 360, y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85 + 360, -33 + 360))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
      } else {
        g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data = data,aes(x = lon + 360, y = lat,fill = temp),alpha = 0.7, na.rm = T) +
          
          #geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
      
          stat_subset(data=data, aes(x = lon + 360, y = lat, z = temp, subset = temp <= v), shape = 20, size = size, color = color, alpha = alpha, geom = "point")+
          
          geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          #guides(fill = guide_legend(reverse = TRUE))+
          
          geom_polygon(data=mapa, aes(x=long + 360, y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " - " , est[i], sep = ""))+
          scale_x_continuous(limits = c(-85 + 360, -33 + 360))+
          theme(axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14), axis.title.y = element_text(size = 14),
                axis.title.x  = element_text( size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        ggsave(paste(ruta, salida, nombre_fig, "_", est[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
        
      }
      
    }
  }
  
  
}

#### MAKS_TOPO ####
mask_topo = function(altura){
  
  # la topografia se baja con metR en forma de data.frame. para tener una mascara para los calculos -->  data.frame -> array
  
  library("metR")
  
  topo <- metR::GetTopography(-85 + 359.5, -29 + 359.5, 15.5,  -60.5, 
                              resolution = 1) # 1x1 igual q la grilla de datos
  
  topo$h[which(topo$h<altura)]=NA # altura para a partir de la cual se borran los datos

  value_topo = topo$h  
  lon_topo = topo$lon[1:56]
  lat_topo = seq(range(topo$lat)[1], range(arg$lat)[2], by = 1) #esto solo con res = 1
  
  mask_topo = array( data = NA, dim = c(56,76))
  for(j in 0:75){
    mask_topo[,76-j] = value_topo[(1+56*j):(56+56*j)]  # latitud invertida en el data frame 
  }
  
  #mascara
  
  mask_topo[which(is.na(mask_topo))] = 1
  mask_topo[which(mask_topo != 1)] = NA
  
  return(mask_topo)
  
}


#### FIG 10 ####
fig10 = function(prom_cajas, prom_ensamble, variable, base_datos){
  
  if(variable == "temp" ){
    
    base_datos = NA
    
    data_t = matrix(data = NA, nrow = 5*8, ncol = 6)
    data_ensamble_t = matrix(data = NA, nrow = 5, ncol = 6)
    data_t[,6] = seq(1:8)
    data_ensamble_t[,6] = 1
    data_ensamble_t[,1] = seq(1,5)
    for(i in 0:3){
      
      data_t[(0 + 1+i*8):(8 + i*8),1] = cajas_num[i+1]
      if((i + 1) == 4){ 
        data_t[33:40,1] = 5
      } else {
        next
      }
      
    }
    
    for(s in 1:4){
      
      for(i in 1:5){
        
        data_t[which(data_t[,1] == i ), (s + 1)] = prom_cajas[[i]][s,]
        data_ensamble_t[which(data_ensamble_t[,1] == i), (s +1)] = prom_ensamble[[i]][s]
        
      }
      
    }
    
    
    data_ensamble_t[,1] = seq(0.87, 4.87)
    data_t[,1] = seq(0.9, 4.9)
    
    data_t = as.data.frame(data_t)
    data_ensamble_t = as.data.frame(data_ensamble_t)
    colnames(data_t) = c("cajas", "MAM", "JJA", "SON", "DJF", "value")
    colnames(data_ensamble_t) = c("cajas", "MAM", "JJA", "SON", "DJF", "value")
    
    V = list()
    V[[1]] = data_t
    V[[2]] = data_ensamble_t
    
    return(V)
    
    
  } else {
    
    data_pp = matrix(data = NA, nrow = 5*8, ncol = 6)
    data_ensamble_pp = matrix(data = NA, nrow = 5, ncol = 6)
    data_pp[,6] = seq(1:8)
    data_ensamble_pp[,6] = 1
    data_ensamble_pp[,1] = seq(1,5)
    for(i in 0:3){
      
      data_pp[(0 + 1+i*8):(8 + i*8),1] = cajas_num[i+1]
      if((i + 1) == 4){ 
        data_pp[33:40,1] = 5
      } else {
        next
      }
      
    }
    
    for(s in 1:4){
      
      for(i in 1:5){
        
        data_pp[which(data_pp[,1] == i ), (s + 1)] = prom_cajas[[i]][,,base_datos][s,]
        data_ensamble_pp[which(data_ensamble_pp[,1] == i), (s +1)] = prom_ensamble[[i]][s,base_datos]
        
      }
      
    }
    
    
    if(base_datos == 1){
      data_ensamble_pp[,1] = seq(1.07,5.07)
      data_pp[,1] = seq(1.1, 5.1)
    } else if( base_datos == 2){
      data_ensamble_pp[,1] = seq(1.22,5.22)
      data_pp[,1] = seq(1.25, 5.25)
    } else {
      data_ensamble_pp[,1] = seq(1.37, 5.37)
      data_pp[,1] = seq(1.4, 5.4)
    }
    
    data_pp = as.data.frame(data_pp)
    data_ensamble_pp = as.data.frame(data_ensamble_pp)
    colnames(data_pp) = c("cajas", "MAM", "JJA", "SON", "DJF", "value")
    colnames(data_ensamble_pp) = c("cajas", "MAM", "JJA", "SON", "DJF", "value")
    
    V = list()
    V[[1]] = data_pp
    V[[2]] = data_ensamble_pp
    
    return(V)
    
  }
  
  
}
#### FIG 8 ####

fig8 = function(v1, v2, lon, lat, titulo, color, y.name, x.name, nombre.fig, salida){
  
  ruta = getwd()
  
  est=c("MAM", "JJA", "SON", "DJF")
  
  for(i in 1:4){
    
    data = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 4)
    
    data[,1] = array(data = v1[,,i]*mask, dim = length(lon2)*length(lat2))
    data[,2] = array(data = v2[,,i]*mask, dim = length(lon2)*length(lat2))

    
    data<-as.data.frame(data)
    
    colnames(data)<-c("Y", "X")
    
    
  g  = ggplot(data = data, mapping = aes(x = X, y = Y)) + theme_minimal()+
      geom_point(color = color)+
      
      scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.20), name = x.name)+
      scale_y_continuous(limits = c(0,1),breaks = seq(0,1,by = 0.20), name = y.name)+
      ggtitle(paste(titulo, " - " , est[i], sep = ""))+
      theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
            axis.title.x  = element_text(size = 14), panel.grid.major = element_line(colour = 'grey44', size = 0.5, linetype = "dashed")
            , panel.grid.minor = element_line(colour = 'grey44', size = 0.25, linetype = "dashed"),
            panel.border = element_rect(colour = "black", fill = NA, size = 3),
            panel.ontop = TRUE,
            plot.title = element_text(hjust = 0.5))
    
  ggsave(paste(ruta, salida, nombre.fig, "_", est[i], ".jpg", sep = ""), plot = g, width = 18, height = 15  , units = "cm")
  }
  
}

#### corr ####
corr = function(mod, obs, lon, lat, cf){
  corr =  array(data = NA, dim = c(lon, lat,2))
  a = 1- cf
  for(i in 1:lon){
    for(j in 1:lat){
      
      # si cada serie [i,j,] no tiene una cantidad de al menos 3 valores no NA, cor.test da error y corta el ciclo
      if(is.na(obs[i,j,1])){
        
        corr[i,j,2] = NA
        
      } else {
        
        l = cor.test(mod, obs[i,j,], method = "pearson", conf.level = cf, alternative = "two.sided")
        
        corr[i,j,1] = l$estimate
        
        # esto con lo de arriba deberia no hacer falta, pero en caso de que dos valores sean iguales el resultado de p.value es NA
        # seria muy raro que pase.
        if(is.na(l$p.value)){
          
          corr[i,j,2] = NA
          
        } else if(l$p.value < a){
          
          corr[i,j,2] = 1
        }
        
      }
      
    }
  }
  
  return(corr)
  
}

#### MAPA_TOPO3 ####
# a ver si me puedo quedar con una sola q sirva para todo...
mapa_topo3 = function(variable, variable.sig = NULL, variable.cont = NULL, u = NULL, v = NULL, lon, lat, contour.fill = T, contour = F, viento = F
                      , colorbar = "Spectral", niveles = 9, revert = F, escala = NULL, resta = 0, resta.vsig = 0, resta.vcont = 0, nivel.vcont = NULL, color.vsig = "black", color.vcont = "red", alpha.vsig, sig = F
                      , titulo = NULL, label.escala = "value", x.label = "x", y.label = "y", fill.mapa = F, colorbar.pos = "right"
                      , mapa = NULL, altura.topo = 0, r = 1, na.fill = NULL, nombre.fig = "fig", width = 25, height = 20, salida = NULL){
  
  library(maps)
  library(ncdf4)
  require(fields)
  require(mapdata)
  library(ggplot2)
  library(RColorBrewer)
  library(mapproj)
  library(metR)
  
  ruta = getwd()
  
  
  limites = c(min(escala), max(escala))
  
  if(mapa == "asia"){
    
    #load("RDatas/topo_india.RData")
    #topo2 = topo_india
    #rm(topo_india)
    
    #topo2[which(topo2$h<altura.topo)]=NA
    
    map <- map_data("world2", region = c("India", "Sri Lanka", "Bangladesh", "Nepal", "Bhutan", "Pakistan"
                                         ,"Oman", "Yemen", "Somalia", "Eriopia", "Birmania"
                                         , "Malasya", "United Arab Emirates", "Singapur", "Myanmar", "Iran", 
                                         "Turkmenistan", "Afghanistan", "Tajikistan", "Uzbekistan", "Kyrgyzstan", "China", "Mongolia", 
                                         "Bangladesh", "North Korea", "South Korea",  "Taiwan", "Laos", "Thailand", "Vietnam", "Cambodia", 
                                         "Malasya", "Indonesia", "Philippines"), colour = "black")
    
    breaks.lon = seq(40, 140, by = 10)
    breaks.lat = seq(-10, 55, by = 10)
    
  } else if(mapa == "mundo"){
    
    map = map_data("world2", colour = "black")
    
    breaks.lon = seq(0, 360, by = 30)
    breaks.lat = seq(-90, 90, by = 20)
    
    
  } else if(mapa == "sa") {
    
    load("topo.RData")
    topo2 = topo
    rm(topo)
    
    topo2[which(topo2$h<altura.topo)]=NA
    
    map <- map_data("world2", region = c("Brazil", "French Guiana", "Suriname", "Colombia", "Venezuela","Argentina", "Chile", "Uruguay",
                                         "Bolivia", "Ecuador", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua",
                                         "Martinique"), colour = "black")
    
    
    breaks.lon = seq(250, 350, by = 10)
    breaks.lat = seq(-60, 20, by = 10)
    
  }
  
  g = list()
  num = seq(1, r, by = 1)
  
  for(i in 1:r){
    
    data = expand.grid(lon = lon, lat = lat)
    
    data[,3] = array(variable[,,i], dim = length(lon)*length(lat)) - resta
    
    colnames(data)<-c("lon", "lat", "var")
    
    if(mapa == "SA"){
      
      g = ggplot(topo2, aes(lon, lat)) + theme_minimal() +
        xlab(x.label) + ylab(y.label) + 
        theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
        geom_tile(data = data, aes(x = lon, y = lat, fill = var), alpha = 0.8, na.rm = T) 
      
      
    } else {
      
      g = ggplot() + theme_minimal() +
        xlab(x.label) + ylab(y.label) + 
        theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
        geom_tile(data = data, aes(x = lon, y = lat, fill = var), alpha = 0.8, na.rm = T) 
      
    }
    
    if(colorbar.pos == "right"){
      
      if(contour.fill == T & revert == T ){
        
        g = g +  geom_contour_fill(data = data, aes(x = lon, y = lat, z = var),alpha = 1, na.fill = na.fill , breaks = escala) +
          scale_fill_stepsn(limits = limites, name = label.escala, colours = rev(brewer.pal(n=niveles , colorbar)), na.value = "white", breaks = escala,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T))
      } else if(contour.fill == T & revert == F ){
        g = g +  geom_contour_fill(data = data, aes(x = lon, y = lat, z = var),alpha = 1, na.fill = na.fill , breaks = escala) +
          scale_fill_stepsn(limits = limites, name = label.escala, colours = brewer.pal(n=niveles , colorbar), na.value = "white", breaks = escala,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) 
      } else if(contour.fill == F & revert == T){
        g = g + scale_fill_stepsn(limits = limites, name = label.escala, colours = rev(brewer.pal(n=niveles , colorbar)), na.value = "white", breaks = escala,
                                  guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) 
      } else {
        g + scale_fill_stepsn(limits = limites, name = label.escala, colours = brewer.pal(n=niveles , colorbar), na.value = "white", breaks = escala,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) 
      }
      
      
    } else {
      
      if(contour.fill == T & revert == T ){
        
        g = g +  geom_contour_fill(data = data, aes(x = lon, y = lat, z = var),alpha = 1, na.fill = na.fill , breaks = escala) +
          scale_fill_stepsn(limits = limites, name = label.escala, colours = rev(brewer.pal(n=niveles , colorbar)), na.value = "white", breaks = escala,
                            guide = guide_colorbar(barwidth = 35, barheight = 0.8, title.position = "left", title.hjust = 0.5, raster = F, ticks = T, direction = "horizontal")) 
      } else if(contour.fill == T & revert == F ){
        g = g +  geom_contour_fill(data = data, aes(x = lon, y = lat, z = var),alpha = 1, na.fill = na.fill , breaks = escala) +
          scale_fill_stepsn(limits = limites, name = label.escala, colours = brewer.pal(n=niveles , colorbar), na.value = "white", breaks = escala,
                            guide = guide_colorbar(barwidth = 35, barheight = 0.8, title.position = "left", title.hjust = 0.5, raster = F, ticks = T, direction = "horizontal")) 
      } else if(contour.fill == F & revert == T){
        g = g + scale_fill_stepsn(limits = limites, name = label.escala, colours = rev(brewer.pal(n=niveles , colorbar)), na.value = "white", breaks = escala,
                                  guide = guide_colorbar(barwidth = 35, barheight = 0.8, title.position = "left", title.hjust = 0.5, raster = F, ticks = T, direction = "horizontal")) 
      } else {
        g + scale_fill_stepsn(limits = limites, name = label.escala, colours = brewer.pal(n=niveles , colorbar), na.value = "white", breaks = escala,
                              guide = guide_colorbar(barwidth = 35, barheight = 0.8, title.position = "left", title.hjust = 0.5, raster = F, ticks = T, direction = "horizontal")) 
      }
      
      
    }
    
    if(sig == T){
      
      data2 = expand.grid(lon = lon, lat = lat)
      data2[,3] = array(variable.sig[,,i], dim = length(lon)*length(lat)) - resta.vsig
      colnames(data2)<-c("lon", "lat", "var")
      
      g = g +  geom_tile(data = subset(data2, is.na(var)),aes(x = lon, y = lat, fill = is.na(var)), alpha = alpha.vsig, fill = color.vsig, show.legend = F)

      
    }
    
    
    if(mapa == "SA"){
      
      g = g + geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black", show.legend = F) 
      
    }
    
    if(viento == T){
      
      u = array(u[,,i], dim = length(lon)*length(lat))
      v = array(v[,,i], dim = length(lon)*length(lat))
      
      data = cbind(data, u)
      data = cbind(data, v)
      
      colnames(data)<-c("lon", "lat", "temp", "u", "v")
      
      g = g +  geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black"
                          , skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)
    } 
    
    
    if(fill.mapa == T){
      g = g + geom_polygon(data = map, aes(x = long ,y = lat, group = group),fill = "black", color = "black", alpha = 0.3) 
    } else {
      g = g + geom_polygon(data = map, aes(x = long ,y = lat, group = group),fill = NA, color = "black") 
    }
    
    
    
    if(contour == T){
      
      data2 = expand.grid(lon = lon, lat = lat)
      data2[,3] = array(variable.cont[,,i], dim = length(lon)*length(lat)) - resta.vcont
      colnames(data2)<-c("lon", "lat", "cont")
      
      g = g +  stat_contour(data = data2, aes(x = lon, y = lat, z = cont), color = color.vcont, size = 1, breaks = nivel.vcont )
      
    }
    
    g = g + ggtitle(titulo) +
      scale_x_longitude(breaks = breaks.lon, name = x.label)+
      scale_y_latitude(breaks = breaks.lat, name = y.label)+
      theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
            axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill = NA, size = 3),
            panel.ontop = TRUE,
            plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0, color = "black") 
    if(colorbar.pos == "bottom"){
      g = g + theme(legend.position = "bottom")
    }
      
    
    ggsave(paste(ruta, salida, nombre.fig, num[i], ".jpg", sep = ""), plot = g, width = width, height = height, units = "cm")
    
  }
}     
