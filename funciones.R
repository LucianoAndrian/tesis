#### Funciones ####

### MAPA ####
mapa = function(lista, titulo, nombre_fig, escala, label_escala, resta, brewer, revert, niveles, contour, lon, lat, escala_dis, salida){
  
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
        g = ggplot() + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon, y= lat,fill = temp),alpha=0.9, na.rm = T)+
          
          geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
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
          
          scale_fill_gradientn(limits=escala,name=label_escala,colours=rev(brewer.pal(n=niveles,brewer)),na.value = "white", guide = "legend",breaks = escala_dis) +
          
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
        
      }
      
    } else {
      if(contour == "si"){
        
        g = ggplot() + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon, y= lat,fill = temp),alpha=0.9, na.rm = T)+
          
          geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          scale_fill_gradientn(limits=escala,name=label_escala,colours=brewer.pal(n=niveles,brewer),na.value = "white", guide = "legend",breaks = escala_dis)+
          
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
  
  # SD
  
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
  
  # SD
  
  PP2 =  array(NA, dim = c(length(lon2), length(lat2), length(anios),4))
  for(i in 1:4){
    PP2[,,,i] =  apply(PP[,,,,,i], c(1,2,5), mean, na.rm = TRUE)
  }
  
  sd_pp = array(NA, dim = c(length(lon2), length(lat2),4)) 
  for(i in 1:4){
    sd_pp[,,i] = apply(PP2[,,,i], c(1,2), sd, na.rm = TRUE)*mask
  }
  
  T_PP = list()
  T_PP[[1]] = T1
  T_PP[[2]] = sd_t
  T_PP[[3]] = PP1
  T_PP[[4]] = sd_pp
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
    
    mask = as.matrix(read.table("mascara.txt"))
    
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
  mask = as.matrix(read.table("mascara.txt"))                  # esto antes andaba sin as.matrix porque ya estaba definida mask arr en el codigo modelos.
  
  mask_arr = array(NA, dim = c(length(lon), length(lat), 4))
  for(i in 1:4){
    mask_arr[,,i] = mask
  }
  
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