test_cos = function(SS){
  
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
  
  k = c(10, 10, 12, 12, 4, 28, 10, 20) #miembros de cada modelo
  t = 29 #anios
  m = 8 #modelos
  
  mask=as.matrix(read.table("mascara.txt"))
  mask_arr = array(NA, dim = c(length(lon2), length(lat2), 4))
  for(i in 1:4){
    mask_arr[,,i] = mask
  }
  # 
  
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
  return(sig)
}
