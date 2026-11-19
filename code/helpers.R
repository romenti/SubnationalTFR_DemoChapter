
#### Read Population Data france ####

read_year = function(y){
  
  x = read_excel(
    file,
    sheet = as.character(y),
    skip = 5,
    col_names = FALSE
  )
  
  names(x) = c(
    "prov","prov_name",
    paste0("Both_", age_groups),
    paste0("Male_", age_groups),
    paste0("Female_", age_groups)
  )
  
  x %>%
    filter(!is.na(prov)) %>%
    mutate(year = y) %>%
    pivot_longer(
      -c(prov, prov_name, year),
      names_to = c("sex","age_group"),
      names_sep = "_",
      values_to = "pop"
    )
}

#### Uniform boundaries Pakistan ####

pk_adm2_crosswalk = c(
  
  # Spelling and naming differences
  "SHEERANI"                    = "SHERANI",
  "D I KHAN"                    = "DERA ISMAIL KHAN",
  "LEIAH"                       = "LAYYAH",
  "RAHIMYAR KHAN"               = "RAHIM YAR KHAN",
  "TORGHAR"                     = "TOR GHAR",
  "MALAKAND PROTECTED AREA"     = "MALAKAND",
  # Karachi districts
  "CENTRAL KARACHI"             = "KARACHI CENTRAL",
  "EAST KARACHI"                = "KARACHI EAST",
  "SOUTH KARACHI"               = "KARACHI SOUTH",
  "WEST KARACHI"                = "KARACHI WEST",
  "KORANGI"                     = "KARACHI KORANGI",
  "KORANGI KARACHI"             = "KARACHI KORANGI",
  "MALIR"                       = "KARACHI MALIR",
  "MALIR KARACHI"               = "KARACHI MALIR",
  
  # Other Sindh spelling differences
  "KAMBAR SHAHDAD KOT"          = "KAMBAR SHAHDADKOT",
  "UMER KOT"                    = "UMERKOT",
  "SHAHEED BENAZIR ABAD"        = "SHAHEED BENAZIRABAD",
  
  # Gilgit-Baltistan
  "BALTISTAN"                   = "SKARDU",
  "RONDU"                       = "SKARDU",
  "ROUNDU"                      = "SKARDU",
  "DAREL"                       = "DIAMIR",
  "TANGIR"                      = "DIAMIR",
  "GUPIS-YASIN"                 = "GHIZER",
  "GUPIS YASIN"                 = "GHIZER",
  
  # Khyber Pakhtunkhwa
  "CHITRAL LOWER"               = "CHITRAL",
  "CHITRAL UPPER"               = "CHITRAL",
  "KOHISTAN LOWER"              = "KOHISTAN",
  "KOHISTAN UPPER"              = "KOHISTAN",
  "KOLAI PALAS KOHISTAN"        = "KOHISTAN",
  
  # Newer Balochistan districts
  "SHAHEED SIKANDARABAD"        = "KALAT",
  "DUKI"                        = "LORALAI",
  "CHAMAN"                      = "KILLA ABDULLAH",
  
  # Lehri cannot be assigned exactly to only Sibi or Kachhi
  "SIBI"                        = "SIBI-KACHHI",
  "KACHHI"                      = "SIBI-KACHHI",
  "LEHRI"                       = "SIBI-KACHHI"
)

standardize_pk_adm2 = function(x) {
  
  x = x %>%
    as.character() %>%
    str_to_upper() %>%
    str_squish() %>%
    
    # Standardize different types of hyphens
    str_replace_all("[‐-–—]", "-") %>%
    
    # Remove administrative suffixes
    str_remove("\\s+(DISTRICT|AGENCY)$") %>%
    
    # D. I. KHAN becomes D I KHAN
    str_replace_all("\\.", "") %>%
    str_squish()
  
  # Apply crosswalk
  position = match(x, names(pk_adm2_crosswalk))
  matched = !is.na(position)
  
  x[matched] = unname(
    pk_adm2_crosswalk[position[matched]]
  )
  
  x
}



#### Mortality Functions ####


derive_lx_values = function(nmx, ages){
  a0 = 0.07 + 1.7*nmx[1] 
  a1 = 1.5
  a_inf = 1/nmx[length(nmx)]
  nax = c(a0, a1, ((ages - lag(ages))/2)[4:(length(ages))], a_inf) 
  ns = c(1,4, (ages - lag(ages))[4:(length(ages))], Inf) #interval length
  nqx = ns*nmx / (1 + (ns - nax)*nmx)
  nqx[length(nqx)] = 1 #last age prob dying =1
  l0 = 1
  lx = c(l0, rep(0, length(nqx)-1))
  for (i in 2:length(nqx)){
    lx[i]= lx[i-1]*(1-nqx[i-1])
  }
  
  return(lx)
}


derive_nLx_values = function (nmx, ages){
  
  lx = derive_lx_values(nmx, ages)
  a0 = 0.07 + 1.7*nmx[1] 
  a1 = 1.5
  a_inf = 1/nmx[length(nmx)]
  nax = c(a0, a1, ((ages - lag(ages))/2)[4:(length(ages))], a_inf) 
  ns = c(1,4, (ages - lag(ages))[4:(length(ages))], Inf) #interval length
  
  # 
  ndx = rep(NA, length(lx))
  for(i in 1:(length(ndx)-1)){
    ndx[i] = lx[i] - lx[i+1]
  }
  ndx[length(ndx)] = lx[length(ndx)]
  
  nLx = rep(NA, length(nmx))
  for(i in 1:(length(nLx)-1)){
    nLx[i] = ns[i]*lx[i+1] + nax[i]*ndx[i]
  }
  
  nLx[length(nLx)] =  nax[length(nax)]*ndx[length(ndx)]
  
  return(nLx)
}


mort_par = function(lo_q,up_q){
  LearnBayes::beta.select( list(x=lo_q, p=.05), list(x=up_q, p=.95))
}

#### Build Matrix for a Random Walk Process of Order 1 ####

build_RW1 = function(Ti) {
  Q = matrix(0, nrow = Ti, ncol = Ti)
  
  diag(Q) = c(1, rep(2, Ti - 2), 1)
  
  for (i in 1:(Ti - 1)) {
    Q[i, i + 1] = -1
    Q[i + 1, i] = -1
  }
  
  return(Q)
}






#### Model Input Data Functions ####

data_input_men_france = function(data,
                                 fra_geo,
                                 m,
                                 X,
                                 years=2000:2020){
  
  
  nb_france = poly2nb(fra_geo, queen = FALSE)
  adj_matrix_france = nb2mat(nb_france, style = "B", zero.policy = TRUE)
  colnames(adj_matrix_france) = unique(data$prov)
  rownames(adj_matrix_france) = unique(data$prov)
  spatial_object_france = as.carAdjacency(adj_matrix_france)
  
  years = 2000:2020
  
  data = filter(data,year %in% years) 
  
  year_i = data %>%
    ungroup() %>%
    #arrange(year) %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    arrange(prov) %>%
    dplyr::select(prov) %>%
    pull()
  
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$prov)))
  
  W = as.matrix(adj_matrix_france)
  W = ifelse(W > 0, 1, 0)
  diag(W) = 0
  
  
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$prov))
  time = (years-mean(years))/sd(years)
  
  Q_time = build_RW1(Ti)      
  Q_ST = kronecker(Q_time, W)
  
  nb_ST   = mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  = sapply(nb_ST, length)
  adj_ST  = unlist(nb_ST)
  weights_ST = sapply(seq_along(nb_ST), function(i){
    j_list = nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST = unlist(weights_ST)
  L_ST = length(adj_ST)
  
  
  W.scale = -W
  diag(W.scale) = abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] = 1
  for(i in 1:(Ti-1)) Wt[i+1,i] = 1
  Wnb = mat2listw(Wt)
  Wnb = nb2WB(nb = Wnb$neighbours)
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                n  = length(spatial_object_france$num),
                L = length(spatial_object_france$weights),
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object_france$adj, 
                num = spatial_object_france$num,
                weights= spatial_object_france$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda = 4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  nimble_input = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,55,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,55,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,55,5))]),
    TFR_nat = data$tfr,
    m = m,
    X = X)
  
  
  
  
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*12,4.7,5),N,12),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,
              consts=consts,
              init=init))
  
  
  
}


data_input_women_france = function(data,
                                   fra_geo,
                                   m,
                                   X,
                                   years=2000:2020){
  
  
  nb_france = poly2nb(fra_geo, queen = FALSE)
  adj_matrix_france = nb2mat(nb_france, style = "B", zero.policy = TRUE)
  colnames(adj_matrix_france) = unique(data$prov)
  rownames(adj_matrix_france) = unique(data$prov)
  spatial_object_france = as.carAdjacency(adj_matrix_france)
  
  
  data = filter(data,year %in% years) 
  
  year_i = data %>%
    ungroup() %>%
    #arrange(year) %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    arrange(prov) %>%
    dplyr::select(prov) %>%
    pull()
  
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$prov)))
  
  W = as.matrix(adj_matrix_france)
  W = ifelse(W > 0, 1, 0)
  diag(W) = 0
  
  
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$prov))
  time = (years-mean(years))/sd(years)
  
  Q_time = build_RW1(Ti)      
  Q_ST = kronecker(Q_time, W)
  
  nb_ST   = mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  = sapply(nb_ST, length)
  adj_ST  = unlist(nb_ST)
  weights_ST = sapply(seq_along(nb_ST), function(i){
    j_list = nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST = unlist(weights_ST)
  L_ST = length(adj_ST)
  
  
  W.scale = -W
  diag(W.scale) = abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] = 1
  for(i in 1:(Ti-1)) Wt[i+1,i] = 1
  Wnb = mat2listw(Wt)
  Wnb = nb2WB(nb = Wnb$neighbours)
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                n  = length(spatial_object_france$num),
                L = length(spatial_object_france$weights),
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object_france$adj, 
                num = spatial_object_france$num,
                weights= spatial_object_france$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda = 4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  nimble_input = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,45,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,45,5))]),
    TFR_nat = data$tfr,
    m = m,
    X = X)
  
  
  
  
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*10,4.7,5),N,10),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,
              consts=consts,
              init=init))
  
  
  
}




data_input_women_spain = function(data,
                                  esp_geo,
                                  m,
                                  X,
                                  years=2000:2020){
  
  
  nb_spain = poly2nb(esp_geo, queen = FALSE)
  adj_matrix_spain = nb2mat(nb_spain, style = "B", zero.policy = TRUE)
  colnames(adj_matrix_spain) = unique(data$prov)
  rownames(adj_matrix_spain) = unique(data$prov)
  spatial_object_spain = as.carAdjacency(adj_matrix_spain)
  
  years = 2000:2020
  
  data = filter(data,year %in% years) 
  
  year_i = data %>%
    ungroup() %>%
    #arrange(year) %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    arrange(prov) %>%
    dplyr::select(prov) %>%
    pull()
  
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$prov)))
  
  W = as.matrix(adj_matrix_spain)
  W = ifelse(W > 0, 1, 0)
  diag(W) = 0
  
  
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$prov))
  time = (years-mean(years))/sd(years)
  
  Q_time = build_RW1(Ti)      
  Q_ST = kronecker(Q_time, W)
  
  nb_ST   = mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  = sapply(nb_ST, length)
  adj_ST  = unlist(nb_ST)
  weights_ST = sapply(seq_along(nb_ST), function(i){
    j_list = nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST = unlist(weights_ST)
  L_ST = length(adj_ST)
  
  
  W.scale = -W
  diag(W.scale) = abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] = 1
  for(i in 1:(Ti-1)) Wt[i+1,i] = 1
  Wnb = mat2listw(Wt)
  Wnb = nb2WB(nb = Wnb$neighbours)
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                n  = length(spatial_object_spain$num),
                L = length(spatial_object_spain$weights),
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object_spain$adj, 
                num = spatial_object_spain$num,
                weights= spatial_object_spain$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda = 4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  nimble_input = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,45,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,45,5))]),
    TFR_nat = data$tfr,
    m = m,
    X = X)
  
  
  
  
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*10,4.7,5),N,10),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,
              consts=consts,
              init=init))
  
  
  
}



data_input_men_spain = function(data,
                                esp_geo,
                                m,
                                X,
                                years=2000:2020){
  

  nb_spain = poly2nb(esp_geo, queen = FALSE)
  adj_matrix_spain = nb2mat(nb_spain, style = "B", zero.policy = TRUE)
  colnames(adj_matrix_spain) = unique(data$prov)
  rownames(adj_matrix_spain) = unique(data$prov)
  spatial_object_spain = as.carAdjacency(adj_matrix_spain)
  
  years = 2000:2020
  
  data = filter(data,year %in% years) 
  
  year_i = data %>%
    ungroup() %>%
    #arrange(year) %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    arrange(prov) %>%
    dplyr::select(prov) %>%
    pull()
  
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$prov)))
  
  W = as.matrix(adj_matrix_spain)
  W = ifelse(W > 0, 1, 0)
  diag(W) = 0
  
  
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$prov))
  time = (years-mean(years))/sd(years)
  
  Q_time = build_RW1(Ti)      
  Q_ST = kronecker(Q_time, W)
  
  nb_ST   = mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  = sapply(nb_ST, length)
  adj_ST  = unlist(nb_ST)
  weights_ST = sapply(seq_along(nb_ST), function(i){
    j_list = nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST = unlist(weights_ST)
  L_ST = length(adj_ST)
  
  
  W.scale = -W
  diag(W.scale) = abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] = 1
  for(i in 1:(Ti-1)) Wt[i+1,i] = 1
  Wnb = mat2listw(Wt)
  Wnb = nb2WB(nb = Wnb$neighbours)
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                n  = length(spatial_object_spain$num),
                L = length(spatial_object_spain$weights),
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object_spain$adj, 
                num = spatial_object_spain$num,
                weights= spatial_object_spain$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda = 4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  nimble_input = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,55,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,55,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,55,5))]),
    TFR_nat = data$tfr,
    m = m,
    X = X)
  
  
  
  
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*12,4.7,5),N,12),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,
              consts=consts,
              init=init))
  
  
  
}




data_input_women_bangladesh = function(data,
                                       bgd_geo,
                                       m,
                                       X,
                                       years=2000:2020){
  
  
  
  nb_bangladesh = poly2nb(bgd_geo, queen = FALSE)
  adj_matrix_bangladesh = nb2mat(nb_bangladesh, style = "B", zero.policy = TRUE)
  
  colnames(adj_matrix_bangladesh) = unique(bgd_geo$adm2_name)
  rownames(adj_matrix_bangladesh) = unique(bgd_geo$adm2_name)
  spatial_object_bangladesh = as.carAdjacency(adj_matrix_bangladesh)
  
  data = filter(data,year %in% years) 
  
  year_i = data %>%
    ungroup() %>%
    #arrange(year) %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    arrange(adm2) %>%
    dplyr::select(adm2) %>%
    pull()
  
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$adm2)))
  
  W = as.matrix(adj_matrix_bangladesh)
  W = ifelse(W > 0, 1, 0)
  diag(W) = 0
  
  
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$adm2))
  time = (years-mean(years))/sd(years)
  
  Q_time = build_RW1(Ti)      
  Q_ST = kronecker(Q_time, W)
  
  nb_ST   = mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  = sapply(nb_ST, length)
  adj_ST  = unlist(nb_ST)
  weights_ST = sapply(seq_along(nb_ST), function(i){
    j_list = nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST = unlist(weights_ST)
  L_ST = length(adj_ST)
  
  
  W.scale = -W
  diag(W.scale) = abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] = 1
  for(i in 1:(Ti-1)) Wt[i+1,i] = 1
  Wnb = mat2listw(Wt)
  Wnb = nb2WB(nb = Wnb$neighbours)
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                n  = length(spatial_object_bangladesh$num),
                L = length(spatial_object_bangladesh$weights),
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object_bangladesh$adj, 
                num = spatial_object_bangladesh$num,
                weights= spatial_object_bangladesh$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda = 4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  nimble_input = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,45,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,45,5))]),
    TFR_nat = data$tfr,
    m = m,
    X = X)
  
  
  
  
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*10,4.7,5),N,10),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,
              consts=consts,
              init=init))
  
  
  
}




data_input_men_bangladesh = function(data,
                                     bgd_geo,
                                     m,
                                     X,
                                     years=2000:2020){
  
  
  
  nb_bangladesh = poly2nb(bgd_geo, queen = FALSE)
  adj_matrix_bangladesh = nb2mat(nb_bangladesh, style = "B", zero.policy = TRUE)
  
  colnames(adj_matrix_bangladesh) = unique(bgd_geo$adm2_name)
  rownames(adj_matrix_bangladesh) = unique(bgd_geo$adm2_name)
  spatial_object_bangladesh = as.carAdjacency(adj_matrix_bangladesh)
  
  data = filter(data,year %in% years) 
  
  year_i = data %>%
    ungroup() %>%
    #arrange(year) %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    arrange(adm2) %>%
    dplyr::select(adm2) %>%
    pull()
  
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$adm2)))
  
  W = as.matrix(adj_matrix_bangladesh)
  W = ifelse(W > 0, 1, 0)
  diag(W) = 0
  
  
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$adm2))
  time = (years-mean(years))/sd(years)
  
  Q_time = build_RW1(Ti)      
  Q_ST = kronecker(Q_time, W)
  
  nb_ST   = mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  = sapply(nb_ST, length)
  adj_ST  = unlist(nb_ST)
  weights_ST = sapply(seq_along(nb_ST), function(i){
    j_list = nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST = unlist(weights_ST)
  L_ST = length(adj_ST)
  
  
  W.scale = -W
  diag(W.scale) = abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] = 1
  for(i in 1:(Ti-1)) Wt[i+1,i] = 1
  Wnb = mat2listw(Wt)
  Wnb = nb2WB(nb = Wnb$neighbours)
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                n  = length(spatial_object_bangladesh$num),
                L = length(spatial_object_bangladesh$weights),
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object_bangladesh$adj, 
                num = spatial_object_bangladesh$num,
                weights= spatial_object_bangladesh$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda = 4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  nimble_input = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,70,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,70,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,70,5))]),
    TFR_nat = data$tfr,
    m = m_bangladesh_male,
    X = X_bangladesh_male)
  
  
  
  
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*15,4.7,5),N,15),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,
              consts=consts,
              init=init))
  
  
  
}







data_input_women_pakistan = function(data,
                                     pak_geo,
                                     m,
                                     X,
                                     years=2000:2020){
  
  
  
  nb_pakistan = poly2nb(pak_geo, queen = FALSE)
  adj_matrix_pakistan = nb2mat(nb_pakistan, style = "B", zero.policy = TRUE)
  
  colnames(adj_matrix_pakistan) = unique(pak_geo$adm2_standard)
  rownames(adj_matrix_pakistan) = unique(pak_geo$adm2_standard)
  spatial_object_pakistan = as.carAdjacency(adj_matrix_pakistan)
  
  data = filter(data,year %in% years) 
  
  year_i = data %>%
    ungroup() %>%
    #arrange(year) %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    arrange(adm2_standard) %>%
    dplyr::select(adm2_standard) %>%
    pull()
  
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$adm2_standard)))
  
  W = as.matrix(adj_matrix_pakistan)
  W = ifelse(W > 0, 1, 0)
  diag(W) = 0
  
  
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$adm2_standard))
  time = (years-mean(years))/sd(years)
  
  Q_time = build_RW1(Ti)      
  Q_ST = kronecker(Q_time, W)
  
  nb_ST   = mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  = sapply(nb_ST, length)
  adj_ST  = unlist(nb_ST)
  weights_ST = sapply(seq_along(nb_ST), function(i){
    j_list = nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST = unlist(weights_ST)
  L_ST = length(adj_ST)
  
  
  W.scale = -W
  diag(W.scale) = abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] = 1
  for(i in 1:(Ti-1)) Wt[i+1,i] = 1
  Wnb = mat2listw(Wt)
  Wnb = nb2WB(nb = Wnb$neighbours)
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                n  = length(spatial_object_pakistan$num),
                L = length(spatial_object_pakistan$weights),
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object_pakistan$adj, 
                num = spatial_object_pakistan$num,
                weights= spatial_object_pakistan$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda = 4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  nimble_input = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,45,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,45,5))]),
    TFR_nat = data$tfr,
    m = m,
    X = X)
  
  
  
  
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*10,4.7,5),N,10),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,
              consts=consts,
              init=init))
  
  
  
}




data_input_men_pakistan = function(data,
                                   pak_geo,
                                   m,
                                   X,
                                   years=2000:2020){
  
  
  
  nb_pakistan = poly2nb(pak_geo, queen = FALSE)
  adj_matrix_pakistan = nb2mat(nb_pakistan, style = "B", zero.policy = TRUE)
  
  colnames(adj_matrix_pakistan) = unique(pak_geo$adm2_standard)
  rownames(adj_matrix_pakistan) = unique(pak_geo$adm2_standard)
  spatial_object_pakistan = as.carAdjacency(adj_matrix_pakistan)
  
  data = filter(data,year %in% years) 
  
  year_i = data %>%
    ungroup() %>%
    #arrange(year) %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    arrange(adm2_standard) %>%
    dplyr::select(adm2_standard) %>%
    pull()
  
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$adm2_standard)))
  
  W = as.matrix(adj_matrix_pakistan)
  W = ifelse(W > 0, 1, 0)
  diag(W) = 0
  
  
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$adm2_standard))
  time = (years-mean(years))/sd(years)
  
  Q_time = build_RW1(Ti)      
  Q_ST = kronecker(Q_time, W)
  
  nb_ST   = mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  = sapply(nb_ST, length)
  adj_ST  = unlist(nb_ST)
  weights_ST = sapply(seq_along(nb_ST), function(i){
    j_list = nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST = unlist(weights_ST)
  L_ST = length(adj_ST)
  
  
  W.scale = -W
  diag(W.scale) = abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] = 1
  for(i in 1:(Ti-1)) Wt[i+1,i] = 1
  Wnb = mat2listw(Wt)
  Wnb = nb2WB(nb = Wnb$neighbours)
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                n  = length(spatial_object_pakistan$num),
                L = length(spatial_object_pakistan$weights),
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object_pakistan$adj, 
                num = spatial_object_pakistan$num,
                weights= spatial_object_pakistan$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda = 4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  nimble_input = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,70,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,70,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,70,5))]),
    TFR_nat = data$tfr,
    m = m,
    X = X)
  
  
  
  
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*15,4.7,5),N,15),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,
              consts=consts,
              init=init))
  
  
  
}





#### Run chains using MCMC ####

run_MCMC_allcode = function(data, code, consts,init,init_name,niter=niter,nburnin=250000) {
  library(nimble)
  
  dynModel = nimbleModel(code = code,
                         data = data, 
                         constants = consts, 
                         inits = init)
  
  
  cat("============\n[compiling]\n============\n")
  CdynModel = compileNimble(dynModel,  showCompilerOutput = TRUE)
  cat("============\n[building]\n============\n")
  dynMCMC = buildMCMC(dynModel,  monitors = init_name, print=T)
  cat("=========\n[merging]\n============\n")
  CdynMCMC = compileNimble(dynMCMC, project = dynModel, showCompilerOutput = T)
  
  
  samples = runMCMC(CdynMCMC, 
                    niter = niter,
                    nburnin = nburnin,
                    thin = 50)
  
  
  
  return(samples)
}









