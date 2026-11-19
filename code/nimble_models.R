
#### Model to estimate the male TFR ####

code_male <- nimbleCode({
  # Priors
  sigma_b ~ dexp(rate=lambda)
  rho ~ dbeta(2,2)
  sigma_delta ~ dexp(rate=lambda)
  sigma_alpha ~ dexp(rate=lambda)
  sigma_nu ~ dexp(rate=lambda)
  tau_nu <- 1/pow(sigma_nu,2)
  tau_delta <- 1 / pow(sigma_delta, 2)
  
  # In the priors section
  mu_log_r ~ dnorm(0, sd = 10) 
  sigma_log_r ~ dexp(rate=1)
  
  for(j in 1:n) {
    log_r[j] ~ dnorm(mean=mu_log_r, sd = sigma_log_r)
    r[j] <- exp(log_r[j])
  }
  
  for(i in 1:N) {
    for(j in 1:12) {
      Lx[i, j] ~ dnorm(mean=Lx_star[i, j],sd=VAR_Lx_star[i, j])
    }
  }
  
  for(i in 1:N) {
    for(p in 1:2) {
      beta[i, p] ~ dnorm(mean=0,sd=1)
    }
  }
  
  s[1:n] ~ dcar_normal(adj[1:L],weights[1:L],num[1:n],1,zero_mean = 1)
  
  for(i in 1:n){
    us[i] ~ dnorm(mean = 0,sd = 1)
  }
  
  for(i in 1:n){
    b[i] <- sigma_b*(sqrt((1-rho))*us[i] + sqrt(rho/scale)*s[i])
  }
  
  nu[1:N] ~ dcar_normal(
    adj     = adj_ST[1:L_ST],
    weights = weights_ST[1:L_ST],
    num     = num_ST[1:N],
    tau     = tau_nu,
    zero_mean = 1
  )
  
  # Within-county means (length n)
  #mu_nu_space[1:n] <- Z_space[1:n, 1:N]%*%nu[1:N]
  
  
  # Within-time means (length Ti)
  #mu_nu_time[1:Ti] <- Z_time[1:Ti, 1:N]%*%nu[1:N]
  
  
  # for(i in 1:N){
  # nu_center[i] <- nu[i] - mu_nu_space[county_i[i]] - mu_nu_time[year_i[i]] 
  #}
  
  #delta[1] ~ dnorm(mean = 0, sd = sigma_delta_abs)
  delta[1:Ti] ~ dcar_normal(
    adj     = adj_time[1:L_time],
    weights = weights_time[1:L_time],
    num     = num_time[1:Ti],
    tau     = tau_delta,
    zero_mean = 1
  )
  
  
  for(i in 1:n){
    alpha[i] ~ dnorm(mean=0,sd=sigma_alpha)
  }
  
  
  # Transformations
  for(i in 1:N){
    log_TFR_mu[i] <- log(TFR_nat[i])+b[county_i[i]]+
      delta[year_i[i]]+alpha[county_i[i]]*time[year_i[i]]+nu[i]
    TFR[i] <- exp(log_TFR_mu[i])
    gamma[i, 1:9] <- m[1:9] + beta[i, 1] * X[1, 1:9] + beta[i, 2] * X[2, 1:9] 
    phi[i, 1:9] <- exp(gamma[i, 1:9])/sum(exp(gamma[i,1:9]))
    Fx[i,1] <- 0
    Fx[i,2:10] <- 0.2*phi[i,1:9]*TFR[i]
    Sx[i,1:9] <- Lx[i,3:11]/Lx[i,4:12]
    Kx[i,1:9] <- (Sx[i,1:9]*Fx[i,1:9]+Fx[i,2:10])*Lx[i,1]*0.5
    Kx_star[i] <- inprod(M[i, 1:9],Kx[i, 1:9])
  }
  # Data model
  
  for(i in 1:N){
    C[i] ~ dnegbin(size=r[county_i[i]],prob=r[county_i[i]]/(r[county_i[i]]+Kx_star[i]))
    res[i] <- (C[i]-Kx_star[i])/sqrt(Kx_star[i]+(pow(Kx_star[i],2)/r[county_i[i]]))
  }
  
})


#### Model to estimate the female TFR ####


code_female <- nimbleCode({
  # Priors
  sigma_b ~ dexp(rate=lambda)
  rho ~ dbeta(2,2)
  sigma_delta ~ dexp(rate=lambda)
  sigma_alpha ~ dexp(rate=lambda)
  sigma_nu ~ dexp(rate=lambda)
  tau_nu <- 1/pow(sigma_nu,2)
  tau_delta <- 1 / pow(sigma_delta, 2)
  
  # In the priors section
  mu_log_r ~ dnorm(0, sd = 10) 
  sigma_log_r ~ dexp(rate=1)
  
  for(j in 1:n) {
    log_r[j] ~ dnorm(mean=mu_log_r, sd = sigma_log_r)
    r[j] <- exp(log_r[j])
  }
  
  for(i in 1:N) {
    for(j in 1:10) {
      Lx[i, j] ~ dnorm(mean=Lx_star[i, j],sd=VAR_Lx_star[i, j])
    }
  }
  
  for(i in 1:N) {
    for(p in 1:2) {
      beta[i, p] ~ dnorm(mean=0,sd=1)
    }
  }
  
  s[1:n] ~ dcar_normal(adj[1:L],weights[1:L],num[1:n],1,zero_mean = 1)
  
  for(i in 1:n){
    us[i] ~ dnorm(mean = 0,sd = 1)
  }
  
  for(i in 1:n){
    b[i] <- sigma_b*(sqrt((1-rho))*us[i] + sqrt(rho/scale)*s[i])
  }
  
  
  nu[1:N] ~ dcar_normal(
    adj     = adj_ST[1:L_ST],
    weights = weights_ST[1:L_ST],
    num     = num_ST[1:N],
    tau     = tau_nu,
    zero_mean = 1
  )
  
  
  
  #delta[1] ~ dnorm(mean = 0, sd = sigma_delta_abs)
  delta[1:Ti] ~ dcar_normal(
    adj     = adj_time[1:L_time],
    weights = weights_time[1:L_time],
    num     = num_time[1:Ti],
    tau     = tau_delta,
    zero_mean = 1
  )
  
  
  for(i in 1:n){
    alpha[i] ~ dnorm(mean=0,sd=sigma_alpha)
  }
  alpha_center[1:n] <- alpha[1:n] - mean(alpha[1:n])
  
  # Transformations
  for(i in 1:N){
    log_TFR_mu[i] <- log(TFR_nat[i])+b[county_i[i]]+
      delta[year_i[i]]+alpha_center[county_i[i]]*time[year_i[i]]+nu[i]
    TFR[i] <- exp(log_TFR_mu[i])
    gamma[i, 1:7] <- m[1:7] + beta[i, 1] * X[1, 1:7] + beta[i, 2] * X[2, 1:7] 
    phi[i, 1:7] <- exp(gamma[i, 1:7])/sum(exp(gamma[i,1:7]))
    Fx[i,1] <- 0
    Fx[i,2:8] <- 0.2*phi[i,1:7]*TFR[i]
    Sx[i,1:7] <- Lx[i,3:9]/Lx[i,4:10]
    Kx[i,1:7] <- (Sx[i,1:7]*Fx[i,1:7]+Fx[i,2:8])*Lx[i,1]*0.5
    Kx_star[i] <- inprod(W[i, 1:7],Kx[i, 1:7])
  }
  # Data model
  
  for(i in 1:N){
    C[i] ~ dnegbin(size=r[county_i[i]],prob=r[county_i[i]]/(r[county_i[i]]+Kx_star[i]))
    res[i] <- (C[i]-Kx_star[i])/sqrt(Kx_star[i]+(pow(Kx_star[i],2)/r[county_i[i]]))
  }
  
})


##### Model to estimate the male TFR in Bangladesh and Pakistan (different reproductive span) #####

code_male_bgd_pak <- nimbleCode({
  # Priors
  sigma_b ~ dexp(rate=lambda)
  rho ~ dbeta(2,2)
  sigma_delta ~ dexp(rate=lambda)
  sigma_alpha ~ dexp(rate=lambda)
  sigma_nu ~ dexp(rate=lambda)
  tau_nu <- 1/pow(sigma_nu,2)
  tau_delta <- 1 / pow(sigma_delta, 2)
  
  # In the priors section
  mu_log_r ~ dnorm(0, sd = 10) 
  sigma_log_r ~ dexp(rate=1)
  
  for(j in 1:n) {
    log_r[j] ~ dnorm(mean=mu_log_r, sd = sigma_log_r)
    r[j] <- exp(log_r[j])
  }
  
  for(i in 1:N) {
    for(j in 1:15) {
      Lx[i, j] ~ dnorm(mean=Lx_star[i, j],sd=VAR_Lx_star[i, j])
    }
  }
  
  for(i in 1:N) {
    for(p in 1:2) {
      beta[i, p] ~ dnorm(mean=0,sd=1)
    }
  }
  
  s[1:n] ~ dcar_normal(adj[1:L],weights[1:L],num[1:n],1,zero_mean = 1)
  
  for(i in 1:n){
    us[i] ~ dnorm(mean = 0,sd = 1)
  }
  
  for(i in 1:n){
    b[i] <- sigma_b*(sqrt((1-rho))*us[i] + sqrt(rho/scale)*s[i])
  }
  
  
  nu[1:N] ~ dcar_normal(
    adj     = adj_ST[1:L_ST],
    weights = weights_ST[1:L_ST],
    num     = num_ST[1:N],
    tau     = tau_nu,
    zero_mean = 1
  )
  
  
  delta[1:Ti] ~ dcar_normal(
    adj     = adj_time[1:L_time],
    weights = weights_time[1:L_time],
    num     = num_time[1:Ti],
    tau     = tau_delta,
    zero_mean = 1
  )
  
  
  for(i in 1:n){
    alpha[i] ~ dnorm(mean=0,sd=sigma_alpha)
  }
  alpha_center[1:n] <- alpha[1:n] - mean(alpha[1:n])
  
  # Transformations
  for(i in 1:N){
    log_TFR_mu[i] <- log(TFR_nat[i])+b[county_i[i]]+
      delta[year_i[i]]+alpha[county_i[i]]*time[year_i[i]]+nu[i]
    TFR[i] <- exp(log_TFR_mu[i])
    gamma[i, 1:12] <- m[1:12] + beta[i, 1] * X[1, 1:12] + beta[i, 2] * X[2, 1:12] 
    phi[i, 1:12] <- exp(gamma[i, 1:12])/sum(exp(gamma[i,1:12]))
    Fx[i,1] <- 0
    Fx[i,2:13] <- 0.2*phi[i,1:12]*TFR[i]
    Sx[i,1:12] <- Lx[i,3:14]/Lx[i,4:15]
    Kx[i,1:12] <- (Sx[i,1:12]*Fx[i,1:12]+Fx[i,2:13])*Lx[i,1]*0.5
    Kx_star[i] <- inprod(M[i, 1:12],Kx[i, 1:12])
  }
  # Data model
  
  for(i in 1:N){
    C[i] ~ dnegbin(size=r[county_i[i]],prob=r[county_i[i]]/(r[county_i[i]]+Kx_star[i]))
    res[i] <- (C[i]-Kx_star[i])/sqrt(Kx_star[i]+(pow(Kx_star[i],2)/r[county_i[i]]))
  }
  
})

