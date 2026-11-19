#### load packages ####

source("code/upload_packages.R")

#### load functions ####
source("code/helpers.R")


#### load models written in NIMBLE ####

source("code/nimble_models.R")


#### France ####

load("data/france/model_input/data_input_france.RData")

##### Female TFR #####

this_cluster = makeCluster(4)


results_female_model_france = parLapply(cl = this_cluster, 
                                         X = 1:4, 
                                         fun = run_MCMC_allcode, 
                                         data = women_france_input$nimble_input, 
                                         code = code_female,
                                         consts = women_france_input$consts,
                                         init=women_france_input$init,
                                         init_name = c("TFR","Kx","r","res"),
                                         niter=250000)

stopCluster(this_cluster)






france_TFR_final_female = MCMCvis::MCMCsummary(
  results_female_model_france,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 


france_TFR_final_female$year = CM_france$year
france_TFR_final_female$prov = CM_france$prov
france_TFR_final_female$prov_name = CM_france$prov_name
names(france_TFR_final_female)[3:5] = c('TFR_low','TFR_median','TFR_high')


##### Male TFR #####


this_cluster = makeCluster(4)


results_male_model_france = parLapply(cl = this_cluster, 
                                       X = 1:4, 
                                       fun = run_MCMC_allcode, 
                                       data = men_france_input$nimble_input, 
                                       code = code_male,
                                       consts = men_france_input$consts,
                                       init=men_france_input$init,
                                       init_name = c("TFR","Kx"),
                                       niter=250000)

stopCluster(this_cluster)


france_TFR_final_male = MCMCvis::MCMCsummary(
  results_male_model_france,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 


france_TFR_final_male$year = CM_france$year
france_TFR_final_male$prov = CM_france$prov
france_TFR_final_male$prov_name = CM_france$prov_name
names(france_TFR_final_male)[3:5] = c('TFR_low','TFR_median','TFR_high')


save(france_TFR_final_male,
     france_TFR_final_female,
     file="results/france_model_tfr_summary.RData")


#### Spain ####

load("data/spain/model_input/data_input_spain.RData")

##### Female TFR #####


this_cluster = makeCluster(4)


results_female_model_spain = parLapply(cl = this_cluster, 
                                        X = 1:4, 
                                        fun = run_MCMC_allcode, 
                                        data = women_spain_input$nimble_input, 
                                        code = code_female,
                                        consts = women_spain_input$consts,
                                        init=women_spain_input$init,
                                        init_name = c("TFR","res","r","Kx_star"),
                                        niter=250000)

stopCluster(this_cluster)



spain_TFR_final_female = MCMCvis::MCMCsummary(
  results_female_model_spain,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 


spain_TFR_final_female$year = CW_spain$year
spain_TFR_final_female$prov = CW_spain$prov
spain_TFR_final_female$prov_name = CM_spain$province_name
names(spain_TFR_final_female)[3:5] = c('TFR_low','TFR_median','TFR_high')


##### Male TFR #####



this_cluster = makeCluster(4)


results_male_model_spain = parLapply(cl = this_cluster, 
                                      X = 1:4, 
                                      fun = run_MCMC_allcode, 
                                      data = men_spain_input$nimble_input, 
                                      code = code_male,
                                      consts = men_spain_input$consts,
                                      init= men_spain_input$init,
                                      init_name = c("TFR","res","r","Kx_star"),
                                      niter=250000)

stopCluster(this_cluster)





spain_TFR_final_male = MCMCvis::MCMCsummary(
  results_male_model_spain,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 


spain_TFR_final_male$year = CM_spain$year
spain_TFR_final_male$prov = CM_spain$prov
spain_TFR_final_male$prov_name = CM_spain$province_name

names(spain_TFR_final_male)[3:5] = c('TFR_low','TFR_median','TFR_high')




save(spain_TFR_final_male,
     spain_TFR_final_female,
     file="results/spain_model_tfr_summary.RData")

#### Bangladesh ####

load("data/bangladesh/model_input/data_input_bangladesh.RData")

##### Female TFR #####




this_cluster = makeCluster(4)


results_female_model_bangladesh = parLapply(cl = this_cluster, 
                                             X = 1:4, 
                                             fun = run_MCMC_allcode, 
                                             data = women_bangladesh_input$nimble_input, 
                                             code = code_female,
                                             consts = women_bangladesh_input$consts,
                                             init=women_bangladesh_input$init,
                                             init_name = c("TFR","Kx","r","res"),
                                             niter=250000)

stopCluster(this_cluster)


bangladesh_TFR_final_female = MCMCvis::MCMCsummary(
  results_female_model_bangladesh,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 


bangladesh_TFR_final_female$year = CW_bangladesh$year
bangladesh_TFR_final_female$prov = CW_bangladesh$adm2
names(bangladesh_TFR_final_female)[3:5] = c('TFR_low','TFR_median','TFR_high')


##### Male TFR #####




results_male_model_bangladesh = parLapply(cl = this_cluster, 
                                           X = 1:4, 
                                           fun = run_MCMC_allcode, 
                                           data = men_bangladesh_input$nimble_input, 
                                           code = code_male_bgd_pak,
                                           consts = men_bangladesh_input$consts,
                                           init= men_bangladesh_input$init,
                                           init_name = c("TFR","Kx","r","res"),
                                           niter=250000)

stopCluster(this_cluster)


bangladesh_TFR_final_male = MCMCvis::MCMCsummary(
  results_male_model_bangladesh,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 


bangladesh_TFR_final_male$year = CM_bangladesh$year
bangladesh_TFR_final_male$prov = CM_bangladesh$adm2
names(bangladesh_TFR_final_male)[3:5] = c('TFR_low','TFR_median','TFR_high')


save(bangladesh_TFR_final_male,
     bangladesh_TFR_final_female,
     file="results/bangladesh_model_tfr_summary.RData")




#### Pakistan ####

load("data/pakistan/model_input/data_input_pakistan.RData")

##### Female TFR #####




this_cluster = makeCluster(4)


results_female_model_pakistan = parLapply(cl = this_cluster, 
                                           X = 1:4, 
                                           fun = run_MCMC_allcode, 
                                           data = input_data_pakistan$nimble_input, 
                                           code = code_female,
                                           consts = input_data_pakistan$consts,
                                           init=input_data_pakistan$init,
                                           init_name = c("TFR","Kx","r","res"),
                                           niter=250000)

stopCluster(this_cluster)


pakistan_TFR_final_female = MCMCvis::MCMCsummary(
  results_female_model_pakistan,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 


pakistan_TFR_final_female$year = CW_pakistan$year
pakistan_TFR_final_female$prov = CW_pakistan$adm2_standard
pakistan_TFR_final_female$adm2_standard = CW_pakistan$adm2_standard
names(pakistan_TFR_final_female)[3:5] = c('TFR_low','TFR_median','TFR_high')



##### Male TFR #####



this_cluster = makeCluster(4)


results_male_model_pakistan = parLapply(cl = this_cluster, 
                                         X = 1:4, 
                                         fun = run_MCMC_allcode, 
                                         data = input_data_pakistan_men$nimble_input, 
                                         code = code_male_bgd_pak,
                                         consts = input_data_pakistan_men$consts,
                                         init=input_data_pakistan_men$init,
                                         init_name = c("TFR","Kx","r","res"),
                                         niter=250000)

stopCluster(this_cluster)




pakistan_TFR_final_male = MCMCvis::MCMCsummary(
  results_male_model_pakistan,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 


pakistan_TFR_final_male$year = CM_pakistan$year
pakistan_TFR_final_male$prov = CM_pakistan$adm2_standard
pakistan_TFR_final_male$adm2_standard = CM_pakistan$adm2_standard
names(pakistan_TFR_final_male)[3:5] = c('TFR_low','TFR_median','TFR_high')




save(pakistan_TFR_final_male,
     pakistan_TFR_final_female,
     file="results/pakistan_model_tfr_summary.RData")

