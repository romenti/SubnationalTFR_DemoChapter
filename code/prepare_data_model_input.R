#### Upload libraries ####

source("code/upload_packages.R")

#### Upload Functions ####

source("code/helpers.R")

#### France ####
load("data/france/pop/pop_data_input_france.RData")
load("data/france/geo/france_geo.RData")
load("data/france/mortality/res_Lx_france.RData")
load("data/france/fertility/france_fert_input.RData")

children_Lx_france = res_Lx_total_france %>%
  filter(age %in% c("0-4")) %>%
  select(prov,age,year,L0=median) %>%
  arrange(prov,year)


children_se_Lx_france = res_Lx_total_france %>%
  filter(age %in% c("0-4")) %>%
  select(prov,age,year,se_L0=se) %>%
  arrange(prov,year)


##### Men #####

male_Lx_france_wide = res_Lx_men_france %>%
  filter(age %in% c("5-9","10-14","15-19","20-24","25-29",
                    "30-34","35-39","40-44","45-49","50-54",
                    "55-59")) %>%
  select(prov,age,year,median) %>%
  pivot_wider(names_from = "age",values_from = "median") %>%
  arrange(prov,year)


names(male_Lx_france_wide)[3:13] = paste0('L',seq(5,55,5))

male_se_Lx_france_wide = res_Lx_men_france %>%
  filter(age %in% c("5-9","10-14","15-19","20-24","25-29",
                    "30-34","35-39","40-44","45-49","50-54",
                    "55-59")) %>%
  select(prov,age,year,se) %>%
  pivot_wider(names_from = "age",values_from = "se") %>%
  arrange(prov,year)


names(male_se_Lx_france_wide)[3:13] = paste0('se_L',seq(5,55,5))


CM_france = children_france %>%
  left_join(men_france) %>%
  left_join(children_Lx_france) %>%
  left_join(male_Lx_france_wide) %>%
  left_join(children_se_Lx_france) %>%
  left_join(male_se_Lx_france_wide) %>%
  filter(!(prov %in% c("2A","2B"))) %>%
  left_join(nat_tfr_france_male_final)



men_france_input = data_input_men_france(data = CM_france,
                                         fra_geo = france_shape_file_uniform,
                                         m = m_france_male,
                                         X = X_france_male)








##### Women #####

female_Lx_france_wide = res_Lx_women_france %>%
  filter(age %in% c("5-9","10-14","15-19","20-24","25-29",
                    "30-34","35-39","40-44","45-49")) %>%
  select(prov,age,year,median) %>%
  pivot_wider(names_from = "age",values_from = "median") %>%
  arrange(prov,year)


names(female_Lx_france_wide)[3:11] = paste0('L',seq(5,45,5))

female_se_Lx_france_wide = res_Lx_women_france %>%
  filter(age %in% c("5-9","10-14","15-19","20-24","25-29",
                    "30-34","35-39","40-44","45-49")) %>%
  select(prov,age,year,se) %>%
  pivot_wider(names_from = "age",values_from = "se") %>%
  arrange(prov,year)


names(female_se_Lx_france_wide)[3:11] = paste0('se_L',seq(5,45,5))


CW_france = children_france %>%
  left_join(women_france) %>%
  left_join(children_Lx_france) %>%
  left_join(female_Lx_france_wide) %>%
  left_join(children_se_Lx_france) %>%
  left_join(female_se_Lx_france_wide) %>%
  filter(!(prov %in% c("2A","2B"))) %>%
  left_join(nat_tfr_france_female)



women_france_input = data_input_women_france(data = CW_france,
                                             fra_geo = france_shape_file_uniform,
                                             m = m_france_female,
                                             X = X_france_female)





save(women_france_input,men_france_input,file=paste0("data/france/model_input/data_input_france.RData"))


#### Spain ####

load("data/spain/pop/pop_data_input_spain.RData")
load("data/spain/geo/spain_geo.RData")
load("data/spain/mortality/res_Lx_spain.RData")
load("data/spain/fertility/spain_fert_input.RData")



children_Lx_spain = res_Lx_total_spain %>%
  filter(age %in% c("0-4")) %>%
  ungroup() %>%
  select(prov,year,L0=median) %>%
  arrange(prov,year)


children_se_Lx_spain = res_Lx_total_spain %>%
  filter(age %in% c("0-4")) %>%
  ungroup() %>%
  select(prov,year,se_L0=se) %>%
  arrange(prov,year)


##### Men #####

# read spatial data 
load("data/spain/geo/spain_geo.RData")

male_Lx_spain_wide = res_Lx_men_spain %>%
  filter(age %in% c("5-9","10-14","15-19","20-24","25-29",
                    "30-34","35-39","40-44","45-49","50-54",
                    "55-59")) %>%
  select(prov,age,year,median) %>%
  pivot_wider(names_from = "age",values_from = "median") %>%
  arrange(prov,year)


names(male_Lx_spain_wide)[3:13] = paste0('L',seq(5,55,5))

male_se_Lx_spain_wide = res_Lx_men_spain %>%
  filter(age %in% c("5-9","10-14","15-19","20-24","25-29",
                    "30-34","35-39","40-44","45-49","50-54",
                    "55-59")) %>%
  select(prov,age,year,se) %>%
  pivot_wider(names_from = "age",values_from = "se") %>%
  arrange(prov,year)


names(male_se_Lx_spain_wide)[3:13] = paste0('se_L',seq(5,55,5))


CM_spain = children_spain %>%
  left_join(men_spain) %>%
  rename(prov=province_code) %>%
  left_join(children_Lx_spain) %>%
  left_join(male_Lx_spain_wide) %>%
  left_join(children_se_Lx_spain) %>%
  left_join(male_se_Lx_spain_wide) %>%
  filter(!(prov %in% c("2A","2B",
                       "07", "35", "38", "51", "52"))) %>%
  left_join(nat_tfr_spain_male_final)




men_spain_input = data_input_men_spain(data = CM_spain,
                                       esp_geo = spain_provinces,
                                         m = m_spain_male,
                                         X = X_spain_male)




##### Women #####

female_Lx_spain_wide = res_Lx_women_spain %>%
  filter(age %in% c("5-9","10-14","15-19","20-24","25-29",
                    "30-34","35-39","40-44","45-49")) %>%
  select(prov,age,year,median) %>%
  pivot_wider(names_from = "age",values_from = "median") %>%
  arrange(prov,year)


names(female_Lx_spain_wide)[3:11] = paste0('L',seq(5,45,5))

female_se_Lx_spain_wide = res_Lx_women_spain %>%
  filter(age %in% c("5-9","10-14","15-19","20-24","25-29",
                    "30-34","35-39","40-44","45-49")) %>%
  select(prov,age,year,se) %>%
  pivot_wider(names_from = "age",values_from = "se") %>%
  arrange(prov,year)


names(female_se_Lx_spain_wide)[3:11] = paste0('se_L',seq(5,45,5))


CW_spain = children_spain %>%
  left_join(women_spain) %>%
  rename(prov=province_code) %>%
  left_join(children_Lx_spain) %>%
  left_join(female_Lx_spain_wide) %>%
  left_join(children_se_Lx_spain) %>%
  left_join(female_se_Lx_spain_wide) %>%
  filter(!(prov %in% c("2A","2B",
                       "07", "35", "38", "51", "52"))) %>%
  left_join(nat_tfr_spain_female)



women_spain_input = data_input_women_spain(data = CW_spain,
                                            esp_geo = spain_provinces,
                                             m = m_spain_female,
                                             X = X_spain_female)





save(women_spain_input,men_spain_input,file=paste0("data/spain/model_input/data_input_spain.RData"))



#### Bangladesh ####


load("data/bangladesh/pop/bangladesh_pop_data.RData")
load("data/bangladesh/geo/bangladesh_adm2.RData")
load("data/bangladesh/mortality/bangladesh_Lx_estimates.RData")
load("data/bangladesh/fertility/bangladesh_fert_input.RData")

#### Women ####

names(children_bangladesh)[1:2] = c("adm1","adm2")
CW_bangladesh = children_bangladesh %>%
  left_join(women_bangladesh) %>%
  mutate(
    region = case_when(
      adm1 == "BARISHAL"   ~ "Barishal",
      adm1 == "CHATTOGRAM" ~ "Chattogram",
      adm1 == "DHAKA"      ~ "Dhaka",
      adm1 == "MYMENSINGH" ~ "Dhaka",
      adm1 == "KHULNA"     ~ "Khulna",
      adm1 == "RĀJSHĀHI"   ~ "Rajshahi",
      adm1 == "RANGPUR"    ~ "Rajshahi",
      adm1 == "SYLHET"     ~ "Sylhet",
      TRUE                 ~ NA_character_
    )
  ) %>%
  left_join(Lx_hat_female_bangladesh) %>%
  left_join(Lx_se_female_bangladesh) %>%
  left_join(tfr_bangladesh_female) %>%
  arrange(adm2,year) 


women_bangladesh_input = data_input_women_bangladesh(data = CW_bangladesh,
                                                 bgd_geo = shape_file_bangladesh_admin2,
                                                 m = m_bangladesh_female,
                                                 X = X_bangladesh_female)




#### Men ####


CM_bangladesh = children_bangladesh %>%
  left_join(men_bangladesh) %>%
  mutate(
    region = case_when(
      adm1 == "BARISHAL"   ~ "Barishal",
      adm1 == "CHATTOGRAM" ~ "Chattogram",
      adm1 == "DHAKA"      ~ "Dhaka",
      adm1 == "MYMENSINGH" ~ "Dhaka",
      adm1 == "KHULNA"     ~ "Khulna",
      adm1 == "RĀJSHĀHI"   ~ "Rajshahi",
      adm1 == "RANGPUR"    ~ "Rajshahi",
      adm1 == "SYLHET"     ~ "Sylhet",
      TRUE                 ~ NA_character_
    )
  ) %>%
  left_join(Lx_hat_male_bangladesh) %>%
  left_join(Lx_se_male_bangladesh) %>%
  left_join(bangladesh_male_single_tfr) %>%
  arrange(adm2,year) 


men_bangladesh_input = data_input_men_bangladesh(data = CM_bangladesh,
                                                 bgd_geo = shape_file_bangladesh_admin2,
                                                 m = m_bangladesh_male,
                                                 X = X_bangladesh_male)





save(women_bangladesh_input,men_bangladesh_input,file=paste0("data/bangladesh/model_input/data_input_bangladesh.RData"))




#### Pakistan ####





children_pakistan = children_pakistan %>%
  mutate(adm1 = str_to_upper(str_squish(adm1)),
         
         region = case_when(
           adm1 == "BALOCHISTAN" ~ "Balochistan",
           
           adm1 %in% c(
             "KHYBER PAKHTUNKHWA",
             "FEDERALLY ADMINISTERED TRIBAL AREAS",
             "GILGIT-BALTISTAN"
           ) ~ "Khyber Pakhtunkhwa",
           
           adm1 %in% c(
             "PUNJAB",
             "ISLAMABAD",
             "AZAD KASHMIR"
           ) ~ "Punjab",
           
           adm1 == "SINDH" ~ "Sindh",
           
           TRUE ~ NA_character_
         ))



women_pakistan = women_pakistan %>%
  mutate(adm1 = str_to_upper(str_squish(adm1)),
         
         region = case_when(
           adm1 == "BALOCHISTAN" ~ "Balochistan",
           
           adm1 %in% c(
             "KHYBER PAKHTUNKHWA",
             "FEDERALLY ADMINISTERED TRIBAL AREAS",
             "GILGIT-BALTISTAN"
           ) ~ "Khyber Pakhtunkhwa",
           
           adm1 %in% c(
             "PUNJAB",
             "ISLAMABAD",
             "AZAD KASHMIR"
           ) ~ "Punjab",
           
           adm1 == "SINDH" ~ "Sindh",
           
           TRUE ~ NA_character_
         ))



CW_pakistan = children_pakistan_harmonized %>%
  left_join(women_pakistan_harmonized) %>%
  group_by(year,adm2_standard,region) %>%
  summarise(C=sum(C),
            W15=sum(W15),
            W20=sum(W20),
            W25=sum(W25),
            W30=sum(W30),
            W35=sum(W35),
            W40=sum(W40),
            W45=sum(W45)) %>%
  left_join(Lx_hat_female_pakistan) %>%
  left_join(Lx_se_female_pakistan) %>%
  left_join(tfr_pakistan_female) %>%
  arrange(adm2_standard,year)






women_input_data_pakistan = data_input_women_pakistan(data=CW_pakistan,
                                                pak_geo=shape_file_pakistan_admin2_harmonized,
                                                m=m_pakistan_female,
                                                X=X_pakistan_female,
                                                years=2000:2020)




#### Men ####




CM_pakistan = children_pakistan_harmonized %>%
  left_join(men_pakistan_harmonized) %>%
  group_by(year,adm2_standard,region) %>%
  summarise(C=sum(C),
            M15=sum(M15),
            M20=sum(M20),
            M25=sum(M25),
            M30=sum(M30),
            M35=sum(M35),
            M40=sum(M40),
            M45=sum(M45),
            M50=sum(M50),
            M55=sum(M55),
            M60=sum(M60),
            M65=sum(M65),
            M70=sum(M70)) %>%
  left_join(Lx_hat_male_pakistan) %>%
  left_join(Lx_se_male_pakistan) %>%
  left_join(pakistan_male_single_tfr) %>%
  arrange(adm2_standard,year)



men_input_data_pakistan = data_input_men_pakistan(data=CM_pakistan,
                                                  pak_geo=shape_file_pakistan_admin2_harmonized,
                                                  m=m_pakistan_male,
                                                  X=X_pakistan_male,
                                                  years=2000:2020)



save(women_input_data_pakistan,
     men_input_data_pakistan,file=paste0("data/pakistan/model_input/data_input_pakistan.RData"))



