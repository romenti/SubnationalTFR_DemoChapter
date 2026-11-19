#### France ####

france_deaths_national = read.csv("data/france/mortality/fra_deaths_national.txt", sep="")

age_levels = c(
  "0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
  "60-64", "65-69", "70-74", "75-79", "80-84", "85+"
)

france_deaths_national = france_deaths_national %>%
  mutate(Age = ifelse(Age %in% c("85-89","90-94",
                                 "95-99","100-104",
                                 "105-109","110+"),"85+",Age)) %>%
  filter(Year %in% 2000:2020) %>%
  group_by(Year,Age) %>%
  summarise(Total=sum(Total),
            Male=sum(Male),
            Female=sum(Female)) |>
  pivot_longer(!c("Year","Age"),values_to = "deaths",names_to = "sex") %>%
  ungroup() %>%
  mutate(Age=factor(Age,levels=age_levels)) %>%
  arrange(Year,Age)


france_pop_national = read.csv("data/france/mortality/fra_pop_nat.txt", sep="")

france_pop_national = france_pop_national %>%
  mutate(Age = ifelse(Age %in% c("85-89","90-94",
                                 "95-99","100-104",
                                 "105-109","110+"),"85+",Age),
         Year = as.numeric(Year)) %>%
  filter(Year %in% 2000:2020) %>%
  group_by(Year,Age) %>%
  summarise(Total=sum(Total),
            Male=sum(Male),
            Female=sum(Female)) |>
  pivot_longer(!c("Year","Age"),values_to = "pop",names_to = "sex") %>%
  ungroup() %>%
  mutate(Age=factor(Age,levels=age_levels))  %>%
  arrange(Year,Age)


mx_france_national_rates_total = france_deaths_national %>%
  left_join(france_pop_national) %>%
  mutate(log_mx=log(deaths/pop)) %>%
  filter(sex=="Total") %>%
  select(-sex) %>%
  ungroup() %>%
  pivot_wider(c("Age"),names_from = "Year",values_from = "log_mx") %>%
  select(-Age) %>%
  as.matrix()


mx_france_national_rates_male = france_deaths_national %>%
  left_join(france_pop_national) %>%
  mutate(log_mx=log(deaths/pop)) %>%
  filter(sex=="Male") %>%
  select(-sex) %>%
  ungroup() %>%
  pivot_wider(c("Age"),names_from = "Year",values_from = "log_mx") %>%
  select(-Age) %>%
  as.matrix()


mx_france_national_rates_female = france_deaths_national %>%
  left_join(france_pop_national) %>%
  mutate(log_mx=log(deaths/pop)) %>%
  filter(sex=="Female") %>%
  select(-sex) %>%
  ungroup() %>%
  pivot_wider(c("Age"),names_from = "Year",values_from = "log_mx") %>%
  select(-Age) %>%
  as.matrix()




##### SVD values #####

svd_results_france_total = svd(mx_france_national_rates_total-rowMeans(mx_france_national_rates_total))
Y_total = matrix(NA,19,3)
Y_total[,1] = rowMeans(mx_france_national_rates_total)
Y_total[,2] = round(svd_results_italy$u[,1],6)
Y_total[,3] = round(svd_results_italy$u[,2],6)


svd_results_france_male = svd(mx_france_national_rates_male-rowMeans(mx_france_national_rates_male))
Y_men = matrix(NA,19,3)
Y_men[,1] = rowMeans(mx_france_national_rates_male)
Y_men[,2] = round(svd_results_france_male$u[,1],6)
Y_men[,3] = round(svd_results_france_male$u[,2],6)

svd_results_france_female = svd(mx_france_national_rates_female-rowMeans(mx_france_national_rates_female))
Y_women = matrix(NA,19,3)
Y_women[,1] = rowMeans(mx_france_national_rates_female)
Y_women[,2] = round(svd_results_france_female$u[,1],6)
Y_women[,3] = round(svd_results_france_female$u[,2],6)

#### Get the data into JAGS format ####

france_deaths_subnational = read_delim("data/france/mortality/france_mort_data.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

load("data/france/mortality/pop_france_prov.RData")

france_deaths_subnational_female = france_deaths_subnational %>%
  filter(sex=="Females",year %in% 2000:2020)


france_deaths_subnational_male = france_deaths_subnational %>%
  filter(sex=="Males",year %in% 2000:2020)

france_deaths_subnational_total = france_deaths_subnational %>%
  group_by(prov,year,age_group) %>%
  summarise(deaths=sum(deaths),
            pop=sum(pop))

pop_france_total = population_france_final %>%
  mutate(age_group = ifelse(age_group %in% c("85-89","90-94",
                                 "95-99","100-104",
                                 "105-109","110+"),"85+",age_group)) %>%
  filter(year %in% 2000:2020,
         sex=="Both") %>%
  group_by(year,age_group) %>%
  summarise(pop=sum(pop))) |>
  pivot_longer(!c("Year","Age"),values_to = "pop",names_to = "sex") %>%
  ungroup() %>%
  mutate(Age=factor(Age,levels=age_levels))  %>%
  arrange(Year,Age)

years = 2000:2020
age_groups = age_levels
departments = unique(france_deaths_subnational$prov)


# initialise arrays (note I only have one state)
y.xtas = array(NA, c(length(age_groups), length(years), length(departments),1))
pop.xtas = array(NA, c(length(age_groups), length(years), length(departments),1))

for(j in 1:length(years)){
 
  this_value = as.matrix(france_deaths_subnational_total %>% filter(year == years[j]) %>% ungroup() %>% select(prov, age_group, deaths) %>% spread(prov, deaths) %>% select(-age_group))
  this_pop = as.matrix(france_deaths_subnational_total %>% filter(year == years[j]) %>% ungroup() %>% select(prov, age_group, pop) %>% spread(prov, pop) %>% select(-age_group))
  these_dep = colnames(this_value)
  these_dep_pop = colnames(this_pop)
  for(k in 1:length(these_dep)){
    y.xtas[,j,which(departments==these_dep[k]),] = this_value[,k]
  }
  for(l in 1:length(these_dep_pop)){
    pop.xtas[,j,which(departments==these_dep_pop[l]),] = this_pop[,l]
  }
}

y.xtas[is.na(y.xtas)] = 0
pop.xtas[is.na(pop.xtas)] = 0



# initialise arrays 
fem_y.xtas = array(NA, c(length(age_groups), length(years), length(departments),1))
fem_pop.xtas = array(NA, c(length(age_groups), length(years), length(departments),1))

for(j in 1:length(years)){
  
  this_value = as.matrix(france_deaths_subnational_female %>% filter(year == years[j]) %>% ungroup() %>% select(prov, age_group, deaths) %>% spread(prov, deaths) %>% select(-age_group))
  this_pop = as.matrix(france_deaths_subnational_female %>% filter(year == years[j]) %>% ungroup() %>% select(prov, age_group, pop) %>% spread(prov, pop) %>% select(-age_group))
  these_dep = colnames(this_value)
  these_dep_pop = colnames(this_pop)
  for(k in 1:length(these_dep)){
    fem_y.xtas[,j,which(departments==these_dep[k]),] = this_value[,k]
  }
  for(l in 1:length(these_dep_pop)){
    fem_pop.xtas[,j,which(departments==these_dep_pop[l]),] = this_pop[,l]
  }
}

fem_y.xtas[is.na(y.xtas)] = 0
fem_pop.xtas[is.na(pop.xtas)] = 0


# initialise arrays 
male_y.xtas = array(NA, c(length(age_groups), length(years), length(departments),1))
male_pop.xtas = array(NA, c(length(age_groups), length(years), length(departments),1))

for(j in 1:length(years)){
  
  this_value = as.matrix(france_deaths_subnational_male %>% filter(year == years[j]) %>% ungroup() %>% select(prov, age_group, deaths) %>% spread(prov, deaths) %>% select(-age_group))
  this_pop = as.matrix(france_deaths_subnational_male %>% filter(year == years[j]) %>% ungroup() %>% select(prov, age_group, pop) %>% spread(prov, pop) %>% select(-age_group))
  these_dep = colnames(this_value)
  these_dep_pop = colnames(this_pop)
  for(k in 1:length(these_dep)){
    male_y.xtas[,j,which(departments==these_dep[k]),] = this_value[,k]
  }
  for(l in 1:length(these_dep_pop)){
    male_pop.xtas[,j,which(departments==these_dep_pop[l]),] = this_pop[,l]
  }
}

male_y.xtas[is.na(y.xtas)] = 0
male_pop.xtas[is.na(pop.xtas)] = 0

jags_data_france_total = list(y.xtas = y.xtas, 
                  pop.xtas = pop.xtas, 
                  Yx = Y_total,
                  S = 1, X= length(age_groups), T = length(years), 
                  n.a=length(departments), n.amax=length(departments), P=3 )




jags_data_france_male = list(y.xtas = male_y.xtas, 
                               pop.xtas = male_pop.xtas, 
                               Yx = Y_men,
                               S = 1, X= length(age_groups), T = length(years), 
                               n.a=length(departments), n.amax=length(departments), P=3 )


jags_data_france_female = list(y.xtas = fem_y.xtas, 
                              pop.xtas = fem_pop.xtas, 
                              Yx = Y_women,
                              S = 1, X= length(age_groups), 
                              T = length(years), 
                              n.a=length(departments), 
                              n.amax=length(departments), 
                              P=3)



##### Run JAGS models #####

parnames = c("beta.tas", "mu.beta" ,"sigma.beta", "tau.mu", "u.xtas", "mx.xtas")


france_mod_total = jags(data = jags_data_france_total, 
                      parameters.to.save=parnames, 
                      n.iter = 60000,
                      model.file = "mortality_model_jags/model.txt")


france_mod_total_array = france_mod_total$BUGSoutput$sims.array

france_mod_male = jags(data = jags_data_france_male, 
                         parameters.to.save=parnames, 
                         n.iter = 60000,
                         model.file = "mortality_model_jags/model.txt")


france_mod_male_array = france_mod_male$BUGSoutput$sims.array

france_mod_female = jags(data = jags_data_france_female, 
                        parameters.to.save=parnames, 
                        n.iter = 60000,
                        model.file = "mortality_model_jags/model.txt")


france_mod_female_array = france_mod_female$BUGSoutput$sims.array





res_Lx_total_france = c()

ages = c(0,1,seq(5,85,5))

for (c in seq_along(departments)) {
  for (i in seq_along(years)) {
    mx_draws = sapply(seq_along(age_groups), function(j) {
      france_mod_total_array[, , paste0("mx.xtas[", j, ",", i, ",", c, ",1]")]
    })
    
    
    nLx_draws = apply(mx_draws, 1, derive_nLx_values, ages = ages)
    
    res_Lx_total_france = rbind(res_Lx_total_france,tibble(
      age = age_groups,
      department = departments[c],
      year = years[i],
      median = apply(nLx_draws, 1, median),
      se = apply(nLx_draws, 1, sd),
      lower = apply(nLx_draws, 1, quantile, probs = 0.025),
      upper = apply(nLx_draws, 1, quantile, probs = 0.975))
    )
    
  }
}


res_Lx_male_france = c()


for (c in seq_along(departments)) {
  for (i in seq_along(years)) {
    mx_draws = sapply(seq_along(age_groups), function(j) {
      france_mod_male_array[, , paste0("mx.xtas[", j, ",", i, ",", c, ",1]")]
    })
    
    
    nLx_draws = apply(mx_draws, 1, derive_nLx_values, ages = ages)
    
    res_Lx_male_france = rbind(res_Lx_male_france,tibble(
      age = age_groups,
      county = counties[c],
      year = years[i],
      median = apply(nLx_draws, 1, median),
      se = apply(nLx_draws, 1, sd),
      lower = apply(nLx_draws, 1, quantile, probs = 0.025),
      upper = apply(nLx_draws, 1, quantile, probs = 0.975))
    )
    
  }
}




res_Lx_female_france = c()


for (c in seq_along(departments)) {
  for (i in seq_along(years)) {
    mx_draws = sapply(seq_along(age_groups), function(j) {
      france_mod_female_array[, , paste0("mx.xtas[", j, ",", i, ",", c, ",1]")]
    })
    
    
    nLx_draws = apply(mx_draws, 1, derive_nLx_values, ages = ages)
    
    res_Lx_female_france = rbind(res_Lx_female_france,tibble(
      age = age_groups,
      county = counties[c],
      year = years[i],
      median = apply(nLx_draws, 1, median),
      se = apply(nLx_draws, 1, sd),
      lower = apply(nLx_draws, 1, quantile, probs = 0.025),
      upper = apply(nLx_draws, 1, quantile, probs = 0.975))
    )
    
  }
}


save(res_Lx_female_france,
     res_Lx_male_france,
     res_Lx_total_france,
     file="data/france/mortality/res_Lx_france.RData")


#### Spain ####

spain_deaths_national = read.csv("data/spain/mortality/spain_deaths_nat_hmd.txt", sep="")

age_levels = c(
  "0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
  "60-64", "65-69", "70-74", "75-79", "80-84", "85+"
)

spain_deaths_national = spain_deaths_national %>%
  mutate(Age = ifelse(Age %in% c("85-89","90-94",
                                 "95-99","100-104",
                                 "105-109","110+"),"85+",Age)) %>%
  filter(Year %in% 2000:2020) %>%
  group_by(Year,Age) %>%
  summarise(Total=sum(Total),
            Male=sum(Male),
            Female=sum(Female)) |>
  pivot_longer(!c("Year","Age"),values_to = "deaths",names_to = "sex") %>%
  ungroup() %>%
  mutate(Age=factor(Age,levels=age_levels)) %>%
  arrange(Year,Age)


spain_pop_national = read.csv("data/spain/mortality/spain_pop_nat_hmd.txt", sep="")

spain_pop_national = spain_pop_national %>%
  mutate(Age = ifelse(Age %in% c("85-89","90-94",
                                 "95-99","100-104",
                                 "105-109","110+"),"85+",Age),
         Year = as.numeric(Year)) %>%
  filter(Year %in% 2000:2020) %>%
  group_by(Year,Age) %>%
  summarise(Total=sum(Total),
            Male=sum(Male),
            Female=sum(Female)) |>
  pivot_longer(!c("Year","Age"),values_to = "pop",names_to = "sex") %>%
  ungroup() %>%
  mutate(Age=factor(Age,levels=age_levels))  %>%
  arrange(Year,Age)


mx_spain_national_rates_total = spain_deaths_national %>%
  left_join(spain_pop_national) %>%
  mutate(log_mx=log(deaths/pop)) %>%
  filter(sex=="Total") %>%
  select(-sex) %>%
  ungroup() %>%
  pivot_wider(c("Age"),names_from = "Year",values_from = "log_mx") %>%
  select(-Age) %>%
  as.matrix()


mx_spain_national_rates_male = spain_deaths_national %>%
  left_join(spain_pop_national) %>%
  mutate(log_mx=log(deaths/pop)) %>%
  filter(sex=="Male") %>%
  select(-sex) %>%
  ungroup() %>%
  pivot_wider(c("Age"),names_from = "Year",values_from = "log_mx") %>%
  select(-Age) %>%
  as.matrix()


mx_spain_national_rates_female = spain_deaths_national %>%
  left_join(spain_pop_national) %>%
  mutate(log_mx=log(deaths/pop)) %>%
  filter(sex=="Female") %>%
  select(-sex) %>%
  ungroup() %>%
  pivot_wider(c("Age"),names_from = "Year",values_from = "log_mx") %>%
  select(-Age) %>%
  as.matrix()




##### SVD values #####

svd_results_spain_total = svd(mx_spain_national_rates_total-rowMeans(mx_spain_national_rates_total))
Y_total = matrix(NA,19,3)
Y_total[,1] = rowMeans(mx_spain_national_rates_total)
Y_total[,2] = round(svd_results_italy$u[,1],6)
Y_total[,3] = round(svd_results_italy$u[,2],6)


svd_results_spain_male = svd(mx_spain_national_rates_male-rowMeans(mx_spain_national_rates_male))
Y_men = matrix(NA,19,3)
Y_men[,1] = rowMeans(mx_spain_national_rates_male)
Y_men[,2] = round(svd_results_spain_male$u[,1],6)
Y_men[,3] = round(svd_results_spain_male$u[,2],6)

svd_results_spain_female = svd(mx_spain_national_rates_female-rowMeans(mx_spain_national_rates_female))
Y_women = matrix(NA,19,3)
Y_women[,1] = rowMeans(mx_spain_national_rates_female)
Y_women[,2] = round(svd_results_spain_female$u[,1],6)
Y_women[,3] = round(svd_results_spain_female$u[,2],6)

#### Get the data into JAGS format ####


subnational_deaths_spain = read_delim("data/spain/mortality/deaths_age_sex.csv", 
                                       delim = ";", escape_double = FALSE, 
                                       trim_ws = TRUE,
                                       locale = locale(decimal_mark = ",", grouping_mark = "."),
                                       col_types = cols(Total = col_number()))

spain_deaths_0008 = read_delim(
  "https://www.ine.es/jaxiT3/files/t/csv_bdsc/6547.csv",
  delim = ";",
  locale = locale(decimal_mark = ",", grouping_mark = "."),
  col_types = cols(.default = col_character())
) %>%
  mutate(
    Periodo = as.integer(Periodo),
    Total = parse_number(
      Total,
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    )
  ) %>%
  filter(between(Periodo, 2000, 2008))


names(spain_deaths_0008) = names(subnational_deaths_spain)
spain_deaths_0008 = spain_deaths_0008 %>%
  select(prov=Provinces,Age,Sex,Year=Periodo,deaths=Total) %>%
  filter(!(prov %in% c("Total") )) %>%
  mutate(Age=ifelse(Age %in% c("Menores de un año"),0,readr::parse_number(Age)),
         prov = stringr::str_extract(prov, "^\\d{2}"),
         Sex = case_when(Sex=="Hombres"~"Male",
                         Sex=="Mujeres"~"Female",
                         TRUE~"Total")) %>%
  filter(
    !is.na(prov),
    !prov %in% c("07", "35", "38", "51", "52")
  ) %>%
  mutate(
    age_group = case_when(
      Age == 0 ~ "0",
      Age <= 4 ~ "1-4",
      Age >= 85 ~ "85+",
      TRUE ~ paste0(5 * (Age %/% 5), "-", 5 * (Age %/% 5) + 4)
    ),
    age_group = factor(age_group, levels = age_levels, ordered = TRUE)
  ) %>%
  group_by(prov, Sex, Year, age_group) %>%
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  arrange(prov, Sex, Year, age_group)



spain_deaths_subnational = subnational_deaths_spain %>%
  filter(`Month of death`=="Total") %>%
  select(prov=`National and Provinces`,Age,Sex,Year=Periodo,deaths=Total) %>%
  filter(Age!="All ages",prov!="National") %>%
  mutate(Age=ifelse(Age %in% c("Under 1 year old"),0,readr::parse_number(Age)),
         prov = stringr::str_extract(prov, "^\\d{2}"),
         Sex = case_when(Sex=="Males"~"Male",
                         Sex=="Females"~"Female",
                         TRUE~"Total")) %>%
  filter(
    !is.na(prov),
    !prov %in% c("07", "35", "38", "51", "52")
  ) %>%
  mutate(
    age_group = case_when(
      Age == 0 ~ "0",
      Age <= 4 ~ "1-4",
      Age >= 85 ~ "85+",
      TRUE ~ paste0(5 * (Age %/% 5), "-", 5 * (Age %/% 5) + 4)
    ),
    age_group = factor(age_group, levels = age_levels, ordered = TRUE)
  ) %>%
  group_by(prov, Sex, Year, age_group) %>%
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  arrange(prov, Sex, Year, age_group)
  


spain_deaths_subnational_total = bind_rows(spain_deaths_0008,spain_deaths_subnational) %>%
  arrange(prov, Sex, Year, age_group)

write.csv(spain_deaths_subnational_total,file="data/spain/mortality/spain_subnational_deaths.csv",row.names = F)

spain_subnational_deaths = read_csv("data/spain/mortality/spain_subnational_deaths.csv")

spain_deaths_subnational_female = spain_subnational_deaths %>%
  filter(Sex=="Female",Year %in% 2000:2020) %>%
  select(prov,year=Year,age_group,deaths) %>%
  left_join(spain_population_long %>%
              filter(sex=="Female") %>%
              select(prov=province_code,year,age_group,pop=population)) %>%
  arrange(prov,year, age_group)


spain_deaths_subnational_male = spain_subnational_deaths %>%
  filter(Sex=="Male",Year %in% 2000:2020) %>%
  select(prov,year=Year,age_group,deaths) %>%
  left_join(spain_population_long %>%
              filter(sex=="Male") %>%
              select(prov=province_code,year,age_group,pop=population))



spain_deaths_subnational_total = spain_subnational_deaths %>%
  filter(Sex=="Total",Year %in% 2000:2020) %>%
  select(prov,year=Year,age_group,deaths) %>%
  left_join(spain_population_long %>%
              select(prov=province_code,year,age_group,pop=population) %>%
              group_by(prov,year,age_group) %>%
              summarise(pop=sum(pop)))


years = 2000:2020
age_groups = age_levels
province = unique(spain_deaths_subnational_total$prov)


# initialise arrays (note I only have one state)
y.xtas = array(NA, c(length(age_groups), length(years), length(province),1))
pop.xtas = array(NA, c(length(age_groups), length(years), length(province),1))

for(j in 1:length(years)){
  
  this_value = as.matrix(spain_deaths_subnational_total %>% filter(year == years[j]) %>% ungroup() %>% select(prov, age_group, deaths) %>% spread(prov, deaths) %>% select(-age_group))
  this_pop = as.matrix(spain_deaths_subnational_total %>% filter(year == years[j]) %>% ungroup() %>% select(prov, age_group, pop) %>% spread(prov, pop) %>% select(-age_group))
  these_dep = colnames(this_value)
  these_dep_pop = colnames(this_pop)
  for(k in 1:length(these_dep)){
    y.xtas[,j,which(province==these_dep[k]),] = this_value[,k]
  }
  for(l in 1:length(these_dep_pop)){
    pop.xtas[,j,which(province==these_dep_pop[l]),] = this_pop[,l]
  }
}

y.xtas[is.na(y.xtas)] = 0
pop.xtas[is.na(pop.xtas)] = 0



# initialise arrays 
fem_y.xtas = array(NA, c(length(age_groups), length(years), length(province),1))
fem_pop.xtas = array(NA, c(length(age_groups), length(years), length(province),1))

for(j in 1:length(years)){
  
  this_value = as.matrix(spain_deaths_subnational_female %>% filter(year == years[j]) %>% ungroup() %>% select(prov, age_group, deaths) %>% spread(prov, deaths) %>% select(-age_group))
  this_pop = as.matrix(spain_deaths_subnational_female %>% filter(year == years[j]) %>% ungroup() %>% select(prov, age_group, pop) %>% spread(prov, pop) %>% select(-age_group))
  these_dep = colnames(this_value)
  these_dep_pop = colnames(this_pop)
  for(k in 1:length(these_dep)){
    fem_y.xtas[,j,which(province==these_dep[k]),] = this_value[,k]
  }
  for(l in 1:length(these_dep_pop)){
    fem_pop.xtas[,j,which(province==these_dep_pop[l]),] = this_pop[,l]
  }
}

fem_y.xtas[is.na(y.xtas)] = 0
fem_pop.xtas[is.na(pop.xtas)] = 0


# initialise arrays 
male_y.xtas = array(NA, c(length(age_groups), length(years), length(departments),1))
male_pop.xtas = array(NA, c(length(age_groups), length(years), length(departments),1))

for(j in 1:length(years)){
  
  this_value = as.matrix(spain_deaths_subnational_male %>% filter(year == years[j]) %>% ungroup() %>% select(prov, age_group, deaths) %>% spread(prov, deaths) %>% select(-age_group))
  this_pop = as.matrix(spain_deaths_subnational_male %>% filter(year == years[j]) %>% ungroup() %>% select(prov, age_group, pop) %>% spread(prov, pop) %>% select(-age_group))
  these_dep = colnames(this_value)
  these_dep_pop = colnames(this_pop)
  for(k in 1:length(these_dep)){
    male_y.xtas[,j,which(departments==these_dep[k]),] = this_value[,k]
  }
  for(l in 1:length(these_dep_pop)){
    male_pop.xtas[,j,which(departments==these_dep_pop[l]),] = this_pop[,l]
  }
}

male_y.xtas[is.na(y.xtas)] = 0
male_pop.xtas[is.na(pop.xtas)] = 0



jags_data_spain_total = list(y.xtas = y.xtas, 
                               pop.xtas = pop.xtas, 
                               Yx = Y_total,
                               S = 1, X= length(age_groups), T = length(years), 
                               n.a=length(departments), n.amax=length(departments), P=3 )




jags_data_spain_male = list(y.xtas = male_y.xtas, 
                              pop.xtas = male_pop.xtas, 
                              Yx = Y_men,
                              S = 1, X= length(age_groups), T = length(years), 
                              n.a=length(departments), n.amax=length(departments), P=3 )


jags_data_spain_female = list(y.xtas = fem_y.xtas, 
                                pop.xtas = fem_pop.xtas, 
                                Yx = Y_women,
                                S = 1, X= length(age_groups), 
                                T = length(years), 
                                n.a=length(departments), 
                                n.amax=length(departments), 
                                P=3)



##### Run JAGS models #####

parnames = c("beta.tas", "mu.beta" ,"sigma.beta", "tau.mu", "u.xtas", "mx.xtas")


spain_mod_total = jags(data = jags_data_spain_total, 
                         parameters.to.save=parnames, 
                         n.iter = 60000,
                         model.file = "mortality_model_jags/model.txt")


spain_mod_total_array = spain_mod_total$BUGSoutput$sims.array

spain_mod_male = jags(data = jags_data_spain_male, 
                        parameters.to.save=parnames, 
                        n.iter = 60000,
                        model.file = "mortality_model_jags/model.txt")


spain_mod_male_array = spain_mod_male$BUGSoutput$sims.array

spain_mod_female = jags(data = jags_data_spain_female, 
                          parameters.to.save=parnames, 
                          n.iter = 60000,
                          model.file = "mortality_model_jags/model.txt")


spain_mod_female_array = spain_mod_female$BUGSoutput$sims.array


spain_mod_total_array = mcmc.array_total_spain
res_Lx_total_spain = c()

ages = c(0,1,seq(5,85,5))

for (c in seq_along(departments)) {
  for (i in seq_along(years)) {
    mx_draws = sapply(seq_along(age_groups), function(j) {
      spain_mod_total_array[, , paste0("mx.xtas[", j, ",", i, ",", c, ",1]")]
    })
    
    
    nLx_draws = apply(mx_draws, 1, derive_nLx_values, ages = ages)
    
    res_Lx_total_spain = rbind(res_Lx_total_spain,tibble(
      age = age_groups,
      department = departments[c],
      year = years[i],
      median = apply(nLx_draws, 1, median),
      se = apply(nLx_draws, 1, sd),
      lower = apply(nLx_draws, 1, quantile, probs = 0.025),
      upper = apply(nLx_draws, 1, quantile, probs = 0.975))
    )
    
  }
}


res_Lx_male_spain = c()


for (c in seq_along(departments)) {
  for (i in seq_along(years)) {
    mx_draws = sapply(seq_along(age_groups), function(j) {
      spain_mod_male_array[, , paste0("mx.xtas[", j, ",", i, ",", c, ",1]")]
    })
    
    
    nLx_draws = apply(mx_draws, 1, derive_nLx_values, ages = ages)
    
    res_Lx_male_spain = rbind(res_Lx_male_spain,tibble(
      age = age_groups,
      county = counties[c],
      year = years[i],
      median = apply(nLx_draws, 1, median),
      se = apply(nLx_draws, 1, sd),
      lower = apply(nLx_draws, 1, quantile, probs = 0.025),
      upper = apply(nLx_draws, 1, quantile, probs = 0.975))
    )
    
  }
}




res_Lx_female_spain = c()


for (c in seq_along(departments)) {
  for (i in seq_along(years)) {
    mx_draws = sapply(seq_along(age_groups), function(j) {
      spain_mod_female_array[, , paste0("mx.xtas[", j, ",", i, ",", c, ",1]")]
    })
    
    
    nLx_draws = apply(mx_draws, 1, derive_nLx_values, ages = ages)
    
    res_Lx_female_spain = rbind(res_Lx_female_spain,tibble(
      age = age_groups,
      county = counties[c],
      year = years[i],
      median = apply(nLx_draws, 1, median),
      se = apply(nLx_draws, 1, sd),
      lower = apply(nLx_draws, 1, quantile, probs = 0.025),
      upper = apply(nLx_draws, 1, quantile, probs = 0.975))
    )
    
  }
}


save(res_Lx_female_spain,
     res_Lx_male_spain,
     res_Lx_total_spain,
     file="data/spain/mortality/res_Lx_spain.RData")



#### Bangladesh ####


survey_years = c(2004,2007,2011,2014,2017)


data.dir = c("bangladesh/dhs/2004/",
             "bangladesh/dhs/2007/",
             "bangladesh/dhs/2011/",
             "bangladesh/dhs/2014/",
             "bangladesh/dhs/2017/")
##### 2004 #####

data_temp = read_dta(paste0(data.dir[1],"/BDBR4JDT/BD",survey_years[1],".dta"))


raw.dat.tmp = data_temp
alive = attr(raw.dat.tmp$b5, which = 'labels')
names(alive) = tolower(names(alive))
raw.dat.tmp$b5 = ifelse(raw.dat.tmp$b5 == alive['yes'][[1]], 'yes', 'no')
raw.dat.tmp$b5 = factor(raw.dat.tmp$b5, levels = c('yes', 'no'))
strat = attr(raw.dat.tmp$v025,which='labels')
names(strat) = tolower(names(strat))
raw.dat.tmp$v025 = ifelse(raw.dat.tmp$v025 == strat['urban'][[1]],'urban', 'rural')
raw.dat.tmp$v025 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp$v022 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp = raw.dat.tmp %>%
  mutate(
    admin1 = as.character(v024),
    admin1 = str_remove(admin1, "\\s+(urban|rural)$"),   # remove urban/rural at end
    admin1 = str_remove(admin1, "^(nc|ne|nw|se|ss|sw)\\s+"),  # remove zone prefix at start
    admin1 = str_to_title(admin1)
  )


dat.tmp = SUMMER::getBirths(data = raw.dat.tmp,
                             surveyyear = 2004,
                             year.cut = seq(2000, 2004+1, 1),
                             cmc.adjust = 0)

dat.tmp$admin1.char = as.character(as_factor(dat.tmp$v024))
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char, "\\s+(urban|rural)$")
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char,  "^(nc|ne|nw|se|ss|sw)\\s+")
dat.tmp$admin1.char = str_to_title(dat.tmp$admin1.char)  




dat.tmp = dat.tmp[,c('v001', 'v005', 'v023','v024', 'v025', 'strata', 'age', 'time', 'died','dob', 'admin1.char')] %>%
  dplyr:::mutate(dob = floor((dob-1)/12)+1900) %>%
  dplyr::summarise(died = sum(died),
                   total = n(),
                   .by = c('v001', 'v005','v023', 'v024', 'v025', 'strata', 'age', 'time', 'dob','admin1.char'))


# finish preparing data ###
dat.tmp$survey = 2004
dat.tmp = dat.tmp[,c('v001','survey', 'age', 'time','dob', 'total', 'died', 'v005', 'v025','strata',
                      'v023', 'admin1.char')]


colnames(dat.tmp) = c('cluster','survey','age', 'years','cohort', 'total', 'Y', 'v005', 'urban','strata',
                       'v023', 'admin1.char')



births.2004 = 
  dat.tmp %>% 
  dplyr::filter(survey == 2004, years %in% 2000:2004) %>% 
  dplyr::mutate(years = years %>% as.character() %>% as.numeric(),
                v005 = v005/1e6,
                died = Y,
                total = total %>% as.numeric())


##### 2007 #####



data_temp = read_dta(paste0(data.dir[2],"/BDBR51DT/BD",survey_years[2],".dta"))


raw.dat.tmp = data_temp
alive = attr(raw.dat.tmp$b5, which = 'labels')
names(alive) = tolower(names(alive))
raw.dat.tmp$b5 = ifelse(raw.dat.tmp$b5 == alive['yes'][[1]], 'yes', 'no')
raw.dat.tmp$b5 = factor(raw.dat.tmp$b5, levels = c('yes', 'no'))
strat = attr(raw.dat.tmp$v025,which='labels')
names(strat) = tolower(names(strat))
raw.dat.tmp$v025 = ifelse(raw.dat.tmp$v025 == strat['urban'][[1]],'urban', 'rural')
raw.dat.tmp$v025 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp$v022 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp = raw.dat.tmp %>%
  mutate(
    admin1 = as.character(v024),
    admin1 = str_remove(admin1, "\\s+(urban|rural)$"),   # remove urban/rural at end
    admin1 = str_remove(admin1, "^(nc|ne|nw|se|ss|sw)\\s+"),  # remove zone prefix at start
    admin1 = str_to_title(admin1)
  )


dat.tmp = SUMMER::getBirths(data = raw.dat.tmp,
                             surveyyear = 2007,
                             year.cut = seq(2004, 2007+1, 1),
                             cmc.adjust = 0)

dat.tmp$admin1.char = as.character(as_factor(dat.tmp$v024))
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char, "\\s+(urban|rural)$")
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char,  "^(nc|ne|nw|se|ss|sw)\\s+")
dat.tmp$admin1.char = str_to_title(dat.tmp$admin1.char)  




dat.tmp = dat.tmp[,c('v001', 'v005', 'v023','v024', 'v025', 'strata', 'age', 'time', 'died','dob', 'admin1.char')] %>%
  dplyr:::mutate(dob = floor((dob-1)/12)+1900) %>%
  dplyr::summarise(died = sum(died),
                   total = n(),
                   .by = c('v001', 'v005','v023', 'v024', 'v025', 'strata', 'age', 'time', 'dob','admin1.char'))


# finish preparing data ###
dat.tmp$survey = 2007
dat.tmp = dat.tmp[,c('v001','survey', 'age', 'time','dob', 'total', 'died', 'v005', 'v025','strata',
                      'v023', 'admin1.char')]


colnames(dat.tmp) = c('cluster','survey','age', 'years','cohort', 'total', 'Y', 'v005', 'urban','strata',
                       'v023', 'admin1.char')



births.2007 = 
  dat.tmp %>% 
  dplyr::filter(survey == 2007, years %in% 2005:2007) %>% 
  dplyr::mutate(years = years %>% as.character() %>% as.numeric(),
                v005 = v005/1e6,
                died = Y,
                total = total %>% as.numeric())




##### 2011 #####

data_temp = read_dta(paste0(data.dir[3],"/BDBR61DT/BD",survey_years[3],".dta"))


raw.dat.tmp = data_temp
alive = attr(raw.dat.tmp$b5, which = 'labels')
names(alive) = tolower(names(alive))
raw.dat.tmp$b5 = ifelse(raw.dat.tmp$b5 == alive['yes'][[1]], 'yes', 'no')
raw.dat.tmp$b5 = factor(raw.dat.tmp$b5, levels = c('yes', 'no'))
strat = attr(raw.dat.tmp$v025,which='labels')
names(strat) = tolower(names(strat))
raw.dat.tmp$v025 = ifelse(raw.dat.tmp$v025 == strat['urban'][[1]],'urban', 'rural')
raw.dat.tmp$v025 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp$v022 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp = raw.dat.tmp %>%
  mutate(
    admin1 = as.character(v024),
    admin1 = str_remove(admin1, "\\s+(urban|rural)$"),   # remove urban/rural at end
    admin1 = str_remove(admin1, "^(nc|ne|nw|se|ss|sw)\\s+"),  # remove zone prefix at start
    admin1 = str_to_title(admin1)
  )


dat.tmp = SUMMER::getBirths(data = raw.dat.tmp,
                             surveyyear = 2011,
                             year.cut = seq(2008, 2011+1, 1),
                             cmc.adjust = 0)

dat.tmp$admin1.char = as.character(as_factor(dat.tmp$v024))
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char, "\\s+(urban|rural)$")
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char,  "^(nc|ne|nw|se|ss|sw)\\s+")
dat.tmp$admin1.char = str_to_title(dat.tmp$admin1.char)  




dat.tmp = dat.tmp[,c('v001', 'v005', 'v023','v024', 'v025', 'strata', 'age', 'time', 'died','dob', 'admin1.char')] %>%
  dplyr:::mutate(dob = floor((dob-1)/12)+1900) %>%
  dplyr::summarise(died = sum(died),
                   total = n(),
                   .by = c('v001', 'v005','v023', 'v024', 'v025', 'strata', 'age', 'time', 'dob','admin1.char'))


# finish preparing data ###
dat.tmp$survey = 2011
dat.tmp = dat.tmp[,c('v001','survey', 'age', 'time','dob', 'total', 'died', 'v005', 'v025','strata',
                      'v023', 'admin1.char')]


colnames(dat.tmp) = c('cluster','survey','age', 'years','cohort', 'total', 'Y', 'v005', 'urban','strata',
                       'v023', 'admin1.char')



births.2011 = 
  dat.tmp %>% 
  dplyr::filter(survey == 2011, years %in% 2008:2011) %>% 
  dplyr::mutate(years = years %>% as.character() %>% as.numeric(),
                v005 = v005/1e6,
                died = Y,
                total = total %>% as.numeric())



##### 2014 #####


data_temp = read_dta(paste0(data.dir[4],"/BDBR72DT/BD",survey_years[4],".dta"))


raw.dat.tmp = data_temp
alive = attr(raw.dat.tmp$b5, which = 'labels')
names(alive) = tolower(names(alive))
raw.dat.tmp$b5 = ifelse(raw.dat.tmp$b5 == alive['yes'][[1]], 'yes', 'no')
raw.dat.tmp$b5 = factor(raw.dat.tmp$b5, levels = c('yes', 'no'))
strat = attr(raw.dat.tmp$v025,which='labels')
names(strat) = tolower(names(strat))
raw.dat.tmp$v025 = ifelse(raw.dat.tmp$v025 == strat['urban'][[1]],'urban', 'rural')
raw.dat.tmp$v025 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp$v022 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp = raw.dat.tmp %>%
  mutate(
    admin1 = as.character(v024),
    admin1 = str_remove(admin1, "\\s+(urban|rural)$"),   # remove urban/rural at end
    admin1 = str_remove(admin1, "^(nc|ne|nw|se|ss|sw)\\s+"),  # remove zone prefix at start
    admin1 = str_to_title(admin1)
  )


dat.tmp = SUMMER::getBirths(data = raw.dat.tmp,
                             surveyyear = 2014,
                             year.cut = seq(2012, 2014+1, 1),
                             cmc.adjust = 0)

dat.tmp$admin1.char = as.character(as_factor(dat.tmp$v024))
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char, "\\s+(urban|rural)$")
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char,  "^(nc|ne|nw|se|ss|sw)\\s+")
dat.tmp$admin1.char = str_to_title(dat.tmp$admin1.char)  




dat.tmp = dat.tmp[,c('v001', 'v005', 'v023','v024', 'v025', 'strata', 'age', 'time', 'died','dob', 'admin1.char')] %>%
  dplyr:::mutate(dob = floor((dob-1)/12)+1900) %>%
  dplyr::summarise(died = sum(died),
                   total = n(),
                   .by = c('v001', 'v005','v023', 'v024', 'v025', 'strata', 'age', 'time', 'dob','admin1.char'))


# finish preparing data ###
dat.tmp$survey = 2014
dat.tmp = dat.tmp[,c('v001','survey', 'age', 'time','dob', 'total', 'died', 'v005', 'v025','strata',
                      'v023', 'admin1.char')]


colnames(dat.tmp) = c('cluster','survey','age', 'years','cohort', 'total', 'Y', 'v005', 'urban','strata',
                       'v023', 'admin1.char')



births.2014 = 
  dat.tmp %>% 
  dplyr::filter(survey == 2014, years %in% 2012:2014) %>% 
  dplyr::mutate(years = years %>% as.character() %>% as.numeric(),
                v005 = v005/1e6,
                died = Y,
                total = total %>% as.numeric())




##### 2017 #####


data_temp = read_dta(paste0(data.dir[5],"/BDBR7RDT/BD",survey_years[5],".dta"))


raw.dat.tmp = data_temp
alive = attr(raw.dat.tmp$b5, which = 'labels')
names(alive) = tolower(names(alive))
raw.dat.tmp$b5 = ifelse(raw.dat.tmp$b5 == alive['yes'][[1]], 'yes', 'no')
raw.dat.tmp$b5 = factor(raw.dat.tmp$b5, levels = c('yes', 'no'))
strat = attr(raw.dat.tmp$v025,which='labels')
names(strat) = tolower(names(strat))
raw.dat.tmp$v025 = ifelse(raw.dat.tmp$v025 == strat['urban'][[1]],'urban', 'rural')
raw.dat.tmp$v025 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp$v022 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp = raw.dat.tmp %>%
  mutate(
    admin1 = as.character(v024),
    admin1 = str_remove(admin1, "\\s+(urban|rural)$"),   # remove urban/rural at end
    admin1 = str_remove(admin1, "^(nc|ne|nw|se|ss|sw)\\s+"),  # remove zone prefix at start
    admin1 = str_to_title(admin1)
  )


dat.tmp = SUMMER::getBirths(data = raw.dat.tmp,
                             surveyyear = 2017,
                             year.cut = seq(2015, 2017+1, 1),
                             cmc.adjust = 0)

dat.tmp$admin1.char = as.character(as_factor(dat.tmp$v024))
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char, "\\s+(urban|rural)$")
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char,  "^(nc|ne|nw|se|ss|sw)\\s+")
dat.tmp$admin1.char = str_to_title(dat.tmp$admin1.char)  




dat.tmp = dat.tmp[,c('v001', 'v005', 'v023','v024', 'v025', 'strata', 'age', 'time', 'died','dob', 'admin1.char')] %>%
  dplyr:::mutate(dob = floor((dob-1)/12)+1900) %>%
  dplyr::summarise(died = sum(died),
                   total = n(),
                   .by = c('v001', 'v005','v023', 'v024', 'v025', 'strata', 'age', 'time', 'dob','admin1.char'))


# finish preparing data ###
dat.tmp$survey = 2017
dat.tmp = dat.tmp[,c('v001','survey', 'age', 'time','dob', 'total', 'died', 'v005', 'v025','strata',
                      'v023', 'admin1.char')]


colnames(dat.tmp) = c('cluster','survey','age', 'years','cohort', 'total', 'Y', 'v005', 'urban','strata',
                       'v023', 'admin1.char')



births.2017 = 
  dat.tmp %>% 
  dplyr::filter(survey == 2017, years %in% 2015:2017) %>% 
  dplyr::mutate(years = years %>% as.character() %>% as.numeric(),
                v005 = v005/1e6,
                died = Y,
                total = total %>% as.numeric())




#### combine data ####

births = rbind(births.2004,births.2007,births.2011, haven::zap_labels(births.2014),
               haven::zap_labels(births.2017))

#### Make sure regions' name are uniform ####




beg.year = 2000
end.year = 2020


direct.admin1.yearly.u5 =  
  SUMMER::getDirect(births = births, 
                    years = beg.year:end.year,
                    regionVar = 'admin1.char',
                    timeVar = 'years', 
                    clusterVar =  '~cluster',
                    ageVar = 'age', 
                    Ntrials = 'total',
                    weightsVar = 'v005',
                    national.only = F)




# 4) Build neighbor list (queen contiguity; use rook=TRUE if you prefer)

load("data/bangladesh/geo/bangladesh_adm1.RData")

# Create a spatial matrix

nb_bangladesh = poly2nb(as_Spatial(shape_file_bangladesh_admin1), queen = TRUE)

A = nb2mat(nb_bangladesh, style = "B", zero.policy = TRUE)

# 5) Attach names exactly as expected by your function
rownames(A) = colnames(A) = shape_file_bangladesh_admin1$region

# 
# Smoothed direct estimates  ------------------------------------------------------

# time model
time.model = c('rw2','ar1')[1]
direct.admin1.yearly.u5 = 
  direct.admin1.yearly.u5 %>% 
  dplyr::filter(years %in% beg.year:end.year)

direct.admin1.yearly.u5[is.na(direct.admin1.yearly.u5)] = 0.01


fit.admin1.yearly.u5 = SUMMER::smoothDirect(data = data.admin1.yearly.u5, 
                                             Amat = A, 
                                             time.model = time.model,
                                             year_label = as.character(beg.year:end.year),
                                             type.st = 4,
                                             year_range = c(beg.year, end.year), 
                                             is.yearly = F)

smooth_child_mort_est_bangladesh = SUMMER::getSmoothed(inla_mod = fit.admin1.yearly.u5,
                                           Amat = A,
                                           year_label = as.character(beg.year:end.year),
                                           year_range = c(beg.year, end.year),
                                           save.draws = TRUE)





#### Wilmoth Constants ####


wilmoth = 
  read.csv(text = '
           age,am,bm,cm,vm,af,bf,cf,vf
           0,  -0.5101, 0.8164,-0.0245,     0,-0.6619, 0.7684,-0.0277,     0
           1,      -99,    -99,    -99,   -99,    -99,    -99,    -99,   -99
           5,  -3.0435, 1.5270, 0.0817,0.1720,-2.5608, 1.7937, 0.1082,0.2788
           10, -3.9554, 1.2390, 0.0638,0.1683,-3.2435, 1.6653, 0.1088,0.3423
           15, -3.9374, 1.0425, 0.0750,0.2161,-3.1099, 1.5797, 0.1147,0.4007
           20, -3.4165, 1.1651, 0.0945,0.3022,-2.9789, 1.5053, 0.1011,0.4133
           25, -3.4237, 1.1444, 0.0905,0.3624,-3.0185, 1.3729, 0.0815,0.3884
           30, -3.4438, 1.0682, 0.0814,0.3848,-3.0201, 1.2879, 0.0778,0.3391
           35, -3.4198, 0.9620, 0.0714,0.3779,-3.1487, 1.1071, 0.0637,0.2829
           40, -3.3829, 0.8337, 0.0609,0.3530,-3.2690, 0.9339, 0.0533,0.2246
           45, -3.4456, 0.6039, 0.0362,0.3060,-3.5202, 0.6642, 0.0289,0.1774
           50, -3.4217, 0.4001, 0.0138,0.2564,-3.4076, 0.5556, 0.0208,0.1429
           55, -3.4144, 0.1760,-0.0128,0.2017,-3.2587, 0.4461, 0.0101,0.1190
           60, -3.1402, 0.0921,-0.0216,0.1616,-2.8907, 0.3988, 0.0042,0.0807
           65, -2.8565, 0.0217,-0.0283,0.1216,-2.6608, 0.2591,-0.0135,0.0571
           70, -2.4114, 0.0388,-0.0235,0.0864,-2.2949, 0.1759,-0.0229,0.0295
           75, -2.0411, 0.0093,-0.0252,0.0537,-2.0414, 0.0481,-0.0354,0.0114
           80, -1.6456, 0.0085,-0.0221,0.0316,-1.7308,-0.0064,-0.0347,0.0033
           85, -1.3203,-0.0183,-0.0219,0.0061,-1.4473,-0.0531,-0.0327,0.0040
           90, -1.0368,-0.0314,-0.0184,     0,-1.1582,-0.0617,-0.0259,     0
           95, -0.7310,-0.0170,-0.0133,     0,-0.8655,-0.0598,-0.0198,     0
           100,-0.5024,-0.0081,-0.0086,     0,-0.6294,-0.0513,-0.0134,     0
           105,-0.3275,-0.0001,-0.0048,     0,-0.4282,-0.0341,-0.0075,     0
           110,-0.2212,-0.0028,-0.0027,     0,-0.2966,-0.0229,-0.0041,     0
           ')

af = wilmoth$af[1:11]  # keep age 0,1,...45 
bf = wilmoth$bf[1:11]  # keep age 0,1,...45 
cf = wilmoth$cf[1:11]  # keep age 0,1,...45 
vf = wilmoth$vf[1:11]  # keep age 0,1,...45 


am = wilmoth$am[1:16]  # keep age 0,1,...70 
bm = wilmoth$bm[1:16]  # keep age 0,1,...70 
cm = wilmoth$cm[1:16]  # keep age 0,1,...70 
vm = wilmoth$vm[1:16]  # keep age 0,1,...70



#### Female Survival ####
set.seed(1)



data_mort_female = mapply(mort_par,
                          smooth_child_mort_est_bangladesh$lower,
                          smooth_child_mort_est_bangladesh$upper)


N = nrow(smooth_child_mort_est_bangladesh)
smooth_child_mort_est_bangladesh$q5_a = data_mort_female[1,]
smooth_child_mort_est_bangladesh$q5_b = data_mort_female[2,]




# Compute Lx

n = nrow(smooth_child_mort_est_bangladesh)
B = 1000   # number of bootstrap replicates


library(future.apply)

plan(multisession, workers = 4)

n = nrow(smooth_child_mort_est_bangladesh)
B = 1000

one_boot = function(b) {
  # draw uncertain inputs
  qx = rbeta(n, smooth_child_mort_est_bangladesh$q5_a, smooth_child_mort_est_bangladesh$q5_b)
  h  = log(qx)
  k  = rnorm(n, 0, 0.01)
  
  # mx: n x 11
  mx1 = exp(af[1] + bf[1] * h + cf[1] * h^2 + vf[1] * k)
  mx2 = -0.25 * (mx1 + log(1 - qx))
  mx_rest = exp(
    matrix(af[3:11], n, 9, byrow = TRUE) +
      outer(h,   bf[3:11]) +
      outer(h^2, cf[3:11]) +
      outer(k,   vf[3:11])
  )
  mx = cbind(mx1, mx2, mx_rest)
  
  # lx: n x 12
  lx = matrix(NA_real_, n, 12)
  lx[, 1] = 1
  lx[, 2] = exp(-mx[, 1])
  lx[, 3] = lx[, 2] * exp(-4 * mx[, 2])
  lx[, 4:12] = lx[, 3] * exp(t(apply(-5 * mx[, 3:11], 1, cumsum)))
  
  # Lx: n x 10
  Lx = matrix(NA_real_, n, 10)
  Lx[, 1] = 0.5 * (lx[, 1] + lx[, 2]) + 2 * (lx[, 2] + lx[, 3])
  for (z in 2:10) {
    Lx[, z] = 2.5 * (lx[, z + 1] + lx[, z + 2])
  }
  Lx
}

# run replicates in parallel, with reproducible parallel-safe RNG streams
set.seed(123)
res = future_lapply(seq_len(B), one_boot, future.seed = TRUE)

# storage: [observation, age interval, bootstrap replicate]
Lx_boot = array(unlist(res), dim = c(n, 10, B))

plan(sequential)  # release workers

# summaries
Lx_hat = apply(Lx_boot, c(1, 2), mean)
Lx_se  = apply(Lx_boot, c(1, 2), sd)
Lx_lo  = apply(Lx_boot, c(1, 2), quantile, probs = 0.025)
Lx_hi  = apply(Lx_boot, c(1, 2), quantile, probs = 0.975)



age_groups = seq(0,45,5)
n = nrow(Lx_hat)

Lx_df = data.frame(
  obs       = rep(seq_len(n), times = 10),
  age_group = factor(rep(age_groups, each = n), levels = age_groups),
  Lx        = as.vector(Lx_hat),
  se        = as.vector(Lx_se),
  lo        = as.vector(Lx_lo),
  hi        = as.vector(Lx_hi)
)

# optional: attach identifiers (district, year, ...) from the input data
ids = smooth_child_mort_est_bangladesh[, c("region", "years")]  # adjust column names
ids$obs = seq_len(n)
Lx_df = merge(ids, Lx_df, by = "obs")

Lx_hat_female_bangladesh =  Lx_df %>%
  select(age_group,Lx,region,years) %>%
  pivot_wider(names_from = "age_group",values_from = "Lx")

past0(Lx_hat_female_bangladesh)[1:10] = paste0("L",seq(0,45,5))

Lx_se_female_bangladesh = Lx_df %>%
  select(age_group,se,region,years) %>%
  pivot_wider(names_from = "age_group",values_from = "se")



#### Male Survival ####
set.seed(1)



data_mort_female = mapply(mort_par,
                          smooth_child_mort_est_bangladesh$lower,
                          smooth_child_mort_est_bangladesh$upper)


N = nrow(smooth_child_mort_est_bangladesh)
smooth_child_mort_est_bangladesh$q5_a = data_mort_female[1,]
smooth_child_mort_est_bangladesh$q5_b = data_mort_female[2,]




# Compute Lx

n = nrow(smooth_child_mort_est_bangladesh)
B = 1000   # number of bootstrap replicates


library(future.apply)

plan(multisession, workers = 4)

n = nrow(smooth_child_mort_est_bangladesh)
B = 1000

one_boot_male = function(b) {
  # draw uncertain inputs
  qx = rbeta(n, smooth_child_mort_est_bangladesh$q5_a, smooth_child_mort_est_bangladesh$q5_b)
  h  = log(qx)
  k  = rnorm(n, 0, 0.01)
  
  # mx: n x 16
  mx1 = exp(am[1] + bm[1] * h + cm[1] * h^2 + vm[1] * k)
  mx2 = -0.25 * (mx1 + log(1 - qx))
  mx_rest = exp(
    matrix(am[3:16], n, 9, byrow = TRUE) +
      outer(h,   bm[3:16]) +
      outer(h^2, cm[3:16]) +
      outer(k,   vm[3:16])
  )
  mx = cbind(mx1, mx2, mx_rest)
  
  # lx: n x 17
  lx = matrix(NA_real_, n, 17)
  lx[, 1] = 1
  lx[, 2] = exp(-mx[, 1])
  lx[, 3] = lx[, 2] * exp(-4 * mx[, 2])
  lx[, 4:12] = lx[, 3] * exp(t(apply(-5 * mx[, 3:11], 1, cumsum)))
  
  # Lx: n x 15
  Lx = matrix(NA_real_, n, 15)
  Lx[, 1] = 0.5 * (lx[, 1] + lx[, 2]) + 2 * (lx[, 2] + lx[, 3])
  for (z in 2:10) {
    Lx[, z] = 2.5 * (lx[, z + 1] + lx[, z + 2])
  }
  Lx
}

# run replicates in parallel, with reproducible parallel-safe RNG streams
set.seed(123)
res = future_lapply(seq_len(B), one_boot_male, future.seed = TRUE)

# storage: [observation, age interval, bootstrap replicate]
Lx_boot = array(unlist(res), dim = c(n, 10, B))

plan(sequential)  # release workers

# summaries
Lx_hat = apply(Lx_boot, c(1, 2), mean)
Lx_se  = apply(Lx_boot, c(1, 2), sd)
Lx_lo  = apply(Lx_boot, c(1, 2), quantile, probs = 0.025)
Lx_hi  = apply(Lx_boot, c(1, 2), quantile, probs = 0.975)



age_groups = seq(0,70,5)
n = nrow(Lx_hat)

Lx_df = data.frame(
  obs       = rep(seq_len(n), times = 15),
  age_group = factor(rep(age_groups, each = n), levels = age_groups),
  Lx        = as.vector(Lx_hat),
  se        = as.vector(Lx_se),
  lo        = as.vector(Lx_lo),
  hi        = as.vector(Lx_hi)
)

# optional: attach identifiers (district, year, ...) from the input data
ids = smooth_child_mort_est_bangladesh[, c("region", "years")]  # adjust column names
ids$obs = seq_len(n)
Lx_df = merge(ids, Lx_df, by = "obs")

Lx_hat_male_bangladesh =  Lx_df %>%
  select(age_group,Lx,region,years) %>%
  pivot_wider(names_from = "age_group",values_from = "Lx")

past0(Lx_hat_male_bangladesh)[1:15] = paste0("L",seq(0,70,5))

Lx_se_male_bangladesh = Lx_df %>%
  select(age_group,se,region,years) %>%
  pivot_wider(names_from = "age_group",values_from = "se")

past0(Lx_se_female_bangladesh)[1:15] = paste0("se_L",seq(0,70,5))


save(Lx_hat_female_bangladesh,
     Lx_se_female_bangladesh,
     Lx_hat_male_bangladesh,
     Lx_se_male_bangladesh,
     file="data/bangladesh/mortality/bangladesh_Lx_estimates.RData")
 

#### Pakistan ####


survey_years = c(2006,2012,2017)

data.dir = c("pakistan/dhs/2006/",
             "pakistan/dhs/2012/",
             "pakistan/dhs/2017/")

##### 2004 #####

data_temp = read_dta(paste0(data.dir[1],"/PKBR53DT/PK",survey_years[1],".dta"))


raw.dat.tmp = data_temp
alive = attr(raw.dat.tmp$b5, which = 'labels')
names(alive) = tolower(names(alive))
raw.dat.tmp$b5 = ifelse(raw.dat.tmp$b5 == alive['yes'][[1]], 'yes', 'no')
raw.dat.tmp$b5 = factor(raw.dat.tmp$b5, levels = c('yes', 'no'))
strat = attr(raw.dat.tmp$v025,which='labels')
names(strat) = tolower(names(strat))
raw.dat.tmp$v025 = ifelse(raw.dat.tmp$v025 == strat['urban'][[1]],'urban', 'rural')
raw.dat.tmp$v025 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp$v022 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp = raw.dat.tmp %>%
  mutate(
    admin1 = as.character(v024),
    admin1 = str_remove(admin1, "\\s+(urban|rural)$"),   # remove urban/rural at end
    admin1 = str_remove(admin1, "^(nc|ne|nw|se|ss|sw)\\s+"),  # remove zone prefix at start
    admin1 = str_to_title(admin1)
  )


dat.tmp = SUMMER::getBirths(data = raw.dat.tmp,
                             surveyyear = 2006,
                             year.cut = seq(2000, 2006+1, 1),
                             cmc.adjust = 0)

dat.tmp$admin1.char = as.character(as_factor(dat.tmp$v024))
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char, "\\s+(urban|rural)$")
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char,  "^(nc|ne|nw|se|ss|sw)\\s+")
dat.tmp$admin1.char = str_to_title(dat.tmp$admin1.char)  




dat.tmp = dat.tmp[,c('v001', 'v005', 'v023','v024', 'v025', 'strata', 'age', 'time', 'died','dob', 'admin1.char')] %>%
  dplyr:::mutate(dob = floor((dob-1)/12)+1900) %>%
  dplyr::summarise(died = sum(died),
                   total = n(),
                   .by = c('v001', 'v005','v023', 'v024', 'v025', 'strata', 'age', 'time', 'dob','admin1.char'))


# finish preparing data ###
dat.tmp$survey = 2006
dat.tmp = dat.tmp[,c('v001','survey', 'age', 'time','dob', 'total', 'died', 'v005', 'v025','strata',
                      'v023', 'admin1.char')]


colnames(dat.tmp) = c('cluster','survey','age', 'years','cohort', 'total', 'Y', 'v005', 'urban','strata',
                       'v023', 'admin1.char')



births.2006 = 
  dat.tmp %>% 
  dplyr::filter(survey == 2006, years %in% 2000:2006) %>% 
  dplyr::mutate(years = years %>% as.character() %>% as.numeric(),
                v005 = v005/1e6,
                died = Y,
                total = total %>% as.numeric())




##### 2012 #####



data_temp = read_dta(paste0(data.dir[2],"/PKBR61DT/PK",survey_years[2],".dta"))


raw.dat.tmp = data_temp
alive = attr(raw.dat.tmp$b5, which = 'labels')
names(alive) = tolower(names(alive))
raw.dat.tmp$b5 = ifelse(raw.dat.tmp$b5 == alive['yes'][[1]], 'yes', 'no')
raw.dat.tmp$b5 = factor(raw.dat.tmp$b5, levels = c('yes', 'no'))
strat = attr(raw.dat.tmp$v025,which='labels')
names(strat) = tolower(names(strat))
raw.dat.tmp$v025 = ifelse(raw.dat.tmp$v025 == strat['urban'][[1]],'urban', 'rural')
raw.dat.tmp$v025 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp$v022 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp = raw.dat.tmp %>%
  mutate(
    admin1 = as.character(v024),
    admin1 = str_remove(admin1, "\\s+(urban|rural)$"),   # remove urban/rural at end
    admin1 = str_remove(admin1, "^(nc|ne|nw|se|ss|sw)\\s+"),  # remove zone prefix at start
    admin1 = str_to_title(admin1)
  )


dat.tmp = SUMMER::getBirths(data = raw.dat.tmp,
                             surveyyear = 2012,
                             year.cut = seq(2007, 2012+1, 1),
                             cmc.adjust = 0)

dat.tmp$admin1.char = as.character(as_factor(dat.tmp$v024))
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char, "\\s+(urban|rural)$")
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char,  "^(nc|ne|nw|se|ss|sw)\\s+")
dat.tmp$admin1.char = str_to_title(dat.tmp$admin1.char)  




dat.tmp = dat.tmp[,c('v001', 'v005', 'v023','v024', 'v025', 'strata', 'age', 'time', 'died','dob', 'admin1.char')] %>%
  dplyr:::mutate(dob = floor((dob-1)/12)+1900) %>%
  dplyr::summarise(died = sum(died),
                   total = n(),
                   .by = c('v001', 'v005','v023', 'v024', 'v025', 'strata', 'age', 'time', 'dob','admin1.char'))


# finish preparing data ###
dat.tmp$survey = 2012
dat.tmp = dat.tmp[,c('v001','survey', 'age', 'time','dob', 'total', 'died', 'v005', 'v025','strata',
                      'v023', 'admin1.char')]


colnames(dat.tmp) = c('cluster','survey','age', 'years','cohort', 'total', 'Y', 'v005', 'urban','strata',
                       'v023', 'admin1.char')



births.2012 = 
  dat.tmp %>% 
  dplyr::filter(survey == 2012, years %in% 2007:2012) %>% 
  dplyr::mutate(years = years %>% as.character() %>% as.numeric(),
                v005 = v005/1e6,
                died = Y,
                total = total %>% as.numeric())





##### 2018 #####


##### 2012 #####



data_temp = read_dta(paste0(data.dir[2],"/PKBR61DT/PK",survey_years[2],".dta"))


raw.dat.tmp = data_temp
alive = attr(raw.dat.tmp$b5, which = 'labels')
names(alive) = tolower(names(alive))
raw.dat.tmp$b5 = ifelse(raw.dat.tmp$b5 == alive['yes'][[1]], 'yes', 'no')
raw.dat.tmp$b5 = factor(raw.dat.tmp$b5, levels = c('yes', 'no'))
strat = attr(raw.dat.tmp$v025,which='labels')
names(strat) = tolower(names(strat))
raw.dat.tmp$v025 = ifelse(raw.dat.tmp$v025 == strat['urban'][[1]],'urban', 'rural')
raw.dat.tmp$v025 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp$v022 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp = raw.dat.tmp %>%
  mutate(
    admin1 = as.character(v024),
    admin1 = str_remove(admin1, "\\s+(urban|rural)$"),   # remove urban/rural at end
    admin1 = str_remove(admin1, "^(nc|ne|nw|se|ss|sw)\\s+"),  # remove zone prefix at start
    admin1 = str_to_title(admin1)
  )


dat.tmp = SUMMER::getBirths(data = raw.dat.tmp,
                             surveyyear = 2012,
                             year.cut = seq(2007, 2012+1, 1),
                             cmc.adjust = 0)

dat.tmp$admin1.char = as.character(as_factor(dat.tmp$v024))
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char, "\\s+(urban|rural)$")
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char,  "^(nc|ne|nw|se|ss|sw)\\s+")
dat.tmp$admin1.char = str_to_title(dat.tmp$admin1.char)  




dat.tmp = dat.tmp[,c('v001', 'v005', 'v023','v024', 'v025', 'strata', 'age', 'time', 'died','dob', 'admin1.char')] %>%
  dplyr:::mutate(dob = floor((dob-1)/12)+1900) %>%
  dplyr::summarise(died = sum(died),
                   total = n(),
                   .by = c('v001', 'v005','v023', 'v024', 'v025', 'strata', 'age', 'time', 'dob','admin1.char'))


# finish preparing data ###
dat.tmp$survey = 2012
dat.tmp = dat.tmp[,c('v001','survey', 'age', 'time','dob', 'total', 'died', 'v005', 'v025','strata',
                      'v023', 'admin1.char')]


colnames(dat.tmp) = c('cluster','survey','age', 'years','cohort', 'total', 'Y', 'v005', 'urban','strata',
                       'v023', 'admin1.char')



births.2012 = 
  dat.tmp %>% 
  dplyr::filter(survey == 2012, years %in% 2007:2012) %>% 
  dplyr::mutate(years = years %>% as.character() %>% as.numeric(),
                v005 = v005/1e6,
                died = Y,
                total = total %>% as.numeric())




##### 2017 #####

data_temp = read_dta(paste0(data.dir[3],"/PKBR71DT/PK",survey_years[3],".dta"))


raw.dat.tmp = data_temp
alive = attr(raw.dat.tmp$b5, which = 'labels')
names(alive) = tolower(names(alive))
raw.dat.tmp$b5 = ifelse(raw.dat.tmp$b5 == alive['yes'][[1]], 'yes', 'no')
raw.dat.tmp$b5 = factor(raw.dat.tmp$b5, levels = c('yes', 'no'))
strat = attr(raw.dat.tmp$v025,which='labels')
names(strat) = tolower(names(strat))
raw.dat.tmp$v025 = ifelse(raw.dat.tmp$v025 == strat['urban'][[1]],'urban', 'rural')
raw.dat.tmp$v025 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp$v022 = factor(raw.dat.tmp$v025, levels = c('urban', 'rural'))
raw.dat.tmp = raw.dat.tmp %>%
  mutate(
    admin1 = as.character(v024),
    admin1 = str_remove(admin1, "\\s+(urban|rural)$"),   # remove urban/rural at end
    admin1 = str_remove(admin1, "^(nc|ne|nw|se|ss|sw)\\s+"),  # remove zone prefix at start
    admin1 = str_to_title(admin1)
  )


dat.tmp = SUMMER::getBirths(data = raw.dat.tmp,
                             surveyyear = 2018,
                             year.cut = seq(2013, 2018+1, 1),
                             cmc.adjust = 0)

dat.tmp$admin1.char = as.character(as_factor(dat.tmp$v024))
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char, "\\s+(urban|rural)$")
dat.tmp$admin1.char = str_remove(dat.tmp$admin1.char,  "^(nc|ne|nw|se|ss|sw)\\s+")
dat.tmp$admin1.char = str_to_title(dat.tmp$admin1.char)  




dat.tmp = dat.tmp[,c('v001', 'v005', 'v023','v024', 'v025', 'strata', 'age', 'time', 'died','dob', 'admin1.char')] %>%
  dplyr:::mutate(dob = floor((dob-1)/12)+1900) %>%
  dplyr::summarise(died = sum(died),
                   total = n(),
                   .by = c('v001', 'v005','v023', 'v024', 'v025', 'strata', 'age', 'time', 'dob','admin1.char'))


# finish preparing data ###
dat.tmp$survey = 2018
dat.tmp = dat.tmp[,c('v001','survey', 'age', 'time','dob', 'total', 'died', 'v005', 'v025','strata',
                      'v023', 'admin1.char')]


colnames(dat.tmp) = c('cluster','survey','age', 'years','cohort', 'total', 'Y', 'v005', 'urban','strata',
                       'v023', 'admin1.char')



births.2018 = 
  dat.tmp %>% 
  dplyr::filter(survey == 2018, years %in% 2013:2018) %>% 
  dplyr::mutate(years = years %>% as.character() %>% as.numeric(),
                v005 = v005/1e6,
                died = Y,
                total = total %>% as.numeric())




#### combine data ####

births = rbind(births.2006,births.2012,births.2018)

#### Make sure regions' name are uniform ####




beg.year = 2000
end.year = 2020


direct.admin1.yearly.u5 =  
  SUMMER::getDirect(births = births, 
                    years = beg.year:end.year,
                    regionVar = 'admin1.char',
                    timeVar = 'years', 
                    clusterVar =  '~cluster',
                    ageVar = 'age', 
                    Ntrials = 'total',
                    weightsVar = 'v005',
                    national.only = F)




# 4) Build neighbor list (queen contiguity; use rook=TRUE if you prefer)
load("data/pakistan/geo/pakistan_adm1.RData")


# Create a spatial matrix

nb_pakistan = poly2nb(as_Spatial(shape_file_pakistan_admin1), queen = TRUE)


A = nb2mat(nb_pakistan, style = "B", zero.policy = TRUE)

# 5) Attach names exactly as expected by your function
rownames(A) = colnames(A) = shape_file_pakistan_admin1$region


# 
# Smoothed direct estimates  ------------------------------------------------------

# time model
time.model = c('rw2','ar1')[1]
direct.admin1.yearly.u5 = 
  direct.admin1.yearly.u5 %>% 
  dplyr::filter(years %in% beg.year:end.year)

direct.admin1.yearly.u5[is.na(direct.admin1.yearly.u5)] = 0.01


fit.admin1.yearly.u5 = SUMMER::smoothDirect(data = data.admin1.yearly.u5, 
                                             Amat = A, 
                                             time.model = time.model,
                                             year_label = as.character(beg.year:end.year),
                                             type.st = 4,
                                             year_range = c(beg.year, end.year), 
                                             is.yearly = F)

smooth_child_mort_est_pakistan = SUMMER::getSmoothed(inla_mod = fit.admin1.yearly.u5,
                                                        Amat = A,
                                                        year_label = as.character(beg.year:end.year),
                                                        year_range = c(beg.year, end.year),
                                                        save.draws = TRUE)




#### Female Survival ####
set.seed(1)



data_mort_female = mapply(mort_par,
                          smooth_child_mort_est_pakistan$lower,
                          smooth_child_mort_est_pakistan$upper)


N = nrow(smooth_child_mort_est_bangladesh)
smooth_child_mort_est_pakistan$q5_a = data_mort_female[1,]
smooth_child_mort_est_pakistan$q5_b = data_mort_female[2,]




# Compute Lx

n = nrow(smooth_child_mort_est_pakistan)
B = 1000   # number of bootstrap replicates


library(future.apply)

plan(multisession, workers = 4)

n = nrow(smooth_child_mort_est_pakistan)
B = 1000

one_boot = function(b) {
  # draw uncertain inputs
  qx = rbeta(n, smooth_child_mort_est_pakistan$q5_a, smooth_child_mort_est_pakistan$q5_b)
  h  = log(qx)
  k  = rnorm(n, 0, 0.01)
  
  # mx: n x 11
  mx1 = exp(af[1] + bf[1] * h + cf[1] * h^2 + vf[1] * k)
  mx2 = -0.25 * (mx1 + log(1 - qx))
  mx_rest = exp(
    matrix(af[3:11], n, 9, byrow = TRUE) +
      outer(h,   bf[3:11]) +
      outer(h^2, cf[3:11]) +
      outer(k,   vf[3:11])
  )
  mx = cbind(mx1, mx2, mx_rest)
  
  # lx: n x 12
  lx = matrix(NA_real_, n, 12)
  lx[, 1] = 1
  lx[, 2] = exp(-mx[, 1])
  lx[, 3] = lx[, 2] * exp(-4 * mx[, 2])
  lx[, 4:12] = lx[, 3] * exp(t(apply(-5 * mx[, 3:11], 1, cumsum)))
  
  # Lx: n x 10
  Lx = matrix(NA_real_, n, 10)
  Lx[, 1] = 0.5 * (lx[, 1] + lx[, 2]) + 2 * (lx[, 2] + lx[, 3])
  for (z in 2:10) {
    Lx[, z] = 2.5 * (lx[, z + 1] + lx[, z + 2])
  }
  Lx
}

# run replicates in parallel, with reproducible parallel-safe RNG streams
set.seed(123)
res = future_lapply(seq_len(B), one_boot, future.seed = TRUE)

# storage: [observation, age interval, bootstrap replicate]
Lx_boot = array(unlist(res), dim = c(n, 10, B))

plan(sequential)  # release workers

# summaries
Lx_hat = apply(Lx_boot, c(1, 2), mean)
Lx_se  = apply(Lx_boot, c(1, 2), sd)
Lx_lo  = apply(Lx_boot, c(1, 2), quantile, probs = 0.025)
Lx_hi  = apply(Lx_boot, c(1, 2), quantile, probs = 0.975)



age_groups = seq(0,45,5)
n = nrow(Lx_hat)

Lx_df = data.frame(
  obs       = rep(seq_len(n), times = 10),
  age_group = factor(rep(age_groups, each = n), levels = age_groups),
  Lx        = as.vector(Lx_hat),
  se        = as.vector(Lx_se),
  lo        = as.vector(Lx_lo),
  hi        = as.vector(Lx_hi)
)

# optional: attach identifiers (district, year, ...) from the input data
ids = smooth_child_mort_est_pakistan[, c("region", "years")]  # adjust column names
ids$obs = seq_len(n)
Lx_df = merge(ids, Lx_df, by = "obs")

Lx_hat_female_pakistan =  Lx_df %>%
  select(age_group,Lx,region,years) %>%
  pivot_wider(names_from = "age_group",values_from = "Lx")

past0(Lx_hat_female_pakistan)[1:10] = paste0("L",seq(0,45,5))

Lx_se_female_pakistan = Lx_df %>%
  select(age_group,se,region,years) %>%
  pivot_wider(names_from = "age_group",values_from = "se")



#### Male Survival ####
set.seed(1)



data_mort_female = mapply(mort_par,
                          smooth_child_mort_est_pakistan$lower,
                          smooth_child_mort_est_pakistan$upper)


N = nrow(smooth_child_mort_est_pakistan)
smooth_child_mort_est_pakistan$q5_a = data_mort_female[1,]
smooth_child_mort_est_pakistan$q5_b = data_mort_female[2,]




# Compute Lx

n = nrow(smooth_child_mort_est_pakistan)
B = 1000   # number of bootstrap replicates


library(future.apply)

plan(multisession, workers = 4)

n = nrow(smooth_child_mort_est_pakistan)
B = 1000

one_boot_male = function(b) {
  # draw uncertain inputs
  qx = rbeta(n, smooth_child_mort_est_pakistan$q5_a, smooth_child_mort_est_pakistan$q5_b)
  h  = log(qx)
  k  = rnorm(n, 0, 0.01)
  
  # mx: n x 16
  mx1 = exp(am[1] + bm[1] * h + cm[1] * h^2 + vm[1] * k)
  mx2 = -0.25 * (mx1 + log(1 - qx))
  mx_rest = exp(
    matrix(am[3:16], n, 9, byrow = TRUE) +
      outer(h,   bm[3:16]) +
      outer(h^2, cm[3:16]) +
      outer(k,   vm[3:16])
  )
  mx = cbind(mx1, mx2, mx_rest)
  
  # lx: n x 17
  lx = matrix(NA_real_, n, 17)
  lx[, 1] = 1
  lx[, 2] = exp(-mx[, 1])
  lx[, 3] = lx[, 2] * exp(-4 * mx[, 2])
  lx[, 4:12] = lx[, 3] * exp(t(apply(-5 * mx[, 3:11], 1, cumsum)))
  
  # Lx: n x 15
  Lx = matrix(NA_real_, n, 15)
  Lx[, 1] = 0.5 * (lx[, 1] + lx[, 2]) + 2 * (lx[, 2] + lx[, 3])
  for (z in 2:10) {
    Lx[, z] = 2.5 * (lx[, z + 1] + lx[, z + 2])
  }
  Lx
}

# run replicates in parallel, with reproducible parallel-safe RNG streams
set.seed(123)
res = future_lapply(seq_len(B), one_boot_male, future.seed = TRUE)

# storage: [observation, age interval, bootstrap replicate]
Lx_boot = array(unlist(res), dim = c(n, 10, B))

plan(sequential)  # release workers

# summaries
Lx_hat = apply(Lx_boot, c(1, 2), mean)
Lx_se  = apply(Lx_boot, c(1, 2), sd)
Lx_lo  = apply(Lx_boot, c(1, 2), quantile, probs = 0.025)
Lx_hi  = apply(Lx_boot, c(1, 2), quantile, probs = 0.975)



age_groups = seq(0,70,5)
n = nrow(Lx_hat)

Lx_df = data.frame(
  obs       = rep(seq_len(n), times = 15),
  age_group = factor(rep(age_groups, each = n), levels = age_groups),
  Lx        = as.vector(Lx_hat),
  se        = as.vector(Lx_se),
  lo        = as.vector(Lx_lo),
  hi        = as.vector(Lx_hi)
)

# optional: attach identifiers (district, year, ...) from the input data
ids = smooth_child_mort_est_pakistan[, c("region", "years")]  # adjust column names
ids$obs = seq_len(n)
Lx_df = merge(ids, Lx_df, by = "obs")

Lx_hat_male_pakistan =  Lx_df %>%
  select(age_group,Lx,region,years) %>%
  pivot_wider(names_from = "age_group",values_from = "Lx")

past0(Lx_hat_male_pakistan)[1:15] = paste0("L",seq(0,70,5))

Lx_se_male_pakistan = Lx_df %>%
  select(age_group,se,region,years) %>%
  pivot_wider(names_from = "age_group",values_from = "se")

past0(Lx_se_female_pakistan)[1:15] = paste0("se_L",seq(0,70,5))


save(Lx_hat_female_pakistan,
     Lx_se_female_pakistan,
     Lx_hat_male_pakistan,
     Lx_se_male_pakistan,
     file="data/pakistan/mortality/pakistan_Lx_estimates.RData")


