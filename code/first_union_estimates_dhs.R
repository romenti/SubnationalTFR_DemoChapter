#### Upload all the packages ####

source("code/upload_packages.R")

#### Upload all the functions ####

source("code/helpers.R")

#### Bangladesh ####

data2017_bang = read_dta("data/bangladesh/dhs/BDIR7RFL.DTA")
shape_file_bang_admin2_dhs = st_read("data/bangladesh/dhs/BDGE7SFL.shp")
setdiff(unique(toupper(shape_file_bangladesh_admin2$adm1_name)),unique(shape_file_bang_admin2_dhs$ADM1NAME))


shape_file_bangladesh_admin2_dhs = shape_file_bangladesh_admin2 %>%
  mutate(adm1_name = toupper(trimws(adm1_name)))

shape_file_bang_admin2_trial = shape_file_bang_admin2_trial %>%
  mutate(
    ADM1NAME = recode(
      toupper(trimws(ADM1NAME)),
      "BARISAL"    = "BARISHAL",
      "CHITTAGONG" = "CHATTOGRAM"
    )
  )

# Check that both contain the same set of names
setequal(
  shape_file_bangladesh_admin2_dhs$adm1_name,
  shape_file_bang_admin2_trial$ADM1NAME
)

# Prepare ADM2 boundaries
adm2_bd = st_make_valid(shape_file_bangladesh_admin2_dhs)

# Match coordinate reference systems
clusters_bd = st_transform(
  shape_file_bangladesh_admin2_dhs,
  st_crs(adm2_bd)
)



# Attach ADM2 attributes to each cluster
bangladesh_clusters_adm2 = st_join(
  clusters_bd,
  adm2_bd,
  join = st_within,
  left = TRUE,
  suffix = c("_cluster", "_adm2")
) %>%
  sf::st_drop_geometry() %>%
  select(v001=DHSCLUST,adm1_name,adm2_name)

bang_data = full_join(data2017_bang,bangladesh_clusters_adm2) %>%
  filter(
    v502 == 1,                 # Currently in union
    v503 == 1,                 # Married only once
    between(v012, 15, 49),
    between(v730, 1, 95),       # Exclude missing/unknown and 96+ ages
    !is.na(adm2_name),
    !is.na(v005), v005 > 0
  ) %>%
  mutate(
    age_gap = as.numeric(v730) - as.numeric(v012),
    weight  = as.numeric(v005) / 1e6
  ) %>%
  group_by(adm2_name) %>%
  summarise(
    mean_age_gap = weighted.mean(age_gap, weight),
    n_women     = n(),
    n_clusters  = n_distinct(v001),
    .groups     = "drop"
  )

save(bang_data,file="results/union_data_bangladesh.RData")

#### Pakistan ####

data2017_pak = read_dta("data/pakistan/dhs/PKIR71FL.dta"))

pak_data = data2017 %>%
  filter(
    v502 == 1,                 # Currently in union
    v503 == 1,                 # Married only once
    between(v012, 15, 49),
    between(v730, 1, 95),       # Exclude missing/unknown and 96+ ages
    !is.na(adm2_standard),
    !is.na(v005), v005 > 0
  ) %>%
  mutate(
    age_gap = as.numeric(v730) - as.numeric(v012),
    weight  = as.numeric(v005) / 1e6
  ) %>%
  group_by(adm2_standard) %>%
  summarise(
    mean_age_gap = weighted.mean(age_gap, weight),
    n_women     = n(),
    n_clusters  = n_distinct(v001),
    .groups     = "drop"
  )


save(pak_data,file="results/union_data_pakistan.RData")
