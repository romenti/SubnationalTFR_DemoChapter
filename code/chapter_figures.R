#### load packages ####

source("code/upload_packages.R")

#### load functions ####
source("code/helpers.R")


#### load models written in NIMBLE ####

source("code/nimble_models.R")

#### load model results ####

load("results/france_model_tfr_summary.RData")
load("results/bangladesh_model_tfr_summary.RData")
load("results/spain_model_tfr_summary.RData")
load("results/pakistan_model_tfr_summary.RData")

#### France ####

# Download French NUTS 3 boundaries
france_nuts3 = gisco_get_nuts(
  year       = 2024,
  country    = "FR",
  nuts_level = 3,
  resolution = "03",
  epsg       = 4326
) %>%
  # Keep only continental France
  filter(
    !str_starts(NUTS_ID, "FRY"),  # exclude overseas France
    !str_starts(NUTS_ID, "FRM")   # exclude Corsica
  ) %>%
  # Lambert-93: suitable projection for mainland France
  st_transform(2154)

# Create the external border of continental France
france_outline = france_nuts3 %>%
  summarise()


france_nuts3$geo
# Draw the map
ggplot() +
  geom_sf(
    data = france_nuts3,
    fill = "grey90",
    color = "white",
    linewidth = 0.3
  ) +
  geom_sf(
    data = france_outline,
    fill = NA,
    color = "black",
    linewidth = 0.6
  ) +
  labs(
    title = "Continental France",
    subtitle = "NUTS 3 territorial units",
    caption = "Source: Eurostat GISCO"
  ) +
  theme_void()


data_france_map_male = france_shape_file_uniform %>%
  left_join(france_TFR_final_male, by = c("prov")) %>%
  filter(year==2020)



data_france_map_female = france_shape_file_uniform %>%
  left_join(france_TFR_final_female, by = c("prov")) %>%
  filter(year==2020)

plot = ggplot(france_map) +
  geom_sf(aes(fill = TFR_median), 
          color = "black", 
          linewidth = 0.2) +
  scale_fill_viridis_c(na.value = "grey90") +
  theme_void()



transformed_states = data_france_map_male %>%
  filter(prov %in% c("75","93","24")) %>%
  st_as_sf %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    # manual offsets for nicer placement
    lon_label = lon + c(-2, 1, 1),
    lat_label = lat + c(1, -1, 1)
  )


plot_male_france_map =  ggplot(data_france_map_male) +
  geom_sf(aes(fill = TFR_median), 
          color = "black", 
          linewidth = 0.2) + 
  scale_fill_steps(
    name = "Male TFR",
    low = "#f0fff0",
    high = "#004d00",
    n.breaks = 8
  )+
  geom_segment(
    data = transformed_states,
    aes(x = lon_label, y = lat_label, xend = lon, yend = lat),
    arrow = arrow(length = unit(0.5, "cm")),
    color = "black",
    linewidth = 1
  ) +
  ggrepel::geom_label_repel(
    data = transformed_states,
    aes(x = lon_label, y = lat_label, label = prov_name),
    color = "black",
    fill = "white",
    size = 5,
    fontface = "bold",
    label.size = 0.5,            # border thickness
    box.padding = 0.5,
    min.segment.length = Inf,    # no extra ggrepel line, your own arrow is used
    seed = 42
  )+
  ggtitle("France\n Male")+
  guides(
    fill = guide_colorsteps(
      barheight = unit(0.6, "cm"),  # thickness of bar
      barwidth  = unit(10, "cm"),   # length → spreads labels apart
      title.position = "top",
      label.position = "bottom",
      ticks = TRUE
    )
  )+
  theme_void()+
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, 'cm'),
        legend.text  = element_text(size=12),
        legend.title  = element_text(face="bold",size=12),
        text = element_text(family = "sans"),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=12),
        strip.text.y = element_text(face="bold",size=12),
        plot.title = element_text(hjust=0.5,face="bold",size=22))    




plot_female_france_map =  ggplot(data_france_map_female) +
  geom_sf(aes(fill = TFR_median), 
          color = "black", 
          linewidth = 0.2) + 
  scale_fill_steps(
    name = "Female\n TFR",
    low = "#f7eef7",
    high = "#4d004d",
    n.breaks = 8
  ) +
  geom_segment(
    data = transformed_states,
    aes(x = lon_label, y = lat_label, xend = lon, yend = lat),
    arrow = arrow(length = unit(0.5, "cm")),
    color = "black",
    linewidth = 1
  ) +
  ggrepel::geom_label_repel(
    data = transformed_states,
    aes(x = lon_label, y = lat_label, label = prov_name),
    color = "black",
    fill = "white",
    size = 5,
    fontface = "bold",
    label.size = 0.5,            # border thickness
    box.padding = 0.5,
    min.segment.length = Inf,    # no extra ggrepel line, your own arrow is used
    seed = 42
  )+
  ggtitle("France\n Female")+
  guides(
    fill = guide_colorsteps(
      barheight = unit(0.6, "cm"),  # thickness of bar
      barwidth  = unit(10, "cm"),   # length → spreads labels apart
      title.position = "top",
      label.position = "bottom",
      ticks = TRUE
    )
  )+
  theme_void()+
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, 'cm'),
        legend.text  = element_text(size=12),
        legend.title  = element_text(face="bold",size=12),
        text = element_text(family = "sans"),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=12),
        strip.text.y = element_text(face="bold",size=12),
        plot.title = element_text(hjust=0.5,face="bold",size=22))    




plot_time_series_france = rbind(france_TFR_final_male %>%
                                  select(year,prov,prov_name,TFR_median,TFR_low,TFR_high) %>%
                                  mutate(sex="Male"),
                                france_TFR_final_female %>%
                                  select(year,prov,prov_name,TFR_median,TFR_low,TFR_high) %>%
                                  mutate(sex="Female")) %>%
  filter(prov %in% c("75","93","24")) %>%
  mutate(prov_name = factor(prov_name,levels=c("Seine-Saint-Denis","Dordogne","Paris"))) %>%
  ggplot(aes(x=year,y=TFR_median,color=sex))+
  geom_line(aes(linetype=sex),linewidth = 1)+
  geom_ribbon(aes(x=year,ymin = TFR_low, ymax = TFR_high,fill=sex), color=NA, alpha = 0.5, show.legend = F)+
  facet_wrap(~prov_name)+
  scale_linetype_manual(values = c("solid",
                                   "longdash"),
                        name="",
                        labels=c("Female","Male"))+
  scale_colour_manual(values = c("#4d004d",
                                 "#004d00"),
                      name="",
                      labels=c("Female","Male"))+
  coord_cartesian(ylim=c(1,2.5))+
  scale_x_continuous(breaks = c(1982,seq(1990,2020,5),2024),
                     labels = c(1982,seq(1990,2020,5),2024)) +
  geom_hline(aes(yintercept = 2.1),linetype='dotted')+
  cowplot::theme_cowplot()+
  ylab("TFR estimate")+
  xlab("Year")+
  ggtitle("France")+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=12),
    axis.text.y = element_text(size=12),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 15, color = "black"),
    legend.title = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid = element_line(color = "grey95"),
    panel.spacing = unit(.3, "lines"),
    text = element_text(family = "sans"),
    aspect.ratio = .8,
    legend.position = c(0.9, 1),  # (x, y) inside panel [0–1]
    legend.justification = c(1, 1),
    legend.key.width = unit(2, "cm"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0,0,0,0)
  ) 


ch#### Spain ####

data_spain_map_male = spain_provinces %>%
  left_join(spain_TFR_final_male, by = c("prov")) %>%
  filter(year==2020)


data_spain_map_female = spain_provinces %>%
  left_join(spain_TFR_final_female, by = c("prov")) %>%
  filter(year==2020)

data_spain_map_female %>%
  arrange(TFR_median)
transformed_states = data_spain_map_female %>%
  filter(prov_name %in% c("Barcelona","Zamora","Sevilla")) %>%
  st_as_sf %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    # manual offsets for nicer placement
    lon_label = lon + c(-2, 1, 1),
    lat_label = lat + c(1, -1, 1)
  )

plot_female_spain_map =  ggplot(data_spain_map_female) +
  geom_sf(aes(fill = TFR_median), 
          color = "black", 
          linewidth = 0.2) + 
  scale_fill_steps(
    name = "Female\n TFR",
    low = "#f7eef7",
    high = "#4d004d",
    n.breaks = 8
  ) +
  geom_segment(
    data = transformed_states,
    aes(x = lon_label, y = lat_label, xend = lon, yend = lat),
    arrow = arrow(length = unit(0.5, "cm")),
    color = "black",
    linewidth = 1
  ) +
  ggrepel::geom_label_repel(
    data = transformed_states,
    aes(x = lon_label, y = lat_label, label = prov_name),
    color = "black",
    fill = "white",
    size = 5,
    fontface = "bold",
    label.size = 0.5,            # border thickness
    box.padding = 0.5,
    min.segment.length = Inf,    # no extra ggrepel line, your own arrow is used
    seed = 42
  )+
  ggtitle("Spain\n Female")+
  guides(
    fill = guide_colorsteps(
      barheight = unit(0.6, "cm"),  # thickness of bar
      barwidth  = unit(10, "cm"),   # length → spreads labels apart
      title.position = "top",
      label.position = "bottom",
      ticks = TRUE
    )
  )+
  theme_void()+
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, 'cm'),
        legend.text  = element_text(size=12),
        legend.title  = element_text(face="bold",size=12),
        text = element_text(family = "sans"),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=12),
        strip.text.y = element_text(face="bold",size=12),
        plot.title = element_text(hjust=0.5,face="bold",size=22))    



plot_male_spain_map =  ggplot(data_spain_map_male) +
  geom_sf(aes(fill = TFR_median), 
          color = "black", 
          linewidth = 0.2) + 
  scale_fill_steps(
    name = "Male TFR",
    low = "#f0fff0",
    high = "#004d00",
    n.breaks = 8
  )+
  geom_segment(
    data = transformed_states,
    aes(x = lon_label, y = lat_label, xend = lon, yend = lat),
    arrow = arrow(length = unit(0.5, "cm")),
    color = "black",
    linewidth = 1
  ) +
  ggrepel::geom_label_repel(
    data = transformed_states,
    aes(x = lon_label, y = lat_label, label = prov_name),
    color = "black",
    fill = "white",
    size = 5,
    fontface = "bold",
    label.size = 0.5,            # border thickness
    box.padding = 0.5,
    min.segment.length = Inf,    # no extra ggrepel line, your own arrow is used
    seed = 42
  )+
  ggtitle("Spain\n Male")+
  guides(
    fill = guide_colorsteps(
      barheight = unit(0.6, "cm"),  # thickness of bar
      barwidth  = unit(10, "cm"),   # length → spreads labels apart
      title.position = "top",
      label.position = "bottom",
      ticks = TRUE
    )
  )+
  theme_void()+
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, 'cm'),
        legend.text  = element_text(size=12),
        legend.title  = element_text(face="bold",size=12),
        text = element_text(family = "sans"),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=12),
        strip.text.y = element_text(face="bold",size=12),
        plot.title = element_text(hjust=0.5,face="bold",size=22))    



plot_time_series_spain = rbind(spain_TFR_final_male %>%
                                 select(year,prov,prov_name,TFR_median,TFR_low,TFR_high) %>%
                                 mutate(sex="Male"),
                               spain_TFR_final_female %>%
                                 select(year,prov,prov_name,TFR_median,TFR_low,TFR_high) %>%
                                 mutate(sex="Female")) %>%
  filter(prov_name %in% c("Barcelona","Zamora","Sevilla")) %>%
  mutate(prov_name = factor(prov_name,levels=c("Sevilla","Barcelona","Zamora"))) %>%
  ggplot(aes(x=year,y=TFR_median,color=sex))+
  geom_line(aes(linetype = sex),linewidth=1)+
  geom_ribbon(aes(x=year,ymin = TFR_low, ymax = TFR_high,fill=sex),color=NA, alpha = 0.5, show.legend = F)+
  facet_wrap(~prov_name)+
  scale_colour_manual(values = c("#4d004d",
                                 "#004d00"),
                      name="",
                      labels=c("Female","Male"))+
  scale_linetype_manual(values = c("solid",
                                   "longdash"),
                        name="",
                        labels=c("Female","Male"))+
  coord_cartesian(ylim=c(0.5,1.6))+
  scale_x_continuous(breaks = c(1982,seq(1990,2020,5),2024),
                     labels = c(1982,seq(1990,2020,5),2024)) +
  geom_hline(aes(yintercept = 2.1),linetype='dotted')+
  cowplot::theme_cowplot()+
  ylab("TFR estimate")+
  xlab("Year")+
  ggtitle("Spain")+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=12),
    axis.text.y = element_text(size=12),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 15, color = "black"),
    legend.title = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid = element_line(color = "grey95"),
    panel.spacing = unit(.3, "lines"),
    text = element_text(family = "sans"),
    aspect.ratio = .8,
    legend.position = c(0.9, 1),  # (x, y) inside panel [0–1]
    legend.justification = c(1, 1),
    plot.title = element_text(hjust = 0.5),
    legend.key.width = unit(2, "cm"),
    plot.margin = margin(0,0,0,0)
  ) 




plot_map_spain = cowplot::plot_grid(plot_female_spain_map, 
                                    plot_male_spain_map,
                                    ncol = 2,
                                    nrow = 1,
                                    rel_widths = c(1, 1)) 



plot_map_france = cowplot::plot_grid(plot_female_france_map, 
                                     plot_male_france_map,
                                     ncol = 2,
                                     nrow = 1,
                                     rel_widths = c(1, 1)) 

plot_france_spain = cowplot::plot_grid(plot_time_series_france, 
                                       plot_map_france,
                                       plot_time_series_spain, 
                                       plot_map_spain,
                                       ncol = 2,
                                       nrow = 2,
                                       labels = "AUTO",
                                       rel_widths = c(1, 1))  # optional but keeps balance'




ggsave(plot = plot_france_spain, filename = "figure/france_spain_tfr_fig.png", height = 20, width = 24)

#### Pakistan ####



plot_time_series_pakistan = rbind(pakistan_TFR_final_male %>%
                                    select(year,prov,TFR_median,TFR_low,TFR_high) %>%
                                    mutate(sex="Male"),
                                  pakistan_TFR_final_female %>%
                                    select(year,prov,TFR_median,TFR_low,TFR_high) %>%
                                    mutate(sex="Female")) %>%
  filter(prov %in% c("DERA BUGTI","PESHAWAR","KARACHI CENTRAL")) %>%
  mutate(prov=factor(prov,levels=c("DERA BUGTI","PESHAWAR","KARACHI CENTRAL"))) %>%
  ggplot(aes(x=year,y=TFR_median,color=sex))+
  geom_line(aes(linetype=sex),linewidth=1)+
  geom_ribbon(aes(x=year,ymin = TFR_low, ymax = TFR_high,fill=sex), color = NA, alpha = 0.5, show.legend = F)+
  facet_wrap(~prov)+
  scale_colour_manual(values = c("#4d004d",
                                 "#004d00"),
                      name="",
                      labels=c("Female","Male"))+
  scale_linetype_manual(values = c("solid",
                                   "longdash"),
                        name="",
                        labels=c("Female","Male"))+
  coord_cartesian(ylim=c(1,12))+
  scale_x_continuous(breaks = c(1982,seq(1990,2020,5),2024),
                     labels = c(1982,seq(1990,2020,5),2024)) +
  geom_hline(aes(yintercept = 2.1),linetype='dotted')+
  cowplot::theme_cowplot()+
  ylab("TFR estimate")+
  xlab("Year")+
  ggtitle("Pakistan")+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=12),
    axis.text.y = element_text(size=12),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 15, color = "black"),
    legend.title = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid = element_line(color = "grey95"),
    panel.spacing = unit(.3, "lines"),
    text = element_text(family = "sans"),
    aspect.ratio = .8,
    legend.position = c(0.9, 1),  # (x, y) inside panel [0–1]
    legend.justification = c(1, 1),
    legend.key.width =  unit(2, "cm"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0,0,0,0)
  ) 



plot_map_france = cowplot::plot_grid(plot_female_france, 
                                     plot_male_france,
                                     ncol = 2,
                                     nrow = 1,
                                     rel_widths = c(1, 1)) 



plot_combined_france = cowplot::plot_grid(plot_time_series_france, 
                                          plot_map_france,
                                          ncol = 1,
                                          nrow = 2,
                                          #labels = "AUTO",
                                          rel_widths = c(1, 1))  # optional but keeps balance'




pakistan_TFR_final_male_map = pakistan_TFR_final_male %>%
  filter(year==2020) %>%
  left_join(shape_file_pakistan_admin2_harmonized) %>%
  st_as_sf() 

pakistan_TFR_final_female_map = pakistan_TFR_final_female %>%
  filter(year==2020) %>%
  left_join(shape_file_pakistan_admin2_harmonized) %>%
  st_as_sf()

pakistan_TFR_final_male_map %>%
  filter(is.na(center_lon))
pakistan_TFR_final_male %>%
  filter(prov=="KARACHI CENTRAL")

pakistan_TFR_final_female %>%
  arrange(desc(TFR_median)) %>%
  filter(year==2020) 
transformed_regions = pakistan_TFR_final_female %>%
  filter(year==2020) %>% 
  left_join(shape_file_pakistan_admin2_harmonized) %>%
  filter(adm2_standard %in% c("DERA BUGTI","PESHAWAR","KARACHI CENTRAL")) %>%
  st_as_sf() %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    # manual offsets for nicer placement
    lon_label = lon + c(-2),
    lat_label = lat + c(1)
  )


transformed_regions = pakistan_TFR_final_female_map %>%
  filter(adm2_standard %in% c("DERA BUGTI","RAJANPUR","KARACHI CENTRAL")) %>%
  mutate(adm2_standard = str_to_title(adm2_standard)) %>%
  st_as_sf() %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    # manual offsets for nicer placement
    lon_label = lon + 2*c(-2, 1, 1),
    lat_label = lat + 2*c(1, -1, 1)
  )


offsets = tibble::tribble(
  ~adm2_standard,    ~dx, ~dy,
  "Dera Bugti",      -4.5,   2,
  "Rajanpur",         1.5, -1.5,
  "Karachi Central",  -1.2,  -1.2
)


transformed_regions = pakistan_TFR_final_female_map %>%
  filter(adm2_standard %in% c("DERA BUGTI","RAJANPUR","KARACHI CENTRAL")) %>%
  mutate(adm2_standard = str_to_title(adm2_standard)) %>%
  st_as_sf() %>%
  st_centroid() %>%
  left_join(offsets) %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    # manual offsets for nicer placement
    lon_label = lon + dx,
    lat_label = lat + dy
  )


plot_map_male_pakistan = ggplot(data = pakistan_TFR_final_male_map) +
  geom_sf(aes(fill=TFR_median), color = "black") +
  scale_fill_steps(
    name = "Male\n TFR",
    low = "#F7FCF5",
    high = "#00441B",
    n.breaks = 6
  ) +
  theme_void()+
  geom_segment(
    data = transformed_regions,
    aes(x = lon_label, y = lat_label, xend = lon, yend = lat),
    arrow = arrow(length = unit(0.5, "cm")),
    color = "black",
    linewidth = 1
  ) +
  ggrepel::geom_label_repel(
    data = transformed_regions,
    aes(x = lon_label, y = lat_label, label = adm2_standard),
    color = "black",
    fill = "white",
    size = 5,
    fontface = "bold",
    label.size = 0.5,            # border thickness
    box.padding = 0.5,
    min.segment.length = Inf,    # no extra ggrepel line, your own arrow is used
    seed = 42
  )+
  ggtitle("")+
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.text  = element_text(size=12),
        legend.title  = element_text(face="bold",size=12),
        text = element_text(family = "sans"),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=12),
        strip.text.y = element_text(face="bold",size=12),
        plot.title = element_text(hjust=0.5,face="bold",size=22))    




plot_map_female_pakistan = ggplot(data = pakistan_TFR_final_female_map) +
  geom_sf(aes(fill=TFR_median), color = "black") +
  scale_fill_steps(
    name = "Female\n TFR",
    low = "#F7F4F9",
    high = "#49006A",
    n.breaks = 6
  )+
  theme_void()+
  geom_segment(
    data = transformed_regions,
    aes(x = lon_label, y = lat_label, xend = lon, yend = lat),
    arrow = arrow(length = unit(0.5, "cm")),
    color = "black",
    linewidth = 1
  ) +
  ggrepel::geom_label_repel(
    data = transformed_regions,
    aes(x = lon_label, y = lat_label, label = adm2_standard),
    color = "black",
    fill = "white",
    size = 5,
    fontface = "bold",
    label.size = 0.5,            # border thickness
    box.padding = 0.5,
    min.segment.length = Inf,    # no extra ggrepel line, your own arrow is used
    seed = 42
  )+
  ggtitle("")+
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.text  = element_text(size=12),
        legend.title  = element_text(face="bold",size=12),
        text = element_text(family = "sans"),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=12),
        strip.text.y = element_text(face="bold",size=12),
        plot.title = element_text(hjust=0.5,face="bold",size=22))    





plot_map_pakistan = cowplot::plot_grid(plot_map_female_pakistan, 
                                       plot_map_male_pakistan,
                                       ncol = 2,
                                       nrow = 1,
                                       rel_widths = c(1, 1)) 

plot_combined_pakistan = cowplot::plot_grid(plot_time_series_pakistan, 
                                            plot_map_pakistan,
                                            ncol = 1,
                                            nrow = 2,
                                            #labels = "AUTO",
                                            rel_widths = c(1, 1))  # optional but keeps balance'


plot_combined_bangladesh = cowplot::plot_grid(plot_time_series_bangladesh, 
                                              plot_map_bangladesh,
                                              ncol = 1,
                                              nrow = 2,
                                              #labels = "AUTO",
                                              rel_widths = c(1, 1))  # optional but keeps balance'

plot_pakistan_bangladesh = cowplot::plot_grid(plot_time_series_bangladesh, 
                                              plot_map_bangladesh,
                                              plot_time_series_pakistan, 
                                              plot_map_pakistan,
                                              ncol = 2,
                                              nrow = 2,
                                              labels = "AUTO",
                                              rel_widths = c(1, 1))  # optional but keeps balance'


ggsave(plot = plot_pakistan_bangladesh, filename = "figure/bangladesh_pakistan_tfr_fig.png", height = 24, width = 20)

#### Bangladesh ####


plot_time_series_bangladesh = rbind(bangladesh_TFR_final_male %>%
                                      select(year,prov,TFR_median,TFR_low,TFR_high) %>%
                                      mutate(sex="Male"),
                                    bangladesh_TFR_final_female %>%
                                      select(year,prov,TFR_median,TFR_low,TFR_high) %>%
                                      mutate(sex="Female"))  %>%
  filter(prov %in% c("BRAHMANBARIA","MAGURA","DHAKA")) %>%
  mutate(prov = factor(prov,levels=c("BRAHMANBARIA","MAGURA","DHAKA"))) %>%
  ggplot(aes(x=year,y=TFR_median,color=sex))+
  geom_line(aes(linetype = sex),size=1)+
  geom_ribbon(aes(x=year,ymin = TFR_low, ymax = TFR_high,fill=sex), color=NA, alpha = 1, show.legend = F)+
  facet_wrap(~prov)+
  scale_colour_manual(values = c("#4d004d",
                                 "#004d00"),
                      name="",
                      labels=c("Female","Male"))+
  scale_linetype_manual(values = c("solid",
                                   "longdash"),
                        name="",
                        labels=c("Female","Male"))+
  coord_cartesian(ylim=c(1,12))+
  scale_x_continuous(breaks = c(1982,seq(1990,2020,5),2024),
                     labels = c(1982,seq(1990,2020,5),2024)) +
  geom_hline(aes(yintercept = 2.1),linetype='dotted')+
  cowplot::theme_cowplot()+
  ylab("TFR estimate")+
  xlab("Year")+
  ggtitle("Bangladesh")+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=12),
    axis.text.y = element_text(size=12),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 15, color = "black"),
    legend.title = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid = element_line(color = "grey95"),
    panel.spacing = unit(.3, "lines"),
    text = element_text(family = "sans"),
    aspect.ratio = .8,
    legend.position = c(0.9, 1),  # (x, y) inside panel [0–1]
    legend.justification = c(1, 1),
    legend.key.width =  unit(2, "cm"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0,0,0,0)
  ) 

bangladesh_TFR_final_male_map = bangladesh_TFR_final_male %>%
  filter(year==2020) %>%
  left_join(shape_file_bangladesh_admin2 %>%
              rename(prov=adm2_name)) %>%
  st_as_sf()


bangladesh_TFR_final_female_map = bangladesh_TFR_final_female %>%
  filter(year==2020) %>%
  left_join(shape_file_bangladesh_admin2 %>%
              rename(prov=adm2_name)) %>%
  st_as_sf()

transformed_regions = bangladesh_TFR_final_male_map %>%
  filter(prov %in% c("BRAHMANBARIA","MAGURA","DHAKA")) %>%
  mutate(prov = str_to_title(prov)) %>%
  st_as_sf %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    # manual offsets for nicer placement
    lon_label = lon + c(-2, 1, 1),
    lat_label = lat + c(1, -1, 1)
  )

offsets = tibble::tribble(
  ~prov,          ~dx,  ~dy,
  "Brahmanbaria",  1.0, -1,
  "Magura",       -1, -1,
  "Dhaka",        -1.5,  1.5
)

transformed_regions = bangladesh_TFR_final_male_map %>%
  filter(prov %in% c("BRAHMANBARIA","MAGURA","DHAKA")) %>%
  mutate(prov = str_to_title(prov)) %>%
  left_join(offsets, by = "prov") %>%
  st_as_sf %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    # manual offsets for nicer placement
    lon_label = lon + dx,
    lat_label = lat + dy
  )


plot_map_male_bangladesh = ggplot(data = bangladesh_TFR_final_male_map) +
  geom_sf(aes(fill=TFR_median), color = "black") +
  scale_fill_steps(
    name = "Male\nTFR",
    low = "#F7FCF5",
    high = "#00441B",
    n.breaks = 6
  )+
  theme_void()+
  geom_segment(
    data = transformed_regions,
    aes(x = lon_label, y = lat_label, xend = lon, yend = lat),
    arrow = arrow(length = unit(0.5, "cm")),
    color = "black",
    linewidth = 1
  ) +
  ggrepel::geom_label_repel(
    data = transformed_regions,
    aes(x = lon_label, y = lat_label, label = prov),
    color = "black",
    fill = "white",
    size = 5,
    fontface = "bold",
    label.size = 0.5,            # border thickness
    box.padding = 0.5,
    min.segment.length = Inf,    # no extra ggrepel line, your own arrow is used
    seed = 42
  )+
  ggtitle("")+
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.text  = element_text(size=12),
        legend.title  = element_text(face="bold",size=12),
        text = element_text(family = "sans"),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=12),
        strip.text.y = element_text(face="bold",size=12),
        plot.title = element_text(hjust=0.5,face="bold",size=22))    


plot_map_female_bangladesh = ggplot(data = bangladesh_TFR_final_female_map) +
  geom_sf(aes(fill=TFR_median), color = "black") +
  scale_fill_steps(
    name = "Female\nTFR",
    low = "#F7F4F9",
    high = "#49006A",
    n.breaks = 6
  )+
  theme_void()+
  geom_segment(
    data = transformed_regions,
    aes(x = lon_label, y = lat_label, xend = lon, yend = lat),
    arrow = arrow(length = unit(0.5, "cm")),
    color = "black",
    linewidth = 1
  ) +
  ggrepel::geom_label_repel(
    data = transformed_regions,
    aes(x = lon_label, y = lat_label, label = prov),
    color = "black",
    fill = "white",
    size = 5,
    fontface = "bold",
    label.size = 0.5,            # border thickness
    box.padding = 0.5,
    min.segment.length = Inf,    # no extra ggrepel line, your own arrow is used
    seed = 42
  )+
  ggtitle("")+
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.text  = element_text(size=12),
        legend.title  = element_text(face="bold",size=12),
        text = element_text(family = "sans"),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=12),
        strip.text.y = element_text(face="bold",size=12),
        plot.title = element_text(hjust=0.5,face="bold",size=22))    




plot_map_bangladesh = cowplot::plot_grid(plot_map_female_bangladesh, 
                                         plot_map_male_bangladesh,
                                         ncol = 2,
                                         nrow = 1,
                                         rel_widths = c(1, 1)) 
plot_combined_bangladesh = cowplot::plot_grid(plot_time_series_bangladesh, 
                                              plot_map_bangladesh,
                                              ncol = 1,
                                              nrow = 2,
                                              labels = "AUTO",
                                              rel_widths = c(1, 1))  # optional but keeps balance'


iqr_plot = rbind(rbind(bangladesh_TFR_final_female %>%
                         group_by(year) %>%
                         summarise(iqr=IQR(TFR_median)) %>%
                         mutate(sex="Female"),
                       bangladesh_TFR_final_male %>%
                         group_by(year) %>%
                         summarise(iqr=IQR(TFR_median)) %>%
                         mutate(sex="Male")) %>%
                   mutate(country="Bangladesh"),
                 rbind(pakistan_TFR_final_female %>%
                         group_by(year) %>%
                         summarise(iqr=IQR(TFR_median)) %>%
                         mutate(sex="Female"),
                       pakistan_TFR_final_male %>%
                         group_by(year) %>%
                         summarise(iqr=IQR(TFR_median)) %>%
                         mutate(sex="Male")) %>%
                   mutate(country="Pakistan"),
                 rbind(spain_TFR_final_female %>%
                         group_by(year) %>%
                         summarise(iqr=IQR(TFR_median)) %>%
                         mutate(sex="Female"),
                       spain_TFR_final_male %>%
                         group_by(year) %>%
                         summarise(iqr=IQR(TFR_median)) %>%
                         mutate(sex="Male")) %>%
                   mutate(country="Spain"),
                 rbind(france_TFR_final_female %>%
                         group_by(year) %>%
                         summarise(iqr=IQR(TFR_median)) %>%
                         mutate(sex="Female"),
                       france_TFR_final_male %>%
                         group_by(year) %>%
                         summarise(iqr=IQR(TFR_median)) %>%
                         mutate(sex="Male")) %>%
                   mutate(country="France")) %>%
  mutate(country=factor(country,levels=c("France","Spain","Bangladesh","Pakistan"))) %>%
  ggplot(aes(x=year,y=iqr,color=sex))+
  geom_line(aes(linetype=sex),size=1.5)+
  facet_wrap(~country)+
  scale_x_continuous(breaks = seq(2000,2020,5),
                     labels = seq(2000,2020,5)) +
  cowplot::theme_cowplot()+
  scale_colour_manual(values = c("#4d004d",
                                 "#004d00"),
                      name="",
                      labels=c("Female","Male"))+
  scale_linetype_manual(values = c("solid",
                                   "longdash"),
                        name="",
                        labels=c("Female","Male"))+
  ylab("IQR for the TFR")+
  xlab("Year")+
  coord_cartesian(ylim=c(0,2.5))+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=12),
    axis.text.y = element_text(size=12),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 15, color = "black"),
    legend.title = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid = element_line(color = "grey95"),
    panel.spacing = unit(.3, "lines"),
    text = element_text(family = "sans"),
    aspect.ratio = .8,
    legend.position = c(0.9, 1),  # (x, y) inside panel [0–1]
    legend.justification = c(1, 1),
    legend.key.width =  unit(2, "cm"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0,0,0,0)
  ) 




sd_plot = rbind(rbind(bangladesh_TFR_final_female %>%
                        group_by(year) %>%
                        summarise(sd=sd(TFR_median)) %>%
                        mutate(sex="Female"),
                      bangladesh_TFR_final_male %>%
                        group_by(year) %>%
                        summarise(sd=sd(TFR_median)) %>%
                        mutate(sex="Male")) %>%
                  mutate(country="Bangladesh"),
                rbind(pakistan_TFR_final_female %>%
                        group_by(year) %>%
                        summarise(sd=sd(TFR_median)) %>%
                        mutate(sex="Female"),
                      pakistan_TFR_final_male %>%
                        group_by(year) %>%
                        summarise(sd=sd(TFR_median)) %>%
                        mutate(sex="Male")) %>%
                  mutate(country="Pakistan"),
                rbind(spain_TFR_final_female %>%
                        group_by(year) %>%
                        summarise(sd=sd(TFR_median)) %>%
                        mutate(sex="Female"),
                      spain_TFR_final_male %>%
                        group_by(year) %>%
                        summarise(sd=sd(TFR_median)) %>%
                        mutate(sex="Male")) %>%
                  mutate(country="Spain"),
                rbind(france_TFR_final_female %>%
                        group_by(year) %>%
                        summarise(sd=sd(TFR_median)) %>%
                        mutate(sex="Female"),
                      france_TFR_final_male %>%
                        group_by(year) %>%
                        summarise(sd=sd(TFR_median)) %>%
                        mutate(sex="Male")) %>%
                  mutate(country="France")) %>%
  mutate(country=factor(country,levels=c("France","Spain","Bangladesh","Pakistan"))) %>%
  ggplot(aes(x=year,y=sd,color=sex))+
  geom_line(aes(linetype=sex),size=1.5)+
  facet_wrap(~country)+
  scale_x_continuous(breaks = seq(2000,2020,5),
                     labels = seq(2000,2020,5)) +
  cowplot::theme_cowplot()+
  scale_colour_manual(values = c("#4d004d",
                                 "#004d00"),
                      name="",
                      labels=c("Female","Male"))+
  scale_linetype_manual(values = c("solid",
                                   "longdash"),
                        name="",
                        labels=c("Female","Male"))+
  ylab("SD for the TFR")+
  xlab("Year")+
  coord_cartesian(ylim=c(0,2.5))+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=12),
    axis.text.y = element_text(size=12),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 15, color = "black"),
    legend.title = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid = element_line(color = "grey95"),
    panel.spacing = unit(.3, "lines"),
    text = element_text(family = "sans"),
    aspect.ratio = .8,
    legend.position = c(0.9, 1),  # (x, y) inside panel [0–1]
    legend.justification = c(1, 1),
    plot.title = element_text(hjust = 0.5),
    legend.key.width =  unit(2, "cm"),
    plot.margin = margin(0,0,0,0)
  ) 


plot_sd_iqr = cowplot::plot_grid(iqr_plot,
                                 sd_plot,
                                 ncol = 2,
                                 nrow = 1,
                                 labels = "AUTO",
                                 rel_widths = c(1, 1))  # optional but keeps balance'


ggsave(plot = plot_sd_iqr, filename = "figure/sd_iqr_plot.png", height = 5, width = 12)


#### Scatter Plots ####

load("results/pop_data.RData")

##### Reproductive Sex Ratios #####


scatter_plot_sex_ratio = bind_rows(france_TFR_final_female %>%
                   select(prov,year,female_tfr=TFR_median) %>%
                   left_join(france_TFR_final_male %>%
                               select(prov,year,male_tfr=TFR_median)) %>%
                   mutate(tfr_ratio=male_tfr/female_tfr) %>%
                   left_join(CM_france %>%
                               select(prov,year,M30,M35,M40) %>%
                               mutate(M=M30+M35+M40) %>%
                               left_join(CW_france %>%
                                           select(prov,year,W25,W30,W35) %>%
                                           mutate(W=W30+W25+W35)) %>%
                               mutate(sex_ratio=M/W)) %>%
                   filter(year==2017) %>%
                   mutate(country="France") %>%
                   select(country,tfr_ratio,sex_ratio),
                 spain_TFR_final_female %>%
                   select(prov,year,female_tfr=TFR_median) %>%
                   left_join(spain_TFR_final_male %>%
                               select(prov,year,male_tfr=TFR_median)) %>%
                   mutate(tfr_ratio=male_tfr/female_tfr) %>%
                   left_join(CM_spain %>%
                               select(prov,year,M35,M40,M45) %>%
                               mutate(M=M35+M40+M45) %>%
                               left_join(CW_spain %>%
                                           select(prov,year,W30,W35,W40) %>%
                                           mutate(W=W30+W35+W40)) %>%
                               mutate(sex_ratio=M/W)) %>%
                   filter(year==2017) %>%
                   mutate(country="Spain") %>%
                   select(country,tfr_ratio,sex_ratio),
                 pakistan_TFR_final_female %>%
                   select(year,prov,tfr_female=TFR_median) %>%
                   left_join(pakistan_TFR_final_male %>%
                               select(year,prov,tfr_male=TFR_median)) %>%
                   mutate(tfr_ratio=tfr_male/tfr_female) %>%
                   left_join(men_pakistan_harmonized %>%
                               select(year,prov=adm2_standard,M30,M25,M35,region) %>%
                               left_join(women_pakistan_harmonized %>%
                                           select(year,prov=adm2_standard,W30,W20,W25,region)) %>%
                               mutate(M=M30+M25+M35,
                                      W=W30+W25+W20,
                                      sex_ratio=(M/W))) %>%
                   filter(year==2017) %>%
                   mutate(country="Pakistan") %>%
                   select(country,tfr_ratio,sex_ratio),
                 bangladesh_TFR_final_female %>%
                   select(year,prov,tfr_female=TFR_median) %>%
                   left_join(bangladesh_TFR_final_male %>%
                               select(year,prov,tfr_male=TFR_median)) %>%
                   mutate(tfr_ratio=tfr_male/tfr_female) %>%
                   left_join(CM_bangladesh %>%
                                 select(year,prov=adm2,M25,M30,M20) %>%
                                 mutate(M=M30+M25+M20) %>%
                                 left_join(CW_bangladesh %>%
                                             select(year,prov=adm2,W25,W15,W20)) %>%
                                 mutate(W=W15+W20+W25) %>%
                                 mutate(sex_ratio=(M/W))) %>% 
                   filter(year==2017) %>%
                   mutate(country="Bangladesh") %>%
                   select(country,tfr_ratio,sex_ratio)) %>%
  ggplot(aes(x=sex_ratio,y=tfr_ratio))+
  geom_point(color="red",alpha = .5,size = 2)+
  geom_smooth(method="lm",se=F)+
  ggpubr::stat_cor(
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    r.accuracy = 0.01,
    p.accuracy = 0.001,
    size = 4
  )  +
  cowplot::theme_cowplot()+
  xlab("Reproductive sex ratio")+
  ylab("Male-to-female TFR ratio")+
  facet_wrap(~country,nrow=2,scales = "free")+
  theme(
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 15, color = "black"),
    legend.title = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid = element_line(color = "grey95"),
    panel.spacing = unit(.3, "lines"),
    text = element_text(family = "sans"),
    aspect.ratio = .8,
    legend.position = c(0.9, 1),  # (x, y) inside panel [0–1]
    legend.justification = c(1, 1),
    legend.key.width =  unit(2, "cm"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0,0,0,0)
  ) 

ggsave(plot = scatter_plot_sex_ratio, filename = "figures/scatter_plot_sex_ratio.png", height = 5, width = 12)


##### Sex Differences in Age at First Union #####



union_data = pakistan_TFR_final_female %>%
  select(year,prov,tfr_female=TFR_median) %>%
        left_join(pakistan_TFR_final_male %>%
                    select(year,prov,tfr_male=TFR_median)) %>%
  mutate(tfr_ratio=tfr_male/tfr_female) %>%
  filter(year == 2017) %>%
  left_join(pak_data %>%
              rename(prov=adm2_standard)) %>%
  mutate(country="Pakistan") %>%
  select(year,country,tfr_ratio,mean_age_gap) %>%
  bind_rows(bangladesh_TFR_final_female %>%
              select(year,prov,tfr_female=TFR_median) %>%
              filter(year== 2017) %>%
              left_join(bangladesh_TFR_final_male %>%
                          select(year,prov,tfr_male=TFR_median)) %>%
              mutate(tfr_ratio=tfr_male/tfr_female) %>%
              left_join(bang_data %>%
                          rename(prov=adm2_name)) %>%
              mutate(country="Bangladesh") %>%
              select(year,country,tfr_ratio,mean_age_gap))




plot_marriage_ratio_pakistan = ggplot(data = union_data,aes(x=mean_age_gap,y=tfr_ratio))+
  geom_point(color="red",alpha = .5)+
  geom_smooth(method="lm",se=F)+
  ggpubr::stat_cor(
    method = "pearson",
    label.x.npc = "left",
    label.y.npc = "top",
    r.accuracy = 0.01,
    p.accuracy = 0.001,
    size = 8
  )  +
  cowplot::theme_cowplot()+
  xlab("Difference in the mean age at first union")+
  ylab("Male-to-female TFR ratio")+
  facet_wrap(~country,ncol=2,scales="free")+
  theme(
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 15, color = "black"),
    legend.title = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid = element_line(color = "grey95"),
    panel.spacing = unit(.3, "lines"),
    text = element_text(family = "sans"),
    aspect.ratio = .8,
    legend.position = c(0.9, 1),  # (x, y) inside panel [0–1]
    legend.justification = c(1, 1),
    legend.key.width =  unit(2, "cm"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0,0,0,0)
  ) 




ggsave(plot=plot_marriage_ratio_pakistan,file="figures/plot_marriage_pakistan_bangladesh.png",width=12,height=5)









