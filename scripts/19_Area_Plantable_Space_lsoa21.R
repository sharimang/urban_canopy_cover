#### Calculating Area of Plantable Space for each Landcover Type ####

## Total area deemed plantable for all greenspace, no private garden greenspace, public greenspace, and pavement ##
## -- Area calculation uses polygons 

## Composition of land cover also explored with raster data ##
## Categorized as being in all greenspace, no private garden greenspace, public greenspace, pavement, OR both pavement and greenspace ##
## -- There are places where a plantable cell falls over greenspace and pavement, so tree could be planted in either.


## Written by: Shari Mang ##
## Date written: 27/11/2025 ##
## Date last updated: 30/04/2026 ##

## Written for RStudio software version 4.4.0 ##


# ============= #
#### Set up ####
# ============= #

# Load packages
if(!require('pacman')) {install.packages('pacman')}
library(pacman)
pacman::p_load(terra,
               sf, 
               tidyverse, 
               tidyr,
               dplyr,
               scales,
               here, 
               conflicted,
               ggplot2,
               MetBrewer,
               ggview)
conflicted::conflict_prefer("here", "here")
conflicted::conflicts_prefer(dplyr::filter)

## File paths ##
p.regions.2021 <- here("data_processed/urban/LSOA2021_EW/regions_boundary/") # p.regions.correct
p.grsp.avail <- here("data_processed/os_greenspace/plantable_available_lsoa2021/")
p.pave.avail <- here("data_processed/os_topo/manmade_plantable/pavement_plantable_available_lsoa2021/")
p.lsoa.out <- here("data_processed/lsoa_2021_vars/")
p.area.plantable <- here("data_processed/assessment/lsoa2021/plantable_area_calc/")
p.rast.town <- here("data_processed/model_rasters/LSOA2021_town_border/")
p.plots <- here("data_processed/assessment/lsoa2021/plantable_area_calc/plots/")
p.plots.paper <- here("data_processed/assessment/lsoa2021/town_target/plots_paper/")


## Parameters ##
# Values used throughout and changed depending on assumed crown size per tree.
crown_area <- 0.0032 # (in ha)  ~~ 0.0032 = median crown area of current urban trees; 0.0050 = mean crown area of current urban trees 
crown_label <- 32 # crown area size for output labels
target_cover <- 0.20 # Target town level canopy cover


# ======================================================================== #
#### 1. Calculate Area of Plantable Space in All LSOAs - from Polygons ####
# ======================================================================== #

## Use polygons not rasters to calculate plantable area -- more accurate ##
# Output summarized for each LSOA and by town

# Files #
grsp_all_poly <- list.files(p.grsp.avail, pattern = "plant_park_buff_", full.names = TRUE)
grsp_npg_poly <- list.files(p.grsp.avail, pattern = "plant_npg_park_buff_", full.name = TRUE)
grsp_pub_poly <- list.files(p.grsp.avail, pattern = "plant_public_park_buff_", full.name = TRUE)
pave_poly <- list.files(p.pave.avail, pattern = "gpkg", full.names = TRUE)
urban_files <- list.files(p.regions.2021, pattern = "gpkg", full.name = T)
# LSOA
lsoa_dat <- st_read(paste0(p.lsoa.out, "/can_imd2025_lsoa2021_EngWales.gpkg"))
# simplify LSOA data
lsoa_loop <- lsoa_dat %>%
  dplyr::select(-c(PolyName, lsoa_name, lsoa_area_m2, TCITY15CD, TCITY15NM))
# Output list
area_national_list <- vector("list", length = length(urban_files))
i <- 1
j <- 1


ggplot() + 
  geom_sf(data = city, colour = "black") +  geom_sf(data = grsp_npg_j, aes(fill = TownsNM))

for(i in seq_along(urban_files)) {
  # Isolate region i
  region_i <- sf::st_read(urban_files[[i]])
  reg_name <- unique(region_i$region)
  # Get Town names in region i
  towns <- region_i$TownsNM
  # output list for each data type, remade for each region
  grsp_town_list <- vector("list", length = length(towns))
  grsp_npg_town_list <- vector("list", length = length(towns))
  grsp_pub_town_list <- vector("list", length = length(towns))
  pave_town_list <- vector("list", length = length(towns))

  # Go by town
  for(j in seq_along(towns)){
    # Reduce LSOAs to town j
    lsoa_j <- lsoa_loop %>%
      dplyr::filter(TownsNM == towns[[j]])
    # geometry for urban area
    city_geo_j <- st_as_text(st_geometry(region_i[region_i$TownsNM == towns[[j]],]))  # city boundary for town j

    ## Calculate area of plantable greenspace ##
    
    # ~~~ All Greenspace ~~~ #
    # Read in greenspace data for the given urban area
    grsp_j <- sf::st_read(grsp_all_poly[[i]], wkt_filter = city_geo_j) 
    # intersect greenspace with LSOAs to cut polygons by LSOA and assign associated LSOA ID
    grsp_j <- sf::st_intersection(grsp_j, lsoa_j[c("PolyID")]) %>%
      sf::st_collection_extract(type = "POLYGON") %>%
      dplyr::mutate(grsp_area_m2 = round(units::drop_units(sf::st_area(.)), 4)) %>%
      sf::st_drop_geometry(.) %>%
      # summarize area for each LSOA
      dplyr::group_by(PolyID) %>%
      dplyr::summarise(
        grsp_all_area_ha = round(sum(grsp_area_m2)/10000,4)
      ) %>%
      dplyr::mutate(TownsNM = towns[[j]],
                    region = reg_name) %>%
      as.data.frame()
    # Add to output list for region
    grsp_town_list[[j]] <- grsp_j

    # ~~~ NPG Greenspace ~~~ #
    # Read in greenspace data for the given urban area
    grsp_npg_j <- sf::st_read(grsp_npg_poly[[i]], wkt_filter = city_geo_j) 
    # intersect greenspace with LSOAs to cut polygons by LSOA and assign associated LSOA ID
    grsp_npg_j <- sf::st_intersection(grsp_npg_j, lsoa_j[c("PolyID")]) %>%
      sf::st_collection_extract(type = "POLYGON") %>%
      dplyr::mutate(grsp_npg_area_m2 = round(units::drop_units(sf::st_area(.)), 4)) %>%
      sf::st_drop_geometry(.) %>%
      # summarize area for each LSOA
      dplyr::group_by(PolyID) %>%
      dplyr::summarise(
        grsp_npg_area_ha = round(sum(grsp_npg_area_m2)/10000,4)
      ) %>%
      dplyr::mutate(TownsNM = towns[[j]],
                    region = reg_name) %>%
      as.data.frame()
    # Add to output list for region
    grsp_npg_town_list[[j]] <- grsp_npg_j
    
    # ~~~ PUB Greenspace ~~~ #
    # Read in greenspace data for the given urban area
    grsp_pub_j <- sf::st_read(grsp_pub_poly[[i]], wkt_filter = city_geo_j) 
    # intersect greenspace with LSOAs to cut polygons by LSOA and assign associated LSOA ID
    grsp_pub_j <- sf::st_intersection(grsp_pub_j, lsoa_j[c("PolyID")]) %>%
      sf::st_collection_extract(type = "POLYGON") %>%
      dplyr::mutate(grsp_pub_area_m2 = round(units::drop_units(sf::st_area(.)), 4)) %>%
      sf::st_drop_geometry(.) %>%
      # summarize area for each LSOA
      dplyr::group_by(PolyID) %>%
      dplyr::summarise(
        grsp_pub_area_ha = round(sum(grsp_pub_area_m2)/10000,4)
      ) %>%
      dplyr::mutate(TownsNM = towns[[j]],
                    region = reg_name) %>%
      as.data.frame()
    # Add to output list for region
    grsp_pub_town_list[[j]] <- grsp_pub_j
    
    # ~~~ Plantable Pavement ~~~ #
    # Read in greenspace data for the given urban area
    pave_j <- sf::st_read(pave_poly[[i]], wkt_filter = city_geo_j)
    # intersect greenspace with LSOAs to cut polygons by LSOA and assign associated LSOA ID
    pave_j <- sf::st_intersection(pave_j, lsoa_j[c("PolyID")]) %>%
      sf::st_collection_extract(type = "POLYGON") %>%
      dplyr::mutate(pave_area_m2 = round(units::drop_units(sf::st_area(.)), 4)) %>%
      sf::st_drop_geometry(.) %>%
      # summarize area for each LSOA
      dplyr::group_by(PolyID) %>%
      dplyr::summarise(
        pave_area_ha = round(sum(pave_area_m2)/10000,4)
      ) %>%
      dplyr::mutate(TownsNM = towns[[j]],
                    region = reg_name) %>%
      as.data.frame()
    # Add to output list for region
    pave_town_list[[j]] <- pave_j
  }
  ## Combine area values for each plantable option into single dataframe ##
  # LSOA data for region i
  all_lsoa_i <- lsoa_loop %>%
    dplyr::filter(region == reg_name)

  ## Combine towns for region i into single object ##
  # ~~~ All Greenspace ~~~ #
  grsp_reg_i <- do.call(rbind, grsp_town_list)
  # ~~~ NPG Greenspace  ~~~ #
  grsp_npg_reg_i <- do.call(rbind, grsp_npg_town_list)
  # ~~~ PUB Greenspace  ~~~ #
  grsp_pub_reg_i <- do.call(rbind, grsp_pub_town_list)
  # ~~~ Plantable Pavement ~~~ #
  pave_reg_i <- do.call(rbind, pave_town_list)

  # Bind all data together with LSOA
  area_lsoa_i <- all_lsoa_i %>%
    dplyr::left_join(select(grsp_reg_i, PolyID, grsp_all_area_ha), by = "PolyID") %>%
    dplyr::left_join(select(grsp_npg_reg_i, PolyID, grsp_npg_area_ha), by = "PolyID") %>%
    dplyr::left_join(select(grsp_pub_reg_i, PolyID, grsp_pub_area_ha), by = "PolyID") %>%
    dplyr::left_join(select(pave_reg_i, PolyID, pave_area_ha), by = "PolyID") %>%
    dplyr::mutate(across(contains("area"), ~ replace_na(., 0))) %>%
    dplyr::mutate(grsp_all_pave_ha = grsp_all_area_ha + pave_area_ha, # Total plantable area for All Greenspace and Pavement
                  grsp_npg_pave_ha = grsp_npg_area_ha + pave_area_ha, # Total plantable area for NPG Greenspace and Pavement
                  grsp_pub_pave_ha = grsp_pub_area_ha + pave_area_ha) # Total plantable area for PUB Greenspace and Pavement

  # Save out for region i
  sf::st_write(area_lsoa_i, paste0(p.area.plantable, "/plantable_area_by_region/plantable_area_lsoa_all_", reg_name, ".gpkg"))
  # Append to national csv output
  area_national_list[[i]] <- area_lsoa_i %>% sf::st_drop_geometry()
}
# Bind national list into national df of areas per lsoa
area_national_df <- do.call(rbind, area_national_list)
# Save out
write.csv(area_national_df, paste0(p.area.plantable, "/plantable_area_lsoa_all_national.csv"), row.names = FALSE)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# > 1.1 Town summary for all LSOA -- plantable area  ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
area_national_df <- read.csv(paste0(p.area.plantable, "/plantable_area_lsoa_all_national.csv"))
area_by_town <- area_national_df %>%
  #dplyr::group_by(TownsNM, region) %>%
  summarise(.by = c(TownsNM, region),
    gs_all_ha = sum(grsp_all_area_ha), # Plantable area for All Greenspace
    gs_npg_ha = sum(grsp_npg_area_ha), # Plantable area for NPG Greenspace
    gs_pub_ha = sum(grsp_pub_area_ha), # Plantable area for PUB Greenspace
    pave_ha = sum(pave_area_ha) # Plantable area for pavement
  ) %>%
  mutate(
    pr_gard_ha = gs_all_ha - gs_npg_ha, # Plantable area of Private Gardens
    total_gs_all_pave_ha = gs_all_ha + pave_ha, # Total plantable area for All Greenspace and Pavement
    total_gs_npg_pave_ha = gs_npg_ha + pave_ha, # Total plantable area for NPG Greenspace and Pavement
    total_gs_pub_pave_ha = gs_pub_ha + pave_ha, # Total plantable area for PUB Greenspace and Pavement
    gs_all_prop = round(gs_all_ha/total_gs_all_pave_ha, 4), # Proportion of total plantable area that is All Greenspace
    gs_pg_prop = round(pr_gard_ha/total_gs_all_pave_ha, 4), # Proportion of total plantable area that is private gardens 
    gs_npg_prop = round(gs_npg_ha/total_gs_all_pave_ha, 4), # Proportion of total plantable area that is NPG Greenspace
    gs_non_dom_priv_prop = round((gs_npg_ha - gs_pub_ha)/total_gs_all_pave_ha, 4), # Proportion of total plantable area that is Non-domestic (garden) private land
    gs_pub_prop = round(gs_pub_ha/total_gs_all_pave_ha, 4), # Proportion of total plantable area that is Pub Greenspace
    pave_all_prop = round(pave_ha/total_gs_all_pave_ha, 4), # Proportion of total plantable area that is Pavement (in All Greenspace version)
    pave_npg_prop = round(pave_ha/total_gs_npg_pave_ha, 4), # Proportion of total plantable area that is Pavement (in NPG Greenspace version)
    pr_gard_prop = round(pr_gard_ha/gs_all_ha, 4), # Proportion of All Greenspace that is Private Gardens
    non_dom_priv_prop = round((gs_npg_ha - gs_pub_ha)/gs_all_ha, 4), # Proportion of All Greenspace that is Non-domestic (garden) private land
    public_prop = round(gs_pub_ha/gs_all_ha, 4) # Proportion of All Greenspace that is Public 
  )
# save out
write.csv(area_by_town, paste0(p.area.plantable, "/summary_plantable_area_town.csv"), row.names = FALSE)
View(area_by_town)

mean(area_by_town$gs_pub_prop); sd(area_by_town$gs_pub_prop) # 0.225
mean(area_by_town$gs_pg_prop); sd(area_by_town$gs_pg_prop) # 0.7167
mean(area_by_town$gs_non_dom_priv_prop); sd(area_by_town$gs_non_dom_priv_prop)  # 0.02166
mean(area_by_town$pave_all_prop); sd(area_by_town$pave_all_prop) # 0.03631


## Plot Data ##
area_by_town <- read.csv(paste0(p.area.plantable, "/summary_plantable_area_town.csv")) %>% 
  dplyr::mutate(region = factor(region))

# Define colour palettes 
scales::show_col(met.brewer("Veronese", n = 15,  type = "continuous"))
scales::show_col(met.brewer("VanGogh3", n = 15,  type = "continuous"))
show_col(met.brewer("Isfahan1", n = 15,  type = "continuous")) # gives hex codes

lc_colours <- c("Private Garden" = "#164E48", "Non-Dom Private" = "#6E948C", "Public GS" = "#B68F26", "Pavement" = "#7C461E") # "Veronese", n = 15
lc_colours2 <- c("Private Garden" = "#164E48", "Non-Dom Private" = "#67322E", "Public GS" = "#B78112", "Pavement" = "#929159") # "Veronese", n = 15
# Vangogh3 version 
lc_colours3 <-c("Pavement" = "#ae8548", "Public GS"= "#D4DDB8", "Non-Dom Private" = "#81AF73", "Private Garden" = "#1F5B25")
# Define labels for land cover 
lc_labels <- c("AGS", "NPG", "PGS")

# Define labels for regions 
region_labels <- c("E England", "E Midlands", "Gr London", "NE England", "NW England", 
                   "SE England", "SW England", "Wales", "W Midlands", "York-Humber")
# dataframe with region code and corresponding region label
region_names_df <- data.frame(region_code = levels(area_by_town$region), region_label = region_labels)


# Compare public vs private greenspace in towns
pr_v_pub  <- area_by_town %>%
  dplyr::select(c(TownsNM, region, gs_pg_prop, gs_non_dom_priv_prop, gs_pub_prop, pave_all_prop)) %>%
  pivot_longer(cols = c(gs_pg_prop, gs_non_dom_priv_prop, gs_pub_prop, pave_all_prop),names_to = "private_v_public", values_to = "prop_gs_area") %>%
  dplyr::mutate(private_v_public = factor(private_v_public),
                private_v_public = dplyr::recode(private_v_public,
                                                 "gs_pg_prop" = "Private Garden",
                                                 "gs_non_dom_priv_prop" = "Non-Dom Private",
                                                 "gs_pub_prop" = "Public GS",
                                                 "pave_all_prop" = "Pavement"))

## ~~ ##
# > 1.2 Regional summary for all LSOA -- Plantable area ####
## ~~ ##
# Plot for each region
regions_plot <- unique(pr_v_pub$region)
pr_pub_plot <- vector("list", length = length(regions_plot))
i <- 1
for(i in seq_along(regions_plot)){
  p <-  ggplot(subset(pr_v_pub, region == regions_plot[i]), aes(x = TownsNM, y = prop_gs_area, fill = private_v_public)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(y = "Prop. of plantable greenspace", x = regions_plot[i]) +
    scale_y_continuous(labels = comma, expand = c(0,0)) +
    scale_fill_manual(values = lc_colours,  
                      guide = guide_legend(title = "Greenspace Type",
                                           byrow = TRUE)) +
    theme_classic()+
    theme(axis.text.x = element_text(size = 14, color = "black",
                                     angle = 45, hjust = 1),
          axis.text.y = element_text(size = 14, color = "black"),
          axis.title.x = element_text(size = 16, color = "black"),
          axis.title.y = element_text(size = 16, color = "black"),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          plot.margin = unit(c(.5, .7, .5, .5), "cm"))
  pr_pub_plot[[i]] <- p
  # ggsave(plot = p, paste0(p.plots, "/greenspace_private_v_public_", regions_plot[i], ".png"),
  #        device = "png",
  #        width = 30, height = 18, units = "cm")
}
pr_pub_plot[[10]]


# Summarized for regions
area_by_region <- area_by_town %>%
  group_by(region) %>%
  summarise(
    gs_all_ha = sum(gs_all_ha), # Plantable area for All Gs
    gs_npg_ha = sum(gs_npg_ha), # Plantable area for NPG GS
    gs_pub_ha = sum(gs_pub_ha), # Plantable area for PUB GS
    pave_ha = sum(pave_ha), # PLantable area for pavement
    pr_gard_ha = sum(pr_gard_ha), # Plantable area for private gardens
    total_gs_all_pave_ha = sum(total_gs_all_pave_ha) # Total plantable area for All Greenspace and Pavement
  ) %>%
  ## change denominator to all plantable space (GS + pavement)
  mutate(
    gs_all_prop = round(gs_all_ha/total_gs_all_pave_ha, 4), # Proportion of all plantable area that is All Greenspace
    gs_pg_prop = round(pr_gard_ha/total_gs_all_pave_ha, 4), # Proportion of all plantable area that is private gardens 
    gs_npg_prop = round(gs_npg_ha/total_gs_all_pave_ha, 4), # Proportion of total plantable area that is NPG Greenspace
    gs_non_dom_priv_prop = round((gs_npg_ha - gs_pub_ha)/total_gs_all_pave_ha, 4), # Proportion of total plantable area that is Non-domestic (garden) private land
    gs_pub_prop = round(gs_pub_ha/total_gs_all_pave_ha, 4), # Proportion of total plantable area that is Pub Greenspace
    pave_all_prop = round(pave_ha/total_gs_all_pave_ha, 4), # Proportion of total plantable area that is Pavement (in All Greenspace version)
    pr_gard_prop = round(pr_gard_ha/gs_all_ha, 4), # proportion of all greenspace that is private garden
    non_dom_priv_prop = round((gs_npg_ha - gs_pub_ha)/gs_all_ha, 4), # Proportion of All Greenspace that is Non-domestic (garden) private land
    public_prop = round(gs_pub_ha/gs_all_ha, 4)) # Proportion of All Greenspace that is Public

# save out
write.csv(area_by_region, paste0(p.area.plantable, "/summary_plantable_area_region.csv"), row.names = FALSE)
View(area_by_region)

# PLot
area_by_region %>% 
  pivot_longer(cols = c(gs_pg_prop, gs_non_dom_priv_prop, gs_pub_prop, pave_all_prop), names_to = "lc_type", values_to = "prop_plant_area") %>%
  dplyr::mutate(lc_type = factor(lc_type, levels = c("gs_pg_prop","gs_non_dom_priv_prop", "pave_all_prop", "gs_pub_prop")),
                lc_type = dplyr::recode(lc_type,
                                        "gs_pg_prop" = "Private Garden",
                                        "gs_non_dom_priv_prop" = "Non-Dom Private",
                                        "gs_pub_prop" = "Public GS",
                                        "pave_all_prop" = "Pavement")) %>% 
ggplot(mapping = aes(x = region, y = prop_plant_area, fill = lc_type)) +
  geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.8) +
  labs(y = "Percent of plantable area", x = "Region") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) + # labels = comma
  scale_fill_manual(values = lc_colours2,
                    guide = guide_legend(title = "Land cover type",
                                         byrow = TRUE)) +
  scale_x_discrete(labels = region_labels) +
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black",
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.margin = unit(c(.5, .7, .5, .5), "cm"))
ggsave(paste0(p.plots.paper, "/greenspace_lc_type_by_region.png"), plot = last_plot(),
       device = "png",
       width = 30, height = 18, units = "cm")








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# > 1.2 Town summary for LSOA < 20% Canopy Cover -- plantable area  ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
area_national_df <- read.csv(paste0(p.area.plantable, "/plantable_area_lsoa_all_national.csv"))
#_lcc indicates low canopy cover versions
area_by_town_low <- area_national_df %>%
  dplyr::filter(prop_can_cover < target_cover) %>%
  dplyr::group_by(TownsNM, region) %>%
  summarise(
    gs_all_ha_lcc = sum(grsp_all_area_ha),
    gs_npg_ha_lcc = sum(grsp_npg_area_ha),
    pave_ha_lcc = sum(pave_area_ha)
  ) %>%
  mutate(
    pr_gard_ha_lcc = gs_all_ha_lcc - gs_npg_ha_lcc, # Plantable area of Private Gardens
    total_gs_all_pave_ha_lcc = gs_all_ha_lcc + pave_ha_lcc, # Total plantable area for All Greenspace and Pavement
    total_gs_npg_pave_ha_lcc = gs_npg_ha_lcc + pave_ha_lcc, # Total plantable area for NPG Greenspace and Pavement
    gs_all_prop_lcc = round(gs_all_ha_lcc/total_gs_all_pave_ha_lcc, 4), # Proportion of total plantable area that is All Greenspace
    gs_npg_prop_lcc = round(gs_npg_ha_lcc/total_gs_npg_pave_ha_lcc, 4), # Proportion of total plantable area that is NPG Greenspace
    pave_all_prop_lcc = round(pave_ha_lcc/total_gs_all_pave_ha_lcc, 4), # Proportion of total plantable area that is Pavement (in All Greenspace version)
    pave_npg_prop_lcc = round(pave_ha_lcc/total_gs_npg_pave_ha_lcc, 4), # Proportion of total plantable area that is Pavement (in NPG Greenspace version)
    pr_gard_prop_lcc = round(pr_gard_ha_lcc/gs_all_ha_lcc, 4), # Proportion of All Greenspace that is Private Gardens
    non_pg_prop_lcc = 1 - pr_gard_prop_lcc # Proportion of All Greenspace that is Public (i.e. not private gardens)
  )
# save out
write.csv(area_by_town_low, paste0(p.area.plantable, "/summary_plantable_area_town_20cc.csv"), row.names = F)



# ========================================================================================== #
#### 2. Calculate proportion of trees allocated to Greenspace vs Pavement - from rasters ####
# ========================================================================================== #

lsoa_simp <- st_read(paste0(p.lsoa.out, "/can_imd2025_lsoa2021_EngWales.gpkg"))%>%
  dplyr::select(c(PolyID, TownsNM, region))
urban_files <- list.files(p.regions.2021, pattern = "reference_boundary", full.name = T)
# Plantable layers, binary
pave_bin_files <- list.files(p.rast.town, pattern = "pavement_plantable_binary_", full.names = T)
gs_all_files <- list.files(p.rast.town, pattern = "grsp_park_buff_plantable_binary_", full.names = T)
gs_npg_files <- list.files(p.rast.town, pattern = "grsp_park_buff_plantable_wout_priv_garden_binary_", full.names = T)
gs_pub_files <- list.files(p.rast.town, pattern = "grsp_park_buff_plantable_public_binary_", full.names = T)
# Lists for national level outputs
summary_by_town_national <- vector("list", length = length(urban_files))
summary_by_lsoa_national <- vector("list", length = length(urban_files))

i <- 1
j <- 1
for (i in seq_along(urban_files)) {
  region_i <- sf::st_read(urban_files[[i]]) # Region boundary
  towns <- unique(region_i$TownsNM) # Towns in region
  reg_name_i <- unique(region_i$region)
  # Output lists
  summary_df_i <- vector("list", length = length(towns)) # output list for summary by LSOA for towns in each region -- all plantable cells
 
  # For each town within region i
  for(j in seq_along(towns)) {
    town_j <- region_i %>%
      dplyr::filter(TownsNM == towns[[j]])
    # Extent town boundary
    ext_j <- terra::ext(town_j)
    # Read in binary rasters of plantable cells
    pave_j <- terra::rast(pave_bin_files[[i]],  win = ext_j, snap = "near")
    gs_all_j <- terra::rast(gs_all_files[[i]],  win = ext_j, snap = "near")
    gs_npg_j <- terra::rast(gs_npg_files[[i]],  win = ext_j, snap = "near")
    gs_pub_j <- terra::rast(gs_pub_files[[i]],  win = ext_j, snap = "near")
    # Read in LSOA data for town j
    lsoa_j <- lsoa_simp %>%
      dplyr::filter(TownsNM == towns[[j]])

    ## Combine rasters for planting scenarios ##
    # Change values in pavement raster from 1 to 2
    pave_j <- terra::subst(pave_j, 1, 2)
    # ~ All Greenspace + Pavement ~ #
    gs_all_pave_j <- gs_all_j + pave_j
    names(gs_all_pave_j) <- "gs_all_pave_val"
    # ~ NPG Greenspace + Pavement ~ #
    gs_npg_pave_j <- gs_npg_j + pave_j
    names(gs_npg_pave_j) <- "gs_npg_pave_val"
    # ~ Pub Greenspace + Pavement ~ #
    gs_pub_pave_j <- gs_pub_j + pave_j
    names(gs_pub_pave_j) <- "gs_pub_pave_val"

    ### ~~~ All Plantable Cells in All LSOAs ~~~ ###

    ## Extract plantable cell values and join with lsoa data ##
    # ~~ All Greenspace + Pavement ~~ #
    val_gs_all_pave_j <- terra::as.points(gs_all_pave_j, values = TRUE, na.rm = T) %>%
      sf::st_as_sf() %>%
      dplyr::filter(gs_all_pave_val >=1)
    # Combine raster values with LSOA information
    gs_all_pave_lsoa_j <- sf::st_join(lsoa_j, val_gs_all_pave_j, .predicate = st_intersects) %>%
      sf::st_drop_geometry(.)
    # Summarize by LSOA the number of plantable cells for each landcover type
    summary_all_pave <- gs_all_pave_lsoa_j %>%
      group_by(PolyID) %>%
      summarise(
        gs_all_only = sum(gs_all_pave_val == 1), # All Greenspace cells only
        pave_only_all = sum(gs_all_pave_val == 2), # Pavement cells only (in the All Greenspace combination)
        gs_all_pave_overlap = sum(gs_all_pave_val == 3) # Cells plantable as All Greenspace and Pavement
      )%>%
      replace(is.na(.), 0) %>%
      dplyr::mutate(total_cells_all = rowSums(across(where(is.numeric))))

    # ~~ NPG Greenspace + Pavement ~~ #
    val_gs_npg_pave_j <- terra::as.points(gs_npg_pave_j, values = TRUE, na.rm = T) %>%
      sf::st_as_sf() %>%
      dplyr::filter(gs_npg_pave_val >=1)
    # Combine raster values with LSOA information
    gs_npg_pave_lsoa_j <- sf::st_join(lsoa_j, val_gs_npg_pave_j, .predicate = st_intersects) %>%
      sf::st_drop_geometry(.)
    # Summarize by LSOA
    summary_npg_pave <- gs_npg_pave_lsoa_j %>%
      group_by(PolyID) %>%
      summarise(
        gs_npg_only = sum(gs_npg_pave_val == 1), # NPG Greenspace cells only
        pave_only_npg = sum(gs_npg_pave_val == 2), # Pavement cells only (in the NPG Greenspace combination)
        gs_npg_pave_overlap = sum(gs_npg_pave_val == 3) # Cells plantable as NPG Greenspace and Pavement
      )%>%
      replace(is.na(.), 0) %>%
      dplyr::mutate(total_cells_npg = rowSums(across(where(is.numeric))))
    
    # ~~ PUB Greenspace + Pavement ~~ #
    val_gs_pub_pave_j <- terra::as.points(gs_pub_pave_j, values = TRUE, na.rm = T) %>%
      sf::st_as_sf() %>%
      dplyr::filter(gs_pub_pave_val >=1)
    # Combine raster values with LSOA information
    gs_pub_pave_lsoa_j <- sf::st_join(lsoa_j, val_gs_pub_pave_j, .predicate = st_intersects) %>%
      sf::st_drop_geometry(.)
    # Summarize by LSOA
    summary_pub_pave <- gs_pub_pave_lsoa_j %>%
      group_by(PolyID) %>%
      summarise(
        gs_pub_only = sum(gs_pub_pave_val == 1), # PUB Greenspace cells only
        pave_only_pub = sum(gs_pub_pave_val == 2), # Pavement cells only (in the PUB Greenspace combination)
        gs_pub_pave_overlap = sum(gs_pub_pave_val == 3) # Cells plantable as PUB Greenspace and Pavement
      )%>%
      replace(is.na(.), 0) %>%
      dplyr::mutate(total_cells_pub = rowSums(across(where(is.numeric))))
    
    # Combine planting scenario data sets ##
    summary_lsoa_j <- merge(summary_all_pave, summary_npg_pave, by = "PolyID") %>%
      dplyr::mutate(TownsNM = towns[[j]],
                    total_gs_all_cells = gs_all_only+gs_all_pave_overlap, # number of trees (cells) in all the greenspace even if it overlaps with pavement
                    total_gs_npg_cells = gs_npg_only + gs_npg_pave_overlap, # number of trees (cells) in the NPG greenspace even if it overlaps with pavement
                    trees_pg = total_gs_all_cells - total_gs_npg_cells, # number of trees in private gardens only
                    total_gs_pub_cells = gs_pub_only + gs_pub_pave_overlap # number of trees (cells) in the PUB greenspace even if it overlaps with pavement
      )
    # append to output list for region i
    summary_df_i[[j]] <- summary_lsoa_j
  }
  ### ~~~ All Plantable Cells in All LSOAs ~~~ ###
  # Bind together region data and append to output list
  summary_by_lsoa_i <- do.call(rbind, summary_df_i) %>%
    dplyr::mutate(region = reg_name_i)
  summary_by_lsoa_national[[i]] <- summary_by_lsoa_i

  # Summarize by town
  summary_towns_i <- summary_by_lsoa_i %>%
    group_by(TownsNM, region) %>%
    summarise(
      gs_all_only = sum(gs_all_only), # num greenspace cells in the All greenspace version (no overlap with pavement)
      pave_only_all = sum(pave_only_all), # num pavement cells in the All greenspace version (no overlap with greenspace)
      gs_all_pave_overlap = sum(gs_all_pave_overlap), # num cells that overlap b/w greenspace and pavement in the All Greenspace version
      trees_pg = sum(trees_pg), # number of cells (trees) in private garden greenspace only
      total_cells_all = sum(total_cells_all), # total number of plantable cells in All Greenspace version
     # ~ #
      gs_npg_only = sum(gs_npg_only), # num greenspace cells in the NPG greenspace version (no overlap with pavement)
      pave_only_npg = sum(pave_only_npg), # num pavement cells in the NPG greenspace version (no overlap with greenspace)
      gs_npg_pave_overlap = sum(gs_npg_pave_overlap), # num cells that overlap b/w greenspace and pavement in the NPG Greenspace version
      total_cells_npg = sum(total_cells_npg), # total number of plantable cells in NPG Greenspace version
     # ~ #
     gs_pub_only = sum(gs_pub_only), # num greenspace cells in the PUB greenspace version (no overlap with pavement)
     pave_only_pub = sum(pave_only_pub), # num pavement cells in the PUB greenspace version (no overlap with greenspace)
     gs_pub_pave_overlap = sum(gs_pub_pave_overlap), # num cells that overlap b/w greenspace and pavement in the PUB Greenspace version
     total_cells_pub = sum(total_cells_pub) # total number of plantable cells in PUB Greenspace version
    )
  # Append to national output
  summary_by_town_national[[i]] <- summary_towns_i
}
# All plantable cells
summary_by_lsoa_df <- do.call(rbind, summary_by_lsoa_national)
write.csv(summary_by_lsoa_df, paste0(p.area.plantable, "/lc_contribution_by_lsoa_plantable.csv"), row.names = F)
summary_by_town_df <- do.call(rbind, summary_by_town_national)
write.csv(summary_by_town_df, paste0(p.area.plantable, "/lc_contribution_by_town_plantable.csv"), row.names = F)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# > 2.1 Explore landcover contributions -- All Plantable cells ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
summary_by_town_df <- read.csv(paste0(p.area.plantable, "/lc_contribution_by_town_plantable.csv"))
summary_by_town_df <- summary_by_town_df %>% dplyr::mutate(total_gs_all_cells = gs_all_only + gs_all_pave_overlap, # number of trees (cells) in all the greenspace even if it overlaps with pavement
                                                           total_gs_npg_cells = gs_npg_only + gs_npg_pave_overlap, # number of trees (cells) in the NPG greenspace even if it overlaps with pavement
                                                           total_gs_pub_cells = gs_pub_only + gs_pub_pave_overlap) # number of trees (cells) in the PUB greenspace even if it overlaps with pavement


# Overall landcover contributions for each greenspace option and type of analysis
lc_cont <- summary_by_town_df %>%
  dplyr::select(-c(TownsNM, region)) %>%
  summarise(across(everything(), sum)) %>%
  dplyr::mutate(prop_gs_all = round(gs_all_only/total_cells_all, 4),
                prop_pave_all = round(pave_only_all/total_cells_all, 4),
                prop_overlap_all = round(gs_all_pave_overlap/total_cells_all, 4),
                prop_gs_npg = round(gs_npg_only/total_cells_npg, 4),
                prop_pave_npg = round(pave_only_npg/total_cells_npg, 4),
                prop_overlap_npg = round(gs_npg_pave_overlap/total_cells_npg, 4),
                prop_gs_pub = round(gs_pub_only/total_cells_pub, 4),
                prop_pave_pub = round(pave_only_pub/total_cells_pub, 4),
                prop_overlap_pub = round(gs_pub_pave_overlap/total_cells_pub, 4)
                ) %>%
  dplyr::select(contains("prop")) %>%
  pivot_longer(cols = starts_with("prop") , names_to = "scenario", values_to = "proportion") %>%
  dplyr::mutate(
    # gs_type = case_when(str_detect(scenario, "all") ~ "All Greenspace",
    #                                 str_detect(scenario, "npg") ~ "NPG Greenspace",
    #                                 str_detect(scenario, "pub") ~ "Public Greenspace"),
                land_cover = factor(case_when(str_detect(scenario, "gs") ~ "Greenspace",
                                              str_detect(scenario, "pave") ~ "Pavement",
                                              str_detect(scenario, "overlap") ~ "Greenspace or Pavement")),
                # Alternative GS labels
                gs_type = case_when(str_detect(scenario, "all") ~ "AGS",
                                    str_detect(scenario, "npg") ~ "NPG",
                                    str_detect(scenario, "pub") ~ "PGS"),
                analysis = "All Plantable Cells")



## Plot outputs ##

# Define colour palettes 
gs_colours <- c("Greenspace" = "#015e5d", "Greenspace or Pavement" = "#e9c281", "Pavement" = "#895a14") # met.brew palette "VanGogh3"
# Define labels for regions 
#region_labels <- c("All Greenspace", "No Private Gardens", "Public Greenspace")
gs_labels <- c("AGS", "NPG", "PGS")

ggplot(lc_cont, aes(x = gs_type, y = proportion, fill = land_cover)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  labs(y =  "Percent of plantable space", x = "Greenspace Scenario") +
  # scale_y_continuous(labels = comma, expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = c(0,0)) +
  scale_fill_manual(values = gs_colours,  
                    guide = guide_legend(title = "Surface type",
                                         byrow = TRUE
                    ),
                    labels = label_wrap(12)) +
  scale_x_discrete(labels = gs_labels) +
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.justification = "center",
        legend.key.spacing.y = unit(0.2, "cm"),
        plot.margin = unit(c(.5, .7, .5, .5), "cm")
  ) +
  ggview::canvas(units = "cm",  width = 30, height = 18)

ggsave(paste0(p.plots, "/land_cover_contribution.png"), plot = last_plot(),
       device = "png",
       width = 30, height = 18, units = "cm")




