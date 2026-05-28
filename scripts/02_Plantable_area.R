#### Calculating Area of Plantable Space for each Landcover Type ####

## Calculating the total area available for planting using the vector spatial data ##
## Examined for each planting scenario as a whole and for each land cover category separately ##

## Written by: Shari Mang ##
## Date written: 27/11/2025 ##
## Date last updated: 28/05/2026 ##

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
p.regions <- here("data/geography/") # urban boundaries
p.output <- here("data/outputs/") # output data from the assessment
p.lsoa.vars <-  here("data/lsoa_variables/") # variables summarised to LSOA level; canopy cover and IMD
p.plots <- here("data/outputs/plots/") # output plots
p.greenspace <- here("data/greenspace/") # Plantable greenspace polygons
p.pavement <- here("data/pavement/") # Plantable pavement polygons


## Parameters ##
crown_area <- 0.0032 # Assumed crown area in ha  ~~ 0.0032 = median crown area of current urban trees
crown_label <- 32 # crown area size for output labels
target_cover <- 0.20 # City level canopy cover target (20%)


# ========================================================== #
#### 1. Calculate Area of Plantable Space in All LSOAs  ####
# ========================================================== #

# Files #
grsp_all_poly <- list.files(p.greenspace, pattern = "plant_all_park_buff_", full.names = TRUE)
grsp_npg_poly <- list.files(p.greenspace, pattern = "plant_npg_park_buff_", full.name = TRUE)
grsp_pub_poly <- list.files(p.greenspace, pattern = "plant_public_park_buff_", full.name = TRUE)
pave_poly <- list.files(p.pavement, pattern = "gpkg", full.names = TRUE)
urban_files <- list.files(p.regions, pattern = "gpkg", full.name = T)
# LSOA
lsoa_dat <- st_read(paste0(p.lsoa.vars, "/can_imd2025_lsoa2021_EngWales.gpkg"))
# simplify LSOA data
lsoa_loop <- lsoa_dat %>%
  dplyr::select(-c(PolyName, lsoa_name, lsoa_area_m2, TCITY15CD, TCITY15NM))
# Output list
area_national_list <- vector("list", length = length(urban_files))
i <- 1
j <- 1
for(i in seq_along(urban_files)) {
  # Isolate region i
  region_i <- sf::st_read(urban_files[[i]])
  reg_name <- unique(region_i$region)
  towns <- region_i$TownsNM # Towns in region i
  # output list for each data type, remade for each region
  grsp_town_list <- vector("list", length = length(towns)) # All Greenspace output
  grsp_npg_town_list <- vector("list", length = length(towns)) # No Private garden output
  grsp_pub_town_list <- vector("list", length = length(towns)) # Public Greenspace output
  pave_town_list <- vector("list", length = length(towns)) # Pavement output

  ### For each town within region ###
  for(j in seq_along(towns)){
    # Reduce LSOAs to town j
    lsoa_j <- lsoa_loop %>%
      dplyr::filter(TownsNM == towns[[j]])
    # geometry for urban area
    city_geo_j <- st_as_text(st_geometry(region_i[region_i$TownsNM == towns[[j]],]))  # city boundary for town j

    ### Calculate area of plantable greenspace ###
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
  ## Combine area values for each land cover option into single dataframe ##
  # LSOA-level data for region i
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
    dplyr::left_join(select(grsp_reg_i, PolyID, grsp_all_area_ha), by = "PolyID") %>% # All Greenspace data
    dplyr::left_join(select(grsp_npg_reg_i, PolyID, grsp_npg_area_ha), by = "PolyID") %>% # No Private Garden data
    dplyr::left_join(select(grsp_pub_reg_i, PolyID, grsp_pub_area_ha), by = "PolyID") %>% # Public Greenspace data
    dplyr::left_join(select(pave_reg_i, PolyID, pave_area_ha), by = "PolyID") %>% # Pavement data
    dplyr::mutate(across(contains("area"), ~ replace_na(., 0))) %>%
    dplyr::mutate(grsp_all_pave_ha = grsp_all_area_ha + pave_area_ha, # Total plantable area for All Greenspace and Pavement
                  grsp_npg_pave_ha = grsp_npg_area_ha + pave_area_ha, # Total plantable area for NPG Greenspace and Pavement
                  grsp_pub_pave_ha = grsp_pub_area_ha + pave_area_ha) # Total plantable area for PUB Greenspace and Pavement

  # Save out for region i
  sf::st_write(area_lsoa_i, paste0(p.output, "/plantable_area_by_region/plantable_area_lsoa_all_", reg_name, ".gpkg"))
  # Append to national csv output
  area_national_list[[i]] <- area_lsoa_i %>% sf::st_drop_geometry()
}
# Bind national list into national df of areas per lsoa
area_national_df <- do.call(rbind, area_national_list)
# Save out
write.csv(area_national_df, paste0(p.output, "/plantable_area_lsoa_all_national.csv"), row.names = FALSE)



# ========================================= #
#### 2 Town Summary -- Plantable Area  ####
# ========================================= #
area_national_df <- read.csv(paste0(p.output, "/plantable_area_lsoa_all_national.csv"))
area_by_town <- area_national_df %>%
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
# Save out
write.csv(area_by_town, paste0(p.output, "/summary_plantable_area_town.csv"), row.names = FALSE)


# ============================================= #
#### 3. Regional Summary -- Plantable Area ####
# ============================================= #

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
# Save out
write.csv(area_by_region, paste0(p.output, "/summary_plantable_area_region.csv"), row.names = FALSE)


# =============== #
# > 3.1 Plots ####
# =============== #
# Colour palette 
lc_colours <- c("Private Garden" = "#164E48", "Non-Dom Private" = "#67322E", "Public GS" = "#B78112", "Pavement" = "#929159") # "Veronese", n = 15
# Define labels for land cover 
lc_labels <- c("AGS", "NPG", "PGS")
# Define labels for regions 
region_labels <- c("E England", "E Midlands", "Gr London", "NE England", "NW England", 
                   "SE England", "SW England", "Wales", "W Midlands", "York-Humber")
# dataframe with region code and corresponding region label
region_names_df <- data.frame(region_code = levels(area_by_town$region), region_label = region_labels)

# Reformat dataframe
area_by_region %>% 
  pivot_longer(cols = c(gs_pg_prop, gs_non_dom_priv_prop, gs_pub_prop, pave_all_prop), names_to = "lc_type", values_to = "prop_plant_area") %>%
  dplyr::mutate(lc_type = factor(lc_type, levels = c("gs_pg_prop","gs_non_dom_priv_prop", "pave_all_prop", "gs_pub_prop")),
                lc_type = dplyr::recode(lc_type,
                                        "gs_pg_prop" = "Private Garden",
                                        "gs_non_dom_priv_prop" = "Non-Dom Private",
                                        "gs_pub_prop" = "Public GS",
                                        "pave_all_prop" = "Pavement")) %>% 
  # Plot data
  ggplot(mapping = aes(x = region, y = prop_plant_area, fill = lc_type)) +
  geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.8) +
  labs(y = "Percent of plantable area", x = "Region") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) + 
  scale_fill_manual(values = lc_colours,
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
ggsave(paste0(p.plots, "/greenspace_lc_type_by_region.png"), plot = last_plot(),
       device = "png",
       width = 30, height = 18, units = "cm")


