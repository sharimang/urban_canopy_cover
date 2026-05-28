#### Assessing Number of Trees needed to Reach Town Level Target of 20% cover ####

## Calculating the number of trees needed for a city to reach 20% cover and the number of trees able to be planted in pavement and greenspace ##
## Done for 3 scenarios of greenspace: ##
## -- 1) All greenspace
## -- 2) Excluding Private Gardens
## -- 3) Public greenspace only

## First section is running through the spatial data for each region to get the numbers ##
## Second section is specific metrics to answer the various questions of interest and making plots and tables for paper ##

## Written by: Shari Mang ##
## Date written: 15/12/2025 ##
## Date last updated: 26/05/2026 ##

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
               MetBrewer)
conflicted::conflict_prefer("here", "here")
conflicted::conflicts_prefer(dplyr::filter)

## File paths ##
p.regions <- here("data/geography/") # urban boundaries
p.plantable.raster <- here("data/plantable_cells/") # raster data indicating if a cell is plantable (1) or not plantable (0) under the 3 planting scenarios
p.output <- here("data/outputs/") # output data from the assessment
p.lsoa.vars <-  here("data/lsoa_variables/") # variables summarised to LSOA level; canopy cover and IMD
p.plots <- here("data/outputs/plots/") # output plots

## Parameters ##
crown_area <- 0.0032 # Assumed crown area in ha  ~~ 0.0032 = median crown area of current urban trees
crown_label <- 32 # crown area size for output labels
target_cover <- 0.20 # City level canopy cover target (20%)


# =========================================================================== #
#### 1. Summarize number of plantable cells and trees needed in all LSOAs ####
# =========================================================================== #
## Assessed for each LSOA in the cities and then summarised to city level ##

## Calculated per LSOA:
## --- the number of plantable cells in each LSOA
## --- How many trees are needed 

## LSOA specific metric for how many trees needed to get 20% cover in each LSOA ## 
## Number of trees needed depends on crown area - defined in parameters below ##

can_lsoa <- st_read(paste0(p.lsoa.vars, "/can_imd2025_lsoa2021_EngWales.gpkg"))
urban_files <- list.files(p.regions, pattern = "reference_boundary", full.name = T)
# Plantable cells - all greenspace
plant_mask_files <- list.files(p.plantable.raster, pattern = "plantable_all_binary_mask", full.names = TRUE)
# Plantable cells - Private garden excluded greenspace
plant_no_pg_mask_files <- list.files(p.plantable.raster, pattern = "plantable_NPG_binary_mask_", full.names = TRUE)
# Plantable cells - public greenspace
plant_public_mask_files <- list.files(p.plantable.raster, pattern = "plantable_public_binary_mask_", full.names = TRUE)
# output database 
lsoa_cc_national <- vector("list", length = length(urban_files))
i <- 1
j <- 1
k <- 1
for(i in seq_along(urban_files)) {
  region_i <- sf::st_read(urban_files[[i]]) # Region boundary
  towns <- unique(region_i$TownsNM) # Towns in region
  reg_name_i <- unique(region_i$region)
  
  ### Regional outputs ###
  lsoa_data_i <- vector("list", length = length(towns)) # output list for dataframe of cell selection metrics by LSOA -- All and NPG in same output
  
  ### For each town within region ###
  for(j in seq_along(towns)) { 
    town_j <- region_i %>% 
      dplyr::filter(TownsNM == towns[[j]])
    # Extent town boundary
    ext_j <- terra::ext(town_j) 
    
    ### Read in plantability score raster for town j and make template ###
    # ~~~ All greenspace ~~~ #
    score_j <- terra::rast(plant_mask_files[[i]], win = ext_j, snap = "near")
    names(score_j) <- "plantable"
    rast_template <- score_j
    values(rast_template) <- NA # set values to NA
    # ~~~ NPG Greenspace ~~~ #
    npg_score_j <- terra::rast(plant_no_pg_mask_files[[i]], win = ext_j, snap = "near")
    names(npg_score_j) <- "npg_plantable"
    npg_rast_template <- npg_score_j
    values(npg_rast_template) <- NA # set values to NA
    # ~~~ Public Greenspace ~~~ #
    pub_score_j <- terra::rast(plant_public_mask_files[[i]], win = ext_j, snap = "near")
    names(pub_score_j) <- "pub_plantable"
    pub_rast_template <- pub_score_j
    values(pub_rast_template) <- NA # set values to NA
    
    ### LSOA specific metrics for achieving 20% cover ### 
    cc_j <- can_lsoa %>% 
      dplyr::filter(TownsNM == towns[[j]]) %>% 
      dplyr::mutate(target_can_area_ha = lsoa_area_ha*target_cover, # total tree cover required for LSOA to have 20% cover
                    req_can_area_ha = target_can_area_ha - can_area_ha, # the area needing to be planted to reach 30% cover
                    req_can_area_ha = case_when(req_can_area_ha <0 ~ 0,
                                                req_can_area_ha >0 ~ req_can_area_ha),
                    n_cell_needed = round(req_can_area_ha/crown_area)) # number of cells = crown area required/crown area per tree, with crown area set above as parameter
    
    # ~~~ All greenspace ~~~ #
    # Extract values from raster
    plantable_cells_j <- terra::as.points(score_j, values = TRUE, na.rm = TRUE) %>%
      sf::st_as_sf() %>%
      dplyr::mutate(cell_id = terra::cells(score_j))
    # Combine raster values with LSOA information 
    plantable_cells_lsoa_j <- sf::st_join(cc_j, plantable_cells_j, .predicate = st_intersects) %>%
      sf::st_drop_geometry(.) 

    # ~~~ NPG Greenspace ~~~ #
    # Extract values from raster
    npg_plantable_cells_j <- terra::as.points(npg_score_j, values = TRUE, na.rm = TRUE) %>%
      sf::st_as_sf() %>%
      dplyr::mutate(cell_id = terra::cells(npg_score_j))
    # Combine raster values with LSOA information
    npg_plantable_cells_lsoa_j <- sf::st_join(cc_j, npg_plantable_cells_j, .predicate = st_intersects) %>%
      sf::st_drop_geometry(.)
     
    # ~~~ Public Greenspace ~~~ #
    # Extract values from raster
    pub_plantable_cells_j <- terra::as.points(pub_score_j, values = TRUE, na.rm = TRUE) %>%
      sf::st_as_sf() %>%
      dplyr::mutate(cell_id = terra::cells(pub_score_j)) 
    # Combine raster values with LSOA information 
    pub_plantable_cells_lsoa_j <- sf::st_join(cc_j, pub_plantable_cells_j, .predicate = st_intersects) %>% 
      sf::st_drop_geometry(.) 
    
    ### Output objects for town j ###
    # loop through lsoas
    poly_id_j <- unique(plantable_cells_lsoa_j$PolyID) # --> Same polygon ID for all 3 greenspace scenarios
    
    ## Make output lists --> vector to record number of plantable cells in each lsoa in town j
    # ~~~ All Greenspace ~~~ #
    n_cell_plantable_j <- integer(length(poly_id_j))
    # ~~~ NPG Greenspace ~~~ #
    npg_n_cell_plantable_j <- integer(length(poly_id_j)) 
    # ~~~ Public Greenspace ~~~ #
    pub_n_cell_plantable_j <- integer(length(poly_id_j)) 
    
    for(k in seq_along(poly_id_j)) {
      # ~~~ All Greenspace ~~~ #
      lsoa_k <- plantable_cells_lsoa_j %>%
        dplyr::filter(PolyID == poly_id_j[[k]]) %>%
        dplyr::filter(plantable == 1)
      # Record the number of cells available in each LSOA
      n_cell_plantable_j[[k]] <- nrow(lsoa_k)

      # ~~~ NPG Greenspace ~~~ #
      npg_lsoa_k <- npg_plantable_cells_lsoa_j %>%
        dplyr::filter(PolyID == poly_id_j[[k]]) %>%
        dplyr::filter(npg_plantable == 1)
      # Record the number of cells available in each LSOA
      npg_n_cell_plantable_j[[k]] <- nrow(npg_lsoa_k)
      
      # ~~~ Public Greenspace ~~~ #
      pub_lsoa_k <- pub_plantable_cells_lsoa_j %>% 
        dplyr::filter(PolyID == poly_id_j[[k]]) %>% 
        dplyr::filter(pub_plantable == 1)
      # Record the number of cells available in each LSOA
      pub_n_cell_plantable_j[[k]] <- nrow(pub_lsoa_k)
    }
    ### Add cell information to lsoa data frame ###
    ## All Greenspace and NPG Greenspace together in same data frame ##
    cc_j <- cc_j %>% 
      # ~~~ All Greenspace ~~~ #
      dplyr::mutate(n_cell_plantable = n_cell_plantable_j, # total number of plantable cells
                    insufficient = case_when(n_cell_plantable < n_cell_needed ~ 1,
                                             n_cell_plantable > n_cell_needed ~ 0), # indicates if LSOA has insufficient cells: 1 = insufficient, 0 = sufficient
                    deficit = n_cell_needed - n_cell_plantable) %>% # number of cells (trees) needed - number of cells (trees) plantable
      # ~~~ NPG Greenspace ~~~ #
      dplyr::mutate(n_cell_plantable_npg = npg_n_cell_plantable_j, # total number of plantable cells
                    insufficient_npg = case_when(n_cell_plantable_npg < n_cell_needed ~ 1,
                                                 n_cell_plantable_npg > n_cell_needed ~ 0), # indicates if LSOA has insufficient cells: 1 = insufficient, 0 = sufficient
                    deficit_npg = n_cell_needed - n_cell_plantable_npg)  %>%
      dplyr::mutate(deficit = case_when(deficit <= 0 ~ 0,
                                        deficit > 0 ~ deficit),
                    deficit_npg = case_when(deficit_npg <= 0 ~ 0,
                                            deficit_npg > 0 ~ deficit_npg)) %>%
      # ~~~ Public Greenspace ~~~ #
      dplyr::mutate(n_cell_plantable_pub = pub_n_cell_plantable_j, # total number of plantable cells
                    insufficient_pub = case_when(n_cell_plantable_pub < n_cell_needed ~ 1, 
                                                 n_cell_plantable_pub > n_cell_needed ~ 0), # indicates if LSOA has insufficient cells: 1 = insufficient, 0 = sufficient 
                    deficit_pub = n_cell_needed - n_cell_plantable_pub,
                    deficit_pub = case_when(deficit_pub <= 0 ~ 0,
                                            deficit_pub > 0 ~ deficit_pub))  
    lsoa_data_i[[j]] <- cc_j # add to regional list of lsoa dataframes for each town
    
  }
  # Dataframe and SF of all LSOA info for region i
  # ~~~ All and NPG Greenspace and Public Greeenspace ~~~ #
  lsoa_cc_i <- do.call(rbind, lsoa_data_i)
  lsoa_cc_national[[i]] <- lsoa_cc_i %>%  sf::st_drop_geometry() # Save csv nationally
}
cell_summary_df <- do.call(rbind, lsoa_cc_national)
# Save out 
write.csv(cell_summary_df, paste0(p.output, "/plantable_cells_all_lsoa_crown", crown_label, "_national.csv"), row.names = F)



# ==================================================================== #
#### 2. Make summary tables for achieving 20% cover at Town level ####
# ==================================================================== #

## Questions: ##
# 1. What proportion of towns have enough space to reach 20% canopy cover?
# 2. How many towns already have 20% cover?
# 3. Of the cities that don't have 20% cover, how many can achieve it? How does it vary across planting scenarios?
# 4. What is the canopy cover if all areas are planted?
# 5. How many tree can be planted under each scenario?
# 6. How many trees does each city need to reach 20% cover?

# Read in data
all_lsoa_plantable <- read.csv(paste0(p.output, "/plantable_cells_all_lsoa_crown", crown_label, "_national.csv"))

# make imd quantiles/deciles factors
all_lsoa_plantable  <- all_lsoa_plantable %>%
  dplyr::mutate(imd_national_decile = as.factor(imd_national_decile),
                imd_national_quantile = as.factor(imd_national_quantile),
                imd_town_quantile = as.factor(imd_town_quantile),
                region = factor(region))


# ======================================================================== #
# > 2.1 Summary of potential canopy cover and number of trees by town ####
# ======================================================================== #
# If all plantable space was used - what is the potential canopy cover per LSOA and per town

### Note: ###
# n_cell_needed in all_lsoa_plantable refers to number needed for that LSOA to achieve 20% cover. Need to recalculate for towns.
# Planting scenarios distinguished by suffix 
# -- no suffix == All Greenspace 
# -- _npg == No Private Gardens 
# -- _pub == Public Greenspace only

## Summary of potential canopy cover by town ##
town_summary <- all_lsoa_plantable %>%
  group_by(TownsNM, region) %>%
  dplyr::summarise(.groups = "drop_last",
    town_area_ha = sum(lsoa_area_ha),
    current_can_ha = sum(can_area_ha),
    current_can_prop = round(current_can_ha/town_area_ha, 4), # Current canopy cover
    n_trees_plantable = sum(n_cell_plantable), # Total number of trees plantable (maximizing all plantable cells)
    n_trees_plantable_npg = sum(n_cell_plantable_npg),
    n_trees_plantable_pub = sum(n_cell_plantable_pub),
    gained_can_ha = n_trees_plantable * crown_area, # The gained canopy area if all trees planted
    gained_can_npg_ha = n_trees_plantable_npg * crown_area,
    gained_can_pub_ha = n_trees_plantable_pub * crown_area,
    potential_can_ha = round(current_can_ha + gained_can_ha, 4), # Potential total canopy area (current + gained) if all trees planted
    potential_can_npg_ha = round(current_can_ha + gained_can_npg_ha, 4),
    potential_can_pub_ha = round(current_can_ha + gained_can_pub_ha, 4),
    potential_can_prop = round((potential_can_ha/town_area_ha), 4), # Potential proportion canopy cover if all trees planted
    potential_can_npg_prop = round((potential_can_npg_ha/town_area_ha), 4),
    potential_can_pub_prop = round((potential_can_pub_ha/town_area_ha), 4),
    shortfall_prop = target_cover - potential_can_prop, # Shortfall from canopy cover target; Positive value is the shortfall, negative value is the surplus
    shortfall_npg_prop = target_cover - potential_can_npg_prop,
    shortfall_pub_prop = target_cover - potential_can_pub_prop 
  )
write.csv(town_summary, paste0(p.output, "/summary_town_plantable_cells_crown", crown_label, ".csv"), row.names = F)

# Mean and SD canopy cover 
# All Cities
mean(town_summary$current_can_prop); sd(town_summary$current_can_prop)  
# England
mean(town_summary$current_can_prop[town_summary$region != "Wales"]); sd(town_summary$current_can_prop[town_summary$region != "Wales"]) 
# Wales
mean(town_summary$current_can_prop[town_summary$region == "Wales"]); sd(town_summary$current_can_prop[town_summary$region == "Wales"]) 


# =================================================================== #
# > 2.2 Number of trees needed to reach 20% canopy cover by town ####
# =================================================================== #

n_trees_20_town <- town_summary %>%
  dplyr::select(c(TownsNM, region, town_area_ha, current_can_ha, current_can_prop, n_trees_plantable, n_trees_plantable_npg, n_trees_plantable_pub)) %>%
  dplyr::mutate(target_can_area_ha = town_area_ha*target_cover, # total tree cover required for town to reach target cover (20%)
                req_can_area_ha = target_can_area_ha - current_can_ha, # the area needing to be planted to reach target cover
                req_can_area_ha = case_when(req_can_area_ha < 0 ~ 0, # Remove (-) for those that don't need additional tree cover
                                            req_can_area_ha > 0 ~ req_can_area_ha),
                n_trees_needed = round(req_can_area_ha/crown_area), # number of trees needed with assumed crown area to reach the target cover
                deficit = n_trees_needed - n_trees_plantable, # difference between trees needed and plantable to reach 20% cover
                deficit_npg = n_trees_needed - n_trees_plantable_npg, 
                deficit_pub = n_trees_needed - n_trees_plantable_pub) %>%
  dplyr::mutate(deficit = case_when(deficit < 0 ~ 0, # Remove (-) for those that don't have a deficit
                                    deficit > 0 ~ deficit),
                deficit_npg = case_when(deficit_npg < 0 ~ 0,
                                        deficit_npg > 0 ~ deficit_npg),
                deficit_pub = case_when(deficit_pub < 0 ~ 0,
                                        deficit_pub > 0 ~ deficit_pub))
write.csv(n_trees_20_town, paste0(p.output, "/summary_town_num_trees_crown", crown_label, ".csv"), row.names = F)
sum(n_trees_20_town$n_trees_needed) # 9.46 million trees needed



# ======================================================= #
#### 3.0 Plots and Summary Tables for Main Questions ####
# ======================================================= #
# Read in data
town_summary <- read.csv(paste0(p.output, "/summary_town_plantable_cells_crown", crown_label, ".csv")) %>% 
  dplyr::mutate(region = factor(region))
n_trees_20_town <- read.csv(paste0(p.output, "/summary_town_num_trees_crown", crown_label, ".csv"))
all_lsoa_plantable <- read.csv(paste0(p.output, "/plantable_cells_all_lsoa_crown", crown_label, "_national.csv"))

## > Plotting set up ####
# Define colour palettes 
scenario_colours <- c("Needed" = "#ae8548", "PGS"= "#D4DDB8", "NPG" = "#81AF73", "AGS" = "#1F5B25")
scenario2_colours <- c("Current" = "#ae8548", "PGS"= "#D4DDB8", "NPG" = "#81AF73", "AGS" = "#1F5B25")
# Define labels for regions 
region_labels <- c("E England", "E Midlands", "Gr London", "NE England", "NW England", 
                   "SE England", "SW England", "Wales", "W Midlands", "York-Humber")
# dataframe with region code and corresponding region label
region_names_df <- data.frame(region_code = levels(all_lsoa_plantable$region), region_label = region_labels)


# ========================================================= #
# > Q1 Proportion of towns that could get to 20% cover ####
# ========================================================= #
target_town <- town_summary %>%
  dplyr::group_by(region) %>%
  summarise(
    n_towns = n(),  # number of towns in region
    n_current_20per = sum(current_can_prop >= target_cover), # current number of towns that meet 20% target
    n_current_low_cc = sum(current_can_prop <= target_cover), # current number of towns that don't meed 20% target
    n_20per_all_gs = sum(potential_can_prop >= target_cover), # number of towns that meet 20% target -- scenario = all greenspace 
    prop_20perc_all_gs = round(sum(potential_can_prop >= target_cover)/n_towns, 4), # proportion of towns that meet 20% target -- scenario = all greenspace 
    n_20per_npg_gs = sum(potential_can_npg_prop >= target_cover), # number of towns that meet 20% target -- scenario = no private gardens
    prop_20perc_npg_gs = round(sum(potential_can_npg_prop >= target_cover)/n_towns, 4), # proportion of towns that meet 20% target -- scenario = no private gardens 
    n_20per_pub_gs = sum(potential_can_pub_prop >= target_cover), # number of towns that meet 20% target -- scenario = public greenspace
    prop_20perc_pub_gs = round(sum(potential_can_pub_prop >= target_cover)/n_towns, 4)  # proportion of towns that meet 20% target -- scenario = public greenspace
  )


# =============================================== #
# > Q2 How many towns already have 20% cover ####
# =============================================== #
# Summarize number of towns in each region that have >=20% cover
current_town <- target_town %>%
  dplyr::select(region, n_towns, n_current_20per) %>%
  mutate(prop_current_20 = round(n_current_20per/n_towns, 4))


# ========================================================================= #
# > Q3 How many cities of those below the target can achieve 20% cover ####
# ========================================================================= #

## Regional summary table ##
# Reduce to towns starting with < 20% canopy cover
target_low_cc_town <- town_summary %>%
  dplyr::filter(current_can_prop < 0.20) %>% 
  dplyr::group_by(region) %>%
  summarise(
    n_towns_low_cc = n(), # Number of towns in region
    n_increase_all_gs = sum(potential_can_prop >= target_cover), # number of towns that have increased their cover to target -- scenario = all greenspace
    prop_increase_all_gs = round(sum(potential_can_prop >= target_cover)/n_towns_low_cc, 4),  # proportion of towns that have increased their cover to target -- scenario = all greenspace
    n_increase_npg_gs = sum(potential_can_npg_prop >= target_cover), # number of towns that have increased their cover to target -- scenario = no private gardens
    prop_increase_npg_gs = round(sum(potential_can_npg_prop >= target_cover)/n_towns_low_cc, 4), # proportion of towns that have increased their cover to target -- scenario = no private gardens
    n_increase_pub_gs = sum(potential_can_pub_prop >= target_cover), # number of towns that have increased their cover to target -- scenario = public greenspace
    prop_increase_pub_gs = round(sum(potential_can_pub_prop >= target_cover)/n_towns_low_cc, 4) # proportion of towns that have increased their cover to target -- scenario = public greenspace
  )


# ============================================================ #
# > Q4 What is the canopy cover if all areas are planted? ####
# ============================================================ #

## Plot regional variation ##
# Format data
town_summary %>%
  dplyr::select(c(TownsNM, region, current_can_prop, potential_can_prop, potential_can_npg_prop, potential_can_pub_prop)) %>%
  pivot_longer(cols = c(current_can_prop, potential_can_prop, potential_can_npg_prop, potential_can_pub_prop),
               names_to = "scenario", values_to = "prop_cover") %>% 
  dplyr::mutate(scenario = factor(scenario, levels = c("current_can_prop", "potential_can_pub_prop", "potential_can_npg_prop", "potential_can_prop")),
                scenario = dplyr::recode(scenario, 
                                         "current_can_prop" = "Current",
                                         "potential_can_npg_prop" = "NPG", # "No Private Garden"
                                         "potential_can_prop" = "AGS", # "All GS"
                                         "potential_can_pub_prop" = "PGS")) %>%  # "Public GS"
# plot variation across regions
ggplot(mapping = aes(x = region, y = prop_cover, fill = scenario)) +
  geom_boxplot() +
  geom_hline(yintercept=0.2, linetype="dashed", color = "#67322E", linewidth = 0.75) +
  labs(y = "Percent canopy cover",
       x = "Region") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = scenario2_colours, 
                    guide = guide_legend(title = "Scenario",
                                         byrow = TRUE)) +
  scale_x_discrete(labels = region_labels) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14, color = "black",
                                   angle = 45, hjust = 0.9),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
        ) 
ggsave(paste0(p.plots, "/canopy_cover_by_region_crown", crown_label, ".png"), plot = last_plot(),
       device = "png",
       width = 25, height = 15, units = "cm")


# =============================================================================================== #
# > Q5 and Q6 Number of trees that could be planted and Number of trees needed for 20% cover ####
# =============================================================================================== #

## Plot regional variation ##
# Summarise by region
n_trees_20_town %>%
  dplyr::select(c(TownsNM, region, n_trees_needed, n_trees_plantable, n_trees_plantable_npg, n_trees_plantable_pub)) %>%
  group_by(region) %>%
  summarise(n_trees_needed = sum(n_trees_needed),
            n_trees_plantable = sum(n_trees_plantable),
            n_trees_plantable_npg = sum(n_trees_plantable_npg),
            n_trees_plantable_pub = sum(n_trees_plantable_pub)) %>%
  pivot_longer(cols = c( n_trees_needed, n_trees_plantable, n_trees_plantable_npg, n_trees_plantable_pub),
               names_to = "scenario", values_to = "n_trees") %>%
  dplyr::mutate(scenario = factor(scenario, levels = c("n_trees_needed", "n_trees_plantable_pub", "n_trees_plantable_npg", "n_trees_plantable")),
                scenario = dplyr::recode(scenario,
                                         "n_trees_needed" = "Needed",
                                         "n_trees_plantable" = "AGS", # "All GS"
                                         "n_trees_plantable_npg" = "NPG", #"No Private Garden"
                                         "n_trees_plantable_pub" = "PGS")) %>% # "Public GS"
  # Plot
  ggplot(mapping = aes(x = region, y = n_trees, fill = scenario)) +
  geom_bar(position = "dodge", stat= "identity", colour = "black") +
  xlab("Region") + 
  ylab("Number of Trees (millions)") +
  scale_y_continuous(expand = c(0,0), , limits = c(0, 6000000),
                     labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  scale_fill_manual(values = scenario_colours,
                     guide = guide_legend(title = "Scenario",
                                          byrow = TRUE))+
  scale_x_discrete(labels = region_labels) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14, color = "black",
                                   angle = 45, hjust = 0.9),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 16, color = "black",
                                    margin = margin(t = -5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.margin = unit(c(.5, 0.3, 0.5, 0.3), "cm")
        )
ggsave(paste0(p.plots, "/number_trees_by_region_crown", crown_label, ".png"), plot = last_plot(),
       device = "png",
       width = 25, height = 15, units = "cm")


# Number of trees needed in england and wales
sum(n_trees_20_town$n_trees_needed)
# Wales only
sum(n_trees_20_town$n_trees_needed[n_trees_20_town$region == "Wales"])

