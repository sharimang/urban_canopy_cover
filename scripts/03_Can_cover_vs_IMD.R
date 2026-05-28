#### Relationship between Canopy Cover and Deprivations at City Level ####

## Evaluating the relationship between Index of Multiple Deprivation (city level) and canopy cover metrics ##
## IMD Rank is assigned at LSOA level so created a population-weighted average for city level following methods outlines by Office of National Statistics ##
## --- ONS methods outlined in English Indices of Deprivation 2025 - Research Report ##

## Note that IMD Rank is *ordinal* not continuous --> the step between ranks is unknown and unequal. ##

## Written by: Shari Mang ##
## Date written: 09/03/2026
## Date last modified: 28/05/2026 ##


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
               janitor,
               here, 
               conflicted,
               scales,
               MetBrewer,
               ggplot2
               )
conflicted::conflict_prefer("here", "here")
conflicted::conflicts_prefer(dplyr::filter)

## File paths ##
p.imd <- here("data/imd/")
p.output <- here("data/outputs/") # output data from the assessment
p.population <- here("data/population/")
p.plots <- here("data/outputs/plots/") # output plots

## Parameters ##
crown_area <- 0.0032 # Assumed crown area in ha  ~~ 0.0032 = median crown area of current urban trees
crown_label <- 32 # crown area size for output labels
target_cover <- 0.20 # City level canopy cover target (20%)


# =============================================================================== #
#### 1.0 Calculate Cities' Average IMD Rank; Combine w/ Plantable Cells Data ####
# =============================================================================== #
## Note from UK Gov: when making IMD ranking for Local Authority or City, they use a population weighted average 
## -- "Population weighted average of the combined ranks for the LSOAs in a larger area." 
## -- "This measure is calculated by averaging all of the LSOA ranks in each larger area after they have been population weighted." 
## -- Ranking is reversed prior to calculating average so that 1 becomes least deprived 
## ---- "For the purpose of calculating the score for the larger area, LSOAs are ranked such that the most deprived LSOA is given the rank of 33,755"
## -- After averaging, the values are ordered and re-ranked such that 1 is most deprived so as to be consistent with the standard IMD metric ##
## IMD must always be evaluated separately for each nation ##


# =========================================== #
# > 1.1 Combine IMD with population data ####
# =========================================== #

## Use full national data set so that reverse ranking are reflective of national standing ##
imd_ew <- read.csv(paste0(p.imd, "/national_imd2025_lsoa2021_EngWales.csv"))
# Note: IMD = 1 is most deprived; higher IMD = lower deprivation

## England ##
# reorder to reverse the IMD Rank values -- needed for averaging IMD for towns
imd_eng <- imd_ew %>% dplyr::filter(nation == "England") %>% 
  dplyr::arrange(desc(imd_rank)) %>% 
  dplyr::mutate(imd_rank_reverse = row_number()) # Now IMD = 1 is least deprived
## Wales ##
imd_wales <- imd_ew %>% dplyr::filter(nation == "Wales") %>% 
  dplyr::arrange(desc(imd_rank)) %>% 
  dplyr::mutate(imd_rank_reverse = row_number()) # Now IMD = 1 is least deprived
# Recombine England and Wales 
imd_ew <- rbind(imd_eng, imd_wales)

## Combine IMD rank with plantable cell data for LSOAs ##
# Plantable cells data 
all_lsoa_plantable <- read.csv(paste0(p.output, "/plantable_cells_all_lsoa_crown", crown_label, "_national.csv"))
# Append on reversed IMD rank
all_lsoa_plantable <- merge(all_lsoa_plantable, imd_ew[c("PolyID", "imd_rank_reverse")], by = "PolyID")

## Add on population data ##
pop_ew <- read.csv(paste0(p.population, "/population_Mid-2022 LSOA 2021.csv"))
pop_ew <- pop_ew %>% 
  janitor::clean_names(.) %>% 
  dplyr::select(c(lsoa_2021_code, total)) %>% 
  dplyr::rename(population = total,
                PolyID = lsoa_2021_code) 
# Append on population data
all_lsoa_plantable <- merge(all_lsoa_plantable, pop_ew, by = "PolyID")
# Save out version - info on plantable cells, reversed imd rank, and population
write.csv(all_lsoa_plantable, paste0(p.output, "/plantable_cells_all_lsoa_crown", crown_label, "_national_population.csv"), row.names = F)


# =================================================== #
# > 1.2 Calculate average IMD Rank for each city ####
# =================================================== #
all_lsoa_plantable <- read.csv(paste0(p.output, "/plantable_cells_all_lsoa_crown", crown_label, "_national_population.csv"))

town_imd <- all_lsoa_plantable %>%
  group_by(TownsNM,region) %>%
  summarise(
    city_population = sum(population, na.rm = TRUE), # city population
    avg_imd_rank = round(sum(imd_rank_reverse * population, na.rm = TRUE) / city_population, 1) # population-weighted average using the reversed order ranking
  ) # Currently: min value average rank = least deprived; max average rank = most deprived


## Rank the average IMD ranks ##
# -- Done separately for England and Wales #
## England ##
town_imd_eng <- town_imd %>% 
  dplyr::filter(region != "Wales") %>% 
  dplyr::arrange(desc(avg_imd_rank)) %>% # Inverse them so it's back to the original way IMD is ranked where lowest value = most deprived
  as.data.frame(row.names = 1:nrow(.)) %>% 
  dplyr::mutate(ranked_avg_imd_rank = row_number()) # apply ordered ranking; 1 = most deprived; 141 = least deprived
## Wales ##
town_imd_wls <- town_imd %>% 
  dplyr::filter(region == "Wales") %>% 
  dplyr::arrange(desc(avg_imd_rank)) %>% # Inverse them so it's back to the original way IMD is ranked where lowest value = most deprived
  as.data.frame(row.names = 1:nrow(.)) %>% 
  dplyr::mutate(ranked_avg_imd_rank = row_number()) # apply ordered ranking; 1 = most deprived; 3 = least deprived
# Combine England and Wales again 
town_imd <- rbind(town_imd_eng, town_imd_wls)

## Combine with plantable cell summary for towns ##
# Data by Town 
town_summary <- read.csv(paste0(p.output, "/summary_town_plantable_cells_crown", crown_label, ".csv"))
town_summary <- merge(town_summary, town_imd, by = c("TownsNM", "region"))
# Save out a version
write.csv(town_summary, paste0(p.output, "/summary_town_plantable_cells_crown", crown_label, "_avg_imd.csv"), row.names = F)



# ============================================================= #
#### 2.0 Explore relationship between Canopy Cover and IMD ####
# ============================================================= #

# =================== #
# > 2.1 Prep data ####
# =================== #
town_summary <- read.csv(paste0(p.output, "/summary_town_plantable_cells_crown", crown_label, "_avg_imd.csv"))
# Isolate England cities
eng_town_summary <- town_summary %>% filter(region != "Wales")

# Truncate shortfall metric so that those with a canopy cover >=20% (a negative shortfall) have a shortfall of 0
eng_town_summary <- eng_town_summary %>% 
  mutate(shortfall_prop_trunc = case_when(shortfall_prop <=0 ~ 0,
                                          shortfall_prop >0 ~ shortfall_prop),
         shortfall_npg_prop_trunc = case_when(shortfall_npg_prop <=0 ~ 0,
                                              shortfall_npg_prop >0 ~ shortfall_npg_prop),
         shortfall_pub_prop_trunc = case_when(shortfall_pub_prop <=0 ~ 0,
                                              shortfall_pub_prop >0 ~ shortfall_pub_prop)) 

# Subset data to cities with current cover <20% -- places that require tree planting 
eng_town_low_cc <- eng_town_summary %>%  
  dplyr::filter(current_can_prop < 0.20)


# ========================================================================= #
# > Q1 What is the relationship between existing canopy cover and IMD? ####
# ========================================================================= #

# Canopy cover distribution 
ggplot(eng_town_summary, aes(x = current_can_prop)) + 
  geom_histogram(col = "white") +
  labs(y = "Number of observations", x = "Current canopy cover", title = "") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

# Correlation between IMD and current canopy cover
imd_v_can <- cor.test(eng_town_summary$ranked_avg_imd_rank, eng_town_summary$current_can_prop, method = "spearman") # Spearman as rank is ordinal

# Plot relationship b/w imd and canopy cover
ggplot(eng_town_summary, aes(x = ranked_avg_imd_rank, y = current_can_prop)) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 0.31), expand = c(0,0),
                     breaks=seq(0, 0.31, by = 0.10),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0,142), expand = c(0,0), 
                     breaks=seq(0, 140, by = 20)) +
  annotate("text", x = 20, y = 0.3, size = 5, 
           label = paste0(
             "italic(R[s]) == ",
             round(imd_v_can$estimate, 2)),
           parse = TRUE) +
  annotate("text", x = 20, y = 0.28, size = 5, 
           label = "italic(p) < 0.001",
           parse = TRUE
  ) +
  smplot2::sm_statCorr(
    show_text = FALSE,
    corr_method = "spearman",
    fit.params = list(
      color = "#cf1c00",
      linetype = "dashed"
    )
  )+
  xlab("Ranked Average IMD Rank") +
  ylab("Current canopy cover (%)") +
  theme_classic()+ 
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black")) 
ggsave(paste0(p.plots, "/current_can_by_avg_IMD_spearman.png"), plot = last_plot(),
       device = "png",
       width = 25, height = 15, units = "cm")


# ================================================================================================== #
# > Q2 Of those who currently have <20% cover, does the shortfall from 20% cover relate to IMD? ####
# ================================================================================================== #
## Use dataset for cities that currently have < 20% canopy cover ##
## Use variable with only shortfall but no surplus ##

# >> Scenario for No Private Garden Greenspace ####
# Shortfall distribution
ggplot(eng_town_low_cc, aes(x = shortfall_npg_prop_trunc)) +
  geom_histogram(col = "white") +
  labs(y = "Number of observations", x = "Cover shortfall (NPG)", title = "") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

# Correlation between IMD and shortfall under No Private Garden scenario
imd_v_short_npg <- cor.test(eng_town_low_cc$ranked_avg_imd_rank, eng_town_low_cc$shortfall_npg_prop_trunc, method = "spearman")

# Plot relationship b/w IMD rank and canopy cover shortfall 
ggplot(eng_town_low_cc, aes(x = ranked_avg_imd_rank, y = shortfall_npg_prop_trunc)) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 0.15),
                     breaks=seq(0, 0.15, by = 0.05),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0,142), expand = c(0,0),
                     breaks=seq(0, 140, by = 20)) +
  annotate("text", x = 20, y = 0.15, size = 5, 
           label = paste0(
             "italic(R[s]) == ",
             round(imd_v_short_npg$estimate, 2)
           ),
           parse = TRUE) +
  annotate("text", x = 20, y = 0.14, size = 5, 
           label = paste0(
             "italic(p) == ",
             round(imd_v_short_npg$p.value, 2)
           ),
           parse = TRUE) +
  smplot2::sm_statCorr(
    show_text = FALSE,
    corr_method = "spearman",
    fit.params = list(
      color = "#cf1c00",
      linetype = "dashed"
    )
  )+
  xlab("Ranked Average IMD Rank") +
  ylab("Shortfall from 20% target (%)") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"),
        plot.margin = unit(c(.5, 0.3, 0.5, 0.3), "cm")) 
ggsave(paste0(p.plots, "/shortfall_npg_by_avg_IMD_spearman.png"), plot = last_plot(),
       device = "png",
       width = 25, height = 15, units = "cm")

# >> Scenario for Public Greenspace ####
# Shortfall distribution
ggplot(eng_town_low_cc, aes(x = shortfall_pub_prop_trunc)) +
  geom_histogram(col = "white") +
  labs(y = "Number of observations", x = "Cover shortfall (PUB)", title = "") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

# Correlation between IMD and shortfall under Public Greenspace scenario
imd_v_short_pub <- cor.test(eng_town_low_cc$ranked_avg_imd_rank, eng_town_low_cc$shortfall_pub_prop_trunc, method = "spearman")

# Plot relationship b/w IMD rank and canopy cover shortfall 
ggplot(eng_town_low_cc, aes(x = ranked_avg_imd_rank, y = shortfall_pub_prop_trunc)) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 0.15), 
                     breaks=seq(0, 0.15, by = 0.05),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0,142), expand = c(0,0),
                     breaks=seq(0, 140, by = 20)) +
  annotate("text", x = 20, y = 0.15, size = 5, 
           label = paste0(
             "italic(R[s]) == ",
             round(imd_v_short_pub$estimate, 2)), 
           parse = TRUE) +
  annotate("text", x = 20, y = 0.14, size = 5, 
           label = paste0(
             "italic(p) == ",
             round(imd_v_short_pub$p.value, 2)), 
           parse = TRUE) +
  smplot2::sm_statCorr(
    show_text = FALSE,
    corr_method = "spearman",
    fit.params = list(
      color = "#cf1c00",
      linetype = "dashed"
    )
  )+
  xlab("Ranked Average IMD Rank") +
  ylab("Shortfall from 20% target (%)") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"),
        plot.margin = unit(c(.5, 0.3, 0.5, 0.3), "cm")) 
ggsave(paste0(p.plots, "/shortfall_pub_by_avg_IMD_spearman.png"), plot = last_plot(),
         device = "png",
         width = 25, height = 15, units = "cm")


# =================================================================================== #
# > Q3 Does the ratio of Private Gardens to Public Greenspace correlate with IMD? ####
# =================================================================================== #
# Read in data 
area_by_town <- read.csv(paste0(p.output, "/summary_plantable_area_town.csv"))
town_summary <- read.csv(paste0(p.output, "/summary_town_plantable_cells_crown", crown_label, "_avg_imd.csv"))
# Reduce to England cities
eng_avg_imd <- town_summary %>% 
  filter(region != "Wales") %>% 
  dplyr::select(c(TownsNM, ranked_avg_imd_rank))
# Combine datasets
eng_avg_imd <- merge(area_by_town, eng_avg_imd, by = "TownsNM")
# Calculate ratio of private to public 
eng_avg_imd <- eng_avg_imd %>% 
  dplyr::mutate(priv_pub_ratio = pr_gard_ha/gs_pub_ha) 

# Ratio distribution 
ggplot(eng_avg_imd, aes(x = priv_pub_ratio)) + 
  geom_histogram(col = "white") +
  labs(y = "Number of observations", x = "Ratio private:public", title = "") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

# Correlation between ratio and IMD 
pub_v_priv <- cor.test(eng_avg_imd$ranked_avg_imd_rank, eng_avg_imd$priv_pub_ratio, method = "spearman")

# Plot relationship b/w ratio and IMD
ggplot(eng_avg_imd, aes(x = ranked_avg_imd_rank, y = priv_pub_ratio)) +
  geom_point(size = 2.5) + 
  scale_y_continuous(limits = c(0,9), breaks=seq(0, 8, by = 2),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(0,142), expand = c(0,0),
                     breaks=seq(0, 140, by = 20)) +
  annotate("text", x = 18, y = 8.5, size = 5, 
           label = paste0(
             "italic(R[s]) == ",
             round(pub_v_priv$estimate, 2)),
           parse = TRUE) +
  annotate("text", x = 18, y = 8.0, size = 5, 
           label = "italic(p) < 0.001", parse = T) +
  smplot2::sm_statCorr(
    show_text = FALSE,
    corr_method = "spearman",
    fit.params = list(
      color = "#cf1c00",
      linetype = "dashed"
    )
  )+
  xlab("Ranked Average IMD Rank") +
  ylab("Ratio of Private Garden\nto Public Greenspace") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"),
        plot.margin = unit(c(.5, 0.3, 0.5, 0.3), "cm"))
ggsave(paste0(p.plots, "/pg_pub_ratio_by_avg_IMD_spearman.png"), plot = last_plot(),
       device = "png",
       width = 25, height = 15, units = "cm")

