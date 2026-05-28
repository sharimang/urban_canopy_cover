#### Tree Cover vs IMD (City level) ####

## Evaluating the relationship between IMD at city level and canopy cover metrics ##
## IMD Rank is assigned at LSOA level - create a population-weighted average for city level following ONS methods ##
## --- Details in script section for IMD calculation ##
## --- ONS methods outlined in English Indices of Deprivation 2025 - Research Report ##

## Note that IMD Rank is *ordinal* not continuous --> the step between ranks is unknown and unequal. ##


## Written by: Shari Mang ##
## Date written: 09/03/2026
## Date last modified: 20/05/2026 ##


# ============= #
#### Set up ####
# ============= #
rm(list = ls()) # clear environment if needed

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
               ggplot2,
               ggview,
               glmmTMB,
               smplot2,
               lme4,
               splines,
               performance,
               DHARMa)
conflicted::conflict_prefer("here", "here")
conflicted::conflicts_prefer(dplyr::filter)
pacman::p_load(effects, 
               ggeffects)


## File paths ##
p.town.trees <- here("data_processed/assessment/lsoa2021/town_target/")
p.area.plantable <- here("data_processed/assessment/lsoa2021/plantable_area_calc/")
p.lsoa.out <-  here("data_processed/lsoa_2021_vars/")
p.pop <- here("data_raw/population/")
p.imd <- here("data_processed/imd/")
p.plots.paper <- here("data_processed/assessment/lsoa2021/town_target/plots_paper/")

## Parameters ##
# Values used throughout and changed depending on assumed crown size per tree.
crown_area <- 0.0032 # (in ha)  ~~ 0.0032 = median crown area of current urban trees; 0.0050 = mean crown area of current urban trees 
crown_label <- 32 # crown area size for output labels
target_cover <- 0.20 # Target town level canopy cover




### ~~~ Modelling Notes ~~~ ###
# 1. Do all data exploration - plot data, histograms etc. 
# 2. Two main questions: 
  # Q1. What is the relationship between current canopy cover and IMD (town level)?
  # Q2: Of the cities with <20% canopy cover, how does the space available for planting relate to IMD? 
    # a) Looking at the shortfall/surplus to the 20% canopy cover target 
    # b) As a binary of having sufficient or insufficent space. 

# Approaches: 
# First run as standard linear models (cover ~ IMD rank)
# Add in offset or weighting for city population size 
# Explore spearman-rank correlation with weighting
# Explore modelling options to account for IMD rank being ordinal 

## Everything is done at town level -- LSOA explored elsewhere ##




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
all_lsoa_plantable <- read.csv(paste0(p.town.trees, "/plantable_cells_all_lsoa_crown", crown_label, "_national.csv"))
# make imd quantiles/deciles factors
all_lsoa_plantable  <- all_lsoa_plantable %>%
  dplyr::mutate(imd_national_decile = as.factor(imd_national_decile),
                imd_national_quantile = as.factor(imd_national_quantile),
                imd_town_quantile = as.factor(imd_town_quantile))
# Append on reversed IMD rank
all_lsoa_plantable <- merge(all_lsoa_plantable, imd_ew[c("PolyID", "imd_rank_reverse")], by = "PolyID")

# Add on population data 
pop_ew <- read.csv(paste0(p.pop, "/population_Mid-2022 LSOA 2021.csv"))
pop_ew <- pop_ew %>% 
  janitor::clean_names(.) %>% 
  dplyr::select(c(lsoa_2021_code, total)) %>% 
  dplyr::rename(population = total,
                PolyID = lsoa_2021_code) 
# Append on Population data
all_lsoa_plantable <- merge(all_lsoa_plantable, pop_ew, by = "PolyID")
any(is.na(all_lsoa_plantable$population)) # no empty values
# Save out version - info on plantable cells, reversed imd rank, and population
write.csv(all_lsoa_plantable, paste0(p.town.trees, "/plantable_cells_all_lsoa_crown", crown_label, "_national_population.csv"), row.names = F)


# =================================================== #
# > 1.1 Calculate average IMD Rank for each city ####
# =================================================== #
all_lsoa_plantable <- read.csv(paste0(p.town.trees, "/plantable_cells_all_lsoa_crown", crown_label, "_national_population.csv"))

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
  dplyr::arrange(desc(avg_imd_rank)) %>% # Inverse them so it's back to the original way IMD is ranked with lowest value = most deprived
  as.data.frame(row.names = 1:nrow(.)) %>% 
  dplyr::mutate(ranked_avg_imd_rank = row_number()) # apply ordered ranking; 1 = most deprived; 141 = least deprived

## Wales ##
town_imd_wls <- town_imd %>% 
  dplyr::filter(region == "Wales") %>% 
  dplyr::arrange(desc(avg_imd_rank)) %>% # Inverse them so it's back to the original way IMD is ranked with lowest value = most deprived
  as.data.frame(row.names = 1:nrow(.)) %>% 
  dplyr::mutate(ranked_avg_imd_rank = row_number()) # apply ordered ranking; 1 = most deprived; 3 = least deprived

# Combine England and Wales again 
town_imd <- rbind(town_imd_eng, town_imd_wls)

## Combine with plantable cell summary for towns ##
# Data by Town 
town_summary <- read.csv(paste0(p.town.trees, "/summary_town_plantable_cells_crown", crown_label, ".csv"))
town_summary <- merge(town_summary, town_imd, by = c("TownsNM", "region"))

write.csv(town_summary, paste0(p.town.trees, "/summary_town_plantable_cells_crown", crown_label, "_avg_imd.csv"), row.names = F)



# ============================================================= #
#### 2.0 Explore relationship between Canopy Cover and IMD ####
# ============================================================= #

# > 2.1 Prep data ####
town_summary <- read.csv(paste0(p.town.trees, "/summary_town_plantable_cells_crown", crown_label, "_avg_imd.csv"))
# Separate nations
eng_town_summary <- town_summary %>% filter(region != "Wales")
wales_town_summary <- town_summary %>% filter(region == "Wales")

# Add a version of shortfall that's truncated to 0 -- anything with a surplace has a shortfall of 0
# Scale city population data
eng_town_summary <- eng_town_summary %>% 
  mutate(shortfall_prop_trunc = case_when(shortfall_prop <=0 ~ 0,
                                          shortfall_prop >0 ~ shortfall_prop),
         shortfall_npg_prop_trunc = case_when(shortfall_npg_prop <=0 ~ 0,
                                              shortfall_npg_prop >0 ~ shortfall_npg_prop),
         shortfall_pub_prop_trunc = case_when(shortfall_pub_prop <=0 ~ 0,
                                              shortfall_pub_prop >0 ~ shortfall_pub_prop)) %>% 
  dplyr::mutate(insufficient = case_when(shortfall_prop <= 0 ~ 0, 
                                         shortfall_prop > 0 ~ 1),
                insufficient_npg = case_when(shortfall_npg_prop <= 0 ~ 0, 
                                             shortfall_npg_prop > 0 ~ 1),
                insufficient_pub = case_when(shortfall_pub_prop <= 0 ~ 0, 
                                             shortfall_pub_prop > 0 ~ 1)
  ) %>% 
  dplyr::mutate(city_pop_scale = (city_population - min(city_population))/ (max(city_population) - min(city_population)))


## Assign quantiles for city average IMD ##
labs.quant <- c("01", "02", "03", "04")
intervals <- classInt::classIntervals(eng_town_summary$ranked_avg_imd_rank, n = 4, style = "quantile", intervalClosure = "left", na.rm = TRUE)
brks <- intervals$brks
cut <- cut(eng_town_summary$ranked_avg_imd_rank, breaks = brks, right = FALSE, include.lowest = TRUE, labels = labs.quant)
eng_town_summary <- eng_town_summary %>% 
  dplyr::mutate(avg_imd_rank_quant = cut)


## Make a subset of data for towns that currently have <20% cover -- places that need tree planting to increase tree cover ##
eng_town_low_cc <- eng_town_summary %>%  
  dplyr::filter(current_can_prop < 0.20)
summary(eng_town_low_cc$current_can_prop)


# > Q1 What is the relationship between existing canopy cover and IMD? ####
# Response (canopy cover) distribution 
ggplot(eng_town_summary, aes(x = current_can_prop)) + 
  geom_histogram(col = "white") +
  labs(y = "Number of observations", x = "Current canopy cover", title = "") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
# Kind of Bimodal distribution 

# Correlation
imd_v_can <- cor.test(eng_town_summary$ranked_avg_imd_rank, eng_town_summary$current_can_prop, method = "spearman") # Spearman as rank is ordinal
# rho = 0.3565
# p-value = 1.43e-05
# Current cover increases with decreasing deprivation

## Plot for paper ##
# Plot relationship b/w imd and canopy cover
ggplot(eng_town_summary, aes(x = ranked_avg_imd_rank, y = current_can_prop)) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 0.31), expand = c(0,0),
                     breaks=seq(0, 0.31, by = 0.10),
                     labels = scales::percent_format(accuracy = 1)) +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
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
        axis.title.y = element_text(size = 16, color = "black")) #+
  ggview::canvas(units = "cm",  width = 25, height = 15)


ggsave(paste0(p.plots.paper, "/current_can_by_avg_IMD_spearman.png"), plot = last_plot(),
       device = "png",
       width = 25, height = 15, units = "cm")


# > Q2 Of those who currently have <20% cover, does the shortfall from 20% cover relate to IMD? ####
# a) in terms of shortfall from 20% target 
# b) as a binary of having sufficient or insufficient space to reach 20%

## Use dataset for cities that currently have < 20% canopy cover ##
## Use variable with only shortfall but no surplus ##

# >> NPG Greenspace ####
# Response distribution
ggplot(eng_town_low_cc, aes(x = shortfall_npg_prop_trunc)) +
  geom_histogram(col = "white") +
  labs(y = "Number of observations", x = "Cover shortfall (NPG)", title = "") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
# zero inflated

# Correlation ** Not Significant **
imd_v_short_npg <- cor.test(eng_town_low_cc$ranked_avg_imd_rank, eng_town_low_cc$shortfall_npg_prop_trunc, method = "spearman")
imd_v_short_npg$estimate # -0.115
imd_v_short_npg$p.value #  0.228


## Plot for paper ##
# Plot relationship b/w IMD rank vs tree cover shortfall 
ggplot(eng_town_low_cc, aes(x = ranked_avg_imd_rank, y = shortfall_npg_prop_trunc)) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 0.15), #expand = c(0,0),
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
        plot.margin = unit(c(.5, 0.3, 0.5, 0.3), "cm")) +
ggview::canvas(units = "cm",  width = 25, height = 15)

ggsave(paste0(p.plots.paper, "/shortfall_npg_by_avg_IMD_spearman.png"), plot = last_plot(),
       device = "png",
       width = 25, height = 15, units = "cm")

# >> Public Greenspace ####
# Response distribution
ggplot(eng_town_low_cc, aes(x = shortfall_pub_prop_trunc)) +
  geom_histogram(col = "white") +
  labs(y = "Number of observations", x = "Cover shortfall (PUB)", title = "") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
# zero inflated

# Correlation ** Not Significant **
imd_v_short_pub <- cor.test(eng_town_low_cc$ranked_avg_imd_rank, eng_town_low_cc$shortfall_pub_prop_trunc, method = "spearman")
imd_v_short_pub$estimate #-0.125
imd_v_short_pub$p.value # 0.1901


## Plot for paper ##
# Plot relationship b/w IMD rank vs tree cover shortfall 
ggplot(eng_town_low_cc, aes(x = ranked_avg_imd_rank, y = shortfall_pub_prop_trunc)) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 0.15), #expand = c(0,0),
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
        plot.margin = unit(c(.5, 0.3, 0.5, 0.3), "cm")) +
  ggview::canvas(units = "cm",  width = 25, height = 15)

ggsave(paste0(p.plots.paper, "/shortfall_pub_by_avg_IMD_spearman.png"), plot = last_plot(),
         device = "png",
         width = 25, height = 15, units = "cm")


# Plot NPG and PUB together #
eng_town_low_cc %>% 
  select(c(TownsNM, region, ranked_avg_imd_rank, shortfall_npg_prop_trunc, shortfall_pub_prop_trunc)) %>% 
  pivot_longer(cols = c(shortfall_npg_prop_trunc, shortfall_pub_prop_trunc),
               names_to = "scenario", values_to = "shortfall") %>% 
  ggplot(mapping = aes(x = ranked_avg_imd_rank, y = shortfall)) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 0.15), expand = c(0,0),
                     breaks=seq(0, 0.15, by = 0.05),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(limits = c(0,142), expand = c(0,0),
                     breaks=seq(0, 140, by = 20)) +
  xlab("Ranked Average IMD Rank") +
  ylab("Shortfall from 20% target (%)") +
  smplot2::sm_statCorr(
    show_text = FALSE,
    corr_method = "spearman",
    fit.params = list(
      color = "#cf1c00",
      linetype = "dashed"
    )
  ) +
facet_wrap(~scenario, ncol = 1) +
  theme_classic()





## >> Binary sufficient or insufficient ####
# Q: Are more deprived cities more likely to have insufficient space?

ggplot(eng_town_low_cc, aes(x = ranked_avg_imd_rank, y = insufficient_npg)) + # plotted when insufficient is still 0/1 rather than text
  geom_hex() + 
  scale_fill_continuous(type = "viridis") + 
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))


# > Q3 Does the ratio of Private to Public greenspace change with IMD? ####
# Read in data and calculate ratio
area_by_town <- read.csv(paste0(p.area.plantable, "/summary_plantable_area_town.csv"))
town_summary <- read.csv(paste0(p.town.trees, "/summary_town_plantable_cells_crown", crown_label, "_avg_imd.csv"))
eng_avg_imd <- town_summary %>% 
  filter(region != "Wales") %>% 
  dplyr::select(c(TownsNM, ranked_avg_imd_rank)) # want ranked_avg_imd_rank
## Make sure eng_town_summary is read in and made above ##


# ratio private to public 
abt <- merge(area_by_town, eng_avg_imd, by = "TownsNM")
# Append on population data 
abt <- merge(abt, eng_town_summary[, c("TownsNM", "city_population", "city_pop_scale", "avg_imd_rank_quant")], by = "TownsNM")
abt <- abt %>% 
  dplyr::mutate(priv_pub_ratio = pr_gard_ha/gs_pub_ha,
                all_priv_pub_ratio = (pr_gard_ha +(gs_npg_ha - gs_pub_ha))/gs_pub_ha) # private being all non public land; private gardens and institutional grounds
# private garden only vs all private --> same pattern and near equal strength of relationship. Which to use depends on narrative.

tmp <- abt[c(1:7,21, 24,25)]

## Response distribution ##
ggplot(abt, aes(x = priv_pub_ratio)) + 
  geom_histogram(col = "white") +
  labs(y = "Number of observations", x = "Ratio private:public", title = "") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))


## Correlation between ratio and IMD ##
pub_v_priv <- cor.test(abt$ranked_avg_imd_rank, abt$priv_pub_ratio, method = "spearman")
pub_v_priv$estimate # rho = 0.32989
pub_v_priv$p.value # p-value = 7.102e-05
## All private greenspace version
cor.test(abt$ranked_avg_imd_rank, abt$all_priv_pub_ratio, method = "spearman")
# rho = 0.3372
# p-value = 4.796e-05

## Plot for paper ##
ggplot(abt, aes(x = ranked_avg_imd_rank, y = priv_pub_ratio)) +
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
        plot.margin = unit(c(.5, 0.3, 0.5, 0.3), "cm"))+
ggview::canvas(units = "cm",  width = 25, height = 15)

ggsave(paste0(p.plots.paper, "/pg_pub_ratio_by_avg_IMD_spearman.png"), plot = last_plot(),
       device = "png",
       width = 25, height = 15, units = "cm")



# ============================= #
#### 3.0 Model Exploration ####
# ============================= #
## IMD Rank is not continuous; models are treating it as such so interpret with caution ##

## > Q1 What is the relationship between existing canopy cover and IMD? ####
## Model weighted by city population ##
m_cc <- glmmTMB::glmmTMB(current_can_prop ~ ranked_avg_imd_rank, 
           weights = city_pop_scale,
           data = eng_town_summary,
           family = gaussian)
summary(m_cc) # Significant p = 0.04
performance::check_model(m_cc)
m_cc_resid <- simulateResiduals(m_cc)
plot(m_cc_resid)
plotResiduals(m_cc_resid)

# Plot effects
m_effects <- effects::effect(mod = m_cc, term = "ranked_avg_imd_rank", xlevels = 141, se=list(level = 0.95))
m_effects <- as.data.frame(m_effects)

ggplot() +
  geom_line(data = m_effects, aes(x = ranked_avg_imd_rank, y = fit), 
            colour = "red", linewidth = 1.5) +
  geom_ribbon(data = m_effects, aes(x = ranked_avg_imd_rank, ymin = lower, ymax = upper), 
              alpha = 0.4, colour = NA, fill = "grey") + 
  geom_point(data = eng_town_summary, aes(x = ranked_avg_imd_rank, y = current_can_prop), 
             size = 2.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(expand = c(0,0), limits = c(0,142), 
                     breaks=seq(0, 140, by = 20)) +
  ylab("Current canopy cover") + xlab("City average IMD rank") +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
         axis.title = element_text(size = 16)) #+
  #ggview::canvas(units = "cm",  width = 25, height = 15)

ggsave(paste0(p.plots.paper, "/current_can_by_avg_IMD_crown", crown_label, ".png"), plot = last_plot(),
       device = "png",
       width = 25, height = 15, units = "cm")


## Without weighting by population ##
m_cc <- glmmTMB::glmmTMB(current_can_prop ~ ranked_avg_imd_rank, 
                         data = eng_town_summary)
plot(simulateResiduals(m_cc))
summary(m_cc) # Significant p < 0.001
# Alt with population as explanatory variable
m_cc <- glmmTMB::glmmTMB(current_can_prop ~ ranked_avg_imd_rank + log(city_population), 
                         data = eng_town_summary)
plot(simulateResiduals(m_cc))
summary(m_cc)


# > Q2 Of those who currently have <20% cover, does the shortfall from 20% cover relate to IMD? ####
# a) in terms of shortfall from 20% target 
# b) as a binary of having sufficient or insufficient space to reach 20%

## Use dataset for cities that currently have < 20% canopy cover ##
## Use variable with only shortfall but no surplus ##

# >> NPG Greenspace ####
# Is now zero-inflated
m_truncate <- glmmTMB::glmmTMB(shortfall_npg_prop_trunc ~ ranked_avg_imd_rank, 
                               data = eng_town_low_cc,
                               family = tweedie(link = "log"))
m_trunc_resid <- simulateResiduals(m_truncate)
plot(m_trunc_resid) # Deviance in QQ and residuals
check_residuals(m_truncate)
summary(m_truncate) # No significant relationship

## Model weighted by population ##
m_truncate_w <- glmmTMB::glmmTMB(shortfall_npg_prop_trunc ~ ranked_avg_imd_rank, 
                                 data = eng_town_low_cc,
                                 weight = city_pop_scale,
                                 family = tweedie(link = "log"))
m_trunc_resid <- simulateResiduals(m_truncate_w)
plot(m_trunc_resid)
check_residuals(m_truncate_w) ## residuals issue now
summary(m_truncate_w) # Not significant
check_overdispersion(m_truncate_w)
check_model(m_truncate_w)
# Plot effects
m_effects <- effects::effect(mod = m_truncate_w, term = "ranked_avg_imd_rank", xlevels = 144, se=list(level = 0.95))
m_effects <- as.data.frame(m_effects)

ggplot() +
  geom_point(data = eng_town_low_cc, aes(x = ranked_avg_imd_rank, y = shortfall_npg_prop_trunc)) +
  geom_line(data = m_effects, aes(x = ranked_avg_imd_rank, y = fit), colour = "red") +
  geom_ribbon(data = m_effects, aes(x = ranked_avg_imd_rank, ymin = lower, ymax = upper), alpha = 0.4, colour = NA, fill = "grey") + 
  theme_classic()

# Plot with ggpredict 
plot(ggpredict(m_truncate_w, term = "ranked_avg_imd_rank"))


## Playing with different modelling approaches ## 
## Trying to spline ##
m_spline <- glmmTMB::glmmTMB(shortfall_npg_prop_trunc ~ ns(ranked_avg_imd_rank, df = 3), 
                                 data = eng_town_low_cc,
                                 weight = city_pop_scale,
                                 family = tweedie(link = "log"))
performance::check_model(m_spline)
summary(m_spline) 

## Different distribution family 
m_zgam <- glmmTMB::glmmTMB(shortfall_npg_prop_trunc ~ ranked_avg_imd_rank,
                                 data = eng_town_low_cc,
                                 weight = city_population,
                                 family = ziGamma(link = "log"), ziformula = ~1)
m_zgam_resid <- simulateResiduals(m_zgam)
plot(m_zgam_resid)
check_residuals(m_zgam) ## residuals issue now
summary(m_zgam)

## With IMD as quantiles --> Not significant (with or without population)
m_trunc_w_imdq <- glmmTMB::glmmTMB(shortfall_npg_prop_trunc ~ avg_imd_rank_quant, 
                                 data = eng_town_low_cc,
                                 weight = city_pop_scale,
                                 family = tweedie(link = "log"))
check_model(m_trunc_w_imdq)
summary(m_trunc_w_imdq)



## >> Binary sufficient or insufficient ####
# Q: Are more deprived cities more likely to have insufficient space?
m_insuff <- glmmTMB::glmmTMB(insufficient_npg ~ ranked_avg_imd_rank,
                             data = eng_town_low_cc,  
                             family = binomial)
m_insuff_resid <- simulateResiduals(m_insuff)
plot(m_insuff_resid) # 
check_model(m_insuff)
summary(m_insuff) # Not significant 

# Binary effects plot
m_effects <- effects::effect(mod = m_insuff, term = "ranked_avg_imd_rank", xlevels = 141, se=list(level = 0.95))
m_effects <- as.data.frame(m_effects)

ggplot() +
  geom_hex(data = eng_town_low_cc, aes(x = ranked_avg_imd_rank, y = insufficient_npg)) +
  geom_line(data = m_effects, aes(x = ranked_avg_imd_rank, y = fit), colour = "red") +
  geom_ribbon(data = m_effects, aes(x = ranked_avg_imd_rank, ymin = lower, ymax = upper), alpha = 0.4, colour = NA, fill = "grey") + 
  theme_classic()


## Playing with different modelling approaches ## 
# Quadratic
m_insuff_2 <- glmmTMB::glmmTMB(insufficient_npg ~ poly(ranked_avg_imd_rank, 2),
                             data = eng_town_low_cc,  
                             family = binomial)
plot(simulateResiduals(m_insuff_2))
summary(m_insuff_2) # not significant 

# Spline
m_insuff_3 <- glmmTMB::glmmTMB(insufficient_npg ~ ns(ranked_avg_imd_rank, df = 3),
                               data = eng_town_low_cc,  
                               family = binomial)
plot(simulateResiduals(m_insuff_3))
summary(m_insuff_3) # not significant 



# > Q3 Does the ratio of Private to Public greenspace change with IMD? ####
# Basic model 
m_ratio_1 <- glmmTMB(priv_pub_ratio ~ ranked_avg_imd_rank, 
                     data = abt,
                     family = gaussian())
plot(simulateResiduals(m_ratio_1)) # huge mess
summary(m_ratio_1) # nothing significant

# Try Gamma to account for skew
m_ratio_2 <- glmmTMB(priv_pub_ratio ~ ranked_avg_imd_rank, 
                     data = abt,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_2)) # less of a mess
summary(m_ratio_2) # significant
plot(fitted(m_ratio_2), resid(m_ratio_2))
testOutliers(simulateResiduals(m_ratio_2)) ## there are two outliers 

## Try with outlier removed (Hammersmith) ##
m_ratio_3 <- glmmTMB(priv_pub_ratio ~ ranked_avg_imd_rank, 
                     data = abt2,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_3)) # Good
summary(m_ratio_3) # significant

# linear with city population weighted
m_ratio_3a <- glmmTMB(priv_pub_ratio ~ ranked_avg_imd_rank, 
                      data = abt2,
                      weight = city_pop_scale,
                      family = Gamma("log"))
plot(simulateResiduals(m_ratio_3a)) # Bad
summary(m_ratio_3a) # Not significant


## Play with other modelling frameworks ##
# quadratic
m_ratio_3 <- glmmTMB(priv_pub_ratio ~ poly(ranked_avg_imd_rank,2), 
                     data = abt2,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_3)) # Good
summary(m_ratio_3) # linear term significant, quadratic not significant

plot(ggpredict(m_ratio_3, term = "ranked_avg_imd_rank[all]")) # basically just a linear plot with varying error

# quadratic with population weighted
m_ratio_3a <- glmmTMB(priv_pub_ratio ~ poly(ranked_avg_imd_rank,2), 
                      data = abt2,
                      weight = city_pop_scale,
                      family = Gamma("log"))
plot(simulateResiduals(m_ratio_3a)) # Bad
summary(m_ratio_3a) # not significant

# Splines
m_ratio_4 <- glmmTMB(priv_pub_ratio ~ ns(ranked_avg_imd_rank, df = 3), 
                     data = abt2,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_4)) # Good
summary(m_ratio_4) # Significant for spline 2 and 3

# Plot spline 
## With ggpredict 
plot(ggpredict(m_ratio_4, term = "ranked_avg_imd_rank[all]"))

# Examine residuals
residuals <- residuals(m_ratio_4)
plot(abt2$ranked_avg_imd_rank, residuals, main = "Residuals of Spline Regression", xlab = "X", 
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Weighting by population
m_ratio_5 <- glmmTMB(priv_pub_ratio ~ ns(ranked_avg_imd_rank, df = 3), 
                     data = abt2,
                     weight = city_pop_scale,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_5)) # Back to a mess
summary(m_ratio_5) 





### Old Script -- keep for reference for now ###

## Relationship between IMD and private garden availability and no private garden availability. ##
# Could do as amount of space available in private gardens 

p.area.plantable <- here("data_processed/assessment/lsoa2021/plantable_area_calc/")
area_by_town <- read.csv(paste0(p.area.plantable, "/summary_plantable_area_town.csv"))

town_summary <- read.csv(paste0(p.town.trees, "/summary_town_plantable_cells_crown", crown_label, "_avg_imd.csv"))
eng_avg_imd <- town_summary %>% 
  filter(region != "Wales") %>% 
  dplyr::select(c(TownsNM, ranked_avg_imd_rank))
# want ranked_avg_imd_rank

# ratio private to public 
abt <- merge(area_by_town, eng_avg_imd, by = "TownsNM")
abt <- abt %>% 
  dplyr::mutate(priv_pub_ratio = pr_gard_ha/gs_pub_ha) 
abt2 <- abt %>% 
  dplyr::filter(priv_pub_ratio <= 40)

hist(abt$priv_pub_ratio)
ggplot(abt, aes(x = ranked_avg_imd_rank, y = priv_pub_ratio)) +
  geom_point() + 
  #scale_y_continuous(expand = c(0,0)) +
 # scale_x_continuous(expand = c(0,0)) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  theme_classic()+ 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
cor.test(abt$ranked_avg_imd_rank, abt$priv_pub_ratio, method = "spearman")
# rho = 0.2835
# p-value = 0.0006881

ggplot(abt2, aes(x = ranked_avg_imd_rank, y = priv_pub_ratio)) + # y = pr_gard_prop
  geom_point() + 
  #scale_y_continuous(expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0)) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  theme_classic()+ 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
cor.test(abt2$ranked_avg_imd_rank, abt2$priv_pub_ratio, method = "spearman")
# is significant 
# rho = 0.283
# p-value = 0.0007395


# Distribution of ratio
ggplot(abt, aes(x = priv_pub_ratio)) + 
  geom_histogram(col = "white") +
  labs(y = "Number of observations", x = "Ratio private:public", title = "") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
# Slight right skew (when ratio = 50 removed)


ggplot(abt2, aes(x = ranked_avg_imd_rank, y = priv_pub_ratio)) +
  geom_hex(bins = 20) + 
  scale_fill_continuous(type = "viridis") +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth", 
              colour = "red") +
  theme_classic() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))


# Append on population data 
abt <- merge(abt, eng_town_summary[, c("TownsNM", "city_population", "city_pop_scale", "avg_imd_rank_quant")], by = "TownsNM")


# Basic model 
m_ratio_1 <- glmmTMB(priv_pub_ratio ~ ranked_avg_imd_rank, 
                     data = abt,
                     family = gaussian())
plot(simulateResiduals(m_ratio_1)) # huge mess
summary(m_ratio_1) # nothing significant

# Try Gamma to account for skew
m_ratio_2 <- glmmTMB(priv_pub_ratio ~ ranked_avg_imd_rank, 
                     data = abt,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_2)) # less of a mess
summary(m_ratio_2) # significant

plot(fitted(m_ratio_2), resid(m_ratio_2))
testOutliers(simulateResiduals(m_ratio_2)) ## there are two outliers 

## Remove outliers -- just remove single very high value
abt2 <- abt %>% 
  dplyr::filter(priv_pub_ratio <= 50) 

# Same with outliers removed
m_ratio_3 <- glmmTMB(priv_pub_ratio ~ ranked_avg_imd_rank, 
                     data = abt2,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_3)) # Good
summary(m_ratio_3) # significant

plot(fitted(m_ratio_3), resid(m_ratio_3))
testOutliers(simulateResiduals(m_ratio_3)) 

# linear with city population weighted
m_ratio_3a <- glmmTMB(priv_pub_ratio ~ ranked_avg_imd_rank, 
                     data = abt2,
                     weight = city_pop_scale,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_3a)) # Bad
summary(m_ratio_3a) # Not significant

# linear with city population as predictor 
m_ratio_3b <- glmmTMB(priv_pub_ratio ~ ranked_avg_imd_rank + city_pop_scale, 
                     data = abt2,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_3b)) # Good
summary(m_ratio_3b) # significant - both terms; AIC lower than without city population


# quadratic
m_ratio_3 <- glmmTMB(priv_pub_ratio ~ poly(ranked_avg_imd_rank,2), 
                     data = abt2,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_3)) # Good
summary(m_ratio_3) # linear term significant, quadratic not significant

plot(ggpredict(m_ratio_3, term = "ranked_avg_imd_rank[all]")) # basically just a linear plot with varying error

# quadratic with population weighted
m_ratio_3a <- glmmTMB(priv_pub_ratio ~ poly(ranked_avg_imd_rank,2), 
                     data = abt2,
                     weight = city_pop_scale,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_3a)) # Bad
summary(m_ratio_3a) # not significant

# Splines
m_ratio_4 <- glmmTMB(priv_pub_ratio ~ ns(ranked_avg_imd_rank, df = 3), 
                     data = abt2,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_4)) # Good
summary(m_ratio_4) # Significant for spline 2 and 3


# Plot spline 
## With ggpredict 
plot(ggpredict(m_ratio_4, term = "ranked_avg_imd_rank[all]"))


# Examine residuals
residuals <- residuals(m_ratio_4)
plot(abt2$ranked_avg_imd_rank, residuals, main = "Residuals of Spline Regression", xlab = "X", 
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)


## GAMMS 
library(mgcv)
# Build the model
m_ratio_gam <- gam(priv_pub_ratio ~ s(ranked_avg_imd_rank, bs = "cr"), 
                   data = abt2,
                   family = Gamma("log"))
summary(m_ratio_gam)

ggplot(abt2, aes(x = ranked_avg_imd_rank, y = priv_pub_ratio) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))
plot(ggpredict(m_ratio_gam, term = "ranked_avg_imd_rank[all]"))

AIC(m_ratio_gam); summary(m_ratio_gam)$r.sq 
AIC(m_ratio_4); summary(m_ratio_4)$r.sq 

# Weighting by population
m_ratio_5 <- glmmTMB(priv_pub_ratio ~ ns(ranked_avg_imd_rank, df = 3), 
                     data = abt2,
                     weight = city_pop_scale,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_5)) # Back to a mess
summary(m_ratio_5) 

# Population as predictor
m_ratio_6 <- glmmTMB(priv_pub_ratio ~ ns(ranked_avg_imd_rank, df = 3) + city_pop_scale, 
                     data = abt2,
                     family = Gamma("log"))
plot(simulateResiduals(m_ratio_6)) # Back to a mess
summary(m_ratio_6) 


# Log response and use splines -- better to use gamma than log() with gaussian
model <- glmmTMB(
  log(priv_pub_ratio) ~ ns(ranked_avg_imd_rank, df = 3) + log(city_population),
  family = gaussian(),
  data = abt
)
summary(model) # nothing significant
plot(simulateResiduals(model)) # good fit


m_ratio <- glmmTMB::glmmTMB(log(priv_pub_ratio) ~ poly(ranked_avg_imd_rank,2), 
                        # weights = city_pop_scale,
                       # offset = city_pop_scale,
                         data = abt,
                         family = gaussian()
                         )
summary(m_ratio) # nothing significant
performance::check_model(m_ratio)
plot(simulateResiduals(m_ratio)) # Good fit


# With splines
library(splines)
log(priv_pub_ratio) ~ ns(avg_imd_rank, df = 3)

m_spline <- glmmTMB::glmmTMB(log(priv_pub_ratio) ~ ns(ranked_avg_imd_rank, df = 3), 
                           weights = city_pop_scale,
                           #offset = city_pop_scale,
                            data = abt,
                            family = gaussian())
summary(m_spline)
plot(simulateResiduals(m_spline)) # no improvement

ggeffects::predict_response(m_spline, terms="ranked_avg_imd_rank [all]") %>% 
  plot(show_data = TRUE)


# Gamma model 
model_gamma <- glmmTMB(
  priv_pub_ratio ~ ns(ranked_avg_imd_rank, df = 3), 
  #priv_pub_ratio ~ poly(ranked_avg_imd_rank, 2), 
  #weights = city_pop_scale,
  #offset = city_pop_scale,
  family = Gamma(link = "log"),
  data = abt
)
summary(model_gamma)
plot(simulateResiduals(model_gamma))


## With quantiles ##
m_ratio_q <- glmmTMB(priv_pub_ratio ~ avg_imd_rank_quant, 
                     data = abt2, 
                     family = Gamma("log"))
summary(m_ratio_q)
plot(simulateResiduals(m_ratio_q))


# Weights = “how certain is this observation?”
# Offset = “how much exposure produced this outcome?”
# Predictor = “this variable influences the outcome”

predict_response(spline_model, c("visits", "discount")) |> plot(show_data = TRUE)
