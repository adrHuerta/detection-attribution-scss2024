#!/bin/Rscript
###### HEADER - DandA_Exercise.R ############################################
# Author:  Thomas Frölicher (thomas.froelicher@unibe.ch)
#          Erich M. Fischer (erich.fischer@env.ethz.ch)
#          Joel Zeder (joel.zeder@env.ethz.ch)
#          Natacha Le Grix (natacha.legrix@unibe.ch)
#.         Jitendra Singh (jitendra.singh@env.ethz.ch)
# Date:    01.09.24 and 06.09.2024
# Purpose: Detection and Attribution Exercise (Swiss Climate Summer School 2024)
##############################################################################
rm(list=ls())
#### LOAD LIBRARIES ##########################################################
library(dplyr)
library(ggplot2)
library(readr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(evd)
# -> load your libraries here

#### SETTINGS ################################################################
path_data_in <- "~/Data_Input/"
#### MAIN BODY ###############################################################
## Select input data (by un-commenting the respective line):

##Western Europe Drought - regional and local (Lyon gridpoint):
DA_data_path <- file.path(path_data_in, "drought_we", "WesternEurope_Drought.csv")
#DA_data_path <- file.path(path_data_in, "drought_we", "Lyon_Drought.csv")

## Read input data:
DA_data_df   <- read_csv(DA_data_path, col_types = cols(year = col_integer())) %>% 
  mutate(model=factor(model), ens=factor(ens)) %>% as.data.frame()

name_index  <- names(DA_data_df)[4]
DA_obs2022_df <- DA_data_df %>% filter(year==2022, model %in% "ERA5")
# Print summary measures (UNCOMMENT TO PRINT):
#str(DA_data_df)  
#summary(DA_data_df) 

## filter data based on model
data_by_model<- DA_data_df %>% 
  filter(model %in% c("CanESM5","UKESM1")) %>% group_by(model) %>% group_split()

## ----------------- DON'T UNCOMMENT THIS SECTION ----------------------------------------------------------
## How does the data look like?
## One data.frame with columns
## "model"     - models depending on location/variable
## "ens"       - ensemble member, always 1 for ERA5 or satellite observations
## "year"      - model or observation year
## <index>     - the extreme value index (e.g. P-E, ...)
## <index_ano> - anomaly against 1981 - 2010 baseline (e.g. JJA average)
## "gmst_ano"  - smoothed GMST anomaly against 1981 - 2010

########################################################################################
# Detection & attribution analysis starts here
########################################################################################
# Function to estimate exceedance probabilities
exceedance_prob<-function(data_to_fit_model, obs2022, period) {
  # Filter to the 1850-1949 period and fit GEV, then calculate exceedance probabilities
  data_to_fit_model %>%
    filter(between(year, period[1], period[2])) %>% 
    mutate(
      fitGEV = list(fgev(p_e)), # Fit GEV for the filtered period
      ecdffit = list(ecdf(p_e)), # Fit EMP for the filtered period
      exceedProb =  pgev(sort(p_e),  fitGEV[[1]]$estimate["loc"],fitGEV[[1]]$estimate["scale"], fitGEV[[1]]$estimate["shape"]),
      exceedProb_obs2022 =  pgev(obs2022,  fitGEV[[1]]$estimate["loc"],fitGEV[[1]]$estimate["scale"], fitGEV[[1]]$estimate["shape"]),
      # Calculate exceedance probability using empirical distribution
      exceedProb_EmpDist = ecdffit[[1]](sort(p_e)),
      
      # Calculate exceedance probability for the observed value using empirical distribution
      exceedProb_obs2022_EmpDist =  ecdffit[[1]](obs2022),
      p_e_arrenged = sort(p_e)) #%>%  
  #select(-fitGEV_cFact, -ecdf_func_cFact) # Remove the GEV fit object if not needed
}

cFactPeriod<-c(1850,1949) # define counter factual world; CHANGE HERE IF REQUIRED
FactPeriod<-c(1990,2022) # define factual (or current) world; CHANGE HERE IF REQUIRED
FutPeriod<-c(2025,2055) # define future world; CHANGE HERE IF REQUIRED

# Estimate exceedance probabilities for each model and period
cFact_stats <- data_by_model %>% lapply(exceedance_prob, obs2022 = DA_obs2022_df$p_e, period = cFactPeriod) %>% bind_rows() 
cFact_stats<- cFact_stats[!names(cFact_stats) %in% c('fitGEV', 'ecdffit')];  str(cFact_stats)

Fact_stats <- data_by_model %>% lapply(exceedance_prob, obs2022 = DA_obs2022_df$p_e, period = FactPeriod) %>% bind_rows() 
Fact_stats<- Fact_stats[!names(Fact_stats) %in% c('fitGEV', 'ecdffit')];  str(Fact_stats)

Fut_stats <- data_by_model %>% lapply(exceedance_prob, obs2022 = DA_obs2022_df$p_e, period = FutPeriod) %>% bind_rows() 
Fut_stats<- Fut_stats[!names(Fut_stats) %in% c('fitGEV', 'ecdffit')];  str(Fut_stats)
##################################
## Plot start here
####################################
# Define the name_index variable
name_index <- "p_e"
plot_title <- tools::file_path_sans_ext(basename(DA_data_path))

# Define a manual color palette
cFact_label <- sprintf("cFact nEP\n(%d-%d)", cFactPeriod[1], cFactPeriod[2])
Fact_label <- sprintf("Fact nEP\n(%d-%d)", FactPeriod[1], FactPeriod[2])
Fut_label <- sprintf("Fut nEP\n(%d-%d)", FutPeriod[1], FutPeriod[2])

manual_colors <- c( "Observed 2022" = "purple", "cFact Exceed Prob" = "royalblue1",
                    "Fact Exceed Prob" = "#fc8d59","Fut Exceed Prob" = "#b30000")

#Extract exceedance probabilities for annotations
exceedance_probs <- cFact_stats %>%
  filter(model %in% c("CanESM5", "UKESM1")) %>% group_by(model) %>%
  summarise(cFact_prob = mean(exceedProb_obs2022, na.rm = TRUE),
            .groups = 'drop') %>% left_join( Fact_stats %>% filter(model %in% c("CanESM5", "UKESM1")) %>%
                                               group_by(model) %>% summarise(Fact_prob = mean(exceedProb_obs2022, na.rm = TRUE), .groups = 'drop'),by = "model") %>%
  left_join(Fut_stats %>% filter(model %in% c("CanESM5", "UKESM1")) %>% group_by(model) %>% 
              summarise(Fut_prob = mean(exceedProb_obs2022, na.rm = TRUE), .groups = 'drop'),by = "model")
# # Prepare text for annotation
exceedance_probs <- exceedance_probs %>% mutate(annotation_text = paste0("P_cFact = ", round(cFact_prob, 3), "\n",
                             "P_Fact = ", round(Fact_prob, 3), "\n","P_Fut = ", round(Fut_prob, 3)))

exceedance_probs_EmpDist <- cFact_stats %>% filter(model %in% c("CanESM5", "UKESM1")) %>%
  group_by(model) %>% summarise( cFact_prob_EmpDist = mean(exceedProb_obs2022_EmpDist, na.rm = TRUE),
                                 .groups = 'drop') %>% left_join( Fact_stats %>% filter(model %in% c("CanESM5", "UKESM1")) %>%
                        group_by(model) %>% summarise(Fact_prob_EmpDist = mean(exceedProb_obs2022_EmpDist, na.rm = TRUE), .groups = 'drop'),
                                          by = "model") %>% left_join(Fut_stats %>% filter(model %in% c("CanESM5", "UKESM1")) %>%
                                 group_by(model) %>% summarise(Fut_prob_EmpDist = mean(exceedProb_obs2022_EmpDist, na.rm = TRUE), .groups = 'drop'),
                                                       by = "model")
# # Prepare text for annotation
exceedance_probs_EmpDist <- exceedance_probs_EmpDist %>%mutate(annotation_text_EmpDist = paste0("P_cFact = ", round(cFact_prob_EmpDist, 3), "\n",
                                        "P_Fact = ", round(Fact_prob_EmpDist, 3), "\n","P_Fut = ", round(Fut_prob_EmpDist, 3)))

# Scatter plots for all three models in the first row 
p1 <- DA_data_df %>%
  ggplot(aes_string(x = "year", y = name_index)) +
  facet_grid(~model) +
  geom_point(size = 0.3, alpha = 0.5, color = "black") +  
  geom_point(data = DA_obs2022_df, color = "purple", size = 2) +  # Larger point for observed 2022
  geom_hline(aes_string(yintercept = mean(DA_obs2022_df[[name_index]]), color = "'Observed 2022'"), linetype = "dashed") +  # Mean line
  labs(title = paste("Scatter Plots for", plot_title), color = "Legend") +  # Legend title
  scale_color_manual(name="Legend",values = manual_colors) +  # Apply manual colors
  theme_minimal()

# Line plots (based on GEV) for CanESM5 and CESM2 in the second row
p2 <- cFact_stats %>% filter(model %in% c("CanESM5", "UKESM1")) %>%
  ggplot(aes(x = p_e_arrenged)) +
  facet_wrap(~model, scales = "free_y") +
  geom_line(aes(y = exceedProb, color = "cFact Exceed Prob"), size = 1) +  # Line for counterfactual
  geom_line(data = Fact_stats, aes( y = exceedProb, color = "Fact Exceed Prob"), size = 1) +  # Line for factual
  geom_line(data = Fut_stats, aes( y = exceedProb, color = "Fut Exceed Prob"), size = 1) +  # Line for future
  geom_vline(xintercept = DA_obs2022_df$p_e, color = "purple", linetype = "dashed") +  # Vertical line for observed 2022
  labs(title = "Non-exceedance probability distribution (based on GEV)", x = "p_e", y = "Non-exceedance probability (nEP)", color = "Legend") +  # Legend title
  scale_color_manual(name="Legend",values = manual_colors, labels = c(cFact_label, Fact_label, Fut_label)) +  # Apply manual colors
  theme_minimal() +theme(legend.text = element_text(size = 12))+
  # Add EP values
  geom_text(data = exceedance_probs, aes(x = max(cFact_stats$p_e, na.rm = TRUE) * 0.3, 
                                         y = 0.9, label = annotation_text), hjust = 0, vjust = 1.5, size = 4, color = c("black"), 
            position = position_nudge(y = -0.1))  # Adjust the vertical position of text

# Line plots (based on Empirical distribution) for CanESM5 and CESM2 in the second row
p3 <- cFact_stats %>% filter(model %in% c("CanESM5", "UKESM1")) %>%
  ggplot(aes(x = p_e_arrenged)) +
  facet_wrap(~model, scales = "free_y") +
  geom_line(aes(y = exceedProb_EmpDist, color = "cFact Exceed Prob"), size = 1) +  # Line for counterfactual
  geom_line(data = Fact_stats, aes(y = exceedProb_EmpDist, color = "Fact Exceed Prob"), size = 1) +  # Line for factual
  geom_line(data = Fut_stats, aes(y = exceedProb_EmpDist, color = "Fut Exceed Prob"), size = 1) +  # Line for future
  geom_vline(xintercept = DA_obs2022_df$p_e, color = "purple", linetype = "dashed") +  # Vertical line for observed 2022
  labs(title = "Non-exceedance probability distribution (based on Empirical distribution)", x = "p_e", y = "Non-exceedance probability (nEP)", color = "Legend") +  # Legend title
  scale_color_manual(name="Legend",values = manual_colors, labels = c(cFact_label, Fact_label, Fut_label)) +  # Apply manual colors
  theme_minimal() +
  theme(legend.text = element_text(size = 12))+
  # Add EP values
  geom_text(data = exceedance_probs_EmpDist, aes(x = max(Fut_stats$p_e, na.rm = TRUE) * 0.3, 
                                                 y = 0.9, label = annotation_text_EmpDist), 
            hjust = 0, vjust = 1.5, size = 4, 
            color = c("black"), 
            position = position_nudge(y = -0.1))  # Adjust the vertical position of text
# Arrange the plots in a grid
combined_plot<-grid.arrange(p1, p2, p3, nrow = 3)

######################################################################
# Save plot and output files (UNCOMMENT AND PROVIDE YOUR PATH)
######################################################################
# ggsave("/YOUR PATH/GOES HERE/combined_plot.png", plot = combined_plot, width = 10, height = 15, dpi = 300)
# write.csv(cFact_stats, file='/YOUR PATH/GOES HERE/cFact_stats.csv')
# write.csv(Fact_stats, file='/YOUR PATH/GOES HERE/Fact_stats.csv')
# write.csv(Fut_stats, file='/YOUR PATH/GOES HERE/Fut_stats.csv')
