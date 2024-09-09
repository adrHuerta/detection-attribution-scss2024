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

## ---------------------------------------------------------------------------
## Select input data (by un-commenting the respective line):

## Western Europe Heatwave  - regional and local (London gridpoint):
# DA_data_path <- file.path(path_data_in, "heatwave_we", "WesternEurope_Heatwave.csv")
DA_data_path <- file.path(path_data_in, "heatwave_we", "London_Heatwave.csv")

## Read input data:
DA_data_df   <- read_csv(DA_data_path, col_types = cols(year = col_integer())) %>% 
  mutate(model=factor(model), ens=factor(ens)) %>% as.data.frame()

name_index  <- names(DA_data_df)[5]
DA_obs2022_df <- DA_data_df %>% filter(year==2022, model %in% "ERA5")

# Print summary measures: (UNCOMMENT TO PRINT):
# str(DA_data_df)
# summary(DA_data_df)

## ----------------- DON'T UNCOMMENT THIS SECTION ----------------------------------------------------------

## How does the data look like?
## One data.frame with columns
## "model"     - models depending on location/variable
## "ens"       - ensemble member, always 1 for ERA5 or satellite observations
## "year"      - model or observation year
## <index>     - the extreme value index (e.g. tx5d, ...)
## <index_ano> - anomaly against 1981 - 2010 baseline (e.g. JJA average)
## "gmst_ano"  - smoothed GMST anomaly against 1981 - 2010
##################################
## Plotting start here
####################################
plot_title <- tools::file_path_sans_ext(basename(DA_data_path))
manual_colors <- c( "Observed 2022" = "purple")
# Scatter plots for all three models in the first row
DA_data_df %>%
  ggplot(aes_string(x = "year", y = name_index)) +
  facet_grid(~model) +
  geom_point(size = 0.3, alpha = 0.5, color = "black") +  
  geom_point(data = DA_obs2022_df, color = "purple", size = 2) +  # Larger point for observed 2022
  geom_hline(aes_string(yintercept = mean(DA_obs2022_df[[name_index]]), color = "'Observed 2022'"), linetype = "dashed") +  # Mean line
  labs(title = paste("Scatter Plots for", plot_title), color = "Legend") +  # Legend title
  scale_color_manual(name="Legend",values = manual_colors) +  # Apply manual colors
  theme_minimal()


head(DA_data_df)

DA_data_df[DA_data_df$year > 1950 & DA_data_df$year < 2022, ] %>%
  ggplot(aes_string(x = "year", y = name_index)) +
  facet_grid(~model) +
  geom_point(size = 0.3, alpha = 0.5, color = "black") +  
  geom_point(data = DA_obs2022_df, color = "purple", size = 2) +  # Larger point for observed 2022
  geom_hline(aes_string(yintercept = mean(DA_obs2022_df[[name_index]]), color = "'Observed 2022'"), linetype = "dashed") +  # Mean line
  labs(title = paste("Scatter Plots for", plot_title), color = "Legend") +  # Legend title
  scale_color_manual(name="Legend",values = manual_colors) +  # Apply manual colors
  theme_minimal()


head(DA_data_df[DA_data_df$year > 1950 & DA_data_df$year < 2023, ])

DA_data_df[DA_data_df$model == "ERA5", ]
DA_data_df[DA_data_df$model == "CanESM5", ]
DA_data_df[DA_data_df$model == "CESM2", ]

CanESM5_n <- aggregate(. ~ year, DA_data_df[DA_data_df$year > 1950 & DA_data_df$year < 2023 & DA_data_df$model == "CanESM5", c(-2, -1)], FUN = median)[, c("year", "tx5d", "tx5d_ano", "gmst_ano")]
CESM2_n <- aggregate(. ~ year, DA_data_df[DA_data_df$year > 1950 & DA_data_df$year < 2023 & DA_data_df$model == "CESM2", c(-2, -1)], FUN = median)[, c("year", "tx5d", "tx5d_ano", "gmst_ano")]
ERA5 <- DA_data_df[DA_data_df$year > 1950 & DA_data_df$year < 2023 & DA_data_df$model == "ERA5", ][, c("year", "tx5d", "tx5d_ano", "gmst_ano")]


DA_data_df_n <- cbind(data.frame(CanESM5_n, model = "CanESM5_n")[, "tx5d"],
                      data.frame(CESM2_n, model = "CESM2_n")[, "tx5d"],
                      data.frame(ERA5, model = "ERA5")[, "tx5d"])
colnames(DA_data_df_n) <- c("CanESM5_n", "CESM2_n", "ERA5")
pairs(DA_data_df_n)




CanESM5_n <- DA_data_df[DA_data_df$year > 1950 & DA_data_df$year < 2023 & DA_data_df$model == "CanESM5", c(-2, -1)][, c("year", "tx5d", "tx5d_ano", "gmst_ano")]
CESM2_n <- DA_data_df[DA_data_df$year > 1950 & DA_data_df$year < 2023 & DA_data_df$model == "CESM2", c(-2, -1)][, c("year", "tx5d","tx5d_ano", "gmst_ano")]
ERA5 <- DA_data_df[DA_data_df$year > 1950 & DA_data_df$year < 2023 & DA_data_df$model == "ERA5", ][, c("year", "tx5d","tx5d_ano", "gmst_ano")]

DA_data_df_n <- rbind(data.frame(CanESM5_n, model = "CanESM5_n"),
                      data.frame(CESM2_n, model = "CESM2_n"),
                      data.frame(ERA5, model = "ERA5"))


p0 <- 1901 / 1980
p1 -> 2007 / 1991

DA_data_df_n %>%
  ggplot(aes(x = tx5d_ano, colour = model)) +
  geom_density(adjust = 1, linewidth = 1.5) +
  geom_vline(xintercept = 12)

DA_data_df_n %>%
  ggplot(aes(x = tx5d, colour = model)) +
  geom_density(adjust = 1, linewidth = 1.5)


DA_data_df_n
