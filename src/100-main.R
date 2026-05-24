###################################################################
#----------- Lifetime stillbirths and neonatal deaths ------------# 
###################################################################

## --- FILE 100-main.R: INSTALL AND LOAD PACKAGES, RUN ALL OTHER '100' FILES ---

## clear environment
rm(list = ls())

## packages to be installed from cran
from.cran <- c("data.table",  
               "gt", "gtsummary", "scales", "patchwork",
               "here","rnaturalearth", "sf", 
               "officer", "flextable", "ggrepel", "devEMF",
               "rnaturalearthdata", "tidyverse", "viridis", "ggsci", "gridExtra",
               "readr", "readxl", "paletteer", "biscale", "cowplot", "classInt", 
               "purrr", "tibble")


for(i in c(from.cran)){
  
  ## check if installed, else install
  if(system.file(package = i) == ""){install.packages(i)}
  
  ## load packages    
  library(i, character.only = TRUE)
  
}

## set path
here::i_am("src/100-main.R")

## calculate country-level lt indicators
source(here::here("src", "101-calc-country-lt.R"))

## generate country-level visualizations of lt indicators
source(here::here("src", "102-viz-country-lt.R"))

## calculate region-level lt
source(here::here("src", "103-calc-region-lt.R"))

## generate region-level visualizations of lt indicators
source(here::here("src", "104-viz-region-lt.R"))

## bivariate analysis of ltr and tfr
source(here::here("src", "105-tfr-bivar.R"))

## analysis of stb contribution to ltr-loss
source(here::here("src", "106-stb-contribution.R"))

## analysis of ante- and intrapartum stb contribution to ltr-stb
source(here::here("src", "107-stb-timing.R"))

## sensitivity analysis of population heterogeneity
source(here::here("src", "108-heterogeneity.R"))

## statistics for manuscript
source(here::here("src", "109-intext-statistics.R"))
