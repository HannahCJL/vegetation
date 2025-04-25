# Defra historical time series for N and S

# Edits ----
usr <- "hanlit" # "hanlit", "edcarn"
files_create <- F

# Libraries ----
pacman::p_load(
  "terra","dplyr","stringr","tidyterra","magrittr","ggplot2","ncdf4","knitr",
  "data.table","SpatialUKCEH","RColorBrewer","DT","leaflet","sf",
  "AgricInvUKCEH","writexl")

## 1.3 - File directories ----
if(usr == "hanlit"){
  # working
  wd <- "P:/NEC07358_NitrogenFutures/4_NF-2_Modelling_2022/ScenarioModelling/"
  # project
  pd <- str_c(wd, "R/hanlit/nfutures_scenario_modelling/")}

# CBED files path
cbed_path <- "P:/08868_CWI-TBC-JNCC_APIS_2024/rCBED/outputsCBED_historic_new_emissions_no_duplicates/"
# files input
fd <- str_c(wd, "input/historical_emep/")
od <- str_c("output/") 

## 1.4 - Functions script ----
source("functions.R")


dep_type <- "M" # "W","M","G"
dep_type_select <- "totaln" # "totaln", "oxn", "rdn", "sox"

source("defra_historical_preparation_veg.R")
totaln_m_diff <- emep_diff
totaln_m_comp <- mergedrast

ndep_map <- maps_comp_hist(totaln_m_comp)
ndep_map

ggsave(str_c(od, "N dep map.png"), ndep_map, width = 12, height = 10)
