# Defra historical time series for N and S

# 1 - Housekeeping ----

## 1.1 - Edits ----
usr <- "hanlit" # "hanlit", "edcarn"
files_create <- F
maps_create <- F

## 1.2 - Libraries ----
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
# files output
od <- str_c("output/") 

## 1.4 - Functions script ----
source("functions.R")

# 2 - Create plots ----

## 2.1 - N dep, M ----
dep_type <- "M" # "W","M","G"
dep_type_select <- "totaln" # "totaln", "oxn", "rdn", "sox"

source("defra_historical_preparation_veg.R")
totaln_m_diff <- emep_diff
totaln_m_comp <- mergedrast

dep_map <- maps_comp_hist(totaln_m_comp)
dep_map

if (maps_create == T) {
  ggsave(filename = str_c(od, "historical_", dep_type_select,  
                          "_cbed_emep_", colname, "_map.png"), 
         plot = dep_map, width = 12, height = 10)
}


## 2.2 - N dep, W ----
dep_type <- "W" # "W","M","G"
dep_type_select <- "totaln" # "totaln", "oxn", "rdn", "sox"

source("defra_historical_preparation_veg.R")
totaln_w_diff <- emep_diff
totaln_w_comp <- mergedrast

dep_map <- maps_comp_hist(totaln_w_comp)
dep_map

if (maps_create == T) {
  ggsave(filename = str_c(od, "historical_", dep_type_select,  
                          "_cbed_emep_", colname, "_map.png"), 
         plot = dep_map, width = 12, height = 10)
}

## 2.3 - S dep moorland ----
dep_type <- "M" # "W","M","G"
dep_type_select <- "sox" # "totaln", "oxn", "rdn", "sox"

source("defra_historical_preparation_veg.R")
sox_m_diff <- emep_diff
sox_m_comp <- mergedrast

dep_map <- maps_comp_hist(sox_m_comp)
dep_map

if (maps_create == T) {
  ggsave(filename = str_c(od, "historical_", dep_type_select,  
                          "_cbed_emep_", colname, "_map.png"), 
         plot = dep_map, width = 12, height = 10)
}

## 2.3 - S dep moorland ----
dep_type <- "W" # "W","M","G"
dep_type_select <- "sox" # "totaln", "oxn", "rdn", "sox"

source("defra_historical_preparation_veg.R")
sox_w_diff <- emep_diff
sox_w_comp <- mergedrast

dep_map <- maps_comp_hist(sox_w_comp)
dep_map

if (maps_create == T) {
  ggsave(filename = str_c(od, "historical_", dep_type_select,  
                          "_cbed_emep_", colname, "_map.png"), 
         plot = dep_map, width = 12, height = 10)
}
