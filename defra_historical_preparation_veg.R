# Defra Species Abundance Modelling ----
# Historical time series of N & S dep
# Data Preparation Script 
# Difference to present day (2020)

# 1 - Housekeeping ----

## 1.5 - Assigning selections ----
### dep types
if (dep_type == "G") {
  title = "Grid Average"
  deptype = "gridavg"
  colname = "Grid"
} else if (dep_type == "M") {
  title = "Low-growing Semi-natural"
  deptype = "moor"
  colname = "Seminat"
} else if (dep_type == "W") { 
  title = "Woodland"
  deptype = "forest"
  colname = "Forest"
}

### EMEP subds
if (dep_type_select == "totaln") {
  emep_varnm <- c("WDEP_OXN","WDEP_RDN",str_c("DDEP_OXN_m2", colname),
                  str_c("DDEP_RDN_m2", colname))
} else if (dep_type_select == "oxn") {
  emep_varnm <- c("WDEP_OXN",str_c("DDEP_OXN_m2", colname))
} else if  (dep_type_select == "rdn") {
  emep_varnm <- c("WDEP_RDN",str_c("DDEP_RDN_m2", colname))
} else if (dep_type_select == "sox") {
  emep_varnm <- c("WDEP_SOX",str_c("DDEP_SOX_m2", colname))
}

### CBED cols 
if (dep_type_select %in% c("totaln")) {
  cbed_col_select <- c("NOx", "NHx")
  masseq <- 14
  leg_label <- "N"
} else if (dep_type_select == "oxn") {
  cbed_col_select <- "NOx"
  masseq <- 14 
  leg_label <- "N"
} else if (dep_type_select == "rdn") {
  cbed_col_select <- "NHx"
  masseq <- 14 
  leg_label <- "N"
} else if (dep_type_select == "sox") {
  cbed_col_select <- "SOx"
  masseq <- 32 
  leg_label <- "S"
}

#
# Sulpur info ----
# xSOx - non marine
# from CBED SOx everything incl marine compononent
# EMEP:
# "DDEP_SOX_m2Grid"           "DDEP_SOX_m2Conif"         
# [66] "DDEP_SOX_m2Seminat"      "DDEP_SOX_m2Decid"         
# wet:
#   "WDEP_SOX" 
#

# 2 - EMEP Data wrangling ----

## 2.1 - Files ----
nc_fls <- list.files(fd, pattern = "0_fullrun\\.nc$") # years edit ----
# Checks/look at file
nc1 <- nc_open(str_c(fd,nc_fls[1]))
names(nc1$var)
nc1$var$DDEP_SOX_m2Seminat$units
nc1$var$WDEP_RDN$units

## 2.2 - Extract scenario names from file names ----
scenarios <- sapply(nc_fls, function(x) {
  # sub(".*trend(....).*", "\\1", x)
  sub(".*trend(....).*", str_c("yr", "\\1"), x)
})

## 2.3 - Preparing EMEP rasters ----
# Reading in all raster files
# emep_hist is in mg equivalent /m2 /year
if (dep_type %in% c("M","G")) {
  emep_hist <- rast(mapply(
    FUN = function(x) {
      r_emep <- rast(str_c(fd, x),
                     subds = c(emep_varnm)) %>%
        app(sum)
    }, x = nc_fls)) %>%
    rename_with(~ scenarios) %>%
    app(., fun=function(x){x/masseq}) # %>% # /14
  # app(., fun=unitchange) # /100
  emep_hist_df <- emep_hist %>% as.data.frame(xy=T)
}

if (dep_type == "W") {
  emep_hist <- rast(mapply(
    FUN = forest_fraction_scen, nc_fl = nc_fls, 
    ndep_type = dep_type_select)) %>%
    rename_with(~ scenarios) %>%
    app(., fun=function(x){x/masseq}) %>%
    app(., fun=function(x){x*100}) #converting to mg/m2
  emep_hist_df <- emep_hist %>% as.data.frame(xy=T)
}

# List of 5-year intervals
years <- seq(1960, 2020, by = 10)

# Create new df
emep_interpolated <- emep_hist

# Loop through each 5-year interval and interpolate
# for (i in 1:(length(years) - 1)) {
#   start_year <- str_c("yr", years[i])
#   end_year <- str_c("yr", years[i + 1])
#   emep_interpolated <- interpolate_years(
#     emep_interpolated, start_year, end_year)
# }

# Reorder columns alphabetically
emep_interpolated <- emep_interpolated %>%
  select(sort(names(.))) 

# change units from mgN/m2 to kg/ha - already done above
# emep_interpolated <- emep_interpolated/100

# View the updated data frame
print(emep_interpolated)
emep_interpolated_df <- emep_interpolated %>% as.data.frame(xy=T)

### Save emep comparison ----
if (files_create == T) {
  writeRaster(
    x = emep_interpolated,
    filename = str_c(
      od, "historical_", dep_type_select,
      "_mgeqm2year_", "emep_comparison_", colname, ".tif"),
    overwrite = T)
  write_xlsx(
    emep_interpolated_df,
    str_c(od, "historical_", dep_type_select,
          "_mgeqm2year_", "emep_comparison_", colname, ".xlsx"))
}


## 2.4 - Difference to present day (2020) ----
# Compare all years to 2020

# year - year2020 ?
emep_interpolated_diff <- emep_interpolated %>%
  mutate(across(
    1:6,
    ~ .x - yr2020
  )) %>%
  select(-yr2020) 
emep_interpolated_diff_df <- emep_interpolated_diff %>% as.data.frame(xy=T)

### Save emep difference ----
if (files_create == T) {
  writeRaster(
    x = emep_interpolated_diff,
    filename = str_c(od, "historical_", dep_type_select,
                     "_mgeqm2year_", "emep_difference_", colname, ".tif"),
    overwrite = T)
  write_xlsx(
    emep_interpolated_diff_df,
    str_c(od, "historical_", dep_type_select,
          "_mgeqm2year_", "emep_difference_", colname, ".xlsx"))
}


### Converting EMEP units to kg/ha/yr non equivalent ----
emep_diff_kg <- emep_interpolated_diff %>%
  app(., fun = function(x){x*masseq}) %>% # eq to non eq
  app(., fun = unitchange) %>% # mg/m2 to kg/ha 
  mutate(yr2020 = 0)
emep_diff_kg_df <- emep_diff_kg %>% as.data.frame(xy=T)

### Converting EMEP to bng ----
emep_diff <- emep_to_bng(r=emep_diff_kg, res=1000) %>%
  # Remove coastal values
  terra::mask(., sf_uk)
emep_diff_df <- emep_diff %>% as.data.frame(xy=T)


# 3 - CBED and spatial data wrangling ----

## 3.1 - CBED baseline ----
# read in CBED file
cbed_base_read <- fread(str_c(
  cbed_path,"rCBED_Fdep_", deptype, "_keqHaYr_20202022_1km.csv")) 

# Choose coords and nox, nhx. Rasterise 
# cbed_base is in kg non equiv /ha /year
cbed_base <- cbed_base_read %>%
  select(easting, northing, all_of(cbed_col_select)) %>%
  rast(type = "xyz", crs = "EPSG:27700") %>%
  app(., fun = function(x){
    if (dep_type_select == "totaln") {
      sum(x * masseq)} 
    else if (dep_type_select %in% c("sox","oxn","rdn")) {
      x*masseq 
    }})
  # app(., fun = function(x){sum(x)})
# 14 is kilo equivalent. kg of Nitrogen
cbed_base_df <-cbed_base %>% as.data.frame(xy=T)

## 3.2 - Spatial crs wrangling ----
# Check extents
ext(emep_diff)
ext(cbed_base)
# Different extents, extend to emep extent
cbed_base <- extend(cbed_base, ext(emep_diff))
# emep_diff <- extend(emep_diff, ext(cbed_base))

# Check origin and resolution of both spatial rasters
origin(emep_diff)
origin(cbed_base)
res(emep_diff)
res(cbed_base)
crs(emep_diff)
crs(cbed_base)

## 3.3 - Merge CBED and EMEP rasters ----
# this is in kg non equiv /ha /yr, as are both emep_diff and cbed_base
mergedrast <- app(c(emep_diff, cbed_base), fun = c) %>%
  mutate(V1 = lyr.1) %>% select(-lyr.1) %>%
  mutate(across( 
    yr1960:yr2020,
    ~ .x + V1)) %>%
  select(-V1) %>%
  # Remove coastal values
  terra::mask(., sf_uk)
mrgrast <- mergedrast %>% as.data.frame(xy=T)

### convert to mg eq /m2/yr ----
merged_mgeq <- mergedrast %>%
  app(., fun=function(x){x/masseq}) %>%
  app(., fun=function(x){x*100}) 
merged_mgeq_df <- merged_mgeq %>% as.data.frame(xy=T)

if (files_create == T) {
  # 3.4 - Save CBED with EMEP difference ----
  writeRaster(
    x = merged_mgeq,
    filename = str_c(
      od, "historical_", dep_type_select,
      "_mgeqm2year_", "cbed_emep_", colname, ".tif"),
    overwrite = T)
  write_xlsx(
    merged_mgeq_df,
    str_c(od, "historical_", dep_type_select,
           "_mgeqm2year_", "cbed_emep_", colname, ".xlsx"))
}


#### END OF SCRIPT ####