# Bryophyte historical data

# Libraries ----
pacman::p_load(
  "terra","dplyr","stringr","tidyterra","magrittr","ggplot2","ncdf4","knitr",
  "data.table","SpatialUKCEH","RColorBrewer","DT","sf",
  "AgricInvUKCEH","writexl","readr","janitor","readxl")

wd <- "C:/Users/hanlit/OneDrive - UKCEH/Documents/R/GitHub/vegetation"

# NBN atlas data ----
# moss <- read_csv("data/records-2025-04-22.csv")
# 
# moss1 <- moss %>%
#   clean_names() %>%
#   filter(scientific_name == "Pleurozium schreberi") %>%
#   select(scientific_name, occurrence_status, start_date, start_date_year,
#          latitude_wgs84, longitude_wgs84, identification_verification_status)

# Hypnum cupressiforme ----
# moss <- read_csv("data/records-2025-04-26_-_hypnum_cupressiforme_var_cup.csv")
# 
# moss1 <- moss %>%
#   clean_names() %>%
#   select(scientific_name, occurrence_status, start_date, start_date_year,
#          latitude_wgs84, longitude_wgs84, identification_verification_status) %>%
#   arrange(start_date_year)
# 
# moss1_sf <- st_as_sf(moss1, coords = c("longitude_wgs84", "latitude_wgs84"), crs = 4326)

# Map for each year
# Decadal data?

p <- ggplot(moss1_sf) +
  geom_sf(aes(color = occurrence_status)) +
  facet_wrap(~ start_date_year, nrow = 2) +
  labs(title = "Occurrences by Year",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)

# ggsave(str_c(
#   wd, "/output/vegetation_map_",moss1$scientific_name[1], ".png"),
#   p, width = 10, height = 10, dpi = 300, bg = "#FFFFFF")


####

# Define the raster template
r_template <- rast(crs = "EPSG:4326", ext = ext(moss1_sf), resolution = 0.001)

# Rasterize the vector data for each year
moss_raster_list <- list()
unique_years <- unique(moss1_sf$start_date_year)

for (year in unique_years) {
  moss_year <- moss1_sf %>% filter(start_date_year == year)
  moss_raster_year <- rasterize(moss_year, r_template, field = "occurrence_status", fun = "count")
  moss_raster_year <- as.data.frame(moss_raster_year, xy = TRUE)
  moss_raster_year$year <- year
  moss_raster_list[[as.character(year)]] <- moss_raster_year
}

moss_raster_df <- bind_rows(moss_raster_list)

p <- ggplot(moss_raster_df) +
  geom_tile(aes(x = x, y = y, fill = occurrence_status)) +
  facet_wrap(~ year, nrow = 3) +
  labs(title = "Occurrences of Pleurozium schreberi",
       x = "Longitude",
       y = "Latitude",
       fill = "Occurrence Status") +
  theme_minimal()

print(p)


# BRYOATT dataset ----

bryoatt <- read_excel("data/Bryoatt_updated_2017/Bryoatt_updated_2017.xls") %>%
  clean_names()
bryoatt_n <- bryoatt %>%
  select(name_new, taxon_name, ml, ord, l,f,r,n,s) %>%
  filter(n == 2)

fsc_spec <- read_excel("data/fsc_collected_species.xlsx") %>%
  clean_names() %>%
  rename(name_new = species)

comb <- semi_join(bryoatt_n, fsc_spec, by = "name_new")

bryoatt_pp <- bryoatt %>%
  filter(name_new == "Polytrichum piliferum")

summary(bryoatt$n)

bryoatt_nlove <- bryoatt %>%
  filter(n %in% c(5,6,7)) %>%
  arrange(desc(g_bno), desc(n)) %>%
  slice_head(n = 15) %>%
  select(name_new, taxon_name, ml, ord, g_bno, l,f,r,n,s) 
  
bryoatt_nhate <- bryoatt %>%
  filter(n %in% c(1,2)) %>%
  arrange(desc(g_bno)) %>%
  # slice_head(n = 15) %>%
  select(name_new, taxon_name, ml, ord, g_bno, l,f,r,n,s) 


comb_nlove <- semi_join(bryoatt_nlove, fsc_spec, by = "name_new")
comb_nhate <- semi_join(bryoatt_nhate, fsc_spec, by = "name_new")
spec_nlove <- comb_nlove %>% select(name_new)
spec_nhate <- comb_nhate %>% select(name_new)
# Save
# fwrite(spec_nlove, str_c(
#   "output/", "species_nlove.csv"))
# fwrite(spec_nhate, str_c(
#   "output/", "species_nhate.csv"))

comb_nlove_simple <- comb_nlove %>%
  select(name_new, taxon_name, ml, ord, l,f,r,n,s)
# remove atrichum undulatum for similar number of records


# g_bno for counts across great britain



# Dot plots ----

## Prep ----
wd <- "C:/Users/hanlit/OneDrive - UKCEH/Documents/R/GitHub/vegetation"

# file_name <- "records-2025-04-27_N_hating_species_incl_2019"
file_name <- "records-2025-04-27_N_loving_species_incl_2019"
nsensitive = T

# annual count of observations data 
annual <- read_csv(str_c("data/annual_obs_count.csv")) %>%
  rename(annual_count = count)

moss <- read_csv(str_c("data/", file_name, ".csv")) 

moss1 <- moss %>%
  clean_names() %>%
  select(scientific_name, occurrence_status, start_date, start_date_year,
         latitude_wgs84, longitude_wgs84,identification_verification_status) %>%
  arrange(start_date_year) %>%
  filter(!is.na(latitude_wgs84)) %>%
  filter(start_date_year != 2020) %>%
  mutate(start_date_year = ifelse(start_date_year == 2019, 2020,
                                  start_date_year)) %>%
  left_join(., annual, by = c("start_date_year"="year")) 

moss1_sf <- st_as_sf(moss1, coords = c("longitude_wgs84", "latitude_wgs84"), 
                     crs = 4326)
moss1_sf <- st_transform(moss1_sf, crs = 27700)

years <- c(1960,1970,1980,1990,2000,2010,2020)
# years <- c(1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2020)


## By year ----
lapply(X = years, FUN = function(yr){
  moss_sf <- moss1_sf %>%
    filter(start_date_year == yr)
  
  ndep <- mergedrast %>%
    subset(str_c("yr",yr))
  
  extracted <- terra::extract(ndep, moss_sf) 
  names(extracted) <- c("id","value")
  extracted <- extracted %>%
    filter(!is.na(value))
  
  return(data.frame(year = yr, nsens = nsensitive, count = nrow(extracted), mean = mean(extracted$value),
                    median = median(extracted$value)))
}) %>% do.call(rbind, .)

## By species and year ----
sp_yr_n <- lapply(X = unique(moss1_sf$scientific_name), FUN = function(spname) {
  
lapply(X = years, FUN = function(yr){
  moss_sf <- moss1_sf %>%
    filter(start_date_year == yr,
           scientific_name == spname)
  
  ndep <- mergedrast %>%
    subset(str_c("yr",yr))
  
  extracted <- terra::extract(ndep, moss_sf) 
  names(extracted) <- c("id","value")
  extracted <- extracted %>%
    filter(!is.na(value))
  
  return(data.frame(year = yr, nsens = nsensitive, scientific_name = spname, 
                    count = nrow(extracted), mean = mean(extracted$value),
                    median = median(extracted$value)))
}) %>% do.call(rbind, .)
}) %>% do.call(rbind, .)

sp_plot <- issp_yr_n %>%
  ggplot() +
  geom_point(aes(x = mean, y = count, colour = `scientific_name`), size=3)+
  geom_text(aes(x = mean, y = count, label = year), vjust = -0.5, size = 3,
            colour = "grey") +
  # geom_point(aes(x = mean, y = percentage, colour = `scientific_name`), size=3)+
  # geom_text(aes(x = mean, y = percentage, label = year), vjust = -0.5, size = 3,
  #           colour = "grey") + 
  facet_wrap(~ scientific_name, scales = "free_y") +
  labs(title = "",
       x = "Mean nitrogen deposition",
       y = "Count of recorded species") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

sp_plot

# Save 
ggsave(str_c(
  wd, "/output/dotplot_spec_ntol", ".png"),
  sp_plot, width = 8, height = 7, dpi = 300, bg = "#FFFFFF")

## Weighted plot species and year ----

issp_yr_n <- lapply(X = unique(moss1_sf$scientific_name), FUN = function(spname) {
  
  lapply(X = years, FUN = function(yr){
    moss_sf <- moss1_sf %>%
      filter(start_date_year == yr,
             scientific_name == spname)
    
    ndep <- mergedrast %>%
      subset(str_c("yr", yr))
    
    extracted <- terra::extract(ndep, moss_sf) 
    names(extracted) <- c("id", "value")
    extracted <- extracted %>%
      filter(!is.na(value))
    
    # Get the annual count for the year
    annual_count <- moss1_sf %>%
      filter(start_date_year == yr) %>%
      pull(annual_count) %>%
      unique()
    
    # Calculate percentage
    percentage <- (nrow(extracted) / annual_count) * 100
    
    return(data.frame(year = yr, nsens = nsensitive, scientific_name = spname, 
                      count = nrow(extracted), percentage = percentage, mean = mean(extracted$value),
                      median = median(extracted$value)))
  }) %>% do.call(rbind, .)
}) %>% do.call(rbind, .)

sp_plot <- issp_yr_n %>%
  ggplot() +
  geom_point(aes(x = mean, y = percentage, colour = `scientific_name`), size=3)+
  geom_text(aes(x = mean, y = percentage, label = year), vjust = -0.5, size = 3,
            colour = "grey") + 
  facet_wrap(~ scientific_name, scales = "free_y") +
  labs(title = "",
       x = "Mean nitrogen deposition",
       y = "Percentage of annual records of all bryophytes") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

sp_plot

# Save 

ggsave(str_c(
  wd, "/output/dotplot_spec_nsens_pct", ".png"),
  sp_plot, width = 8, height = 7, dpi = 300, bg = "#FFFFFF")


# Species ----
sort(unique(moss1$scientific_name))
# nsens - both ellenberg 2
"Ptilidium ciliare"          "Rhytidiadelphus loreus"  
# ntol -all 5
"Atrichum undulatum"  "Lophocolea heterophylla"  "Eurhynchium striatum"   