# Bryophyte historical data

# Libraries ----
pacman::p_load(
  "terra","dplyr","stringr","tidyterra","magrittr","ggplot2","ncdf4","knitr",
  "data.table","SpatialUKCEH","RColorBrewer","DT","sf",
  "AgricInvUKCEH","writexl","readr","janitor")

wd <- "C:/Users/hanlit/OneDrive - UKCEH/Documents/R/GitHub/vegetation"
moss <- read_csv("data/records-2025-04-22.csv")

moss1 <- moss %>%
  clean_names() %>%
  filter(scientific_name == "Pleurozium schreberi") %>%
  select(scientific_name, occurrence_status, start_date, start_date_year,
         latitude_wgs84, longitude_wgs84, identification_verification_status)


moss1_sf <- st_as_sf(moss1, coords = c("longitude_wgs84", "latitude_wgs84"), crs = 4326)

# Create a plot for each year
unique_years <- unique(moss1_sf$start_date_year)

for (year in unique_years) {
  moss_year <- moss1_sf %>% filter(start_date_year == year)
  
  p <- ggplot() +
    geom_sf(data = moss_year, aes(color = occurrence_status)) +
    labs(title = paste("Occurrences in", year),
         x = "Longitude",
         y = "Latitude") +
    theme_minimal()
  
  print(p)
}

p

# ggsave(str_c(
#   wd, "/output/veg_pleuroz", ".png"),
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
