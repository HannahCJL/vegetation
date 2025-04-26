# Change from mgN/m2 to kg/ha
# mg to kg: /1,000,000
# m^-2 to ha^-1: *10,000
# overall: /100
unitchange = function(i) {i/100}


# Function to convert EMEP4UK output to British National Grid (for site comparison)
emep_to_bng <- function(r,res){
  crs(r) <- "+proj=stere +ellps=sphere +lat_0=90.0 +lon_0=0 +x_0=0.0 +y_0=0.0 +units=m +k_0=0.933012701892219 +no_defs +type=crs"
  r_ref <- rast(crs = "EPSG:27700",
                ext = c(xmin = -10000, xmax = 660000,
                        ymin = 0, ymax = 1220000), res = res)
  project(r, r_ref, method = "bilinear")
}

# raster_select <- totaln_m_comp
maps_comp_hist <- function(raster_select) {
  
  # Spatial preparation 
  stk_diff <- raster_select
  r_ref <- rast(crs = "EPSG:27700",
                ext = c(xmin = -10000, xmax = 660000,
                        ymin = 0, ymax = 1220000), res = 1000)
  
  r_prj <- project(stk_diff, r_ref, method = "bilinear")
  
  # brks <- c(-20,-10,-5,-1,-0.5,0.5,1,5,10,20)
  
  # brks <- c(0,5,10,20,30,50)
  brks <- c(0,5,10,15,20,40)
  
  
  mnVal <- global(r_prj,min,na.rm = T) %>% min %>% floor
  mxVal <- global(r_prj,max,na.rm = T) %>% max %>% ceiling
  brks_sub <- brks[brks > mnVal & brks < mxVal]
  brks_sub <- c(mnVal, brks_sub, mxVal)
  
  # Functions from SpatialUKCEH package
  labs <- brks_2_labs(brks = brks_sub,include.lowest = T,include.max = F)
  # cols <- brks_2_cols(brks = brks_sub,midpnt = 0)
  cols <- c("#9ccfff",colorRampPalette(c("#fff06b", "#ffa82e", "#ff2e2e",
                                         "#5400a8"))(length(brks_sub)-1))
  
  rcl <- matrix(c(c(brks_sub[-length(brks_sub)]),
                  c(brks_sub[2:length(brks_sub)]),
                  1:(length(brks_sub)-1)), 
                ncol = 3)
  
  msrm <- str_c(leg_label, " deposition")
  unit <- str_c("kg~",leg_label,"~ha^-1~year^-1")
  
  r_cla <- classify(r_prj, rcl = rcl, include.lowest = T) %>% 
    terra::as.factor()
  
  ## Plot maps
  p <- ggplot()
  # Spatial raster
  p <- p + geom_spatraster(data = r_cla)
  # Plot with plots side by side
  p <- p + facet_wrap(~lyr, nrow = 2,
                      labeller = label_wrap_gen(width = 15,multi_line = T))
  # Use own colours and fit to bins/breaks
  if (unit != "") {
    p <- p + scale_fill_manual(
      name = bquote(atop(.(msrm),.(parse(text = unit)[[1]]))),
      breaks = 1:length(labs), 
      labels = labs, values = cols, 
      na.value = "transparent")
  } else {
    p <- p + scale_fill_manual(
      name = msrm,
      breaks = 1:length(c), 
      labels = labs, values = cols, 
      na.value = "transparent")
  }
  # Add country outline
  p <- p + geom_sf(data = sf_uk[sf_uk$COUNTRY != "Isle of Man",],
                   colour = "black", fill = NA, size = 0.2)
  p <- p + ggpl_thme(fnt_sze = 14, axes_lne = F,axes_txt = F)
  
  p
  
}

# Function to select the subds for tree type and oxidation, reduction or total N
forest_subds <- function(ndep_type, tree) {
  
  if (ndep_type == "totaln") {
    subds_select <- paste0(
      c("WDEP_OXN","WDEP_RDN",paste0("DDEP_OXN_m2",tree),
        paste0("DDEP_RDN_m2",tree)))
  }
  else if (ndep_type == "oxn") {
    subds_select <- paste0(
      c("WDEP_OXN",paste0("DDEP_OXN_m2", tree)))
  }
  else if (ndep_type == "rdn") {
    subds_select <- paste0(
      c("WDEP_RDN",paste0("DDEP_RDN_m2", tree)))
  }
  else if (ndep_type == "sox") {
    subds_select <- paste0(
      c("WDEP_SOX",paste0("DDEP_SOX_m2", tree)))
  }
}

# Calculates the area fraction of coniferous vs deciduous and applies to 
# N dep
forest_fraction_scen <- function(nc_fl, ndep_type){
  
  # File path
  scen_fp <- paste0(
    fd, nc_fl)
  
  # Load 
  r_emep_cnf <- rast(
    scen_fp,
    subds = forest_subds(ndep_type, "Conif")) %>%
    app(., fun=unitchange)
  r_emep_dec <- rast(
    scen_fp,
    subds = forest_subds(ndep_type, "Decid")) %>%
    app(., fun=unitchange)
  r_lnd_fract <- rast(
    scen_fp,
    subds = c("Area_Conif_Frac","Area_Decid_Frac"))
  
  # Calculate area fraction
  # decid/total weight
  r_wght <- r_lnd_fract[[2]]/(r_lnd_fract[[1]] + r_lnd_fract[[2]])
  r_wght[is.na(r_wght)] <- 0.5
  r_wght_df <- r_wght %>% as.data.frame()
  
  # Apply weighting
  r_emep_dec_adj <- r_emep_dec * r_wght
  r_emep_cnf_adj <- r_emep_cnf * (1 - r_wght)
  
  r_emep_dec_adj_df <- r_emep_dec_adj %>% as.data.frame()
  r_emep_cnf_adj_df <- r_emep_cnf_adj %>% as.data.frame()
  
  # Apply to each ndep_varnm
  r_emep_21 <- lapply(X = 1:nlyr(r_emep_dec_adj), FUN = function(i){
    r_emep_dec_adj[[i]] + r_emep_cnf_adj[[i]]
  }) %>% do.call(c,.)
  
  # Sum the ndep types for 1 sum
  r_emep_21 <- app(r_emep_21, sum)
  r_emep_21_df <- r_emep_21 %>% as.data.frame()
  
  r_emep_21
}
