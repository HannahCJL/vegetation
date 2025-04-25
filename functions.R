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
