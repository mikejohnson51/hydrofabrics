# 00. This script is reading in the output from Dave B. code,

source('workflow/00_rectify_gfv20.R')
# 01. This script is aggregating catchments less then a set size
#   along the LevelPathID
# Notes:
#     min_size   <-  3
#     ideal_size <-  10
#     max_size   <-  15    # in km2
source('workflow/01_dissolve_levelpath.R')
# 02. Remove Interior Rings
source('workflow/02_island_removal.R')
# 03. Dissolve Along First Orders
source('workflow/03_first_order_dissolve.R')
# 04. Remove Interior Rings
source('workflow/04_island_removal.R')


################################################################
################################################################
################################################################
################################################################

path  <- "workflow/cache/ngen_01a-4.gpkg"

cats = read_sf(path, "catchments") %>%
  select(ID, areasqkm)

fl = read_sf(path, "flowpaths") %>%

write_sf(cats, paste0("data/", Sys.Date(), "_catchments.geojson"))
write_sf(fl, paste0("data/", Sys.Date(), "_catchments.geojson"))

################################################################
################################################################
################################################################
################################################################
