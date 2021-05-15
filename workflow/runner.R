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

nexus_prefix     = "nex-"
catchment_prefix = "cat-"
waterbody_prefix = "wb-"

path  <- "workflow/cache/ngen_01a-2.gpkg"
dir = paste0("data/", Sys.Date(), "/")
fs::dir_create(dir)

cfile <- file.path(dir, "catchment_data.geojson")
wfile <- file.path(dir, "flowpath_data.geojson")
nfile <- file.path(dir, "nexus_data.geojson")

cfe <- file.path(dir, "catchment_edge_list.json")
wfe <- file.path(dir, "waterbody_edge_list.json")

###############################################################

fline = read_sf(path, "flowpaths") %>%
  select(ID, toID, lengthkm, LevelPathID) %>%
  mutate(from_nID = fline$ID)

fline <- left_join(fline,
                   select(st_drop_geometry(fline), .data$ID, to_nID = .data$from_nID),
                   by = c("toID" = "ID")) %>%
         mutate(to_nID = ifelse(is.na(to_nID), 0, to_nID))

nexus <- fline %>%
  st_coordinates() %>%
  as.data.frame() %>%
  group_by(L2) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(X, Y) %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(fline)) %>%
  mutate(ID = paste0(nexus_prefix, fline$to_nID)) %>%
  group_by(ID) %>%
  filter(row_number() == 1) %>%
  ungroup()


catchment_edge_list = bind_rows(
    select(st_drop_geometry(fline), ID = ID, toID = to_nID) %>%
      mutate(ID  = paste0(catchment_prefix, ID), toID = paste0(nexus_prefix, toID)),

    left_join(tibble(ID = unique(fline$to_nID)),
              select(st_drop_geometry(fline), ID = from_nID, toID = ID),
              by = "ID") %>%
    mutate(toID = ifelse(is.na(toID), 0, toID),
           ID = paste0(nexus_prefix, ID),
           toID = paste0(catchment_prefix, toID))

)

waterbody_edge_list <- select(st_drop_geometry(fline), ID, toID) %>%
    mutate(toID = ifelse(is.na(toID), 0, toID)) %>%
    mutate(ID = paste0(waterbody_prefix, ID),
           toID = paste0(waterbody_prefix, toID))

catchment_data = read_sf(path, "catchments") %>%
  select(ID, areasqkm) %>%
  mutate(ID = paste0(catchment_prefix, ID)) %>%
  left_join(catchment_edge_list, by = "ID")

flowpath_data = select(fline, ID, lengthkm, main_id = .data$LevelPathID) %>%
  mutate(ID = paste0(catchment_prefix, ID)) %>%
  left_join(catchment_edge_list, by = "ID")

nexus_data =  left_join(nexus, catchment_edge_list, by = "ID")

#taken from DB code...
write_geojson <- function(x, y) {
  names(x) <- tolower(names(x))
  write_sf(st_make_valid(st_transform(x, 4326)), y, layer_options = c("ID_FIELD=id", "ID_TYPE=String"))
}

write_fun(catchment_data, cfile)
write_fun(flowpath_data, wfile)
write_fun(x = nexus_data, nfile)

names(catchment_edge_list) <- tolower(names(catchment_edge_list))
names(waterbody_edge_list) <- tolower(names(waterbody_edge_list))

jsonlite::write_json(catchment_edge_list, cfe, pretty = TRUE)
jsonlite::write_json(waterbody_edge_list, wfe, pretty = TRUE)

write_sf(catchment_data, paste0(dir, "catchments.gpkg"), 'cat')
write_sf(flowpath_data, paste0(dir, "catchments.gpkg"), 'fl')
write_sf(nexus_data, paste0(dir, "catchments.gpkg"), 'nex')


