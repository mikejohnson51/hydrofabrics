source('workflow/utils.R')
#############################################################################
path       <- "workflow/cache/ngen_01a-2.gpkg"
out_path   <- "workflow/cache/ngen_01a-3.gpkg"
min_area   <- 3
max_area   <- 15
min_length <- 0.6

cat = read_sf(path, "catchments") %>%
  select(-areasqkm)

fl  = read_sf(path, "flowpaths") %>%
  left_join(st_drop_geometry(cat), by = "ID") %>%
  flowpaths_to_linestrings()

cat = left_join(cat, st_drop_geometry(select(fl, ID, toID)), by = "ID")

#############################################################################
# merge short
shorts = filter(fl, stream_order == 1, lengthkm < min_length)

cat_dt = mutate(data.table(cat), ind = 1:n())
fl_dt  = mutate(data.table(fl),  ind = 1:n())

removed_ids = list()

for (i in 1:nrow(shorts)) {
  short_cat = filter(cat_dt,  ID == shorts$ID[i])
  int_cat   = filter(cat_dt,  ID == shorts$toID[i])
  if(nrow(short_cat) != 0 & nrow(int_cat) != 0 ){
    new = st_union(short_cat$geom, int_cat$geom)
    cat_dt[int_cat$ind, geom := st_as_sf(new)]
  }

  removed_ids[[i]] = shorts$ID[i]
  short_fl = filter(fl_dt,  ID == shorts$ID[i])
  int_fl = filter(fl_dt,  ID == shorts$toID[i])

  if(nrow(short_fl) != 0 & nrow(int_fl) != 0 ){
    new = st_union(short_fl$geom, int_fl$geom)
    fl_dt[int_fl$ind, geom := st_as_sf(new)]
    fl_dt[int_fl$ind, member_COMID := paste0(short_fl$member_COMID, int_fl$member_COMID)]
  }
}

removed_ids = unlist(removed_ids)

new_cat = filter(st_as_sf(cat_dt), !ID %in% removed_ids) %>%
  mutate(areasqkm = as.numeric(st_area(.)/1e6)) %>%
  select(ID, areasqkm)

new_fl =  filter(st_as_sf(fl_dt), !ID %in% removed_ids) %>%
  flowpaths_to_linestrings() %>%
  mutate(lengthkm = as.numeric(st_length(.)/1e3)) %>%
  select(-ind, -areasqkm) %>%
  left_join(st_drop_geometry(new_cat), by = 'ID')

new_cat = left_join(new_cat, st_drop_geometry(select(fl, ID, stream_order, toID)), by = "ID")

#################################################
# write_sf(new_cat, "workflow/cache/tester.gpkg", "cats")
# write_sf(new_fl, "workflow/cache/tester.gpkg", "fl")
#################################################

stouts = filter(new_cat, areasqkm <= min_area) %>%
  filter(stream_order == 1)  %>%
  mutate(int_area = new_cat$areasqkm[match(toID, new_cat$ID)],
         merge_area = areasqkm + int_area) %>%
  filter(merge_area <= (max_area + 1))

cat_dt2 = mutate(data.table(new_cat), ind = 1:n())

removed_ids2 = list()

for (i in 1:nrow(stouts)) {
  short_cat = filter(cat_dt2,  ID == stouts$ID[i])
  int_cat   = filter(cat_dt2,  ID == stouts$toID[i])

  if(nrow(short_cat) != 0 & nrow(int_cat) != 0 ){
    new = st_union(short_cat$geom, int_cat$geom)
    cat_dt2[int_cat$ind, geom := st_as_sf(new)]
    removed_ids2[[i]] = stouts$ID[i]
  }
}

removed_ids2 = unlist(removed_ids2)

new_cat2 = filter(st_as_sf(cat_dt2), !ID %in% removed_ids2) %>%
  mutate(areasqkm = as.numeric(st_area(.)/1e6)) %>%
  select(ID, areasqkm)

new_fl2 =  filter(new_fl, !ID %in% removed_ids2) %>%
  flowpaths_to_linestrings() %>%
  mutate(lengthkm = as.numeric(st_length(.)/1e3))

plot(new_fl2[3,])

#################################################
#################################################
make_plot(new_fl2, new_cat2, "First Order Dissolve HF") %>%
  ggsave(filename = "workflow/cache/img/03-ngen-firstorder-dissolve.png",
         units = "in", height = 4, width = 8)

message("Dropped: ", nrow(cat) - nrow(new_cat2), " features")

unlink(out_path)
write_sf(new_cat2, out_path, "catchments")
write_sf(new_fl2, out_path, "flowpaths")

#################################################
#################################################
