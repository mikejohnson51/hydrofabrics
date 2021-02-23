purge_flowlines = function(fl, cat){
  fl <- lapply(1:nrow(cat), function(x) { fromJSON(cat$comids[x]) }) %>%
    purrr::map_df(tibble::enframe, name = "tmp", .id = "ID") %>%
    dplyr::select(-tmp, comid = value) %>%
    mutate(ID = cat$ID[as.numeric(ID)]) %>%
    right_join(fl) %>%
    group_by(ID) %>%
    slice_max(streamorde) %>%
    st_as_sf() %>%
    summarize(hydroseq_min = min(hydroseq_min))

  for (i in 1:nrow(fl)) {
    if (st_geometry_type(fl$geom[i]) == "MULTILINESTRING") {
      fl$geom[i] <- st_line_merge(fl$geom[i])
    }
  }

  fl$lengthkm <- as.numeric(set_units(st_length(fl), "km"))

  return(list(fl = fl, cat = cat))
}
