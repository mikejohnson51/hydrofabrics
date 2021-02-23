find_node = function(fl, position){
  tmp = data.frame(st_coordinates(fl))

  if(ncol(tmp) == 4){
    tmp %>%
      group_by(L2) %>%
      slice(ifelse(is.null(position), n(), position)) %>%
      ungroup() %>%
      st_as_sf(coords = c("X", "Y"), crs = st_crs(fl)) %>%
      st_geometry()
  } else {
    tmp %>%
      group_by(L1) %>%
      slice(ifelse(is.null(position), n(), position)) %>%
      ungroup() %>%
      st_as_sf(coords = c("X", "Y"), crs = st_crs(fl)) %>%
      st_geometry()
  }
}

aggregate_by_connectivity = function(fl, ideal_size){

inlets     <- st_set_geometry(fl, find_node(fl, position = 1)) %>%
  filter(!fromnode %in% tonode)

out        <- list()

for (i in 1:nrow(inlets)) {
  this.lp <- filter(fl, levelpathi == inlets$levelpathi[i]) %>% arrange(-hydroseq)
  values  <- this.lp$areasqkm
  indexes <- c()
  count   <- 1

  while (length(values) > 0) {
    inds    <- 1:which.min(abs(cumsum(values) - ideal_size))
    values  <- values[-inds]
    indexes <- c(indexes, rep(count, length(inds)))
    count   <- count + 1
  }

  all_from  <- lapply(1:nrow(this.lp), function(x){ fromJSON(this.lp$fromCOMID[x]) })

  out[[i]] <-  group_by(this.lp, ind = indexes) %>%
    summarise(levelpathi  = levelpathi[1],
              hydroseq_min = min(hydroseq),
              streamorde  = max(streamorde),
              toCOMID     = toJSON(toCOMID),
              fromCOMID   = toJSON(unlist(all_from)),
              comids      = toJSON(comid)) %>%
    st_line_merge() %>%
    mutate(lengthkm = set_units(st_length(.), "km"))
}

new_fl <- do.call(rbind, out) %>%
  mutate(lengthkm = as.numeric(set_units(st_length(.), "km")),
         ID = 1:n(), levelpathi,
         ind = NULL)

catchments = lapply(1:nrow(new_fl), function(i){
  filter(cat, comid %in% fromJSON(new_fl$comids[i])) %>%
    summarize(ID = new_fl$ID[i]) %>%
    st_cast("POLYGON")
})

new_cat <- do.call(rbind, catchments) %>%
  mutate(areasqkm = as.numeric(set_units(st_area(.), "km2"))) %>%
  rmapshaper::ms_simplify(.9)

return(list(fl = new_fl, cat = new_cat))
}
