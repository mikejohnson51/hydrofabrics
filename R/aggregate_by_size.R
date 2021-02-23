aggregate_by_size = function(fl, cat, min_size, max_size){

merge_cat <- left_join(cat, st_drop_geometry(fl), by = "ID")


while(min(merge_cat$areasqkm) < min_size) {

  this_cat   <- filter(merge_cat, areasqkm == min(merge_cat$areasqkm))
  toCOMIDs   <- fromJSON(this_cat$toCOMID)
  fromCOMIDs <- fromJSON(this_cat$fromCOMID)

  all <- lapply(1:nrow(merge_cat), function(x) { fromJSON(merge_cat$comids[x])})

  ds_cat <- merge_cat %>%
    filter(grepl(paste(toCOMIDs, collapse = "|"), all)) %>%
    filter(ID != this_cat$ID) %>%
    mutate(type = "DS")

  us_cat <- merge_cat %>%
    filter(grepl(paste(fromCOMIDs, collapse = "|"), all)) %>%
    filter(ID != this_cat$ID) %>%
    mutate(type = "US")

  topo <- do.call(rbind, list(ds_cat, us_cat))

  if (this_cat$streamorde != 1) {
    # If stream order is not 1
    # then only allow merging with same order stream
    topo <- filter(topo, streamorde == this_cat$streamorde)

    if (nrow(topo) > 1) {
      to_merge <- filter(topo, type == "DS")
    } else {
      to_merge <- topo
    }
  } else {
    to_merge <- filter(topo, type == "DS")
  }

  new_area <- (to_merge$areasqkm + this_cat$areasqkm)

  if (new_area < max_size) {
    keep_id <- which(merge_cat$ID == to_merge$ID)
    lp = c(this_cat$levelpathi,to_merge$levelpathi)[which.max(c(this_cat$streamorde,
                                                                to_merge$streamorde))]

    merge_cat$comids[keep_id]      <- toJSON(c(fromJSON(this_cat$comids),
                                               fromJSON(to_merge$comids)))
    merge_cat$toCOMID[keep_id]     <- toJSON(c(fromJSON(this_cat$toCOMID),
                                               fromJSON(to_merge$toCOMID)))
    merge_cat$fromCOMID[keep_id]   <- toJSON(c(fromJSON(this_cat$fromCOMID),
                                               fromJSON(to_merge$fromCOMID)))
    merge_cat$geom[keep_id]        <- st_union(this_cat$geom, to_merge$geom)
    merge_cat$levelpathi[keep_id]  <- lp
    merge_cat$hydroseq_min[keep_id] <- min(this_cat$hydroseq_min,
                                           to_merge$hydroseq_min)

    merge_cat$areasqkm[keep_id]  <- new_area

    merge_cat <- filter(merge_cat, ID != this_cat$ID)

    fl    <- mutate(fl, ID = ifelse(ID == this_cat$ID, to_merge$ID, ID))
  } else {
    this_id   <- which(merge_cat$ID == this_cat$ID)
    merge_cat$areasqkm[this_id] <- min_size + 1
  }
}

merge_cat$areasqkm <- as.numeric(set_units(st_area(merge_cat), "km2"))

return(list(fl = fl, cat = merge_cat))
}
