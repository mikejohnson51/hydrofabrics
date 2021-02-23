topo_fix <- function(base_cat, anchor) {

  pt = tryCatch({
    st_intersection(base_cat) %>% st_collection_extract("POINT")},
     error = function(e){ NULL },
     warning = function(w) { NULL}
  )


  cond = if(!is.null(pt)){
    # 2 30m NED cells
    as.numeric(st_distance(anchor, pt )) > 60
  } else {
    FALSE
  }

  if(is.null(pt) | cond){
    for (i in 1:nrow(base_cat)) {
      t2 <- st_cast(st_geometry(base_cat[i, ]), "POINT")
      index = which.min(st_distance(t2, anchor))
      t2[index] <- anchor$geom
      base_cat$geom[i] <- st_cast(st_combine(t2), "POLYGON")
    }
  } else {
    for (i in 1:nrow(base_cat)) {
      t2 <- st_cast(st_geometry(base_cat[i, ]), "POINT")
      t2[which(t2 == pt$geom)] <- anchor$geom
      base_cat$geom[i] <- st_cast(st_combine(t2), "POLYGON")
    }
  }

  base_cat
}



topology_doctor = function(fl, cat){

  nexi <- st_intersection(fl) %>%
    filter(n.overlaps > 1) %>%
    dplyr::select() %>%
    distinct()

  for (x in 1:nrow(nexi)) {
    inters <- st_intersects(nexi[x, ], fl)[[1]]
    tmp <- filter(cat, ID %in% fl$ID[inters])
    cat$geom[inters] <- topo_fix(base_cat = tmp, anchor = nexi[x, ])$geom
  }

  o <- st_intersection(cat, fl) %>%
    filter(ID != ID.1, as.numeric(st_length(.)) > 1)

  for (x in 1:nrow(o)) {

    to_cut   <- cat[cat$ID == o$ID[x], ]
    to_merge <- cat[cat$ID == o$ID.1[x], ]

    spliter  <- st_split(to_cut, st_union(fl)) %>%
      st_collection_extract("POLYGON") %>%
      mutate(area = as.numeric(st_area(.))) %>%
      arrange(area)

    oddball <- bind_rows(to_merge, spliter[1, ]) %>%
      st_snap_to_grid(size = .001) %>%
      summarize()

    the_rest <- spliter[2:nrow(spliter), ] %>%
      st_snap_to_grid(size = .001) %>%
      summarize()

    pts_fl        <- st_cast(fl$geom[fl$ID == o$ID[x]], "POINT")
    pts_fl1       <- pts_fl[1]
    pts_fl2       <- pts_fl[length(pts_fl)]

    pts_cut       <- st_cast(o$geom[x], "POINT")

    magic_pt      <- if(pts_fl1 %in% pts_cut) { pts_fl1} else { pts_fl2 }

    pt_to_move    <- pts_cut[!pts_cut %in% magic_pt, ]

    oddball_pts   <- st_cast(oddball, "POINT")
    id            <- which.min(abs(st_distance(oddball_pts$geom, pt_to_move )))
    oddball_pts$geom[id] <- magic_pt
    oddball_poly  <- st_combine(oddball_pts) %>% st_cast("POLYGON")

    the_rest_pts  <- st_cast(the_rest, "POINT")
    id            <- which.min(abs(st_distance(the_rest_pts$geom, pt_to_move)))
    the_rest_pts$geom[id] <- magic_pt
    the_rest_poly <- st_combine(the_rest_pts) %>% st_cast("POLYGON")

    cat$geom[cat$ID == to_cut$ID]   <- the_rest_poly
    cat$geom[cat$ID == to_merge$ID] <- oddball_poly
  }

  return(list(fl = fl, cat = cat))
}
