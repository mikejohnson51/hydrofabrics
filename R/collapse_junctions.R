collapse_junctions = function(graph, og_catchments, threshold = 1, max_size = 15){

  candidates = filter(graph$nodes, count == 2)
  # look for its from and to node, are any within 600 meters?
  paths_to_collapse = filter(graph$edges, from %in% candidates$nodeID | to %in% candidates$nodeID) %>%
    filter(lengthkm < threshold)

  message("Trying to collapse: ", nrow(paths_to_collapse), " junction")
  for(i in 1:nrow(paths_to_collapse)){
    line = paths_to_collapse[i,]
    junction2 = filter(candidates, nodeID %in% c(line$to, line$from))
    junction3 = filter(graph$nodes, nodeID %in% c(line$to, line$from), count == 3)

    paths        = filter(graph$edges, from %in% c(line$to, line$from) | to %in% c(line$to, line$from))
    path_to_join = filter(paths, ID2 != line$ID2,  from == junction2$nodeID | to == junction2$nodeID)
    all_touching = filter(graph$edges,  from == junction2$nodeID | to == junction2$nodeID |
                            from == junction3$nodeID | to == junction3$nodeID)
    all_cat = filter(graph$cat, ID %in% all_touching$ID)

    graph$edges[which(graph$edges$ID2 == path_to_join$ID2),]$geom = st_union(line$geom, path_to_join$geom) %>% st_line_merge()
    graph$edges = graph$edges %>% filter(ID2 != line$ID2)

    og_nhd_catchment   = og_catchments[unlist(st_intersects(line, og_catchments)),] %>% st_buffer(.001)
    graph$cat[which(graph$cat$ID == path_to_join$ID),]$geom  = st_union(og_nhd_catchment$geom, filter(graph$cat, ID == path_to_join$ID)$geom)
    graph$cat[which(graph$cat$ID == line$ID),]$geom  =  st_difference(filter(graph$cat, ID == line$ID)$geom, og_nhd_catchment)

    tmp    <- filter(graph$cat, ID %in% all_cat$ID)

    graph$cat$geom[which(graph$cat$ID %in% all_cat$ID)] <- topo_fix(base_cat = tmp, anchor = junction3)$geom

    graph$edges = graph$edges %>%
      group_by(ID) %>%
      summarise(hydroseq_min = hydroseq_min[1], comids = comids[1])

    for (i in 1:nrow(graph$edges)) {
      if (st_geometry_type(graph$edges$geom[i]) == "MULTILINESTRING") {
        graph$edges$geom[i] <- st_line_merge(graph$edges$geom[i])
      }
    }
  }

  graph = nexus_flow_graph(graph$edges, graph$cat)

  ######

  candidates = filter(graph$nodes, count == 2) %>%
    mutate(area = NA, catID = NA)

  for(i in 1:nrow(candidates)){
    tmp  = filter(graph$edges, from %in% candidates$nodeID[i] | to %in% candidates$nodeID[i]) %>%
      left_join(st_drop_geometry(graph$cat), by = "ID") %>%
      summarize(a = sum(areasqkm), catID = toJSON(ID))

    candidates$area[i] = tmp$a
    candidates$catID[i] = tmp$catID
  }

  h = filter(candidates, area <= max_size)

  for(i in 1:nrow(h)){

    tmp = filter(graph$cat, ID %in% fromJSON(h$catID[i])) %>%
      left_join(st_drop_geometry(graph$edges), by = "ID")

    lower = slice_min(tmp, hydroseq_min, n= 1)$ID
    upper = slice_max(tmp, hydroseq_min, n= 1)$ID

    graph$cat[which(graph$cat$ID == lower),]$geom = st_union(tmp$geom)
    graph$cat  = filter(graph$cat, ID != upper)
    graph$edges[which(graph$edges$ID == lower),]$geom = st_union(filter(graph$edges, ID %in% fromJSON(h$catID[i]))$geom)
    graph$edges  = filter(graph$edge, ID != upper)


    graph$cat = graph$cat %>% mutate(areasqkm = as.numeric(set_units(st_area(.), "km2")))
    graph$edges = graph$edges %>% mutate(lengthkm = as.numeric(set_units(st_length(.), "km")))
  }

  for (i in 1:nrow(graph$edges)) {
    if (st_geometry_type(graph$edges$geom[i]) == "MULTILINESTRING") {
      graph$edges$geom[i] <- st_line_merge(graph$edges$geom[i])
    }
  }

  nexus_flow_graph(graph$edges, graph$cat)

}
