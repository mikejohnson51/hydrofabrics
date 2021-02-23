nexus_flow_graph = function(fl, cat){

  start = fl %>%
    arrange(hydroseq_min) %>%
    dplyr::select(hydroseq_min) %>%
    mutate(ID = 1:n())

  nexus  <- st_set_geometry(start, find_node(start, position = NULL)) %>%
    mutate(type = "to")
  inlets  <- st_set_geometry(start, find_node(start, position = 1)) %>%
    mutate(type = "from")

  nexi = do.call(rbind, list(inlets, nexus)) %>%
    mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2]) %>%
    mutate(xy = paste(.$X, .$Y)) %>%
    mutate(nodeID = paste0("nex-", group_indices(., factor(xy, levels = unique(xy)))))

  start = fl %>%
    arrange(hydroseq_min) %>%
    dplyr::select(hydroseq_min) %>%
    mutate(ID = 1:n()) %>%
    lwgeom::st_snap_to_grid(size = 1-5) %>%
    lwgeom::st_split(nexi) %>%
    st_collection_extract("LINESTRING") %>%
    mutate(ID2 = 1:n(), length = st_length(.))

  nexus  <- st_set_geometry(start, find_node(start, position = NULL)) %>%
    mutate(type = "to")
  inlets  <- st_set_geometry(start, find_node(start, position = 1)) %>%
    mutate(type = "from")


  nexi = do.call(rbind, list(inlets, nexus)) %>%
    mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2]) %>%
    mutate(xy = paste(.$X, .$Y)) %>%
    mutate(nodeID = paste0("nex-", group_indices(., factor(xy, levels = unique(xy))))) %>%
    dplyr::select(-xy)

  bounds = nexi %>%
    group_by(nodeID) %>%
    arrange(type) %>%
    filter(n() == 1)  %>%
    mutate(type = ifelse(type == "from", "inlet", "outlet"))

  nexi$nodeID[which(nexi$nodeID == filter(bounds, type == "outlet")$nodeID)] = "nex-0"

  map    <- st_intersects(nexi, start)
  grow   <-  list()

  for(i in 1:nrow(nexi)){

    contributing = start[map[[i]], ]

    grow[[i]] = filter(nexi, ID2 %in% contributing$ID2) %>%
      st_drop_geometry() %>%
      tidyr::pivot_wider(id_cols = ID2,
                         names_from = type,
                         values_from = nodeID)
  }


  nodes = do.call(rbind, grow) %>%
    filter(!duplicated(.)) %>%
    arrange(ID2) %>%
    mutate(from =
             ifelse(from %in% filter(bounds, type == "inlet")$nodeID,
                    paste0("cat-", bounds$ID[which(bounds$ID2 %in% ID2)]), from),
           to =
             ifelse(to %in% filter(bounds, type == "outlet")$nodeID, "cat-0", to))

  nodes_sf = filter(nexi, nodeID %in% c(nodes$to, nodes$from)) %>%
    dplyr::select(nodeID) %>%
    group_by(nodeID) %>%
    mutate(count = n()) %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    arrange(nodeID)

  edges = left_join(start, nodes, by = "ID2")

  #TODO: need to define the "to" of each node
  # nodes go to cathments
  # Need to align fp to catchments, use same ID

  return(list(nodes = nodes_sf, edges = edges, cat = cat))
}


