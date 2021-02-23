prep_flownetwork = function(flownetwork, cat){
  fl <- setNames(flownetwork, tolower(names(flownetwork))) %>%
    dplyr::select(comid, tonode, fromnode, streamorde, levelpathi, hydroseq) %>%
    left_join(st_drop_geometry(cat)) %>%
    left_join(st_drop_geometry(dplyr::select(., toCOMID = comid, fromnode)),
              by = c("tonode" = "fromnode"))

  left_join(fl, st_drop_geometry(dplyr::select(fl,  fromCOMID = comid, tonode)),
            by = c("fromnode" = "tonode")) %>%
    st_drop_geometry() %>%
    dplyr::select(comid, fromCOMID) %>%
    group_by(comid) %>%
    summarize(fromCOMID = toJSON(fromCOMID)) %>%
    right_join(fl) %>%
    st_as_sf() %>%
    mutate(lengthkm = as.numeric(set_units(st_length(.), "km")))
}
