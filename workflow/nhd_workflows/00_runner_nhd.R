# 00. This script is reading in the output from Dave B. code,
source('workflow/nhd_workflows/01_runner.R')
source('workflow/nhd_workflows/02_frag_fix.R')
source('workflow/nhd_workflows/03_levelpath.R')



usgs = readRDS("data/usgs_rc.rds") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  select(COMID, siteID, name) %>%
  st_transform(5070)

cat = read_sf("workflow/cache/ngen_01a-2.gpkg", "catchments")

oo = st_filter(usgs, cat) %>%
  st_join(cat)
