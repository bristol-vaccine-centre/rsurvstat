# "FedState_2012_V7.shp", "NUTS2_2010_V7.shp", "County_2016_V8.shp"

# 38 NUTS2 code format DE##
# nest_hm %>% filter(Level == 2 & DimensionId == "[DeutschlandNodes].[Kreise71MapWeb]")

# 411 counties with format ##### e.g. 08315
# nest_hm %>% filter(Level == 3 & DimensionId == "[DeutschlandNodes].[Kreise71MapWeb]") %>% glimpse()
#

# Initial mapping was from GADM
# dataZip = "~/Downloads/gadm41_DEU_shp.zip"
# id = "GADM-DEU"
# unzipDir = fs::path(tempdir(), id)
# fs::dir_create(unzipDir)
# paths = utils::unzip(dataZip, exdir = unzipDir, junkpaths = TRUE)
# mapFile = fs::dir_ls(fs::path_abs(unzipDir), recurse = TRUE, glob = "*.shp")
# level1 = sf::st_read(mapFile[2]) %>% sf::st_transform(crs = 4326)
# map = level1 %>%
#   as_tibble() %>%
#   transmute(
#     NAME_1,
#     NUTS_ID = sprintf("DE%s", (c(as.character(1:9), LETTERS))[row_number()]),
#     ComponentId = sprintf("[%s]", CC_1)
#   )
# readr::write_csv(map, here::here("data-raw/nuts1-mapping.csv"))

if (!exists("nest_hm")) {
  source(here::here("data-raw/hierarchy.R"))
}

map = readr::read_csv(here::here("data-raw/nuts1-mapping.csv"))

# GeoJson Files:
NUTS1 = arear::downloadGeojson(
  "https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_20M_2010_4326_LEVL_1.geojson"
)
NUTS2 = arear::downloadGeojson(
  "https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_20M_2010_4326_LEVL_2.geojson"
)

berlin = arear::downloadGeojson(
  "https://raw.githubusercontent.com/m-hoerz/berlin-shapes/refs/heads/master/berliner-bezirke.geojson"
)
l3kreis = arear::downloadGeojson(
  "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/georef-germany-kreis/exports/geojson/?lang=en&timezone=Europe%2FLondon"
)

## Federal States:

level1survstat = nest_hm %>%
  filter(Level == 1 & DimensionId == "[DeutschlandNodes].[Kreise71Web]")

FedStateKey71Map = NUTS1 %>%
  filter(CNTR_CODE == "DE") %>%
  inner_join(map, by = "NUTS_ID") %>%
  inner_join(level1survstat %>% select(Id, ComponentId), by = "ComponentId") %>%
  transmute(
    Id,
    ComponentId,
    HierarchyId = "[DeutschlandNodes].[Kreise71Web].[FedStateKey71]",
    Name = NUTS_NAME
  ) %>%
  glimpse()

# Shoudl be just unknown left
level1survstat %>%
  anti_join(
    FedStateKey71Map,
    by = "ComponentId"
  )

##  NUTS level 2

level2survstat = nest_hm %>%
  filter(
    Level == 2 & DimensionId == "[DeutschlandNodes].[Kreise71Web]"
  )

NutsKey71Map = NUTS2 %>%
  filter(CNTR_CODE == "DE") %>%
  mutate(ComponentId = sprintf("[%s]", NUTS_ID)) %>%
  inner_join(level2survstat %>% select(Id, ComponentId), by = "ComponentId") %>%
  transmute(
    Id,
    ComponentId,
    HierarchyId = "[DeutschlandNodes].[Kreise71Web].[NutsKey71]",
    Name = NUTS_NAME
  ) %>%
  glimpse()

level2survstat %>%
  anti_join(
    NutsKey71Map,
    by = "ComponentId"
  )

## County level (Kreis in German) = a few extras for Berlin

# Missing values mostly Berlin counties and Gottingen
level3survstat = nest_hm %>%
  filter(
    Level == 3 & DimensionId == "[DeutschlandNodes].[Kreise71Web]"
  )

CountyKey71Map = bind_rows(
  l3kreis %>%
    transmute(
      Name = unlist(krs_name_short),
      ComponentId = sprintf("[%s]", krs_code)
    ),
  berlin %>%
    transmute(
      Name = unlist(spatial_alias),
      ComponentId = sprintf("[110%s]", spatial_name)
    )
) %>%
  inner_join(level3survstat %>% select(Id, ComponentId), by = "ComponentId") %>%
  transmute(
    Id,
    ComponentId,
    HierarchyId = "[DeutschlandNodes].[Kreise71Web].[CountyKey71]",
    Name
  ) %>%
  glimpse()

level3survstat %>%
  anti_join(
    CountyKey71Map,
    by = "ComponentId"
  )


BerlinMap = l3kreis %>%
  filter(krs_code == "11000") %>%
  transmute(Name = unlist(krs_name_short)) %>%
  glimpse()


usethis::use_data(BerlinMap, overwrite = TRUE, compress = "xz")
usethis::use_data(FedStateKey71Map, overwrite = TRUE, compress = "xz")
usethis::use_data(NutsKey71Map, overwrite = TRUE, compress = "xz")
usethis::use_data(CountyKey71Map, overwrite = TRUE, compress = "xz")

ggplot(FedStateKey71Map) + geom_sf()
ggplot(NutsKey71Map) + geom_sf()
ggplot(CountyKey71Map) + geom_sf()
