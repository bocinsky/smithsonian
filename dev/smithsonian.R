library(magrittr)

counties <- 
  tigris::counties() %>%
  dplyr::mutate(NAME=ifelse(NAMELSAD == "Baltimore city",
                            "Baltimore City",
                            NAME)) %>%
  dplyr::select(fp = STATEFP, 
                county = NAME) %>%
  dplyr::left_join(
    tigris::states(cb = TRUE) %>%
      dplyr::select(fp = STATEFP,
                    state = NAME) %>%
      sf::st_drop_geometry()
  ) %>%
  dplyr::arrange(state) %>%
  dplyr::select(state, county) %>%
  sf::st_transform("WGS84")

annapolis <- 
  sf::read_sf("states/Annapolis.shp/") %>%
  sf::st_transform("WGS84") %>%
  dplyr::transmute(state = "Maryland",
                   county = "Annapolis")

meve <-
  "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Protected_Areas_Fee_Manager/FeatureServer/0/query" %>%
  httr::modify_url(
    query = list(
      f = "json",
      where = "Unit_Nm='Mesa Verde National Park'",
      returnGeometry = "true"
    )
  ) %>%
  sf::read_sf() %>%
  sf::st_transform("WGS84") %>%
  dplyr::summarise() %>%
  dplyr::transmute(state = "Colorado",
                   county = "Mesa Verde National Park")

yell <- 
  "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Protected_Areas_Fee_Manager/FeatureServer/0/query" %>%
  httr::modify_url(
    query = list(
      f = "json",
      where = "Unit_Nm='Yellowstone National Park'",
      returnGeometry = "true"
    )
  ) %>%
  sf::read_sf() %>%
  sf::st_transform("WGS84") %>%
  dplyr::summarise() %>% 
  sf::st_intersection(counties[counties$state == "Wyoming","geometry"] %>%
                        sf::st_union() %>%
                        sf::st_make_valid()) %>%
  dplyr::transmute(state = "Wyoming",
                   county = "Yellowstone National Park") 


counties[counties$county == "Anne Arundel","geometry"] %<>%
  sf::st_difference(annapolis$geometry)

counties[counties$county == "Montezuma","geometry"] %<>%
  sf::st_difference(meve$geometry)

wy <- counties[counties$state == "Wyoming","geometry"]
counties[counties$state == "Wyoming","geometry"] %<>%
  sf::st_difference(yell$geometry)

counties[counties$state == "Wyoming" & counties$county == "Teton",]$geometry %<>%
  sf::st_collection_extract(type = "POLYGON") %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid() %>%
  magrittr::extract(.,sf::st_area(.) > units::set_units(1,m^2))

counties[counties$state == "Wyoming" & counties$county == "Park",]$geometry %<>%
  sf::st_collection_extract(type = "POLYGON") %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid() %>%
  magrittr::extract(.,sf::st_area(.) > units::set_units(100000,m^2))

yell$geometry <- 
  wy %>%
  sf::st_union() %>%
  sf::st_make_valid() %>%
  sf::st_difference(
    counties[counties$state == "Wyoming","geometry"] %>%
      sf::st_union() %>%
      sf::st_make_valid()
  )


counties %<>%
  dplyr::bind_rows(list(annapolis, meve, yell)) %>%
  sf::st_cast("MULTIPOLYGON")

county_codes <- 
  list.files("states", 
             full.names = TRUE,
             pattern = "counties") %>%
  magrittr::set_names(.,stringr::str_remove(basename(tools::file_path_sans_ext(.)),"_counties")) %>%
  purrr::map_dfr(readr::read_csv, .id = "State", na = "") %>%
  dplyr::rename(state = State,
                county = County,
                county_code = Abbreviation) %>%
  dplyr::mutate(county_code = stringr::str_to_upper(county_code))

state_codes <-
  readr::read_csv("smithsonian_states.csv")

# Alaska (quads)
tempfile(fileext = ".zip") %T>%
  download.file("https://mrdata.usgs.gov/ak/quadrangles/ak250.zip",
                destfile = .) %>%
  unzip(exdir = tempdir())
ak_quads <- 
  sf::read_sf(tempdir(),
              layer = "ak250") %>%
  dplyr::mutate(state = "Alaska",
                QUADNO = as.character(QUADNO)) %>%
  dplyr::select(state,
                quad = QUADNO) %>%
  sf::st_transform("WGS84") %>%
  sf::st_intersection(tigris::states() %>%
                        dplyr::filter(STUSPS == "AK") %>%
                        sf::st_geometry() %>%
                        sf::st_transform("WGS84"))

# Arizona (quads)
tempfile(fileext = ".zip") %T>%
  download.file("https://prd-tnm.s3.amazonaws.com/StagedProducts/MapIndices/GDB/MAPINDICES_Arizona_State_GDB.zip",
                destfile = .) %>%
  unzip(exdir = tempdir())

az_quads <- 
  sf::read_sf(file.path(tempdir(),"MAPINDICES_Arizona_State_GDB.gdb"), 
              layer = "CellGrid_7_5Minute") %>%
  dplyr::filter(!(PRIMARY_STATE == "Utah"),
                !(CELL_NAME %in% c("Terry Benches","Tinajas Altas OE S", "Whale Mountain"))) %>%
  dplyr::transmute(Name = stringr::str_to_upper(CELL_NAME)) %>%
  dplyr::full_join(readr::read_csv("states/Arizona_quads.csv") %>%
                     dplyr::transmute(Name = `Quad Name`,
                                      Quad = `Quad No`)) %>%
  dplyr::arrange(Name) %>%
  dplyr::mutate(quad = stringr::str_remove(Quad, ":[^:]+$")) %>%
  dplyr::group_by(quad) %>%
  dplyr::summarise() %>%
  dplyr::mutate(state = "Arizona") %>%
  dplyr::select(state, quad, geometry = SHAPE) %>%
  sf::st_transform("WGS84") %>%
  sf::st_intersection(tigris::states() %>%
                        dplyr::filter(STUSPS == "AZ") %>%
                        sf::st_geometry() %>%
                        sf::st_transform("WGS84"))

# # Maine (quads)
# tempfile(fileext = ".zip") %T>%
#   download.file("https://prd-tnm.s3.amazonaws.com/StagedProducts/MapIndices/GDB/MAPINDICES_Maine_State_GDB.zip",
#                 destfile = .) %>%
#   unzip(exdir = tempdir())
# 
# me_quads <- 
#   sf::read_sf(file.path(tempdir(),"MAPINDICES_Maine_State_GDB.gdb"), 
#               layer = "CellGrid_15Minute") %>%
#   dplyr::filter(!(PRIMARY_STATE == "Utah"),
#                 !(CELL_NAME %in% c("Terry Benches","Tinajas Altas OE S", "Whale Mountain"))) %>%
#   dplyr::transmute(Name = stringr::str_to_upper(CELL_NAME)) %>%
#   dplyr::full_join(readr::read_csv("states/Arizona_quads.csv") %>%
#                      dplyr::transmute(Name = `Quad Name`,
#                                       Quad = `Quad No`)) %>%
#   dplyr::arrange(Name) %>%
#   dplyr::mutate(quad = stringr::str_remove(Quad, ":[^:]+$")) %>%
#   dplyr::group_by(quad) %>%
#   dplyr::summarise() %>%
#   dplyr::mutate(state = "Arizona") %>%
#   dplyr::select(state, quad, geometry = SHAPE) %>%
#   sf::st_transform("WGS84") %>%
#   sf::st_intersection(tigris::states() %>%
#                         dplyr::filter(STUSPS == "AZ") %>%
#                         sf::st_geometry() %>%
#                         sf::st_transform("WGS84"))

# Hawaii (islands and quads)
tempfile(fileext = ".zip") %T>%
  download.file("https://files.hawaii.gov/dbedt/op/gis/data/usgs_quads_n83.shp.zip",
                destfile = .) %>%
  unzip(exdir = tempdir())

hi_quads <-
  sf::read_sf(tempdir(),
              layer = "usgs_quads_n83") %>%
  sf::st_transform("WGS84") %>% 
  sf::st_intersection(tigris::states() %>%
                        dplyr::filter(STUSPS == "HI") %>%
                        sf::st_geometry() %>%
                        sf::st_transform("WGS84")) %>%
  dplyr::select(island_no, 
                quad = topo_no) %>%
  tidyr::separate(island_no, into = c("state_code",
                                      "island_code"),
                  extra = "drop",
                  convert = FALSE) %>%
  dplyr::mutate(quad = stringr::str_remove(quad, "-"),
                state = "Hawaii") %>%
  dplyr::select(state, island_code, quad)


# New York (Minor Civil Divisions)
ny_mcds <- 
  tigris::county_subdivisions(state = "NY") %>%
  dplyr::transmute(state = "New York",
                minor_civil_division = COUSUBFP) %>%
  sf::st_transform("WGS84")

counties %>%
  dplyr::left_join(county_codes) %>%
  dplyr::filter(!is.na(county_code)) %>%
  dplyr::bind_rows(ak_quads) %>%
  dplyr::bind_rows(az_quads) %>%
  dplyr::bind_rows(hi_quads) %>%
  dplyr::bind_rows(ny_mcds) %>%
  dplyr::bind_rows(counties %>%
                     dplyr::filter(state %in% c("Connecticut","New Mexico","Rhode Island")) %>%
                     dplyr::group_by(state) %>%
                     dplyr::summarise()) %>%
  dplyr::left_join(state_codes) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(smithsonian_id = glue::glue(format)) %>%
  sf::write_sf("../smithsonian.geojson",
               delete_dsn = TRUE)

zip("../smithsonian.geojson.zip","../smithsonian.geojson")

# mapview::mapview(counties %>%
#                    dplyr::left_join(county_codes) %>%
#                    dplyr::filter(state == "Minnesota",
#                                  is.na(county_code)))

# readr::read_csv("states/Ohio_counties.csv") %>%
#   dplyr::mutate(County = stringr::str_to_title(County)) %>%
#   readr::write_csv("states/Ohio_counties.csv")

sf::read_sf("../smithsonian.geojson") %>%
  rmapshaper::ms_simplify() %>%
  sf::write_sf("../smithsonian_simple.geojson",
               delete_dsn = TRUE)

sf::read_sf("../smithsonian_simple.geojson") %>%
  mapview::mapview(label = "smithsonian_id",
                   layer.name = "Site Numbers")

