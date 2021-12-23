library(magrittr)

counties <- 
  tigris::counties() %>%
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


counties %>%
  dplyr::left_join(county_codes) %>%
  dplyr::filter(!is.na(county_code)) %>%
  dplyr::bind_rows(ak_quads) %>%
  dplyr::bind_rows(az_quads) %>%
  dplyr::bind_rows(hi_quads) %>%
  dplyr::left_join(state_codes) %>%
  # dplyr::filter(state == "Alabama") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(smithsonian_id = glue::glue(format)) %T>%
  sf::write_sf("../smithsonian.geojson",
               delete_dsn = TRUE) %>%
  mapview::mapview(label = "smithsonian_id")

  # mapview::mapview(counties %>%
  #                    dplyr::left_join(county_codes) %>%
  #                    dplyr::filter(state == "Nebraska",
  #                                  is.na(county_code)))

# geo2topo ../smithsonian.geojson > ../smithsonian.topojson


# readr::read_csv("states/North Carolina_counties.csv") %>%
#   dplyr::mutate(County = stringr::str_to_title(County)) %>%
#   readr::write_csv("states/North Carolina_counties.csv")



