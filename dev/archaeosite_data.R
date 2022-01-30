library(magrittr)

# Download and prepare the US Census Tigris county data
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


# Maryland includes Annapolis city as its own entity using a custom boundary
annapolis <- 
  sf::read_sf("../states/Annapolis.shp/") %>%
  sf::st_transform("WGS84") %>%
  dplyr::transmute(state = "Maryland",
                   county = "Annapolis")
# Remove Annapolis from within the boundary of Anne Arundel county
counties[counties$county == "Anne Arundel","geometry"] %<>%
  sf::st_difference(annapolis$geometry)

# Colorado includes Mesa Verde National Park as its own entity
# Download the boundary of Mesa Verde from the USA Protected Areas Database (PAD-US)
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

# Remove Mesa Verde from within the boundary of Montezuma county
counties[counties$county == "Montezuma","geometry"] %<>%
  sf::st_difference(meve$geometry)


# Wyoming includes Yellowstone National Park as its own entity
# Download the boundary of Yellowstone from the USA Protected Areas Database (PAD-US)
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

# Remove Yellowstone from within the boundary of Wyoming counties
wy <- counties[counties$state == "Wyoming","geometry"]
counties[counties$state == "Wyoming","geometry"] %<>%
  sf::st_difference(yell$geometry)
# Correct topology exceptions for Teton county
counties[counties$state == "Wyoming" & counties$county == "Teton",]$geometry %<>%
  sf::st_collection_extract(type = "POLYGON") %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid() %>%
  magrittr::extract(.,sf::st_area(.) > units::set_units(1,m^2))
# Correct topology exceptions for Park county
counties[counties$state == "Wyoming" & counties$county == "Park",]$geometry %<>%
  sf::st_collection_extract(type = "POLYGON") %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid() %>%
  magrittr::extract(.,sf::st_area(.) > units::set_units(100000,m^2))
# Define the final Wyoming Yellowstone NP geometry by the remainder of Wyoming
yell$geometry <- 
  wy %>%
  sf::st_union() %>%
  sf::st_make_valid() %>%
  sf::st_difference(
    counties[counties$state == "Wyoming","geometry"] %>%
      sf::st_union() %>%
      sf::st_make_valid()
  )

# Michigan includes Isle Royale National Park as its own entity
# Download the boundary of Isle Royale from the USA Protected Areas Database (PAD-US)
isro <-
  "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Protected_Areas_Fee_Manager/FeatureServer/0/query" %>%
  httr::modify_url(
    query = list(
      f = "json",
      where = "Unit_Nm='Isle Royale National Park'",
      returnGeometry = "true"
    )
  ) %>%
  sf::read_sf() %>%
  sf::st_transform("WGS84") %>%
  dplyr::summarise() %>%
  dplyr::transmute(state = "Michigan",
                   county = "Isle Royale National Park") %>%
  sfheaders::sf_remove_holes()

# Remove Isle Royale from within the boundary of Michigan counties
counties[counties$state == "Michigan","geometry"] %<>%
  sf::st_difference(isro$geometry)

# Add Annapolis, Mesa Verde NP, and Yellowstone NP as "counties"
counties %<>%
  dplyr::bind_rows(list(annapolis, meve, yell, isro)) %>%
  sf::st_cast("MULTIPOLYGON")

# For states that include county codes, read in the codes
county_codes <- 
  list.files("../states", 
             full.names = TRUE,
             pattern = "counties") %>%
  magrittr::set_names(.,stringr::str_remove(basename(tools::file_path_sans_ext(.)),"_counties")) %>%
  purrr::map_dfr(readr::read_csv, .id = "State", na = "") %>%
  dplyr::rename(state = State,
                county = County,
                county_code = Abbreviation)# %>%
  # dplyr::mutate(county_code = stringr::str_to_upper(county_code))

# Read in the Smithsonian Trinomial state codes and site numbering systems
state_codes <-
  readr::read_csv("../archaeosite_states.csv")

# Alaska uses a custom quadrangle abbreviation system
# Download and prepare the AK quadrangles system using AK quad boundaries
tempfile(fileext = ".zip") %T>%
  download.file("https://mrdata.usgs.gov/ak/quadrangles/ak250.zip",
                destfile = .) %>%
  unzip(exdir = tempdir())

ak_quads <- 
  sf::read_sf(tempdir(),
              layer = "ak250") %>%
  dplyr::transmute(state = "Alaska",
                   quad = SHORTNAME) %>%
  sf::st_transform("WGS84") %>%
  sf::st_intersection(counties %>%
                        dplyr::filter(state == "Alaska") %>%
                        sf::st_geometry() %>%
                        sf::st_transform("WGS84") %>%
                        sf::st_union())

# Arizona uses a custom 7.5 minute quadrangle abbreviation system
# Download and prepare the AZ quadrangles system using USGS quad boundaries
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
  dplyr::full_join(readr::read_csv("../states/Arizona_quads.csv") %>%
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

# Maine uses a custom 7.5 minute quadrangle abbreviation system
# Download and prepare the ME quadrangles system using USGS quad boundaries
tempfile(fileext = ".zip") %T>%
  download.file("https://prd-tnm.s3.amazonaws.com/StagedProducts/MapIndices/GDB/MAPINDICES_Maine_State_GDB.zip",
                destfile = .) %>%
  unzip(exdir = tempdir())

me_quads <-
  sf::read_sf(file.path(tempdir(),"MAPINDICES_Maine_State_GDB.gdb"),
              layer = "CellGrid_7_5Minute")  %>%
  dplyr::left_join(readr::read_csv("../states/Maine_quads.csv")) %>%
  dplyr::transmute(state = "Maine", 
                   quad = as.character(quad)) %>%
  dplyr::select(state, quad, geometry = SHAPE) %>%
  dplyr::group_by(state, quad) %>%
  dplyr::summarise() %>%
  sf::st_transform("WGS84") %>%
  sf::st_intersection(counties %>%
                        dplyr::filter(state == "Maine") %>%
                        sf::st_geometry() %>%
                        sf::st_transform("WGS84") %>%
                        sf::st_union())

me_quads$geometry[[90]] <-
  me_quads$geometry[[90]][[2]]

me_quads %<>%
  sf::st_cast("MULTIPOLYGON")

# Hawaii uses a custom 7.5 minute quadrangle abbreviation system that incorporates
# unique island and quad identifiers.
# Download and prepare the HI quadrangles system using HI quad boundaries
tempfile(fileext = ".zip") %T>%
  download.file("https://files.hawaii.gov/dbedt/op/gis/data/usgs_quads_n83.shp.zip",
                destfile = .) %>%
  unzip(exdir = tempdir())

hi_quads <-
  sf::read_sf(tempdir(),
              layer = "usgs_quads_n83") %>%
  dplyr::transmute(state = "Hawaii",
                   island_code = stringr::str_remove_all(island_no,"^50|-"),
                   quad = stringr::str_remove_all(topo_no,"-")) %>%
  sf::st_transform("WGS84") %>%
  sf::st_intersection(counties %>%
                        dplyr::filter(state == "Hawaii") %>%
                        sf::st_geometry() %>%
                        sf::st_transform("WGS84") %>%
                        sf::st_union())


# New York uses Minor Civil Divisions, as defined in the "County Subdivisions" 
# dataset in the US Census Tigris database
ny_towns <- 
  tigris::county_subdivisions(state = "NY") %>%
  dplyr::transmute(state = "New York",
                   town = NAME,
                   town_code = COUSUBFP) %>%
  sf::st_transform("WGS84") %>%
  sf::st_intersection(tigris::counties(state = "New York") %>%
                        sf::st_geometry() %>%
                        sf::st_transform("WGS84") %>%
                        sf::st_union())

# Connecticut uses town codes, as defined in the "County Subdivisions" 
# dataset in the US Census Tigris database. Towns are arranged in alphabetical
# order, then numbered sequentially.
ct_towns <- 
  tigris::county_subdivisions(state = "CT") %>%
  dplyr::filter(NAME != "County subdivisions not defined") %>%
  dplyr::arrange(NAME) %>%
  dplyr::transmute(state = "Connecticut",
                   town = NAME,
                   town_code = as.character(dplyr::row_number())) %T>%
  {
    sf::st_drop_geometry(.) %>%
      dplyr::select(Town = town,
                    `Town Code` = town_code) %>%
      readr::write_csv("../states/Connecticut_towns.csv")
  } %>%
  sf::st_transform("WGS84") %>%
  sf::st_intersection(tigris::counties(state = "Connecticut") %>%
                        sf::st_geometry() %>%
                        sf::st_transform("WGS84") %>%
                        sf::st_union())

## Massachusetts town codes
## These are only relevant for historic sites
# tempfile(fileext = ".zip") %T>%
#   download.file("https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/gdbs/MHC_Inventory_GDB.zip",
#                 destfile = .) %>%
#   unzip(exdir = tempdir())
#   
# ma_towns <-
#   sf::read_sf(file.path(tempdir(),"MHC_Inventory.gdb"),
#               layer = "MHCTOWNS_POLY") %>%
#   dplyr::select(NAME = TOWN,
#                 CODE) %>%
#   sf::st_drop_geometry() %>%
#   dplyr::right_join(tigris::county_subdivisions(state = "MA") %>%
#                       dplyr::mutate(NAME = stringr::str_remove(NAME, " Town"),
#                                     NAME = ifelse(NAME == "Manchester-by-the-Sea",
#                                                   "Manchester",
#                                                   NAME))) %>%
#   dplyr::filter(NAME != "County subdivisions not defined") %>%
#   sf::st_as_sf() %>%
#   dplyr::transmute(state = "Massachusetts",
#                    town = NAME,
#                    town_code = CODE) %>%
#   sf::st_transform("WGS84")

archaeosite_ids <-
  counties %>%
  dplyr::left_join(county_codes) %>%
  dplyr::filter(!is.na(county_code)) %>%
  dplyr::bind_rows(ak_quads) %>%
  dplyr::bind_rows(az_quads) %>%
  dplyr::bind_rows(hi_quads) %>%
  dplyr::bind_rows(ny_towns) %>%
  dplyr::bind_rows(ct_towns) %>%
  dplyr::bind_rows(me_quads) %>%
  dplyr::bind_rows(counties %>%
                     dplyr::filter(state %in% c("Connecticut","New Mexico","Rhode Island")) %>%
                     dplyr::group_by(state) %>%
                     dplyr::summarise()) %>%
  dplyr::left_join(state_codes) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(site_id = glue::glue(format))

archaeosite_ids %>%
  geojsonsf::sf_geojson() %>%
  geojsonio::geo2topo(object_name = "archaeosite_ids",
                      quantization = 1e6) %>%
  jsonlite::fromJSON(simplifyVector = FALSE) %>%
  # rlist::list.clean(recursive = TRUE) %>%
  jsonlite::toJSON(auto_unbox = TRUE) %>%
  geojsonio:::write_topojson("../archaeosite_ids.topojson")

sf::read_sf("../archaeosite_ids.topojson") %>%
  sf::st_set_crs(4326) %>%
  mapview::mapview()

leaflet::leaflet() %>% 
  leaflet::addTopoJSON("../archaeosite_ids.topojson",
                       group = "main",
                       weight = 0.2, 
                       color = "white", 
                       opacity = 1,
                       fillOpacity = 0.7, 
                       smoothFactor = 0.5)


