library(magrittr)

save_leaflet <- function(plot, file, overwrite = FALSE){
  # save the file if it doesn't already exist or if overwrite == TRUE
  if( !file.exists(file) | overwrite ){
    withr::with_dir(new = dirname(file), 
                    code = htmlwidgets::saveWidget(plot, 
                                                   file = basename(file),
                                                   libdir = "lib",
                                                   selfcontained = FALSE))
  } else {
    print("File already exists and 'overwrite' == FALSE. Nothing saved to file.")
  }
}

as_sf_tibble <- function(x){
  x %>%
    sf::st_as_sf() %>%
    tibble::as_tibble() %>%
    sf::st_as_sf()
}

rotate = function(x, a){
  r = a * pi / 180 #degrees to radians
  x * matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

smithsonian <-
  sf::read_sf("../smithsonian.geojson") %>%
  sf::st_transform("EPSG:2163")

ak <-
  smithsonian[smithsonian$state=="Alaska",] %>%
  as("Spatial") %>%
  maptools::elide(rotate=-39) %>%
  maptools::elide(., scale=max(apply(sp::bbox(.), 1, diff)) / 2) %>%
  maptools::elide(shift=c(-2500000, -2400000)) %>%
  as_sf_tibble()
sf::st_crs(ak) <- "EPSG:2163"

hi <-
  smithsonian[smithsonian$state=="Hawaii",] %>%
  as("Spatial") %>%
  maptools::elide(rotate=-35) %>%
  maptools::elide(shift=c(5200000, -1300000)) %>%
  as_sf_tibble()
sf::st_crs(hi) <- "EPSG:2163"

smithsonian %<>%
  dplyr::filter(!(state %in% c("Alaska", "Hawaii"))) %>%
  dplyr::bind_rows(list(ak, hi))

smithsonian %<>%
  rmapshaper::ms_simplify(keep = 0.1)

conus_colors <-
  tigris::states() %>%
  # dplyr::filter(!(NAME %in% c("Alaska", "Hawaii"))) %>%
  {
    tibble::tibble(state = .$NAME, 
                   color = as(., "Spatial") %>%
                     MapColoring::getColoring())
  }

pal <- thematic::okabe_ito(4)

epsg2163 <- 
  leaflet::leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:2163",
    proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
    resolutions = 2^(16:7))

smithsonian %>%
  sf::st_transform(4326) %>%
  dplyr::left_join(conus_colors) %>%
  leaflet::leaflet(options = leaflet::leafletOptions(crs = epsg2163,
                                                     zoomControl = FALSE)) %>%
  # htmlwidgets::onRender("function(el, x) {
  #       L.control.zoom({ position: 'topright' }).addTo(this)
  #   }") %>%
  leaflet::addPolygons(group = "main",
                        weight = 0.2, 
                       color = "white", 
                       opacity = 1,
                       fillColor = ~pal[color],
                       fillOpacity = 0.7, 
                       smoothFactor = 0.5,
                       label = ~smithsonian_id,
                       labelOptions = leaflet::labelOptions(direction = "auto")) %>%
  leaflet.extras::setMapWidgetStyle(list(background= "white")) %>%
  leaflet.extras::addSearchFeatures(targetGroups = "main",
                                    options = leaflet.extras::searchFeaturesOptions(
                                      moveToLocation = FALSE,
                                      autoResize = FALSE,
                                      collapsed = FALSE,
                                      textPlaceholder = "Search Site IDs")) %T>%
  save_leaflet("../docs/index.html",
               overwrite = TRUE)
