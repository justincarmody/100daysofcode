# Weekend Data Course - Leaflet Course ----
# This project is just working through this guys tutorial from his website 
library(tidyverse)
library(magrittr)
library(rlang)
library(leaflet)
library(ggmap)
library(htmltools)
# library(mapview) -- fuuuucking gdal
library(htmlwidgets)
library(shiny)
library(shinydashboard)

# Intro ----
# leaflet() function creates a map widget object - just an empty pane with no tiles or layers on it
m <- leaflet()

# you can then add tiles - the most basic one being a map tile
# default one is from openmaps or something
m <- leaflet() %>%
    # note: addTiles() creates a new map object
    addTiles() %>%
    # can also pick where map centres by default
    setView(lng = -149.4937, lat = 64.2008, zoom = 4)

# Customising Base Maps ----

# Use the addProviderTiles() fcn to choose from a bunch of map tile templates
m <- leaflet() %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 4)

m <- leaflet() %>%
    addProviderTiles(providers$Stamen.Watercolor) %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 4)

# note: this bit won't run because of the stupid gdal install thing
# but example code here is still good

ak_counties <- readOGR("data/tl_2013_02_cousub/tl_2013_02_cousub.shp")
m <- leaflet() %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
    # can then add shape files and play with aesthetics of them
    addPolygons(data = ak_counties,
                color = "#660000",
                weight = 1,              # transparency
                smoothFactor = 0.5)      # <1: adding vertices, >1 removing vertices from polygon edges


# Data Preparation
# creating geometric representations of whatever data you have
# in this case, it's FBI crime data for alaska
# note again: this probably won't work because of the stupid gdal thing

fbi_data <- readRDS(file='data/database.rds')
fbi_tbl <- as.tibble(fbi_data)

# main annoyance here is that we don't have lat/lng for this stuff
# so we'll be using some geocode function - which is an API that has a limit for queries per day
#### note to self - find out if a MapInfo license can get around this

fbi_tbl %>%
    filter(State == 'Alaska') %>%
    mutate(address = paste(City, State, "United States")) ->
    ak

addresses <- unique(ak$address)

# then we use the ggmap package - a google maps API package
geocodes <- geocode(addresses, source = "google")

# but this API is weird so have to join datasets, check missing ones, then rerun to get them
addresses_and_coords <- tibble(address = addresses,
                               lng = geocodes$lon,
                               lat = geocodes$lat)

counter <- 0
while(sum(is.na(addresses_and_coords$lng)) > 0 && counter < 10){
    addresses_and_coords %>%
        filter(is.na(lng)) ->
        missing_addresses
    
    addresses <- missing_addresses$address
    geocodes <- geocode(addresses, source = "google")
    
    addresses_and_coords %>%
        filter(is.na(lng)==FALSE) ->
        addresses_and_coords
    
    new_addresses <- tibble(address = addresses,
                            lng = geocodes$lon,
                            lat = geocodes$lat)
    
    addresses_and_coords <- rbind(addresses_and_coords, new_addresses)
    
    counter <- counter + 1
}

# clean up environment - wouldn't be necessary if above thing was a function
rm(geocodes, addresses, missing_addresses, new_addresses, counter)

# then have to join coords to main tbl
ak <- left_join(ak, addresses_and_coords, by = 'address')

# to make things less stupid, we're adding some noise to the coords - because we only have one coordindate 
# for a whole region, so the multiple data points get stacked
ak$lng <- jitter(ak$lng, factor = 1)
ak$lat <- jitter(ak$lat, factor = 1)


# Circle Markers and Coords ----
ak %>%
    filter(Crime.Type == 'Murder or Manslaughter') %>%
    filter(Crime.Solved == 'No') ->
    ak_unsolved

m <- leaflet() %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
    # this polygons step I can't do because of the stupid gdal thing again
#     addPolygons(data = ak_counties,
#                 color = '#660000',
#                 weight = 0.5,
#                 smoothFactor = 1) %>%
    addCircleMarkers(lng = ak_unsolved$lng, 
                     lat = ak_unsolved$lat,
                     color = 'ffffff',
                     weight = 1,
                     radius = 5)

# Adding Labels to Markers ----

# labels need to have HTML content to be displayed
# you can create the HTML and store it as a var to paste into for later
ak_unsolved$label <- paste("<p>", ak_unsolved$City, "</p>",
                           "<p>", ak_unsolved$Month, " ", ak_unsolved$Year, "</p>",
                           "<p>", ak_unsolved$Victim.Age, "yo", ak_unsolved$Victim.Sex, "</p>",
                           "<p>", ak_unsolved$Victim.Race, "</p>",
                           "<p> Weapon:", ak_unsolved$Weapon, "</p>")

m <- leaflet() %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
    addCircleMarkers(lng = ak_unsolved$lng, 
                     lat = ak_unsolved$lat,
                     color = 'ffffff',
                     weight = 1,
                     radius = 5,
                     label = lapply(ak_unsolved$label, htmltools::HTML)) # needs a char vec of HTML content for the label


# note that there are bulk other popups - but they all have similar syntax to this

# Clustering Markers ----

m <- leaflet() %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
    addCircleMarkers(lng = ak_unsolved$lng, 
                     lat = ak_unsolved$lat,
                     color = 'ffffff',
                     weight = 1,
                     radius = 5,
                     label = lapply(ak_unsolved$label, htmltools::HTML),
                     clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE) 
                     )


# Interactive Controls ----
# aim is to have solved and unsolved crimes, and be able to toggle between the layers

ak %>%
    filter(Crime.Solved == "Yes") %>%
    filter(Crime.Type == "Murder or Manslaughter") ->
    ak_solved

ak_solved$label <- paste("<p>", ak_solved$City, "</p>",
                           "<p>", ak_solved$Month, " ", ak_solved$Year, "</p>",
                           "<p>", ak_solved$Victim.Age, "yo", ak_solved$Victim.Sex, "</p>",
                           "<p>", ak_solved$Victim.Race, "</p>",
                           "<p> Weapon:", ak_solved$Weapon, "</p>")

m <- leaflet() %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
    addCircleMarkers(lng = ak_unsolved$lng, 
                     lat = ak_unsolved$lat,
                     color = 'red',
                     weight = 1,
                     radius = 5,
                     label = lapply(ak_unsolved$label, htmltools::HTML),
                     # need to assign a group name to each set of markers
                     group = 'Unsolved') %>%
    addCircleMarkers(lng = ak_solved$lng,
                     lat = ak_solved$lat,
                     color = 'blue',
                     weight = 1,
                     radius = 5,
                     label = lapply(ak_solved$label, htmltools::HTML),
                     # same here, need a group to identify them
                     group = 'Solved') %>%
    # then you add a 'control layer' to make things interactive
    addLayersControl(overlayGroups = c('Unsolved', 'Solved'),
                     option = layersControlOptions(collapsed = FALSE))

# can you customise the tickbox layer legend..? to show the colours for reference


# Aligning Data with Shapefile ----
# this section is mostly about data transforms to link values to shapefiles, and to prepare the data
# for turning into choropleths 

# metric we'll be mapping to the colour aesthetic of the choropleth - solve rate over entire time horizon
# this little processing pipeline is actually fkn sweet
fbi_data %>%
    mutate(solved = ifelse(Crime.Solved == "Yes", 1, 0)) %>% # could also use dplyr::case_when() here
    filter(Crime.Type == "Murder or Manslaughter") %>%
    group_by(State) %>%
    summarise(Num.Murders = n(),
              Num.Solved = sum(solved)) %>%
    mutate(Num.Unsolved = Num.Murders - Num.Solved,
           Solve.Rate = Num.Solved / Num.Murders) ->
    us

# we then need to map these summary stats to our shapefiles
# whiiiiich won't work because of fucking gdal
states <- readOGR('data/cb_2016_us_state/cb_2016_us_state.shp')

is.element(us$State, states$NAME) # returns logical(): T if value from us$State is found within states$NAME

# so ^this tells us theres a spelling difference in Rhode Island - so have to correct it
levels(us$State)[40] <- "Rhode Island"  # this is shitty factor notation, which I hate


# then have to check existence the other way
is.element(states$NAME, us$State)
# which shows up a bunch of missing values
# so subset to remove them (lol)
states <- subset (states, is.element(states$NAME, us$State))
# have to then reorder the rows to match the order in the shape file
# i feel like a join makes more sense here....
us <- us[order(match(us$State, states$NAME)),]



# Creating a Choropleth ----
# choropleth - associates a data value to a colour to a geographic area

# need to bin values and create a colour mapping to them
bins <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
pal <- colorBin("RdYlBu",
                domain = us$Solve.Rate,
                bins = bins) # note: pal is now a fcn


m <- leaflet() %>%
    setView(-96, 37.8, zoom = 4) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addPolygons(data = states,
                weight = 1,
                smoothFactor = 0.5,
                color = 'white',
                fillOpacity = 0.8,
                # this step here uses the pal() fcn created above and runs it over the solve rate vector
                fillColor = pal(us$Solve.Rate),
                # this then adds some highlighting and popups to make things nicer
                highlight = highlightOptions(
                    weight = 5,
                    colour = '#666666',
                    dashArray = '',     # can change to dashed lines if you want
                    fillOpacity = 0.7,
                    bringToFront = TRUE    # brings highlighted values to the front
                )) 

labels <- paste("<p>", us$State, "</p>",
                "<p>", "Solve Rate: ", round(us$Solve.Rate, digits = 3), "</p>",
                sep = "")


m <- leaflet() %>%
    setView(-96, 37.8, zoom = 4) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addPolygons(data = states,
                weight = 1,
                smoothFactor = 0.5,
                color = 'white',
                fillOpacity = 0.8,
                # this step here uses the pal() fcn created above and runs it over the solve rate vector
                fillColor = pal(us$Solve.Rate),
                # this then adds some highlighting and popups to make things nicer
                highlight = highlightOptions(
                    weight = 5,
                    colour = '#666666',
                    dashArray = '',     # can change to dashed lines if you want
                    fillOpacity = 0.7,
                    bringToFront = TRUE    # brings highlighted values to the front
                ),
                label = lapply(labels, htmltools::HTML)) %>%
    addLegend(pal = pal,
              values = us$Solve.Rate,
              opacity = 0.7,
              position = "topright")



# Sharing Maps - Basic Exporting ----

# easy dumb-boi option - Viewer Pane > Export > save as image (this gives you static output)
# one level up - Viewer Pane > Export > save as web page (retains dynamic content, and you can still just send
# the little html files in an email)

# then you can automate exporting with the library(mapview) package - but fucking gdal fuck

# can also use the saveWidget() fucntion from the library(htmlwidgets) package
# and now this fucking doesn't work because I can't set pandoc up properly
saveWidget(m, file = 'dynamic_map.html')











