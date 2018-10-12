# Author:
#       Brandon Dey
#
# Date: 
#       9.9.18
#
# Purpose: 
#       This script is the tag-a-long .R for ODSC article 3 on IDW geospatial interpolation.
# 

#################
## ENVIRONMENT ##
#################
# load libraries
library(tidyverse)
library(rgdal)
library(leaflet)
library(geosphere)
library(directlabels)
library(RColorBrewer)

# get data
#breweries
#read.csv("./Data/Raw/breweries-brew-pubs-in-the-usa/7160_1.csv") -> breweries
read.csv("./Data/Raw/breweries-brew-pubs-in-the-usa/8260_1.csv") -> breweries_new


# remove missing values
paste0("Missing values in ",nrow(breweries_new) - na.omit(breweries_new) %>% nrow, " observations of ", nrow(breweries_new))
breweries_new <- na.omit(breweries_new)

# Find epicenter of brewery activity in each state
geographic_average <- function(lon, lat, weight = NULL) {
  if (is.null(weight)) {
    weight <- rep(1, length(lon))
  }
  lon <- weighted.mean(lon, w = weight)
  lat <- weighted.mean(lat, w = weight)
  data.frame(lon = lon, lat = lat)
}

# limit to breweries in the continguous U.S. 
breweries_new %>% 
  filter(between(longitude, -124.446359, -70.6539763) & 
           between(latitude, 25.8192058, 47.3873012) &
           nchar(as.character(province)) == 2) -> breweries_new 

breweries_us <- breweries_new

epicenters <- data.frame(state = unique(breweries_us$province), lon = NA, lat = NA, breweries = NA)
epicenters <- filter(epicenters, str_count(state) == 2)

for(s in 1:nrow(epicenters)) {
  
  state <- epicenters[s,1]
  s_df <- filter(breweries_us, province == state)
  s_epi <- geographic_average(lon = s_df$longitude, lat = s_df$latitude)
  s_brs <- nrow(s_df)
  epicenters[s, 2] <- s_epi[,1]
  epicenters[s, 3] <- s_epi[,2]
  epicenters[s, 4] <- s_brs
  } 


# Find U.S. Brewery Epicenter
geographic_average(lon = breweries_us$longitude, 
                   breweries_us$latitude) -> nat_epicenter

# plot epicenters 
ggplot(epicenters, 
       aes(x = lon, y = lat)) + 
  xlim(-125, -65) + 
  ylim(24, 51) + 
  borders('state', 
          alpha = 1, 
          size = 0.5, 
          fill = "#fec44f") + 
  # plot breweries
  geom_point(data = breweries_us, 
             aes(x = longitude, 
                 y = latitude), 
             alpha = .25,
             col = "#fff7bc", 
             size = 1) +
  # plot state epicenters
  geom_point(col = "#d95f0e", 
             aes(size = breweries)) +
  # plot state labels 
  geom_text(aes(x = lon, 
                y = lat, 
                label = state), 
            nudge_y = .25) +
  labs(title = "The State(s) of Breweries", 
       size = "Geographic \"Center\" \nof State's Beer Scene\n",
       caption = "Figure 1: \nEvery brewery and/or brew pub in the contiguous U.S. \nThe size of each dark orange dot is proportional to the count of breweries and/or brew pubs in that state. \nData Source: https://www.kaggle.com/datafiniti/breweries-brew-pubs-in-the-usa/version/2 ") +
  theme_void(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 1)) -> gg_us

ggsave(gg_us, 
       filename = "./Plots/US.jpeg", 
       dpi = 1200, 
       width = 11, 
       height = 6)

# summarize cities
breweries_us %>% 
  group_by(city, province) %>% 
  summarize(breweries = n_distinct(name), 
            lat = mean(latitude), # mean bc some cities include adjacent areas. 
            lon = mean(longitude)) %>% 
  ungroup %>%
  mutate(state = province) %>% 
  select(-province) -> city_sum

# join epicenters to compare geographic average to just picking the city with most breweries
city_sum %>% left_join(epicenters, 
                       by = "state") %>% 
  mutate(lat = lat.x, 
         lon = lon.x, 
         lat_geoavg = lat.y, 
         lon_geoavg = lon.y, 
         breweries_state = breweries.y, 
         breweries_city = breweries.x) %>% 
  select(-contains(".")) -> city_sum

city_sum %>%
  filter(nchar(as.character(state)) == 2) -> city_sum

# Plot WI
ggplot(data = filter(city_sum, state == "WI"), 
       aes(x = lon, 
           y = lat)) +
  borders("state", "WI", 
          fill = "#203731", 
          col = "#FFB612") +
  # plot cities with breweries
  geom_point(aes(x = lon, 
                 y = lat, 
                 size = breweries_city), 
             alpha = .75,
             fill = "#FFB612",
             col = "#FFB612") +
  # plot epicenter
  geom_point(aes(x = lon_geoavg, 
                 y = lat_geoavg), 
             col = "#d95f0e") +
  # Epicenter label 
  geom_dl(aes(label = "Geographic \"Center\" \n of Beer Scene", x = lon_geoavg, y = lat_geoavg), 
          method = list(dl.trans(x = x - 1.2), "last.points", cex = 0.8)) +
  
  # MKE label 
  geom_dl(data = filter(city_sum, city == "Milwaukee" & state == "WI"),
          aes(label = "Better Epicenter of \n Beer Scene", x = lon, y = lat), method = list(dl.trans(x = x + 0.5), "last.points", cex = 0.8)) +
  
  labs(caption = "Figure 2: \nEvery location in Wisconsin with a brewery and/or brew pub. \nThe size of each dot is proportional to the count of breweries and/or brew pubs in that city. \nData Source: https://www.kaggle.com/datafiniti/breweries-brew-pubs-in-the-usa/version/2 ") +
  scale_size_area() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  coord_quickmap() + 
  guides(size = F, 
         alpha = F, 
         col = F) -> gg_wi

ggsave(gg_wi, 
       filename = "./Plots/WI.jpeg", 
       dpi = 1200, 
       width = 7, 
       height = 7)

# Plot OR
ggplot(data = filter(city_sum, state == "OR"), 
       aes(x = lon, 
           y = lat)) +
  borders("state", "OR", 
          fill = "#002A86", 
          col = "#FFEA0F") +
  # plot cities with breweries
  geom_point(aes(x = lon, 
                 y = lat, 
                 size = breweries_city, 
             alpha = 1), 
             col = "#FFEA0F") +

  # Portland label 
  geom_dl(data = filter(city_sum, city == "Portland" & state == "OR"),
          aes(label = "Portland", x = lon, y = lat), method = list(dl.trans(x = x , y = y + .5), "last.points", cex = 0.8)) +
  
  labs(caption = "Figure 3: \nEvery location in Oregon with a brewery and/or brew pub. \nThe size of each dot is proportional to the count of breweries and/or brew pubs in that city. \nData Source: https://www.kaggle.com/datafiniti/breweries-brew-pubs-in-the-usa/version/2 ") +
  scale_size_area() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  coord_quickmap() + 
  guides(size = F, 
         alpha = F, 
         col = F) -> gg_or

ggsave(gg_or, 
       filename = "./Plots/OR.jpeg", 
       dpi = 1200, 
       width = 7, 
       height = 7)



###############################################################################
## DISTANCE MATRIX ############################################################
###############################################################################
# Create a distance matrix of distances between every brewery and the nearest brewery in WI and OR.
# WI 
breweries_wi <- breweries_us[breweries_us$province == "WI",]

mat_wi <- distm(breweries_wi[,c("longitude","latitude")], 
                breweries_wi[,c("longitude","latitude")],
             fun = distVincentyEllipsoid) # The shortest distance between two points (i.e., the 'great-circle-distance' or 'as the crow flies'), according to the 'Vincenty (ellipsoid)' method. This method uses an ellipsoid and the results are very accurate. The method is computationally more intensive than the other great-circled methods in this package. # Earth isn't a perfect sphere. It's not. It's eppiloidal.

# convert meters to miles
mat_wi <- mat_wi/1609.344

# don't want to include itself so replace with a big number that'll never be the smallest.
mat_wi[mat_wi == 0] <- 1000000 

breweries_wi %>% 
  mutate(closest_pub = breweries_wi$name[max.col(-mat_wi)],
         closest_pub_city = breweries_wi$city[max.col(-mat_wi)],
         closest_pub_address = breweries_wi$address[max.col(-mat_wi)],
         closest_lat = breweries_wi$latitude[max.col(-mat_wi)],
         closest_lon = breweries_wi$longitude[max.col(-mat_wi)]) -> breweries_wi 

breweries_wi$miles_to_closest <- distVincentyEllipsoid(p1 = breweries_wi[,c("longitude", "latitude")],
                      p2 = breweries_wi[,c("closest_lon", "closest_lat")]) / 1609.344


# explore the closest pubs in WI...
ggplot(data = breweries_wi) +
  borders("state", "WI", 
          fill = "#203731", 
          col = "#FFB612") +
  # plot pubs
  geom_point(aes(x = longitude, 
                 y = latitude), 
             fill = "#FFB612",
             col = "#FFB612") +
  # plot nearest pub
  geom_point(aes(x = closest_lon, 
                 y = closest_lat, 
                 size = miles_to_closest), 
             col = "#d95f0e", 
             alpha = .5) +
  labs(caption = "Figure 4: \nEvery location in Wisconsin with a brewery and/or brew pub. \nThe size of each dot is proportional to the count of breweries and/or brew pubs in that city. \nData Source: https://www.kaggle.com/datafiniti/breweries-brew-pubs-in-the-usa/version/2 ") +
  scale_size_area() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  coord_quickmap() + 
  guides(size = F, 
         alpha = F, 
         col = F) 


# Now do the same for OR
breweries_or <- breweries_us[breweries_us$province == "OR",]

# OR distance matrix 
mat_or <- distm(breweries_or[,c("longitude","latitude")], 
                breweries_or[,c("longitude","latitude")],
                fun = distVincentyEllipsoid) 

mat_or <- mat_or/1609.344 # convert meters to miles
mat_or[mat_or == 0] <- 1000000 # don't want to include itself so replace with a big number that'll never be the smallest.

breweries_or %>% 
  mutate(closest_pub = breweries_or$name[max.col(-mat_or)],
         closest_pub_city = breweries_or$city[max.col(-mat_or)],
         closest_pub_address = breweries_or$address[max.col(-mat_or)],
         closest_lat = breweries_or$latitude[max.col(-mat_or)],
         closest_lon = breweries_or$longitude[max.col(-mat_or)]) -> breweries_or

breweries_or$miles_to_closest <- distVincentyEllipsoid(p1 = breweries_or[,c("longitude", "latitude")],
                                                       p2 = breweries_or[,c("closest_lon", "closest_lat")]) / 1609.344

ggplot(data = breweries_or) +
  borders("state", "OR", 
          fill = "#002A86", 
          col = "#FFEA0F") +
  
  # plot pubs
  geom_point(aes(x = longitude, 
                 y = latitude), 
             fill = "#FFB612",
             col = "#FFB612") +
  
  # plot closest pub
  geom_point(aes(x = closest_lon, 
                 y = closest_lat, 
                 size = miles_to_closest), 
             col = "#d95f0e", 
             alpha = .5) +
  # Portland label 
  geom_dl(data = filter(city_sum, city == "Portland" & state == "OR"),
          aes(label = "Portland", x = lon, y = lat), method = list(dl.trans(x = x , y = y + .5), "last.points", cex = 0.8)) +
  
  labs(caption = "Figure 3: \nEvery location in Oregon with a brewery and/or brew pub. \nThe size of each dot is proportional to the count of breweries and/or brew pubs in that city. \nData Source: https://www.kaggle.com/datafiniti/breweries-brew-pubs-in-the-usa/version/2 ") +
  scale_size_area() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  coord_quickmap() + 
  guides(size = F, 
         alpha = F, 
         col = F) 

summary(breweries_or$miles_to_closest)
summary(breweries_wi$miles_to_closest)

###############################################################################
## MODEL ######################################################################
###############################################################################

# U.S Shapefile
us <- rgdal::readOGR("./Data/Raw/US shapefile", 
                     "tl_2017_us_state") 

# isolate to contiguous U.S.
no_thanks <- c('Alaska', 'American Samoa', 'Puerto Rico', 'Guam', 
               'Commonwealth of the Northern Mariana Islands United States Virgin Islands', 
               'Commonwealth of the Northern Mariana Islands', 
               'United States Virgin Islands', 'Hawaii')

us_cont <- subset(us, !(us@data$NAME %in% no_thanks))
wi <- subset(us, (us@data$NAME %in% "Wisconsin"))

# place a grid around shapefile
grid_us <- makegrid(us_cont, 
                    n = 20000) %>% 
  SpatialPoints(proj4string = CRS(proj4string(us))) %>% 
  .[us_cont, ] # subset to contiguous U.S.

makegrid(wi, n = 2000000) %>% SpatialPoints(proj4string = CRS(proj4string(us)))  %>% 
  .[wi, ] -> grid_wi

plot(grid_wi)

# convert the data to a spacial dataframe.
sp::coordinates(breweries_wi) = ~longitude + latitude

# make sure that the projection matches the grid we've built.
proj4string(breweries_wi) <- CRS(proj4string(wi)) 

# fit basic inverse distance model
idw_model <- gstat::idw(
  formula = miles_to_closest ~ 1, 
  locations = breweries_wi, 
  newdata = grid_wi,
  idp = 3) 

# extract interpolated predictions 
interpolated_results = as.data.frame(idw_model) %>% {# output is defined as a data table
  names(.)[1:3] <- c("longitude", "latitude", "miles_to_closest")  # give names to the modeled variables
  . } %>% 
  select(longitude, latitude, miles_to_closest)

interpolated_results %>% 
  head() %>% 
  knitr::kable()


# plot map with distances a la IDW

# ['#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#c7eae5','#80cdc1','#35978f','#01665e','#003c30']
guide_tinker =  guide_legend(
  title.position = "top",
  label.position="bottom",
  label.hjust = 0.5,
  direction = "horizontal",
  keywidth = 1,
  nrow = 1 )

colourCount = interpolated_results$miles_to_closest %>% unique() %>% length()
palette = colorRampPalette(brewer.pal(9, "YlGnBu"))(colourCount)


ggplot(interpolated_results,
       aes(x = longitude, 
           y = latitude)) + 
  
  geom_raster( aes(fill = miles_to_closest)) +
    scale_fill_manual(values = palette,
                      guide = guide_tinker)  +
  scale_fill_distiller(palette = 'YlGn', 
                       direction = -1) +
  theme_void() +
    theme(
      text = element_text(family = 'Montserrat'),
      legend.justification = c(0,0),
      legend.position = c(0,0.02),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.box.background = element_rect(fill = '#f0f0f0', color = NA)
    ) +
  
  labs(fill = "working title") +
  borders('state', "WI",
          alpha = 0.1, 
          size = 0.1)


# interpolated_swings %>% 
#   mutate(swing_in_5s = factor(round(swing_in_5s)))%>% 
#   ggplot(aes(x = lon, y = lat)) + 
#   geom_raster( aes(fill = swing_in_5s)) +
#   scale_fill_manual(values = palette, 
#                     guide = guide_tinker)  +
#   xlim(-130,-67) + ylim(24,50) +
#   theme_void() +
#   theme(
#     text = element_text(family = 'Montserrat'),
#     legend.justification = c(0,0),
#     legend.position = c(0,0.02),
#     legend.title = element_text(size = 10),
#     legend.text = element_text(size = 8),
#     legend.box.background = element_rect(fill = '#f0f0f0', color = NA)
#   ) + 
#   labs(
#     title = "The United States of Seasons",
#     subtitle = "Difference between the hottest and coldest days of the year",
#     fill = "Temp swing in degrees") +
#   borders('state', alpha = 0.1, size = 0.1)


# Save data
save(breweries_wi,
        breweries_or,
        breweries_new,
        city_sum,
        epicenters,
        file = "./Data/Clean/clean_data.rta")


# Fini!

###############################################################################
## GRAVEYARD ##################################################################
###############################################################################


us.cities %>% 
  mutate(city = substr(name, start = 1, nchar(name)-3)) -> us.cities # create city var by removing state from name

city_sum %>% 
  left_join(us.cities, by = c("city", "province" = "country.etc")) -> city_sum

# renanme and reorder vars
city_sum %>% 
  mutate(state = province, 
         lat = lat.x,
         lon = lon.x
  ) %>% 
  select(city, state, pop, breweries, lat, lon, capital,-(contains("."))) -> city_sum




# find cities with suspicious lon values
filter(city_sum, lon > 0) %>% arrange(desc(lon))

# fix burlington, WI
city_sum[city_sum$lon > 0 & city_sum$city =="Burlington", 5] <- 42.6762677 
city_sum[city_sum$lon > 0 & city_sum$city =="Burlington", 6] <- -88.3422618

# fix sacramento, CA
city_sum[city_sum$lon > 0 & city_sum$city =="Sacramento", 5] <- -38.5725121
city_sum[city_sum$lon > 0 & city_sum$city =="Sacramento", 6] <- -121.4857704

plot(subset(us, (us@data$STUSPS %in% "WI")))

# Get coordinates of every city in every state
cities %>% separate(Geolocation, into = c("lat", "long"), sep = ",") -> cities 

cities %>%
  mutate(lat = as.numeric(gsub(x = cities$lat, pattern = "\\(", replacement = "")), 
         long = as.numeric(gsub(x = cities$long, pattern = "\\)", replacement = ""))) -> cities

