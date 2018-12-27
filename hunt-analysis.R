install.packages('tmap')
install.packages('sp')
install.packages('rgdal')
install.packages('tidyverse')
install.packages('geojsonio')
install.packages("spatstat")

library(tmap) 
library(geojsonio)
library(sp)
library(rgdal)
library(tidyverse)
library(tmaptools)
library(spatstat)
library(maptools)
library(sf)
library(plyr)


## Question 1: How far did you travel (according to the trace they recorded We all can calculate google travel distance using routing service so do not do this.
## The hunt path of the last year:
hunt <- geojson_read("https://www.dropbox.com/s/wa2ip35tcmt93g3/Team7.geojson?raw=1", what = "sp")
class(hunt)

# Set the hunt path to the BNG. 
BNG <- "+init=epsg:27700"
WGS84 <- "+init=epsg:4326"
hunt <- spTransform(hunt, BNG)

# View the path of the hunt from last year.
tmap_mode("view")
tm_shape(hunt) +
  tm_lines(col = "green", lwd = 4)

## Convert the Spatiallines Dataframe into an sf object. 
huntSF <- st_as_sf(hunt)
class(huntSF)
summary(huntSF)
## Extract the geometry column!
huntSF <- huntSF[,c("geometry")]
attr(huntSF, "sf_column")
print(huntSF, n=3)
hunt_SF_geom <- st_geometry(huntSF)
hunt_SF_geom[1]

## Check if the geometry is valid or not. 
st_is_valid(huntSF[1,])
## Calculate the length of the path!
st_length(huntSF)


## Question 2: How many TfL station did your route pass within 100 metres distance?

## Upload the tubestations from London as a sf file, read the kml file. 
tubestations <- sf::st_read("London stations.kml")
tubestations_sf <- sf::st_sf(tubestations)
class(tubestations_sf)
summary(tubestations_sf)

# Set the tube stations to the BNG. 
tubestations_sf <- st_transform(tubestations_sf, BNG)

# Create buffers around each tube stations, distance fo buffer 100m. 
tubestations_buff_100m <- st_buffer(tubestations_sf, dist = 100)

# Path of the hunt from last year.
tmap_mode("view")
tm_shape(hunt) +
  tm_lines(col = "green", lwd = 4)+
  tm_shape(tubestations_buff_100m)+
  tm_polygons(col = "blue")

# Find the buffers of the station that intersect with the hunt path.
intersect_tube_hunt <- st_intersects(tubestations_buff_100m, huntSF)
# Visualize the matrix of the values (true/false) that intersect with the hunt path. We want to sum all the true value (=1) to know the total number of the tubestations that the team pass on their road. 
# Create a dataframe with the true value. Sum all the column value of the created dataframe. 
lengths(intersect_tube_hunt)
intersection_df <- data.frame(intersect_tube_hunt)
View(intersection_df)
# Calculate the number of stations that the team passed by!
colSums(intersection_df)


## 3. How many points did you score based on treasure hunt locations they managed to get within 300 metres of? 
huntaddresses <- read.csv("huntaddresses.csv")
# edit file outside R
write_csv(huntaddresses,"huntaddresses.csv")
# another way to edit the file! From inside R: huntaddresses <- edit(huntaddresses)

# Specify the column with the coordinate!
coordinates(huntaddresses) <- c("lon","lat")
# First define the coordinate of the huntaddresses.
proj4string(huntaddresses) <- WGS84
# Change the huntaddresses to an sf object.
huntaddressesSF <- st_as_sf(huntaddresses)
# Change the coordinate from a sf object. 
huntaddressesSF_BNG <- st_transform(huntaddressesSF, BNG)

# Map the huntadresses
tmap_mode("view")
tm_shape(hunt) +
  tm_lines(col = "green", lwd = 4)+
  tm_shape(huntaddressesSF_BNG)+
  tm_dots(col = "blue")

# Create buffers around each huntaddresses, distance fo buffer 300m.
huntaddresses_buff_300m <- st_buffer(huntaddressesSF_BNG, dist = 300)

# Find the buffers of the huntaddresses that intersect with the hunt path.
intersect_hunt_path <- st_intersects(huntaddresses_buff_300m, huntSF)

# Map the points with the buffer around it!
tmap_mode("view")
tm_shape(hunt) +
  tm_lines(col = "green", lwd = 4)+
  tm_shape(huntaddresses_buff_300m)+
  tm_polygons(col = "blue")

# Create a dataframe with the huntaddresses that intersect the path. 
intersection_hunt_path_df <- data.frame(intersect_hunt_path)
View(intersection_hunt_path_df)
# Create a list of the huntaddresses id.
row_intersection <- intersection_hunt_path_df[,1]
# Selection only the huntaddresses id with the amount of points that intersect the path. 
huntaddresses_intersection <- huntaddressesSF_BNG[row_intersection,2]
# Calculate the sum of the points!
# Set the column point as numeric. 
huntaddresses_intersection[,1] <- as.numeric(huntaddresses_intersection[,1])
# Remove the geometry!
st_geometry(huntaddresses_intersection) <- NULL
# Finally sum all the points!
colSums(huntaddresses_intersection)
View(huntaddresses)

# Other possibility of summing the points! -> sum(huntaddresses_intersection$Points)


## 4. Which wards did you pass through that had the (a) lowest and (b) the highest reates of Male Life Expectancy?

####### Join the wards for city of London! ####### 
# Open the LondonWardsBoundaries
LondonWards <- readOGR("LondonWardsBoundaries/LondonWardsNew.shp", layer="LondonWardsNew")
# Change the sp object back to the sf object. 
LondonWardsSF <- st_as_sf(LondonWards)
city <- LondonWardsSF[1:25,]
citycombined <- st_union(city)
class(citycombined)
# convert to sf object to merge it with the other sf!
citycombined <- st_sf(citycombined)
citycombined <- cbind(citycombined, LondonWardsSF[1,c(1:7)])
#remove the extra geometry
citycombined <- citycombined[,-c(9)]
# rename the generated geometry of citycombined to geometry
names(citycombined)[8] <- "geometry"
st_geometry(citycombined) <- "geometry"
# rename the names in the citycombined
citycombined$WD11CD <-"E09000001"
citycombined$WD11CDO <- "00AA"
citycombined$WD11NM <- "CityofLondon"
# Add the citycombined to LondonWardsSF -> (rbind -> add a new row to all the columns, if the columns are matching!)
LondonWardsSF_city <- rbind(citycombined,LondonWardsSF)
View(LondonWardsSF_city)
# Delete the rows of city of london wards and the columns that are unnecessary!
LondonWardsSF_city <- LondonWardsSF_city[-c(2:26),-c(5:7)]
# Check if the city of London has been merged and correctly added to the LondonWards!
qtm(LondonWardsSF_city)
# Check the coordinate system of the object!
st_crs(LondonWardsSF_city)
# Need to set the coordinate back to BNG! 
LondonWardsSF_city <- st_transform(LondonWardsSF_city, WGS84)
LondonWardsSF_city <- st_transform(LondonWardsSF_city, BNG)

####### Join the LondonData with the LondonWardsSF_city! ####### 
# Open the datafile with male expectancy for each wards. 
LondonData <- read.csv("LondonData.csv")
names(LondonData)

# Select only the columns with the name of the new code and the malelifeexpectancy in the LondonData.
LondonData_malelifeexpectancy <- LondonData[,c(3,19)]
# Join the LondonData_malelifeexpectancy with the LondonWardsSF_city!
names(LondonData_malelifeexpectancy)[1] <- "WD11CD"
LondonWardsSF_city_maledata <- merge(LondonWardsSF_city,LondonData_malelifeexpectancy, by=c("WD11CD"), all=TRUE)
LondonWardsSF_city_maledata <- LondonWardsSF_city_maledata[-c(626:658),]

####### Find the wards the team 7 have been going through! ####### 
# Find the intersection of the path with the LondonWardsSF_city_maledata. 
wards_hunt_intersect <- st_intersects(LondonWardsSF_city_maledata, huntSF)
# Create a dataframe with the huntwards that intersect the path. 
intersection_wards_hunt_df <- data.frame(wards_hunt_intersect)
View(intersection_wards_hunt_df)
# Create a list of the wards the path is going through (wards id).
wards_intersection <- intersection_wards_hunt_df[,1]
# Selection only the wards with the male expectancy that intersect the path. 
wardshunt_intersection <- LondonWardsSF_city_maledata[wards_intersection,]
View(wardshunt_intersection)

####### Find the wards with the min and max values of male expectancy! #######
max_male_wards <- wardshunt_intersection[wardshunt_intersection$Malelifeexpectancy200913 == max(wardshunt_intersection$Malelifeexpectancy200913),]
max_male_wards$WD11NM
min_male_wards <- wardshunt_intersection[wardshunt_intersection$Malelifeexpectancy200913 == min(wardshunt_intersection$Malelifeexpectancy200913),]
min_male_wards$WD11NM


## 5. Taking the average of all Wards that you passed through, what was the average life expectancy at birth for babies born in those wards along the whole route? This can be both Male and Female life expectancies.

# Open the datafile with life expectancy at birth each wards. 
Lifeexpectancy <- read.csv("life-expectancy-ward-at-Birth.csv")
View(Lifeexpectancy)

# Select only the columns with the name of the ward code and the female life expectancy at birth for the year 2010/2014.
Lifeexpectancy_female <- Lifeexpectancy[,c(1,75)]
View(Lifeexpectancy_female)

# Join the Lifeexpectancy female with the LondonWardsSF_city!
names(Lifeexpectancy_female)[1] <- "WD11CD"
LondonWardsSF_city_female <- merge(LondonWardsSF_city,Lifeexpectancy_female, by=c("WD11CD"), all=TRUE)
View(LondonWardsSF_city_female)

# Selection only the wards with the female life expectancy at birth that intersect the path. 
wardshunt_intersection_female <- LondonWardsSF_city_female[wards_intersection,]
View(wardshunt_intersection_female)

####### Calculate the average life expectancy for the female born in those wards between years 2010 and 2014! #######
mean(wardshunt_intersection_female$X2010.14.Female.Life.expectancy.at.birth)


## 6. Is there any spatial patterns for CASA Treasure Hunt locations? Or are they randonly distributed?
# Point pattern analysis
library(spdep)
library(rgdal)

# First have a look at how the hunt addresses are distributed in London!
tmap_mode("view")
tm_shape(LondonWardsSF_city) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(huntaddressesSF)+
  tm_dots(col = "blue", size=0.04)

######### Density Analysis ############
# Combine all the wards together
Londoncombined <- st_union(LondonWardsSF_city)
Londoncombined <- st_sf(Londoncombined)

# Check that the London wards have been combined together!
tm_shape(Londoncombined) +
  tm_polygons(col = NA, alpha = 0.5)

#Set the coordinate system for huntaddresses!
huntlocation <- st_transform(huntaddressesSF,BNG)
#clip the hunt locations to London!
London_hunt <- huntlocation[Londoncombined,]

# Check that the huntlocations have been clip to London!
tmap_mode("view")
tm_shape(Londoncombined) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(London_hunt) +
  tm_dots(col = "blue")

# Convert it to sp object!
Londoncombined_sp <- as(Londoncombined, "Spatial")
London_hunt_sp <- as(London_hunt, "Spatial")
# Create an observation window to carry on the spatial anlysis!
window <- as.owin(Londoncombined_sp)
plot(window)

# Create a ppp object
London_hunt_sp.ppp <- ppp(x=London_hunt_sp@coords[,1],y=London_hunt_sp@coords[,2],window=window)
plot(London_hunt_sp.ppp,pch=16,cex=0.5)

# Kernel Density 
plot(density(London_hunt_sp.ppp,sigma=600))

## Ripley's K analysis! 
K <- Kest(London_hunt_sp.ppp, correction="border")
plot(K)
# The plot shows that every location are clustered! -> No dispersion??


library(rgdal)
library(GISTools)

######## Analysis for number of hunt in each wards! ##########
huntlocation <- st_transform(huntaddressesSF,BNG)
## Transform huntlocation and LondonWards Map to SpatialPointDataFrame  
huntlocation_sp <- as(huntlocation,"Spatial")
LondonWards_sp <- as(LondonWardsSF_city, "Spatial")
## Counts the number of hunt in each wards. "poly.counts" function!
counts_hunt <- poly.counts(huntlocation_sp, LondonWards_sp)
View(counts_hunt)

# Add the count number as a column in the spatialPolygonsDataframe
LondonWards_sp@data$huntcount <- counts_hunt
# Calculate the density -> Wards are of different size!
LondonWards_sp@data$huntdensity <- LondonWards_sp$huntcount/poly.areas(LondonWards_sp)
LondonWards_sp@data

# Quick Choropleth map 
tm_shape(LondonWards_sp) +
  tm_polygons("huntdensity",
              style="jenks",
              palette="RdBu",
              midpoint=NA,
              title="Hunt Density")

# Check with a Moran's I test for clustering!
library(spdep)
# Calculate centroids for all Wards in London
coordsW <- coordinates(LondonWards_sp)
# Generate a spatial weights matrix and create a neighbours list
LWard_nb <- poly2nb(LondonWards_sp, queen=T)
# Create a spatial weights object from these weights
Lward.lw <- nb2listw(LWard_nb, style="C")

# Moran's I test -> cluster value (=1) or dispersed values (=-1)
moranI_hunt <- moran.test(LondonWards_sp@data$huntdensity, Lward.lw)
moranI_hunt
## moranI_hunt = 0.11 -> no autocorrelation (almost perfect randomness!)

# Geary's C test -> similar values or dissimilar values clustering?
gearyC_hunt <- geary.test(LondonWards_sp@data$huntdensity, Lward.lw)
gearyC_hunt
## gearyC_hunt = 1.08 -> no spatial autocorrelation

# Getis Ord General G -> high or low values are clustering!
globalG_hunt <- globalG.test(LondonWards_sp@data$huntdensity, Lward.lw)
globalG_hunt
## globalGhunt: G statistic = 0.006 > G Expectation = 0.001
## High Values tend to cluster



################ Moran's I #########################
#use the localmoran function to generate I for each ward in the city
moranI_hunt_local_count <- localmoran(LondonWards_sp@data$huntcount, Lward.lw)
moranI_hunt_local_density <- localmoran(LondonWards_sp@data$huntdensity, Lward.lw)
#what does the output (the localMoran object) look like?
head(moranI_hunt_local_density)
## Copy the I score and z-score to the SpatialPolygonsDataFrame
LondonWards_sp@data$BLocI <- moranI_hunt_local_count[,1]
LondonWards_sp@data$BLocIz <- moranI_hunt_local_count[,4]
LondonWards_sp@data$BLocIR <- moranI_hunt_local_density[,1]
LondonWards_sp@data$BLocIRz <- moranI_hunt_local_density[,4]

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
#Moran's I Interactive Map Viewer. 
tm_shape(LondonWards_sp) +
  tm_polygons("BLocIRz",
              style="fixed",
              breaks=breaks1,
              palette="RdBu",
              midpoint=NA,
              title="Local Moran's I, Hunt Location in London")
## High Clustering in the center of London! 

################ Getis Ord G #########################
# Calculate Getis Ord G for each London Wards
globalG_hunt_local_density <- localG(LondonWards_sp@data$huntdensity, Lward.lw)
head(globalG_hunt_local_density)
# Append the data to the SpatialPolygonsDataframe
LondonWards_sp@data$BLocGiRz <- globalG_hunt_local_density
#Plot the Local Getis Ord G 
tm_shape(LondonWards_sp) +
  tm_polygons("BLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette= "RdBu",
              midpoint=NA,
              title="Local Getis Ord G,Hunt Location in London")
