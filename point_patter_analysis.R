# This script runs spatial point pattern analysis using the spatstat library
# specifically, it's used to uncover the patterns formed by Flickr photo points
# in Sub-Saharan Africa. Two analysis are run:
# 1) Quadrat count test to reject random patterns https://www.rdocumentation.org/packages/spatstat/versions/1.63-3/topics/quadrat.test
# 2) Calculating Ripley's K to further see if the patterns are clustered at varying distances https://rdrr.io/cran/spatstat/man/Kest.html

# some of these are useless, I think lgcp at least?
# uncomment if they are needed
library(spatstat)
#library(spatstat.data)
#library(rgdal)
library(maptools)
library(sf)
library(sp)
#library(lgcp)

setwd("C:\\path_to\\your_project_folder")

# Sub-Saharan parks and photos have been projected to EPSG:102023
# because the data must be in a projected coordinate system for the analysis
# 
parks.sf <- st_read("Sub-saharan_parks_102023.gpkg")
all_photos <- st_read("flickr_photos_in_SS_africa_102023.gpkg")
all_photos_spatial <- as(all_photos, "Spatial")
parks.spatial <- as(parks.sf, "Spatial")

# if you want to learn more about the data format
typeof(all_photos_spatial)

# looping through all the parks
for(i in 1:nrow(parks.spatial)){
  #getting the photos of a single park
  photos_in_park <- all_photos_spatial[parks.spatial[i,],]
  # park border as an "object window"
  park_border <- as(parks.spatial[i,], "owin")
  # spatstat wants the points as ppp, creating that object
  photos_ppp <- ppp(photos_in_park@coords[,1], photos_in_park@coords[,2], window = park_border)
  
  # only run the analysis if there are over the minimum amount of photos in a park
  # change value to affect which parks are analyzed
  # the separate if clauses, since certain border corrections of the K Function are computationally costly 
  # to run on large datasets. Complex corrections are used for smaller ones, simpler for larger
  # the quadrat test (qt) is currently commented out, uncomment it from below if you want to
  # plot the results of that as well
  if(npoints(photos_ppp)>1000 && npoints(photos_ppp)<10000){
    #qt <- quadrat.test(photos_ppp, nx=10, ny=10, alternative = "clustered", method="MonteCarlo")
    #plot(qt, main=paste(parks.spatial[i,]$NAME, npoints(photos_ppp), round(qt$statistic),qt$p.value, sep="/"))
    #plot(photos_ppp, add=TRUE, col = "green")
    ripley <- Kest(photos_ppp, var.approx=TRUE, correction="best")
    # printing and then plotting the results
    print(parks.spatial[i,]$NAME)
    print(ripley)
    plot(ripley, main=parks.spatial[i,]$NAME)
  }
    
  if(npoints(photos_ppp)>=10000){
    #qt <- quadrat.test(photos_ppp, nx=10, ny=10, alternative = "clustered", method="MonteCarlo")
    #plot(qt, main=paste(parks.spatial[i,]$NAME, npoints(photos_ppp), round(qt$statistic),qt$p.value, sep="/"))
    #plot(photos_ppp, add=TRUE, col = "green")
    ripley <- Kest(photos_ppp,var.approx=TRUE, correction="good")
    print(ripley)
    plot(ripley, main=parks.spatial[i,]$NAME)
  }
  
}