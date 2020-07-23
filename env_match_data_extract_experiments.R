library(dplyr)
library(tidyr)
library(ncdf4)  
library(raster)
library(stringr)
library(mregions) 
library(rgeos)
library(rgdal)

setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
stud<-read.table("study_fixed.csv", sep=",",header=T)
dat_all<-read.table("data_for_analysis_withabiotic.csv", sep=",",header=T)   ## names(dat_all)

### prep data  
names(stud)
data1<- stud %>% 
  mutate(unique_study_loc=paste(Original.ref.number,latitude..decimal.degrees., longitude..decimal.degrees., sep="_")) %>% 
  filter(!duplicated(unique_study_loc))  %>%   # from 468 to 301
  dplyr::select(latitude..decimal.degrees., longitude..decimal.degrees.) 


## get study id of coral -  calcifying
unique(dat_all$Taxa)
# for first manuscript, match taht data
calc_coral<- dat_all %>% 
  filter(Taxa=="Calcifying coral") %>% 
  filter(Deep.sea.=="No" | is.na(Deep.sea.))  %>% 
  filter(variable.label.fix.2 !="Other") %>% 
  filter(!is.na(unique_study_loc)) %>% 
  filter(!duplicated(latitude..decimal.degrees. , longitude..decimal.degrees.)) %>% 
  dplyr::select(longitude..decimal.degrees. , latitude..decimal.degrees. ) %>% 
  filter(complete.cases(longitude..decimal.degrees.))
## find weird points
mes<- dat_all %>% 
  filter(Taxa=="Calcifying coral") %>% 
  filter(Deep.sea.=="No" | is.na(Deep.sea.))  %>% 
  filter(variable.label.fix.2 !="Other") %>% 
  filter(!is.na(unique_study_loc)) %>% 
  filter(!duplicated(latitude..decimal.degrees. , longitude..decimal.degrees.)) %>% 
    filter(latitude..decimal.degrees. > 40)

#   write.csv(data1, file="locations_map.csv", row.names = F)
##  get Coral map   @!!!!!!!!!!!!!!!!!!!!!!!!!!!
# coral_map <- readOGR(dsn = "/Users/geraldn/Dropbox/Global_databases/data_unep_wcmc_coral_reef/DataPack-14_001_WCMC008_CoralReefs2010_v2/01_Data", layer = "14_001_WCMC008_CoralReef2010_v2")   # 
##   summary(coral_map)  names

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
##########       map with all points and latitude frequnce      #############################
library(mapdata)
library(maps)
# library(chron)
#?     library(mgcv) #  ,mar=c(4,6.5,0,0)
#par(mar=c(0,0,0,0),oma=c(0,2,0,0))#(bottom, left, top, right)  ## messes up projection
par(fig=c(0,0.85,0,1))
map( "worldHires",xlim = c(-180,180), ylim = c(-85,85),fill=T,col="gray", border=F,
     cex.lab=1,mar=c(3,5,0,2))
map.axes(lty=1.2)
plot(coral_map,col=NULL, border="#E6532263", lwd=.7,add=T)    # ,add=T
points(data1$longitude..decimal.degrees., data1$latitude..decimal.degrees., cex=.7)
mtext("Longitude", SOUTH<-1, line=-5.5, adj=.42,outer=TRUE,cex=1)
mtext("Latitude", WEST<-2, line=-1.2, adj=0.5,outer=TRUE,cex=1)
par(fig=c(0.83,1,0.237,0.765), mar=c(0,.5,0,1), new=TRUE)
h<-hist(data1$latitude..decimal.degrees.,plot=F)
hh<-c(0,0,0,h$counts,0)  #  c(0,0,h$counts,0)
barplot(hh,width=1,horiz=T, beside=T,xlim=c(0,100),
        space=0, las=1)   # names.arg = hh$mids
axis(side=2, at=c(2,7,12), labels=c(-50,0,50),las=0)
box(lty=1.2)
mtext("Locations", SOUTH<-1, line=-5.5, adj=.95,outer=TRUE,cex=1)

#       dev.off() 
##  for checking
#  barplot(hh,width=1,horiz=T, beside=T,xlim=c(0,100),
#        space=0, las=1, names.arg = h$mids)   
################
#############################################################################################################################
#############################################################################################################################
##########       map of corals with latitude frequnce      #############################
library(mapdata)
library(maps)
# library(chron)
#?     library(mgcv) #  ,mar=c(4,6.5,0,0)
#par(mar=c(0,0,0,0),oma=c(0,2,0,0))#(bottom, left, top, right)  ## messes up projection
par(fig=c(0,0.85,0,1),xpd=F)
map( "worldHires",xlim = c(-180,180), ylim = c(-85,85),fill=T,col="gray", border=F,
     cex.lab=1,mar=c(3,5,0,2))
map.axes(lty=1.2)
#plot(coral_map,col=NULL, border="#E6532263", lwd=.7,add=T)    # ,add=T
points(calc_coral$longitude..decimal.degrees., calc_coral$latitude..decimal.degrees., cex=.7)
mtext("Longitude", SOUTH<-1, line=-5, adj=.42,outer=TRUE,cex=1)
mtext("Latitude", WEST<-2, line=-1.2, adj=0.5,outer=TRUE,cex=1)
par(fig=c(0.83,1,0.237,0.765), mar=c(0,.5,0,1), new=TRUE)
h<-hist(calc_coral$latitude..decimal.degrees.,plot=F)
hh<-c(0,0,0,h$counts,0,0,0)  #  c(0,0,h$counts,0)
barplot(hh,width=1,horiz=T, beside=T,xlim=c(0,100),
        space=0, las=1)   # names.arg = hh$mids
axis(side=2, at=c(2,7,12), labels=c(-50,0,50),las=0)
box(lty=1.2)
mtext("Locations", SOUTH<-1, line=-5.0, adj=.95,outer=TRUE,cex=1)

#       dev.off() 
##  for checking
#  barplot(hh,width=1,horiz=T, beside=T,xlim=c(0,100),
#        space=0, las=1, names.arg = h$mids)   
################
#############################################################################################################################

#############################################################################################################################
#############################################################################################################################
##########   extract data     #############################
library(ncdf4)  
library(raster)
library(stringr)
library(mregions) 
library(rgeos)
library(rgdal)

data<- stud %>% #   names(stud)
  mutate(unique_study_loc=paste(latitude..decimal.degrees., longitude..decimal.degrees., sep="_")) %>% 
  filter(!duplicated(unique_study_loc))  %>%   # from 468 to 301
  dplyr::select(unique_study_loc, latitude..decimal.degrees., longitude..decimal.degrees.,Location.name..country.,Original.ref.number,Data.retriever_study) %>% 
  filter(complete.cases(latitude..decimal.degrees.))

data2<-data  ## move one point slightly south to get data  names(data)
  data2$latitude..decimal.degrees.[data2$Original.ref.number==903]<--4
  data2$longitude..decimal.degrees.[data2$Original.ref.number==903]<-39.75
  data<-data2
#######################################################################################################
#######################################################################################################
################        bio oracle
###########      for running single, do not run!!!!     skip to line 91*****      ###################################################
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle")
benthic_temp_max.lt<- raster(read.asciigrid("Present.Benthic.Max.Depth.Temperature.Lt.max.asc"))
#You can plot it to get an idea of the data:
#    plot(benthic_temp_max.lt)
#extract values from rater add values directly back to the original dataframe:
data$benthic_temp_max.lt<- extract(benthic_temp_max.lt ,cbind(data$longitude..decimal.degrees., data$latitude..decimal.degrees.))
# fix na locations  ##############################  if needed    ###########
mes<-data[is.na(data$benthic_temp_max.lt),]
#######################################################################################################
############# loop to get all data   #######
#  set location of files for current, future, or far future data
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/Surface_present")   
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/Benthic_present")   
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/Benthic_future2040_2050_rcp45")
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/Benthic_future2090_2100_rcp45")
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/Surface_future2040_2050_rcp45")
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/Surface_future2090_2100_rcp45")
#
files<-list.files(patter="*.asc")
#####   get names from file name
namess<-as.data.frame(files)
namess<- namess %>%
  dplyr::rename(file_name=files) %>% 
  mutate(file_name=as.character(file_name))
namess$file_name<-substr(namess$file_name,1,nchar(namess$file_name)-4)
### specify files if needed
ff<-c(1:25)  #  c(6:16)
f<-files   #[ff]
cn<-namess$file_name[ff]
###  start loop    i<-1
for (i in 1:length(f)){
  nam<-paste(cn[i])
  x<- raster(read.asciigrid(f[i]))
data$loopname<- extract(x ,cbind(data$longitude..decimal.degrees., data$latitude..decimal.degrees.))
q<-length(names(data))-1  # use for names
names(data) <- c(names(data[,1:q]), paste(nam)) 
}
#  export table set name depending on which oracle database
setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
#  write.table(data,"biooracle_global_abiotic_present.csv", sep=",",row.names=F)
#    write.table(data,"biooracle_global_surface_abiotic_2040.csv", sep=",",row.names=F)
#    write.table(data,"biooracle_global_surface_abiotic_2090.csv", sep=",",row.names=F)
#   write.table(data,"biooracle_global_abiotic_present_surface.csv", sep=",",row.names=F)
#############################################################################################################################
#############################################################################################################################
######################################################################################################################
######################################################################################################################
######   MSEC    human related and temp anomly      ##################################
setwd("/Users/geraldn/Dropbox/Global_databases/MSEC/ecy1884-sup-0002-DataS1")
files<-list.files(patter="*.nc")
## data from 0-360, need to rotate
inv_rotate <- function(r) {
  xmin(r) <- 0
  xmax(r) <- 360
  r <- rotate(r)
  xmin(r) <- 0
  xmax(r) <- 360
  r
}
#####   get names from file name
namess<-as.data.frame(files)
namess<- namess %>%
  dplyr::rename(file_name=files) %>% 
  mutate(file_name=as.character(file_name))
namess$file_name<-substr(namess$file_name,1,nchar(namess$file_name)-3)
### specify files if needed
ff<-c(1:5,12,13)  #  c(6:16)   i<-1
f<-files[ff]
cn<-namess$file_name[ff]
###  start loop    i<-1
for (i in 1:length(f)){
  nam<-paste(cn[i])
  x<- raster(f[i])
  x<-rotate(x)
  data$loopname<- extract(x ,cbind(data$longitude..decimal.degrees., data$latitude..decimal.degrees.))
  q<-length(names(data))-1  # use for names
  names(data) <- c(names(data[1:q]), paste(nam)) 
}
# fix na locations  ##############################  if needed  names(data)   ###########
mes<-data[is.na(data$sstanom),]
### temp anomolie  #############
## few fixes for locations
data2<-data  ## move one point slightly south to get data
data2$latitude..decimal.degrees.[data2$Original.ref.number==52]<-41.451
data2$longitude..decimal.degrees.[data2$Original.ref.number==52]<--71.362
data2$latitude..decimal.degrees.[data2$Original.ref.number==37]<-48.410
data2$longitude..decimal.degrees.[data2$Original.ref.number==37]<--122.800
data2$latitude..decimal.degrees.[data2$Original.ref.number==599]<-47.292
data2$longitude..decimal.degrees.[data2$Original.ref.number==599]<--124.270
data2$longitude..decimal.degrees.[data2$Original.ref.number==562]<-0.725

#
setwd("/Users/geraldn/Dropbox/Global_databases/OHI/sst")
sstanom<- raster("sst_2006_2010_wgs.tif")     #   hist(nutpol)
data$sstanom<- extract(sstanom ,cbind(data2$longitude..decimal.degrees., data2$latitude..decimal.degrees.))
#
#  overall OHI  #########
setwd("/Users/geraldn/Dropbox/Global_databases/OHI/cumulative_impact_one_2013_global_cumul_impact_2013_mol_20150714053146")
OHI_2013<- raster("global_cumul_impact_2013_all_layers_wgs.tif")     #   hist(nutpol)
data$OHI_2013<- extract(OHI_2013 ,cbind(data$longitude..decimal.degrees., data$latitude..decimal.degrees.))
hist(data$OHI_2013)
##  gmed depth
setwd("/Users/geraldn/Dropbox/Global_databases/GMED/depth")
global_depth <- raster(read.asciigrid("gb_depth.asc"))
data$global_depth <- extract(global_depth,cbind(data$longitude..decimal.degrees., data$latitude..decimal.degrees.))
# make NA = -0.5
data$global_depth[is.na(data$global_depth)]<-0.5
#  export table
setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
write.table(data,"global_human_and_abiotic_present.csv", sep=",",row.names=F)

#############################################################################################################################
######################################################################################################################
######################################################################################################################
######     get  meow region data      ##################################
#  use ""data"" from above  
#
## should already be loaded from above   -   library(rgdal)
# ##   dsn folder,   layer is shp
ogrInfo("/Users/geraldn/Dropbox/Global_databases/regions/MEOW-TNC", "meow_ecos")
shape <- readOGR(dsn = "/Users/geraldn/Dropbox/Global_databases/regions/MEOW-TNC", layer = "meow_ecos")   # 
summary(shape)
#   plot(shape)    summary(shape)     spplot(shape, z="shannon")
crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84 copy from summary
pt <-dplyr::select(data, longitude..decimal.degrees., latitude..decimal.degrees.)
coordinates(pt) <- c("longitude..decimal.degrees.","latitude..decimal.degrees.")  # se
proj4string(pt) <- crs.geo  
###   pt<-SpatialPoints(pt)  if didn't use previous code
e<-over(pt,shape)  #  
data3<-bind_cols(data[,1:3],e)
####### get coral region and diveristy in that region  ######
cor_shape <- readOGR(dsn = "/Users/geraldn/Dropbox/Global_databases/coral_regions", layer = "Ecoregions")   # 
#div<-read.table("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export/coral_species_ranges.csv", sep=",",header=T)
#
e<-over(pt,cor_shape)
data3.1<-bind_cols(data3,e)
cor_reg<-read.table("coral_regions_diversity_table.csv", sep=",",header=T) # names(cor_reg)   names(data3.1)
cor_reg$ID=as.factor(as.character(cor_reg$ID))
#
data3.2<- data3.1 %>%   ## data ID is factor    names(data3.2)
  left_join(cor_reg, by=(c("ID" = "ID"))) %>% 
  rename(coral_region_ID=ID, coral_region=region, coral_region_coral_richness=Species.richness ) %>% 
  dplyr::select(unique_study_loc:coral_region_coral_richness)
#  export table
setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
write.table(data3.2,"Study_regions.csv", sep=",",row.names=F)

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
##########     vocc temperature velocities
#install.packages("devtools")
#library("devtools")
#  devtools::install_github("cbrown5/vocc")  ##  devtools::install_github("jebyrnes/hadsstR")
####     
library("vocc")
library("hadsstr")  ##   hadsstr::load_hadsst
#
par(mfrow = c(1,3))
plot(rtrend) # degree/decade
plot(rgrad)  # degree/km
plot(rvocc)  # km/decade
########################################################################################################
##  use hadsstr   from jbyrnes
setwd("/Users/geraldn/Dropbox/Global_databases/SST_HadISST1")
files<-list.files(patter="*.nc")    # 
x<- raster("HadISST_sst.nc")   # summary(x)    
x<-load_hadsst("HadISST_sst.nc")    # mes<-x@z$Date  last(mes)
  xx<-get_all_rasters(x, years = 1980:2016)
 # just rate
#  names(xx)   xx$names
#    plot(xx[[1]]); points(data2$longitude..decimal.degrees., data2$latitude..decimal.degrees.)
#voc<- extract(xx ,cbind(data2$longitude..decimal.degrees., data2$latitude..decimal.degrees.))  # BAd!!!!!
  crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84 copy from summary
  pt <-dplyr::select(data2, longitude..decimal.degrees., latitude..decimal.degrees.)
  coordinates(pt) <- c("longitude..decimal.degrees.","latitude..decimal.degrees.")  # se
  proj4string(pt) <- crs.geo  
voc<- extract(xx ,pt)  #
voc<-data.frame(voc)
is.na(voc$average_sst)<- !is.finite(voc$average_sst) 
dat1<-bind_cols(data2,voc)
## then take the raster value with lowest distance to point AND non-NA value in the raster
xy<-data2[,c(3,2)] 
r<-xx[[1]] # raster go from 1 to f  names(xx)
sampled = apply(X =xy , MARGIN = 1, FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
dat1$average_sst<- do.call(rbind, lapply(sampled, as.numeric))
r<-xx[[2]] # raster go from 1 to f
sampled = apply(X =xy , MARGIN = 1, FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
dat1$linear_change<- do.call(rbind, lapply(sampled, as.numeric))
r<-xx[[3]] # raster go from 1 to f
sampled = apply(X =xy , MARGIN = 1, FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
dat1$spatial_gradient<- do.call(rbind, lapply(sampled, as.numeric))
r<-xx[[4]] # raster go from 1 to f
sampled = apply(X =xy , MARGIN = 1, FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
dat1$angle<- do.call(rbind, lapply(sampled, as.numeric))
r<-xx[[5]] # raster go from 1 to f
sampled = apply(X =xy , MARGIN = 1, FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
dat1$velocity_magnitude<- do.call(rbind, lapply(sampled, as.numeric))

########################################################
#  get anomoly data
setwd("/Users/geraldn/Dropbox/Global_databases/SST_HadISST1")
x<- raster("HadSST.3.1.1.0.median.nc")   # summary(x)    
x<-load_hadsst("HadSST.3.1.1.0.median.nc")    # mes<-x@z$Date  last(mes)
xx<-get_all_rasters(x, years = 2000:2014)   # mes<-xx@z$Date  last(mes)
voc<- extract(xx ,pt)  #
voc<-data.frame(voc) # names(voc)
dat1$sstanom<-voc$average_sst


xy<-data2[,c(3,2)] 
r<-xx[[1]] # raster go from 1 to f  names(xx)
sampled = apply(X =xy , MARGIN = 1, FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
dat1$sstanom<- do.call(rbind, lapply(sampled, as.numeric))

#############   export
zz<-data.frame(dat1)
setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
write.table(zz,"climate_velocities.csv", sep=",",row.names=F)
########################################################################################################################
#############################################################################################################################
qqq<-2
par(mfrow=c(2,2), mar=c(qqq,qqq,qqq,3))
plot(xx[[1]],main="mean SST"); points(data2$longitude..decimal.degrees., data2$latitude..decimal.degrees.)
plot(xx[[2]],main="SST trend (degree/decade"); points(data2$longitude..decimal.degrees., data2$latitude..decimal.degrees.)
plot(xx[[3]],main="Spatial gradient (degree/km"); points(data2$longitude..decimal.degrees., data2$latitude..decimal.degrees.)
plot(sqrt(xx[[5]]),main="Velocity (km/decade)"); points(data2$longitude..decimal.degrees., data2$latitude..decimal.degrees.)





#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
##########    xtractomatic info      #############################
library("xtractomatic")
library("lubridate")

data1<- stud %>% 
  mutate(unique_study_loc=paste(latitude..decimal.degrees., longitude..decimal.degrees., sep="_")) %>% 
  filter(!duplicated(unique_study_loc))  %>%   # from 468 to 301
  select(latitude..decimal.degrees., longitude..decimal.degrees.) 

getInfo('jplMURSST41mday')### get info of layer make sure it exists
#### search and find dataset    chl     !!! always lowercase
mylist <- list('varname:sst', 'datasetname:mday')
searchResult <- searchData(mylist)
DT::datatable(searchResult)
### jplMURSST41mday   best global sst 2002 to 2018  monthrly
#### jplMURSST41anommday   only anomoly  
#   https://podaac.jpl.nasa.gov/dataset/MUR-JPL-L4-GLOB-v4.1


xpos <- c(238, 238)
ypos <- c(37, 37)
tpos <- c("2002-06-01", "2018-02-16")  ## use first last
dati <- xtracto_3D(xpos, ypos, tpos, "jplMURSST41mday")

dat1$time <- lubridate::as_date(MODIS$time)
# make dataframe
ascatStress <- data.frame(stress = ascat$data, time = as.Date(ascat$time))
