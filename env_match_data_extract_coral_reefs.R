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
#dat<-read.table("data_for_analysis.csv", sep=",",header=T)

### prep data  
#     
names(stud)
data_pt<- stud %>% 
  mutate(unique_study_loc=paste(Original.ref.number,latitude..decimal.degrees., longitude..decimal.degrees., sep="_")) %>% 
  filter(!duplicated(unique_study_loc))  %>%   # from 468 to 301
  dplyr::select(latitude..decimal.degrees., longitude..decimal.degrees.) 
##  get Coral map super detailed, need, used coral from naturalearth data below
 coral_map <- readOGR(dsn = "/Users/geraldn/Dropbox/Global_databases/data_unep_wcmc_coral_reef/DataPack-14_001_WCMC008_CoralReefs2010_v2/01_Data", layer = "14_001_WCMC008_CoralReef2010_v2")   # 
 # summary(coral_map)  #  names(coral_map)
  #coral_map<- readOGR(dsn = "/Users/geraldn/Dropbox/Global_databases/naturalearthdata/ne_10m_reefs", layer = "ne_10m_reefs") 
    # plot(coral_map, col="red", add=T)   
  #############################################################################################################################
#############################################################################################################################
#############################################################################################################################
##########       map with latitude frequnce      #############################
library(mapdata)
library(maps)
# library(chron)
#?     library(mgcv) #  ,mar=c(4,6.5,0,0)
#par(mar=c(0,0,0,0),oma=c(0,2,0,0))#(bottom, left, top, right)  ## messes up projection
par(mfcol=c(4,1), mar=c(.5,.5,.5,.5),oma=c(.5,.5,0,0),xpd=F)
map( "worldHires",xlim = c(-180,180), ylim = c(-35,35),fill=T,col="gray", border=F,
     cex.lab=1)
map.axes(lty=1.2)
plot(coral_map,col="#E6532263", border=NULL, lwd=.7,add=T)    # ,add=T

points(data_pt$longitude..decimal.degrees., data_pt$latitude..decimal.degrees., cex=.4)
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
#############################################################################################################################
##########   extract data     #############################
library(ncdf4)  
library(raster)
library(stringr)
library(mregions) 
library(rgeos)
library(rgdal)

#######################################################################################################
#######################################################################################################
################        bio oracle
###########      for running single, do not run!!!!     skip to line 91*****      ###################################################
sdmpredictors::list_datasets() 
li<-sdmpredictors::list_layers("Bio-ORACLE")  # 
####       files<-list.files(patter="*.asc")    # get file names of wd
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/Benthic_present")
SBT_meandepth_mean<- raster(read.asciigrid("Present.Benthic.Mean.Depth.Temperature.Mean.asc"))
SBT_meandepth_range<- raster(read.asciigrid("Present.Benthic.Mean.Depth.Temperature.Range.asc"))
SBT_meandepth_maxlt<- raster(read.asciigrid("Present.Benthic.Mean.Depth.Temperature.Lt.max.asc"))
SBT_mindepth_mean<- raster(read.asciigrid("Present.Benthic.Min.Depth.Temperature.Mean.asc"))
SBT_mindepth_range<- raster(read.asciigrid("Present.Benthic.Min.Depth.Temperature.Range.asc"))
SBT_mindepth_maxlt<- raster(read.asciigrid("Present.Benthic.Min.Depth.Temperature.Lt.max.asc"))
#
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/Surface_present")
SST_mean<- raster(read.asciigrid("Present.Surface.Temperature.Mean.asc"))
SST_range<- raster(read.asciigrid("Present.Surface.Temperature.Range.asc"))
SST_maxlt<- raster(read.asciigrid("Present.Surface.Temperature.Lt.max.asc"))
#  summary(SBT_meandepth_mean)
#  mes<-raster(SBT_meandepth_mean)    # summary(mes)
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/Surface_future2040_2050_rcp45")
SST_50<- raster(read.asciigrid("2050AOGCM.RCP45.Surface.Temperature.Mean.asc"))
SST_maxlt_50<- raster(read.asciigrid("2050AOGCM.RCP45.Surface.Temperature.Lt.max.asc"))
#
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/Surface_future2090_2100_rcp45")
SST_100<- raster(read.asciigrid("2100AOGCM.RCP45.Surface.Salinity.Mean.asc"))
SST_maxlt_100<- raster(read.asciigrid("2100AOGCM.RCP45.Surface.Temperature.Lt.max.asc"))
#
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/benthicMinDepthrcp4.5_2050")
SBT_mindepth_mean_50<- raster(read.asciigrid("2050AOGCM.RCP45.Benthic.Min.Depth.Temperature.Mean.asc"))
SBT_mindepth_range_50<- raster(read.asciigrid("2050AOGCM.RCP45.Benthic.Min.Depth.Temperature.Mean.asc"))
SBT_mindepth_maxlt_50<- raster(read.asciigrid("2050AOGCM.RCP45.Benthic.Min.Depth.Temperature.Range.asc"))
#
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/benthicMinDepthrcp4.5_2100")
SBT_mindepth_mean_100<- raster(read.asciigrid("2100AOGCM.RCP45.Benthic.Min.Depth.Temperature.Mean.asc"))
SBT_mindepth_range_100<- raster(read.asciigrid("2100AOGCM.RCP45.Benthic.Min.Depth.Temperature.Mean.asc"))
SBT_mindepth_maxlt_100<- raster(read.asciigrid("2100AOGCM.RCP45.Benthic.Min.Depth.Temperature.Range.asc"))
#
setwd("/Users/geraldn/Dropbox/Global_databases/Bio-oracle/BO_bathy")
#  if need to get orignially
#  bathy_min <- sdmpredictors::load_layers("BO_bathymax", equalarea = F,rasterstack=F, datadir="/Users/geraldn/Dropbox/Global_databases/Bio-oracle/BO_bathy") ##Get from bio-oracle website
bathy_mean<- raster("BO_bathymean_lonlat.tif")
bathy_mean@data@values<-values(bathy_mean)  # summary(bathy_mean)
bathy_min<- raster("BO_bathymin_lonlat.tif")
bathy_max<- raster("BO_bathymax_lonlat.tif")
bathy_min@data@values<-values(bathy_min)  # plot(bathy_min2)    # summary(bathy_ma)
bathy_max@data@values<-values(bathy_max)
#bathy_min2<-mask(bathy_min, coral_map)
#   summary(bathy_min)
### stack data
bio_st <- stack(SST_mean,SST_range,SST_maxlt,
                SST_50,SST_maxlt_50,SST_100,SST_maxlt_100,
                SBT_mindepth_mean_50, SBT_mindepth_maxlt_50,
                SBT_mindepth_mean_100,SBT_mindepth_maxlt_100
                )
bio_st2 <- stack(SBT_meandepth_mean,SBT_meandepth_range,SBT_meandepth_maxlt,
                 SBT_mindepth_mean,SBT_mindepth_range,SBT_mindepth_maxlt
                    )

#  summary(bio_st[[20]])
#
###################################################################################################################
###################################################################################################################
#######     old skip until next !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# clip your raster stack to the desired bathymetric limits
    # dep2<-bathy
    # dep2[dep2 < -2000] <- NA  # plot(bathy)     plot(dep2) 
## mask stack, clip to depth < 100
clip <- mask(bio_st, dep2)
plot(bio_st[[2]],xlim = c(130,180), ylim = c(-35,0))   # plot only first in raster stack
plot(coral_map,col="blue", border="blue", lwd=.1,add=T)  
plot(uk, col = gray(uk@data$count/2500)) # example to get color gradiant
## mask raster
clipped_stack <- mask(r_stack, bath_mask)
## create new and subtract
w <- overlay(r, s, fun=function(x, y){x-y} )
##   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###################################################################################################################
###################################################################################################################
#
#
# extract use small and/or weights     df-returns dataframe  sp- if add to polygon
dat <- extract(bio_st, coral_map, fun=mean, na.rm=TRUE, sp = F, small=T, df=T)
dat2 <- extract(bio_st2, coral_map, fun=mean, na.rm=TRUE, sp = F, small=T, df=T)
##   can't get bathy to extract??????   used code below that used lat lon of shapes
datbat <- extract(bathy_mean, coral_map, fun=mean, na.rm=TRUE, sp = F, small=T, df=T)
# get additinoal data from coral        head(dat)
dat_coor<- data.frame(coordinates(coral_map))
dat_coral<-coral_map@data
#   combine 
dat3<-dat %>%   # names(dat3)
  bind_cols(dat,dat2,dat_coor,dat_coral) %>% 
  dplyr::rename(Langitude=X1,Latitude=X2)
dat3<-dat3[,c(74:122)]
dat3<-dat3 %>% 
  dplyr::rename(Longitude=X11,Latitude=X21)

############################################################################
############################################################################
# prep centers of reefs to used these points to get data-quicker
crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84 copy from summary
pt <-dplyr::select(dat3, Longitude, Latitude)
coordinates(pt) <- c("Longitude","Latitude")  # se
proj4string(pt) <- crs.geo  
## get min and mean bathy
dat3$bathy_min<-extract(bathy_min ,cbind(dat3$Longitude, dat3$Latitude))   
dat3$bathy_mean<-extract(bathy_mean ,cbind(dat3$Longitude, dat3$Latitude))   
dat3$bathy_max<-extract(bathy_max ,cbind(dat3$Longitude, dat3$Latitude))   
############################################################################
############################################################################
#####    get other data
## sst anom   - NOT USED - use hadSST below  !!!!!!!
setwd("/Users/geraldn/Dropbox/Global_databases/OHI/sst")
sstanom<- raster("sst_2006_2010_wgs.tif")     # 2006-2010   hist(nutpol)  names(sstanom)
sstanom@data@values<-values(sstanom) 
dat3$sstanom<-extract(sstanom ,cbind(dat3$Longitude, dat3$Latitude))   # names(dat3)
r<-sstanom    #  values(r)
xy<-dat3[,c(20,21)] ## lat,long # names(dat3)
sampled = apply(X =xy , MARGIN = 1, FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
dat3$sstanom_near<- do.call(rbind, lapply(sampled, as.numeric))


######     get  meow region data      ##################################
#  use ""data"" from above  
## should already be loaded from above   -   library(rgdal)
# ##   dsn folder,   layer is shp
#ogrInfo("/Users/geraldn/Dropbox/Global_databases/regions/MEOW-TNC", "meow_ecos")
shape <- readOGR(dsn = "/Users/geraldn/Dropbox/Global_databases/regions/MEOW-TNC", layer = "meow_ecos")   # 
summary(shape)
###   pt<-SpatialPoints(pt)  if didn't use previous code
e<-over(pt,shape)  #  
dat3<-bind_cols(dat3,e)
####### get coral region and diveristy in that region  ######
cor_shape <- readOGR(dsn = "/Users/geraldn/Dropbox/Global_databases/coral_regions", layer = "Ecoregions")   # 
e<-over(pt,cor_shape)
dat3.1<-bind_cols(dat3,e)
setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
cor_reg<-read.table("coral_regions_diversity_table.csv", sep=",",header=T) # names(cor_reg)   names(data3.1)
cor_reg$ID=as.factor(as.character(cor_reg$ID))
#
dat3.1<- dat3.1 %>%   ## data ID is factor    names(data3.1)
  left_join(cor_reg, by=(c("ID" = "ID"))) %>% 
  dplyr::rename(coral_region_ID=ID, coral_region=region, coral_region_coral_richness=Species.richness)
####
######   MSEC   human pop     ##################################
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
ff<-c(3)  #  c(6:16)   i<-1
f<-files[ff]
cn<-namess$file_name[ff]
###  start loop   
i<-1

  nam<-paste(cn[i])
  x<- raster(f[i])
  x<-rotate(x)
  dat3.1$loopname<- extract(x ,cbind(dat3.1$Longitude, dat3.1$Latitude))
  q<-length(names(dat3.1))  # use for names
  names(dat3.1)[q] <- paste(nam)
#############################################################################################################################
  ##########     vocc temperature velocities
  #install.packages("devtools")
  #library("devtools")
  #  devtools::install_github("cbrown5/vocc")  ##  devtools::install_github("jebyrnes/hadsstR")
  ####     
  library("vocc")
  library("hadsstr")  ##   hadsstr::load_hadsst

  ########################################################################################################
  ######  get mean temp and temp velocity     data is monthly
  ##  use hadsstr   from jbyrnes
  setwd("/Users/geraldn/Dropbox/Global_databases/SST_HadISST1")
  files<-list.files(patter="*.nc")    # 
  x<- raster("HadISST_sst.nc")   # summary(x)    
  x<-load_hadsst("HadISST_sst.nc")    # mes<-x@z$Date  last(mes)
  xx<-get_all_rasters(x, years = 1980:2016)
  voc<- extract(xx ,pt)  #
  voc<-data.frame(voc) # names(voc)
  dat3.1$linear_change_temp<-voc$linear_change
  r<-xx[[2]] # raster go from 1 to f for velocity
  sampled = apply(X =xy , MARGIN = 1, FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
  dat3.1$linear_change_near<- do.call(rbind, lapply(sampled, as.numeric))

  ###############################################
  ######  get anomoly data  -- added later to extra steps
  
  #   reload data
  setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
  #  dat3.1<-data.table::fread(file = 'Global_CR_matchinfo.csv', sep = ',', header = TRUE)  # names(dat3.1)
  # prep centers of reefs to used these points to get data-quicker
  crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84 copy from summary
  pt <-dplyr::select(dat3.1, Longitude, Latitude)
  coordinates(pt) <- c("Longitude","Latitude")  # se
  proj4string(pt) <- crs.geo  
  
  #  get anomoly data
  setwd("/Users/geraldn/Dropbox/Global_databases/SST_HadISST1")
  x<- raster("HadSST.3.1.1.0.median.nc")   # summary(x)    
  x<-load_hadsst("HadSST.3.1.1.0.median.nc")    # mes<-x@z$Date  last(mes)
  xx<-get_all_rasters(x, years = 2000:2014)   # mes<-xx@z$Date  last(mes)
  voc<- extract(xx ,pt)  #
  voc<-data.frame(voc) # names(voc)
  dat3.1$sstanom<-voc$average_sst
  r<-xx[[1]] # raster go from 1 to f for velocity
  xy<-dat3.1[,c(20,21)] ## lat,long # names(dat3.1)
  sampled = apply(X =xy , MARGIN = 1, FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
  dat3.1$SST_anom_near<- do.call(rbind, lapply(sampled, as.numeric))
  
  plot(xx[[1]],main="mean SST"); points(data3.1$Longitude, data3.1$Latitude)
#############################################################################################################################
  ## save
  setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
  data.table::fwrite(dat3.1[,c(1:71)], file = 'Global_CR_matchinfo.csv', row.names=F, sep=",")
  # dat<-data.table::fread(file = 'Global_CR_matchinfo.csv', sep = ',', header = TRUE)
#############################################################################################################################
######################################################################################################################
#

#############################################################################################################################
######################################################################################################################
####### get coral region and diveristy in that region  ######
cor_shape <- readOGR(dsn = "/Users/geraldn/Dropbox/Global_databases/coral_regions", layer = "Ecoregions")   # 
#div<-read.table("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export/coral_species_ranges.csv", sep=",",header=T)
#  plot(cor_shape)   # summary(cor_shape)
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
##        old mess    ignore
#############################################################################################################################
###### get all info with simple coral map,  way too much data to feasably plot

coral_map <- readOGR(dsn = "/Users/geraldn/Dropbox/Global_databases/data_unep_wcmc_coral_reef/DataPack-14_001_WCMC008_CoralReefs2010_v2/01_Data", layer = "14_001_WCMC008_CoralReef2010_v2")   # 

coral_map_simp <- readOGR(dsn = "/Users/geraldn/Dropbox/Global_databases/naturalearthdata/ne_10m_reefsf", layer = "ne_10m_reefs.shp")

# add data to coral mapp
coral_map <- tmaptools::append_data(coral_map, pred[,c(1,3,4,65,79,80,81,82)], key.shp = "LAYER_ID", key.data = "ID11")




B <- SpatialPointsDataFrame(gCentroid(poly.pr, byid=TRUE),poly.pr@data, match.ID=FALSE)
plot(A)
points(poly_centroids)
# Overlay points and extract just the code column: 
a.data <- sp::over(A, B[,"code"])
# Add that data back to A:
A$bcode <- a.data$code




#############################################################################################################################
#############################################################################################################################
##############################################################################################################################
