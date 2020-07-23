library(dplyr)
library(tidyr)
#setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
#stud<-read.table("study_fixed.csv", sep=",",header=T)
loc<-read.table("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export/locations_map.csv", sep=",",header=T)
loc<-loc[complete.cases(loc$longitude..decimal.degrees.),]
coral_dist<-read.table("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/csv/Huang&Roy_Distribution.csv", sep=",",header=T)  #  from John Pandolfi
shape <- readOGR(dsn = "/Users/geraldn/Dropbox/Global_databases/coral_regions", layer = "Ecoregions")   # 
coral_reg_nam<-read.table("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/csv/Ecoregion_PD.csv", sep=",",header=T)  #  from John Pandolfi
#  summary(shape) plot(shape)
coral_reg_nam<-coral_reg_nam[,1:3]
### get presence data ready
coral_dist1<-coral_dist %>%
  gather(key='region',value="presence",-Taxon) %>% 
  filter(presence>0) 
### get range of shapes
library(raster)
g<-shape$ID  # ID of regions
ext <- t(sapply(1:length(shape), function(i) as.vector(extent(shape[i,])))) # extract max and mins
colnames(ext) <- c('xmin', 'xmax', 'ymin', 'ymax')
shape_ranges <- data.frame(region=shape$ID, ext)  ## unique(shape$ID)
shape_ranges <- shape_ranges %>% 
  dplyr::rename(ID=region) # %>% 
  # mutate(ID=as.numeric(ID))
## start merging      names(coral_reg_nam)
coral_reg_nam<- coral_reg_nam %>% 
  bind_cols(shape_ranges) %>% 
  dplyr::select(-ID1) %>% 
  rename(region=Ecoregion)
####  get fro all species       unique(coral_reg_nam$region)  unique(dat4$region)  
dat4 <- coral_dist1 %>% 
  mutate(region=gsub("\\.", "", region)) %>%  ## remove "." from region
  left_join(coral_reg_nam) %>% 
  filter(complete.cases(xmin)) %>% 
  group_by(Taxon) %>% 
  summarise(xmin=min(xmin),xmax=max(xmax),ymin=min(ymin),ymax=max(ymax)) %>% 
  mutate(Taxon=sub("^[^_]*_","",Taxon)) %>%   # fix taxon name to match species traits   ^[^:]*:
  mutate(Taxon=gsub("_"," ",Taxon))
#
mes<-dat4[is.na(dat4$xmin),]
## calc xrange
dat4$xrange<-NA
dat4$xrange[which(dat4$xmin>0 & dat4$xmax>0)]<- dat4$xmax[which(dat4$xmin>0 & dat4$xmax>0)] - dat4$xmin[which(dat4$xmin>0 & dat4$xmax>0)]
dat4$xrange[which(dat4$xmin<0 & dat4$xmax>0)]<- dat4$xmax[which(dat4$xmin<0 & dat4$xmax>0)] + abs(dat4$xmin[which(dat4$xmin<0 & dat4$xmax>0)])
dat4$xrange[which(dat4$xmin<0 & dat4$xmax<0)]<- abs(dat4$xmin[which(dat4$xmin<0 & dat4$xmax<0)]) - abs(dat4$xmax[which(dat4$xmin<0 & dat4$xmax<0)])
## calc yrange
dat4$yrange<-NA
dat4$yrange[which(dat4$ymin>0 & dat4$ymax>0)]<- dat4$ymax[which(dat4$ymin>0 & dat4$ymax>0)] - dat4$ymin[which(dat4$ymin>0 & dat4$ymax>0)]
dat4$yrange[which(dat4$ymin<0 & dat4$ymax>0)]<- dat4$ymax[which(dat4$ymin<0 & dat4$ymax>0)] + abs(dat4$ymin[which(dat4$ymin<0 & dat4$ymax>0)])
dat4$yrange[which(dat4$ymin<0 & dat4$ymax<0)]<- abs(dat4$ymin[which(dat4$ymin<0 & dat4$ymax<0)]) - abs(dat4$ymax[which(dat4$ymin<0 & dat4$ymax<0)])

###################################################################
#  export tables
setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
write.table(dat4,"coral_species_ranges.csv", sep=",",row.names=F)

###################################################################
#  export tables
setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
write.table(coral_reg_nam,"coral_regions_diversity_table.csv", sep=",",row.names=F)


########################################################################################################
##### don't need any more  !!!!
#### get region for each location
crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84 copy from summary
pt <-dplyr::select(loc, longitude..decimal.degrees., latitude..decimal.degrees.)
coordinates(pt) <- c("longitude..decimal.degrees.","latitude..decimal.degrees.")  # se
proj4string(pt) <- crs.geo  
###   pt<-SpatialPoints(pt)  if didn't use previous code
e<-over(pt,shape)
loc_regions<-bind_cols(loc,e)
