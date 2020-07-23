
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)

### function to remove attributes appearing after scaling
one_entry <- function(x) {
  for (i in length(x)) attr(x[[i]], "names") <- NULL
  return(x)
}

## function to write table to clipboard
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(my.df)


setwd("/Users/geraldn/Dropbox/Cnidaria meta-data/data/")
dat<-read.table("data_for_analysis_withabiotic_June2020.csv", sep=",",header=T)   ## na
 names(dat)
# "AE_9eV_temp" "AE_9eV_temp_pos_response"  "AE_9eV_temp_variation"                   
# "AE_9eV_CO2ppm"  "AE_9eV_CO2ppm_pos_response" "AE_9eV_CO2ppm_variation"
 #############################################################################################
 ##    general clean up     #####################
 #   unique(dat$Stressors.studied_study)
 sdat<- dat %>% #  names(sdat)
   mutate(temp_change=Experimental_temp.level.degrees-Control_temp.level.degrees) %>% 
   mutate(co2_change=Experimental_pCO2.level.µatm-Control_pCO2.level.µatm) %>% # hist(sdat$co2_change)
   filter(Taxa_catagory=="Calcifying coral")  %>%  ##  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   filter(Deep.sea.=="No" | is.na(Deep.sea.)) %>%  ##  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   filter(Latitude.decimal.degrees < 35 & Latitude.decimal.degrees > -35) %>%  ##  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   filter(co2_change <= 800 | is.na(co2_change)) %>% #  # !!!!!!!!!!!!!!!!limit to realistic changes in CO2
   filter(!is.na(Latitude.decimal.degrees)) %>% 
   # standardize all independent variablse
   mutate(sPresent.Surface.Temperature.Range=scale(Present.Surface.Temperature.Range),
          syrange=scale(yrange),
          sPresent.Surface.Temperature.Lt.max=scale(Present.Surface.Temperature.Lt.max),
          scoral_region_coral_richness=scale(coral_region_coral_richness), 
          ssstanom=scale(sstanom),
          smsec_humanpop_50km=scale(msec_humanpop_50km),  # hist(sdat$msec_humanpop_50km)
          svel_linear_change=scale(vel_linear_change))

 
 sdat <- lapply( sdat, FUN=one_entry)  # remove all attributes from scale !!!
 sdat<-data.frame(sdat)
 mes<-sdat %>% 
   dplyr::select(Reference.ID,Latitude.decimal.degrees,Longitude.decimal.degrees.,
                 coral_region_ID,yrange,Present.Surface.Temperature.Range,Present.Surface.Temperature.Lt.max,
                 sstanom ) %>% 
   filter(!duplicated(Reference.ID) ) %>% 
   filter(!is.na(Latitude.decimal.degrees)) %>% 
   filter(is.na(Present.Surface.Temperature.Range)) 
 
   
HH::vif(xx = as.data.frame(sdat[ ,c("Present.Surface.Temperature.Range","sstanom","Present.Surface.Temperature.Lt.max",
                                       "msec_humanpop_50km","yrange",
                                       "Present.Surface.Salinity.Mean","coral_region_coral_richness",
                                       "vel_linear_change")]))
 #   hist(sdat$msec_humanpop_50km)
 ##  all < 1.5     O2 range and temp range highly correlated, O2 removed , salinity with 2.5 removed then <1.5
#############################################################################################
#############################################################################################
#    get info from global coral  and clean   

c_dat<-data.table::fread(file = '/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export/Global_CR_matchinfo.csv', sep = ',', header = TRUE)

## calc yrange
c_dat$yrange<-NA
c_dat$yrange[which(c_dat$ymin>0 & c_dat$ymax>0)]<- c_dat$ymax[which(c_dat$ymin>0 & c_dat$ymax>0)] - c_dat$ymin[which(c_dat$ymin>0 & c_dat$ymax>0)]
c_dat$yrange[which(c_dat$ymin<0 & c_dat$ymax>0)]<- c_dat$ymax[which(c_dat$ymin<0 & c_dat$ymax>0)] + abs(c_dat$ymin[which(c_dat$ymin<0 & c_dat$ymax>0)])
c_dat$yrange[which(c_dat$ymin<0 & c_dat$ymax<0)]<- abs(c_dat$ymin[which(c_dat$ymin<0 & c_dat$ymax<0)]) - abs(c_dat$ymax[which(c_dat$ymin<0 & c_dat$ymax<0)])

# get scaled
c_dat2<- c_dat %>% #  names(c_dat)
  #mutate(yrange=ymax-ymin) %>% 
  mutate(sPresent.Surface.Temperature.Range=scale(Present.Surface.Temperature.Range.asc11),
         sPresent.Surface.Temperature.Lt.max=scale(Present.Surface.Temperature.Lt.max.asc11),
         scoral_region_coral_richness=scale(coral_region_coral_richness), 
         ssstanom=scale(sstanom),
         syrange=scale(yrange),
         smsec_humanpop_50km=scale(msec_humanpop_50km),  # hist(sdat$msec_humanpop_50km)
         svel_linear_change=scale(linear_change_temp))

c_dat2 <- lapply( c_dat2, FUN=one_entry)  # remove all attributes from scale !!!
c_dat2<-data.frame(c_dat2)

 ######################################## following meeting in June
 #           YES
 #    Depth of collection
 #    lt.max temp
 #   Salinity mean
 #   coral_region_coral_richness
 #           NO 
 #  OHI
###############################################
##  universal variables        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
maxyi<-2  #  set maximum yi, will remove everything larger    Tested at 5 - then 915 studies and same results
###############################################

# warming only --------------------------------------------------------

###############################################
# make new columns for target reesponse
##  use positive AE- better after Shannon plotted these values !!!!!!
###   but norm qq plot better when original data  !!!!!!!!
#sdat$yi<- sdat$log_ratio      # hedgesG  #  class(sdat$yi)      names(sdat)   sdat$AE_9eV_temp   log_ratio
sdat$yi<-sdat$temp_AE
sdat$vi<-sdat$temp_AE_variation
target<-"warming"  # unique(sdat$Stressors.studied_study)---  acidification both warming
minchange<-1  # set minimum change between control and exp

sdat2<- sdat %>% 
  filter(Stressors.studied_study==target & complete.cases(Stressors.studied_study)) %>% 
  filter(complete.cases(yi))  %>% 
  filter(!is.infinite(yi)) %>% 
  filter(temp_change > minchange)  %>%    # as suggested by Carlos
  filter(abs(yi)<= maxyi) %>%   #15?  25 works    hist(sdat$AE_9eV_temp)
  mutate(log_temp_change=log(temp_change)) %>% 
  mutate(yit=log(abs(yi)+1)) %>%   # get log transform o yi
  mutate(yi_log=if_else(yi<0,(yit*-1), yit))

hist(sdat2$yi)
hist(sdat2$yi_log)
#  
tsolo<-sdat2 %>%  #  or tboth, csolo, cboth
      mutate(sPresent.Surface.Temperature.Range=scale(Present.Surface.Temperature.Range),
       syrange=scale(yrange),
       sPresent.Surface.Temperature.Lt.max=scale(Present.Surface.Temperature.Lt.max),
       scoral_region_coral_richness=scale(coral_region_coral_richness), 
       ssstanom=scale(sstanom),
       smsec_humanpop_50km=scale(msec_humanpop_50km),  # hist(sdat$msec_humanpop_50km)
       svel_linear_change=scale(vel_linear_change))

#############   model    +yrange    summary(lmer.mod)    hist(mdat$yi)     hist(log(mdat$yi+15))
mdat<-tsolo
#      mdat<-mdat[log(mdat$yi+15)>1,]    hist(mdat$log_temp_change)

lmer.mod<-lmer(yi_log~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness 
               +ssstanom
               +smsec_humanpop_50km
               +svel_linear_change
               + (1|PROVINCE/Reference.ID)+(1|Genus)
               #+ (1|temp_change)
               , weights=1/vi , data=mdat)
#  interactions - no inter. significant
lmer.mod<-lmer(yi_log~(sPresent.Surface.Temperature.Range
                   +syrange
                   +sPresent.Surface.Temperature.Lt.max
                   +scoral_region_coral_richness 
                   +ssstanom
                   +smsec_humanpop_50km
                   +svel_linear_change)^2
               + (1|PROVINCE/Reference.ID)+(1|Genus)
               #+ (1|temp_change)
               , weights=1/vi , data=mdat)

# not transform has better redis but slightluyy worse qq ---- use this
lmer.mod<-lmer(yi_log~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness 
               +ssstanom
               +smsec_humanpop_50km
               +svel_linear_change
 
             +  scoral_region_coral_richness:svel_linear_change

               
               + (1|PROVINCE/Reference.ID)+(1|Genus)
               #(1|temp_change)
               , weights=1/vi , data=mdat)
summary(lmer.mod)   #  AIC no interactions 2290 2287
AIC(lmer.mod)
# piecewiseSEM::sem.model.fits(lmer.mod)   library(MuMIn)
MuMIn::r.squaredGLMM(lmer.mod)  # m is variance explained by predictors, c is both predictors and random factors
sjPlot::plot_model(lmer.mod, type = "pred", terms=c("sPresent.Surface.Temperature.Range")) # re --plot random effects

jtools::interact_plot(lmer.mod, pred = "sPresent.Surface.Temperature.Lt.max", modx = "scoral_region_coral_richness", 
                      plot.points = TRUE,interval = TRUE,
                      int.width = 0.95,
                      y.label= "Effect size")  #x.label="Ocean health index"
plot(lmer.mod)
q<-qqnorm(resid(lmer.mod))
qqline(resid(lmer.mod))
lmer.mod<-lmer(yi~sPresent.Surface.Temperature.Lt.max
               + (1|PROVINCE/Original.ref.number)+(1|Genus), weights=1/vi , data=mdat)
MuMIn::r.squaredGLMM(lmer.mod)
#######################  get predict
summary(mdat$log_temp_change)
c_dat2<- c_dat2 %>% 
  mutate(syrange=median(mdat$syrange, na.rm=T) )

p<-predict(lmer.mod, newdata = c_dat2, newparams = NULL,
           re.form = NA, random.only=FALSE, terms = NULL
)  #     ,type = c("link", "response") - same  hist(p)

c_dat2$warm_all_predict<-p
## save
#   data.table::fwrite(c_dat2, file = '/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export/Global_CR_matchinfo_predict_warming_all2.csv', row.names=F, sep=",")

###############################################

# warming with co2 --------------------------------------------------------

###############################################
# make new columns for target reesponse
sdat$yi<-sdat$AE_9eV_temp  #  class(sdat$yi)
sdat$vi<-sdat$AE_9eV_temp_variation
target<-"both"  # unique(sdat$Stressors.studied_study)---  acidification both warming
minchange<-1  # set minimum change between control and exp

sdat2<- sdat %>% 
  filter(Stressors.studied_study==target & complete.cases(Stressors.studied_study)) %>% 
  filter(complete.cases(yi))  %>% 
  filter(!is.infinite(yi)) %>% 
  filter(temp_change > minchange)  %>%    # as suggested by Carlos
  filter(abs(yi)<= maxyi) %>%  #15?  25 works    hist(sdat$AE_9eV_temp)   hist(sdat2$co2_change)   range(sdat2$temp_change)
  filter(co2_change>1) %>% 
  filter(complete.cases(co2_change))  %>% 
  mutate(yit=log(abs(yi)+1)) %>%   # get log transform o yi
  mutate(yi_log=if_else(yi<0,(yit*-1), yit))
#  
tboth<-sdat2  %>%  #  or tboth, csolo, cboth
  mutate(sPresent.Surface.Temperature.Range=scale(Present.Surface.Temperature.Range),
         syrange=scale(yrange),
         sPresent.Surface.Temperature.Lt.max=scale(Present.Surface.Temperature.Lt.max),
         scoral_region_coral_richness=scale(coral_region_coral_richness), 
         ssstanom=scale(sstanom),
         smsec_humanpop_50km=scale(msec_humanpop_50km),  # hist(sdat$msec_humanpop_50km)
         svel_linear_change=scale(vel_linear_change))  #  or tboth, csolo, cboth
#############   model    +yrange    summary(lmer.mod)
mdat<-tboth
lmer.mod<-lmer(yi_log~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness 
               +ssstanom
               +smsec_humanpop_50km
               +svel_linear_change
               + (1|PROVINCE/Original.ref.number)+(1|Genus)
               + (1|temp_change) + (1|co2_change)
               , weights=1/vi , data=mdat)
#  interactions - no inter. significant
lmer.mod<-lmer(yi~(sPresent.Surface.Temperature.Range
                   +syrange
                   +sPresent.Surface.Temperature.Lt.max
                   +scoral_region_coral_richness 
                   +ssstanom+msec_humanpop_50km
                   +svel_linear_change)^2
               + (1|PROVINCE/Original.ref.number)+(1|Genus)
          #     + offset(log(temp_change)+log(co2_change))
               , weights=1/vi , data=mdat)

lmer.mod<-lmer(yi~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness
               +ssstanom
               +smsec_humanpop_50km
               +svel_linear_change
               + (1|PROVINCE/Original.ref.number)+(1|Genus)
               + offset(log(temp_change)+log(co2_change))
               , weights=1/vi , data=mdat)
summary(lmer.mod)
MuMIn::r.squaredGLMM(lmer.mod)  # m is variance explained by predictors, c is both predictors and random factors
###############################################

# co2 only  --------------------------------------------------------

###############################################
# make new columns for target reesponse
sdat$yi<-sdat$AE_9eV_CO2ppm  #  class(sdat$yi)  names(sdat)
sdat$vi<-sdat$AE_9eV_CO2ppm_variation
target<-"acidification"  # unique(sdat$Stressors.studied_study)---  acidification both warming
minchange<-100 # similar to 1 degree ??  # set minimum change between control and exp

sdat2<- sdat %>%   # names(sdat)
  filter(Stressors.studied_study==target & complete.cases(Stressors.studied_study)) %>% 
  filter(complete.cases(yi))  %>% 
  filter(!is.infinite(yi)) %>% 
  filter(co2_change > minchange)  %>%    # as suggested by Carlos
  filter(abs(yi)<= maxyi)  #15?  25 works    hist(sdat$AE_9eV_temp)  hist(sdat2$co2_change) 
hist(sdat2$yi)
hist(sqrt(sdat2$yi))
#  
csolo<-sdat2  #  or tboth, csolo, cboth
#############   model    +yrange    summary(lmer.mod)
mdat<-csolo
lmer.mod<-lmer(yi~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness 
               +ssstanom+msec_humanpop_50km
               +svel_linear_change
               + (1|PROVINCE/Original.ref.number)+(1|Genus)
               + offset(log(co2_change))
               , weights=1/vi , data=mdat)
#  interactions - no inter. significant
lmer.mod<-lmer(yi~(sPresent.Surface.Temperature.Range
                   +syrange
                   +sPresent.Surface.Temperature.Lt.max
                   +scoral_region_coral_richness 
                   +ssstanom+msec_humanpop_50km
                   +svel_linear_change)^2
               + (1|PROVINCE/Original.ref.number)+(1|Genus)
               + offset(log(co2_change))
               , weights=1/vi , data=mdat)

lmer.mod<-lmer(yi~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness
               +ssstanom
               +smsec_humanpop_50km
               +svel_linear_change
               +syrange:scoral_region_coral_richness  
               + (1|PROVINCE/Original.ref.number)+(1|Genus)
               + offset(log(co2_change))
               , weights=1/vi , data=mdat)
summary(lmer.mod)
MuMIn::r.squaredGLMM(lmer.mod)  # m is variance explained by predictors, c is both predictors and random factors
###############################################

# co2 with warming  --------------------------------------------------------

###############################################
# make new columns for target reesponse
sdat$yi<-sdat$AE_9eV_CO2ppm  #  class(sdat$yi)  names(sdat)
sdat$vi<-sdat$AE_9eV_CO2ppm_variation
target<-"both"  # unique(sdat$Stressors.studied_study)---  acidification both warming
minchange<-100 # similar to 1 degree ??  # set minimum change between control and exp

sdat2<- sdat %>% 
  filter(Stressors.studied_study==target & complete.cases(Stressors.studied_study)) %>% 
  filter(complete.cases(yi))  %>% 
  filter(!is.infinite(yi)) %>% 
  filter(co2_change > minchange)  %>%    # as suggested by Carlos
  filter(abs(yi)<= maxyi) %>%  #15?  25 works    hist(sdat$AE_9eV_temp)  hist(sdat2$co2_change)   range(sdat2$temp_change)
  filter(temp_change>1)
  # 
hist(sdat2$yi)

cboth<-sdat2  #  or tboth, csolo, cboth
#############   model    +yrange    summary(lmer.mod)
mdat<-cboth
lmer.mod<-lmer(yi~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness 
               +ssstanom+msec_humanpop_50km
               +svel_linear_change
               + (1|PROVINCE/Original.ref.number)+(1|Genus), weights=1/vi , data=mdat)
#  interactions - no inter. significant
lmer.mod<-lmer(yi~(sPresent.Surface.Temperature.Range
                   +syrange
                   +sPresent.Surface.Temperature.Lt.max
                   +scoral_region_coral_richness 
                   +ssstanom+msec_humanpop_50km
                   +svel_linear_change)^2
               + (1|PROVINCE/Original.ref.number)+(1|Genus), weights=1/vi , data=mdat)

lmer.mod<-lmer(yi~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness
               +ssstanom
               +smsec_humanpop_50km
               +svel_linear_change
               +sPresent.Surface.Temperature.Range:syrange
               + (1|PROVINCE/Original.ref.number)+(1|Genus), weights=1/vi , data=mdat)
summary(lmer.mod)
MuMIn::r.squaredGLMM(lmer.mod)  # m is variance explained by predictors, c is both predictors and random factors


##################################################
###############################################

# both     --------------------------------------------------------

###############################################
# make new columns for target reesponse
sdat$yi<-sdat$AE_9eV_both  #  class(sdat$yi)  names(sdat)
sdat$vi<-sdat$AE_9eV_both_variation
target<-"both"  # unique(sdat$Stressors.studied_study)---  acidification both warming
minchange<-100 # similar to 1 degree ??  # set minimum change between control and exp
minchange2<-1 

sdat2<- sdat %>% 
  filter(Stressors.studied_study==target & complete.cases(Stressors.studied_study)) %>% 
  filter(complete.cases(yi))  %>% 
  filter(!is.infinite(yi)) %>% 
  filter(temp_change > minchange2)  %>%    # as suggested by Carlos  co2_change > minchange & 
  filter(abs(yi)<= maxyi)  #15?  25 works    hist(sdat$AE_9eV_temp)
# 
hist(sdat2$yi)

cboth<-sdat2  #  or tboth, csolo, cboth
#############   model    +yrange    summary(lmer.mod)
mdat<-cboth
lmer.mod<-lmer(yi~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness 
               +ssstanom+msec_humanpop_50km
               +svel_linear_change
               + (1|PROVINCE/Original.ref.number)+(1|Genus)
               + offset(log(temp_change)+log(co2_change))
               , weights=1/vi , data=mdat)
#  interactions - no inter. significant
lmer.mod<-lmer(yi~(sPresent.Surface.Temperature.Range
                   +syrange
                   +sPresent.Surface.Temperature.Lt.max
                   +scoral_region_coral_richness 
                   +ssstanom+msec_humanpop_50km
                   +svel_linear_change)^2
               + (1|PROVINCE/Original.ref.number)+(1|Genus)
               + offset(log(temp_change)+log(co2_change))
               , weights=1/vi , data=mdat)

lmer.mod<-lmer(yi~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness
               +ssstanom
               +smsec_humanpop_50km
               +svel_linear_change
               + (1|PROVINCE/Original.ref.number)+(1|Genus)
               + offset(log(temp_change)+log(co2_change))
               , weights=1/vi , data=mdat)
summary(lmer.mod)
MuMIn::r.squaredGLMM(lmer.mod)  # m is variance explained by predictors, c is both predictors and random factors


##################################################





# plot --------------------------------------------------------------------

TeachingDemos

sjPlot::plot_model(lmer.mod, type = "re") # re --plot random effects
#  color palletres
##   "aqua", "warm", "dust", "blambus", "simply", "us", "deep reefs", "breakfast club" and "metro ui"
sjPlot::plot_model(lmer.mod, type = "pred", terms=c("yrange"),
                   colors="red") # re --plot random effects
sjPlot::plot_model(lmer.mod, type = "pred", terms=c("Present.Surface.Temperature.Range")) # re --plot random effects
sjPlot::plot_model(lmer.mod, type = "pred", terms=c("coral_region_coral_richness")) # re
sjPlot::plot_model(lmer.mod, type = "pred", terms=c("msec_humanpop_50km ")) #

jtools::interact_plot(lmer.mod, pred = "Present.Surface.Temperature.Lt.max", modx = "coral_region_coral_richness", 
                      plot.points = TRUE,interval = TRUE,
                      int.width = 0.95,
                      y.label= "Effect size")  #x.label="Ocean health index"
jtools::interact_plot(lmer.mod, pred = "coral_region_coral_richness", modx = "Present.Surface.Temperature.Lt.max", 
                      plot.points = TRUE,interval = TRUE,
                      int.width = 0.95,
                      y.label= "Effect size") 



lmer.mod2<-lmer(yi~yrange
               + (1|PROVINCE/Original.ref.number)+(1|Genus), weights=1/vi , data=sdat)

sjPlot::plot_model(lmer.mod2, type = "pred",terms="yrange") # re --plot random effects

# pred-predicted values







