
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




setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
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
  filter(is.na(Present.Surface.Temperature.Range)) #  

HH::vif(xx = as.data.frame(sdat[ ,c("Present.Surface.Temperature.Range","sstanom","Present.Surface.Temperature.Lt.max",
                                       "msec_humanpop_50km","yrange",
                                      "coral_region_coral_richness",
                                       "vel_linear_change")]))
 #   hist(sdat$msec_humanpop_50km)
 ##  all < 2.3     O2 range and temp range highly correlated, O2 removed
#############################################################################################
#############################################################################################
#    get info from global coral  and clean   - 
c_dat<-data.table::fread(file = '/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export/Global_CR_matchinfo.csv', sep = ',', header = TRUE)

## calc yrange
c_dat$yrange<-NA
c_dat$yrange[which(c_dat$ymin>0 & c_dat$ymax>0)]<- c_dat$ymax[which(c_dat$ymin>0 & c_dat$ymax>0)] - c_dat$ymin[which(c_dat$ymin>0 & c_dat$ymax>0)]
c_dat$yrange[which(c_dat$ymin<0 & c_dat$ymax>0)]<- c_dat$ymax[which(c_dat$ymin<0 & c_dat$ymax>0)] + abs(c_dat$ymin[which(c_dat$ymin<0 & c_dat$ymax>0)])
c_dat$yrange[which(c_dat$ymin<0 & c_dat$ymax<0)]<- abs(c_dat$ymin[which(c_dat$ymin<0 & c_dat$ymax<0)]) - abs(c_dat$ymax[which(c_dat$ymin<0 & c_dat$ymax<0)])

# get scaled
c_dat2<- c_dat %>% #  names(c_dat2)
  #mutate(yrange=ymax-ymin) %>% # found june 2020 what, calculated above
  mutate(sPresent.Surface.Temperature.Range=scale(Present.Surface.Temperature.Range.asc11),
         sPresent.Surface.Temperature.Lt.max=scale(Present.Surface.Temperature.Lt.max.asc11),
         scoral_region_coral_richness=scale(coral_region_coral_richness), 
         ssstanom=scale(sstanom),
         syrange=scale(yrange),
         smsec_humanpop_50km=scale(msec_humanpop_50km),  # hist(sdat$msec_humanpop_50km)
         svel_linear_change=scale(linear_change_temp))

c_dat2 <- lapply( c_dat2, FUN=one_entry)  # remove all attributes from scale !!!
c_dat2<-data.frame(c_dat2)
#############################################################################################
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
maxyi<-2  # 2 set maximum yi, will remove everything larger
###############################################

# warming only --------------------------------------------------------

###############################################
#   !!!!!!!!!!!!!   change for each stat test   !!!!!!!!!!!!!!!!!!!!!!!!!!
# make new columns for target reesponse
##  use positive AE- better after Shannon plotted these values !!!!!!
###   but norm qq plot better when original data  !!!!!!!!
#sdat$yi<-sdat$hedgesG  #  class(sdat$yi) was...sdat$AE_9eV_temp    names(sdat)  hedgesG and hedgesG_variation

sdat$yi<-sdat$temp_AE
 sdat$vi<-sdat$temp_AE_variation
#sdat$vi<-  sdat$hedgesG_variation
target<-"warming"  # unique(sdat$Stressors.studied_study)---  acidification both warming
minchange<-1  # set minimum change between control and exp

sdat2<- sdat %>% 
  filter(Stressors.studied_study==target & complete.cases(Stressors.studied_study)) %>% 
  filter(complete.cases(yi))  %>% 
  filter(!is.infinite(yi)) %>% 
  filter(temp_change > minchange)  %>%    # as suggested by Carlos
  filter(abs(yi)<= maxyi) %>%  #15?  25 works    hist(sdat$AE_9eV_temp)
  mutate(yit=log(abs(yi)+1)) %>%   # get log transform o yi
  mutate(yi_log=if_else(yi<0,(yit*-1), yit))

#  hist(sdat2$yi)     hist(sdat2$sstanom)
# hist(sdat2$yi_log) 
#  
tsolo<-sdat2  #  or tboth, csolo, cboth

## save iff want
#   write.csv(tsolo, file="/Users/geraldn/Dropbox/Cnidaria meta-data/Prod_2_glob/Data/global_env_data_warming_june2020.csv", row.names = F)

###############################################

# warming catagories  --------------------------------------------------------

###############################################
# variable.label.fix.2    and tsolo
mes<-tsolo %>% 
    group_by(Trait.type) %>% 
    dplyr::summarise(n())

#  sdat3  #  or tboth, csolo, cboth
#############   model    +yrange    summary(lmer.mod)
#  for resp log(yi+2.5) , chl  3.5
#  use symbiont density , chla , photochemical efficiency , survival ,  respiration ,   primary productivity  
mdat<-tsolo %>%   # names(tsolo)
  #filter(abs(yi)<= 2) %>%  # 5 , 2 , 1, 0.5
   filter(Trait.type=="symbiont density")  %>% 
  mutate(sPresent.Surface.Temperature.Range=scale(Present.Surface.Temperature.Range),
       syrange=scale(yrange),
       sPresent.Surface.Temperature.Lt.max=scale(Present.Surface.Temperature.Lt.max),
       scoral_region_coral_richness=scale(coral_region_coral_richness), 
       ssstanom=scale(sstanom),
       smsec_humanpop_50km=scale(msec_humanpop_50km),  # hist(sdat$msec_humanpop_50km)
       svel_linear_change=scale(vel_linear_change)) %>% 
       dplyr::select(yi_log,vi, Trait.type,Species, sPresent.Surface.Temperature.Range, syrange,sPresent.Surface.Temperature.Lt.max
                ,scoral_region_coral_richness ,ssstanom,smsec_humanpop_50km,svel_linear_change
                ,PROVINCE,Reference.ID, Genus) %>% 
        drop_na()

## summary table
  mes<-mdat %>% 
      group_by(Trait.type) %>% 
      summarise(n=n(), studies=n_distinct(Reference.ID))

    length(unique(mdat$Reference.ID))
    length(unique(mdat$Species))

  #  hist(mdat$yi)    summary(mdat$yi)   hist(log(mdat$yi+15))
#      mdat<-mdat[log(mdat$yi+15),]
lmer.mod<-lmer(yi_log~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness 
               +ssstanom
               +smsec_humanpop_50km
               +svel_linear_change
           
               + (1|PROVINCE/Reference.ID)+(1|Genus)  # PROVINCE/
               #+ (1|temp_change)
               , weights=1/vi ,  data=mdat)
summary(lmer.mod)   
MuMIn::r.squaredGLMM(lmer.mod) 
#  interactions - no inter. significant    hist(mdat$temp_change)   

lmer.mod<-lmer(yi_log~(sPresent.Surface.Temperature.Range
                   +syrange
                   +sPresent.Surface.Temperature.Lt.max
                   +scoral_region_coral_richness 
                   +ssstanom
                   +smsec_humanpop_50km
                   +svel_linear_change)^2
               + (1|PROVINCE/Reference.ID)
               +(1|Genus)
               #+ (1|temp_change)
               , weights=1/vi , data=mdat, REML=F, na.action = na.fail)

## https://sites.google.com/site/rforfishandwildlifegrads/home/mumin_usage_examples
dd<- MuMIn::dredge(lmer.mod, m.lim = c(7, 9), rank = BIC, subset = (sPresent.Surface.Temperature.Range && syrange && sPresent.Surface.Temperature.Lt.max
                                       && scoral_region_coral_richness  && ssstanom && smsec_humanpop_50km && svel_linear_change ))  # MuMIn::getAllTerms(lmer.mod)
dd2<-subset(dd, delta < 5) # get best < 5 , == 0
#### clean table
sel.table<-as.data.frame(dd2)
## compu to clip board
clip <- pipe("pbcopy", "w")                       
write.table(sel.table, file=clip, sep = '\t', row.names = FALSE)                               
close(clip)



lmer.mod<-lmer(yi_log~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness 
               +ssstanom
               +smsec_humanpop_50km
               +svel_linear_change
               
            # +  sPresent.Surface.Temperature.Lt.max:ssstanom # all 1
           #  +  scoral_region_coral_richness:svel_linear_change # all 2
           #+    sPresent.Surface.Temperature.Range:svel_linear_change # resp 5 and 1, all 5
          # +   ssstanom:syrange  # resp 5
         #      +      smsec_humanpop_50km:sPresent.Surface.Temperature.Lt.max  # pam 2
         # +     smsec_humanpop_50km:sPresent.Surface.Temperature.Range # sym y<2
       # + sPresent.Surface.Temperature.Lt.max:sPresent.Surface.Temperature.Range # chla 5, 2,1 and all 5
          #+  sPresent.Surface.Temperature.Range:ssstanom  # for sym y<1 and pam
        # + sPresent.Surface.Temperature.Range:syrange # survival 5, 2
          #+    scoral_region_coral_richness:syrange # for sym y<0.5 & 0.25  pam 0.5
               + (1|PROVINCE/Reference.ID)+(1|Genus) 
               #+ (1|temp_change), 
              , weights=1/vi , data=mdat)
summary(lmer.mod)   #  AIC no interactions 2290 2287
AIC(lmer.mod)
# piecewiseSEM::sem.model.fits(lmer.mod)   library(MuMIn)
MuMIn::r.squaredGLMM(lmer.mod)  # m is variance explained by predictors, c is both predictors and random factors

plot(lmer.mod)
q<-qqnorm(resid(lmer.mod))
qqline(resid(lmer.mod))
########################################################
#######    for predict    
#   use----       chla    photochemical efficiency  ;  
mdat<-tsolo %>% 
  filter(Trait.type=="chla")  %>% 
  mutate(sPresent.Surface.Temperature.Range=scale(Present.Surface.Temperature.Range),
         syrange=scale(yrange),
         sPresent.Surface.Temperature.Lt.max=scale(Present.Surface.Temperature.Lt.max),
         scoral_region_coral_richness=scale(coral_region_coral_richness), 
         ssstanom=scale(sstanom),
         smsec_humanpop_50km=scale(msec_humanpop_50km),  # hist(sdat$msec_humanpop_50km)
         svel_linear_change=scale(vel_linear_change)) %>% 
         mutate(log_temp_change=log(temp_change)) 


# median(mdat$syrange, na.rm=T)


lmer.mod<-lmer(yi_log~sPresent.Surface.Temperature.Range
               +syrange
               +sPresent.Surface.Temperature.Lt.max
               +scoral_region_coral_richness 
               +ssstanom
               +smsec_humanpop_50km
               +svel_linear_change
               
              + sPresent.Surface.Temperature.Range:sPresent.Surface.Temperature.Lt.max # chl
             #  + sPresent.Surface.Temperature.Lt.max:smsec_humanpop_50km# pam
           
               
               + (1|PROVINCE/Reference.ID)+(1|Genus)  # PROVINCE/
                #   hist(log(mdat$temp_change)) summary(mdat$log_temp_change)
               , weights=1/vi , data=mdat)
summary(lmer.mod)   
MuMIn::r.squaredGLMM(lmer.mod) 

### get median syrange from model input to then use in model for global predict
c_dat2<-c_dat2  %>% 
mutate(syrange=median(mdat$syrange, na.rm=T) )

p<-predict(lmer.mod, newdata = c_dat2, newparams = NULL,
           re.form = NA, random.only=FALSE, terms = NULL
)  #     ,type = c("link", "response") - same  hist(p)


head(p)
psych::describe(p)    # link mean 2.44 sd1.7   when response - 
psych::describe(mdat$yi)   # mean 1.78, sd 2.79
hist(p)

c_dat2$chla_predict<-p
#  or
#c_dat2$survival_predict<-p
#  or
c_dat2$pam_predict<-p

## save
data.table::fwrite(c_dat2, file = '/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export/Global_CR_matchinfo_predict_warming_cat2.csv', row.names=F, sep=",")



