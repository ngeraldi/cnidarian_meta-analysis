####################################################################################################
###   this script is to compile and tidy data from ISI search
###    save all search results into folder using ISI quick download
###   this option only downloads 500 citations at a time, so this need to be combined
#############################################
### basic steps after downloading citations from ISI
## 1- set working directory to where data are
## run code -- shoudl check becuase ISI does change format occasionally
## set folder to output data -- line 49

library(plyr)
library(dplyr)
library(RPMG)

setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/refs")
l<-list.files(path = ".")
for (i in 1:length(l)){
  #i<-1
  x<-read.delim(l[i],fileEncoding="UCS-2LE",row.names = NULL,quote="")
  if (i==1){
    data<-x
  }
  else{
    data<-rbind.fill(data,x)
  }
}

m1<-names(data)## need to correct colnames
m1<-m1[-1]
data<-data[1:length(row.names(data)),c(1:(length(names(data))-1))]  # -1 from names
names(data)<-m1
# refine and rename columns
z<-c("AU","TI","SO","VL","BP","EP","DI","PY","CT","TC","SN")
cn<-c("Authors","Title","Source name","Volume","First page","End page","DOI","Year","Conference_title","Times_cited","SN")
dat<-data[,z]###keep only columns in z
colnames(dat)<-cn####rename columns to cn

##remove conference presentations
dat1<-dat[dat$Conference_title=="",]

dat1$original_refnum<-rownames(dat1)##add column for reference number
dat2<-cbind(dat1[12],dat1[1:11])#reorder columns put ref number first
###randomize rows
dat3 <- dat2[sample(nrow(dat2)),]##randomize rows
dat3$random_refnum<-c(1:length(dat3$original_refnum))
dat4<-cbind(dat3[13],dat3[1:12])##put random number first
rownames(dat4) <- NULL#remove fake

#  export tabel
#    setwd("/Users/geraldn/Dropbox/Documents/KAUST/global change meta/R/export")
#     write.table(dat4,"coral_refs_fin.csv", sep=",",row.names=F)

### mess
apery<-aggregate(Times_cited~Year*system ,FUN="length",data=dat2)##articles per year
hist(dat4$Year)

