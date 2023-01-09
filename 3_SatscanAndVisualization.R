library(smacpod)
library(spdep)
library(rsatscan)
library(rgdal)
library(spatstat)
library(maptools)
library(plyr)
library(ggplot2)
library(tigris)
library(sqldf)
library(grid)
library(mapproj)
library(geosphere)
library(spatialEco)
library(cowplot)
library(sp)
library(raster)
setwd("/home/vmuser/Desktop/PregnancyCVH")

#########
#Overall#
#########
#Load all the data on generated rates
dat1<-read.csv("outputs/predicted_overall.csv") #overall predicted 

#########################################
#Identify poor CVH clusters using SatScan
us<-states()
us<-spTransform(us,CRS("+init=epsg:4326"))
usdata<-us@data
usdata$STATEFP<-as.numeric(usdata$STATEFP)

q<-"select usdata.*, dat1.pop, dat1.cas
    from usdata 
    left outer join dat1 on usdata.STATEFP=dat1.state;"
usdata<-sqldf(q)
usdata<-usdata[usdata$STATEFP %in% dat1$state,]
us@data<-usdata
summary(usdata$cvh)
uscas<-data.frame(us@data[,c("STATEFP","cas")])
uspop<-data.frame(us@data[,c("STATEFP","pop")])
uspop$year<-2017
uspop<-uspop[,c(1,3,2)]
uscrd<-data.frame(us@data[,c("STATEFP","INTPTLAT","INTPTLON")])

td = tempdir()
write.cas(uscas, td, "uscas")
write.pop(uspop, td, "uspop")
write.geo(uscrd, td, "usgeo")

invisible(ss.options(reset=T))
ss.options(list(CaseFile="uscas.cas",PopulationFile="uspop.pop"))
ss.options(list(PrecisionCaseTimes=0,StartDate="2011/01/01",EndDate="2017/12/31"))
ss.options(list(CoordinatesFile="usgeo.geo",CoordinatesType=1,AnalysisType=1))
ss.options(list(ScanAreas=2))
ss.options(list(TimeAggregationUnits=0))
ss.options(list(ReportGiniClusters="n",LogRunToHistoryFile="n"))
write.ss.prm(td,"us")

head(ss.options(),3)

usss <- satscan(td, "us",sslocation = "/usr/local/SaTScan",ssbatchfilename = "SaTScanBatch64")
summary(usss)

#boundary of clusters
cluster<-usss$shapeclust
cluster<-spTransform(cluster,CRS("+init=epsg:4326"))
cluster@data
#mapping
#fortify clusters
us1<-states()
us1<-spTransform(us1,CRS("+init=epsg:4326"))
cd<-SpatialPointsDataFrame(coordinates(us1),data = us1@data)
proj4string(cd)<-CRS("+init=epsg:4326")

cl<-point.in.poly(cd,cluster)
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 2),"GEOID"]),]
us1_3<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 3),"GEOID"]),]
us1_4<-us1[us1$GEOID %in% 36,]
us1_5<-us1[us1$GEOID %in% 17,]
shape1<-aggregate(us1_1,dissolve=T)
shape2<-aggregate(us1_2,dissolve=T)
shape3<-aggregate(us1_3,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(shape2,region="GEOID")
us1_f3<-fortify(shape3,region="GEOID")
us1_f4<-fortify(us1_4,region="GEOID")
us1_f5<-fortify(us1_5,region="GEOID")

#fortify rates
us2<-states()
us2<-spTransform(us2,CRS("+init=epsg:4326"))
usdata<-us2@data
usdata$STATEFP<-as.numeric(usdata$STATEFP)

q<-"select * from usdata
    left outer join dat1 on usdata.STATEFP=dat1.state;"
usdata<-sqldf(q)

us2@data<-usdata
us_f<-fortify(us2,region = "STATEFP")
us_data<-us2@data
us_f<-merge(us_f,us_data[,c("STATEFP","level1")],by.x="id",by.y="STATEFP",all.x=T)

us_f1<-us_f[us_f$id %in% dat1$state & !(us_f$id %in% c(2,15)),]
us_f2<-us_f[us_f$id %in% 2,] #Alaska
us_f3<-us_f[us_f$id %in% 15,] #Hawaii

map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level1)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "bottom", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="Ideal CVH (%)",palette ="Blues",labels = c("> 5.37","5.17-5.37","4.91-5.17","4.66-4.91","\U2264 4.66"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=1)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=2)+geom_path(data = us1_f3,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=3)+geom_path(data = us1_f4,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=4)+geom_path(data = us1_f5,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=5)+scale_alpha(guide="none")

map2<-ggplot()+geom_polygon(data=us_f2,aes(long,lat,group=group,fill=factor(level1)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Blues")

map3<-ggplot()+geom_polygon(data=us_f3,aes(long,lat,group=group,fill=factor(level1)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Blues")

#Put maps together
grid.newpage()
vp<-viewport(width = 1, height = 1)
subvp1<-viewport(width = 0.35, height = 0.35, x= 0.20, y=0.27)
subvp2<-viewport(width = 0.25, height = 0.25, x= 0.22, y=0.35)
print(map1,vp=vp)
print(map2,vp=subvp1)
print(map3,vp=subvp2)

#######################
#low CVH score clusters
us<-states()
us<-spTransform(us,CRS("+init=epsg:4326"))
usdata<-us@data
usdata$STATEFP<-as.numeric(usdata$STATEFP)

q<-"select *
    from usdata 
    left outer join dat1 on usdata.STATEFP=dat1.state;"

usdata<-sqldf(q)
usdata<-usdata[usdata$STATEFP %in% dat1$state,]
us@data<-usdata

uscas<-data.frame(us@data[,c("STATEFP","n","avg_score")])
uscrd<-data.frame(us@data[,c("STATEFP","INTPTLAT","INTPTLON")])

td = tempdir()
write.cas(uscas, td, "uscas")
write.geo(uscrd, td, "usgeo")

invisible(ss.options(reset=T))
ss.options(list(CaseFile="uscas.cas"))
ss.options(list(PrecisionCaseTimes=0,StartDate="2011/01/01",EndDate="2017/12/31"))
ss.options(list(CoordinatesFile="usgeo.geo",CoordinatesType=1,AnalysisType=1,ModelType=5))
ss.options(list(ScanAreas=2))
ss.options(list(TimeAggregationUnits=0))
ss.options(list(ReportGiniClusters="n",LogRunToHistoryFile="n"))
write.ss.prm(td,"us")

head(ss.options(),3)

usss <- satscan(td, "us",sslocation = "/usr/local/SaTScan",ssbatchfilename = "SaTScanBatch64")
summary(usss)

#boundary of clusters
cluster<-usss$shapeclust
cluster<-spTransform(cluster,CRS("+init=epsg:4326"))
cluster@data

#mapping
#fortify clusters
us1<-states()
us1<-spTransform(us1,CRS("+init=epsg:4326"))
cd<-SpatialPointsDataFrame(coordinates(us1),data = us1@data)
proj4string(cd)<-CRS("+init=epsg:4326")

cl<-point.in.poly(cd,cluster)
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% 32,] #Nevada
us1_3<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 3),"GEOID"]),]
shape1<-aggregate(us1_1,dissolve=T)
shape3<-aggregate(us1_3,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(us1_2,region="GEOID")
us1_f3<-fortify(shape3,region="GEOID")

#fortify scores
us2<-states()
us2<-spTransform(us2,CRS("+init=epsg:4326"))
usdata<-us2@data
usdata$STATEFP<-as.numeric(usdata$STATEFP)

q<-"select * from usdata
    left outer join dat1 on usdata.STATEFP=dat1.state;"
usdata<-sqldf(q)

us2@data<-usdata
us_f<-fortify(us2,region = "STATEFP")
us_data<-us2@data
us_f<-merge(us_f,us_data[,c("STATEFP","level2")],by.x="id",by.y="STATEFP",all.x=T)

us_f1<-us_f[us_f$id %in% dat1$state & !(us_f$id %in% c(2,15)),]
us_f2<-us_f[us_f$id %in% 2,] #Alaska
us_f3<-us_f[us_f$id %in% 15,] #Hawaii


map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level2)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "bottom", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="CVH Score",palette ="Purples",labels=c("> 4.59","4.57-4.59","4.53-4.57","4.50-4.53","\u2264 4.50"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",linetype=1,size=0.8)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",linetype=2,size=0.8)+geom_path(data = us1_f3,aes(x=long,y=lat,group=group),color="red",linetype=3,size=0.8)+scale_alpha(guide="none")

map2<-ggplot()+geom_polygon(data=us_f2,aes(long,lat,group=group,fill=factor(level2)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Purples")

map3<-ggplot()+geom_polygon(data=us_f3,aes(long,lat,group=group,fill=factor(level2)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Purples")

#Put maps together
grid.newpage()
vp<-viewport(width = 1, height = 1)
subvp1<-viewport(width = 0.35, height = 0.35, x= 0.20, y=0.27)
subvp2<-viewport(width = 0.25, height = 0.25, x= 0.22, y=0.35)
print(map1,vp=vp)
print(map2,vp=subvp1)
print(map3,vp=subvp2)

##################################################################################
#                                     By year                                    #
##################################################################################
pred_year<-read.csv("outputs/predicted_year.csv") #predicted by year
i<-2011
dat1<-pred_year[pred_year$year %in% i,]
#dat1<-dat1[!is.na(dat1$level1),] #for 2019

#########################################
#Identify poor CVH clusters using SatScan
us<-states()
us<-spTransform(us,CRS("+init=epsg:4326"))
usdata<-us@data
usdata$STATEFP<-as.numeric(usdata$STATEFP)

q<-"select usdata.*, dat1.pop, dat1.cas
    from usdata 
    left outer join dat1 on usdata.STATEFP=dat1.state;"
usdata<-sqldf(q)
usdata<-usdata[usdata$STATEFP %in% dat1$state,]
us@data<-usdata

uscas<-data.frame(us@data[,c("STATEFP","cas")])
uspop<-data.frame(us@data[,c("STATEFP","pop")])
uspop$year<-i
uspop<-uspop[,c(1,3,2)]
uscrd<-data.frame(us@data[,c("STATEFP","INTPTLAT","INTPTLON")])

td = tempdir()
write.cas(uscas, td, "uscas")
write.pop(uspop, td, "uspop")
write.geo(uscrd, td, "usgeo")

invisible(ss.options(reset=T))
ss.options(list(CaseFile="uscas.cas",PopulationFile="uspop.pop"))
ss.options(list(PrecisionCaseTimes=0,StartDate="2011/01/01",EndDate="2011/12/31"))
ss.options(list(CoordinatesFile="usgeo.geo",CoordinatesType=1,AnalysisType=1))
ss.options(list(ScanAreas=2))
ss.options(list(TimeAggregationUnits=0))
ss.options(list(ReportGiniClusters="n",LogRunToHistoryFile="n"))
write.ss.prm(td,"us")

head(ss.options(),3)

usss <- satscan(td, "us",sslocation = "/usr/local/SaTScan",ssbatchfilename = "SaTScanBatch64")
summary(usss)

#boundary of clusters
cluster<-usss$shapeclust
cluster<-spTransform(cluster,CRS("+init=epsg:4326"))
cluster@data

#mapping
#fortify clusters
us1<-states()
us1<-spTransform(us1,CRS("+init=epsg:4326"))
cd<-SpatialPointsDataFrame(coordinates(us1),data = us1@data)
proj4string(cd)<-CRS("+init=epsg:4326")
cl<-point.in.poly(cd,cluster)

#2011,2013,2015,2017,2019
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 2),"GEOID"]),]
us1_3<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 3),"GEOID"]),]
shape1<-aggregate(us1_1,dissolve=T)
shape2<-aggregate(us1_2,dissolve=T)
shape3<-aggregate(us1_3,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(shape2,region="GEOID")
us1_f3<-fortify(shape3,region="GEOID")

#fortify rates
us2<-states()
us2<-spTransform(us2,CRS("+init=epsg:4326"))
usdata<-us2@data
usdata$STATEFP<-as.numeric(usdata$STATEFP)

q<-"select * from usdata
    left outer join dat1 on usdata.STATEFP=dat1.state;"
usdata<-sqldf(q)

us2@data<-usdata
us_f<-fortify(us2,region = "STATEFP")
us_data<-us2@data
us_f<-merge(us_f,us_data[,c("STATEFP","level1")],by.x="id",by.y="STATEFP",all.x=T)

us_f1<-us_f[us_f$id %in% dat1$state & !(us_f$id %in% c(2,15)),]
us_f2<-us_f[us_f$id %in% 2,] #Alaska
us_f3<-us_f[us_f$id %in% 15,] #Hawaii

map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level1)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "right", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="Ideal CVH (%)",palette ="Blues",labels = c("> 5.37","5.17-5.37","4.91-5.17","4.66-4.91","\U2264 4.66"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=1)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=2)+geom_path(data = us1_f3,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=3)+scale_alpha(guide="none")

map2<-ggplot()+geom_polygon(data=us_f2,aes(long,lat,group=group,fill=factor(level1)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Blues")

map3<-ggplot()+geom_polygon(data=us_f3,aes(long,lat,group=group,fill=factor(level1)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Blues")
#Put maps together
grid.newpage()
vp<-viewport(width = 1, height = 1)
subvp1<-viewport(width = 0.35, height = 0.35, x= 0.20, y=0.27)
subvp2<-viewport(width = 0.25, height = 0.25, x= 0.22, y=0.35)
print(map1,vp=vp)
print(map2,vp=subvp1)
print(map3,vp=subvp2)

##########################################
#Identify low CVH score clusters using SatScan
us<-states()
us<-spTransform(us,CRS("+init=epsg:4326"))
usdata<-us@data
usdata$STATEFP<-as.numeric(usdata$STATEFP)

q<-"select *
    from usdata 
    left outer join dat1 on usdata.STATEFP=dat1.state;"
usdata<-sqldf(q)
usdata<-usdata[usdata$STATEFP %in% dat1$state,]
us@data<-usdata

uscas<-data.frame(us@data[,c("STATEFP","n","avg_score")])
uscrd<-data.frame(us@data[,c("STATEFP","INTPTLAT","INTPTLON")])

td = tempdir()
write.cas(uscas, td, "uscas")
write.geo(uscrd, td, "usgeo")

invisible(ss.options(reset=T))
ss.options(list(CaseFile="uscas.cas"))
ss.options(list(PrecisionCaseTimes=0,StartDate="2011/01/01",EndDate="2011/12/31"))
ss.options(list(CoordinatesFile="usgeo.geo",CoordinatesType=1,AnalysisType=1,ModelType=5))
ss.options(list(ScanAreas=2))
ss.options(list(TimeAggregationUnits=0))
ss.options(list(ReportGiniClusters="n",LogRunToHistoryFile="n"))
write.ss.prm(td,"us")

head(ss.options(),3)

usss <- satscan(td, "us",sslocation = "/usr/local/SaTScan",ssbatchfilename = "SaTScanBatch64")
summary(usss)

#boundary of clusters
cluster<-usss$shapeclust
cluster<-spTransform(cluster,CRS("+init=epsg:4326"))
cluster@data

#mapping
#fortify clusters
us1<-states()
us1<-spTransform(us1,CRS("+init=epsg:4326"))
cd<-SpatialPointsDataFrame(coordinates(us1),data = us1@data)
proj4string(cd)<-CRS("+init=epsg:4326")
cl<-point.in.poly(cd,cluster)

#2011
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% 32,]
us1_3<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 3),"GEOID"]),]
shape1<-aggregate(us1_1,dissolve=T)
shape3<-aggregate(us1_3,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(us1_2,region="GEOID")
us1_f3<-fortify(shape3,region="GEOID")

#2013
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 2),"GEOID"]),]
us1_3<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 3),"GEOID"]),]
us1_4<-us1[us1$GEOID %in% 32,]
shape1<-aggregate(us1_1,dissolve=T)
shape2<-aggregate(us1_2,dissolve=T)
shape3<-aggregate(us1_3,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(shape2,region="GEOID")
us1_f3<-fortify(shape3,region="GEOID")
us1_f4<-fortify(us1_4,region="GEOID")

#2015
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% 32,] #Nevada
shape1<-aggregate(us1_1,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(us1_2,region="GEOID")

#2017
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% c(1)),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% c(2)),"GEOID"]),]
us1_3<-us1[us1$GEOID %in% 32,] #Nevada
shape1<-aggregate(us1_1,dissolve=T)
shape2<-aggregate(us1_2,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(shape2,region="GEOID")
us1_f3<-fortify(us1_3,region="GEOID")

#2019
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% c(1)),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% 32,]#Nevada
us1_3<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% c(3)),"GEOID"]),] 
shape1<-aggregate(us1_1,dissolve=T)
shape3<-aggregate(us1_3,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(us1_2,region="GEOID")
us1_f3<-fortify(shape3,region="GEOID")

#fortify scores
us2<-states()
us2<-spTransform(us2,CRS("+init=epsg:4326"))
usdata<-us2@data
usdata$STATEFP<-as.numeric(usdata$STATEFP)

q<-"select * from usdata
    left outer join dat1 on usdata.STATEFP=dat1.state;"
usdata<-sqldf(q)
us2@data<-usdata
us_f<-fortify(us2,region = "STATEFP")
us_data<-us2@data
us_f<-merge(us_f,us_data[,c("STATEFP","level2")],by.x="id",by.y="STATEFP",all.x=T)

us_f1<-us_f[us_f$id %in% dat1$state & !(us_f$id %in% c(2,15)),]
us_f2<-us_f[us_f$id %in% 2,] #Alaska
us_f3<-us_f[us_f$id %in% 15,] #Hawaii

#2011
map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level2)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "right", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="CVH Score",palette ="Purples",labels=c("> 4.59","4.57-4.59","4.53-4.57","4.50-4.53","\u2264 4.50"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",linetype=1,size=0.8)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",linetype=2,size=0.8)+geom_path(data = us1_f3,aes(x=long,y=lat,group=group),color="red",linetype=3,size=0.8)+scale_alpha(guide="none")

#2013
map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level2)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "right", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="CVH Score",palette ="Purples",labels=c("> 4.59","4.57-4.59","4.53-4.57","4.50-4.53","\u2264 4.50"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",linetype=1,size=0.8)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",linetype=2,size=0.8)+geom_path(data = us1_f3,aes(x=long,y=lat,group=group),color="red",linetype=3,size=0.8)+geom_path(data = us1_f4,aes(x=long,y=lat,group=group),color="red",linetype=4,size=0.8)+scale_alpha(guide="none")

#2015
map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level2)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "right", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="CVH Score",palette ="Purples",labels=c("> 4.59","4.57-4.59","4.53-4.57","4.50-4.53","\u2264 4.50"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",linetype=1,size=0.8)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",linetype=2,size=0.8)+scale_alpha(guide="none")

#2017,2019
map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level2)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "right", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="CVH Score",palette ="Purples",labels=c("> 4.59","4.57-4.59","4.53-4.57","4.50-4.53","\u2264 4.50"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",linetype=1,size=0.8)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",linetype=2,size=0.8)+geom_path(data = us1_f3,aes(x=long,y=lat,group=group),color="red",linetype=3,size=0.8)+scale_alpha(guide="none")

map2<-ggplot()+geom_polygon(data=us_f2,aes(long,lat,group=group,fill=factor(level2)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Purples")

map3<-ggplot()+geom_polygon(data=us_f3,aes(long,lat,group=group,fill=factor(level2)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Purples")

#Put maps together
grid.newpage()
vp<-viewport(width = 1, height = 1)
subvp1<-viewport(width = 0.35, height = 0.35, x= 0.20, y=0.27)
subvp2<-viewport(width = 0.25, height = 0.25, x= 0.22, y=0.35)
print(map1,vp=vp)
print(map2,vp=subvp1)
print(map3,vp=subvp2)






################################
###### Individual metrics ######
################################

dat1<-read.csv("outputs/smoke.csv",header = T, stringsAsFactors = F) #overall predicted 

#########################################
#Identify poor CVH clusters using SatScan
us<-states()
us<-spTransform(us,CRS("+init=epsg:4326"))
usdata<-us@data
usdata$STATEFP<-as.numeric(usdata$STATEFP)

q<-"select usdata.*, dat1.pop, dat1.cas
    from usdata 
    left outer join dat1 on usdata.STATEFP=dat1.state;"
usdata<-sqldf(q)
usdata<-usdata[usdata$STATEFP %in% dat1$state,]
us@data<-usdata

uscas<-data.frame(us@data[,c("STATEFP","cas")])
uspop<-data.frame(us@data[,c("STATEFP","pop")])
uspop$year<-2017
uspop<-uspop[,c(1,3,2)]
uscrd<-data.frame(us@data[,c("STATEFP","INTPTLAT","INTPTLON")])

td = tempdir()
write.cas(uscas, td, "uscas")
write.pop(uspop, td, "uspop")
write.geo(uscrd, td, "usgeo")

invisible(ss.options(reset=T))
ss.options(list(CaseFile="uscas.cas",PopulationFile="uspop.pop"))
ss.options(list(PrecisionCaseTimes=0,StartDate="2011/01/01",EndDate="2017/12/31"))
ss.options(list(CoordinatesFile="usgeo.geo",CoordinatesType=1,AnalysisType=1))
ss.options(list(ScanAreas=2))
ss.options(list(TimeAggregationUnits=0))
ss.options(list(ReportGiniClusters="n",LogRunToHistoryFile="n"))
write.ss.prm(td,"us")

head(ss.options(),3)

usss <- satscan(td, "us",sslocation = "/usr/local/SaTScan",ssbatchfilename = "SaTScanBatch64")
summary(usss)

#boundary of clusters
cluster<-usss$shapeclust
cluster<-spTransform(cluster,CRS("+init=epsg:4326"))
cluster@data

#mapping
#fortify clusters
us1<-states()
us1<-spTransform(us1,CRS("+init=epsg:4326"))
cd<-SpatialPointsDataFrame(coordinates(us1),data = us1@data)
proj4string(cd)<-CRS("+init=epsg:4326")
cl<-point.in.poly(cd,cluster)

#hp
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1),"GEOID"]),]
shape1<-aggregate(us1_1,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")

#diab
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1 ),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% 12,]
us1_3<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 3 ),"GEOID"]),]
us1_4<-us1[us1$GEOID %in% 17,]
us1_5<-us1[us1$GEOID %in% 36,]
shape1<-aggregate(us1_1,dissolve=T)
shape3<-aggregate(us1_3,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(us1_2,region="GEOID")
us1_f3<-fortify(shape3,region="GEOID")
us1_f4<-fortify(us1_4,region="GEOID")
us1_f5<-fortify(us1_5,region="GEOID")

#chole
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1 ),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% "12",]
us1_3<-us1[us1$GEOID %in% "34",]
us1_4<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 4 ),"GEOID"]),]
us1_5<-us1[us1$GEOID %in% "09",]
shape1<-aggregate(us1_1,dissolve=T)
shape4<-aggregate(us1_4,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(us1_2,region="GEOID")
us1_f3<-fortify(us1_3,region="GEOID")
us1_f4<-fortify(shape4,region="GEOID")
us1_f5<-fortify(us1_5,region="GEOID")

#smoke
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1 ),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 2 ),"GEOID"]),]
us1_3<-us1[us1$GEOID %in% "49",]
shape1<-aggregate(us1_1,dissolve=T)
shape2<-aggregate(us1_2,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(shape2,region="GEOID")
us1_f3<-fortify(us1_3,region="GEOID")

#bmi
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1 ),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 2 ),"GEOID"]),]
us1_3<-us1[us1$GEOID %in% "17",]
us1_4<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 4 ),"GEOID"]),]
us1_5<-us1[us1$GEOID %in% "36",]
shape1<-aggregate(us1_1,dissolve=T)
shape2<-aggregate(us1_2,dissolve=T)
shape4<-aggregate(us1_4,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(shape2,region="GEOID")
us1_f3<-fortify(us1_3,region="GEOID")
us1_f4<-fortify(shape4,region="GEOID")
us1_f5<-fortify(us1_5,region="GEOID")

#pa
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1 ),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 2 ),"GEOID"]),]
us1_3<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 3 ),"GEOID"]),]
us1_4<-us1[us1$GEOID %in% "36",]
us1_5<-us1[us1$GEOID %in% "17",]
shape1<-aggregate(us1_1,dissolve=T)
shape2<-aggregate(us1_2,dissolve=T)
shape3<-aggregate(us1_3,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(shape2,region="GEOID")
us1_f3<-fortify(shape3,region="GEOID")
us1_f4<-fortify(us1_4,region="GEOID")
us1_f5<-fortify(us1_5,region="GEOID")

#diet
us1_1<-us1[us1$GEOID %in% (cl@data[(cl$CLUSTER %in% 1 ),"GEOID"]),]
us1_2<-us1[us1$GEOID %in% 49,]
shape1<-aggregate(us1_1,dissolve=T)
us1_f1<-fortify(shape1,region="GEOID")
us1_f2<-fortify(us1_2,region="GEOID")


#fortify rates
us2<-states()
us2<-spTransform(us2,CRS("+init=epsg:4326"))
usdata<-us2@data
usdata$STATEFP<-as.numeric(usdata$STATEFP)

q<-"select * from usdata
    left outer join dat1 on usdata.STATEFP=dat1.state;"
usdata<-sqldf(q)

us2@data<-usdata
us_f<-fortify(us2,region = "STATEFP")
us_data<-us2@data
us_f<-merge(us_f,us_data[,c("STATEFP","level")],by.x="id",by.y="STATEFP",all.x=T)

us_f1<-us_f[us_f$id %in% dat1$state & !(us_f$id %in% c(2,15)),]
us_f2<-us_f[us_f$id %in% 2,] #Alaska
us_f3<-us_f[us_f$id %in% 15,] #Hawaii

#hp
map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "right", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="Age-and race-adjusted\nprevalence of ideal status\n of blood pressure (%)",palette ="YlOrBr",labels = c("> 83.83","83.56-83.83","82.65-83.56","81.73-82.65","\U2264 81.73"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=1)+scale_alpha(guide="none")

map2<-ggplot()+geom_polygon(data=us_f2,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="YlOrBr")

map3<-ggplot()+geom_polygon(data=us_f3,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="YlOrBr")

#glucose
map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "right", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="Age- and race-adjusted\nprevalence of ideal status\n of glucose (%)",palette ="Purples",labels = c("> 92.05","91.79-92.05","91.32-91.79","90.92-91.32","\U2264 90.92"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=1)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=2)+geom_path(data = us1_f3,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=3)+geom_path(data = us1_f4,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=4)+geom_path(data = us1_f5,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=5)+scale_alpha(guide="none")

map2<-ggplot()+geom_polygon(data=us_f2,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Purples")

map3<-ggplot()+geom_polygon(data=us_f3,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Purples")

#cholesterol
map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "right", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="Age- and race-adjusted\nprevalence of ideal status\n of total cholesterol (%)",palette ="YlGnBu",labels = c("> 82.36","82.20-82.36","82.03-82.20","91.87-82.03","\U2264 81.87"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=1)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=2)+geom_path(data = us1_f3,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=3)+geom_path(data = us1_f4,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=4)+geom_path(data = us1_f5,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=5)+scale_alpha(guide="none")

map2<-ggplot()+geom_polygon(data=us_f2,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="YlGnBu")

map3<-ggplot()+geom_polygon(data=us_f3,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="YlGnBu")

#smoking
map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "right", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="Age- and race-adjusted\nprevalence of ideal status\n of smoking (%)",palette ="Greys",labels = c("> 84.00","83.39-84.00","82.76-83.39","82.38-82.76","\U2264 82.38"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=1)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=2)+geom_path(data = us1_f3,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=3)+scale_alpha(guide="none")

map2<-ggplot()+geom_polygon(data=us_f2,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Greys")

map3<-ggplot()+geom_polygon(data=us_f3,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Greys")

#bmi
map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "right", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="Age- and race-adjusted\nprevalence of ideal status\n of BMI (%)",palette ="RdPu",labels = c("> 43.37","42.09-43.37","40.51-42.09","39.13-40.51","\U2264 39.13"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=1)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=2)+geom_path(data = us1_f3,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=3)+geom_path(data = us1_f4,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=4)+geom_path(data = us1_f5,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=5)+scale_alpha(guide="none")

map2<-ggplot()+geom_polygon(data=us_f2,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="RdPu")

map3<-ggplot()+geom_polygon(data=us_f3,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="RdPu")

#physical activity
map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "right", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="Age- and race-adjusted\nprevalence of ideal status\n of physical activity (%)",palette ="Blues",labels = c("> 53.53","52.70-53.53","51.79-52.70","50.85-51.79","\U2264 50.85"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=1)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=2)+geom_path(data = us1_f3,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=3)+geom_path(data = us1_f4,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=4)+geom_path(data = us1_f5,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=5)+scale_alpha(guide="none")

map2<-ggplot()+geom_polygon(data=us_f2,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Blues")

map3<-ggplot()+geom_polygon(data=us_f3,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Blues")

#diet
map1<-ggplot()+geom_polygon(data=us_f1,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank(),legend.position = "right", legend.title = element_text(face = "bold",size=14),legend.text = element_text(size = 10))+scale_fill_brewer(name="Age- and race-adjusted\nprevalence of ideal status\n of diet (%)",palette ="Greens",labels = c("> 21.95","21.74-21.95","21.62-21.74","21.54-21.62","\U2264 21.54"))+geom_path(data = us1_f1,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=1)+geom_path(data = us1_f2,aes(x=long,y=lat,group=group),color="red",size=0.8,linetype=2)+scale_alpha(guide="none")

map2<-ggplot()+geom_polygon(data=us_f2,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Greens")

map3<-ggplot()+geom_polygon(data=us_f3,aes(long,lat,group=group,fill=factor(level)),size=0.5,colour="white")+coord_map(projection = "azequalarea")+theme(panel.background=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),legend.position = "none",panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+scale_fill_brewer(palette ="Greens")

#Put maps together
grid.newpage()
vp<-viewport(width = 1, height = 1)
subvp1<-viewport(width = 0.35, height = 0.35, x= 0.20, y=0.27)
subvp2<-viewport(width = 0.25, height = 0.25, x= 0.22, y=0.35)
print(map1,vp=vp)
print(map2,vp=subvp1)
print(map3,vp=subvp2)
