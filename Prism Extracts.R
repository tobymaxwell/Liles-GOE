library(raster)
library(sp)
library(rgdal)
library(rgeos)
Poly<-readOGR("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Round 3/Shapes/ER.kml")
Poly<-readOGR("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Round 3/Shapes/WH.kml")
Poly<-readOGR("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Round 3/Shapes/FF.kml")
plot(Poly)
setwd("/Users/tobymaxwell/Downloads/PRISM_vpdmax_stable_4kmM1_198101_201810_bil/")
list<-list.files(pattern="bil.bil*")
list
list<-list[c(TRUE,FALSE)]
list
rasterlist<-NULL
results<-NULL

for(i in list){
  temp<-readGDAL(i)
  temp.raster<-raster(temp)
  assign(paste0("MAT.", data.frame(strsplit(i,"_"))[5,]), temp.raster)
  rasterlist<-c(rasterlist,paste0("MAT.", data.frame(strsplit(i,"_"))[5,]))
  results<-rbind(results, extract(temp.raster, Poly))
}
results.df <- data.frame(matrix(unlist(results), nrow=length(results), byrow=T))
results.df<-data.frame(results.df[1:444,])
months<-(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
monthrep<-c(rep(months,37))
monthrep
years<-rep(1981:2017, each = 12)
years
rows<-paste0(monthrep,years)
str(results.df)
rownames(results.df)<-rows
colnames(results.df)<-Poly$Name
results.df$Year<-as.vector(years)
results.df$Month<-as.vector(monthrep)
tmean.df<-results.df
head(tmean.df)
write.csv(tmean.df, "/Users/tobymaxwell/Desktop/tmean.csv")

Temp<-read.csv("/Users/tobymaxwell/Desktop/tmean.csv")
str(Temp)
library(dplyr)
Temp<-Temp[-1]
Temp$Year<-as.numeric(Temp$Year)
MAT<-Temp[-3]%>%
group_by(Year) %>% 
  summarise_all(funs(mean))
MAT<-data.frame(MAT[MAT$Year>1985&MAT$Year<2009,])
MAT
library(reshape2)
MAT.long<-reshape(MAT, 
                         idvar="Year", ids = "Year",
                         times=names(MAT[-1]), timevar = "Site",
                         varying=list(names(MAT[-1])), v.names="MAT", 
                         direction = "long")
library(ggplot2)
ggplot(MAT.long, aes(y=MAT, x=Year))+geom_line()
write.csv(MAT.long, "/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Round 3/WHVPD.csv")

MATnormal<-raster("/Users/tobymaxwell/Downloads/PRISM_tmean_30yr_normal_800mM2_annual_bil/PRISM_tmean_30yr_normal_800mM2_annual_bil.bil")
ERMAT<-extract(MATnormal, ER)
WHMAT<-extract(MATnormal, WH)
FFMAT<-extract(MATnormal, FF)
ERMAT
WHMAT
FFMAT

MAPnormal<-raster("/Users/tobymaxwell/Downloads/PRISM_ppt_30yr_normal_800mM2_annual_bil/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")
ERmap<-extract(MAPnormal, ER)
WHmap<-extract(MAPnormal, WH)
FFmap<-extract(MAPnormal, FF)
ERmap
WHmap
FFmap

VPDnormal<-raster("/Users/tobymaxwell/Downloads/PRISM_vpdmax_30yr_normal_800mM2_annual_bil/PRISM_vpdmax_30yr_normal_800mM2_annual_bil.bil")
ERVPD<-extract(VPDnormal, ER)
WHVPD<-extract(VPDnormal, WH)
FFVPD<-extract(VPDnormal, FF)
ERVPD
WHVPD
FFVPD

MAT<-read.csv("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Round 3/MAT.csv")
MAT$Site<-factor(MAT$Site, levels=c("ER", "WH", "FF"))
setwd("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Round 3")
png("100kHighRes300dpi.png", units="px", width=3200, height=1600, res=600)
ggplot(MAT, aes(y=MAT, x=Year, shape=Site))+geom_line(lty='dotted')+
  geom_point(size = 2)+
  theme_bw()+
  scale_shape_manual(values=c(0,1,2))+
  theme(legend.position="bottom")
dev.off()

ddply(MAT, .(Site), summarise,
      MAT=mean(MAT))

MAP<-read.csv("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Round 3/MAP.csv")
MAP$Site<-factor(MAP$Site, levels=c("ER", "WH", "FF"))
png("MAP", units="px", width=3200, height=1600, res=600)
ggplot(MAP, aes(y=MAP, x=Year, shape=Site))+geom_line(lty='dotted')+
  geom_point(size = 2)+
  theme_bw()+
  scale_shape_manual(values=c(0,1,2))+
  theme(legend.position="bottom")
dev.off()
ddply(MAP, .(Site), summarise,
      MAP=mean(MAP))

VPD<-read.csv("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Round 3/VPD.csv")
VPD$Site<-factor(VPD$Site, levels=c("ER", "WH", "FF"))
png("VPD", units="px", width=3200, height=1600, res=600)
ggplot(VPD, aes(y=VPD, x=Year, shape=Site))+geom_line(lty='dotted')+
  geom_point(size = 2)+
  theme_bw()+
  scale_shape_manual(values=c(0,1,2))+
  theme(legend.position="bottom")
dev.off()
ddply(MAP, .(Site), summarise,
      MAP=mean(MAP))
ddply(VPD, .(Site), summarise,
      VPD=mean(VPD))
###############  MAP  ##################
setwd("/Users/Maxwell/Documents/geospatial/PRISM_ppt_stable_4kmM3_198101_201807_bil/")
list<-list.files(pattern="bil.bil*")
list
list<-list[c(TRUE,FALSE)]
list
rasterlist<-NULL
results<-NULL

for(i in list){
  ppt<-readGDAL(i)
  ppt.raster<-raster(temp)
  assign(paste0("MAP.", data.frame(strsplit(i,"_"))[5,]), temp.raster)
  rasterlist<-c(rasterlist,paste0("MAP.", data.frame(strsplit(i,"_"))[5,]))
  results<-rbind(results, extract(ppt.raster, Poly))
}

plot(temp.raster)
data.frame(results)
results<-results[1:444,]
months<-(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
monthrep<-c(rep(months,37))
monthrep
years<-rep(1981:2017, each = 12)
years
rows<-paste0(monthrep,years)
results.df<-data.frame(results)
rownames(results.df)<-rows
colnames(results.df)<-Poly$Name
results.df$Year<-as.vector(years)
results.df$Month<-as.vector(monthrep)
ppt.df<-results.df
df <- data.frame(matrix(unlist(ppt.df), nrow=444, byrow=F),stringsAsFactors=FALSE)
head(results.df)
head(df)
str(YSF)
df[18]<-df[20]
df<-df[-24]
df<-df[-19]
df<-df[-15]
colnames(df)<-c(as.character(Poly$Name), "Year", "Month")
df$ESB<-df$ESF
df$MNF<-df$MNB
head(df)
str(df)
write.csv(df, "/Users/Maxwell/Desktop/df.csv")
ppt<-read.csv("/Users/Maxwell/Desktop/df.csv")
str(ppt)
ppt<-ppt[-1]
MAP<-ppt[-30] %>%
  group_by(Year) %>% 
  summarise_all(funs(sum))
MAP<-data.frame(MAP)
MAP
library(reshape2)
MAP.long<-reshape(MAP, 
                  idvar="Year", ids = "Year",
                  times=names(MAP[-1]), timevar = "Site",
                  varying=list(names(MAP[-1])), v.names="MAP", 
                  direction = "long")

ggplot(MAP.long, aes(y=MAP, x=Year, col=Location))+geom_line()
write.csv(MAP, "/Users/Maxwell/OneDrive - University Of Oregon/Oregon/Nat Geo/Data/climate.csv")

MAP.long
climate<-merge(MAP.long, MAT.long)
write.csv(climate, "/Users/Maxwell/OneDrive - University Of Oregon/Oregon/Nat Geo/Data/climate.csv")
