goeiso<-read.csv("/Users/Maxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/GOE_isotopes.csv")
goeppt<-read.csv("/Users/Maxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/GOE_annual_PT.csv")
str(goeiso)

m1<-lm(d13C~treatment*site, goeiso)
anova(m1)
m2<-lm(d13C~treatment*site + d18O, goeiso)
anova(m2)
goeiso$syt<-paste0(goeiso$site, goeiso$year, goeiso$treatment)
goeiso$sy<-paste0(goeiso$site, goeiso$year)

goe2<-ddply(goeiso[goeiso$treatment=="control",], .(site, year), summarise, 
      meanctrl=mean(d18O),
      meanwue=mean(iWUE),
      meanBA = mean(BA_mm))
goe2$sy<-paste0(goe2$site,goe2$year)
goeiso<-merge(goeiso, goe2)
goeiso2<-merge(goeiso, goeppt[2:8])
library(plyr)
goe3<-ddply(goeiso2, .(site, treatment, year), summarise, 
            d18Onormse = sd(d18O/meanctrl)/length(d18O/meanctrl),
            d18Onorm = mean(d18O/meanctrl),
            d18O = mean(d18O),
            meanctrl = mean(meanctrl),
            d13C=mean(d13C),
            BA=mean(BA_mm),
            BAnorm = mean(BA_mm/meanBA),
            iWUEnormse = sd(iWUE/meanctrl)/length(iWUE/meanctrl),
            iWUE=mean(iWUE),
            iWUEnorm = mean(iWUE/meanwue))

goe4<-merge(goe3, goeppt[2:8])
goe4$st<-paste0(goe4$site, goe4$treatment)

library(ggplot2)
library(agricolae)
tmod<-aov(d18O~st, goe4)
anova(tmod)
tuk<-HSD.test(tmod, trt="st", DFerror=164,MSerror=0.7464, group=T)
tuk<-data.frame(tuk$groups)
tuk<-tuk[order(rownames(tuk)),]
tuk
median<-ddply(goe4, .(site, treatment), summarise, 
               d18Omed<-median(d18O))
tuk$med<-median$..1
              
ggplot(goe4, aes(y=(d18O), x=st, shape = treatment))+geom_boxplot()+theme_classic()+
  annotate("text", label =tuk$groups, y=tuk$med-0.1, x=c(1:12))

tmod<-aov(d18Onorm~st, goe4[goe4$treatment!="control",])
anova(tmod)
tuk<-HSD.test(tmod, trt="st", DFerror=123,MSerror=0.0015392, group=T)
tuk<-data.frame(tuk$groups)
tuk<-tuk[order(rownames(tuk)),]
tuk
median<-ddply(goe4[goe4$treatment!="control",], .(site, treatment), summarise, 
              d18Omed<-median(d18Onorm))
tuk$med<-median$..1

ggplot(goe4[goe4$treatment!="control",], aes(y=(d18Onorm),x=st))+geom_boxplot()+theme_classic()+
  annotate("text", label =tuk$groups, y=tuk$med-0.005, x=c(1:9))

ggplot(goe4[goe4$treatment!="control",], aes(y=(d18Onorm), x=year))+
  geom_line(data=goe4[goe4$treatment!="control",], lty="dotdash")+geom_errorbar(data=goe4[goe4$treatment!="control",], width = 0.2, aes(ymin=d18Onorm-d18Onormse, ymax=d18Onorm+d18Onormse))+
  geom_line(data=goe4[goe4$treatment!="control",], lty = "solid",aes(y=iWUEnorm, x=year))+geom_errorbar(data=goe4[goe4$treatment!="control",], width = 0.2, aes(ymin=iWUEnorm-iWUEnormse, ymax=iWUEnorm+iWUEnormse))+
  theme_bw()+ylab("d18O (dotted), iWUE (solid) normalized to control")+facet_grid(treatment~site) +theme(strip.background = element_rect(fill = "White"))

elkHFts<-goe4[goe4$site=="Elkhorn"&goe4$treatment=="HF",]
elkHFts<-elkHFts[-1]
elkHFts<-elkHFts[-2:-4]
elkHFts<-elkHFts[-3:-7]
elkHFts<-elkHFts[1:3]
elkHFts<-ts(elkHFts)
acfelkHF<-acf(elkHFts, type = "cor")
summary(acfelkHF)
plot(elkHFts$d18O~elkHFts$iWUE)
