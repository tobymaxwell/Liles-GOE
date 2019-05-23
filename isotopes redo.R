library(plyr)
library(ggplot2)
library(lme4)
library(sjPlot)

setwd("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/")
goeiso<-read.csv("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/GOE_isotopes.csv")
goeppt<-read.csv("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/GOE_annual_PT.csv")
str(goeiso)
str(goeppt)
goeclim<-read.csv("Round 3/Climate Data.csv")
tail(goeclim, 100)
goeclim.yr<-ddply(goeclim, .(year, site), summarise, 
                  T.avg.corr = mean(T.avg.corr),
                  rain_mm= mean(rain_mm),
                  T.avg = mean(T_Avg_C))
goeclim.avg<-ddply(goeclim, .(site), summarise, 
                  T.avg.corr = mean(T.avg.corr),
                  rain_mm= mean(rain_mm),
                  T.avg=mean(T_Avg_C))

goeclim.avg

Fig1<-ggplot(goeclim.yr[goeclim.yr$site=="Elkhorn",], aes(x=year, y=T.avg.corr,color=site))+
  geom_line()+
  geom_line(data=goeclim.yr[goeclim.yr$site!="Elkhorn",], aes(x=year, y=T.avg, ))
Fig1

Fig2<-Fig1<-ggplot(goeclim.yr, aes(x=year, y=T.avg.corr,color=site))+
  geom_line()
Fig2
goeiso$syt<-paste0(goeiso$site, goeiso$year, goeiso$treatment)
goeiso$sy<-paste0(goeiso$site, goeiso$year)

goe2<-ddply(goeiso[goeiso$treatment=="control",], .(site, year), summarise, 
      meanctrl=mean(d18O),
      meanwue=mean(iWUE),
      meanBA = mean(BA_mm))
goe2$sy<-paste0(goe2$site,goe2$year)
goeiso<-merge(goeiso, goe2)
goeiso2<-merge(goeiso, goeppt[2:8])



goe3<-ddply(goeiso2, .(site, treatment, year), summarise, 
            n=length(iWUE),
            d18Onormsd = sd((d18O-meanctrl)/meanctrl)/length(d18O/meanctrl),
            d18Onorm = mean((d18O-meanctrl)/meanctrl),
            d18O = mean(d18O),
            meanctrl = mean(meanctrl),
            d13C=mean(d13C),
            BA=mean(BA_mm),
            BAnorm = mean(BA_mm-meanBA/meanBA),
            iWUEse = sd(iWUE)/n,
            iWUEnormsd = sd((iWUE-meanwue)/meanwue)/length(iWUE-meanwue/meanwue),
            iWUE=mean(iWUE),
            iWUEnorm = mean((iWUE-meanwue)/meanwue))
goe4<-merge(goe3, goeppt)
goe.plots<-ddply(goe4[goe4$treatment!="control",], .(site, treatment, year), summarise, 
                 d18Onormsd = mean(d18Onormsd),
                 d18Onorm = mean(d18Onorm),
                 iWUEnormsd = mean(iWUEnormsd),
                 iWUEnorm = mean(iWUEnorm),
                 annual_mm=mean(annual_mm),
                 avg.t=mean(c(avg_t_q1,goe4$avg_t_q2,goe4$avg_t_q3,goe4$avg_t_q4)))

goe4$avg.t<-(goe4$avg_t_q1+goe4$avg_t_q2+goe4$avg_t_q3+goe4$avg_t_q4)/4
goe4$st<-paste0(goe4$site, goe4$treatment)

goe.year<-ddply(goe4[goe4$treatment!="control",], .(site, year, treatment), summarise, 
                iWUEnorm=mean(iWUEnorm),
                d18Onorm=mean(d18Onorm))
goe4$site<-factor(goe4$site, levels=c("Elkhorn", "Whitmore", "Feather Falls"))

setwd("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Round 3")
png("Figure 5", units="px", width=2400, height=3200, res=600)
ggplot(goe4, aes(y=iWUE, x=d18O, shape=site))+
  geom_point(size = 2)+
  facet_wrap(~treatment)+
  theme_classic()+
 scale_shape_manual(values=c(0,1,2))+
  theme(legend.position="bottom")
dev.off()

lm1<-lm(iWUE~d18O, goe4[goe4$treatment=="control",])
lm2<-lm(iWUE~d18O, goe4[goe4$treatment=="fertilizer",])
lm3<-lm(iWUE~d18O, goe4[goe4$treatment=="herbicide",])
lm4<-lm(iWUE~d18O, goe4[goe4$treatment=="HF",])
library(sjPlot)
sjt.lm(lm1,lm2, lm3, lm4)
library(agricolae)
tmod<-aov(d18O~st, goe4)
anova(tmod)
tuk<-HSD.test(tmod, trt="st", DFerror=164,MSerror=0.7464, group=T)
tuk<-data.frame(tuk$groups)
tuk<-tuk[order(rownames(tuk)),]
str(tuk)
median<-ddply(goe4, .(site, treatment), summarise, 
               d18Omed<-median(d18O))
tuk$med<-median$..1
              
ggplot(goe4, aes(y=(d18O), x=st, shape = treatment))+geom_boxplot()+theme_classic()+
  annotate("text", label =tuk$trt, y=tuk$med-0.1, x=c(1:12))

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
  annotate("text", label =tuk$trt, y=tuk$med-0.005, x=c(1:9))

str(goe.plots)
goe.plots[4:9]<-scale(goe.plots[4:9])
goe4$site<-factor(goe4$site, levels=c("Elkhorn", "Whitmore", "Feather Falls"))

ggplot(goe4[goe4$treatment!="control",], aes(y=d18Onorm, x=year))+
  geom_line()+
  geom_errorbar(data=goe4[goe4$treatment!="control",], width = 0.5, aes(ymin=d18Onorm-d18Onormsd, ymax=d18Onorm+d18Onormsd))+
  theme_bw()+ylab("d18Onorm")+
  facet_grid(treatment~site) +theme(strip.background = element_rect(fill = "White"))+
  theme(legend.text = element_text("gelp"))

ggplot(goe4, aes(y=iWUE, x=year))+
  geom_line()+
  geom_errorbar(data=goe4, width = 0.5, aes(ymin=iWUE-iWUEse, ymax=iWUE+iWUEse))+
  theme_bw()+ylab("iWUE")+
  facet_grid(treatment~site) +theme(strip.background = element_rect(fill = "White"))

ggplot(goe4[goe4$treatment!="control",], aes(y=iWUEnorm, x=year))+
  geom_line()+
  #geom_boxplot(data=goe4[goe4$treatment!="control",])+
geom_errorbar(data=goe4[goe4$treatment!="control",], width = 0.5, aes(ymin=iWUEnorm-iWUEnormsd, ymax=iWUEnorm+iWUEnormsd))+
  theme_bw()+facet_grid(treatment~site)+theme(strip.background = element_rect(fill = "White"))

#Isotope Boxplots Fig. 3
isolong<-read.csv("isolong.2.csv")
str(isolong)
isolong.mean<-ddply(isolong[isolong$treatment=="control",], .(site, year, type), summarise,
                    meanraw = mean(raw))
isolong2<-merge(isolong, isolong.mean)
isolong2$norm<-(isolong2$raw-isolong2$meanraw)/isolong2$meanraw

iso18O<-isolong2[isolong2$type=="d18O",]
isoiWUE<-isolong2[isolong2$type=="iWUE",]
library(DescTools)
DunnettTest(goe4[goe4$site=="Elkhorn",]$d18O, g=goe4[goe4$site=="Elkhorn",]$treatment, control="control")

DunnettTest(goe4$d18O, g=goe4$treatment, control="control")

DunnettTest(isoiWUE$norm, g=isoiWUE$treatment, control="control")

tmod<-aov(norm~treatment, isoiWUE[isoiWUE$treatment!="control"&isoiWUE$site=="Elkhorn",])
anova(tmod)
tuk<-HSD.test(tmod, trt="treatment", DFerror=42,MSerror=0.0023717, group=T)
tuk

tmod<-aov(norm~treatment, isoiWUE[isoiWUE$treatment!="control"&isoiWUE$site=="Whitmore",])
anova(tmod)
tuk<-HSD.test(tmod, trt="treatment", DFerror=39,MSerror=0.0026512, group=T)
tuk

tmod<-aov(norm~treatment, isoiWUE[isoiWUE$treatment!="control"&isoiWUE$site=="Feather Falls",])
anova(tmod)
tuk<-HSD.test(tmod, trt="treatment", DFerror=42,MSerror=0.0045145, group=T)
tuk

###tukeys for 18O

tmod<-aov(norm~treatment, iso18O[iso18O$treatment!="control"&iso18O$site=="Elkhorn",])
anova(tmod)
tuk<-HSD.test(tmod, trt="treatment", DFerror=42,MSerror=0.0038725, group=T)
tuk

tmod<-aov(norm~treatment, iso18O[iso18O$treatment!="control"&iso18O$site=="Whitmore",])
anova(tmod)
tuk<-HSD.test(tmod, trt="treatment", DFerror=39,MSerror=0.004271, group=T)
tuk

tmod<-aov(norm~treatment, iso18O[iso18O$treatment!="control"&iso18O$site=="Feather Falls",])
anova(tmod)
tuk<-HSD.test(tmod, trt="treatment", DFerror=42,MSerror=0.0040607, group=T)
tuk

isolong2$site<-factor(isolong$site, levels=c("Elkhorn", "Whitmore", "Feather Falls"))
isolong2$type<-factor(isolong$type, levels=c("iWUE", "d18O"))
type.labs=c("A","B")
site.labs=c("Elkhorn", "B", "Feather Falls")
setwd("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Round 3")
png("Figure 3", units="px", width=2400, height=3200, res=600)
ggplot(isolong2[isolong2$treatment!="control",], aes(y=(norm), x=treatment,shape=treatment))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none",strip.background =element_rect(fill="white"))+
  facet_grid(type~site, labeller = labeller(type=type.labs, site=site.labs))+
  xlab("")+
  ylab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
library(sjPlot)
str(goeiso2)
wuemod<-lmer(iWUE~treatment+(1|tree)+(1|year)+(1|site), goeiso)
sjt.lmer(wuemod)
ggplot(goeiso, aes(y=iWUE, x=treatment))+geom_boxplot()+facet_grid(~site, scales="free")
ggplot(goeiso, aes(y=BA_mm, x=treatment))+geom_boxplot()+facet_grid(~site)
ggplot(goeiso[goeiso$year>2000,], aes(y=iWUE, x=BA_mm))+geom_point()

ggplot(goeiso[goeiso$year==2005,], aes(y=BA_mm, x=treatment))+geom_boxplot()

####Mixed Effects####
library(lme4)

#firest reorganize climate data
climate<-read.csv("GOE_tree_all_climate.csv")
str(climate)
climate$water_qtr<-as.factor(climate$water_qtr)
climate<-ddply(climate, .(year, site, water_qtr), summarise, 
               annual_mm=mean(annual_mm),
               avg_t=mean(avg_t),
               WY_Q_pct=mean(WY_Q_pct),
               rain_WY_mm = mean(rain_WY_mm),
               max_t = mean(max_t),
               min_t = mean(min_t))
clim.wide<-reshape(data=na.omit(climate), 
        idvar = c("year", "site"), timevar = c("water_qtr"),
        direction = "wide")
str(climate)
str(clim.wide)
clim.wide
goe.mem<-merge(goeiso, clim.wide)
str(goe.mem)
library(minpack.lm)
goe.mem$avg.t<-(goe.mem$avg_t.1+goe.mem$avg_t.2+goe.mem$avg_t.3+goe.mem$avg_t.4)/4
mod2<-formula(iWUE~phi1/(1+exp(-(phi2+phi3*(annual_mm.1)))))
mod2<-formula(iWUE~X*exp(K*annual_mm.1))
model<-formula(d18O~a*log(annual_mm.1)+b)
it.ppt<-nlsLM(model,
             data = goe.mem,
             #start=list(phi1=100, phi2=10, phi3=5),
             start=list(a=100, b=50), #e^-kt or alog()+b
             control = list(maxiter = 1000, minFactor=.00001),
             trace = TRUE) 
goe.mem$pptline<-(-.93101*log(goe.mem$annual_mm.1)+32.5524)

mod2.2<-formula(iWUE~phi1/(1+exp(-(phi2+phi3*(avg.t)))))
it.temp.iWUE<-nlsLM(mod2.2,
              data = goe.mem,
              #start=list(a=10,b=80),
              start=list(phi1=5, phi2=.1, phi3=.05),
              trace = TRUE) 

wue.ppt<-ggplot(goe.mem, aes(iWUE, x=annual_mm.1))+geom_point()+
  geom_line(aes(goe.mem$annual_mm.1, y=predict(it.ppt)))+theme_bw()

wue.temp<-ggplot(goe.mem, aes(iWUE, x=avg.t))+geom_point()+
  geom_line(aes(avg.t, y=predict(it.temp.iWUE)))+theme_bw()

mod3<-formula(d18O~phi1/(1+exp(-(phi2+phi3*(avg.t)))))
it.temp<-nlsLM(mod3,
             data = goe.mem,
             start=list(phi1=25, phi2=1, phi3=0.05),
             #start=list(phi1=1, phi2=1, phi3=1),
             trace = TRUE, 
             control = list(maxiter = 1000, minFactor=.00001)) 
mod4<-formula(d18O~phi1/(1+exp(-(phi2+phi3*(1/annual_mm.1)))))
findiplist(x=goe.mem$d18O, y=1/goe.mem$annual_mm.1,2)
it.ppt.18O<-nlsLM(mod4,
               data = goe.mem,
               #start=list(a=10,b=80),
               start=list(phi1=25, phi2=1, phi3=.05),
               trace = TRUE, 
               control = list(maxiter = 1000, minFactor=.00001)) 
predict(m.inc)

O.temp<-ggplot(goe.mem, aes(d18O, x=avg.t))+geom_point()+
  geom_line(aes(goe.mem$avg.t, y=predict(it.temp)))+theme_bw()
O.ppt<-ggplot(goe.mem, aes(d18O, x=1/annual_mm.1))+geom_point()+
  geom_line(aes(1/goe.mem$annual_mm.1, y=predict(it.ppt.18O)))+theme_bw()

summary(lm(scale(d18O)~scale(avg.t), goe.mem))
summary(lm(scale(iWUE)~scale(avg.t), goe.mem))
library(gridExtra)
grid.arrange(O.temp, O.ppt,wue.temp, wue.ppt, nrow = 2)
grid.arrange(wue.temp, wue.ppt, nrow=1)
summary(lm(predict(m.inc)~1/goe.mem$annual_mm.1))

findiplist(x=1/goe.mem$annual_mm.1, y=goe.mem$iWUE,2)
height<-read.csv("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Review edits round 2/GOE_Height_DBH_all.csv")
height<-ddply(height, .(year, site, treatment), summarise,
              ht=mean(HT_cm),
              dbh=mean(dbh_cm))
str(height)
str(goe.mem)
goe.mem.ht<-merge(goe.mem, height)
str()



#Time series

goets<-subset(goe4, select =c("year", "site", "treatment","d18O", "iWUE"))
treats<-c("control", "fertilizer", "HF", "herbicide")
sites<-c("Whitmore", "Elkhorn", "Feather Falls")
rep(control,3)
lag<-NULL
data<-NULL
for(i in sites){
  for(j in treats){
    lag<-ts(goets[goets$site==i&goets$treatment==j,][-2:-3])
    lag<-acf(lag, type = "cor")
    data<-c(data, max(abs(lag$acf[,3,2])))
  }
  
}

acfs<-data.frame(data)
acfs$site<-
elkHFts<-ts(elkHFts)
acfelkHF<-acf(elkHFts, type = "cor")
str(acfelkHF)
acfelkHF$acf[2,3,2]
plot(elkHFts$d18O~elkHFts$iWUE)

####EW LW Growth
setwd("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Review edits round 2/")
growth<-read.csv("GOE_growth.csv")
str(growth)
growth.avg<-ddply(growth, .(year, site, treatment, tree), summarise,
                  EW_BAI=mean( Earlywood_BAI_mmsq ),
                  LW_BAI=mean(Latewood_BAI),
                  T_BAI=mean(Totalwood_BAI  ),
                  T_BAIc=mean(Totalwood_cumulative_BAI  ))
growth.avg2<-ddply(growth.avg[growth.avg$treatment=="control",], .(site, year), summarise, 
            meanEW=mean(EW_BAI),
            meanLW=mean(LW_BAI),
            meanT = mean(T_BAI),
            meanBAIc=mean(T_BAIc))
growth.avg3<-merge(growth.avg, growth.avg2)
growth.avg3$EWnorm<-(growth.avg3$EW_BAI-growth.avg3$meanEW)/growth.avg3$meanEW
growth.avg3$LWnorm<-(growth.avg3$LW_BAI-growth.avg3$meanLW)/growth.avg3$meanLW
growth.avg3$Tnorm<-(growth.avg3$T_BAI-growth.avg3$meanT)/growth.avg3$meanT
growth.avg3$BAIcnorm<-(growth.avg3$T_BAIc-growth.avg3$meanBAIc)/growth.avg3$meanBAIc
growth.avg4<-growth.avg3[growth.avg3$treatment!="control",]
str(growth.avg4)
ggplot(growth.avg4, aes(y=BAIcnorm, x = year, color=treatment))+geom_point()+facet_grid(~site, scales="free")+theme_classic()

goeisosmall<-ddply(goe4, .(site, year, treatment), summarise,
                   d18O=d18O,
                    iWUE=iWUE,
                   d18Onorm=d18Onorm,
                   iWUEnorm=iWUEnorm,
                    avg.t=avg.t,
                    annual_mm=annual_mm)

growth.small<-merge(growth.avg, goeisosmall)

growth.norm2<-merge(growth.avg4, goeisosmall)
str(growth.norm2)

ggplot(growth.norm2, aes(y=EWnorm, x = year, color=treatment))+geom_point()+facet_grid(~site, scales="free")+theme_classic()

#Singlething at the end where we normalize to the control.
#BAI figure plus growth in all forms - ANOVA to test stie and trt effect and interactions
#THen do the exact same thing ANOVA for iWUE and d18O (including control data as trt), shows site effect, trt effect, and interaction
#so far three figs and three tables
#last step - normalize by control bc we are interseted in the universal effect of trt ,site as random, tree ID and size random
#Fixed is iWUE, 18O, no 18O, climate
#Run 2 model, 1 with tree ID, size, nested within year, year then controls for repeated measure of variance
Tmod1<-lm(Tnorm~site*treatment, growth.norm2)
ewmod1<-lm(EWnorm~site*treatment, growth.norm2)
LWmod1<-lm(LWnorm~site*treatment, growth.norm2)
summary(Tmod1)
anova(Tmod1)
anova(ewmod1)
anova(LWmod1)
wuemod1<-lm(iWUEnorm~site*treatment, growth.norm2)
Omod1<-lm(d18Onorm~site*treatment, growth.norm2)
anova(wuemod1)
anova(Omod1)

growth.norm$year<-as.factor(growth.norm$year)
str(growth.norm)

require(car)
## Loading required package: car
require(MASS)
# This is so that distributions that must be non-zero can make sense of my
# data
qqp(sqrt(growth.norm2$LWnorm), "norm")

PQLEW <- glmmPQL(EWnorm ~ d18Onorm+avg.t +treatment ,random = list(~1|year, ~1|site, ~1|tree), family = gaussian(link = "log"), data = growth.norm2, verbose = FALSE, start = coef(lm(EWnorm ~ d18Onorm+avg.t +treatment, data = growth.norm2)))

PQLLW <- glmmPQL(LWnorm ~ d18Onorm+iWUEnorm+avg.t +treatment,random = list(~1|year, ~1|site, ~1|tree), family = gaussian(link = "log"),data = growth.norm2, verbose = FALSE, start = coef(lm(LWnorm ~ d18Onorm+iWUEnorm+avg.t + treatment, data = growth.norm2)))

PQLT <- glmmPQL(Tnorm ~ d18Onorm+iWUEnorm+avg.t +treatment, random = list(~1|year, ~1|site, ~1|tree), family = gaussian(link = "log"),data = growth.norm2, verbose = FALSE, start = coef(lm(Tnorm ~ d18Onorm+iWUEnorm+avg.t +treatment , data = growth.norm2)))

plot(Tnorm~d18Onorm, growth.norm2[growth$treatment=="fertilizer",])

summary(PQLEW)
summary(PQLLW)
summary(PQLT)

Tmod<-lmer(scale(Tnorm)~scale(iWUEnorm)+scale(avg.t)+treatment+(1|site)+(1|tree)+(1|year/T_CUM_dia), growth.norm)
ewmod<-lmer(scale(EWnorm)~scale(iWUEnorm)+scale(avg.t)+treatment+(1|site)+(1|tree)+(1|year/T_CUM_dia), growth.norm)
LWmod<-lmer(scale(LWnorm)~scale(iWUEnorm)+scale(annual_mm)+treatment+(1|site)+(1|tree)+(1|year/T_CUM_dia), growth.norm)
sjt.lmer(Tmod, ewmod, LWmod, pred.labels=c("iWUE", "MAT", "Herbicide", "HF", "MAP"), depvar.labels = c("Total BAI", "Early Wood BAI", "Late Wood BAI"), p.zero=T, separate.ci.col = T, group.pred=T)


summary(Tmod2)


tmod<-aov(CUM_dia~treatment, growth[growth$year==2008,])
anova(tmod)
tuk<-HSD.test(tmod, trt="treatment", DFerror=209,MSerror=65.129, group=T)
tuk<-data.frame(tuk$groups)
tuk<-tuk[order(rownames(tuk)),]
tuk

ggplot(growth[growth$year==2008,], aes(y=CUM_dia, x = treatment))+geom_boxplot()+theme_classic()

growth.wide<-read.csv("GOE_growth_all_wide.csv")
str(growth.wide)

isogrowth<-merge(goeiso, growth.wide, all=T, by=c("site", "year", "treatment", "tree"))

str(isogrowth)

ggplot(isogrowth, aes(y=iWUE, x=T_CUM_dia))+geom_point()+facet_wrap(~treatment, scales="free")+theme_classic()

lm1<-lm(iWUE~T_CUM_dia, isogrowth[isogrowth$treatment=="fertilizer",])
summary(lm1)

#Site Differences
library(agricolae)

tmod<-aov(d18O~treatment, goeiso[goeiso$site=="Whitmore",])
anova(tmod)
tuk<-HSD.test(tmod, trt="treatment", DFerror=157,MSerror=2.3654, group=T)
tuk<-data.frame(tuk$groups)
tuk<-tuk[order(rownames(tuk)),]
tuk

levels(goeiso$site)
tukmod<-aov(iWUE~treatment, goeiso[goeiso$site=="Elkhorn",])
anova(tukmod)
HSD.test(tukmod, trt="treatment", DFerror = 500, MSerror=144.08)


ddply(goeiso, .(site, year), summarise,
      iwuesd=sd(iWUE),
      iWUE= mean(iWUE),
      delta = mean(delta),
      sd18O = sd(d18O),
      d18O = mean(d18O))
str(goeppt)
str(growth.norm2)
ggplot(growth.norm2, aes(y=Tnorm, x=iWUEnorm, color=treatment))+geom_point()+facet_grid(~site)+ylim(-1,10)
summary(lm(Tnorm~iWUEnorm, growth.norm2))

ggplot(growth.norm2, aes(y=EWnorm, x=iWUEnorm, color=treatment))+geom_point()+facet_wrap(~site)+ylim(-1,10)
summary(lm(EWnorm~iWUEnorm, growth.norm2))

ggplot(growth.norm2, aes(y=LWnorm, x=iWUEnorm, color=treatment))+geom_point()+facet_wrap(~site)+ylim(-1,10)
summary(lm(LWnorm~iWUEnorm, growth.norm2))


library(minpack.lm)
mod2<-formula(iWUEnorm~phi1/(1+exp(-(phi2+phi3*(annual_mm)))))

it.ppt<-nlsLM(mod2,
              data = growth.norm2,
              start=list(phi1=5, phi2=.1, phi3=.01),
              control = list(maxiter = 1000, minFactor=.00001),
              trace = TRUE) 
growth.norm2$pptline<-predict(it.ppt)
wue.ppt<-ggplot(growth.norm2, aes(iWUEnorm, x=annual_mm))+geom_point()+
  geom_line(aes(annual_mm, y=predict(it.ppt)))+theme_bw()

temp.wue<-lm(iWUEnorm~avg.t, growth.norm2)

wue.t<-ggplot(growth.norm2, aes(iWUEnorm, x=avg.t))+geom_point()+
  geom_line(aes(y=predict(temp.wue), x=avg.t))+theme_bw()

mod4<-formula(d18Onorm~phi1/(1+exp(-(phi2+phi3*(annual_mm)))))
mod5<-formula(d18Onorm~a*log(annual_mm)+b)
it.ppt.18O<-nlsLM(mod5,
                  data = growth.norm2,
                  start=list(a=10,b=80),
                  #start=list(phi1=0, phi2=1, phi3=0.0001),
                  trace = TRUE, 
                  control = list(maxiter = 1000, minFactor=.00001)) 

temp.18O<-lm(d18Onorm~avg.t, growth.norm2)

O.ppt<-ggplot(growth.norm2, aes(d18Onorm, x=annual_mm))+geom_point()+
  geom_line(aes(growth.norm2$annual_mm, y=predict(it.ppt.18O)))+theme_bw()
O.t<-ggplot(growth.norm2, aes(d18Onorm, x=avg.t))+geom_point()+
  geom_line(aes(y=predict(temp.18O), x=avg.t))+theme_bw()

summary(lm(d18Onorm~avg.t, growth.norm2))
summary(lm(iWUEnorm~avg.t, growth.norm2))
summary(lm(d18Onorm~annual_mm, growth.norm2))
summary(lm(iWUEnorm~annual_mm, growth.norm2))
library(gridExtra)
grid.arrange(O.t, O.ppt,wue.t, wue.ppt, nrow = 2)
grid.arrange(wue.temp, wue.ppt, nrow=1)
summary(lm(predict(m.inc)~1/goe.mem$annual_mm.1))

library(corrplot)
growth.norm2$avg.t<-NA
growth.norm2$annual_mm<-NA

climate.prism<-read.csv("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Round 3/Climate_prism.csv")

climate.prism
growth.norm3<-merge(growth.norm2, climate.prism, by = c("year", "site"))
growth.cor<-growth.norm3[-1:-12]
growth.cor<-growth.cor[-9:-11]
growth.cor<-growth.cor[-5:-6]
M<-corrplot(cor(na.omit(growth.cor)))
colnames(M) <- c("Early Wood BAI", "Late Wood BAI", "Total Wood BAI", "Cumulative BAI", "d18O", "iWUE", "MAT", "MAP")
rownames(M) <- c("Early Wood BAI", "Late Wood BAI", "Total Wood BAI", "Cumulative BAI", "d18O", "iWUE", "MAT", "MAP")
library(corrplot)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(mtcars)
head(p.mat[, 1:5])

res1 <- cor.mtest(na.omit(growth.cor), conf.level = .95)

corrplot(M, p.mat = res1, sig.level = 0.05, method="shade", col=colorRampPalette(c("black", "Grey", "white","black"))(2000))
rownames(M) <- c("Early Wood BAI", "Late Wood BAI", "Total Wood BAI", "Cumulative BAI", "d18O", "iWUE", "MAT", "MAP")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
setwd("/Users/tobymaxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/Round 3")
png("Figure 4", units="px", width=3699, height=3600, res=600)
corrplot(M, method="color", sig.level = 0.05, col=col(200),  
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = res1, 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
dev.off()
## specialized the insignificant value according to the significant level
corrplot(M, p.mat = res1$p, sig.level = .2,diag=FALSE)
