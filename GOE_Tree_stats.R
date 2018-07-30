library(latticeExtra)
library(lattice)
library(plyr)
library(beeswarm)
library(dplR)
library(lme4)
library(grid)

### set the characters for all xy plots
tps <- list(
superpose.line=list(lwd=1.5, lty=c(3,3,3), col=1),
superpose.symbol=list(pch=c(12,10,8), cex=0.7, col=1),
plot.line=list(col='black'),
plot.symbol=list(col='black')
)
trellis.par.set(tps)


P <- read.csv('GOE_Growth_Iso_precip.csv', header=TRUE)
I <- read.csv('GOE_isotopes.csv', header=TRUE) ## isotope data 1994-2008
G1 <- read.csv('GOE_growth_all.csv', header=TRUE) ## All the growth and environemtal data
H <- read.csv('GOE_Height_DBH_all.csv', header=TRUE) ### Plot data for height and DBH over time
F <- read.csv('GOE_fert.csv', header=TRUE) ### Fertilizer data
L <- read.csv('Cover_wide.csv', header=TRUE)  #### brush cover data
PT <- read.csv('GOE_annual_PT_wide.csv', header=TRUE) ### precipitation and temperature metrics
PT <- read.csv('Whit_PT_1990.csv', header=TRUE) ### precipitation and temperature metrics
#H$unique <- paste(H$site, H$plot, H$tree, sep='') ### to create a unique idea by merging columns
### make sure the keys are all of the same data class in this case a factor
P$year_id <- as.factor(P$year_id)
P$year <- as.factor(P$year)
P$plot <- as.factor(P$plot)


I$year <- as.factor(I$year)
I$plot <- as.factor(I$plot)
## tree ring growth 
G1$year <- as.factor(G1$year)
G1$year_id <- as.factor(G$year_id)
G$plot <- as.factor(G$plot)
### tree height
H$year <- as.factor(H$year)
H$year_id <- as.factor(H$year_id)
H$plot <- as.factor(H$plot)
### fertilization events
F$year_id <- as.factor(F$year_id)
F$year <- as.factor(F$year)
### brush cover
L$year_id <- as.factor(L$year_id)
L$year <- as.factor(L$year)
L$plot <- as.factor(L$plot)
### rainfall
PT$year <- as.factor(PT$year)


### order factors
G$site <- factor(G$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'))
G$treatment <- factor(G$treatment, levels=c('control', 'fertilizer', 'herbicide', 'HF'))

## subset to work with individual sites
PF <- subset(G1, subset=site=='Feather Falls')
PW <- subset(G1, subset=site=='Whitmore')
PE <- subset(G1, subset=site=='Elkhorn')

###### assessing total growth for FF
PFe <- subset(PF, subset=year_id==21)
PWe <- subset(PW, subset=year_id==21)
PEe <- subset(PE, subset=year_id==21)

lw <- aov(iWUE ~ treatment , data=PEe, subset=ring=='EW')
summary(lw)
TukeyHSD(lw)
G <- 
ddply(PEe, .(ring), .fun=summarise, BIA=mean(BAIc_ha_m2/1000))


### subset for treatments
c <- subset(P, subset=treatment=='control')
f <- subset(P, subset=treatment=='fertilizer')
h <- subset(P, subset=treatment=='herbicide')
hf <- subset(P, subset=treatment=='HF')

#### general summary of factors The first part of the call is what you what the column to be named
ddply(P, .(treatment,annual_mm, avg_max_q4, site), .fun=summarise, BAIt=mean(T_BAIc, Na.rm=TRUE), BAIt_sd=sd(T_BAIc), BAIlw=mean(LW_BAIc), BAIlw_sd=mean(LW_BAIc), BAIew=mean(EW_BAIc), BAIew_sd=mean(EW_BAIc)) ## basal area


H <- ddply(P, .(site, treatment, year_id, annual_mm,avg_max_q4), .fun=summarise, IWUE=mean(iWUE, Na.rm=TRUE), IWUE_sd=sd(iWUE), IWUEmax=max(iWUE), d18O=mean(d18O), d18O_sd=sd(d18O),d18Omax=max(d18O), BAI=mean(T_BAI))

P1 <- ddply(P, .(year, annual_mm, site,treatment), .fun=summarise, iWUE_xbar=mean(iWUE), iWUE_sd=sd(iWUE), number=length(iWUE))
A <- ddply(P, .(treatment), .fun=summarise, mean=mean(iWUE), sd=sd(iWUE), mean=mean(d18O), sd=sd(d18O))

A <- ddply(rf, .(site, treatment), .fun=summarise, mean=mean(EW_LW), sd=sd(EW_LW),mean=mean(EW), sd=sd(EW), mean=mean(LW), sd=sd(LW))

write.csv(A, file='mean_year_EL.csv')

rf <- read.csv(file='mean_year_EL.csv', header=TRUE)
R<- rf
### simple two way anova within site
Fa <-aov(BAIc*1647 ~ treatment, subset=ring=='LW', data=PWe)
summary(Fa)
TukeyHSD(Fa, 'treatment')
TukeyHSD(Fa, 'site')
bwplot(BAIc ~ treatment, data=PEe, subset=ring=='Total')


### W
Wa <-aov(d18O_mean ~ year, data=PE)
summary(Wa)
TukeyHSD(Wa)
bwplot(BAIc ~ treatment, subset=ring=='Total', data=PWe)

Ea <-aov(iWUE ~ year, data=PF)
summary(Ea)
TukeyHSD(Ea)



#### this set the analysis contrasts 
options(contrasts=c("contr.sum","contr.poly"))
L <- lm(iWUE ~ treatment*year, data=PE)
summary(L)

### linear mixed models (1|vairable is random intercept) (factor*Factor interaction)
pairs(~ BAIc + iWUE + treatment, data=P) ### gives a matrix of the factors you feed it

PF1 <- subset(PF, subset=ring=='Total')
## model with one fixed and a simple random effect
F1 <- lmer(BAI ~ site*treatment + (1|year), data=G1)
F1 <- lmer(iWUE ~ treatment + (1|annual_mm), data=PE)
F1 <- lmer(iWUE ~ treatment + BAI + treatment*BAI+(1|annual_mm), data=PE)
summary(F1)



## model with crossed simple random
L2 <- lmer(BAIc ~ treatment + (1|annual_mm) + (1|BAI), data=PF)


## model with crossed simple random
L3 <- lmer(iWUE ~ treatment + year_id + site, data=PF)
L3 <- lmer(BAIc ~ treatment + (1|annual_mm) + (1|year), data=PF)
summary(L3)




#### general summary of factors
A <- ddply(P, .(site, max_t_q4), .fun=summarise, mean=mean(iWUE), sd=sd(iWUE), mean=mean(d18O), sd=sd(d18O))

#### simple linear models based on treatments
lc <- lm(BAI ~ site*treatment+year, data=G1)
summary(lc)
lc

xyplot(mean.1 ~ max_t_q4, data=A, subset=treatment=='HF', type=c('r', 'p', 'g'))

xyplot(d18O ~ max_t_q4, data=P, groups=site, subset=treatment=='fertilizer', type=c('r', 'p', 'g'))

xyplot(d18O ~ max_t_q4, data=P, groups=site, subset=treatment=='herbicide', type=c('r', 'p', 'g'))

xyplot(d18O ~ max_t_q4, data=P, groups=site, subset=treatment=='HF', type=c('r', 'p', 'g'))


## summarize early and late wood cumulative BAI

#### working with the EW and LW data from the main ring/growth file to calculate EL_ratio
G1 <- subset(G, subset=ring=='EW')
G2 <- subset(G, subset=ring=='LW')
re <- ddply(G1, .(site, treatment, unique, year_id), .fun=summarise, mean=mean(BAIc), sd=sd(BAIc))
names(re) <- c('site', 'treatment', 'unique', 'year_id', 'EW_mean', 'EW_sd')
rl <- ddply(G2, .(site, treatment, unique, year_id), .fun=summarise, mean=mean(BAIc), sd=sd(BAIc))
names(rl) <- c('site', 'treatment', 'unique', 'year_id', 'LW_mean', 'LW_sd')

rt <- merge(x=re, y=rl, by=c('site', 'treatment', 'unique', 'year_id'), all.x=TRUE)


write.csv(rt, file='EWLW_cum-summary.csv')

R<-read.csv(file='EWLW-summary.csv', header=TRUE)
R$year_id <- as.factor(R$year_id)
R$site <- factor(R$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'))
R$treatment <- factor(R$treatment, levels=c('control', 'fertilizer', 'herbicide', 'HF'))
r<-
xyplot(EW/10 ~ LW|treatment, groups=site, data=R, auto.key=list(columns=3, lines=FALSE, cex=0.75), type=c('r', 'p', 'g'), ylab='Cumulative Earlywood Basal Area (cm^2)', xlab='Cumulative Latewood Basal Area (mm^2)', strip=strip.custom(bg=gray(0.8)), layout=c(1,4), xlim=c(-2,36), ylim=c(-2,36))


pdf(file='CUM_EW:LW_trt_tall.pdf', width=5, height=8)
trellis.par.set(tps)
print(r)
dev.off()



## subset to work with individual sites
RF <- subset(R, subset=site=='Feather Falls')
RW <- subset(R, subset=site=='Whitmore')
RE <- subset(R, subset=site=='Elkhorn')


#### simple linear models based on treatments
lc <- lm(EW/10 ~ LW, subset=treatment=='HF',data=RE)
summary(lc)
xyplot(EW_mean ~ LW_mean, subset=treatment=='control', data=RF)



P$annual_mm <- as.numeric(P$annual_mm)

#creating a custom function that will produce an array of summary statistics on the bins you created for DTG
f <- function(i) 
{	
i.conf <- data.frame(t(t.test(i$iWUE, conf.level=0.95, na.action='na.omit')$conf.int))
names(i.conf) <- c('lower', 'upper')
	
p <- c(0.05, 0.25, 0.5, 0.75, 0.95)
i.quant <- data.frame(t(quantile(i$iWUE, probs=p, na.rm=TRUE)))
names(i.quant) <- paste('q', round(p*100), sep='_')

d <- data.frame(sum=sum(i$iWUE, na.rm=TRUE), mean=mean(i$iWUE, na.rm=TRUE), median=median(i$iWUE, na.rm=TRUE), min=min(i$iWUE, na.rm=TRUE), max=max(i$iWUE, na.rm=TRUE), sd=sd(i$iWUE, na.rm=TRUE), var=var(i$iWUE, na.rm=TRUE), i.quant, i.conf)
return(d)			
}
	
G.agg <- ddply(P, .(annual_mm), .fun=f)	

#add a column for the site and treatment and depth
G.agg$site <- 'ALL'
G.agg$type <- 'iWUE'


# make a new csv file of these data
write.csv(G.agg, file='FFdeep_dtg1D.csv')

xyplot(mean + q_25 + q_75 ~ annual_mm, data=G.agg, type='l', lty=c(1,2,2), col=1)

