library(lattice)
library(plyr)
library(beeswarm)
library(grid)
library(latticeExtra)
library(plotrix)
     
P <- read.csv('GOE_Growth_Iso_precip_finalyr.csv', header=TRUE)
LN <- read.csv('Isotopes_long.csv', header=TRUE) ## data to make a panel with all 

I <- read.csv('GOE_isotopes.csv', header=TRUE) ## isotope data 1994-2008
Gr <- read.csv('GOE_growth_all.csv', header=TRUE) ## All the growth and environemtal data
G <- read.csv('GOE_growth_all_wide.csv', header=TRUE) ## All the growth and environemtal data


H <- read.csv('GOE_Height_DBH_all.csv', header=TRUE) ### Plot data for height and DBH over time
H <- read.csv('Tree_height.csv', header=TRUE)

F <- read.csv('GOE_fert.csv', header=TRUE) ### Fertilizer data
L <- read.csv('Cover_wide.csv', header=TRUE)  #### brush cover data

# summing for site and treatment year to look at treatment affects
G <- ddply(L, .(site, year_id,treatment), .fun=summarise, Shrub=mean(shrub_pct), tree=mean(tree_pct))
xyplot(Shrub ~ year_id|site, groups=treatment, data=G, layout=c(3,1), pch=c(1,2,3), type=c('p', 'l'), auto.key=list(columns=4), ylab='pct-shrub cover', xlab='growth year')

PT <- read.csv('GOE_annual_PT_wide.csv', header=TRUE) ### precipitation and temperature metrics
PT <- read.csv('Whit_PT_1990.csv', header=TRUE) ### precipitation and temperature metrics
#H$unique <- paste(H$site, H$plot, H$tree, sep='') ### to create a unique idea by merging columns

str(P)
### make sure the keys are all of the same data class in this case a factor
P$year_id <- as.factor(P$year_id)
P$year <- as.factor(P$year)
P$plot <- as.factor(P$plot)
P$year_id <- factor(P$year_id, levels=c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21'))

P$yr_mm <- paste(P$year_id, P$annual_mm, sep='-')
P$yr_maxT <- paste(P$year_id, P$max_t_q4, sep='-')
P$yr_maxT <- as.factor(P$yr_maxT)

I$year <- as.factor(I$year)
I$plot <- as.factor(I$plot)
## tree ring growth 
G$year <- as.factor(G$year)
G$year_id <- as.factor(G$year_id)
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

##I$unique <- paste(I$site, I$plot, I$tree, sep='') ### to merge filess
### merge the grwoth and isotope data together
#P <- merge(x=P, y=Q, by=c('site','year'), all.x=TRUE)
#P1 <- merge(x=P, y=H, by=c('site', 'year', 'year_id', 'treatment', 'plot', 'tree'), all.x=TRUE) ## Height
#P2 <- merge(x=P1, y=F, by=c('site', 'year', 'year_id', 'treatment'), all.x=TRUE) ## fert
#P3 <- merge(x=P2, y=L, by=c('site', 'year', 'year_id', 'plot', 'treatment'), all.x=TRUE) ## brush 
W <- merge(x=Gr, y=PT, by=c('site', 'year'), all.x=TRUE) 
#W <- merge(x=I, y=O, by='Id_tope', all.x=TRUE)

#G$ring_site <- paste(G$site, G$ring, sep='')
### order factors
#P$site <- factor(P$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'))
#G$treatment <- factor(G$treatment, levels=c('control', 'fertilizer', 'herbicide', 'HF'))
#G$ring <- factor(G$ring, levels=c('LW', 'EW', 'Total'))
#write.csv(W, file='GOE_Growth_Iso_precip.csv')



#### basic summary
G <- ddply(P, .(site, year), .fun=summarise, O=mean(d18O), Omax=max(d18O), Omin=min(d18O), W=mean(Wi), Wmax=max(Wi),Wmin=min(Wi))

G$Wdiff <- G$Wmax-G$Wmin
G$Odiff <- G$Omax-G$Omin 

xyplot(Odiff ~ year, groups=site, data=G, type=c('p','l'), auto.key=list(columns=3))

 
G <- ddply(P, .(site, year), .fun=summarise, iWUEmean=mean(d18O), max13=max(d18O), min13=min(d18O))
G$Ddiff <- G$max18 - G$min18
G$Wdiff <- G$max13 -G$min13
ddply(G, .(site), .fun=summarise, D=mean(Wdiff))

L <- aov(Wdiff ~ site, data=G)
summary(L)
densityplot(~min13|site, data=G, groups=treatment, auto.key=list(columns=4), layout=c(1,3))

densityplot(~T_BAIc|site, data=P, groups=treatment, auto.key=list(columns=4), layout=c(1,3))
str(P)

G <- ddply(P, .(site, year, pct_annual_q4), .fun=summarise, LW=mean(LW_BAI), EW=mean(EW_BAI), T=mean(T_BAI))


xyplot(Imean ~ Tmean|treatment,groups=site, data=G, layout=c(4,1), type=c('p'), auto.key=list(columns=4), ylab='iWUE', xlab='delta 18 O', strip=strip.custom(bg=NA))

P$year <- as.factor(P$year)

xyplot( ~ year, groups=site, data=P, layout=c(1,3))
A<-aov(d18O ~ treatment+year, data=P, subset=site=='Elkhorn')
A<-aov(Wi ~ year, data=Pf, subset=site=='Elkhorn')
A<-aov(d18O ~ treatment, data=P, subset=site=='Feather Falls')
A<-aov(Wi ~d18O*treatment, data=P)
summary(A)
TukeyHSD(A, 'year')





#### linear relationships 
C <- lm(d18O ~ annual_mm*treatment,data=P)

C <- lm(W ~ O, data=G, subset=treatment=='fertilizer')
C <- lm(W ~ O, data=G, subset=treatment=='herbicide')
C <- lm(W ~ O, data=G, subset=treatment=='HF')
summary(C)



### set the characters for all xy plots for sites
tps <- list(
superpose.line=list(lwd=1.5, lty=c(1,2,3,4), col=1),
superpose.symbol=list(pch=c(12,10,8), cex=0.9, col=1),
plot.line=list(col='black'),
plot.symbol=list(col='black')
)
trellis.par.set(tps)


#### bulk growth data 
xyplot(BAIc ~year|site,groups=treatment, data=Gr, pch=c(0,15,1,16), layout=c(3,1), type=c('p'), scales=list(y=list(relation='same')), subset=ring=='EW')


xyplot(BAI/BAIc~year|site,group=treatment, data=G, pch=c(0,15,1,16), layout=c(3,1), type=c('p', 'smooth'), scales=list(y=list(relation='same')), subset=ring=='Total', ylim=c(0,0.1))


G <- ddply(W, .(site, year, treatment, ring, annual_mm), .fun=summarise,BAImed=median(BAI), BAI=mean(BAI), sd=sd(BAI),BAIc=mean(BAIc), BAIn=mean(BAI/BAIc))


#### plotting raw data

xyplot(Wi ~year|site,groups=treatment, data=P, pch=c(0,15,1,16), layout=c(1,1), type=c('p'), scales=list(y=list(relation='free')))



### data that is summarized to annual treatment means
xyplot(avg_t_q3 ~ year, pch=16, lty=2, data=P, type=c('p', 'smooth'), layout=c(1,1), groups=site, auto.key=list(columns=3))


xyplot(T ~ year, group=treatment, pch=16, lty=2, data=Gt, type=c('p', 'smooth'), layout=c(1,1), subset=site=='Elkhorn')

xyplot(O+W~ year|site, pch=c(0,15,1,16), lty=2, data=Gt, type=c('p', 'smooth', 'g'), auto.key=list(columns=4, points=FALSE, lines=TRUE), layout=c(3,2), groups=treatment, scales=list(y=list(relation='free')))


xyplot(O  ~ year|site, groups=treatment,  pch=c(0,15,1,16), lty=2, data=Gt, type=c('p', 'smooth'), auto.key=list(columns=4, points=FALSE, lines=TRUE), layout=c(3,1))

GP <- ddply(P, .(site, year), .fun=summarise, SP=mean(rain_mm_q3), avg=mean(pct_normal), pq3=mean(pct_avg_q3))

P$max_t_q4 <- as.factor(P$max_t_q4)
Gt <- ddply(P, .(site, year, treatment, annual_mm,avg_t_q4), .fun=summarise, W=median(Wi), Wmax=max(Wi), Wmin=min(Wi), O=median(d18O), Omax=max(d18O), Omin=min(d18O), LW=median(LW_BAI), T=median(T_BAI), EW=median(EW_BAI))
Gt$site <- factor(Gt$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'))

Gq3 <- ddply(P, .(site, treatment, year, rain_mm_q3), .fun=summarise, W=median(discrim), Wmax=max(discrim), Wmin=min(discrim)) 

l<-lm(T ~ O*treatment, data=Gt)
summary(l)

xyplot(W~year|site, pch=c(1,2,3,4), lty=2, groups=treatment, data=Gq3, type=c('p', 'l'), auto.key=list(columns=4, points=FALSE, lines=TRUE), layout=c(3,1))



Pc <- subset(P, subset=treatment=='control')
Pf <- subset(P, subset=treatment=='fertilizer')
Ph <- subset(P, subset=treatment=='herbicide')
Phf<- subset(P, subset=treatment=='HF')

C <- lm(W ~O*treatment, data=G)
x <- Gt$O
y <- Gt$T
y <- P$MAT
z <- P$pct_annual_q4
cor(x,y)



#### the bee swarm plot to figure out how to show relative difference from mean precip and d13 C or iWUE
P2 <- G[order(G$mean, G$treatment, G$annual_mm),] ### mean by treatment
P2 <- P[order(P$mean, P$treatment, P$annual_mm),] ### raw data


### bee swarm plot precip across all site

axis(1, at=c(1,5,11,16,21,26,70), labels=c(500, 750, 1000, 1250, 1500, 2000, 2500, 3000, 3500))

## this opens up the stream with all the pieces after this going into the plot
pdf(file='figure.pdf', width=10, height=6)

beeswarm(mean ~ annual_mm,  spacing=0.9, pwpch=as.numeric(treatment), data=G, xlab='Precipitation - mm', ylab='iWUE')


### low for whit and FF
abline(v=1, lty=3, lwd=2, col=gray(0.6)) ### low precip Elk 412 2007
text(1, 72,  'Low Elkhorn - 2007', cex=0.85, font=2)
abline(v=30, lty=1, lwd=2, col=gray(0.8)) ### high precip Elk 1526 1998
text(30, 118.5,  'High Elkhorn - 1998', cex=0.85, font=2)

abline(v=6, lty=3, lwd=2, col=gray(0.6)) ### low precip whit 668 1991
text(6, 72,  'Low Whitmore - 1991', cex=0.85, font=2)
abline(v=33, lty=1, lwd=2, col=gray(0.8)) ### high precip whit 1663 1998
text(33, 117,  'High Whitmore - 1998', cex=0.85, font=2)

abline(v=22, lty=3, lwd=2, col=gray(0.6)) ### low precip FF 1278 1990
text(22, 72,  'Low Feather Falls - 1990', cex=0.85, font=2)
abline(v=43, lty=1, lwd=2, col=gray(0.8)) ### high precip FF 3422 1996
text(42.5, 118,  'High Feather Falls - 1996', cex=0.85, font=2)

## need to figure out how to control the legend placement??
legend(-1.1,119.3, legend=levels(P2$treatment), pch=1:4)
boxplot(mean ~ annual_mm, data=P2, add=T, notch=TRUE, lty=2, lwd=0.5)
dev.copy2pdf(file='iWUE_precip_gradient_swarm.pdf')


## need to figure out how to control the legend placement??
legend(2900,13, legend=levels(I2$treatment), pch=1:4)
legend(, -28, legend=levels(I2$treatment), col=c(1,4,9))

### this closes the devise
dev.off()



P$site <- factor(P$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'))
### to set up the beeswarm to control for site and treatment
P2 <- P[order(P$Wi, P$site, P$treatment),] ## site the treatment
P3 <- P[order(P$d18O, P$site, P$treatment),] 



## this opens up the stream with all the pieces after this going into the plot
pdf(file='iWUE_trt_Beeswarm.pdf', width=10, height=6)

beeswarm(Wi ~ treatment,  spacing=1.3, pwpch=as.numeric(site), data=P2, xlab='', cex=1, ylab='Wi', pch=c(1,2,3), ylim=c(50,130), xlim=c(0.5,4.6))
## overlay boxplot
boxplot(Wi~ treatment, data=P2, add=T, notch=TRUE, lty=2, lwd=0.75)
text(1, 53,  'a', cex=1.25, font=2)
text(2, 53,  'ab', cex=1.25, font=2)
text(3, 53,  'c', cex=1.25, font=2)
text(4, 53,  'bc', cex=1.25, font=2)
legend(4.15, 133.35, legend=levels(P2$site), pch=c(1,2,3), cex=0.9)

dev.off()



### to set up the beeswarm to control for site and treatment
P3 <- P[order(P$iWUE, P$treatment, P$site),] ## treatment then site

pdf(file='iWUE_treatment_swarm.pdf', width=10, height=6)
beeswarm(iWUE ~ treatment,  spacing=1.5, pwpch=as.numeric(site), data=P3, xlab='', cex=0.9, ylim=c(58,135))
## overlay boxplot
boxplot(iWUE ~ treatment, data=P3, add=T, notch=TRUE, lty=2, lwd=1.1)
text(1, 58,  'a', cex=1.25, font=2)
text(2, 58,  'a', cex=1.25, font=2)
text(3, 58,  'b', cex=1.25, font=2)
text(4, 58,  'ab', cex=1.25, font=2)
legend(4.05,138.2, legend=levels(P3$site), pch=1:4)

dev.off()







B <-
bwplot(d18O ~ year | treatment, layout=c(1,4),data=PE, main='Elkhorn 18O', strip=strip.custom(bg=gray(0.75)), pch=1, cex=0.5, ylab=list('d 18O', cex=1, font=2),scales=list(x=list(cex=1, font=2), y=list(cex=0.85, font=2)), notch=TRUE, par.settings = list(plot.symbol = list(pch = 8, col =1, cex=1), box.rectangle = list(col=1, fill=gray(0.8)), box.umbrella = list(col = 1, lty=1), box.ratio=0.5, col=1))

pdf(file='EBW_d18O_year.pdf', width=10, height=6)
print(B)
dev.off()


xyplot(annual_mm ~ BAI, groups=size, xlab='precipitation (mm)', type=c('smooth', 'p'), auto.key=TRUE, data=I)


xyplot(annual_mm~iWUE/d18O|site, groups=size, type=c('smooth', 'p'), data=P, xlab='precipitation (mm)', auto.key=list(columns=3))
dev.copy2pdf(file='13C_precip.pdf')


densityplot(~LW_BAI/T_BAIc|treatment,data=P, groups=site)
   
   
   ### sc
cloud(annual_mm ~T_BAI*iWUE, data=P, scales=list(relation='free'), layout=c(1,1), cex=0.6, screen=list(z=90, x=-0, y=45), auto.key=list(columns=4))


y=c(70, 90, 110) #### iWUE
x=c(23,25,27,29,31) ## d18O
z=c(18, 20, 22, 24, 26) ## annual precip


C <- cloud(avg_t_q4 ~ iWUE * d18O|site, data=P,strip=strip.custom(bg=gray(0.7), par.strip.text=list(font=2, cex=0.8)), screen=list(z=151, x=-75), pch=19, zlim=c(0,4000), xlim=c(22,32),ylim=c(60,120), scales=list(x=list(arrows=FALSE, at=y), y=list(arrows=FALSE, at=x), z=list(arrows=FALSE, at=z)), par.settings=list(par.xlab.text=list(font=2, cex=1), axis.text=list(cex=1, font=2), par.ylab.text=list(font=2, cex=1), par.zlab.text=list(font=2, cex=1)), layout=c(3,1))

pdf(file='figures/LAB_cloud_PM.pdf', width=12, height=8)
trellis.par.set(tps)
print(C)
dev.off()


## subset to work with individual sites
PF <- subset(H, subset=site=='Feather Falls')
PW <- subset(H, subset=site=='Whitmore')
PE <- subset(H, subset=site=='Elkhorn')


### ploting the site by treatmen by year mean iWUE
a=c(1,5,10,15,20)
a=c(500,1000, 1500,2000,2500,3000, 3500)
b=c(70, 80, 90, 100, 110, 120)

B <- 
useOuterStrips(xyplot(BAIc_ha_m2/1000 ~ year_id|site + ring, groups=treatment,data=P, ylab='Basal Area - mm^2', xlab='', layout=c(3,3), type=c('p','l','g'),auto.key=list(columns=4, space='bottom', lines=TRUE)), scales=list(x=list(at=a), relation='free'),strip=strip.custom(bg=NA, strip.left=strip.custom(bg=NA))

scales=list(x=list(relation='same'), relation='free')
pdf(file='test.pdf', width=10, height=6)
trellis.par.set(tps)
print(B)
dev.off()


### need to figure out how to get strips to work properly
## figure 1: plot of C% ~ L-coordinate, split by site, with pearson's cor. printed in each panel 
## 
p1 <- useOuterStrips(xyplot(iWUE ~ year_id|site+treatment, data=P, 
type=c('p','g', 'r'), col=1, as.table=TRUE, scales=list(cex=0.9), auto.key=list(columns=4, lines=TRUE),
ylab='iWUE', xlab='Growth Year',  aspect=1, layout=c(3,4),
strip=strip.custom(bg=NA),strip.left=strip.custom(bg=NA),
panel=function(x, y, ...){
panel.xyplot(x, y, ...)
# compute person's correlation coef
cp <- cor(x,y, use='complete.obs')
cp.text <- paste('r=', round(cp, 2))
# compute sample size
n.text <- paste('n=', length(x))
# add text to each panel
grid.text(
label=cp.text, gp=gpar(fontsize=12), just='right', 
x=unit(0.25, "npc"), y=unit(0.90, "npc")
)
grid.text(
label=n.text, gp=gpar(fontsize=12), just='right', 
x=unit(0.25, "npc"), y=unit(0.80, "npc")
)
}))

pdf(file='iWUE_year_trxsite.pdf', width=12, height=16)
trellis.par.set(tps)
print(p1)
dev.off()


### set the characters for all xy plots for sites
tps <- list(
superpose.line=list(lwd=1.5, lty=c(1), col=1),
superpose.symbol=list(pch=c(1,2,3), cex=0.9, col=1),
plot.line=list(col='black'),
plot.symbol=list(col='black')
)
trellis.par.set(tps)

## figure 1: plot of C% ~ L-coordinate, split by site, with pearson's cor. printed in each panel 
## 
p1 <- xyplot(Imean ~ Tmean|treatment, data=G, 
type=c('p','g','r'), col=1, as.table=TRUE, scales=list(cex=0.9),
ylab='iWUE', xlab='delta 18 O', 
strip=strip.custom(bg=NA), aspect=1, layout=c(4,1),
panel=function(x, y, ...){
panel.xyplot(x, y, ...)
# compute person's correlation coef
cp <- cor(x,y, use='complete.obs')
cp.text <- paste('r=', round(cp, 2))
# compute sample size
n.text <- paste('n=', length(x))
# add text to each panel
grid.text(
label=cp.text, gp=gpar(fontsize=12), just='right', 
x=unit(0.25, "npc"), y=unit(0.90, "npc")
)
grid.text(
label=n.text, gp=gpar(fontsize=12), just='right', 
x=unit(0.25, "npc"), y=unit(0.80, "npc")
)
})

pdf(file='iWUE_18O_trt.pdf', width=12, height=6)
trellis.par.set(tps)
print(p1)
dev.off()



### add a regression line through an xy plot with groups based symbols
# plot y ~ x, with points, grid, and group-wise regression line
# use custom panel function, to add regression line for entire data set
# note that "..." passes all of the default arguments from parent function to child function(s)
xyplot(y ~ x, groups=g, data=d, type=c('p','g', 'r'),
auto.key=list(columns=4, points=TRUE, lines=FALSE),
panel=function(...) {
	panel.superpose(...)
	panel.lmline(..., col=1, lwd=2, lty=2)
	})


newpanel <- function(...) {panel.superpose(...) panel.lmline(x,y,..., col=1, lwd=2, lty=2)})


LN$type <- factor(LN$type, levels=c('iWUE', 'd18O'), labels=c('delta 18 O', 'iWUE'))


G <- ddply(LN, .(type, treatment, site, max_t_q4), .fun=summarise, tope=mean(qnty),topesd=sd(qnty))
G
 
### to plot both Iwue and 18O in the same figure regresses versus temp or precip

tps <- list(
superpose.line=list(lwd=1.5, lty=c(1,2,3,4), col=1),
superpose.symbol=list(pch=c(1,16,2,17), cex=0.7, col=1),
plot.line=list(col='black'),
plot.symbol=list(col='black')
)
trellis.par.set(tps)


a=c(32,34,36,38,40,42,44,46)

T <- 
xyplot(tope ~ max_t_q4|type, data=G, groups=treatment, scales=list(y=list(relation='free', font=2, cex=0.9), x=list(at=a, font=2, cex=0.9)), layout=c(1,2), type='p', strip=FALSE,ylab='', xlab=list(' Maximum Summer Temp (C)', font=2), font=2, col=1, strip.left=strip.custom(bg=NA, TRUE), panel=newPanelFunction)

pdf(file='iWUE_18O_temp_gradient.pdf', width=8, height=6)
trellis.par.set(tps)
print(T)
dev.off()



LN$max_t_q4 <- as.factor(LN$max_t_q4)

bwplot(qnty ~ max_t_q4|type, data=LN, layout=c(1,2), scales=list(y='free'), par.settings = list(plot.symbol = list(pch = 8, col =1, cex=1), box.rectangle = list(col=1, fill=gray(0.8)), box.umbrella = list(col = 1, lty=1), box.ratio=0.5, col=1), strip=FALSE, strip.left=strip.custom(bg=NA, TRUE),ylab='', xlab=list(' Maximum Summer Temp (C)', font=2))








q <-xyplot(mean.1 ~ max_t_q4, groups=site, data=A, 
type=c('p','g'), col=1, scales=list(cex=0.9), auto.key=list(columns=3), ylab='delta 18 O', xlab='Avg Summer Max (C)',strip=strip.custom(bg=grey(0.9)))

pdf(file='d18O_max_temp_avg.pdf', width=8, height=4)
trellis.par.set(tps)
print(q)
dev.off()

### basic analysis of the growth data

## subset to work with individual sites
C <- subset(G, subset=treatment=='control')
H <- subset(G, subset=treatment=='herbicide')
F <- subset(G, subset=treatment=='fertilizer')
HF <- subset(G, subset=treatment=='HF')

### how to plot to an object in base relating avg temp and 18O
plot(C$avg_t_q4, C$Tmean, xaxt='n', ylab=list('delta 18 O', font=2), xlab=list('Max Summer Temp (C)', font=2), ylim=c(23,28.5), xlim=c(32,46.5), type='n', cex.axis=1, font=2, bg=gray(0.87))
#abline(GL1, col=1, lwd=1.5, lty=1)
axis(1, at=c(32, 34, 36, 38, 40, 42, 44, 46), las=2, font=2)
abline(h=c(22,23,24,25,26,27,28), lty=3, lwd=0.7)
abline(v=c(32,34, 36, 38, 40, 42, 44, 46), lty=3, lwd=0.7)

### add the points to the plot to control the cex and lwd elkhorn
points(C$avg_t_q4, C$Tmean, pch=10, col=1,cex=1.1) ## FF
points(F$avg_t_q4, F$Tmean, pch=16, col=1,cex=1.1) # W
points(H$avg_t_q4, H$Tmean, pch=1, col=1,cex=1.1) ## E
points(HF$avg_t_q4, HF$Tmean, pch=18, col=1,cex=1.3) ## E
abline(a=coef(L), lty=3, lwd=1.9)


### add a legend to link points to labels for treatments
legend(31.4, 28.75,  c('Control', 'Fertilizer', 'Herbicide', 'HF'), cex=1.2, pch=c(10,16,1,18 ), col=1)

text(43.7,28.5, 'r = 0.61', font=2, cex=1.2)
text(43.7, 28.2, 'Slope = 0.2', font=2, cex=1.2)
text(43.7, 27.9, 'n  = 176', font=2, cex=1.2)
### base density plots to show how things var

dev.copy2pdf(file='MaxT_18O_all.pdf')



## subset to work with individual sites
C <- subset(P, subset=treatment=='control')
H <- subset(P, subset=treatment=='herbicide')
F <- subset(P, subset=treatment=='fertilizer')
HF <- subset(P, subset=treatment=='HF')

### how to plot to an object in base
plot(C$max_t_q4, C$Tmean, xaxt='n', ylab=list('delta 18 O', font=2), xlab=list('Max Summer Temp (C)', font=2), ylim=c(23,28.5), xlim=c(32,46.5), type='n', cex.axis=1, font=2, bg=gray(0.87))
#abline(GL1, col=1, lwd=1.5, lty=1)
axis(1, at=c(32, 34, 36, 38, 40, 42, 44, 46), las=2, font=2)
abline(h=c(22,23,24,25,26,27,28), lty=3, lwd=0.7)
abline(v=c(32,34, 36, 38, 40, 42, 44, 46), lty=3, lwd=0.7)

### add the points to the plot to control the cex and lwd elkhorn
points(C$max_t_q4, C$Tmean, pch=10, col=1,cex=1.1) ## FF
points(F$max_t_q4, F$Tmean, pch=16, col=1,cex=1.1) # W
points(H$max_t_q4, H$Tmean, pch=1, col=1,cex=1.1) ## E
points(HF$max_t_q4, HF$Tmean, pch=18, col=1,cex=1.3) ## E
abline(a=coef(L), lty=3, lwd=1.9)


### add a legend to link points to labels for treatments
legend(31.4, 28.75,  c('Control', 'Fertilizer', 'Herbicide', 'HF'), cex=1.2, pch=c(10,16,1,18 ), col=1)

text(43.7,28.5, 'r = 0.61', font=2, cex=1.2)
text(43.7, 28.25, 'slope = 0.2', font=2, cex=1.2)
text(43.7, 27.9, 'n  = 176', font=2, cex=1.2)
### base density plots to show how things var

dev.copy2pdf(file='MaxT_18O_all.pdf')

#### solid desity plot of C and O data by treatment and site
D <- densityplot(~ LW_BAIc, subset=year_id=='21',data=W, strip=strip.custom(bg=gray(0.9)), layout=c(1,1),main='Summer max T (C)', pch='', auto.key=list(columns=3, space='bottom', lines=TRUE))


pdf(file='dist-max_T_q4.pdf', width=10, height=6)
trellis.par.set(tps)
print(D)
dev.off()



### summary of annual c and o by site
I <- read.csv(file='mean_year_CO.csv', header=TRUE)

## general plot of iWUE and 18O in panels with precipitation as the common axis
b <- 
xyplot(mean~annual_mm|type, type=c('g','smooth','p'), auto.key=list(columns=3, lines=FALSE, spaces='top'), data=I,layout=c(1,2), scales=list(x=list(relation='same'), relation='free'), ylab=c('delta 18 O', 'iWUE'), strip=FALSE, xlab='Annual Precipitation (mm)')

pdf(file='BAIc_summerT.pdf', width=8, height=6)
trellis.par.set(tps)
print(b)
dev.off()



#### based on average BAIc per treatment year.
H$site <- factor(H$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'))
b<-
xyplot(BAIc ~ avg_t_q4|site, data=H, type=c('r', 'p'), groups=treatment, auto.key=list(columns=4, lines=TRUE), scales=list(x=list(relation='free'), relation='same'), xlab='Average Summer Temperature (C)', ylab='Cumulative BA', strip=strip.custom(bg=(NA)), layout=c(3,1))
pdf(file='BAIc_summerT.pdf', width=8, height=6)
trellis.par.set(tps)
print(b)
dev.off()
