
library(lattice)
library(plyr)
library(beeswarm)
library(grid)
library(latticeExtra)



#### BAI Cumulative final plot. My Goal is to create a plot where the grouping variable on the y axis - ring type - can be be left to vary within its magnitude.
B <- read.csv(file='Cumulative_BAI_average.csv', header=TRUE)

tps <- list(
superpose.line=list(lwd=1.5, lty=3, col=1),
superpose.symbol=list(pch=c(0,1,2,3), cex=0.7, col=1),
plot.line=list(col='black'),
plot.symbol=list(col='black')
)
trellis.par.set(tps)

#order them correclty
B$site <- factor(B$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'))
#order them correclty
B$treatment <- factor(B$treatment, levels=c('control', 'fertilizer', 'herbicide', 'HF'))
##rings
B$ring <- factor(B$ring, levels=c('Latewood', 'Earlywood', 'Total'))
## years for the x axis
y=c(1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007)

C <- 
useOuterStrips(xyplot(BAIc~year|site+ring, groups=treatment, data=B, lty=1, lwd=0.5, layout=c(3,3), type=c('p', 'l', 'g'), scales=list(y=list(relation='free', rot=0), x=list(at=y, rot=45, alternating=FALSE)), ylab=rev(c('Cumulative Basal Area (mm^2) ')), xlim=c(1985,2009), xlab=list('', font=2),auto.key=list(columns=4, points=TRUE, lines=TRUE, space='top', between.columns=0.7, between=0.7)),strip=strip.custom(bg=NA), strip.left=strip.custom(bg=NA))

pdf(file='PT_year+allsites.pdf', width=8, height=8)
trellis.par.set(tps)
print(C)
dev.off()





#### Creating three individual segements of the data to past them back together
Tt <- subset(B, subset=ring=='Total')
Te <- subset(B, subset=ring=='Earlywood')
Tl <- subset(B, subset=ring=='Latewood')


T<- useOuterStrips(xyplot(BAIc~year_id|site+ring, groups=treatment, data=Tt, type=c('p', 'l', 'g'), lty=1, lwd=0.5, layout=c(3,1), scales=list(y=list(relation='same', rot=0), x=list(draw=FALSE)), ylab='mm^2', xlim=c(1985,2009), xlab=list('', font=2),auto.key=list(columns=4, points=TRUE, lines=TRUE, space='top', between.columns=1.7, between=0.7)), strip=strip.custom(bg=NA), strip.left=strip.custom(bg=NA, TRUE))

E<- useOuterStrips(xyplot(BAIc~year_id|site+ring, groups=treatment, data=Te, type=c('p', 'l', 'g'), lty=1, lwd=0.5, layout=c(3,1), scales=list(y=list(relation='same', rot=0), x=list(draw=FALSE)), ylab='mm^2', xlim=c(1985,2009), xlab=list('', font=2)), strip.left=strip.custom(bg=NA), strip=FALSE)

y=c(1986,1989, 1992, 1995, 1998, 2001, 2004, 2007)
L<- useOuterStrips(xyplot(BAIc~year_id|site+ring, groups=treatment, data=Tl, lty=1, lwd=0.5, layout=c(3,1), scales=list(y=list(relation='same', rot=0), x=list(at=y, alternating=FALSE)), ylab='mm^2', xlim=c(1985,2009), xlab=list('', font=2)), strip=FALSE, strip.left=strip.custom(bg=NA))
c(T=T, E=E, L=L, x.same=TRUE, y.same=FALSE, layout=c(3,3))


#### summarizing cumulative growth at year 21 across sites to create an treatment average condition.
P21 <- subset(P, subset=year_id=='21')
G <- ddply(P21, .(treatment, site, year_id), .fun=summarise, BIAc=mean(T_BAIc), sd=sd(T_BAIc))

A <- aov(T_BAIc ~ treatment* site, data=P21)

xyplot(T_BAIc ~ treatment, data=P21)


### plot yearly averages for treatment by site combinations of two isotopic indexes
LN <- read.csv('Isotopes_long.csv', header=TRUE) ## data to make a panel with all 

G <- ddply(LN, .(type, site, treatment, max_t_q4, annual_mm), .fun=summarise, tope=mean(qnty),topesd=sd(qnty))
G$site <- factor(G$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'))
G$type <- factor(G$type, levels=c('18 O', 'Wi'))
 
### to plot both Iwue and 18O in the same figure regresses versus temp or precip

tps <- list(
superpose.line=list(lwd=1.5, lty=1, col=1),
superpose.symbol=list(pch=c(1,16,0,15), cex=0.7, col=1),
plot.line=list(col='black'),
plot.symbol=list(col='black')
)
trellis.par.set(tps)


#a=c(32,34,36,38,40,42,44,46)## vector to set common X axis
a=c(500, 1000, 1500, 2000, 2500, 3000, 3500)
T <-xyplot(tope ~ annual_mm|type, data=G, groups=treatment, auto.key=list(columns=4, font=2), scales=list(y=list(relation='free', font=2, cex=0.9, rot=0), x=list(at=a, font=2, cex=0.9)), layout=c(1,2), type='p', strip=FALSE,ylab='', xlab=list('Annual Precipitation - mm', font=2), font=2, strip.left=strip.custom(bg=NA, TRUE), panel=function(...) {
	panel.superpose(...)
	panel.lmline(..., col=1, lwd=2, lty=2)
	}))

pdf(file='Wi_18O_annual_gradient_treatment.pdf', width=8, height=6)
trellis.par.set(tps)
print(T)
dev.off()



ddply(P, .(site), .fun=summarise, fall=mean(rain_mm_q1), winter=mean(rain_mm_q2), spring=mean(rain_mm_q3), winter=mean(rain_mm_q4))