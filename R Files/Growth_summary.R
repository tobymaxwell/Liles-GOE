library(lattice)
#library(aqp)
library(plyr)

### this is how I summarized the raw growth ring data 
### trying to plot a cumulative distribution function
G1 <- read.csv('FF_C_treerings.csv', header=TRUE)
G2 <- read.csv('FF_F_treerings.csv', header=TRUE)
G3 <- read.csv('FF_H_treerings.csv', header=TRUE)
G4 <- read.csv('G', header=TRUE)

## Elkhorn
G1 <- read.csv('E_C_treerings.csv', header=TRUE)
G2 <- read.csv('E_F_treerings.csv', header=TRUE)
G3 <- read.csv('E_H_treerings.csv', header=TRUE)
G4 <- read.csv('E_HF_treerings.csv', header=TRUE)
## Whitmore
G1 <- read.csv('W_C_treerings.csv', header=TRUE)
G2 <- read.csv('W_F_treerings.csv', header=TRUE)
G3 <- read.csv('W_H_treerings.csv', header=TRUE)
G4 <- read.csv('W_HF_treerings.csv', header=TRUE)
G <-make.groups(G1, G2, G3, G4)

# summing for each tree
B <- ddply(G, .(treatment, plot,tree_id, year, YR, ring), .fun=summarise, mean=mean(rad_mm/10), mean=mean(dia_mm/10))

## cumulative distribution for each set of measurements by rep  
B$CUM_R <-  ave(B$mean, B$ring, B$tree, B$plot, FUN = cumsum)
B$CUM_D <-  ave(B$mean.1, B$ring, B$tree, B$plot, FUN = cumsum)
B$dia <- B$mean*2
B$BAI <- 3.14*(B$mean^2)
B$BAIc <- 3.14*(B$CUM_R^2)
### properly name
names(B) <- c('treatment', 'plot','tree_id', 'year_id', 'year', 'ring', 'R_mm', 'D_mm', 'CUM_R', 'CUM_dia', 'dia', 'BAI', 'BAIc')

#add a column for the site and treatment and depth
B$site <- 'Feather Falls'

B$site <- 'Elkhorn'

B$site <- 'Whitmore'

### make a unique id from site_id, plot and tree
B$unique <- paste(B$site, B$plot, B$tree_id, sep='')
B$unique_tree <- paste(B$plot, B$tree_id, sep='-')
### alter the nature of these vectors
B$plot <- as.factor(B$plot)
B$year <- as.factor(B$year)
B$year_id <- as.numeric(B$year_id)
B$tree_id <- factor(B$tree_id)
str(B)

xyplot(BAIc ~ year | treatment, groups=unique_tree, subset=ring=='LW', data=B, pch=1:9, layout=c(1,4))

xyplot(BAIc ~ year | treatment, groups=site_id, subset=ring=='Total', data=B, pch=16, layout=c(1,4))

bwplot(BAIc ~ pct_avg |treatment, subset=ring=='Total', data=m,layout=c(1,4))

### make summary files
write.csv(B, file='Ftree_all.csv')
write.csv(B, file='Wtree_all.csv')
write.csv(B, file='Etree_all.csv')



### trying to plot a cumulative distribution function
G1 <- read.csv('Ftree_all.csv', header=TRUE)
G2 <- read.csv('Wtree_all.csv', header=TRUE)
G3 <- read.csv('Etree_all.csv', header=TRUE)



B <- make.groups(G1,G2, G3)
B$year_id <- as.factor(B$year_id)
B$year <- as.factor(B$year)
B$plot <- as.factor(B$plot)
B$tree_id <- as.factor(B$tree_id)

str(B)



# summing for each treatment by site
B1 <- ddply(B, .(site, treatment, year_id, ring), .fun=summarise, mean=mean(BAIc))
names(B1) <- c('site', 'treatment', 'year_id', 'ring', 'BAIc')
## this merges the bulk growth data with the treatment summaries
M <- merge(x=B, y=B1, by=c('unique', 'site', 'treatment', 'year', 'ring'))

write.csv(M, file='GOE_growth_all.csv')
#order them correclty
B1$site <- factor(B1$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'))
#order them correclty
B1$treatment <- factor(B1$treatment, levels=rev(c('herbicide', 'control', 'HF', 'fertilizer')))
tps <- list(
superpose.line=list(lwd=1.5, lty=3, col=1),
superpose.symbol=list(pch=c(0,1,2,3), cex=0.7, col=1),
plot.line=list(col='black'),
plot.symbol=list(col='black')
)
trellis.par.set(tps)
B1$site <- factor(B1$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'))
B1$ring <- factor(B1$ring, levels=c('LW', 'EW', 'Total'), labels=c('Latewood', 'Earlywood', 'Total'))



C <- 
useOuterStrips(xyplot(BAIc~year_id|site+ring, groups=treatment, data=B1, lty=1, lwd=0.5, layout=c(1,3), scales=list(y=list(relation='free', rot=0), x=list(at=y)), ylab=rev(c('Precipitation - mm', 'Temp - C', 'Temp - C')), xlim=c(1985,2009), xlab=list('', font=2),auto.key=list(columns=4, points=TRUE, lines=TRUE, space='top', between.columns=0.7, between=0.7)),strip=strip.custom(bg=NA), strip.left=strip.custom(bg=NA))

pdf(file='PT_years+allsites.pdf', width=6, height=8)
trellis.par.set(tps)
print(C)
dev.off()

Tt <- subset(B1, subset=ring=='Total')
Te <- subset(B1, subset=ring=='Earlywood')
Tl <- subset(B1, subset=ring=='Latewood')


T<- useOuterStrips(xyplot(BAIc~year_id|site+ring, groups=treatment, data=Tt, type=c('p', 'l', 'g'), lty=1, lwd=0.5, layout=c(3,1), scales=list(y=list(relation='same', rot=0), x=list(draw=FALSE)), ylab='mm^2', xlim=c(1985,2009), xlab=list('', font=2),auto.key=list(columns=4, points=TRUE, lines=TRUE, space='top', between.columns=1.7, between=0.7)), strip=strip.custom(bg=NA), strip.left=strip.custom(bg=NA, TRUE))

E<- useOuterStrips(xyplot(BAIc~year_id|site+ring, groups=treatment, data=Te, type=c('p', 'l', 'g'), lty=1, lwd=0.5, layout=c(3,1), scales=list(y=list(relation='same', rot=0), x=list(draw=FALSE)), ylab='mm^2', xlim=c(1985,2009), xlab=list('', font=2)), strip.left=strip.custom(bg=NA), strip=FALSE)

y=c(1986,1989, 1992, 1995, 1998, 2001, 2004, 2007)
L<- useOuterStrips(xyplot(BAIc~year_id|site+ring, groups=treatment, data=Tl, lty=1, lwd=0.5, layout=c(3,1), scales=list(y=list(relation='same', rot=0), x=list(at=y, alternating=FALSE)), ylab='mm^2', xlim=c(1985,2009), xlab=list('', font=2)), strip=FALSE, strip.left=strip.custom(bg=NA))
c(T=T, E=E, L=L, x.same=TRUE, y.same=FALSE, layout=c(3,3))

par(mfrow=c(3,1))
print(T)
print(E)
print(L)
## for latewood

G. <- xyplot(BAIc  ~ year|site, group=treatment, data=B,subset=ring=='Total', type=c('l','p','g'), lwd=1.5, strip=strip.custom(bg=gray(0.9)), ylab='Basal Area - mm^2', ylim=c(-20,100), xlim=c(1985, 2009), scales=list(x=list(at=b), y=list(at=a)), auto.key=list(space='top', columns=4), layout=c(3,1))

pdf(file='cumulative_BA-LW_all.pdf', width=6, height=4)
trellis.par.set(tpuseOuterStrips(xyplot(BAIc~year_id|site+ring, subset=ring=='Total', groups=treatment, data=B1, lty=1, lwd=0.5, layout=c(3,1), scales=list(y=list(relation='same', rot=0), x=list(at=y)), ylab='mm^2', xlim=c(1985,2009), xlab=list('', font=2),auto.key=list(columns=4, points=TRUE, lines=TRUE, space='top', between.columns=1.7, between=0.7)), strip.left=strip.custom(bg=NA), strip=strip.custom(bg=NA))
s)
print(G)
dev.off()



dev.copy2pdf(file='GOE_EWcumulative.pdf')
