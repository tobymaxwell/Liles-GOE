library(lattice)
library(plyr)



################## working with the annual incriment data #######

### FEATHER  falls
C <- read.csv('FF_C_treerings.csv', header=TRUE)
F <- read.csv('FF_F_treerings.csv', header=TRUE)
H <- read.csv('FF_H_treerings.csv', header=TRUE)
HF <- read.csv('FF_HF_treerings.csv', header=TRUE)
G <- make.groups(C, F, H, HF)
write.csv(G, file='FF_treerings_all.csv')

## whitmpre
C <- read.csv('W_C_treerings.csv', header=TRUE)
F <- read.csv('W_F_treerings.csv', header=TRUE)
H <- read.csv('W_H_treerings.csv', header=TRUE)
HF <- read.csv('W_HF_treerings.csv', header=TRUE)
G1 <- make.groups(C, F, H, HF)
write.csv(G, file='W_treerings_all.csv')

## elkhorn
C <- read.csv('E_C_treerings.csv', header=TRUE)
F <- read.csv('E_F_treerings.csv', header=TRUE)
H <- read.csv('E_H_treerings.csv', header=TRUE)
HF <- read.csv('E_HF_treerings.csv', header=TRUE)
G2 <- make.groups(C, F, H, HF)
write.csv(G, file='E_treerings_all.csv')

V <- make.groups(G, G1, G2)
write.csv(G, file='GOE_treerings_all.csv')

str(C)

# summing for each site
B <- aggregate(G$dia_mm, list(G$site, G$treatment, G$ring, G$YR), mean)
names(B) <- c('site', 'treatment', 'ring', 'year', 'diameter')

B1 <- aggregate(G$dia_mm, list(G$site, G$treatment, G$ring, G$YR), sum)
names(B1) <- c('site', 'treatment', 'ring', 'year', 'diameter')

str(B)
B1 <- subset(B, subset=B$ring=='Total')
B1$year <- as.numeric(B1$year)

str(B1)
x <- B1$ring

plot.ecdf(x, ylab='diameter', xlab='year', verticals=TRUE)
# summing for all data
B1 <- aggregate(V$dia_mm, list(V$site, V$treatment, V$ring), sum)
names(B1) <- c('site', 'treatment', 'ring', 'year', 'diameter')
str(B1)
B$ring <- factor(B$ring, level=c('EW', 'LW', 'Total'), labels=c('Total', 'Early Wood', 'Late Wood', ))

xyplot(B$diameter/10 ~ B$year | B$treatment + B$ring, data=B, type=c('o','g'), layout=c(4,3), asp=1, main='Annual Growth - Feather Falls', ylab='Ring length - cm', xlab='Years Since Planting', col=1, lwd=2, ylim=c(0,1.6),strip=strip.custom(bg=gray(0.9)))


xyplot(B1$diameter/10 ~ B1$year | B1$treatment + B1$ring, data=B1, type=c('o','g'), layout=c(4,3), asp=1, main='Annual Growth - Feather Falls', ylab='Ring length - cm', xlab='Years Since Planting', col=1, lwd=2, ylim=c(0,10),strip=strip.custom(bg=gray(0.9)))
dev.copy2pdf(file='E_Ring_data.pdf')


### historgram of the totals
B <- aggregate(G$dia_mm/2, list(G$site, G$treatment, G$plot, G$ring), sum)
names(B) <- c('site', 'treatment', 'plot', 'ring' 'radius')
B


# this will show the density
hist(B1$treatment ~ B1$diameter, data=B1, main='FF tree diameters - FF', freq=FALSE, breaks = 10, xlab='cm', xlim=range(0,35), ylim=c(0,20), col='gray', border='orange', lines(density(G$diameter)))






#### reading them all in as G to use the summary function
G <- read.csv('FF_C_treerings.csv', header=TRUE)
G <- read.csv('FF_F_treerings.csv', header=TRUE)
G <- read.csv('FF_H_treerings.csv', header=TRUE)
G <- read.csv('FF_HF_treerings.csv', header=TRUE)



str(G)
G1 <- subset(G, subset=G$ring=='Total')
G1$year <- as.numeric(G$year)

#creating a sequence to subdivide the yearly measthis large continuous data set in this case we are working from 150 to 650 degrees by 2 degree intervals
bins <- seq(1,21, by=1)

# applying the newly created bins to the column of interest in this case it for the temp
C$annual <- cut(C$year, bins)


#creating a custom function that will produce an array of summary statistics on the annual measurements
f <- function(i) 
{
	
i.conf <- data.frame(t(t.test(i$dia_mm, conf.level=0.95, na.action='na.omit')$conf.int))
names(i.conf) <- c('lower', 'upper')
	


d <- data.frame(sum=sum(i$dia_mm, na.rm=TRUE), mean=mean(i$dia_mm, na.rm=TRUE), median=median(i$dia_mm, na.rm=TRUE), min=min(i$dia_mm, na.rm=TRUE), max=max(i$dia_mm, na.rm=TRUE), sd=sd(i$dia_mm, na.rm=TRUE), var=var(i$dia_mm, na.rm=TRUE), i.quant, i.conf)
	
return(d)		
	
}
	
G.agg <- ddply(C, .(bins), .fun=f)	
str(G.agg)


plot(mean ~ year, data=G.agg)

xyplot(mean + lower + upper ~ bins, data=G.agg, main='Annual Growth Mean +/- 95% CI', ylab='mm annual growth', xlab='Year', type=c('o','g'), lty=c(1,2,2), col=1, strip=strip.custom(bg=gray(0.9))) 