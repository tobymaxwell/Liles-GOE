library(lattice)
library(grid)
library(latticeExtra)
library(plyr)
library(IDPmisc)

#### read in to get file into CSV format for future use
#x <- read.table(file='Eagle_pk_precip_temp.txt', skip=6, header=FALSE, na.strings='-9999', as.is=TRUE)
##x <- read.table(file='Arbuckle Basein_RAWS-precip.txt', header=FALSE, skip=3,na.strings='-9999', as.is=TRUE)
#x <- read.table(file='Strawberry_rain.txt', header=FALSE, na.strings='NA', sep=',')

#str(x)
#names(x) <- c('date','precip_mm')
#names(x) <- c('date', 'Temp_C-month', 'T_max', 'junk', 'junk1', 'j','precip_mm')
#names(x) <- c('year', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec', 'total')

#write.csv(x, file='Arbuckle_RAWS.csv')


#w <- read.csv(file='Whit_rain.csv', header=TRUE)
#E <- read.csv(file='Elk_model_1987-2008.csv', header=TRUE)


## read in the temperature data for Feather
#S <- read.csv(file='Strawberry_Precip1986_2008.csv', header=FALSE, na.strings='-9999', skip=2)
#str(S)
#names(S) <- c('date', 'month', 'year', 'T_max', 'T_min', 'T_avg', 'Precip')
#S$month <- factor(S$month, levels=c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))
### make each column numeric
#S$year<- as.numeric(S$year)
##S$T_max<- as.numeric(S$T_max)
#S$T_min<- as.numeric(S$T_min)
#S$T_avg<- as.numeric(S$T_avg)

#S1 <- subset(S, subset=year==1986)
#FT <- ddply(S1, .(month), .fun=summarise, sum=sum(Precip), mean=mean(T_avg),max=max(T_max), min=min(T_min))
#str(FT)
#names(FT) <- c('year', 'month', 'Avg_C', 'Max_C', 'Min_C')
#FT$month<- as.factor(FT$month)
#FT$Site <- 'Feather Falls'
#write.csv(FT, file='FF_monthly_temp.csv')

#bwplot(Min_C ~ month, data=FT)


### the working data set from here on.
##### this is the file with monthly data for precip
PM <- read.csv(file='Monthly_Precip_T_1988-2008.csv', header=TRUE)
PA <- read.csv(file='Annual_precip_1987-2008.csv', header=TRUE)


PA$site <- factor(PA$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'))

PM$month <- factor(PM$month, levels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

str(PM)


# to summarize for each quarter
Q <- ddply(PM, .(site, year),  .fun=summarise, MAT=mean(T_Avg_C))

A<-ddply(PM, .(year, water_yr, site),  .fun=summarise, Q_mm=sum(rain_mm))

ddply(A, .(water_yr, site),  .fun=summarise, mean_mm=mean(Q_mm))

Q$water_yr <- factor(Q$water_yr, levels=c('4','3','2','1'), label=c('Summer', 'Spring', 'Winter', 'Fall'))

Q$water_yr <- factor(Q$water_yr, levels=c('2','3','4','1'), label=c('Winter',  'Spring', 'Summer', 'Fall'))


Q$site <- factor(Q$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'), labels=c('E', 'W', 'FF'))

write.csv(Q, file='WR_averages.csv')

Q1<-read.csv(file='WR_averages.csv', header=TRUE)
str(Q1)
### a BWplot to show the range of seasonal rainfall

B<-
useOuterStrips(bwplot(qnt~ site|water_yr+type, data=Q1, layout=c(4,3), scales=list(y='free'), notch=TRUE, par.settings = list(plot.symbol = list(pch = 8, col =1, cex=0.7), box.rectangle = list(col=1, fill=gray(0.8)), box.umbrella = list(col = 1, lty=1), box.ratio=0.5, col=1), ylab='degrees C', xlab='', main='Average Temperature 1986-2008'),strip=strip.custom(bg=NA),strip.left=strip.custom(bg=NA))

pdf(file='GOE_WR_T.pdf', width=8, height=4)
print(B)
dev.off()




Q1 <- 
ddply(Q, .(site),  .fun=summarise, mean=mean(sum_mm), sd=sd(sum_mm), maxT=mean(MaxT), AvgT=mean(avgT))

names(Q) <- c('site', 'year_id', 'water_yr', 'sum_mm')

### summarize the temp by quarters
T <- 
ddply(PM, .(site, water_yr, year), .fun=summarise, mean_T=mean(T_Avg_C), mean_Tmax=mean(T_max_C),mean_Tmin=mean(T_min_C), Tmax=max(T_max_C), Tmin=min(T_min_C))

### assign names
names(T) <- c('site', 'water_yr', 'year', 'avg_t', 'avg_max', 'avg_min', 'max_t', 'min_t')





### to set all the symbols the same
tps <- list(
superpose.line=list(lwd=1.5, lty=3, col=1.5),
superpose.symbol=list(pch=c(8,11,1), cex=0.8, col=1),
plot.line=list(col='black'),
plot.symbol=list(col='black')
)
trellis.par.set(tps)


## basic plot of annual variation
y=c(5,10,15,20,25,30,35,40,45)
xyplot(avg_max ~ year, groups=site, subset=water_yr==4,strip=strip.custom(bg=NA),data=T, layout=c(1,1), type=c('g', 'o'), ylim=c(15,50), xlim=c(1989,2008),scales=list(y=list(at=y, cex=0.8, font=2), x=list(cex=0.8, font=2, relation='same')), auto.key=list(space='top', columns=3), ylab=list('Temperature C', cex=0.8, font=2), xlab='')


## the base BW plot for monthly precip by site
B <- bwplot(rain_mm ~ month | site, data=PM, layout=c(3,1), strip=strip.custom(bg=NA), pch=1, cex=0.5, ylab=list('Rainfall (mm)', cex=0.8, font=2),scales=list(x=list(rot=90, cex=0.8, font=2), y=list(cex=0.8, font=2)), notch=TRUE, par.settings = list(plot.symbol = list(pch = 8, col =1, cex=0.7), box.rectangle = list(col=1, fill=gray(0.8)), box.umbrella = list(col = 1, lty=1), box.ratio=0.5, col=1))

pdf(file='BW_monthly_precip.pdf', width=10, height=4)
print(B)
dev.off()




#water year data
Q <- read.csv(file='water_year_metrics.csv', header=TRUE)
Q$water_year <- as.factor(Q$water_year)
Q1 <- subset(Q, subset=water_year > 1)
Q23 <- subset(Q1, subset=water_year < 4)
Q23$water_year <- as.factor(Q23$water_year)
xyplot(diff_wy.qrt ~ year |site, groups=water_year, data=Q, scales='free',auto.key=TRUE)

n <- ddply(Q23, .(site, year, water_year), fun=summarize, sum=sum(Q_sum_mm))

### summary of the data summarizing the water year data 
### yearly averages by site

pwy <- ddply(PM, .(site, year, water_yr), .fun=summarise, sum=sum(rain_mm))
names(pwy) <- c('site', 'year', 'water_year', 'Q_sum_mm')
avg_wy<-ddply(pwy, .(site, water_year), .fun=summarise, mean=mean(Q_sum_mm))
names(avg_wy) <- c('site', 'water_year', 'qrt_avg')

pD <- ddply(PM, .(site, year, water_yr), .fun=summarise, sum=sum(diff))
names(pD) <- c('site', 'year', 'water_year', 'Q_sum_diff_mm')
avg_wy_diff<- ddply(pD, .(site, water_year), .fun=summarise, mean=mean(Q_sum_diff_mm))
names(avg_wy_diff) <- c('site', 'water_year', 'qrt_avg_diff')
cm<- cbind(pwy, pD)
write.csv(cm, file='water_year_metrics.csv')

c<- cbind(avg_wy, avg_wy_diff)

str(c)
head(c)

c$WY_pct <- c$Q_sum_diff_mm/c$Q_sum_mm*100

xyplot(qrt_avg/qrt_avg_diff | site, data=c, scales='free', layout=c(1,3), groups=water_year, auto.key=TRUE)
# site average
#ddply(p, .(site), .fun=summarise, mean=mean(sum))
## monthly avearge by site
#ddply(PM, .(site, month), .fun=summarise, median=median(rain_mm), mean=mean(rain_mm), sd=sd(rain_mm))

#EM <- ddply(E, .(year,month), .fun=summarise, mean=mean(precip_mm), median=median(precip_mm), sd=sd(precip_mm), min=min(precip_mm), max=max(precip_mm))


#xyplot(rain_mm ~ SEQ, groups=site, type=c('l', 'g'), xlab=list(rot=90), data=PM1, auto.key=TRUE)
z=0
y=list(c(89, 101, 113, 125, 137, 149, 161, 173, 185, 197, 209),labels=c('May-94'))
xyplot(diff ~ SEQ | site,layout=c(1,3), type=c('p','l','g'),data=PM1, col=1, xlim=c(85,172), labels=list(x=list(at=y)))

xyplot(diff ~ SEQ | site,layout=c(1,3), type=c('l','g'),data=PM,panel=function(x, y, ...) {panel.abline(h=(z), col=1), lwd=1.5) ; panel.xyplot(x, y, ..., col=1, lwd=3)})


### the range of precip for each month
PM$site <- factor(PM$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'), labels=c('Elkhorn - 820 mm', 'Whitmore - 1000 mm', 'Feather Falls - 2000 mm'))





#### to plot the difference between monthly mean and observed
G1 <- subset(PM1, subset=site=='Elkhorn')
G2 <- subset(PM1, subset=site=='Whitmore')
G3 <- subset(PM1, subset=site=='Feather Falls')


plot(PM1$SEQ, PM1$diff, xlab='', xaxt='n', yaxt='n',ylab=list('(mm)', cex=1.4), lwd=2.5, main=list('Annual Rainfall',cex=2, font=2), ylim=c(-500,1000), type='n', cex.axis=1.4, bg=gray(0.87))
axis(1, at=c(89, 101, 113, 125, 137, 149, 161, 173, 185, 197, 209,221, 233, 245, 257), labels=c('May 1994', 'May 1995', 'May 1996', 'May 1997', 'May 1998', 'May 1999', 'May 2000', 'May 2001', 'May 2002', 'May 2003', 'May 2004', 'May 2005', 'May 2006', 'May 2007', 'May 2008'), las=2)
axis(2, at=c(-400, -200, 0, 200, 400, 600, 800), las=2)
#abline(GL1, col=1, lwd=1.5, lty=1)
abline(h=0, lwd=10, col=gray(0.9))

abline(h=c(-400, -300, -200, -100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900), lty=3, lwd=0.7)
abline(v=c(89, 101, 113, 125, 137, 149, 161, 173, 185, 197, 209,221, 233, 245, 257), lty=3, lwd=0.7)

### add the points to the plot to control the cex and lwd elkhorn
points(G1$SEQ, G1$diff, pch=24, col=1,cex=0.8)
lines(G1$SEQ, G1$diff, col=1, lwd=1.4, lty=2)
### add the points to the plot to control the cex and lwd Whitmore
points(G2$SEQ, G2$diff, pch=8, col=1,cex=0.8)
lines(G2$SEQ, G2$diff, col=1, lwd=1.4, lty=2)
### add the points to the plot to control the cex and lwd FF
points(G3$SEQ, G3$diff, pch=1, col=1,cex=0.8)
lines(G3$SEQ, G3$diff, col=1, lwd=1.4, lty=2)
#abline(GL3, col=1, lwd=1.5, lty=1)

### add a legend to link points to labels for treatments
legend(85,1000, c('Elkhorn', 'Whitmore', 'Feather Falls'), cex=0.9, pch=c(24,8, 1), col=1)


##### this is the file with monthly data for precip
PA <- read.csv(file='Annual_Precip_1987-2008.csv', header=TRUE)
PA$site <- factor(PA$site, levels=rev(c('Elkhorn', 'Whitmore', 'Feather Falls')))
##PA$year <- as.factor(PA$year)
str(PA) 

## set the scales
x=c(1986, 1990, 1994, 1998, 2002, 2006, 2010)
mm<- c(500, 1000, 1500, 2000, 2500, 3000, 3500)
mm=c(0,50, 100, 150, 200)
xyplot(pct_avg ~ year, groups=site, data=PA, main='Annual Precipitation', ylab='precipitation (mm)', xlab='year', type=c('p','g', 'l'), strip=strip.custom(bg=gray(0.9)), ylim=c(0,200),xlim=c(1985, 2010), scales=list(x=list(at=x), y=list(at=mm)), auto.key=list(columns=3))
dev.copy2pdf(file='GOE rain.pdf')

### base graphics to show annual precip in one panel
## subset into sites 
E <- subset(PA, subset=site=='Elkhorn')
W <- subset(PA, subset=site=='Whitmore')
F <- subset(PA, subset=site=='Feather Falls')





### layout a plot to have both the annual and monthly data in the same figure

## this works to get a 1 col 2 row plot
layout(matrix(c(1,2), byrow=FALSE, ncol=1), widths=c(30,30), heights=c(15,10), respect=TRUE)
layout.show(2)


par(oma=c(0.5,1,0.5,1)) # sets the outer margine
par(mar=c(0.5,0.5,0.25,0.5)) # set the plot margine

par(fig= c(1,1,0.38,1))
### In base plot set to tpe n that is simply the frame 
rain <- plot(PA$year, PA$rain_mm, xlab='Year', ylab='', xlim=c(1985,2011), ylim=c(0,3500), xaxt='n', yaxt='n', type='n')
axis(1, at=c(1986,1989,1992,1995,1998, 2001,2004, 2007), cex=0.5, font=2)
axis(2, at=c(500, 1000, 1500,2000,2500, 3000), las=2, cex=0.4, font=2)
mtext('(mm)', side=2, line=3, font=2, cex=0.85)
### add the points to the plot to control the cex and lwd elkhorn
points(G1$year, G1$rain_mm, pch=24, col=1,cex=1)
lines(G1$year, G1$rain_mm, col=1, lwd=1.5, lty=1)
#abline(GL1, col=1, lwd=1.5, lty=1)
abline(h=800, lty=3)
### add the points to the plot to control the cex and lwd Whitmore
points(G2$year, G2$rain_mm, pch=8, col=1,cex=1)
lines(G2$year, G2$rain_mm, col=1, lwd=1.5, lty=1)
#abline(GL2, col=1, lwd=1.5, lty=1)
abline(h=1000, lty=3)
### add the points to the plot to control the cex and lwd FF
points(G3$year, G3$rain_mm, pch=1, col=1,cex=1)
lines(G3$year, G3$rain_mm, col=1, lwd=1.5, lty=1)
#abline(GL3, col=1, lwd=1.5, lty=1)
abline(h=2000, lty=3)

### add a legend to link points to labels for treatments
legend(1985,3500, c('Elkhorn', 'Whitmore', 'Feather Falls'), cex=1, pch=c(24,8, 1), col=1)


## put in an arrow to the abline and to link the r^2 value

text(2010, 2000,  '2000 mm', cex=0.85, font=2)
text(2010, 1040,  '1000 mm', cex=0.85, font=2)
text(2010, 770,  '820 mm', cex=0.85, font=2)


##arrows(1, 17, 0.75,17, angle=30, length=0.13, code=2, lwd=3.3)

dev.copy2pdf(file='GOE rain_annual_split.pdf')

