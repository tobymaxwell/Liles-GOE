library(latticeExtra)
library(lattice)
library(plyr)
library(beeswarm)
library(dplR)



### cover data
C <- read.csv(file='GOE_LAI.csv', header=TRUE)
C$site <- factor(C$site, levels=c('Elkhorn', 'Whitmore', 'Feather_Falls'))
C1 <- subset(C, subset=class=='brush')

C2 <- ddply(C1, .(site, Trmt, year, class), .fun=summarise, sum=sum(pct_cover))


### 
y=c(1986, 1990, 1994, 1998, 2002, 2006)

xyplot(sum ~ year|site, groups=Trmt, data=C2, layout=c(3,1), type=c('p', 'l'), ylab='% Cover', scales=list(x=list(at=y)), xlim=c(1986, 2008), strip=strip.custom(bg=gray(0.9)), pch=c(8,9), col=c(1,3))



str(C1)