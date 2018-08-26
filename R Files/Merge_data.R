#### merging data 

### trying to plot a cumulative distribution function
### trying to plot a cumulative distribution function
G1 <- read.csv('Ftree_all.csv', header=TRUE)
G2 <- read.csv('Wtree_all.csv', header=TRUE)
G3 <- read.csv('Etree_all.csv', header=TRUE)

G <- make.groups(G1, G2, G3)
str(G1)

G <- read.csv('GOE_growth_all.csv', header=TRUE)
P <- read.csv(file='GOE_annual_PT.csv', header=TRUE)
## read in the incramental data
fert <- read.csv('GOE_fert.csv', header=TRUE)
lai <- read.csv('Cover_wide.csv', header=TRUE)


### to merge data together aligne object x and y by the factor or factors of interest
X <- merge(x=PA, y=Q, by='unique', all.x=TRUE)
X1 <- merge(x=X, y=Q1, by=c('site','water_yr'), all.x=TRUE)


#### to merge the annual precip and temp data with growth data

G1 <- merge(x=G, y=P, by=c('site','year_id'), all.x=TRUE)
X1 <- merge(x=G, y=Q1, by=c('site','water_yr'), all.x=TRUE)
X1 <- merge(x=X1, y=T, by=c('site','year_id', 'water_yr'), all.x=TRUE

write.csv(G1, file='GOE_growth_enviro.csv')
G2$pct_total <- as.numeric(G2$pct_total)

G2 <- subset(G1, subset=water_yr==3)
a =c(250, 500, 1000, 1500, 2000, 2500, 3000)
xyplot(BAIc ~ year_id+rain_mm | site, groups=treatment, data=G2, scales=list(x=list(at=a)))


xyplot(BAIc ~ year_id+rain_mm | site, groups=treatment, data=G2)
