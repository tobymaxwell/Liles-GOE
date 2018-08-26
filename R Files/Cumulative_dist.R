library(lattice)
library(aqp)
library(plyr)
### trying to plot a cumulative distribution function
G <- read.csv('FF_C_treerings.csv', header=TRUE)
G <- read.csv('FF_F_treerings.csv', header=TRUE)
G <- read.csv('FF_H_treerings.csv', header=TRUE)
G <- read.csv('FF_HF_treerings.csv', header=TRUE)

G <- read.csv('E_C_treerings.csv', header=TRUE)
G <- read.csv('E_F_treerings.csv', header=TRUE)
G <- read.csv('E_H_treerings.csv', header=TRUE)
G <- read.csv('E_HF_treerings.csv', header=TRUE)

G <- read.csv('W_C_treerings.csv', header=TRUE)
G <- read.csv('W_F_treerings.csv', header=TRUE)
G <- read.csv('W_H_treerings.csv', header=TRUE)
G <- read.csv('W_HF_treerings.csv', header=TRUE)

# summing for each tree
G1 <- ddply(G, .(plot, tree_id, tree, year, YR, ring), .fun=summarise, mean=mean(rad_mm))
## cumulative distribution for each set of measurements by rep  
G1$CUM_R <-  ave(G1$mean, G1$ring, G1$tree, G1$plot, FUN = cumsum)
G1$BA <- 3.14*(G1$CUM_R^2)

xyplot(BA ~ year | ring, data=G1, pch=16, col=1, layout=c(1,3))
#add a column for the site and treatment and depth
G1$site <- 'Feather Falls'
G1$site <- 'Elkhorn'
G1$site <- 'Whitmore'

G1$treatment <- 'Control'
G1$treatment <- 'Fertilizer'
G1$treatment <- 'Herbicide'
G1$treatment <- 'HxF'

### alter the nature of these vectors
G1$site <- factor(G1$site)
G1$treatment <- factor(G1$treatment)
G1$year <- as.numeric(G1$year)
G1$tree_id <- factor(G1$tree_id)
G1$tree <- factor(G1$tree)
str(G1)


J1 <- subset(J, ring = 'Total')
J2 <- subset(J1, treatment = 'Control')
xyplot(BA ~ year | tree, data=J2, pch=16, col=1, layout=c(3,3))
### combine them for site based plotting
J <- G1
J1 <- G1
J2 <- G1
J3 <- G1

### combine treatments within a site
K <- make.groups(J, J1, J2, J3)
K1 <- make.groups(J, J1, J2, J3)
K2 <- make.groups(J, J1, J2, J3)

str(K)
xyplot(BA ~ year | treatment + ring, data=K, pch=16, col=1, lwd=0.5, layout=c(4,3))


write.csv(K, file='EALL_treeCS.csv')



### combine all sites
A <- make.groups(K, K1, K2)

### subset 
A1 <- subset(A, subset=A$ring=='Total')
#### order the factor for proper plotting
A1$site <- factor(A1$site, levels=c('Feather Falls',  'Whitmore','Elkhorn'))
A1$treatment <- factor(A1$treatment, levels=c('Control', 'Herbicide', 'Fertilizer', 'HxF'))

xyplot(BA/10  ~ year | treatment + site, data=A1, type=c('p','g'), col=1, pch=19, cex=0.3, strip=strip.custom(bg=gray(0.9)), ylab='Basal Area - cm^2', main='Garden of Eden Cumulative Basel Area', xlim=c(1985, 2010), layout=c(4,3), scales=list(x=list(tick.number=10)))
dev.copy2pdf(file='GOE_CUM_BA_all.pdf')

# make a new csv file of these data
write.csv(A, file='GOE_CUM_ALL.csv') 


##### generate summary stats for each site by treatment combinations
G <- read.csv('FF_C_treerings.csv', header=TRUE)
G <- read.csv('FF_F_treerings.csv', header=TRUE)
G <- read.csv('FF_H_treerings.csv', header=TRUE)
G <- read.csv('FF_HF_treerings.csv', header=TRUE)

G <- read.csv('E_C_treerings.csv', header=TRUE)
G <- read.csv('E_F_treerings.csv', header=TRUE)
G <- read.csv('E_H_treerings.csv', header=TRUE)
G <- read.csv('E_HF_treerings.csv', header=TRUE)

G <- read.csv('W_C_treerings.csv', header=TRUE)
G <- read.csv('W_F_treerings.csv', header=TRUE)
G <- read.csv('W_H_treerings.csv', header=TRUE)
G <- read.csv('W_HF_treerings.csv', header=TRUE)

# summing for each tree
G1 <- ddply(G, .(treatment, year, ring), .fun=summarise, mean=mean(rad_mm))
## cumulative distribution for each set of measurements by rep  
G1$CUM_R <-  ave(G1$mean, G1$ring, G1$treatment, FUN = cumsum)
G1$BA <- 3.14*(G1$CUM_R^2)
G1$tree <- factor(G1$tree)
G1$year <- as.factor(G1$year)
G2 <- subset(G1, subset=G1$ring=='Total')

#creating a custom function that will produce an array of summary statistics on the bins you created for annual tree ring growth data
f <- function(i) 
{
i.conf <- data.frame(t(t.test(i$BA, conf.level=0.95, na.action='na.omit')$conf.int))
names(i.conf) <- c('lower', 'upper')
	
p <- c(0.05, 0.25, 0.5, 0.75, 0.95)
i.quant <- data.frame(t(quantile(i$BA, probs=p, na.rm=TRUE)))
names(i.quant) <- paste('q', round(p*100), sep='_')

d <- data.frame(sum=sum(i$BA, na.rm=TRUE), mean=mean(i$BA, na.rm=TRUE), median=median(i$BA, na.rm=TRUE), min=min(i$BA, na.rm=TRUE), max=max(i$BA, na.rm=TRUE), sd=sd(i$BA, na.rm=TRUE), var=var(i$BA, na.rm=TRUE), i.quant, i.conf)
return(d)			
}	
G.agg <- ddply(G2, .(year), .fun=f)	
#add a column for the site and treatment and depth
G.agg$site <- 'Feather Falls'
G.agg$site <- 'Elkhorn'
G.agg$site <- 'Whitmore'

G.agg$treatment <- 'Control'
G.agg$treatment <- 'Fertilizer'
G.agg$treatment <- 'Herbicide'
G.agg$treatment <- 'HxF'

str(G.agg)
# make a new csv file of these data
write.csv(G.agg, file='EC_tree_summary.csv') 

F <- G.agg
F1 <- G.agg
F2 <- G.agg
F3 <- G.agg

### combine treatments within a site
K <- make.groups(F, F1, F2, F3)
K1 <- make.groups(F, F1, F2, F3)
K2 <- make.groups(F, F1, F2, F3)

xyplot(q_25 + median + q_75 ~ year, data=G.agg, type=c('l', 'g'))

### combine all sites
A <- make.groups(K, K1, K2)

#### order the factor for proper plotting
A$site <- factor(A$site, levels=c('Feather Falls',  'Whitmore','Elkhorn'))
A$treatment <- factor(A$treatment, levels=c('Control', 'Herbicide', 'Fertilizer', 'HxF'))

xyplot(median/10 + q_25/10 + q_75/10 ~ year | treatment + site, data=A, type=c('l', 'g'), lty=c(1,3,3), lwd=c(1.5,2,2), col=1, pch=19, cex=0.3, strip=strip.custom(bg=gray(0.9)), ylab='Basal Area - cm^2', main='GOE Basel Area - median + 1st and 3rd Quartiles', xlim=c(1985, 2010), layout=c(4,3), scales=list(x=list(tick.number=10)))
dev.copy2pdf(file='GOE_BA-median_all.pdf')



# summing for each tree
G1 <- ddply(G, .(plot, tree, year, ring), .fun=summarise, mean=mean(rad_mm), sd=sd(rad_mm), var=var(rad_mm))

# summing for each plot
G1 <- ddply(G, .(plot, year, ring), .fun=summarise, mean=mean(dia_mm), median=median(dia_mm), sd=sd(dia_mm), var=var(dia_mm))

# summing for each treatment
G1 <- ddply(G, .(treatment, year, ring), .fun=summarise, mean=mean(rad_mm), median=median(rad_mm), sd=sd(rad_mm), var=var(rad_mm))


### plot the basic results
xyplot(BA ~ year | ring, data=G1, layout=c(1,3), scales='same')
xyplot(mean ~ year, data=, col=c(1,2,3), pch=c(19,17,16), scales='free')

## subsetting to get only the diameters that represent Total
G1$year <- as.factor(G1$year)
B1 <- subset(G1, subset=G1$ring=='Total')
B2 <- subset(G1, subset=G1$ring=='EW')
B3 <- subset(G1, subset=G1$ring=='LW')

### subsetting a individual column and saving it to an new object
C <- B1[ ,5]
C1 <- B2[ ,5]
C2 <- B3[ ,5]
## the values for the years
Y <- B1[,2]
## cumulative sum function
N <- cumsum(C)# total
N1 <- cumsum(C1) # EW
N2 <- cumsum(C2) #LW
## bind them into a data frame
CS <- as.data.frame(cbind(Y, N, N1, N2))


CS$treatment <- 'Control'
CS$treatment <- 'Herbicide'
CS$treatment <- 'Fertilizer'
CS$treatment <- 'HxF'

CS$site <- 'Feather Falls'
CS$site <- 'Whitmore'
CS$site <- 'Elkhorn'

names(CS) <- c('year','total', 'EW', 'LW', 'treatment', 'site')
str(CS)

write.csv(CS, file='WHF_treeCS.csv')
CS <- read.csv('FFHF_treeCS.csv', header=TRUE)

xyplot(CS$total ~ CS$year, data=CS, ylab='Diameter - mm', xlab='years', xlim=c(0, 21), main='Cumulative Growth Whitmore - C')

plot(CS$year, CS$LW, data=CS, type='l', lwd=3, col=3,main='Cumulative Growth Whitmore - C', xlab='year')

dev.copy2pdf(file='W_cumulative_growth_C.pdf')

barchart(year ~ total, CS, breaks=21)
barchart(year ~ total, CS)

#### summary of all the 
CS <- read.csv('WC_treeCS.csv', header=TRUE)
CS1 <- read.csv('WH_treeCS.csv', header=TRUE)
CS2 <- read.csv('WF_treeCS.csv', header=TRUE)
CS3 <- read.csv('WHF_treeCS.csv', header=TRUE)


T <- make.groups(CS, CS1, CS2, CS3)
write.csv(T, file='WALL_treeCS.csv')


T <- read.csv('FFALL_treeCS.csv', header=TRUE)
T1 <- read.csv('WALL_treeCS.csv', header=TRUE)
T2<- read.csv('EALL_treeCS.csv', header=TRUE)
T3 <- make.groups(T, T1, T2)
str(T2)
xyplot(total ~ year, data=T2)
#order them correclty
T3$site <- factor(T3$site, levels=c('Elkhorn', 'Whitmore', 'Feather Falls'))


#order them correclty
T$treatment <- factor(T$treatment, levels=rev(c('Herbicide', 'Control', 'HxF', 'Fertilizer')))

tps <- list(
superpose.line=list(lwd=1.5, lty=3, col=1.5),
superpose.symbol=list(pch=c(1,11,8,17), cex=0.5, col=1),
plot.line=list(col='black'),
plot.symbol=list(col='black')
)
trellis.par.set(tps)


## for total
#a=c(250, 500, 750, 1000, 1250, 1500, 1750)
## for LW
a=c(0, 100, 200, 300, 400)
b=c(1986,1991, 1996, 2001, 2006)
G <- xyplot(LW  ~ year|site, group=treatment, data=T3, type=c('l','p','g'), lwd=1.5, strip=strip.custom(bg=gray(0.9)), ylab='Basal Area - mm^2', ylim=c(-20,400), xlim=c(1985, 2009), scales=list(x=list(tick.number=10, at=b), y=list(at=a)), auto.key=list(space='top', columns=4, line=TRUE), layout=c(3,1))


pdf(file='cumulative_BA-LW_all.pdf', width=6, height=4)
trellis.par.set(tps)
print(G)
dev.off()



dev.copy2pdf(file='GOE_EWcumulative.pdf')
