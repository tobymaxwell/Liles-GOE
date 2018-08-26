## simple graphing and statistics for tree ring data

library(lattice)
library(plyr)
G <- read.csv('GOE tree diameter.csv', header=TRUE)

#test for normality Use this to identify if the data is conforms to the assumption of normality
shapiro.test(G$diameter)

# linear effects model 
G1<-lm(diameter ~ Treatment*Site, G)
summary(G1)
anova(G1)

op <- par(mfrow = c(2, 2))
plot(G1)

# creating a limit on the data set
G$diameter[G$diameter > 10]

#to inex individual rows or columns
G[1,]

G[,2]

# trying to split a column into constitutant parts
G1 <- split(G$Treatment, G$Site)

# 
G1 <- c(C, F,H, HF)
names(G1) <- c('gray', 'green', 'brown', 'blue')
G1


sort(G$diameter)


# ordering the sites as I want them
G$Site <- factor(G$Site, level=c('Elkhorn', 'Whitmore', 'Feather Falls'), ordered=FALSE)


G$Treatment <- factor(G$Treatment, level=c('Control', 'Herbicide', 'Fertilizer', 'Herb x Fert'), ordered=FALSE)

# to rename them to simplify for the graph
G$Treatment <- factor(G$Treatment, level=c('Control', 'Herbicide', 'Fertilizer', 'Herb x Fert'), labels=c('C', 'H', 'F', 'HF'))
# with the notch added
bwplot(diameter ~ Treatment | Site, data=G, main='Tree Diameter year 20',  ylab='cm', layout=c(3,1), col='black', cex=0.9, auto.key=TRUE, strip=strip.custom(bg=gray(0.9)))

dev.copy2pdf(file='GOE Tree dia bwplot.pdf')


# Trying to order the sites within the plot
bwplot(diameter ~ Treatment | Site, data=G, main='GOE tree diameters',  ylab='cm', layout=c(3,1), col='black', strip=strip.custom(bg=gray(0.9)))

# all sites
dotplot(diameter ~ Site, data=G, main='GOE tree diameters', pch=11, bg=par('bg'), ylab='cm', col='black', strip=strip.custom(bg=gray(0.9)), border='gray')


# with the notch added this turned off one of the sites by using groups and subset
dotplot(diameter ~ Site, data=G, main='GOE tree diameters', groups=Site, subset=Site != 'Whitmore', pch=11, bg=par('bg'), ylab='cm', col='black',strip=strip.custom(bg=gray(0.9)), border='gray')


# with the as.numeric call each treatment gets a color
dotplot(diameter ~ Treatment | Site, data=G, layout=c(1,3), main='GOE tree diameters', ylab='cm', col=as.numeric(G$Site),strip=strip.custom(bg=gray(0.9)), border='gray')

# creating a legend
legend(-3,2.5, c('Elkhorn', 'Feather Falls', 'Whitmore'), col=c(1,2,3), text.col=c(1,2,3), pch=c(19,23,24), bg='gray90')

# make a density plot of all the data
densityplot(diameter ~ Site, data=G, main='GOE tree diameters', type='l', ylab='cm', col='black')

# save to a different object 
p <- bwplot(diameter ~ Treatment | Site, data=G, main='GOE tree diameters',  ylab='cm', layout=c(3,1), col='black', strip=strip.custom(bg=gray(0.9)), border='gray', notch=TRUE)


# change an aspect of my plot
p$fontface <- TRUE

#make a PDF
dev.copy2pdf(file='GOE Tree dia bwplot.pdf')


#making a histogram col- color to fill, borders- color of lines, breaks = specify the # of hist breaks
hist(G$diameter, data=G, main='GOE tree diameters - ALL', xlab='cm', breaks=10, xlim=range(0,35), ylim=c(0,20), col='gray', border='orange')


# this will show the density
hist(G$diameter, data=G, main='GOE tree diameters - ALL', freq=FALSE, breaks = 10, xlab='cm', xlim=range(0,35), ylim=c(0,20), col='gray', border='orange', lines(density(G$diameter)))


# what about the legend(2,9, c('Elkhorn', 'Whitmore', ' Feather Falls'), fill= c('gray', 'red', 'darg green')



############################
f <- function(i) 
{
	
i.conf <- data.frame(t(t.test(i$diameter, conf.level=0.95, na.action='na.omit')$conf.int))
names(i.conf) <- c('lower', 'upper')
	
p <- c(0.05, 0.25, 0.5, 0.75, 0.95)
i.quant <- data.frame(t(quantile(i$, probs=p, na.rm=TRUE)))
names(i.quant) <- paste('q', round(p*100), sep='_')

d <- data.frame(sum=sum(i$diameter, na.rm=TRUE), mean=mean(i$diameter, na.rm=TRUE), median=median(i$diameter, na.rm=TRUE), min=min(i$diameter, na.rm=TRUE), max=max(i$diameter, na.rm=TRUE), sd=sd(i$diameter, na.rm=TRUE), var=var(i$diameter, na.rm=TRUE), i.quant, i.conf)
	
return(d)		
	
}

G.agg <- ddply(G, .(diameter), .fun=f)	