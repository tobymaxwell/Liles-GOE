goeiso<-read.csv("/Users/Maxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/GOE_isotopes.csv")
goeppt<-read.csv("/Users/Maxwell/OneDrive - University Of Oregon/Oregon/Papers/Garrett JGR/GOE_annual_PT.csv")
str(goeiso)

m1<-lm(d13C~treatment*site, goeiso)
anova(m1)
m2<-lm(d13C~treatment*site + d18O, goeiso)
anova(m2)
goeiso$syt<-paste0(goeiso$site, goeiso$year, goeiso$treatment)
goeiso$sy<-paste0(goeiso$site, goeiso$year)

library(dplyr)
goe2<-goeiso %>% 
  group_by(sy) %>% 
    filter(treatment=="control") %>%
    mutate(control18O = mean(d18O))%>%
    summarize(meanctrl=mean(control18O))
  goe2<-data.frame(goe2)
  
goeiso<-merge(goeiso, goe2, all=T)
goeiso2<-merge(goeiso, goeppt[2:8])
goe3<-ddply(goeiso2, .(site, treatment, year), summarise, 
            d18O = mean(d18O),
            meanctrl = mean(meanctrl),
            d13C=mean(d13C),
            BA=mean(BA_mm),
            iWUE=mean(iWUE))

goe4<-merge(goe3, goeppt[2:8])


library(ggplot2)
ggplot(goe4, aes(y=scale((d18O/meanctrl)), x=year))+geom_line(color = "Red")+facet_grid(treatment~site)+theme_bw()+ylab("d18O (Red), iWUE (black), ppt(Blue)")+geom_line(data=goe4, color = "blue", aes(y=scale(pct_avg), x=year))


df <- data.frame(id1=gl(2, 3, labels=c("a", "b")),
                 id2=rep(gl(3, 1, labels=c("live1", "live2", "killed")), 2), 
                 y=c(10, 10, 1, 12, 12, 2),
                 otherFactor = gl(3, 2))
df
library(plyr)
df2 <- ddply(df, .(id1), transform, y = y-y[id2=="killed"])
df2[-which(df2$id2=="killed"),]
