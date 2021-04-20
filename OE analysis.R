data=read.csv("OceanEcologySurveys_all_20201207.csv")
library(ggplot2)
library(lme4)
library(dplyr)
library(MuMIn)
library(GGally)
library(ggpubr)

####    data organization and exploration  #################################################
data$Month=factor(data$Month,levels=c("Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")) 
data$Yr_fac=factor(data$Year)

## check outliers  ###
quantile(data$depth.m,probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
quantile(data$Top20_Temp,probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
quantile(data$Top20_Sal,probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
quantile(data$depth.m,probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
quantile(data$Dist_to_shore_m,probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)

## remove depth outliers (outside 95th percentile) n= 27, and missing temp/sal rows n =109 ####

data=subset(data,data$Depth_m<282)
data=data[complete.cases(data$Top20_Sal),]

###  remove months with few observations, 97% of obs occur in May-Sep  ###### 
mosel=c("May","Jun","Jul","Aug","Sep")
data=subset(data,data$Month%in%mosel)
data$Month=factor(data$Month)

###   scale the covariates (mean = 0, SD = 1) ####

data$zdepth=c(scale(data$Depth_m))
data$ztemp=c(scale(data$Top20_Temp))
data$zsal=c(scale(data$Top20_Sal))
data$zlat=c(scale(data$Lat))
data$zDTS=c(scale(data$Dist_to_shore_m))

unit_inc%>%
  

#####    plot correlation between covariates   #######
ggpairs(data[,c("ztemp","zsal","zlat","zDTS")])


#### plot covariate distributions by year and month  ######

meantempYR=data %>%
  group_by(Yr_fac) %>%
  summarize(x = mean(Top20_Temp, na.rm = TRUE))
meantempMo=data %>%
  group_by(Month) %>%
  summarize(x = mean(Top20_Temp, na.rm = TRUE))
meanSalYR=data %>%
  group_by(Yr_fac) %>%
  summarize(x = mean(Top20_Sal, na.rm = TRUE))
meanSalMo=data %>%
  group_by(Month) %>%
  summarize(x = mean(Top20_Sal, na.rm = TRUE))


TemYr=ggplot(data,aes(x=Yr_fac,y=Top20_Temp))+geom_violin(alpha=.75)+geom_point(data=meantempYR,aes(x=Yr_fac,y=x))+
  geom_line(data=meantempYR,aes(x=as.numeric(Yr_fac),y=x))+
  theme(axis.text.x=element_blank())+xlab("")+ylab("Top 20m Temperature (C)")
TemMo=ggplot(data,aes(x=Month,y=Top20_Temp))+geom_violin(alpha=.75)+geom_point(data=meantempMo,aes(x=Month,y=x))+
  geom_line(data=meantempMo,aes(x=as.numeric(Month),y=x))+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())+xlab("")+ylab("")
SalYr=ggplot(data,aes(x=Yr_fac,y=Top20_Sal))+geom_violin(alpha=.75)+xlab("Year")+geom_point(data=meanSalYR,aes(x=Yr_fac,y=x))+
  geom_line(data=meanSalYR,aes(x=as.numeric(Yr_fac),y=x))+
  ylab("Top 20m Salinity (ppt)")+theme(axis.text.x=element_text(angle=45,hjust=1))
SalMo=ggplot(data,aes(x=Month,y=Top20_Sal))+geom_violin(alpha=.75)+geom_point(data=meanSalMo,aes(x=Month,y=x))+
  geom_line(data=meanSalMo,aes(x=as.numeric(Month),y=x))+
  theme(axis.text.y=element_blank())+ylab("")
ggarrange(TemYr,TemMo,SalYr,SalMo,ncol=2,nrow=2,common.legend = TRUE)



####  linear mixed models   #######################################################
NoAn1=glmer(Northern_anchovy~ztemp+zsal+poly(Lat,2)+zDTS+(1|Yr_fac)+(1|Month),data,family=binomial())
lattice::dotplot(ranef(NoAn1, which = "Yr_fac", condVar = TRUE))
lattice::dotplot(ranef(NoAn1, which = "Month", condVar = TRUE))
tmpdat=data[,c("ztemp","zsal","Lat","zDTS","Month","Yr_fac")]
NoAnj=with(data,seq(from=min(zsal),to=max(zsal),length.out = 100))
pp <- lapply(NoAnj, function(j) {
  tmpdat$zsal <- j
  predict(NoAn1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, NoAnj))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Salinity")
plotdat$SalRS=(plotdat$Salinity*1.32)+31.78  ####  rescale salinity to original values, mean=31.78, sd=1.32 ####
NASal=ggplot(plotdat, aes(x = SalRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Salinity")
NoAnl=with(data,seq(from=min(Lat),to=max(Lat),length.out = 100))
pp <- lapply(NoAnl, function(j) {
  tmpdat$Lat <- j
  predict(NoAn1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, NoAnl))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Latitude")
plotdat$SalRS=(plotdat$Salinity*1.32)+31.78  ####  rescale salinity to original values, mean=31.78, sd=1.32 ####
NALat=ggplot(plotdat, aes(x = Latitude, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))
ggarrange(NASal,NALat,ncol=2)


#############   Pacific sardine  #######################################################################
PaSd1=glmer(Pacific_sardine~ztemp+zsal+poly(Lat,2)+zDTS+(1|Yr_fac)+(1|Month),data,family=binomial())

PaSdt=with(data,seq(from=min(ztemp),to=max(ztemp),length.out = 100))
pp <- lapply(PaSdt, function(j) {
  tmpdat$ztemp <- j
  predict(PaSd1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, PaSdt))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Temperature")
plotdat$tempRS=(plotdat$Temperature*1.54)+11.39  ####  rescale temp to original values, mean=11.39, sd=1.54 ####
PSDtemp=ggplot(plotdat, aes(x = tempRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Temperature")

Pasds=with(data,seq(from=min(zsal),to=max(zsal),length.out = 100))
pp <- lapply(Pasds, function(j) {
  tmpdat$zsal <- j
  predict(PaSd1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, Pasds))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Salinity")
plotdat$SalRS=(plotdat$Salinity*1.32)+31.78  ####  rescale salinity to original values, mean=31.78, sd=1.32 ####
PSDSal=ggplot(plotdat, aes(x = SalRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Salinity")


Pasdl=with(data,seq(from=min(Lat),to=max(Lat),length.out = 100))
pp <- lapply(Pasdl, function(j) {
  tmpdat$Lat <- j
  predict(PaSd1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, Pasdl))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Latitude")
PSDLat=ggplot(plotdat, aes(x = Latitude, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))

Pasdm=with(data,seq(from=min(zDTS),to=max(zDTS),length.out = 100))
pp <- lapply(Pasdm, function(j) {
  tmpdat$zsal <- j
  predict(PaSd1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, Pasdm))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "zDTS")
plotdat$dtsRS=(plotdat$zDTS*12418.88)+19501.75  ####  rescale salinity to original values, mean=19501.75, sd=12418.88 ####
PSDdts=ggplot(plotdat, aes(x = dtsRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Distance offshore (m)")

ggarrange(PSDtemp,PSDSal,PSDLat,PSDdts,ncol=2,nrow=2)
####################  Pacific Mackerel    ######################################################
PaMa1=glmer(Pacific_chub_mackerel~ztemp+zsal+poly(Lat,2)+zDTS+(1|Yr_fac)+(1|Month),data,family=binomial())

PaMat=with(data,seq(from=min(ztemp),to=max(ztemp),length.out = 100))
pp <- lapply(PaMat, function(j) {
  tmpdat$ztemp <- j
  predict(PaMa1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, PaMat))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Temperature")
plotdat$tempRS=(plotdat$Temperature*1.54)+11.39  ####  rescale temp to original values, mean=11.39, sd=1.54 ####
PMKtemp=ggplot(plotdat, aes(x = tempRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Temperature")

PaMal=with(data,seq(from=min(Lat),to=max(Lat),length.out = 100))
pp <- lapply(PaMal, function(j) {
  tmpdat$Lat <- j
  predict(PaMa1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, PaMal))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Latitude")
PMKLat=ggplot(plotdat, aes(x = Latitude, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))

PaMam=with(data,seq(from=min(zDTS),to=max(zDTS),length.out = 100))
pp <- lapply(PaMam, function(j) {
  tmpdat$zsal <- j
  predict(PaMa1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, PaMam))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "zDTS")
plotdat$dtsRS=(plotdat$zDTS*12418.88)+19501.75  ####  rescale salinity to original values, mean=19501.75, sd=12418.88 ####
PMKdts=ggplot(plotdat, aes(x = dtsRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Distance offshore (m)")

ggarrange(PMKtemp,PMKLat,PMKdts,ncol=2,nrow=2)


############  JAck Mackerel  #############################################################################
JaMa1=glmer(Jack_mackerel~ztemp+zsal+poly(Lat,2)+zDTS+(1|Yr_fac)+(1|Month),data,family=binomial())


JaMat=with(data,seq(from=min(ztemp),to=max(ztemp),length.out = 100))
pp <- lapply(JaMat, function(j) {
  tmpdat$ztemp <- j
  predict(JaMa1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, JaMat))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Temperature")
plotdat$tempRS=(plotdat$Temperature*1.54)+11.39  ####  rescale temp to original values, mean=11.39, sd=1.54 ####
JMtemp=ggplot(plotdat, aes(x = tempRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Temperature")

JaMal=with(data,seq(from=min(Lat),to=max(Lat),length.out = 100))
pp <- lapply(JaMal, function(j) {
  tmpdat$Lat <- j
  predict(JaMa1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, JaMal))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Latitude")
JMLat=ggplot(plotdat, aes(x = Latitude, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))

JaMam=with(data,seq(from=min(zDTS),to=max(zDTS),length.out = 100))
pp <- lapply(JaMam, function(j) {
  tmpdat$zsal <- j
  predict(JaMa1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, JaMam))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "zDTS")
plotdat$dtsRS=(plotdat$zDTS*12418.88)+19501.75  ####  rescale salinity to original values, mean=19501.75, sd=12418.88 ####
JMdts=ggplot(plotdat, aes(x = dtsRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Distance offshore (m)")

ggarrange(JMtemp,JMLat,JMdts,ncol=2,nrow=2)



###############  Market Squid   ######################################################################
MaSq1=glmer(CA_market_squid~ztemp+zsal+poly(Lat,2)+zDTS+(1|Yr_fac)+(1|Month),data,family=binomial())

MaSql=with(data,seq(from=min(Lat),to=max(Lat),length.out = 100))
pp <- lapply(MaSql, function(j) {
  tmpdat$Lat <- j
  predict(MaSq1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, MaSql))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Latitude")
MSLat=ggplot(plotdat, aes(x = Latitude, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))

MaSqm=with(data,seq(from=min(zDTS),to=max(zDTS),length.out = 100))
pp <- lapply(MaSqm, function(j) {
  tmpdat$zsal <- j
  predict(MaSq1, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, MaSqm))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "zDTS")
plotdat$dtsRS=(plotdat$zDTS*12418.88)+19501.75  ####  rescale salinity to original values, mean=19501.75, sd=12418.88 ####
MSdts=ggplot(plotdat, aes(x = dtsRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Distance offshore (m)")

ggarrange(MSLat,MSdts,ncol=2)


PaHe1=glmer(Pacific_herring~ztemp+zsal+poly(Lat,2)+zDTS+(1|Yr_fac)+(1|Month),data,family=binomial())


PaHem=with(data,seq(from=min(zDTS),to=max(zDTS),length.out = 100))
pp <- lapply(PaHem, function(j) {
  tmpdat$zsal <- j
  predict(PaHe2, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, PaHem))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "zDTS")
plotdat$dtsRS=(plotdat$zDTS*12418.88)+19501.75  ####  rescale salinity to original values, mean=19501.75, sd=12418.88 ####
PHdts=ggplot(plotdat, aes(x = dtsRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Distance offshore (m)")

PaHet=with(data,seq(from=min(ztemp),to=max(ztemp),length.out = 100))
pp <- lapply(PaHet, function(j) {
  tmpdat$ztemp <- j
  predict(PaHe2, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, PaHet))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Temperature")
plotdat$tempRS=(plotdat$Temperature*1.54)+11.39  ####  rescale temp to original values, mean=11.39, sd=1.54 ####
PHtemp=ggplot(plotdat, aes(x = tempRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Temperature")

PaHel=with(data,seq(from=min(Lat),to=max(Lat),length.out = 100))
pp <- lapply(PaHel, function(j) {
  tmpdat$Lat <- j
  predict(PaHe2, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, PaHel))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Latitude")
PHLat=ggplot(plotdat, aes(x = Latitude, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))


##########  Effort and proportion of observations ###################
obsYr=read.csv("ObsByYear.csv")
NAobs<-ggplot(obsYr,aes(x=Year,y=NoAn))+geom_bar(stat="identity")+theme_bw()+ylab("Proportion of observations")+
  annotate("text", x = 2019, y = 0.73, label = "A",size=7.5)+xlab("")
MSobs=ggplot(obsYr,aes(x=Year,y=MaSq))+geom_bar(stat="identity")+theme_bw()+ylab("Proportion of observations")
JMobs=ggplot(obsYr,aes(x=Year,y=JaMa))+geom_bar(stat="identity")+theme_bw()+ylab("Proportion of observations")+ylim(0,0.6)
PMobs=ggplot(obsYr,aes(x=Year,y=PaMa))+geom_bar(stat="identity")+theme_bw()+ylab("Proportion of observations")+ylim(0,0.6)
PSobs=ggplot(obsYr,aes(x=Year,y=PaSa))+geom_bar(stat="identity")+theme_bw()+ylab("Proportion of observations")+ylim(0,0.6)

NA_pp=c(0,0.007,0.093,0.148,0.196)
PS_pp=c(0,0.144,0,0,0)
pp_yr=c(1997,1998,1999,2000,2001)
ppobsYr=data.frame(NA_pp,PS_pp,pp_yr)
NApp=ggplot(ppobsYr,aes(x=pp_yr,y=NA_pp))+geom_bar(stat="identity")+theme_bw()+ylab("")+xlab("")+ylim(0,0.7)+
annotate("text", x = 2001.4, y = 0.70, label = "B",size=7.5)
PSpp=ggplot(ppobsYr,aes(x=pp_yr,y=PS_pp))+geom_bar(stat="identity")+theme_bw()+ylab("Proportion of observations")+xlab("")
colNames <- names(obsYr)[2:5]
plotList=list()
for (i in colNames){
  pname<-obsYr[,i]
  p<-ggplot(obsYr,aes_string(x=obsYr[,1] ,y=obsYr[,i]))+geom_bar(stat="identity")+theme_bw()
  ggsave(file=paste0(pname,".png"),p)
  plotList[[i]]<-p
}