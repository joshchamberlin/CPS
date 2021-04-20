shelf=read.csv("Combined_reAn.csv")
shelf$Year=factor(shelf$Year)
## check outliers  ###
quantile(shelf$Top20m_Temp,probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
quantile(shelf$Top20m_Sal,probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
quantile(shelf$NEAR_DIST,probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)

## remove depth outliers (outside 95th percentile) n= 27, and missing temp/sal rows n =109 ####

shelf=subset(shelf,shelf$Top20m_Sal<33.65 & shelf$Top20m_Sal>30.77)
shelf=subset(shelf,shelf$Top20m_Temp<14.2 & shelf$Top20m_Temp>8.8)
shelf=subset(shelf,shelf$NEAR_DIST<46158.17 & shelf$NEAR_DIST>2447.56)

shelf=shelf[complete.cases(shelf$Top20m_Sal),]
shelf=shelf[complete.cases(shelf$Top20m_Temp),]




###   scale the covariates (mean = 0, SD = 1) ####

pred$zdepth=zscore(pred$Depth_m)
pred$ztemp=zscore(pred$Top20_Temp)
pred$zsal=zscore(pred$Top20_Sal)
pred$zlat=zscore(pred$Lat)
pred$zDTS=zscore(pred$Dist_to_shore_m)


shelf$ztemp=zscore(shelf$Top20m_Temp)
shelf$zsal=zscore(shelf$Top20m_Sal)
shelf$zlat=zscore(shelf$Lat)
shelf$zDTS=zscore(shelf$NEAR_DIST)


#####    plot correlation between covariates   #######

ggpairs(pred[,c("ztemp","zsal","zlat","zDTS")])
ggpairs(shelf[,c("ztemp","zsal","zlat","zDTS")])


meantempYRp=pred %>%
  group_by(Yr_fac) %>%
  summarize(x = mean(Top20_Temp, na.rm = TRUE))
meantempMop=pred %>%
  group_by(Month) %>%
  summarize(x = mean(Top20_Temp, na.rm = TRUE))
meanSalYRp=pred %>%
  group_by(Yr_fac) %>%
  summarize(x = mean(Top20_Sal, na.rm = TRUE))
meanSalMop=pred %>%
  group_by(Month) %>%
  summarize(x = mean(Top20_Sal, na.rm = TRUE))


TemYrp=ggplot(pred,aes(x=Yr_fac,y=Top20_Temp))+geom_violin(alpha=.75)+geom_point(data=meantempYRp,aes(x=Yr_fac,y=x))+
  geom_line(data=meantempYRp,aes(x=as.numeric(Yr_fac),y=x))+
  theme(axis.text.x=element_blank())+xlab("")+ylab("Top 20m Temperature (C)")
TemMop=ggplot(pred,aes(x=Month,y=Top20_Temp))+geom_violin(alpha=.75)+geom_point(data=meantempMop,aes(x=Month,y=x))+
  geom_line(data=meantempMop,aes(x=as.numeric(Month),y=x))+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())+xlab("")+ylab("")
SalYrp=ggplot(pred,aes(x=Yr_fac,y=Top20_Sal))+geom_violin(alpha=.75)+xlab("Year")+geom_point(data=meanSalYRp,aes(x=Yr_fac,y=x))+
  geom_line(data=meanSalYRp,aes(x=as.numeric(Yr_fac),y=x))+
  ylab("Top 20m Salinity (ppt)")+theme(axis.text.x=element_text(angle=45,hjust=1))
SalMop=ggplot(pred,aes(x=Month,y=Top20_Sal))+geom_violin(alpha=.75)+geom_point(data=meanSalMop,aes(x=Month,y=x))+
  geom_line(data=meanSalMop,aes(x=as.numeric(Month),y=x))+
  theme(axis.text.y=element_blank())+ylab("")
ggarrange(TemYrp,TemMop,SalYrp,SalMop,ncol=2,nrow=2,common.legend = TRUE)



meantempYRs=shelf %>%
  group_by(Year) %>%
  summarize(x = mean(Top20m_Temp, na.rm = TRUE))
meantempMos=shelf %>%
  group_by(Month) %>%
  summarize(x = mean(Top20m_Temp, na.rm = TRUE))
meanSalYRs=shelf %>%
  group_by(Year) %>%
  summarize(x = mean(Top20m_Sal, na.rm = TRUE))
meanSalMos=shelf %>%
  group_by(Month) %>%
  summarize(x = mean(Top20m_Sal, na.rm = TRUE))


TemYrs=ggplot(shelf,aes(x=Year,y=Top20m_Temp))+geom_violin(alpha=.75)+geom_point(data=meantempYRs,aes(x=Year,y=x))+
  geom_line(data=meantempYRs,aes(x=as.numeric(Year),y=x))+
  theme(axis.text.x=element_blank())+xlab("")+ylab("Top 20m Temperature (C)")
TemMos=ggplot(shelf,aes(x=Month,y=Top20m_Temp))+geom_violin(alpha=.75)+geom_point(data=meantempMos,aes(x=Month,y=x))+
  geom_line(data=meantempMos,aes(x=as.numeric(Month),y=x))+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())+xlab("")+ylab("")
SalYrs=ggplot(shelf,aes(x=Year,y=Top20m_Sal))+geom_violin(alpha=.75)+xlab("Year")+geom_point(data=meanSalYRs,aes(x=Year,y=x))+
  geom_line(data=meanSalYRs,aes(x=as.numeric(Year),y=x))+
  ylab("Top 20m Salinity (ppt)")+theme(axis.text.x=element_text(angle=45,hjust=1))
SalMos=ggplot(shelf,aes(x=Month,y=Top20m_Sal))+geom_violin(alpha=.75)+geom_point(data=meanSalMos,aes(x=Month,y=x))+
  geom_line(data=meanSalMos,aes(x=as.numeric(Month),y=x))+
  theme(axis.text.y=element_blank())+ylab("")
ggarrange(TemYrs,TemMos,SalYrs,SalMos,ncol=2,nrow=2,common.legend = TRUE)


#### Northern Anchovy with predator data ######
NoAn1p=glmer(Northern_anchovy~ztemp+zsal+poly(Lat,2)+zDTS+Month+(1|Yr_fac),pred,family=binomial())
NoAn2p=glmer(Northern_anchovy~ztemp*Month+zsal*Month+poly(Lat,2)*Month+zDTS*Month+(1|Yr_fac),pred,family=binomial(),na.action=na.fail)
NoAn2s=glmer(NorthernAnchovy~ztemp*Month+zsal*Month+poly(Lat,2)*Month+zDTS*Month+(1|Year),shelf,family=binomial())
NoAn2sfull=glmer(NorthernAnchovy~ztemp*Month+zsal*Month+zlat*Month+zDTS*Month+
                   ztemp*zsal+ztemp*zlat+ztemp*zDTS+
                   +zsal*zlat+zsal*zDTS+
                   +zDTS*zlat+(1|Year),shelf,family=binomial(),na.action = na.pass)





lattice::dotplot(ranef(NoAn1p, which = "Yr_fac", condVar = TRUE))
#lattice::dotplot(ranef(NoAn1p, which = "Month", condVar = TRUE))
tmpdat=pred[,c("ztemp","zsal","Lat","zDTS","Month","Yr_fac")]
NoAnjp=with(pred,seq(from=min(zsal),to=max(zsal),length.out = 100))
pp <- lapply(NoAnjp, function(j) {
  tmpdat$zsal <- j
  predict(NoAn1p, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, NoAnjp))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Salinity")
plotdat$SalRS=(plotdat$Salinity*1.26)+31.26  ####  rescale salinity to original values, mean=31.26, sd=1.26 ####
NASalp=ggplot(plotdat, aes(x = SalRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Salinity")
NoAntp=with(pred,seq(from=min(ztemp),to=max(ztemp),length.out = 100))
pp <- lapply(NoAntp, function(j) {
  tmpdat$ztemp <- j
  predict(NoAn1p, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, NoAntp))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Temperature")
plotdat$TempRS=(plotdat$Temperature*1.52)+11.47  ####  rescale temp to original values, mean=11.47, sd=1.52 ####
NATemp=ggplot(plotdat, aes(x = TempRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))
ggarrange(NASalp,NATemp,ncol=2)

NoAndp=with(pred,seq(from=min(zDTS),to=max(zDTS),length.out = 100))
pp <- lapply(NoAndp, function(j) {
  tmpdat$zDTS <- j
  predict(NoAn1p, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, NoAndp))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "DTS")
plotdat$DTSRS=(plotdat$DTS*11.64)+22.91  ####  rescale DTS to original values and display as km, mean=22.91, sd=11.64 ####
NADTSp=ggplot(plotdat, aes(x = DTSRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))


#### Northern Anchovy with shelf data ######
NoAn_shelf=glmer(NorthernAnchovy~ztemp*Month+zsal*Month+zlat*Month+zDTS*Month+(1|Year),shelf,family=binomial())



NoAn1s=glmer(Northern_anchovy~ztemp+zsal+poly(Lat,2)+zDTS+Month+(1|Yr_fac),shelf,family=binomial())
NoAnjs=with(shelf,seq(from=min(zsal),to=max(zsal),length.out = 100))
pp <- lapply(NoAnjs, function(j) {
  tmpdat$zsal <- j
  predict(NoAn1s, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, NoAnjs))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Salinity")
plotdat$SalRS=(plotdat$Salinity*1.29)+31.96  ####  rescale salinity to original values, mean=31.96, sd=1.29 ####
NASals=ggplot(plotdat, aes(x = SalRS, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))+xlab("Salinity")
NoAnls=with(shelf,seq(from=min(Lat),to=max(Lat),length.out = 100))
pp <- lapply(NoAnls, function(j) {
  tmpdat$Lat <- j
  predict(NoAn1s, newdata = tmpdat, type = "response")
})
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, NoAnls))
colnames(plotdat) <- c("ProbabilityPresence", "Lower", "Upper", "Latitude")
plotdat$SalRS=(plotdat$Salinity*1.32)+31.78  ####  rescale salinity to original values, mean=31.78, sd=1.32 ####
NALats=ggplot(plotdat, aes(x = Latitude, y = ProbabilityPresence)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + 
  geom_line(size = 2) + ylim(c(0, 1))
ggarrange(NASals,NALats,ncol=2)


####  pacific sardine with predator data ####
PaSd1p=glmer(Pacific_sardine~ztemp+zsal+poly(Lat,2)+zDTS+Month+(1|Yr_fac),pred,family=binomial())
PaSd2p=glmer(Pacific_sardine~ztemp*Month+zsal*Month+L_fac*Month+zDTS*Month+(1|Yr_fac),pred,family=binomial(),na.action=na.fail)
PSp_drg=dredge(PaSd2p)
PSp_best=get.models(PSp_drg,1)[[1]]
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

####################  Pacific Mackerel   (predator) ######################################################
PaMa2p=glmer(Pacific_chub_mackerel~ztemp*Month+zsal*Month+L_fac*Month+zDTS*Month+(1|Yr_fac),pred,family=binomial(),na.action=na.fail)
PMp_drg=dredge(PaMa2p)
PMp_best=get.models(PMp_drg,1)[[1]]

ggpredict(PaMa2p,c("zDTS","Month"))%>%plot
ggpredict(PaMa2p,c("ztemp[all]","Month"))%>%plot

ggpredict(PaMa2p,c("zsal[all]","Month"))%>%plot

############  JAck Mackerel  #############################################################################
JaMa2p=glmer(Jack_mackerel~ztemp*Month+zsal*Month+L_fac*Month+zDTS*Month+(1|Yr_fac),pred,family=binomial(),na.action=na.fail)
JMp_drg=dredge(JaMa2p)
JMp_best=get.models(JMp_drg,1)[[1]]

ggpredict(JaMa2p,c("Month"))%>%plot
ggpredict(JaMa2p,c("zDTS[all]"))%>%plot
ggpredict(JaMa2p,c("ztemp[all]","Month"))%>%plot

###############  Market Squid   ######################################################################
MaSq2p=glmer(CA_market_squid~ztemp*Month+zsal*Month+L_fac*Month+zDTS*Month+(1|Yr_fac),pred,family=binomial(),na.action=na.fail)
MS_drg=dredge(MaSq2p)
MSp_best=get.models(MS_drg,1)[[1]]
summary(MSp_best)

ggpredict(MaSq2p,c("zDTS[all]","Month"))%>%plot
ggpredict(MaSq2p,c("ztemp[all]","Month"))%>%plot

#### shelf analysis ######

NoAn_shelf=glmer(NorthernAnchovy~ztemp*Month+zsal*Month+zlat*Month+zDTS*Month+(1|Year),shelf,family=binomial(),na.action=na.fail)
NoAn_drg=dredge(NoAn_shelf)
NAbest=get.models(NoAn_drg, 1)[[1]]
summary(NAbest)
ggpredict(NAbest,c("zDTS[all]","Month [May,June,September]"))%>%plot

ggpredict(NAbest,c("zlat[all]","Month [May,June,August,September]"))%>%plot(ci.style="dash")
ggpredict(NAbest,c("zsal[all]"))%>%plot()
ggpredict(NAbest,c("ztemp[all]"))%>%plot()


PaSa_shelf=glmer(PacificSardine~ztemp*Month+zsal*Month+zlat*Month+zDTS*Month+(1|Year),shelf,family=binomial(),na.action=na.fail)
PaSa_drg=dredge(PaSa_shelf)
PSbest=get.models(PaSa_drg, 1)[[1]]
summary(PSbest)


MaSq_shelf=glmer(MarketSquid~ztemp*Month+zsal*Month+zlat*Month+zDTS*Month+(1|Year),shelf,family=binomial(),na.action=na.fail)
MSbest=get.models(MaSq_drg, 1)[[1]]
summary(MSbest)


PaHe_shelf=glmer(PacificHerring~ztemp*Month+zsal*Month+zlat*Month+zDTS*Month+(1|Year),shelf,family=binomial(),na.action=na.fail)
PaHe_drg=dredge(PaHe_shelf)
PHbest=get.models(PaHe_drg, 1)[[1]]
summary(PHbest)

PaMa_shelf=glmer(PacificMackerel~ztemp*Month+zsal*Month+zlat*Month+zDTS*Month+(1|Year),shelf,family=binomial(),na.action=na.fail)
PM_drg=dredge(PaMa_shelf)
PMbest=get.models(PM_drg, 1)[[1]]
summary(PMbest)

JMshelf=subset(shelf,shelf$Month!="August")
JaMa_shelf=glmer(JackMackerel~ztemp*Month+zsal*Month+zlat*Month+zDTS*Month+(1|Year),JMshelf,family=binomial(),na.action=na.fail)
JM_drg=dredge(JaMa_shelf)
JMbest=get.models(JM_drg, 1)[[1]]
summary(JMbest)


PaSr_shelf=glmer(PacificSaury~ztemp*Month+zsal*Month+zlat*Month+zDTS*Month+(1|Year),shelf,family=binomial(),na.action=na.fail)
PSr_drg=dredge(PaSr_shelf)
PSrbest=get.models(PSr_drg, 1)[[1]]
summary(PSrbest)
