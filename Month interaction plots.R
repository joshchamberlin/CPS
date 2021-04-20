NoAnlp=with(pred,seq(from=min(Lat),to=max(Lat),length.out = 100))
mos <- lapply(levels(pred$Month), function(mo) {
  tmpdat$Month[] <- mo
  lapply(NoAnlp, function(j) {
    tmpdat$Lat <- j
    predict(NoAn1p_int, newdata = tmpdat, type = "response")
  })
})

plotdat2 <- lapply(mos, function(X) {
  temp <- t(sapply(X, function(x) {
    c(M=mean(x), quantile(x, c(.25, .75)))
  }))
  temp <- as.data.frame(cbind(temp, NoAnlp))
  colnames(temp) <- c("PredictedProbability", "Lower", "Upper", "Latitude")
  return(temp)
})

plotdat2 <- do.call(rbind, plotdat2)
plotdat2$Month <- factor(rep(levels(pred$Month), each = length(NoAnlp)))

latplot=ggplot(plotdat2, aes(x = Latitude, y = PredictedProbability)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper,fill=Month,alpha=0.25)) +
  geom_line(aes(color=Month), size = 1) 

NoAnlp=with(pred,seq(from=min(zDTS),to=max(zDTS),length.out = 100))
mos <- lapply(levels(pred$Month), function(mo) {
  tmpdat$Month[] <- mo
  lapply(NoAnlp, function(j) {
    tmpdat$zDTS <- j
    predict(NoAn1p_int, newdata = tmpdat, type = "response")
  })
})

plotdat2 <- lapply(mos, function(X) {
  temp <- t(sapply(X, function(x) {
    c(M=mean(x), quantile(x, c(.25, .75)))
  }))
  temp <- as.data.frame(cbind(temp, NoAnlp))
  colnames(temp) <- c("PredictedProbability", "Lower", "Upper", "DTS")
  return(temp)
})

plotdat2 <- do.call(rbind, plotdat2)
plotdat2$Month <- factor(rep(levels(pred$Month), each = length(NoAnlp)))
plotdat2$DTS=(plotdat2$DTS*11.64)+22.91  ####  rescale DTS to original values and display as km, mean=22.91, sd=11.64 ####

DTSplot=ggplot(plotdat2, aes(x = DTS, y = PredictedProbability)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper,fill=Month)) +
  geom_line(aes(color=Month), size = 1) 




### Norther Anchovy (predator) ####
ggpredict(NoAn2p,c("L_fac","Month"))%>%plot
ggpredict(NoAn2p,c("zDTS[all]","Month"))%>%plot
ggpredict(NoAn2p,c("ztemp","Month"))%>%plot

### Apcifc Sardine (predaotr) ###

ggpredict(PaSd2p,c("zDTS","Month"))%>%plot
ggpredict(PaSd2p,c("ztemp[all]","Month"))%>%plot

ggpredict(PaSd2p,c("zsal[all]","Month"))%>%plot

