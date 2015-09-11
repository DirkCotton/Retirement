# compare our data to weighted probabilities

faildata <- simulation$failure.year
deathdata <- lifeSpanMale # simulation$death.years
rages <- 65:100
pRuin <- rep(36,0)
pAlive <- pRuin
pAliveandRuined <- pRuin

for (i in 1:36) {
  pRuin[i] <- sum(faildata <= rages[i] )/length(faildata)
  pAlive[i] <- 1- (sum(lifeSpanMale$V1 <= rages[i])/length(lifeSpanMale$V1)) 
  pAliveandRuined[i] <- (pRuin[i] * pAlive [i] ) # weighted prob of survival
}

allData <- data.frame(rages,pRuin,pAlive,pAliveandRuined)
allData.L <- melt(allData,id="rages")

plta <- ggplot(data=allData.L,aes(x=rages, y=value, group=variable,color=variable)) +
  scale_fill_discrete(labels=c("Our Model","Milevsky","Bengen Model")) +
  geom_line() +
  ggtitle("Probability Alive and Ruined by Age") +
  labs(x="Age", y="Probability of Ruin") +
  scale_y_continuous(breaks = seq(0,1,by = .05),limits=c(.5,1)) 
plta