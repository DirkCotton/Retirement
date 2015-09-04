# compare our data to weighted probabilities

faildata <- simulation$failure.year
deathdata <- lifeSpanMale # simulation$death.years
rages <- 65:100
failnums <- rep(36,0)
deathnums <- failnums
wtdprob <- failnums

for (i in 1:36) {
  failnums[i] <- sum(faildata <= rages[i] )/length(faildata)
  deathnums[i] <- 1- (sum(lifeSpanMale$V1 <= rages[i])/length(lifeSpanMale$V1))
  wtdprob[i] <- 1- (failnums[i] * deathnums [i] ) # weighted prob of survival
}

allData <- data.frame(rages,failnums,deathnums,wtdprob)
allData.L <- melt(allData,id="rages")

plta <- ggplot(data=allData.L,aes(x=rages, y=value, group=variable,color=variable)) +
  scale_fill_discrete(labels=c("Our Model","Milevsky","Bengen Model")) +
  geom_line() +
  ggtitle("Weighted Probability of Ruin") +
  labs(x="Age", y="Probability of Ruin") +
  scale_y_continuous(breaks = seq(0,1,by = .05)) 
plta