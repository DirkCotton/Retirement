#
# Graph Rates of Ruin for Our Cary model, Milevsky Formulw, Bengen-style model
# 

library("reshape2")
library("ggplot2")
withdraw <- c(.025,.03,.035,.04,.045,.05,.055)
ourModel <- c(0,.0097,.026,.056,.1,.161,.233)
milevsky <- c(.0169,.03,.0483,.0711,.0985,.1301,.1652)
bengen <- c(.0086,.0272,.0716,.1396,.233,.3505,.47)
data <- data.frame(withdraw,ourModel,milevsky,bengen)

data_long <- melt(data, id="withdraw")  # convert to long format
colnames(data_long) <- c("Annual Withdrawal","Method","pRuin")

ggplot(data=data_long,
  aes(x=data_long$`Annual Withdrawal`, y=pRuin, group=data_long$Model,color=Method)) +
  scale_fill_discrete(labels=c("Our Model","Milevsky","Bengen Model")) +
  geom_line() +
  ggtitle("Probability of Ruin by Three Different Estimating Methods") +
  labs(x="Annual Withdrawal Rate", y="Probability of Ruin") +
  
  scale_y_continuous(breaks = seq(0,.5,by = .05)) 

