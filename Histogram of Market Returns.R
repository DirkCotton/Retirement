# mu=.042
# sigma=.114
# marketReturns <- matrix(rlnorm(10000*50,mu,sigma), n, yrs) 
# mean(marketReturns)
# sd(marketReturns)

returns <- read.csv("Random Market Returns Log-N 10Kx50.csv",header = FALSE)
marketReturns1 <- marketReturns - 1
marketReturns <- data.frame(marketReturns)
annualMeans <- rowMeans(marketReturns1)
annualMeans2 <- rowMeans(marketReturns)
marketReturns1 <- data.frame(marketReturns1,annualMeans)
marketReturns <- data.frame(marketReturns,annualMeans2)


p <- ggplot(marketReturns) +
  aes(annualMeans2) + 
  geom_histogram(
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of Simulated Log-Normal Market Returns") +
  labs(x="Annual Market Return", y="Count") 
p