## Monte Carlo Simulation of Sustainable Withdrawal Rates with Log-Normal Distribution
## Initial Values

# USES SHILLER DATA
## *********************

library(ggplot2)
library(segmented)

set.seed(27514)
years <- 36  ## Number of years in each simulated scenario (include year zero)
mu <- .0663  ## Average portfolio rate of return
sigma <- .1817  ## Portfolio standard deviation
sossec <- 0  ## annual Social Security benefit
portfolio <- 1000000  ## Initial portfolio value
normalspend <- .045*portfolio

x <- 11  ## Soc Sec kicks in after x years.
## *********************
n <- 100  ## Number of samples to generate
s <- normalspend + sossec   ## Set first x years of spending to s, before Soc Sec kicks in. .
decline <- .0  ## annual decline in spending
withdrawal <- normalspend/portfolio  ## Annual spending percentage of initial portfolio value
spend <- withdrawal*portfolio  ## Annual spending amount
## Create log-normal distribution of returns
# to randomize use the next two statements
 
# data <- matrix(rlnorm(n*years,mu,sigma),n,years)
data <- read.csv("~/desktop/R/Shiller Returns 36 Rolling Columns.csv",header=TRUE,nrows=141)
data <- data.frame(data) + 1 # only for Shiller data

# to use a pre-generated files of random returns use the following two lines
# data <- read.csv("Random Market Returns Log-N 10Kx50.csv",header = FALSE)
# data <- data[,1:30]
for(w in 1:n){
}
means <- rep(1,n)
tpv <- rep(portfolio, n)
yrfailed <- rep(0,n)
ps <- rep(0,9) ## Count scenarios that succeed for "years" at a given  portfolio ROR
fl <- rep(0,9)   ## Count scenarios that fail for "years" at a given portfolio ROR
spnd <- rep(0, years)
for(i in 1:years) {
spnd[i] <- spend*((1 - decline)^(i))
}
# for(j in 1:x) {
# spnd[j] <- s
# }
## Calculate geometric mean rate of return for each scenario
for(row in 1:n) {
x <- 1
for(col in 1:years) {
x <- x*data[row, col]
}
means[row] <- x
means[row] <- (x^(years^(-1))) - 1  ## calculate geometric mean ROR
}
for (row in 1:n) {
x <- portfolio
first <- TRUE
for(col in 1:years) {
x <- (x - spnd[col])*data[row, col]
if(x <=0 & first) {
yrfailed[row] <- col
first <- FALSE
}
## print("Spend ",spnd[col],"remaining balance ",x]
}
tpv[row] <- x
i <- min(as.integer(means[row]*100), 6) + 3
if(x <= 0) { fl[i] <- fl[i] + 1} else {
ps[i] <- ps[i] + 1
}
}
failed <- sum(fl)
print(noquote("*****************************"))
print(noquote("Bengen-style P(Ruin) Model with Shiller Returns Data"))
print(noquote(paste("Scenarios ", format(round(as.numeric(n), 0), nsmall=0, big.mark=",") , "    Period length in years: ", years,sep="")))
print(noquote(paste("Annual spending = ", withdrawal*100, " %",sep="")))
print (noquote(paste("Failed scenarios: ", format(round(as.numeric(failed), 0), nsmall=0, big.mark=",") , "   Percent failed: ", failed/n*100, " %",sep="")))
print(noquote("*****************************"))
print(noquote("Success rates:"))

print(noquote(" "))
print(noquote("Annual Spending"))
if(min(spnd)==max(spnd)) {print(noquote(paste("All years= $",format(round(as.numeric(spnd[1]), 0), nsmall=0, big.mark=","),sep="")))
} else {
for(z in 1:(length(spnd))) {
print(paste("Year ", z, " ", spnd[z],sep=""))
}
}
print(noquote(" "))
print(noquote(paste("Mean TPV= $",format(round(as.numeric(mean(tpv)), 0), nsmall=0, big.mark=",") ,"  Median TPV= $",format(round(as.numeric(median(tpv)), 0), nsmall=0, big.mark=",") ,sep="")))

xx <- hist(yrfailed,breaks = 40,plot=FALSE)



b.df <- data.frame(xx$breaks[2:36],xx$counts[2:36])
c.df <- within(b.df, acc_sum <- cumsum(xx.counts.2.36.))
c.df$xx.counts.2.36. <- NULL
colnames(c.df) <- c("Year","CumFailed")

write.csv(c.df$acc_sum, file="/users/dirkcotton/desktop/counts3%.csv")

yy <- ggplot(data = c.df, aes(x = Year, y = CumFailed)) + geom_line(size=1,color="blue") +
xlab("Years in Retirement") + ylab(paste("Percent Ruined Portfolios")) +
theme_gray() + ggtitle(paste("Cumulative Ruined Portfolios with ",normalspend/portfolio*100,"% Annual Withdrawals\n","Shiller Market Return Data",sep="")) +
geom_segment(aes(x = 18, y = 0, xend = 35, yend = 14),color="red",size=.1) + theme(plot.title=element_text(family="Times", size=16),axis.title=element_text(family="Times", size=16)) +
annotate("text", x = 30, y = 5, label = "Best linear fit",color="red",size=5, family="Times",)
  
  


print (yy) 

# x <- c.df$Year
# y <- c.df$CumFailed
# 
# lin.mod <- lm(y ~ x)
# segmented.mod <- segmented(lin.mod, seg.Z = ~x,psi=20)
# 
# plot(x,y, pch=16, ylim=c(5,20))
# plot(segmented.mod)
