
library(survival) 

# *************************************************************************
#
# Monte Carlo Simulation of Retirement Portfolio Survival
#
# 
# N = number of scenarios to run
# WR = annual withdrawal rate as percentage of initial portfolio value
# Portfolio = initial portfolio value
# Household = Male (1), Female (2), Male and Female Couple (3)
# rage = retirement age
# mu = expected market return
# sigma = standard deviation of expected returns 
# *************************************************************************
# 

# Set key parameters

household <- 1 # 1= male, 2= female, 3=couple 
lambda <- 0
n <- 10000
mu <- .05
sigma <- .11
rage <- 65
portfolio <- 1000000
wrstart <- 4  # first withdrawal rate percentage
wrend <- 5  # last withdrawal rate percentage
wrstep <- .5 # withdrawal rate steps

# 
# Initialize variables
# 
hh <- c("Male","Female","Couple")
hhline <- c("red4","dodgerblue4","lightgoldenrod4") # set colors for chart line and confidence interval band
hhband <- c("mistyrose","slategray1","moccasin")
failurerate <- rep(10,0)
wrates <- rep(10,0)
milevest <- rep(10,0)
mediantpv <- rep(10,0)
mlex <- rep(10,0)

#
# Read 10,000 random life times for age 65 retirees from male, female, ot joint data files
#
if (household == 1) soc <- read.csv("Male Random Lifetimes from Age 65.csv",header=FALSE)
if (household == 2) soc <- read.csv("Female Random Lifetimes from Age 65.csv",header=FALSE)
if (household == 3) soc <- read.csv("Joint Random Lifetimes from Age 65.csv",header=FALSE)

#
# Shuffle life expectancie
# sample(soc)
socmd <- read.csv("SOC Male Deaths per Thousand 2.csv", header=TRUE,nrows=50)

# Loop through simulations at various withdrawal rates

wrcount <- 1
for(k in seq(wrstart, wrend, wrstep)) {

  set.seed(27514) # use the same pseudo-random numbers at each withdrawal rate for market returns
  wr <- k/100
  events <- rep(n,0)
  censor <- rep(n,0)
  tpvs <- rep(n,0)
  lives <- rep(n,0)
  lifetimes <- rep(n,0)
  
 
  #
  # Run N scenarios
  #
  
  
  for (i in 1:length(soc[,1])) {
   if (soc[i,1]>66) {
    scenario1 <- scenario(wr,portfolio,household,rage,mu,sigma,soc[i,1])
    events[i] <- scenario1[2]
    censor[i] <- scenario1[4]
    tpvs[i] <- scenario1[3]
    lives[i] <- soc[i,1]
   }
    else {
      events[i] <- 66
      censor[i] <- 1
      tpvs[i] <- portfolio - (wr*portfolio)/2
      lives[i] <- 66
    }
  }
  
  # Calculate probabilty of ruin using Milevsky formla for comparison
  
  milevsky <- function (mu, sigma, withdrawalrate,lifeexpect) {
    lambda <- log(2)/lifeexpect
    a <- ((2*mu + 4* lambda)/(sigma^2 + lambda)) -1
    b <- (sigma^2 + lambda)/2
    # print(paste("a is ", a, " b is ", b,"wr%=",wr,sep=" "))
    
    prob <- pgamma(withdrawalrate,shape=a,scale=b)
    # spv <- 1/(mu - sigma^2 + lambda) 
  }
  
  #
  # function SCENARIO returns a list with the following elements:
  #
  # ruined = 1 if the scenario ended with a ruined portfolio before the retiree died, 
  #  0 if the scenario ended with death before ruin
  # agefailed = Age at which retiree died or went broke
  
  scenario <- function (wr,portfolio,household,rage,mu,sigma,yearofdeath) {

    agefailed <- 0
    mktreturns <- rep(50,0)
    mktreturns <- rlnorm(50,mu,sigma) # generate random log-normal market returns
   #  print(paste("Mkt returns[1]=",mktreturns[1],sep=" "))
    newbal <- portfolio
    tpv <- 0
    ruined <- 1
    censor <- 1
    lr <- yearofdeath - rage 
    agefailed <- yearofdeath
    
      for (j in 1:lr) {
        newbal <- (newbal - wr*portfolio) * mktreturns[j]
        if (newbal <= 0) {
          censor <- 0
          ruined <- 1
          newbal <- 0
          agefailed <- j + rage + 1
          break()
        }
      }
    
    tpv <- max(0,newbal) # tpv = Terminal Portfolio Value
    
    list(ruined, agefailed, tpv,censor)
 
  }
  #
  # function le returns life expectancy when passed:
  #    household -1 (male, 2- Female, 3- Joint)
  #    rage = retirement age
  #
 
  tpvs <- sapply(tpvs, as.numeric) # convert to numeric
  events <- sapply(events, as.numeric) # convert to numeric
  censor <- sapply(censor, as.numeric) # convert to numeric
  
  # Report failure rate
  
 # print (paste("Withdrawal rate is",wr*100,"%",sep=" "))


 # print (paste("Failure rate for",n,"scenarios is",sum(tpvs==0)/n*100,"%",sep=" "))
 
  failurerate[wrcount] <- sum(tpvs==0)/n*100
  wrates[wrcount] <- wr*100
  milevest[wrcount] <- 100*round(milevsky(mu,sigma,wr,mle),3)
  mediantpv[wrcount] <- round(median(tpvs),0)
  
  mle <- median(lives) - rage
  mlex[wrcount] <- median(lives)
  wrcount <- wrcount + 1
  
  # print(paste("Milevsky estimate for comparison: ",100*round(milevsky(mu,sigma,wr,mle),3),"%",sep=" "))
  
  # Generate Histogram of TPVs, print mean, median
  
  # hist(tpvs,plot=TRUE)
  
  # print (paste("Mean terminal portfolio value is", round(mean(tpvs),0)," median is",round(median(tpvs),0),sep=" "))
  # print(paste("Median life expectancy simulated",median(lives),"MLE for Milevsky",mle+rage,sep=" "))
  # print(paste("***********************",sep=" "))
 # print(paste("Household is ",hh[household],sep=""))
  # print(paste("Hazard Rate= ",round(log(2)/mle,4),sep=" "))
 
  
  hist(events,plot=TRUE,breaks=seq(65,115,l=51))
  
  # write.table(events,"/Users/dirkcotton/Desktop/events106.cvs",sep=",")
  # write.table(censor,"/Users/dirkcotton/Desktop/censor106.cvs",sep=",")
  #
  # Plot K-M Survival Curves
  #
  #
  censor = (!censor) # survfit uses different cendor bit than Mathematica
  plottitle <- paste("Kaplan-Meier Probability of Portfolio Survival Curve\n",hh[household],", ",mu*100,"% Return with ",sigma*100,"% SD and ",wr*100,"% Withdrawals",sep="")
  objNpsurv <- npsurv(formula = Surv(events-rage,censor) ~ 1)
  survplot(objNpsurv,xlab="Year of Retirement",ylab="Probability of Portfolio Survival",conf.int=.7,col.fill=hhband[household],col=hhline[household],grid=TRUE,main="XXX")
  title(main=plottitle)
  
}  # End of main program loop
#
# Print results table and simulated life expectancy histogram.
#
cat (paste("Household is ",hh[household],"\n\n",sep=""))
cat (paste("Scenarios= ",n,"\n\n",sep=""))
cat ("Table 1. Withdrawal Rates, failure rate %, Milevsky Estimate of Failure Rate %, Median Terminal Portfolio Value\n and Median Simulated Life Expectancy\n\n")
resultsframe <- data.frame(wrates,failurerate,milevest,mediantpv,mlex)
print(resultsframe)

# myhist <- hist(lives,breaks=51)
hista <- hist(lives, breaks=seq(65,115,l=51),main=c("Simulated Life Expectancies\n",hh[household],"Household"))
points(socmd[,1],socmd[,4+household]*10)

