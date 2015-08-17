set.seed(27514)
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

lambda <- 0
n <- 1000
mu <- .04
sigma <- .11
rage <- 65
portfolio <- 1000000
wr <- .04
soc <- read.csv("Annual Mortality.csv", header=TRUE,nrows=115)

for(k in seq(3,5,.5)) {
  w <- k/100
  z <- multipleScenarios (n, w, portfolio, rage, mu, sigma, soc)
}



#################
# 
#  Import SOC Life Expectancies
#

multipleScenarios <- function (n, wr, portfolio, rage, mu, sigma, soc) {
  #
  # Run N scenarios
  #
  failures <- 0
  household <- 2
 
  events <- rep(n,0)
  censor <- rep(n,0)
  tpvs <- rep(n,0)
  lives <- rep(n,0)
  
  for (i in 1:n) {
    
    scenario1<-scenario(wr,portfolio,household,rage,mu,sigma)
    
    events[i] <- scenario1[2]
    censor[i] <- scenario1[4]
    tpvs[i] <- scenario1[3]
    lives[i] <- as.numeric(scenario1[5])
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
  
  scenario <- function (wr,portfolio,household,rage,mu,sigma) {
    agefailed <- 0
    mktreturns <- rep(50,0)
    life <- le(household,rage,soc) 
    mktreturns <- rlnorm(50,mu,sigma)
    newbal <- portfolio
    tpv <- 0
    ruined <- 1
    censor <- 1
    lr <- 30 #life - rage
    agefailed <- life
    # tpv = Terminal Portfolio Value
    
    for (j in 1:lr) {
      
      newbal <- (newbal - wr*portfolio) * mktreturns[j]
      if (newbal <= 0){
        censor <- 0
        ruined <- 1
        newbal <- 0
        agefailed <- j + rage
        break()
      }
    }
    tpv <- max(0,newbal)
    list(ruined, agefailed, tpv,censor,life)
  }
  #
  # function le returns life expectancy when passed:
  #    household -1 (male, 2- Female, 3- Joint)
  #    rage = retirement age
  #
  le <- function (household,rage,soc) {
    
    quit("yes")
    print(paste("in le",sep=""))
    randomlifeexp(rage,soc,household)
  }
  #
  # Function Randomlifeexp: Generate one random life expectancy" 
  # household= male (1), female (2), couple (3)
  #
  randomlifeexp <- function (rage,soc,household) {
    
    print(paste("In randomlife",sep=""))
    for (i in (rage+1):115) {
      if (household==1) {
        mort <- soc$Male.Annual.Mortality[i]} else {
          if (household==2) {mort <- soc$Female.Annual.Mortality[i]} else {
            mort <- soc$Couple.Annual.Mortality[i]}
        }
     print(paste("mort= ",mort,sep=""))
      y <- runif(1,0,1)
      if (y < mort) {
        dies <- i
        break()
      }
    }
    dies
  }
  
  tpvs <- sapply(tpvs, as.numeric) # convert to numeric
  events <- sapply(events, as.numeric) # convert to numeric
  censor <- sapply(censor, as.numeric) # convert to numeric
  
  # Report failure rate
  
  print (paste("Withdrawal rate is",wr*100,"%",sep=" "))
  
  print (paste("Failure rate for",n,"scenarios is",sum(tpvs==0)/n*100,"%",sep=" "))
  
  # Calculate probabilty of ruin using Milevsky formla for comparison
  
  mle <- median(lives) - rage
  
  print(paste("Milevsky estimate for comparison: ",100*round(milevsky(mu,sigma,wr,mle),3),"%",sep=" "))
  
  # Generate Histogram of TPVs, print mean, median
  
  # hist(tpvs,plot=TRUE)
  
  print (paste("Mean terminal portfolio value is", round(mean(tpvs),0)," median is",round(median(tpvs),0),sep=" "))
  
  print(paste("Median life expectancy simulated",median(lives),"MLE for Milevsky",mle+rage,sep=" "))

  print(paste("Hazard Rate= ",round(log(2)/lifeexpect,4),sep=" "))
  
  print(paste("***********************",sep=" "))
  
  hist(events,plot=TRUE)
  
  # write.table(events,"/Users/dirkcotton/Desktop/events106.cvs",sep=",")
  # write.table(censor,"/Users/dirkcotton/Desktop/censor106.cvs",sep=",")
  #
  # Plot K-M Survival Curves
  #
  #
  censor = (!censor) # survfit uses different cendor bit than Mathematica
  plottitle <- paste("Kaplan-Meier Probability of Portfolio Ruin Curve\n",mu*100,"% Real Return (",sigma*100,"% SD) and ",wr*100,"% Withdrawals",sep="")
  objNpsurv <- npsurv(formula = Surv(events-rage,censor) ~ 1)
  survplot(objNpsurv,xlab="Year of Retirement",ylab="Probability of Portfolio Ruin",conf.int=.7,col.fill="mistyrose",col="red4",grid=TRUE,main="XXX")
  title(main=plottitle)
}
