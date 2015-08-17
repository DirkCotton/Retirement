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



household <- 1 # 1= male, 2= female, 3=couple
lambda <- 0
n <- 10000
mu <- .05
sigma <- .12
rage <- 65
portfolio <- 1000000
wrstart <- 3  # first withdrawal rate percentage
wrend <- 6  # last withdrawal rate percentage
wrstep <- .5 # withdrawal rate steps

# 
# Initialize variables
# 
hh <- c("Male","Female","Couple")
hhline <- c("red4","dodgerblue4","lightgoldenrod4") # set colors for chart line and confidence interval band
hhband <- c("mistyrose","slategray1","moccasin")

randomlives <- function (n,household,rage) {
  #
  # Generate n random lifetimes for a given household
  #
  #
  # function le returns life expectancy when passed:
  #    household -1 (male, 2- Female, 3- Joint)
  #    rage = retirement age
  #
  
  soc <- read.csv("Annual Mortality.csv", header=TRUE,nrows=115) 
  socmd <- read.csv("SOC Male Deaths per Thousand.csv", header=TRUE,nrows=50)
  samplelives <- rep(n,0)
  
  for (j in 1:n) {
    #
    # Function le: Generate one random life expectancy" 
    #
    
    #
    # function le returns life expectancy when passed:
    #    household -1 (male, 2- Female, 3- Joint)
    #    rage = retirement age
    #
    
    #
    # Function le: Generate one random life expectancy" 
    #
    
    y <- runif(1,soc[1,household+1],1) # randomly select a life expectancy
    # print(paste("runif Y=",y,sep=" "))
    
    dies <- 0
    for (i in 1:50) {
      # print(paste("y=",y,"soc=",soc[i,household+1],sep=" "))
      if (y <= soc[i,household+1]) {
        # print(paste("DIES age ",soc[i,1],"y=",y,"soc=",soc[i,household+1],sep=" "))
        dies <- soc[i,1]
        break
      }
    }
    samplelives [j] <- dies
  }
  samplelives
}

#################
# 
#  Build vector of n life expectancies
#
#################
lifetimes <- randomlives(n, household,rage)

# Loop through simulations at various withdrawal rates

for(k in seq(wrstart, wrend, wrstep)) {
  wr <- k/100
  events <- rep(n,0)
  censor <- rep(n,0)
  tpvs <- rep(n,0)
  lives <- rep(n,0)
  lifetimes <- rep(n,0)
 
  #
  # Run N scenarios
  #
  
  
  for (i in 1:n) {
    lt <- lifetimes[n]
    scenario1 <- scenario(wr,portfolio,household,rage,mu,sigma,lt)
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
  
  scenario <- function (wr,portfolio,household,rage,mu,sigma,life) {
    agefailed <- 0
    mktreturns <- rep(50,0)
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
  le <- function (household,rage) {
    
    #
    # Function le: Generate one random life expectancy" 
    #
    
    y <- runif(1,soc[1,household+1],1) # randomly select a life expectancy
    # print(paste("runif Y=",y,sep=" "))
    
    dies <- 0
    for (i in 1:50) {
      # print(paste("y=",y,"soc=",soc[i,household+1],sep=" "))
      if (y <= soc[i,household+1]) {
        # print(paste("DIES age ",soc[i,1],"y=",y,"soc=",soc[i,household+1],sep=" "))
        dies <- soc[i,1]
        break
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
  print(paste("Household is ",hh[household],sep=""))
  print(paste("Hazard Rate= ",round(log(2)/lifeexpect,4),sep=" "))
  print(paste("***********************",sep=" "))
  
  hist(events,plot=TRUE,breaks=50)
  
  # write.table(events,"/Users/dirkcotton/Desktop/events106.cvs",sep=",")
  # write.table(censor,"/Users/dirkcotton/Desktop/censor106.cvs",sep=",")
  #
  # Plot K-M Survival Curves
  #
  #
  censor = (!censor) # survfit uses different cendor bit than Mathematica
  plottitle <- paste("Kaplan-Meier Probability of Portfolio Ruin Curve\n",hh[household],", ",mu*100,"% Return with ",sigma*100,"% SD and ",wr*100,"% Withdrawals",sep="")
  objNpsurv <- npsurv(formula = Surv(events-rage,censor) ~ 1)
  survplot(objNpsurv,xlab="Year of Retirement",ylab="Probability of Portfolio Survival",conf.int=.7,col.fill=hhband[household],col=hhline[household],grid=TRUE,main="XXX")
  title(main=plottitle)
}

myhist <- hist(lives)
hista <- hist(lives, breaks=seq(66,115,l=50))
points(socmd[,1],socmd[,5]*10)
