cat("\014")

#**********************************************

#
# function MILEVSKY - use Milevsky formula for probability of portfolio ruin without simulation
#
# parameters: Milevsky (mu, sigma, withdrawalrate,lifeexpect)
# 
# mu = expected REAL portfolio return
# sigma = portfolio return annual standard deviation
# withdrawal rate = percent of initial portfolio value to spend annually
# lifeexpect = life expectancy for retiree
#
# returns probability of portfolio ruin before death

milevsky <- function (mu, sigma, withdrawalrate,lifeexpect) {
  lambda=  log(2)/lifeexpect
  #   print(paste("Lambda= ",lambda," life expectancy= ",lifeexpect))
  a <- ((2*mu + 4* lambda)/(sigma^2 + lambda)) -1
  b <- (sigma^2 + lambda)/2
  # print(paste("a is ", a, " b is ", b,"wr%=",wr,sep=" "))
  
  prob <- pgamma(withdrawalrate,shape=a,scale=b)
  # spv <- 1/(mu - sigma^2 + lambda) #
}
#**********************************************

randomz <- FALSE # If TRUE, generate a new set of random market returns. If FALSE, input canned random returns from file
graphData <-  matrix(nrow = 45,ncol = 3)
jloop <- 0
jloop2 <- 1

for (wr in seq(.04,.04,by=.005)) {
  for (household in 1:5) {
    
    jloop <- jloop + 1
    
    
#********************************
    
    #This model's Bengen rates are not the same as the independent Bengen model. Until I find the difference
    # use these Bengen failure rates
    
    
    
    # Set initial values
    
    # set.seed(27514)
    # household <- 3 # 1= male, 2= female, 3=couple, 4= Bengen model estimate, 5 = Milevsky Estimate for Male
    hh <- c("Male","Female","Couple","Bengen","Milevsky Male")
    rage <- 65 # retirement age
    n <- 10000
    mu <- .05
    sigma <- .11
    rage <- 65
    portfolio <- 1000000 # $1,000000
    yrs <- 50 # years of market returns
    # wr = .05
    # percentage of initial portfolio value to spend annually
    spend <- wr * portfolio # dollar amount to spend annually
    balances <- matrix(nrow=n,ncol=yrs)
    ageRuined <- rep(n,0)
    events <- rep(n,0)
    censor <- rep ()
    
    # functions
    
    newBalance <- function (lastYearBal,currentReturn,spend) { max(0,(lastYearBal - spend) * currentReturn) }
    
    # Create table of n rows and yrs = 50 columns of random market returns
    
    if (randomz) {
      marketReturns <- matrix(rlnorm(n*yrs,mu,sigma), n, yrs)
    } else {
      marketReturns <- read.csv("Random Market Returns Log-N 10Kx50.csv",header = FALSE)
    }
    
    # Create a vector "ageDied" of n random life expectancies from SOC Mortality Tables
    
    if (household == 1) soc <- read.csv("Male Random Lifetimes from Age 65.csv",header=FALSE,nrows=n)
    if (household == 2) soc <- read.csv("Female Random Lifetimes from Age 65.csv",header=FALSE,nrows=n)
    if (household == 3) soc <- read.csv("Joint Random Lifetimes from Age 65.csv",header=FALSE,nrows=n)
    ageDied <- soc[,1]
    
    # ageDied <- 95 # add this line to get fixed-lifetime Bengen model results. Check the correct column of balances
    # for terminal portfolio values. (Column 30 for age 95, for example)
    
    # Create 50 columns of n rows of portfolio balances based on the market returns matrix and annual spending of wr % of initial portfolio balance
    
    for (i in 1:n) {
      for (j in 1:yrs) {
        if (j == 1) {oldBalance <- portfolio} 
        else {oldBalance <- balances[i,j-1]}
        
        balances[i,j] <- newBalance(oldBalance,marketReturns[i,j],spend)
      }
    }
    
    # Create an "ageRuined" vector showing the age at which the portfolio balance no longer exceeded zero (was depleted)
    
    for (k in 1:n) {
      if(balances[k,yrs] > 0) {
        ageRuined[k] <- yrs + rage 
        censor[k] <- 1
      } else {
        ageRuined[k] <- min(which(balances[k,] <= 0)) + rage
        censor[k] <- 0
      }
      if (ageDied[k] > ageRuined[k]) events[k] <- ageRuined[k] else events[k] <- ageDied[k]
    }
    
    # Create a boolean vector "ruinBeforeDeath" that is TRUE if the portfolio was depleted before death
    
    ruinBeforeDeath <- (ageRuined < ageDied)
    cat("\n\nDeath before ruin",n-sum(ruinBeforeDeath),sep=" ")
    
    cat("\n\nRuin before death",sum(ruinBeforeDeath),sep=" ")
    
    # Display results: vectors of events and censoring
    
    cat("Withdrawal rate= ",wr," Probability of Ruin= ",sum(ruinBeforeDeath)/n)
    
    # build event and censor vectors for analyses. If died first, event <- age died and censor <- 0 (censored). If ruined
    # first, event <- age when ruined and censor <- 1 (not censored). If died and ruined in the same year, that is a
    # successful funding of retirement, so censor
    
    status <- (ageDied > ageRuined) + 1
    censor <- ageRuined >= ageDied
    cmpdf <- data.frame(events,status,censor)
    write.csv(cmpdf,"Censor Data 5 percent.csv",row.names = FALSE)
    
    
    
    # Find Bengen 30-year fixed survival rate
    
    bengen30 <- balances[,30]
    # print(paste("\nBengen 30-year failure= ",sum(bengen30/n <= 0)/n))
    
    graphData[jloop,1] <- wr
    graphData[jloop,2] <- hh[household]
    if (household <= 3) graphData[jloop,3] <- max(.0001,as.double(sum(ruinBeforeDeath)/n)) 
    if (household == 4) {
      graphData[jloop,3] <- bengenModel[jloop2]   # as.double(sum(bengen30 <= 0)/n)
      jloop2 <- jloop2 + 1
    }
    if (household == 5) graphData[jloop,3] <- as.double(milevsky(mu,sigma,wr,18))
    
  }
}

graphData.df <- data.frame(graphData,stringsAsFactors = FALSE)
b <- as.numeric(graphData[,3]) #convert PRuin char to numeric
graphData.df$PRuin <- b

colnames(graphData.df) <- c("WithdrawalRate","Gender","PRuin")

# Plot withdrawal rate .02 to .06
gplt <- ggplot(data=graphData.df, aes(x=WithdrawalRate, y=as.numeric(PRuin), group=Gender,color=Gender)) + geom_line() + ggtitle("Probability of Ruin by Withdrawal Rate") + scale_colour_manual(values=c("red", "blue","green","black","purple"))
gplt
# re-Plot withdrawal rates .02 to .04
gmax4pc <- ggplot(data=graphData.df[1:25,], aes(x=WithdrawalRate, y=as.numeric(PRuin), group=Gender,color=Gender)) + geom_line() + ggtitle("Probability of Ruin by Withdrawal Rate") + scale_colour_manual(values=c("red", "blue","green","black","yellow"))
gmax4pc