## Monte Carlo Simulation of Sustainable Withdrawal Rates with Log-Normal Distribution  


## Initial Values  

## *********************  

set.seed(27514)
years <- 30  ## Number of years in each simulated scenario  

mu <- .05  ## Average portfolio rate of return  

sigma <- .11  ## Portfolio standard deviation  

sossec <- 0  ## annual Social Security benefit  

portfolio <- 1000000  ## Initial portfolio value  

normalspend <- .04*portfolio

for (normalspend in seq(from=25000, to=55000, by=5000)) {
x <- 11  ## Soc Sec kicks in after x years.  
## *********************  


n <- 10000  ## Number of samples to generate   

s <- normalspend + sossec   ## Set first x years of spending to s, before Soc Sec kicks in. .  
decline <- .0  ## annual decline in spending  

withdrawal <- normalspend/portfolio  ## Annual spending percentage of initial portfolio value  
spend <- withdrawal*portfolio  ## Annual spending amount   

## Create log-normal distribution of returns  
# to randomize use the next two statements
data <- matrix(rlnorm(n*years,mu,sigma),n,years)
data <- data.frame(data)

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

for(j in 1:x) {
  
  spnd[j] <- s  
}

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
print(noquote("Bengen-style P(Ruin) Model with Log-Normal Market Returns"))
print(noquote(paste("Scenarios ", format(round(as.numeric(n), 0), nsmall=0, big.mark=",") , "    Period length in years: ", years,sep="")))
print(noquote(paste("Annual spending = ", withdrawal*100, " %",sep="")))
print (noquote(paste("Failed scenarios: ", format(round(as.numeric(failed), 0), nsmall=0, big.mark=",") , "   Percent failed: ", failed/n*100, " %",sep="")))

print(noquote("*****************************"))

print(noquote("Success rates:"))

# print(paste("ROR-2% or less ", round(ps[1]/(ps[1] + fl[1]),3),sep=""))
# print(paste("ROR 1% ", round(ps[2]/(ps[2] + fl[2]),3),sep=""))
# print(paste("ROR 0% ", round(ps[3]/(ps[3] + fl[3]),3),sep=""))
# print(paste("ROR 1% ", round(ps[4]/(ps[4] + fl[4]),3),sep=""))
# print(paste("ROR 2% ", round(ps[5]/(ps[5] + fl[5]),3),sep=""))
# print(paste("ROR 3% ", round(ps[6]/(ps[6] + fl[6]),3),sep=""))
# print(paste("ROR 4% ", round(ps[7]/(ps[7] + fl[7]),3),sep=""))
# print(paste("ROR 5% ", round(ps[8]/(ps[8] + fl[8]),3),sep=""))
# print(paste("ROR ><-6% ", round(ps[9]/(ps[9] + fl[9]),3),sep=""))

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


#hist(tpv)
#
}

