rage <- 65
household <- 1
liv <- rep(10000,0)
#################
soc <- read.csv("Annual Mortality.csv", header=TRUE,nrows=115)
socmd <- read.csv("SOC Male Deaths per Thousand.csv", header=TRUE,nrows=50)
for (n in 1:10000) {
  liv[n] <- le(household,rage)
}

hista <- hist(liv, breaks=seq(66,115,l=50))
points(socmd[,1],socmd[,5]*10)

# function le returns life expectancy when passed:
#    household -1 (male, 2- Female, 3- Joint)
#    rage = retirement age
#
le <- function (household,rage) {
  
  #
  # Function le: Generate one random life expectancy" 
  #
  
  y <- runif(1,socmd[1,household+1],1) # randomly select a life expectancy
  # print(paste("runif Y=",y,sep=" "))
  
  dies <- 0
  for (i in 2:50) {
    # print(paste("y=",y,"soc=",soc[i,household+1],sep=" "))
    if (y <= socmd[i,household+1]) {
      # print(paste("DIES age ",soc[i,1],"y=",y,"soc=",soc[i,household+1],sep=" "))
      dies <- socmd[i-1,1]
      break
    }
  }
  dies
}

