randomlives <- function (i, j) {
  #
  # Generate n random lifetimes for a given household
  #
  #
  # function le returns life expectancy when passed:
  #    household -1 (male, 2- Female, 3- Joint)
  #    rage = retirement age
  #
  
  n <- 10000
  household <- 1
  rage <- 65
  
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
}
    