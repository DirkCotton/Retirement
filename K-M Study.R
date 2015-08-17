le <- function (household,rage) {
  
  #
  # Function le: Generate one random life expectancy" 
  #
  
  y <- runif(1,socmd[2,household+1],1) # randomly select a life expectancy
  # print(paste("runif Y=",y,sep=" "))
  
  dies <- 0
  for (i in 2:50) {
    # print(paste("y=",y,"soc=",soc[i,household+1],sep=" "))
    if (y <= soc[i,household+1]) {
      # print(paste("DIES age ",soc[i,1],"y=",y,"soc=",soc[i,household+1],sep=" "))
      dies <- soc[i,1]
      break
    }
  }
  dies
}
