soc = read.csv("Annual Mortality.csv", header=TRUE,nrows=115)
soc
rage <- 65
data <- rep(1000,0)
for (j in 1:1000) {
  data[j] <- randomlifeexp(rage,soc)
}
print(paste("Median L.E. is",median(data),sep=" "))
randomlifeexp <- function (rage, soc) {
  data
  
  for (i in (rage+1):115) {
    y <- runif(1,0,1)
    if (y < soc$Male.Annual.Mortality[i]) {dies <- i
    break()
    }
  }
  dies
}
