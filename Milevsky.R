#
# funcion MILEVSKY - use Milevsky formula for probability of portfolio ruin without simulation
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


mu <- .05 # expected portfolio return #

sigma <- .12 # expected portfolio standard deviation #

lifeexpect <- 18.9 # remaining life expectancy #


withdrawalrate <- .06
# withdrawal rate #

prob <- milevsky (mu, sigma, withdrawalrate,lifeexpect)
print(paste("Probability of Ruin is ", round(prob*100,2), "%", " for ", 
            withdrawalrate*100, "%", " withdrawals",sep=" "))

for(withdrawalrate in seq(.02,.1,.001)) {   
  prob <- milevsky (mu, sigma, withdrawalrate,lifeexpect)
  
  
  print(paste("Probability of Ruin is ", round(prob*100,2), "%", " for ", 
              withdrawalrate*100, "%", " withdrawals",sep=" "))
}

