#
# function SCENARIO returns a list with the following elements:
#
# ruined = 1 if the scenario ended with a ruined portfolio before the retiree died, 
#  0 if the scenario ended with death before ruin
# agefailed = Age at which retiree died or went broke

scenario <- function (wr,portfolio,household,rage,mu,sigma) {
  agefailed <- 0
  mktreturns <- rep(50,0)
  life <- le(household,rage) 
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
