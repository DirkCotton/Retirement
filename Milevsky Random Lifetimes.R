lambda <- log(2)/18.9 # from Milevsky paper for 65-year old
n <- 10000
alive <- rep(50,0)
alive[1]  <- n
for (yrs in 1:50) {
  years <- 50-yrs+1
  mort[years] <- exp(-lambda*years)
  if (years==1) alive[years] <- (1-mort[years]) * n
  if (years>1) alive[years] <- (1-mort[years]) * alive[years-1]
  print(paste(years,mort[years],round(alive[years],0),sep=" "))
}
