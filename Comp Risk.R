# Gets data from "Censor Data.csv" created by model "Dplyr"


###################################################
### chunk number 1: Rprompt
###################################################
#line 68 "CompRiskSupp"
options(prompt="R> ")


###################################################
### chunk number 2: Load.cmpdf.File
###################################################
#line 103 "cmpdf - competing risks cmpdf frame"
cmpdf <- read.csv("Censor Data 4.5 percent.csv", header=TRUE)


###################################################
### chunk number 3: cmpdfInfo
###################################################
#line 114 "CompRiskSupp"
names(cmpdf)


###################################################
### chunk number 4: EventType
###################################################
#line 134 "CompRiskSupp"
table(cmpdf[,2])


###################################################
### chunk number 5: Load.cmprsk.package
###################################################
#line 152 "CompRiskSupp"
library(cmprsk)


###################################################
### chunk number 6: CumulIncidsOverall
###################################################
#line 166 "CompRiskSupp"
CI.overall <- cuminc(ftime=cmpdf[,1], fstatus=cmpdf[,2])
CI.overall


###################################################
### chunk number 7: CumulIncidPlot
###################################################
#line 179 "CompRiskSupp"
plot(CI.overall, curvlab=c("Death","Portfolio Ruin"), xlab="Age",xlim=c(65,max(cmpdf[,1])),title(main="Competing Risks: Portfolio Ruin and Death\nMale, 4% Annual Withdrawal Rate"))
par(new=FALSE)

###################################################
### chunk number 8: CumulIncid.RC.IVvsV
###################################################
#line 200 "CompRiskSupp"
CI.4vs5 <- cuminc(ftime=cmpdf[,1], fstatus=cmpdf[,2], group=cmpdf[,3])


###################################################
### chunk number 9: CumulIncidPlot4vs5
###################################################
#line 207 "CompRiskSupp"
# plot(CI.4vs5, lty=c(1,1,2,2), col=c("black", "blue", "black", "blue"), curvlab=c("Discharge, RC IV", "Discharge, RC V","Death, RC IV", "Death, RC V"), xlab="Years")



###################################################
### chunk number 10: CumulIncidTests
###################################################
#line 231 "CompRiskSupp"
## Test statistic for RC IV vs V
CI.4vs5$Tests

plot(CI4.overall, col="red",curvlab=c("Death","Portfolio Ruin"), xlab="Age",xlim=c(65,max(cmpdf[,1])),title(main="Competing Risks: Portfolio Ruin and Death\nMale, 4%, 4.5% and 5% Annual Withdrawal Rates"))
par(new=TRUE)
plot(CI45.overall,col="blue", curvlab=c("Death","Portfolio Ruin"), xlab="Age",xlim=c(65,max(cmpdf[,1])))
par(new=TRUE)
plot(CI5.overall,col="darkgreen", curvlab=c("Death","Portfolio Ruin"), xlab="Age",xlim=c(65,max(cmpdf[,1])))
par(new=FALSE)