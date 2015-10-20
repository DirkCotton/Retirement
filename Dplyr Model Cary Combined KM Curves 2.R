cat("\014")
#Dplyr approach - Cary

#Functions
summaryFailureYear <- function (x) {
  balance <- portfolio
  
  runningbalance <- vector(mode = "numeric", length = 0)
  
  for (k in 1:50) {
    balance <- (balance - spending) * x[k]
    runningbalance[k] <- balance
    if (balance < 0) break
  }
  if(k == 50) k <- 99
  return(k)
}

##Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(survival)
library(cmprsk)

##Files and formatting
marketReturns <- read.csv("~/CourseraR/Random Market Returns Log-N 10Kx50.csv",header = FALSE)
lifeSpanMale <- read.csv("~/CourseraR/Male Random Lifetimes from Age 65.csv",header=FALSE)
lifeSpanFemale <- read.csv("~/CourseraR/Female Random Lifetimes from Age 65.csv",header=FALSE)
lifeSpanJoint <- read.csv("~/CourseraR/Joint Random Lifetimes from Age 65.csv",header=FALSE)


#marketReturns
marketReturns.tdf <- tbl_df(marketReturns) %>%
  mutate(series = row_number())
cleanedMarketReturns <- marketReturns.tdf %>%
  gather(year, return.annual, -series, convert = T) %>%
  mutate(year = as.numeric(sub("V", "", year))) %>%
  arrange(series, year) %>%
  group_by(series)
cleanedMarketReturns

#lifeSpanMale
lifeSpanMale.tdf <- tbl_df(lifeSpanMale)

cleanedlifeSpanMale <- lifeSpanMale.tdf %>%
  rename(death.years = V1)
cleanedlifeSpanMale

#lifeSpanFemale
lifeSpanFemale.tdf <- tbl_df(lifeSpanFemale)

cleanedlifeSpanFemale <- lifeSpanFemale.tdf %>%
  rename(death.years = V1)
cleanedlifeSpanFemale

#lifeSpanJoint
lifeSpanJoint.tdf <- tbl_df(lifeSpanJoint)

cleanedlifeSpanJoint <- lifeSpanJoint.tdf %>%
  rename(death.years = V1)
cleanedlifeSpanJoint

##Plotting distributions
weight.inverse <- 1 / (max(cleanedMarketReturns$series) * max(cleanedMarketReturns$year))
g1 <- ggplot(data = cleanedMarketReturns, aes(x = return.annual)) +
  geom_bar(aes(weight = weight.inverse),fill=NA, color="black") +
  scale_y_continuous(labels = percent) +
  xlab("Growth rate of annual portfolio returns") + ylab("Proportion of annual returns") +
  ggtitle("Distribution of Simulated Annual Portfolio Returns") +
  theme_gray()
# g1
ggsave (paste("Port Returns ",as.character(wr),".png",sep=""))

weight.inverse <- 1 / 10000
g2 <- ggplot(data = cleanedlifeSpanMale, aes(x = death.years)) + ggtitle("Simulated Life Expectancy - Male") +
  stat_bin(binwidth = 1, origin = 64.5, aes(weight = weight.inverse),fill=NA, color="black") +
  scale_y_continuous(labels = percent) +
  xlab("Age at death") + ylab("Proportion of population samples") +
  theme_gray()
# g2
ggsave (paste("Male Life Exp ",as.character(wr),".png",sep=""))

g3 <- ggplot(data = cleanedlifeSpanFemale, aes(x = death.years)) +
  stat_bin(binwidth = 1, origin = 64.5, aes(weight = weight.inverse),fill=NA, color="black") +
  scale_y_continuous(labels = percent) +
  xlab("Age at death") + ylab("Proportion of population samples") + ggtitle("Simulated Life Expectancy - Female") +
  theme_gray()
#g3
ggsave (paste("Female Life Exp ",as.character(wr),".png",sep=""))

g4 <- ggplot(data = cleanedlifeSpanJoint, aes(x = death.years)) +
  stat_bin(binwidth = 1, origin = 64.5, aes(weight = weight.inverse),fill=NA, color="black") +
  scale_y_continuous(labels = percent) +
  xlab("Age at death") + ylab("Proportion of population samples") + ggtitle("Simulated Life Expectancy - Couple, At least One survivor") +
  theme_gray()
#g4
ggsave (paste("Couple Life Exp ",as.character(wr),".png",sep=""))


##Parameters
portfolio <- 1000000
wr <-0.05 # percentage of initial portfolio value to spend annually
spending <- wr * portfolio

##Failure years (conditional on above parameters)
solvedMarketScenarios <- cleanedMarketReturns %>%
  summarise(failure.year = summaryFailureYear(return.annual) + 65)

weight.inverse <- 1 / (max(solvedMarketScenarios$series))
g5 <- ggplot(data = solvedMarketScenarios, aes(x = failure.year)) +
  geom_bar(aes(weight = weight.inverse)) +
  scale_y_continuous(labels = percent) + stat_bin(binwidth=1,fill=NA, color="black") +
  xlab("Year of Portfolio Failure") + ylab("Proportion of cases (%)") + xlim(75, 115) +
  theme_gray() +
  ggtitle(paste("Portfolio Failures by Year with ",wr*100,"% Annual Spending",sep=""))
#g5
ggsave (paste("Failures by year ",as.character(wr),".png",sep=""))


######Run an indexed set of n iterations of the simulation########
n <- 5000
#######Query the CI estimations from year.start to year.end#######
year.start <- 66
year.end <- 100
CISpan <- seq(year.start, year.end)

simulation <- tbl_df(data.frame(sim.index = as.numeric(rep(1:n, each=1000)), cohort.member = rep(as.numeric(seq(1, 1000)), n))) %>%
  bind_cols(sample_n(cleanedlifeSpanJoint, n * 1000, replace = T)) %>%
  mutate(series = sample(1:10000, n * 1000, replace=T)) %>%
  left_join(solvedMarketScenarios, by = "series") %>%
  rowwise() %>%
  mutate(event.year = pmin(death.years, failure.year),
         event.type = ifelse(failure.year < death.years, 1, 0)) %>%
  group_by(sim.index)

times <- simulation %>%
  do(time = as.vector(survfit(Surv(as.numeric(.$event.year), .$event.type) ~ 1)$time)) %>%
  unnest(time)

survivals <- simulation %>%
  group_by(sim.index) %>%
  do(surv = as.vector(survfit(Surv(as.numeric(.$event.year), .$event.type) ~ 1)$surv)) %>%
  unnest(surv) %>%
  rename(index2 = sim.index)

combinedSurvResults <- times %>%
  bind_cols(survivals) %>%
  filter(index2 == sim.index)

simwisemeans.surv <- combinedSurvResults %>%
  group_by(time) %>%
  summarise(smean = mean(surv, na.rm = T),
            svar = var(surv, na.rm = T)) %>%
  ungroup()

cumulative.inc <- simulation %>%
  do(cifailure = timepoints(cuminc(as.numeric(.$event.year), .$event.type, cencode = 2), CISpan)) %>%
  mutate(cifunpack = cifailure[1]) %>%
  select(-cifailure) %>%
  do(sim.index = .$sim.index,
     failure = .$cifunpack[1, ], 
     death = .$cifunpack[2, ]) %>%
  mutate(sim.index = as.integer(sim.index))

failure <- cumulative.inc %>%
  select(sim.index, failure) %>%
  unnest(failure)

death <- cumulative.inc %>%
  select(sim.index, death) %>%
  unnest(death) %>%
  rename(index2 = sim.index)

combinedCIResults <- failure %>%
  bind_cols(death) %>%
  filter(index2 == sim.index)

simwisemeans.ci <- combinedCIResults %>%
  mutate(time = rep(CISpan, n)) %>%
  group_by(time) %>%
  summarise(cifmean = mean(failure),
            cidmean = mean(death),
            cifvar = var(failure),
            cidvar = var(death))

##Graph the mean curves with simulation standard error 95% CL
qm <- ggplot(data = simwisemeans.surv, aes(x = time, y = smean)) +
  geom_ribbon(aes(ymin = (smean - 1.96 * sqrt(svar)), ymax = (smean + 1.96 * sqrt(svar))), alpha = 0.5,fill="pink") +
  geom_path(color="darkred") +
  xlab("Age") + ylab("Proportion 1000-Member Cohort Without Portfolio Failure") + xlim(65, 100) + ylim(0, 1) +
  theme_gray() +
  scale_y_continuous(breaks = seq(0,1,by = .05),limits=c(.5,1))  +
  ggtitle(paste("Kaplan-Meier Portfolio Survival Curve with ",wr*100,"% Annual Spending",sep=""))
#qm

ggsave (paste("KM ",as.character(wr),".png",sep=""))


crsim <- cbind(gather(select(simwisemeans.ci, time, cidmean, cifmean), curve, mean, -time),
               select(gather(select(simwisemeans.ci, time, cidvar, cifvar), curve, var, -time), var))

crsim$curve <- factor(crsim$curve, labels = c("Portfolio failure", "Death"))

q <- ggplot(data = crsim, aes(x = time, y = mean)) +
  geom_path(aes(linetype = curve),color="darkred") +
  geom_ribbon(aes(ymin = (mean - 1.96 * sqrt(var)),
                  ymax = (mean + 1.96 * sqrt(var)),
                  linetype = curve),
              alpha = 0.25,fill="pink") +
  xlab("Age") + ylab("Proportion 1000-Member Cohort Specified Outcome") + xlim(65, 100) +
  scale_linetype_discrete(name = "") +
  theme_gray() +
  ggtitle(paste("Probabilities of Death and Ruin with ",wr*100,"% Annual Spending",sep=""))+
  scale_y_continuous(breaks = seq(0,1,by = .1))
#qqm

# ggsave (paste("Competing risk ",as.character(wr),".png",sep=""))
# mean.at.100 <- simwisemeans.ci %>%
#   filter(time == 100) %>%
#   select(cidmean)

# Combined Curves

ggplot(data = c.df, aes(x = c.df$Age, y = value,color=variable)) + geom_line() +
  xlab("Age") + ylab("Conditional Probability of Portfolio Survival") + xlim(65, 100) + ylim(0, 1) + scale_colour_discrete(name = "Annual\nWithdrawal\nRate",labels = c("3%", "4%","5%")) +
  theme_gray() +
  scale_y_continuous(breaks = seq(0,1,by = .1),limits=c(.5,1))  + 
  ggtitle(paste("Kaplan-Meier Portfolio Survival Curves\nCouple, Age 65, mu=.05, sigma=.11",sep=""))+
  ylim(0,1)

