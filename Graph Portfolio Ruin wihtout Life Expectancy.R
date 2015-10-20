# Graph portfoluo ruin by age ignoring life expectancy
# input comes from the "simulation" variable of the DPLYR model

cum.failed <- simulation$failure.year[simulation$failure.year <= 150] # portfolios that don't fail are store in age 164
cum.failed.hist <- hist(cum.failed,breaks=seq(66,114),plot=FALSE)

cum.failed.df <- data.frame(age=1:48,cumsum(cum.failed.hist$counts)) # age is years relative to retirement age

names(cum.failed.df) <- c("Age","Failed Portfolios")

failed.percent <- cum.failed.df$`Failed Portfolios`/length(simulation$sim.index) # change numer of failures to cum failures
cum.failed.df$`Failed Portfolios`<-failed.percent # change cum failures to percent in data frame

# Plot results 

ggplot(data = cum.failed.df, aes(x = cum.failed.df$Age+65, y = cum.failed.df$`Failed Portfolios`,color="darkred")) + geom_line() +
xlab("Age") + ylab("Percent of Failed Portfolios") + ylim(0,1) +
  theme_gray() +
  
  theme(legend.position='none') +
   ggtitle(paste("Percent of Failed Portfolios by Age\nMale with ",wr*100,"% Annual Withdrawals",sep="")) +
  coord_cartesian(ylim = c(0, 1)) + scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1))
