# Graph to compare ruin probability with lifetime ruin probability
# input comer from simulation variable of DPLYR modelfailed.percent <- cum.failed.df$`Failed Portfolios`/length(simulation$sim.index) # change numer of failures to cum failures

cum.failed <- simulation$failure.year[simulation$failure.year <= 150] # portfolios that don't fail are store in age 164

cum.failed.hist <- hist(cum.failed,breaks=seq(66,114),plot=FALSE)
cum.failed.df <- data.frame(age=66:113,cumsum(cum.failed.hist$counts)) # age is years relative to retirement agehead(cum)
cum.failed.df <- cum.failed.df[1:35,]

failed.percent <- cum.failed.df$cumsum.cum.failed.hist.counts./length(simulation$sim.index) # change numer of failures to cum failures

cum.failed.df$`Failed Portfolios`<-failed.percent # change cum failures to percent in data frame
cruin.df <- subset(simwisemeans.ci, select=c(time,cidmean))
short.cum.failed.df <- cbind(cruin.df,cum.failed.df)

a.df <-  subset(short.cum.failed.df,select=c(1,2,5))
names(a.df)<-c("Age","Ruined While Alive","Ruined Portfolio")
a.df <-melt(a.df,id.vars=1)

ggplot(data = a.df, aes(x = `Age`, y = value,group=variable,color=variable)) + geom_line() +
xlab("Age") + ylab("Percent of Failed Portfolios") +
theme_gray() +ggtitle(paste("Probability of Ruin\nversus Probability of Ruin While Alive\n ",wr*100,"% Annual Withdrawals",sep=""))


