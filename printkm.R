
  # Print combined Kaplan-Meier Portfolio Survival Curves
  # parameters 1:3 are data frames from simwisemeans.surv, each with a different annual withdrawal rate or household code (1=male, 2=female, 3=couple)
  # parameter 4 is a list of THREE text labels, either withdrawal rate or household type
  
  c.df <- merge(merge(simwisemeans.surv3,simwisemeans.surv4,by="time"),simwisemeans.surv5,by="time")
  c.df <- subset(c.df,select=-c(svar,svar.x,svar.y))
  c.df <- melt(c.df,id.vars = "time")
  
  cat("Labels =",labls,sep=" ")
  # Combined Curves
  
 ggplot(data = c.df, aes(x = c.df$time, y = value,color=variable)) + geom_line() +
    xlab("Age") + ylab("Conditional Probability of Portfolio Survival") + xlim(65, 100) + ylim(0, 1) + scale_colour_discrete(name = "Annual\nWithdrawal\nRate",labels=c("3%","4%","5%")) +
    theme_gray() +
    scale_y_continuous(breaks = seq(0,1,by = .05),limits=c(.5,1))  + 
    ggtitle(paste("Kaplan-Meier Portfolio Survival Curves",sep="")))





