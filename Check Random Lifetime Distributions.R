#
# Check random lifetime distributions
#
hh <-c("male","female","joint")
soc1 <- read.csv("Male Random Lifetimes from Age 65.csv",header=FALSE,nrows=10000)
soc2 <- read.csv("Female Random Lifetimes from Age 65.csv",header=FALSE,nrows=10000)
soc3 <- read.csv("Joint Random Lifetimes from Age 65.csv",header=FALSE,nrows=10000)
socages <- read.csv("SOC Ages.csv", header=TRUE) #compare to actuarial tables

# hist(socages[,3])

lifeExpect <- data.frame(soc1,soc2,soc3,socages[,1],socages[,2],socages[,3])
colnames(lifeExpect) <- c("MaleSimulated","FemaleSimulated","CoupleSimulated","MaleActuarial","FemaleActuarial","CoupleActuarial")

i <- 1


cat(paste("\n\n----------",sep=" "))
cat(paste("\nParameters for Male, Female and Couples Simulated Life Expectancy Used in this Study",sep=" "))
cat(paste("\n---------- ",sep=" "))
cat(paste("\n\nHousehold is",hh[1],sep=" "))
cat("\nMean lifetime",mean(soc1[,1]),"years, median lifetime ",median(soc1[,1])," years",sep=" ")
cat("\nMinimum lifetime ",min(soc1[,1]),"years, maximum lifetime",max(soc1)," years",sep=" ")
cat("\n",sep="")
cat(paste("\nHousehold is",hh[2],sep=" "))
cat("\nMean lifetime",mean(soc2[,1]),"years, median lifetime ",median(soc2[,1])," years",sep=" ")
cat("\nMinimum lifetime ",min(soc2[,1]),"years, maximum lifetime",max(soc2[,1])," years",sep=" ")
cat("\n",sep="")
cat(paste("\nHousehold is",hh[3],"with at least one survivor",sep=" "))
cat("\nMean lifetime",mean(soc3[,1]),"years, median lifetime ",median(soc3[,1])," years",sep=" ")
cat("\nMinimum lifetime ",min(soc3[,1]),"years, maximum lifetime",max(soc3[,1])," years",sep=" ")
socmd <- read.csv("SOC Male Deaths per Thousand 2.csv", header=TRUE,nrows=50)

p <- ggplot(lifeExpect) +
geom_histogram(aes(MaleSimulated,fill="#c0392b"),binwidth=1,alpha=0.75,col="white") +
geom_histogram(aes(MaleActuarial,fill="black",show_guide=FALSE),binwidth=1, alpha=0,col="black") +
labs(title="10,000 Simulated Male Lifetimes vs. Actuarial Expectations", x="Simulated Age", y="No. of Scenarios This Age") +
# scale_x_continuous(breaks=seq(66,105, by=1)) +
scale_x_continuous(limits = c(66, 105)) +
scale_fill_manual(values=c("#c0392b", "black"), 
                    name="Legend",
                    breaks=c("#c0392b", "black"),
                    labels=c("Simulated Lifetimes", "Actuarial Expectations"))
# plot graph
p

p <- ggplot(lifeExpect) +
  geom_histogram(aes(FemaleSimulated,fill="#c0392b"),binwidth=1,alpha=0.75,col="white") +
  geom_histogram(aes(FemaleActuarial,fill="black",show_guide=FALSE),binwidth=1, alpha=0,col="black") +
  labs(title="10,000 Simulated Female Lifetimes vs. Actuarial Expectations", x="Simulated Age", y="No. of Scenarios This Age") +
  # scale_x_continuous(breaks=seq(66,105, by=1)) +
  scale_x_continuous(limits = c(66, 105)) +
  scale_fill_manual(values=c("#c0392b", "black"), 
                    name="Legend",
                    breaks=c("#c0392b", "black"),
                    labels=c("10,000 Simulated Lifetimes", "Actuarial Expectations"))
p

p <- ggplot(lifeExpect) +
  geom_histogram(aes(CoupleSimulated,fill="#c0392b"),binwidth=1,alpha=0.75,col="white") +
  geom_histogram(aes(CoupleActuarial,fill="black",show_guide=FALSE),binwidth=1, alpha=0,col="black") +
  labs(title="10,000 Simulated Couple Lifetimes vs. Actuarial Expectations", x="Simulated Age", y="No. of Scenarios This Age") +
  # scale_x_continuous(breaks=seq(66,105, by=1)) +
  scale_x_continuous(limits = c(66, 105)) +
  scale_fill_manual(values=c("#c0392b", "black"), 
                    name="Legend",
                    breaks=c("#c0392b", "black"),
                    labels=c("10,000 Simulated Lifetimes", "Actuarial Expectations"))
p

