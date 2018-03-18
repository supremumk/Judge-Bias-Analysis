
## Step 1: Get Judge Information(Name & Country)


data <- read.csv(file="Diving2012csv", as.is = TRUE)
UniRef <- data.frame(Judge=unique(data$Judge), JCountry=rep(NA,length(unique(data$Judge))))
UniRef$JCountry <- apply(as.matrix(UniRef$Judge),1,function(x) data$JCountry[which(data$Judge==x)[1]])


## Step 2: Analysis of Judging Bias by T-test


# Match the nationality of judges and divers
data$match <- data$Country == data$JCountry

# Calculate untrimmed mean, showing a judge's tendency to differ from other judges
temp <- tapply(as.numeric(data$JScore),rep(1:(nrow(data)/7), each=7), mean) 
data$avg <- rep(temp, each=7)
# Calculate the difference between a particular judge's score and the untrimmed mean 
data$discrepancy <- as.numeric(data$JScore) - data$avg

# Find judges whose nationality matched that of the particular diver.
ismatch <- apply(as.matrix(UniRef[,1]),1,function(x) sum(data$match[data$Judge==x])>0)
UniRef$p.value <- rep(0, length(ismatch))
UniRef$ADmatch <- rep(0, length(ismatch))
UniRef$ADnomatch <- rep(0, length(ismatch))

# Judge loop begins here
for (thisjudge in UniRef[ismatch,1]) {
  y <- data[data$Judge==thisjudge,]
  # T-test assuming judges bias towards their own countrymen
  # H0: no bias
  # H1: bias
  test <- t.test(y$discrepancy[y$match],y$discrepancy[!y$match], alternative="great")
  UniRef[UniRef[,1]==thisjudge, 3] <- test$p.value
  UniRef[UniRef[,1]==thisjudge, 4] <- mean(y$discrepancy[y$match])
  UniRef[UniRef[,1]==thisjudge, 5] <- mean(y$discrepancy[!y$match],na.rm = TRUE)
}


library("xtable")

# Paste judge name and country into one column

UniRef[,6] <- paste(UniRef[,1]," (",UniRef[,2],")",sep="")
UniRef[,6] <- gsub("[A-Z]( [A-Z][a-z]+)",",\\1",UniRef[,6])
UniRef <- UniRef[ismatch,]

# Find number of Matched Dives

matchnum <- apply(as.matrix(UniRef[, 1]), 1, function(x) sum(data$match[data$Judge==x]))

# Find number of Non-Matched Dives

nonmatchnum <- apply(as.matrix(UniRef[, 1]), 1, function(x) sum(data$match[data$Judge!= x]))

# Create the table of results

mytable <- data.frame(UniRef[, 6],  matchnum, UniRef$ADmatch, nonmatchnum,
                      UniRef$ADnomatch,UniRef[, 4]-UniRef[, 5],UniRef$p.value)
names(mytable)<- c("Judge", "Number of Matched Dives", "Average Discrepancy for Matched Dives",
                   "Number of Non-Matched Dives", "Average Discrepancy for Non-Matched Dives",
                   "Difference of Average Discrepancies(DoAD)", "p-value")
yourtable <- xtable(mytable, hline.after=c(-1, 0), align= c("|c|", "p{0.3\\textwidth}|", "p{0.07\\textwidth}|", "p{0.1\\textwidth}|", "p{0.1\\textwidth}|", "p{0.1\\textwidth}|", "p{0.1\\textwidth}|", "p{0.05\\textwidth}|"))
print(yourtable, include.rownames = FALSE )



## Step 4: Analysis of the Results

# Find the most and least biased judges according to p-value
# High p-values indicate the discrepancy is likely assuming judge had been unbiased.
# So the higher the p-value is, the least biased a judge is.

# Find the most biased judge with lowest p-value

ind0 <- UniRef$Judge[which.min(mytable$`p-value`)]
y <- data[data$Judge == ind0,]; as.character(ind0)
par(mfrow=c(2,1))
hist(y$discrepancy[y$match],
     main=paste(UniRef$Judge[which.min(mytable$`p-value`)],":Matching Groups"),
     xlab="Discrepancies", xlim=c(-2,2))
abline(v=mean(y$discrepancy[y$match],na.rm = TRUE),col="red",lwd=3,lty=3)
hist(y$discrepancy[!y$match],
     main=paste(UniRef$Judge[which.min(mytable$`p-value`)],":Non-Matching Groups"),
     xlab="Discrepancies", xlim=c(-2,2))
abline(v=mean(y$discrepancy[!y$match],na.rm = TRUE),col="red",lwd=3,lty=3)

# Find the least biased judge with highest p-value

ind2 <- UniRef$Judge[which.max(mytable$`p-value`)]
y2 <- data[data$Judge == ind2,]; as.character(ind2)
hist(y2$discrepancy[y2$match],
     main=paste(UniRef$Judge[which.max(mytable$`p-value`)],":Matching Groups"),
     xlab="Discrepancies", xlim=c(-2,2))
abline(v=mean(y2$discrepancy[y2$match],na.rm = TRUE),col="red",lwd=3,lty=3)
hist(y2$discrepancy[!y2$match],
     main=paste(UniRef$Judge[which.max(mytable$`p-value`)],":Non-Matching Groups"),
     xlab="Discrepancies", xlim=c(-2,2))
abline(v=mean(y2$discrepancy[!y2$match],na.rm = TRUE),col="red",lwd=3,lty=3)

# We assume that judges bias towards their own country, but the least biased judge is not biased toward his matching 
# country according to the histogram.
# So next we find the least biased judge toward his own country by limiting DoAd > 0

ind3 <- which(mytable[,6]>0)
y3 <- data[data$Judge==UniRef$Judge[ind3][which.max(mytable$`p-value`[ind3])],]; 
y3$Judge[1]
hist(y3$discrepancy[y3$match],
     main=paste(UniRef$Judge[ind3][which.max(mytable$`p-value`[ind3])],":Matching Groups"), xlab="Discrepancies", 
     xlim=c(-2,2))
abline(v=mean(y3$discrepancy[y3$match],na.rm = TRUE),col="red",lwd=3,lty=3)
hist(y3$discrepancy[!y3$match],
     main=paste(UniRef$Judge[ind3][which.max(mytable$`p-value`[ind3])],":Non-Matching Groups"), xlab="Discrepancies", xlim=c(-2,2) )
abline(v=mean(y3$discrepancy[!y3$match],na.rm = TRUE),col="red",lwd=3,lty=3)

# The proportion of p-values < 0.1
sum(mytable$`p-value`< 0.1) /dim(mytable)[1]

# It reveals that nationalistic bias was prevalent in 2012 Olympic diving Competition


