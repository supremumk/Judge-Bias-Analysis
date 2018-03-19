# Judge-Bias-Analysis
Cooperated with Cheng Hua,who is Ph.D in School of Management at Yale in Oct 2016.<br>
Main ideas dedicated to the MathHorizons paper. Define Judge Bias as “a biased judge is one who awards higher scores than other judges to his own countrymen, but fails to award higher scores to non-countrymen”.
Our analysis reveals that nationalistic bias was prevalent in 2012 Olympic diving Competition.

## Step 1: Get Judge Information(Name & Country)

```r
data <- read.csv(file="Diving2012csv", as.is = TRUE)
UniRef <- data.frame(Judge=unique(data$Judge), JCountry=rep(NA,length(unique(data$Judge))))
UniRef$JCountry <- apply(as.matrix(UniRef$Judge),1,function(x) data$JCountry[which(data$Judge==x)[1]])
```

## Step 2: Analysis of Judging Bias by T-test

Main ideas dedicated to the [MathHorizons paper](http://www.stat.yale.edu/~jay/EmersonMaterials/MathHorizons.pdf).

Define Judge Bias as "a biased judge is one who awards higher scores than other judges to his own countrymen, but fails to award higher scores to non-countrymen".

Define “discrepancy” as the difference between a particular judge‟s score and the untrimmed mean of all 7 judges‟ scores. (We use the untrimmed mean because we are interested a judge's tendency to differ from other judges; we are not studying effects on thefinal calculated score, a topic for a different study.) A negative discrepancy indicates a score
below the panel average, and suspicious positive discrepancies might be evidence of bias.

## Step 3: Produce Main Table of Results. 

![alt tag](https://github.com/supremumk/Judge-Bias-Analysis/blob/master/discre_table.png)

## Step 4: Analysis of the Results
```r
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
```


