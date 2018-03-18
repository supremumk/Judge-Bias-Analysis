# Judge-Bias-Analysis
Cooperated with Cheng Hua,who is Ph.D in School of Management at Yale in Oct 2016.<br>
Main ideas dedicated to the MathHorizons paper. Define Judge Bias as “a biased judge is one who awards higher scores than other judges to his own countrymen, but fails to award higher scores to non-countrymen”.
Our analysis reveals that nationalistic bias was prevalent in 2012 Olympic diving Competition.

## Step 1: Get Judge Information(Name & Country)

```{r ref}
data <- read.csv(file="Diving2012csv", as.is = TRUE)
UniRef <- data.frame(Judge=unique(data$Judge), JCountry=rep(NA,length(unique(data$Judge))))
UniRef$JCountry <- apply(as.matrix(UniRef$Judge),1,function(x) data$JCountry[which(data$Judge==x)[1]])
```

## Step 2: Analysis of Judging Bias by T-test

Main ideas dedicated to the MathHorizons paper. Define Judge Bias as "a biased judge is one who awards higher scores than other judges to his own countrymen, but fails to award higher scores to non-countrymen".
```{r diving}
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
