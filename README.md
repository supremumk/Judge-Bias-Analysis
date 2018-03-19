# Judge-Bias-Analysis
Cooperated with Cheng Hua,who is Ph.D in School of Management at Yale in Oct 2016.<br>
Main ideas dedicated to the MathHorizons paper. Define Judge Bias as “a biased judge is one who awards higher scores than other judges to his own countrymen, but fails to award higher scores to non-countrymen”.
Our analysis reveals that nationalistic bias was prevalent in 2012 Olympic diving Competition.

## Step 1: Get Judge Information(Name & Country)

Scraped the pdf files that document 2012 Olympics diving score and save the information into Diving2012.csv.


## Step 2: Analysis of Judging Bias by T-test

Main ideas dedicated to the [MathHorizons paper](http://www.stat.yale.edu/~jay/EmersonMaterials/MathHorizons.pdf).

Define Judge Bias as "a biased judge is one who awards higher scores than other judges to his own countrymen, but fails to award higher scores to non-countrymen".

Define “discrepancy” as the difference between a particular judge‟s score and the untrimmed mean of all 7 judges‟ scores. (We use the untrimmed mean because we are interested a judge's tendency to differ from other judges; we are not studying effects on thefinal calculated score, a topic for a different study.) A negative discrepancy indicates a score
below the panel average, and suspicious positive discrepancies might be evidence of bias.

## Step 3: Produce Main Table of Results. 

![alt tag](https://github.com/supremumk/Judge-Bias-Analysis/blob/master/discre_table.png)

## Step 4: Analysis of the Results

 -Find the most and least biased judges according to p-value.
 -High p-values indicate the discrepancy is likely assuming judge had been unbiased.
 -So the higher the p-value is, the least biased a judge is.


#### Find the most biased judge with lowest p-value
 ![alt tag](https://github.com/supremumk/Judge-Bias-Analysis/blob/master/most_bias_judge.png)


#### Find the least biased judge with highest p-value
 ![alt tag](https://github.com/supremumk/Judge-Bias-Analysis/blob/master/least_bias_judge.png)


 We assume that judges bias towards their own country, but the least biased judge is not biased toward his matching 
 country according to the histogram.
 So next we find the least biased judge toward his own country by limiting DoAd > 0
  ![alt tag](https://github.com/supremumk/Judge-Bias-Analysis/blob/master/least_bias_real.png)

##### The proportion of p-values < 0.1
sum(mytable$`p-value`< 0.1) /dim(mytable)[1]=0.4375

#### It reveals that nationalistic bias was prevalent in 2012 Olympic diving Competition.



