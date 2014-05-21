Semantic Satiation among Lexically Ambiguous Words: Experiment 1a
=================================================================

Acquiring the Data
------------------

We will first (1) download the raw CSV from github; (2) convert the relevant variables---meaning bias, repetitions, relatedness, and participant ID---to factors; and (3) create three subsets of the data: one using the full dataset, one using a dataset with outliers $\pm$ 3 standard deviations removed, and a third with outliers more than 1.5 times the IQR above Q3 or below Q1 removed. We will then drop any null values from the dependent variable (MS response time) column.


```r
download.file("https://github.com/faulconbridge/semsat/raw/master/Semsat1a/semsat1a.csv", 
    "semsat1a.csv", "wget", extra = "--no-check-certificate")
semsat1a <- read.csv("semsat1a.csv", header = TRUE, sep = ",")

library(nlme)

# Makes a copy of our data
raw <- semsat1a

# treats our within subjects variables as factors renames a couple of ugly
# variables
raw <- within(raw, {
    BIAS <- factor(BIAS)
    RELATEDNESS <- factor(RELATEDNESS)
    REPS <- factor(REPS)
    PID <- factor(PID)
    STDEV <- MS.3.STDEV
})

# strips out all filler items and incorrect responses
raw <- subset(raw, (BIAS == "dominant" | BIAS == "subordinate") & ((RELATEDNESS == 
    "related" & ANSWER == "M") | (RELATEDNESS == "unrelated" & ANSWER == "C")), 
    select = c(PID, BIAS, RELATEDNESS, REPS, ANSWER, MS, STDEV, IQR))

raw.mean <- aggregate(raw$MS, by = list(raw$PID, raw$BIAS, raw$RELATEDNESS, 
    raw$REPS), FUN = "mean")
colnames(raw.mean) <- c("PID", "BIAS", "RELATEDNESS", "REPS", "MS")

# subsets the data to remove any null values in the RTs trimmed to 3
# standard deviations
raw.stdev <- subset(raw, select = c(PID, BIAS, RELATEDNESS, REPS, ANSWER, STDEV))
raw.stdev <- na.omit(raw.stdev)

raw.stdev <- within(raw.stdev, {
    BIAS <- factor(BIAS)
    RELATEDNESS <- factor(RELATEDNESS)
    REPS <- factor(REPS)
    PID <- factor(PID)
})

raw.mean.stdev <- aggregate(raw.stdev$STDEV, by = list(raw.stdev$PID, raw.stdev$BIAS, 
    raw.stdev$RELATEDNESS, raw.stdev$REPS), FUN = "mean")
colnames(raw.mean.stdev) <- c("PID", "BIAS", "RELATEDNESS", "REPS", "STDEV")
```


Performing the Analyses
-----------------------


```r
raw.aov <- with(raw.mean, aov(MS ~ BIAS * RELATEDNESS * REPS + Error(PID/(BIAS * 
    RELATEDNESS * REPS))))
summary(raw.aov)
```

```
## 
## Error: PID
##           Df   Sum Sq Mean Sq F value Pr(>F)
## Residuals 65 3.67e+08 5645705               
## 
## Error: PID:BIAS
##           Df   Sum Sq Mean Sq F value Pr(>F)
## BIAS       1    81756   81756    0.25   0.62
## Residuals 65 21605285  332389               
## 
## Error: PID:RELATEDNESS
##             Df   Sum Sq Mean Sq F value Pr(>F)   
## RELATEDNESS  1  8309870 8309870    10.6 0.0018 **
## Residuals   65 50872746  782658                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:REPS
##           Df   Sum Sq Mean Sq F value Pr(>F)  
## REPS       1  1352699 1352699    3.86  0.054 .
## Residuals 65 22768745  350288                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:BIAS:RELATEDNESS
##                  Df   Sum Sq Mean Sq F value Pr(>F)   
## BIAS:RELATEDNESS  1  1946779 1946779    10.7 0.0018 **
## Residuals        65 11882474  182807                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:BIAS:REPS
##           Df   Sum Sq Mean Sq F value Pr(>F)
## BIAS:REPS  1    22455   22455    0.04   0.83
## Residuals 65 32826351  505021               
## 
## Error: PID:RELATEDNESS:REPS
##                  Df   Sum Sq Mean Sq F value Pr(>F)  
## RELATEDNESS:REPS  1  1733962 1733962    4.98  0.029 *
## Residuals        65 22629905  348152                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:BIAS:RELATEDNESS:REPS
##                       Df   Sum Sq Mean Sq F value Pr(>F)
## BIAS:RELATEDNESS:REPS  1    43913   43913    0.17   0.68
## Residuals             65 16399919  252306
```

```r

stdev.aov <- with(raw.mean.stdev, aov(STDEV ~ BIAS * RELATEDNESS * REPS + Error(PID/(BIAS * 
    RELATEDNESS * REPS))))
summary(stdev.aov)
```

```
## 
## Error: PID
##           Df   Sum Sq Mean Sq F value Pr(>F)
## Residuals 65 2.86e+08 4394086               
## 
## Error: PID:BIAS
##           Df  Sum Sq Mean Sq F value Pr(>F)  
## BIAS       1  567169  567169    3.79  0.056 .
## Residuals 65 9719440  149530                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:RELATEDNESS
##             Df   Sum Sq Mean Sq F value Pr(>F)   
## RELATEDNESS  1  5118593 5118593    10.7 0.0017 **
## Residuals   65 31121335  478790                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:REPS
##           Df   Sum Sq Mean Sq F value Pr(>F)   
## REPS       1  1831339 1831339    8.26 0.0055 **
## Residuals 65 14406208  221634                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:BIAS:RELATEDNESS
##                  Df  Sum Sq Mean Sq F value Pr(>F)  
## BIAS:RELATEDNESS  1  583935  583935    5.14  0.027 *
## Residuals        65 7385115  113617                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:BIAS:REPS
##           Df   Sum Sq Mean Sq F value Pr(>F)
## BIAS:REPS  1   103489  103489     0.5   0.48
## Residuals 65 13528234  208127               
## 
## Error: PID:RELATEDNESS:REPS
##                  Df   Sum Sq Mean Sq F value Pr(>F)  
## RELATEDNESS:REPS  1   531985  531985    3.27  0.075 .
## Residuals        65 10563487  162515                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:BIAS:RELATEDNESS:REPS
##                       Df  Sum Sq Mean Sq F value Pr(>F)
## BIAS:RELATEDNESS:REPS  1   20396   20396    0.32   0.57
## Residuals             65 4161239   64019
```


We can visualize these differences as:
![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 




Some descriptive statistics for the main effects:

```r
bias <- aggregate(raw.stdev$STDEV, by = list(raw.stdev$PID, raw.stdev$BIAS), 
    mean)
describeBy(bias$x, group = bias$Group.2)
```

```
## group: dominant
##   vars  n mean    sd median trimmed   mad min  max range skew kurtosis
## 1    1 66 1748 754.8   1536    1633 493.3 846 4227  3381 1.42     1.62
##     se
## 1 92.9
## -------------------------------------------------------- 
## group: subordinate
##   vars  n mean    sd median trimmed   mad   min  max range skew kurtosis
## 1    1 66 1811 735.2   1670    1707 581.2 792.3 4606  3813 1.45     2.48
##      se
## 1 90.49
```

```r

relatedness <- aggregate(raw.stdev$STDEV, by = list(raw.stdev$PID, raw.stdev$RELATEDNESS), 
    mean)
describeBy(relatedness$x, group = relatedness$Group.2)
```

```
## group: related
##   vars  n mean    sd median trimmed   mad   min  max range skew kurtosis
## 1    1 66 1675 748.2   1446    1554 513.4 820.9 4064  3244 1.48     1.83
##     se
## 1 92.1
## -------------------------------------------------------- 
## group: unrelated
##   vars  n mean    sd median trimmed mad   min  max range skew kurtosis  se
## 1    1 66 1887 812.7   1708    1763 606 833.2 4779  3945 1.62     2.83 100
```

```r

reps <- aggregate(raw.stdev$STDEV, by = list(raw.stdev$PID, raw.stdev$REPS), 
    mean)
describeBy(reps$x, group = reps$Group.2)
```

```
## group: long
##   vars  n mean  sd median trimmed   mad   min  max range skew kurtosis
## 1    1 66 1836 799   1597    1717 527.5 807.3 4554  3747 1.45     1.82
##      se
## 1 98.36
## -------------------------------------------------------- 
## group: short
##   vars  n mean    sd median trimmed   mad   min  max range skew kurtosis
## 1    1 66 1722 707.5   1578    1624 611.2 850.6 4193  3343 1.33     1.66
##      se
## 1 87.08
```


And we can more formally conduct follow-up tests where appropriate. First, we will look at the bias by relatedness interaction:

```
## 
## 	Welch Two Sample t-test
## 
## data:  domrel and domunrel
## t = -4.547, df = 2182, p-value = 5.751e-06
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -348.8 -138.6
## sample estimates:
## mean of x mean of y 
##      1624      1868
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  domrel and subrel
## t = -2.223, df = 1934, p-value = 0.02631
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -235.75  -14.77
## sample estimates:
## mean of x mean of y 
##      1624      1749
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  subrel and subunrel
## t = -2.348, df = 1884, p-value = 0.01898
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -235.97  -21.17
## sample estimates:
## mean of x mean of y 
##      1749      1878
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  domunrel and subunrel
## t = -0.1953, df = 2174, p-value = 0.8451
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -112.0   91.7
## sample estimates:
## mean of x mean of y 
##      1868      1878
```


Next, the relatedness by repetition interaction:

```
## 
## 	Welch Two Sample t-test
## 
## data:  relshort and rellong
## t = -1.754, df = 257.1, p-value = 0.08069
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -384.83   22.29
## sample estimates:
## mean of x mean of y 
##      1599      1780
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  relshort and unrelshort
## t = -2.581, df = 259.6, p-value = 0.01041
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -459.10  -61.71
## sample estimates:
## mean of x mean of y 
##      1599      1860
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  rellong and unrellong
## t = -1.236, df = 261.5, p-value = 0.2178
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -346.11   79.23
## sample estimates:
## mean of x mean of y 
##      1780      1914
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  unrelshort and unrellong
## t = -0.514, df = 262, p-value = 0.6077
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -262.3  153.7
## sample estimates:
## mean of x mean of y 
##      1860      1914
```

