Semantic Satiation among Lexically Ambiguous Words: Experiment 1a
=================================================================

Acquiring the Data
------------------

We will first (1) download the raw CSV from github; (2) convert the relevant variables---meaning bias, repetitions, relatedness, and participant ID---to factors; and (3) create three subsets of the data: one using the full dataset, one using a dataset with outliers $\pm$ 3 standard deviations removed, and a third with outliers more than 1.5 times the IQR above Q3 or below Q1 removed. We will then drop any null values from the dependent variable (MS response time) column.


```r
download.file("https://github.com/faulconbridge/semsat/raw/master/Semsat1a/Semsat1a_compiled.csv", 
    "semsat1a.csv", "wget", extra = "--no-check-certificate")
semsat1a <- read.csv("semsat1a.csv", header = TRUE, sep = ",")

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
raw.mean.stdev <- aggregate(raw.stdev$STDEV, by = list(raw.stdev$PID, raw.stdev$BIAS, 
    raw.stdev$RELATEDNESS, raw.stdev$REPS), FUN = "mean")
colnames(raw.mean.stdev) <- c("PID", "BIAS", "RELATEDNESS", "REPS", "STDEV")

# same thing trimming at +- 1.5 times the IQR
raw.iqr <- subset(raw, select = c(PID, BIAS, RELATEDNESS, REPS, ANSWER, IQR))
raw.iqr <- na.omit(raw.iqr)
raw.mean.iqr <- aggregate(raw.iqr$IQR, by = list(raw.iqr$PID, raw.iqr$BIAS, 
    raw.iqr$RELATEDNESS, raw.iqr$REPS), FUN = "mean")
colnames(raw.mean.iqr) <- c("PID", "BIAS", "RELATEDNESS", "REPS", "IQR")
```


Performing the Analyses
-----------------------


```r
raw.aov <- with(raw.mean, aov(MS ~ BIAS * RELATEDNESS * REPS + Error(PID)))

stdev.aov <- with(raw.mean.stdev, aov(STDEV ~ BIAS * RELATEDNESS * REPS + Error(PID)))

iqr.aov <- with(raw.mean.iqr, aov(IQR ~ BIAS * RELATEDNESS * REPS + Error(PID)))
```


### ANOVA of the untrimmed data ###
<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Wed May 14 15:24:53 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Df </TH> <TH> Sum Sq </TH> <TH> Mean Sq </TH> <TH> F value </TH> <TH> Pr(&gt;F) </TH>  </TR>
  <TR> <TD> Residuals </TD> <TD align="right"> 65 </TD> <TD align="right"> 366970847.13 </TD> <TD align="right"> 5645705.34 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
  <TR> <TD> BIAS                  </TD> <TD align="right"> 1 </TD> <TD align="right"> 81756.28 </TD> <TD align="right"> 81756.28 </TD> <TD align="right"> 0.21 </TD> <TD align="right"> 0.6487 </TD> </TR>
  <TR> <TD> RELATEDNESS           </TD> <TD align="right"> 1 </TD> <TD align="right"> 8309870.23 </TD> <TD align="right"> 8309870.23 </TD> <TD align="right"> 21.12 </TD> <TD align="right"> 0.0000 </TD> </TR>
  <TR> <TD> REPS                  </TD> <TD align="right"> 1 </TD> <TD align="right"> 1352698.96 </TD> <TD align="right"> 1352698.96 </TD> <TD align="right"> 3.44 </TD> <TD align="right"> 0.0643 </TD> </TR>
  <TR> <TD> BIAS:RELATEDNESS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 1946779.07 </TD> <TD align="right"> 1946779.07 </TD> <TD align="right"> 4.95 </TD> <TD align="right"> 0.0266 </TD> </TR>
  <TR> <TD> BIAS:REPS             </TD> <TD align="right"> 1 </TD> <TD align="right"> 22454.54 </TD> <TD align="right"> 22454.54 </TD> <TD align="right"> 0.06 </TD> <TD align="right"> 0.8113 </TD> </TR>
  <TR> <TD> RELATEDNESS:REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 1733961.92 </TD> <TD align="right"> 1733961.92 </TD> <TD align="right"> 4.41 </TD> <TD align="right"> 0.0363 </TD> </TR>
  <TR> <TD> BIAS:RELATEDNESS:REPS </TD> <TD align="right"> 1 </TD> <TD align="right"> 43912.58 </TD> <TD align="right"> 43912.58 </TD> <TD align="right"> 0.11 </TD> <TD align="right"> 0.7384 </TD> </TR>
  <TR> <TD> Residuals             </TD> <TD align="right"> 455 </TD> <TD align="right"> 178985423.37 </TD> <TD align="right"> 393374.56 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
   </TABLE>


### ANOVA of the stdev-trimmed data ###
<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Wed May 14 15:24:53 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Df </TH> <TH> Sum Sq </TH> <TH> Mean Sq </TH> <TH> F value </TH> <TH> Pr(&gt;F) </TH>  </TR>
  <TR> <TD> Residuals </TD> <TD align="right"> 65 </TD> <TD align="right"> 285615571.80 </TD> <TD align="right"> 4394085.72 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
  <TR> <TD> BIAS                  </TD> <TD align="right"> 1 </TD> <TD align="right"> 567168.62 </TD> <TD align="right"> 567168.62 </TD> <TD align="right"> 2.84 </TD> <TD align="right"> 0.0927 </TD> </TR>
  <TR> <TD> RELATEDNESS           </TD> <TD align="right"> 1 </TD> <TD align="right"> 5118593.07 </TD> <TD align="right"> 5118593.07 </TD> <TD align="right"> 25.63 </TD> <TD align="right"> 0.0000 </TD> </TR>
  <TR> <TD> REPS                  </TD> <TD align="right"> 1 </TD> <TD align="right"> 1831338.64 </TD> <TD align="right"> 1831338.64 </TD> <TD align="right"> 9.17 </TD> <TD align="right"> 0.0026 </TD> </TR>
  <TR> <TD> BIAS:RELATEDNESS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 583934.56 </TD> <TD align="right"> 583934.56 </TD> <TD align="right"> 2.92 </TD> <TD align="right"> 0.0880 </TD> </TR>
  <TR> <TD> BIAS:REPS             </TD> <TD align="right"> 1 </TD> <TD align="right"> 103489.31 </TD> <TD align="right"> 103489.31 </TD> <TD align="right"> 0.52 </TD> <TD align="right"> 0.4720 </TD> </TR>
  <TR> <TD> RELATEDNESS:REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 531985.40 </TD> <TD align="right"> 531985.40 </TD> <TD align="right"> 2.66 </TD> <TD align="right"> 0.1034 </TD> </TR>
  <TR> <TD> BIAS:RELATEDNESS:REPS </TD> <TD align="right"> 1 </TD> <TD align="right"> 20396.32 </TD> <TD align="right"> 20396.32 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.7495 </TD> </TR>
  <TR> <TD> Residuals             </TD> <TD align="right"> 455 </TD> <TD align="right"> 90885057.97 </TD> <TD align="right"> 199747.38 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
   </TABLE>


### ANOVA of the IQR-trimmed data ###
<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Wed May 14 15:24:53 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Df </TH> <TH> Sum Sq </TH> <TH> Mean Sq </TH> <TH> F value </TH> <TH> Pr(&gt;F) </TH>  </TR>
  <TR> <TD> Residuals </TD> <TD align="right"> 65 </TD> <TD align="right"> 190366606.85 </TD> <TD align="right"> 2928717.03 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
  <TR> <TD> BIAS                  </TD> <TD align="right"> 1 </TD> <TD align="right"> 481774.90 </TD> <TD align="right"> 481774.90 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 0.0259 </TD> </TR>
  <TR> <TD> RELATEDNESS           </TD> <TD align="right"> 1 </TD> <TD align="right"> 4463016.99 </TD> <TD align="right"> 4463016.99 </TD> <TD align="right"> 46.29 </TD> <TD align="right"> 0.0000 </TD> </TR>
  <TR> <TD> REPS                  </TD> <TD align="right"> 1 </TD> <TD align="right"> 1525242.72 </TD> <TD align="right"> 1525242.72 </TD> <TD align="right"> 15.82 </TD> <TD align="right"> 0.0001 </TD> </TR>
  <TR> <TD> BIAS:RELATEDNESS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 79408.92 </TD> <TD align="right"> 79408.92 </TD> <TD align="right"> 0.82 </TD> <TD align="right"> 0.3646 </TD> </TR>
  <TR> <TD> BIAS:REPS             </TD> <TD align="right"> 1 </TD> <TD align="right"> 122594.00 </TD> <TD align="right"> 122594.00 </TD> <TD align="right"> 1.27 </TD> <TD align="right"> 0.2601 </TD> </TR>
  <TR> <TD> RELATEDNESS:REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 93444.32 </TD> <TD align="right"> 93444.32 </TD> <TD align="right"> 0.97 </TD> <TD align="right"> 0.3254 </TD> </TR>
  <TR> <TD> BIAS:RELATEDNESS:REPS </TD> <TD align="right"> 1 </TD> <TD align="right"> 13795.82 </TD> <TD align="right"> 13795.82 </TD> <TD align="right"> 0.14 </TD> <TD align="right"> 0.7054 </TD> </TR>
  <TR> <TD> Residuals             </TD> <TD align="right"> 455 </TD> <TD align="right"> 43864187.74 </TD> <TD align="right"> 96404.81 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
   </TABLE>


We can visualize these differences as:
![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


And we can more formally conduct follow-up tests where appropriate. First, we will look at the bias by relatedness interaction:

```
## 
## 	Welch Two Sample t-test
## 
## data:  domrel and domunrel
## t = -2.51, df = 262, p-value = 0.01266
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -470.05  -56.82
## sample estimates:
## mean of x mean of y 
##      1624      1887
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  domrel and subrel
## t = -1.274, df = 261.7, p-value = 0.2038
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -336.16   72.04
## sample estimates:
## mean of x mean of y 
##      1624      1756
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  subrel and subunrel
## t = -1.249, df = 261.4, p-value = 0.2129
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -336.05   75.23
## sample estimates:
## mean of x mean of y 
##      1756      1886
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  domunrel and subunrel
## t = 0.0091, df = 261.8, p-value = 0.9927
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -207.2  209.1
## sample estimates:
## mean of x mean of y 
##      1887      1886
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

