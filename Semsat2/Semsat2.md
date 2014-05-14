Semantic Satiation among Lexically Ambiguous Words: Experiment 2
================================================================

Acquiring the Data
------------------

We will first download the raw CSV from github and do a bit of preprocessing on it:


```r
download.file("https://github.com/faulconbridge/semsat/raw/master/Semsat2/Semsat2_compiled.csv", 
    "semsat2.csv", "wget", extra = "--no-check-certificate")
semsat2 <- read.csv("semsat2.csv", header = TRUE, sep = ",")

raw <- semsat2

# Specify which variables should be treated as factors
raw <- within(raw, {
    BIAS <- factor(BIAS)
    HomType <- factor(HomType)
    REPS <- factor(REPS)
    PID <- factor(PID)
    STDEV <- MS.3.STDEV
    IQR <- MS.IQR
})

# Strip out all filler and incorrectly-answered items
raw <- subset(raw, (BIAS == "dominant" & ANSWER == "M") | (BIAS == "subordinate" & 
    ANSWER == "M") | (BIAS == "unrelated" & (HomType == "NV" | HomType == "NN") & 
    ANSWER == "C"), select = c(PID, BIAS, HomType, REPS, ANSWER, MS, STDEV, 
    IQR))

# Strip out participants who encountered bugs during the experiment
raw <- subset(raw, PID != 57 | PID != 42 | PID != 14, select = c(PID, BIAS, 
    HomType, REPS, ANSWER, MS, STDEV, IQR))

# Subset data to include untrimmed MS response times
raw.MS <- subset(raw, select = c(PID:MS))
raw.MS <- na.omit(raw.MS)

# Take the participant average RTs for each condition
raw.mean.MS <- aggregate(raw.MS$MS, by = list(raw.MS$PID, raw.MS$BIAS, raw.MS$HomType, 
    raw.MS$REPS), FUN = "mean")
colnames(raw.mean.MS) <- c("PID", "BIAS", "HomType", "REPS", "MS")

# Subset the data to remove outliers beyond +-3 sd
raw.stdev <- subset(raw, select = c(PID, BIAS, HomType, REPS, STDEV))
raw.stdev <- na.omit(raw.stdev)

# Take the participant average RTs for each condition

raw.mean.stdev <- aggregate(raw.stdev$STDEV, by = list(raw.stdev$PID, raw.stdev$BIAS, 
    raw.stdev$HomType, raw.stdev$REPS), FUN = "mean")
colnames(raw.mean.stdev) <- c("PID", "BIAS", "HomType", "REPS", "STDEV")

# Subset the data to remove outliers 1.5x IQR beyond Q1 or Q3
raw.IQR <- subset(raw, select = c(PID, BIAS, HomType, REPS, IQR))
raw.IQR <- na.omit(raw.IQR)

# Take the participant average RTs for each condition
raw.mean.IQR <- aggregate(raw.IQR$IQR, by = list(raw.IQR$PID, raw.IQR$BIAS, 
    raw.IQR$HomType, raw.IQR$REPS), FUN = "mean")
colnames(raw.mean.IQR) <- c("PID", "BIAS", "HomType", "REPS", "IQR")
```


Performing the Analyses
-----------------------
First we can look at the untrimmed data:

```r
raw.aov <- aov(MS ~ BIAS * HomType * REPS + Error(PID), data = raw.mean.MS)
```

```
## Loading required package: mvtnorm
## Loading required package: survival
## Loading required package: splines
## Loading required package: TH.data
```

<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Wed May 14 13:26:57 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Df </TH> <TH> Sum Sq </TH> <TH> Mean Sq </TH> <TH> F value </TH> <TH> Pr(&gt;F) </TH>  </TR>
  <TR> <TD> BIAS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 4853496.45 </TD> <TD align="right"> 4853496.45 </TD> <TD align="right"> 0.19 </TD> <TD align="right"> 0.6614 </TD> </TR>
  <TR> <TD> HomType   </TD> <TD align="right"> 1 </TD> <TD align="right"> 12823397.06 </TD> <TD align="right"> 12823397.06 </TD> <TD align="right"> 0.51 </TD> <TD align="right"> 0.4769 </TD> </TR>
  <TR> <TD> REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 90025073.96 </TD> <TD align="right"> 90025073.96 </TD> <TD align="right"> 3.59 </TD> <TD align="right"> 0.0619 </TD> </TR>
  <TR> <TD> Residuals </TD> <TD align="right"> 80 </TD> <TD align="right"> 2008699426.87 </TD> <TD align="right"> 25108742.84 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
  <TR> <TD> BIAS              </TD> <TD align="right"> 2 </TD> <TD align="right"> 38663066.69 </TD> <TD align="right"> 19331533.34 </TD> <TD align="right"> 18.26 </TD> <TD align="right"> 0.0000 </TD> </TR>
  <TR> <TD> HomType           </TD> <TD align="right"> 1 </TD> <TD align="right"> 5594529.45 </TD> <TD align="right"> 5594529.45 </TD> <TD align="right"> 5.28 </TD> <TD align="right"> 0.0218 </TD> </TR>
  <TR> <TD> REPS              </TD> <TD align="right"> 1 </TD> <TD align="right"> 482447.51 </TD> <TD align="right"> 482447.51 </TD> <TD align="right"> 0.46 </TD> <TD align="right"> 0.4998 </TD> </TR>
  <TR> <TD> BIAS:HomType      </TD> <TD align="right"> 2 </TD> <TD align="right"> 7350351.61 </TD> <TD align="right"> 3675175.81 </TD> <TD align="right"> 3.47 </TD> <TD align="right"> 0.0315 </TD> </TR>
  <TR> <TD> BIAS:REPS         </TD> <TD align="right"> 2 </TD> <TD align="right"> 674669.51 </TD> <TD align="right"> 337334.75 </TD> <TD align="right"> 0.32 </TD> <TD align="right"> 0.7272 </TD> </TR>
  <TR> <TD> HomType:REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 518789.63 </TD> <TD align="right"> 518789.63 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> 0.4841 </TD> </TR>
  <TR> <TD> BIAS:HomType:REPS </TD> <TD align="right"> 2 </TD> <TD align="right"> 5567072.26 </TD> <TD align="right"> 2783536.13 </TD> <TD align="right"> 2.63 </TD> <TD align="right"> 0.0727 </TD> </TR>
  <TR> <TD> Residuals         </TD> <TD align="right"> 908 </TD> <TD align="right"> 961391831.98 </TD> <TD align="right"> 1058801.58 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
   </TABLE>


Then trimming based on standard deviations:

```r
stdev.aov <- aov(STDEV ~ BIAS * HomType * REPS + Error(PID), data = raw.mean.stdev)
```

<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Wed May 14 13:26:57 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Df </TH> <TH> Sum Sq </TH> <TH> Mean Sq </TH> <TH> F value </TH> <TH> Pr(&gt;F) </TH>  </TR>
  <TR> <TD> BIAS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 7873612.05 </TD> <TD align="right"> 7873612.05 </TD> <TD align="right"> 0.38 </TD> <TD align="right"> 0.5374 </TD> </TR>
  <TR> <TD> HomType   </TD> <TD align="right"> 1 </TD> <TD align="right"> 11262294.84 </TD> <TD align="right"> 11262294.84 </TD> <TD align="right"> 0.55 </TD> <TD align="right"> 0.4610 </TD> </TR>
  <TR> <TD> REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 96854068.86 </TD> <TD align="right"> 96854068.86 </TD> <TD align="right"> 4.72 </TD> <TD align="right"> 0.0328 </TD> </TR>
  <TR> <TD> Residuals </TD> <TD align="right"> 80 </TD> <TD align="right"> 1641606933.76 </TD> <TD align="right"> 20520086.67 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
  <TR> <TD> BIAS              </TD> <TD align="right"> 2 </TD> <TD align="right"> 25243306.61 </TD> <TD align="right"> 12621653.30 </TD> <TD align="right"> 22.25 </TD> <TD align="right"> 0.0000 </TD> </TR>
  <TR> <TD> HomType           </TD> <TD align="right"> 1 </TD> <TD align="right"> 1094335.32 </TD> <TD align="right"> 1094335.32 </TD> <TD align="right"> 1.93 </TD> <TD align="right"> 0.1652 </TD> </TR>
  <TR> <TD> REPS              </TD> <TD align="right"> 1 </TD> <TD align="right"> 2040907.80 </TD> <TD align="right"> 2040907.80 </TD> <TD align="right"> 3.60 </TD> <TD align="right"> 0.0582 </TD> </TR>
  <TR> <TD> BIAS:HomType      </TD> <TD align="right"> 2 </TD> <TD align="right"> 418252.47 </TD> <TD align="right"> 209126.24 </TD> <TD align="right"> 0.37 </TD> <TD align="right"> 0.6918 </TD> </TR>
  <TR> <TD> BIAS:REPS         </TD> <TD align="right"> 2 </TD> <TD align="right"> 767006.70 </TD> <TD align="right"> 383503.35 </TD> <TD align="right"> 0.68 </TD> <TD align="right"> 0.5089 </TD> </TR>
  <TR> <TD> HomType:REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 821331.90 </TD> <TD align="right"> 821331.90 </TD> <TD align="right"> 1.45 </TD> <TD align="right"> 0.2292 </TD> </TR>
  <TR> <TD> BIAS:HomType:REPS </TD> <TD align="right"> 2 </TD> <TD align="right"> 3666420.60 </TD> <TD align="right"> 1833210.30 </TD> <TD align="right"> 3.23 </TD> <TD align="right"> 0.0400 </TD> </TR>
  <TR> <TD> Residuals         </TD> <TD align="right"> 908 </TD> <TD align="right"> 515093667.40 </TD> <TD align="right"> 567283.77 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
   </TABLE>


And finally based on interquatrile range:

```r
iqr.aov <- aov(IQR ~ BIAS * HomType * REPS + Error(PID), data = raw.mean.IQR)
```

<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Wed May 14 13:26:57 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Df </TH> <TH> Sum Sq </TH> <TH> Mean Sq </TH> <TH> F value </TH> <TH> Pr(&gt;F) </TH>  </TR>
  <TR> <TD> BIAS      </TD> <TD align="right"> 2 </TD> <TD align="right"> 45028697.46 </TD> <TD align="right"> 22514348.73 </TD> <TD align="right"> 2.12 </TD> <TD align="right"> 0.1265 </TD> </TR>
  <TR> <TD> HomType   </TD> <TD align="right"> 1 </TD> <TD align="right"> 14608488.81 </TD> <TD align="right"> 14608488.81 </TD> <TD align="right"> 1.38 </TD> <TD align="right"> 0.2441 </TD> </TR>
  <TR> <TD> Residuals </TD> <TD align="right"> 80 </TD> <TD align="right"> 848850768.72 </TD> <TD align="right"> 10610634.61 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
  <TR> <TD> BIAS              </TD> <TD align="right"> 2 </TD> <TD align="right"> 17773069.82 </TD> <TD align="right"> 8886534.91 </TD> <TD align="right"> 34.93 </TD> <TD align="right"> 0.0000 </TD> </TR>
  <TR> <TD> HomType           </TD> <TD align="right"> 1 </TD> <TD align="right"> 1034003.46 </TD> <TD align="right"> 1034003.46 </TD> <TD align="right"> 4.06 </TD> <TD align="right"> 0.0441 </TD> </TR>
  <TR> <TD> REPS              </TD> <TD align="right"> 1 </TD> <TD align="right"> 3136651.91 </TD> <TD align="right"> 3136651.91 </TD> <TD align="right"> 12.33 </TD> <TD align="right"> 0.0005 </TD> </TR>
  <TR> <TD> BIAS:HomType      </TD> <TD align="right"> 2 </TD> <TD align="right"> 935814.03 </TD> <TD align="right"> 467907.02 </TD> <TD align="right"> 1.84 </TD> <TD align="right"> 0.1595 </TD> </TR>
  <TR> <TD> BIAS:REPS         </TD> <TD align="right"> 2 </TD> <TD align="right"> 342143.48 </TD> <TD align="right"> 171071.74 </TD> <TD align="right"> 0.67 </TD> <TD align="right"> 0.5107 </TD> </TR>
  <TR> <TD> HomType:REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 199345.74 </TD> <TD align="right"> 199345.74 </TD> <TD align="right"> 0.78 </TD> <TD align="right"> 0.3763 </TD> </TR>
  <TR> <TD> BIAS:HomType:REPS </TD> <TD align="right"> 2 </TD> <TD align="right"> 9811.62 </TD> <TD align="right"> 4905.81 </TD> <TD align="right"> 0.02 </TD> <TD align="right"> 0.9809 </TD> </TR>
  <TR> <TD> Residuals         </TD> <TD align="right"> 906 </TD> <TD align="right"> 230500108.53 </TD> <TD align="right"> 254415.13 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
   </TABLE>


Now we will perform a test on the three-way BIAS x HomType x REPS interaction:

```r
raw.mean.stdev$BHR <- with(raw.mean.stdev, interaction(BIAS, HomType, REPS, 
    sep = "x"))

raw.mean.stdev$BHR <- factor(raw.mean.stdev$BHR)

stdev.lme <- lme(STDEV ~ BHR, random = ~1 | PID, data = raw.mean.stdev)

l2 <- glht(stdev.lme, linfct = mcp(BHR = "Tukey"))
summary(l2)
```

```
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: lme.formula(fixed = STDEV ~ BHR, data = raw.mean.stdev, random = ~1 | 
##     PID)
## 
## Linear Hypotheses:
##                                                  Estimate Std. Error
## subordinatexNNxlong - dominantxNNxlong == 0        338.25     116.22
## unrelatedxNNxlong - dominantxNNxlong == 0          460.06     116.22
## dominantxNVxlong - dominantxNNxlong == 0           263.27     116.22
## subordinatexNVxlong - dominantxNNxlong == 0        470.16     116.22
## unrelatedxNVxlong - dominantxNNxlong == 0          434.19     116.22
## dominantxNNxshort - dominantxNNxlong == 0           91.82     116.61
## subordinatexNNxshort - dominantxNNxlong == 0       218.53     116.60
## unrelatedxNNxshort - dominantxNNxlong == 0         387.86     116.22
## dominantxNVxshort - dominantxNNxlong == 0          -90.91     116.22
## subordinatexNVxshort - dominantxNNxlong == 0       334.49     116.99
## unrelatedxNVxshort - dominantxNNxlong == 0         481.85     116.61
## unrelatedxNNxlong - subordinatexNNxlong == 0       121.81     116.22
## dominantxNVxlong - subordinatexNNxlong == 0        -74.98     116.22
## subordinatexNVxlong - subordinatexNNxlong == 0     131.91     116.22
## unrelatedxNVxlong - subordinatexNNxlong == 0        95.94     116.22
## dominantxNNxshort - subordinatexNNxlong == 0      -246.42     116.61
## subordinatexNNxshort - subordinatexNNxlong == 0   -119.72     116.60
## unrelatedxNNxshort - subordinatexNNxlong == 0       49.61     116.22
## dominantxNVxshort - subordinatexNNxlong == 0      -429.16     116.22
## subordinatexNVxshort - subordinatexNNxlong == 0     -3.76     116.99
## unrelatedxNVxshort - subordinatexNNxlong == 0      143.60     116.61
## dominantxNVxlong - unrelatedxNNxlong == 0         -196.79     116.22
## subordinatexNVxlong - unrelatedxNNxlong == 0        10.10     116.22
## unrelatedxNVxlong - unrelatedxNNxlong == 0         -25.87     116.22
## dominantxNNxshort - unrelatedxNNxlong == 0        -368.23     116.61
## subordinatexNNxshort - unrelatedxNNxlong == 0     -241.53     116.60
## unrelatedxNNxshort - unrelatedxNNxlong == 0        -72.20     116.22
## dominantxNVxshort - unrelatedxNNxlong == 0        -550.97     116.22
## subordinatexNVxshort - unrelatedxNNxlong == 0     -125.57     116.99
## unrelatedxNVxshort - unrelatedxNNxlong == 0         21.79     116.61
## subordinatexNVxlong - dominantxNVxlong == 0        206.89     116.22
## unrelatedxNVxlong - dominantxNVxlong == 0          170.93     116.22
## dominantxNNxshort - dominantxNVxlong == 0         -171.44     116.61
## subordinatexNNxshort - dominantxNVxlong == 0       -44.74     116.60
## unrelatedxNNxshort - dominantxNVxlong == 0         124.59     116.22
## dominantxNVxshort - dominantxNVxlong == 0         -354.17     116.22
## subordinatexNVxshort - dominantxNVxlong == 0        71.22     116.99
## unrelatedxNVxshort - dominantxNVxlong == 0         218.58     116.61
## unrelatedxNVxlong - subordinatexNVxlong == 0       -35.97     116.22
## dominantxNNxshort - subordinatexNVxlong == 0      -378.33     116.61
## subordinatexNNxshort - subordinatexNVxlong == 0   -251.63     116.60
## unrelatedxNNxshort - subordinatexNVxlong == 0      -82.30     116.22
## dominantxNVxshort - subordinatexNVxlong == 0      -561.07     116.22
## subordinatexNVxshort - subordinatexNVxlong == 0   -135.67     116.99
## unrelatedxNVxshort - subordinatexNVxlong == 0       11.69     116.61
## dominantxNNxshort - unrelatedxNVxlong == 0        -342.37     116.61
## subordinatexNNxshort - unrelatedxNVxlong == 0     -215.67     116.60
## unrelatedxNNxshort - unrelatedxNVxlong == 0        -46.33     116.22
## dominantxNVxshort - unrelatedxNVxlong == 0        -525.10     116.22
## subordinatexNVxshort - unrelatedxNVxlong == 0      -99.70     116.99
## unrelatedxNVxshort - unrelatedxNVxlong == 0         47.65     116.61
## subordinatexNNxshort - dominantxNNxshort == 0      126.70     116.99
## unrelatedxNNxshort - dominantxNNxshort == 0        296.03     116.61
## dominantxNVxshort - dominantxNNxshort == 0        -182.73     116.61
## subordinatexNVxshort - dominantxNNxshort == 0      242.66     117.30
## unrelatedxNVxshort - dominantxNNxshort == 0        390.02     116.92
## unrelatedxNNxshort - subordinatexNNxshort == 0     169.33     116.60
## dominantxNVxshort - subordinatexNNxshort == 0     -309.43     116.60
## subordinatexNVxshort - subordinatexNNxshort == 0   115.96     117.37
## unrelatedxNVxshort - subordinatexNNxshort == 0     263.32     116.99
## dominantxNVxshort - unrelatedxNNxshort == 0       -478.77     116.22
## subordinatexNVxshort - unrelatedxNNxshort == 0     -53.37     116.99
## unrelatedxNVxshort - unrelatedxNNxshort == 0        93.99     116.61
## subordinatexNVxshort - dominantxNVxshort == 0      425.40     116.99
## unrelatedxNVxshort - dominantxNVxshort == 0        572.75     116.61
## unrelatedxNVxshort - subordinatexNVxshort == 0     147.36     117.30
##                                                  z value Pr(>|z|)    
## subordinatexNNxlong - dominantxNNxlong == 0         2.91    0.136    
## unrelatedxNNxlong - dominantxNNxlong == 0           3.96    <0.01 ** 
## dominantxNVxlong - dominantxNNxlong == 0            2.27    0.502    
## subordinatexNVxlong - dominantxNNxlong == 0         4.05    <0.01 ** 
## unrelatedxNVxlong - dominantxNNxlong == 0           3.74    <0.01 ** 
## dominantxNNxshort - dominantxNNxlong == 0           0.79    1.000    
## subordinatexNNxshort - dominantxNNxlong == 0        1.87    0.775    
## unrelatedxNNxshort - dominantxNNxlong == 0          3.34    0.041 *  
## dominantxNVxshort - dominantxNNxlong == 0          -0.78    1.000    
## subordinatexNVxshort - dominantxNNxlong == 0        2.86    0.156    
## unrelatedxNVxshort - dominantxNNxlong == 0          4.13    <0.01 ** 
## unrelatedxNNxlong - subordinatexNNxlong == 0        1.05    0.997    
## dominantxNVxlong - subordinatexNNxlong == 0        -0.65    1.000    
## subordinatexNVxlong - subordinatexNNxlong == 0      1.14    0.993    
## unrelatedxNVxlong - subordinatexNNxlong == 0        0.83    1.000    
## dominantxNNxshort - subordinatexNNxlong == 0       -2.11    0.612    
## subordinatexNNxshort - subordinatexNNxlong == 0    -1.03    0.997    
## unrelatedxNNxshort - subordinatexNNxlong == 0       0.43    1.000    
## dominantxNVxshort - subordinatexNNxlong == 0       -3.69    0.012 *  
## subordinatexNVxshort - subordinatexNNxlong == 0    -0.03    1.000    
## unrelatedxNVxshort - subordinatexNNxlong == 0       1.23    0.987    
## dominantxNVxlong - unrelatedxNNxlong == 0          -1.69    0.872    
## subordinatexNVxlong - unrelatedxNNxlong == 0        0.09    1.000    
## unrelatedxNVxlong - unrelatedxNNxlong == 0         -0.22    1.000    
## dominantxNNxshort - unrelatedxNNxlong == 0         -3.16    0.070 .  
## subordinatexNNxshort - unrelatedxNNxlong == 0      -2.07    0.643    
## unrelatedxNNxshort - unrelatedxNNxlong == 0        -0.62    1.000    
## dominantxNVxshort - unrelatedxNNxlong == 0         -4.74    <0.01 ***
## subordinatexNVxshort - unrelatedxNNxlong == 0      -1.07    0.996    
## unrelatedxNVxshort - unrelatedxNNxlong == 0         0.19    1.000    
## subordinatexNVxlong - dominantxNVxlong == 0         1.78    0.829    
## unrelatedxNVxlong - dominantxNVxlong == 0           1.47    0.948    
## dominantxNNxshort - dominantxNVxlong == 0          -1.47    0.949    
## subordinatexNNxshort - dominantxNVxlong == 0       -0.38    1.000    
## unrelatedxNNxshort - dominantxNVxlong == 0          1.07    0.996    
## dominantxNVxshort - dominantxNVxlong == 0          -3.05    0.096 .  
## subordinatexNVxshort - dominantxNVxlong == 0        0.61    1.000    
## unrelatedxNVxshort - dominantxNVxlong == 0          1.87    0.776    
## unrelatedxNVxlong - subordinatexNVxlong == 0       -0.31    1.000    
## dominantxNNxshort - subordinatexNVxlong == 0       -3.24    0.054 .  
## subordinatexNNxshort - subordinatexNVxlong == 0    -2.16    0.580    
## unrelatedxNNxshort - subordinatexNVxlong == 0      -0.71    1.000    
## dominantxNVxshort - subordinatexNVxlong == 0       -4.83    <0.01 ***
## subordinatexNVxshort - subordinatexNVxlong == 0    -1.16    0.992    
## unrelatedxNVxshort - subordinatexNVxlong == 0       0.10    1.000    
## dominantxNNxshort - unrelatedxNVxlong == 0         -2.94    0.128    
## subordinatexNNxshort - unrelatedxNVxlong == 0      -1.85    0.790    
## unrelatedxNNxshort - unrelatedxNVxlong == 0        -0.40    1.000    
## dominantxNVxshort - unrelatedxNVxlong == 0         -4.52    <0.01 ***
## subordinatexNVxshort - unrelatedxNVxlong == 0      -0.85    0.999    
## unrelatedxNVxshort - unrelatedxNVxlong == 0         0.41    1.000    
## subordinatexNNxshort - dominantxNNxshort == 0       1.08    0.995    
## unrelatedxNNxshort - dominantxNNxshort == 0         2.54    0.315    
## dominantxNVxshort - dominantxNNxshort == 0         -1.57    0.921    
## subordinatexNVxshort - dominantxNNxshort == 0       2.07    0.646    
## unrelatedxNVxshort - dominantxNNxshort == 0         3.34    0.041 *  
## unrelatedxNNxshort - subordinatexNNxshort == 0      1.45    0.953    
## dominantxNVxshort - subordinatexNNxshort == 0      -2.65    0.250    
## subordinatexNVxshort - subordinatexNNxshort == 0    0.99    0.998    
## unrelatedxNVxshort - subordinatexNNxshort == 0      2.25    0.511    
## dominantxNVxshort - unrelatedxNNxshort == 0        -4.12    <0.01 ** 
## subordinatexNVxshort - unrelatedxNNxshort == 0     -0.46    1.000    
## unrelatedxNVxshort - unrelatedxNNxshort == 0        0.81    1.000    
## subordinatexNVxshort - dominantxNVxshort == 0       3.64    0.015 *  
## unrelatedxNVxshort - dominantxNVxshort == 0         4.91    <0.01 ***
## unrelatedxNVxshort - subordinatexNVxshort == 0      1.26    0.984    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
```


Or, more compactly:

<img src="https://raw.githubusercontent.com/faulconbridge/semsat/master/Semsat2/figure/Tukey.png" />
