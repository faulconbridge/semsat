Semantic Satiation among Lexically Ambiguous Words: Experiment 2
================================================================

Acquiring the Data
------------------

We will first download the raw CSV from github and do a bit of preprocessing on it:


```r
library(nlme)
library(multcomp)
```

```
## Loading required package: mvtnorm
## Loading required package: survival
## Loading required package: splines
## Loading required package: TH.data
```

```r

download.file("https://github.com/faulconbridge/semsat/raw/master/Semsat2/Semsat2_compiled.csv", 
    "semsat2.csv", "wget", extra = "--no-check-certificate")
semsat2 <- read.csv("semsat2.csv", header = TRUE, sep = ",")

raw <- semsat2

# Strip out all filler and incorrectly-answered items
raw <- subset(raw, (BIAS == "dominant" & ANSWER == "M") | (BIAS == "subordinate" & 
    ANSWER == "M") | (BIAS == "unrelated" & (HomType == "NV" | HomType == "NN") & 
    ANSWER == "C"), select = c(PID, BIAS, HomType, REPS, ANSWER, MS, MS.3.STDEV, 
    MS.IQR))

# Specify which variables should be treated as factors
raw <- within(raw, {
    BIAS <- factor(BIAS)
    HomType <- factor(HomType)
    REPS <- factor(REPS)
    PID <- factor(PID)
    STDEV <- MS.3.STDEV
    IQR <- MS.IQR
})

# Strip out participants who encountered bugs during the experiment
raw <- subset(raw, PID != 57 & PID != 42 & PID != 14, select = c(PID, BIAS, 
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
## 
## Error: PID
##           Df   Sum Sq  Mean Sq F value Pr(>F)
## Residuals 80 2.01e+09 25108743               
## 
## Error: Within
##                    Df   Sum Sq  Mean Sq F value  Pr(>F)    
## BIAS                2 3.87e+07 19328179   19.79 3.9e-09 ***
## HomType             1 2.83e+06  2832038    2.90  0.0889 .  
## REPS                1 2.07e+05   206746    0.21  0.6455    
## BIAS:HomType        2 9.97e+06  4986733    5.11  0.0062 ** 
## BIAS:REPS           2 1.75e+05    87741    0.09  0.9141    
## HomType:REPS        1 5.12e+05   512203    0.52  0.4691    
## BIAS:HomType:REPS   2 3.66e+06  1828797    1.87  0.1543    
## Residuals         880 8.59e+08   976420                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Then trimming based on standard deviations:

```r
stdev.aov <- aov(STDEV ~ BIAS * HomType * REPS + Error(PID), data = raw.mean.stdev)
```

```
## 
## Error: PID
##           Df   Sum Sq  Mean Sq F value Pr(>F)
## Residuals 80 1.64e+09 20520087               
## 
## Error: Within
##                    Df   Sum Sq  Mean Sq F value  Pr(>F)    
## BIAS                2 2.50e+07 12494157   26.59 6.1e-12 ***
## HomType             1 1.16e+05   116433    0.25    0.62    
## REPS                1 1.44e+06  1442910    3.07    0.08 .  
## BIAS:HomType        2 1.18e+06   591808    1.26    0.28    
## BIAS:REPS           2 1.50e+05    75039    0.16    0.85    
## HomType:REPS        1 7.75e+05   774896    1.65    0.20    
## BIAS:HomType:REPS   2 2.11e+06  1055090    2.25    0.11    
## Residuals         880 4.13e+08   469861                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


And finally based on interquatrile range:

```r
iqr.aov <- aov(IQR ~ BIAS * HomType * REPS + Error(PID), data = raw.mean.IQR)
```

```
## 
## Error: PID
##           Df   Sum Sq  Mean Sq F value Pr(>F)
## Residuals 80 8.49e+08 10610635               
## 
## Error: Within
##                    Df   Sum Sq Mean Sq F value  Pr(>F)    
## BIAS                2 1.96e+07 9790762   43.32 < 2e-16 ***
## HomType             1 7.77e+05  777430    3.44 0.06396 .  
## REPS                1 2.85e+06 2847595   12.60 0.00041 ***
## BIAS:HomType        2 1.67e+06  833181    3.69 0.02544 *  
## BIAS:REPS           2 1.13e+05   56654    0.25 0.77831    
## HomType:REPS        1 2.72e+05  272331    1.21 0.27261    
## BIAS:HomType:REPS   2 3.03e+04   15167    0.07 0.93509    
## Residuals         880 1.99e+08  225985                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Now we can perform a follow-up test looking at our three levels of bias:

```r
stdev.lme <- lme(STDEV ~ BIAS * HomType * REPS, random = ~1 | PID, data = raw.mean.stdev)
l2 <- glht(stdev.lme, linfct = mcp(BIAS = "Tukey"))
```

```
## Warning: covariate interactions found -- default contrast might be
## inappropriate
```

```r
summary(l2)
```

```
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: lme.formula(fixed = STDEV ~ BIAS * HomType * REPS, data = raw.mean.stdev, 
##     random = ~1 | PID)
## 
## Linear Hypotheses:
##                              Estimate Std. Error z value Pr(>|z|)    
## subordinate - dominant == 0       301        108    2.79    0.014 *  
## unrelated - dominant == 0         481        108    4.46   <0.001 ***
## unrelated - subordinate == 0      180        108    1.67    0.217    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
```

