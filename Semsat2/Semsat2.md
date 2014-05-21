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
```


Performing the Analyses
-----------------------
First we can look at the untrimmed data:

```r
raw.aov <- aov(MS ~ BIAS * HomType * REPS + Error(PID/(BIAS * HomType * REPS)), 
    data = raw.mean.MS)
```

```
## 
## Error: PID
##           Df   Sum Sq  Mean Sq F value Pr(>F)
## Residuals 80 2.01e+09 25108743               
## 
## Error: PID:BIAS
##            Df   Sum Sq  Mean Sq F value Pr(>F)    
## BIAS        2 3.87e+07 19328179    22.3  3e-09 ***
## Residuals 160 1.39e+08   868428                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:HomType
##           Df   Sum Sq Mean Sq F value Pr(>F)  
## HomType    1  2832038 2832038    4.88   0.03 *
## Residuals 80 46386699  579834                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:REPS
##           Df   Sum Sq Mean Sq F value Pr(>F)
## REPS       1 2.07e+05  206746    0.13   0.72
## Residuals 80 1.31e+08 1633679               
## 
## Error: PID:BIAS:HomType
##               Df   Sum Sq Mean Sq F value Pr(>F)   
## BIAS:HomType   2 9.97e+06 4986733    6.31 0.0023 **
## Residuals    160 1.26e+08  790145                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:BIAS:REPS
##            Df   Sum Sq Mean Sq F value Pr(>F)
## BIAS:REPS   2 1.75e+05   87741    0.07   0.93
## Residuals 160 1.92e+08 1199874               
## 
## Error: PID:HomType:REPS
##              Df   Sum Sq Mean Sq F value Pr(>F)
## HomType:REPS  1   512203  512203    0.76   0.39
## Residuals    80 54098299  676229               
## 
## Error: PID:BIAS:HomType:REPS
##                    Df   Sum Sq Mean Sq F value Pr(>F)
## BIAS:HomType:REPS   2 3.66e+06 1828797    1.71   0.18
## Residuals         160 1.71e+08 1066993
```


Then trimming based on standard deviations:

```r
stdev.aov <- aov(STDEV ~ BIAS * HomType * REPS + Error(PID/(BIAS * HomType * 
    REPS)), data = raw.mean.stdev)
```

```
## 
## Error: PID
##           Df   Sum Sq  Mean Sq F value Pr(>F)
## Residuals 80 1.64e+09 20520087               
## 
## Error: PID:BIAS
##            Df   Sum Sq  Mean Sq F value Pr(>F)    
## BIAS        2 24988314 12494157    22.8  2e-09 ***
## Residuals 160 87854303   549089                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:HomType
##           Df   Sum Sq Mean Sq F value Pr(>F)
## HomType    1   116433  116433    0.29   0.59
## Residuals 80 31860688  398259               
## 
## Error: PID:REPS
##           Df   Sum Sq Mean Sq F value Pr(>F)
## REPS       1  1442910 1442910    2.29   0.13
## Residuals 80 50515361  631442               
## 
## Error: PID:BIAS:HomType
##               Df   Sum Sq Mean Sq F value Pr(>F)
## BIAS:HomType   2  1183615  591808    1.37   0.26
## Residuals    160 68884595  430529               
## 
## Error: PID:BIAS:REPS
##            Df   Sum Sq Mean Sq F value Pr(>F)
## BIAS:REPS   2   150078   75039     0.2   0.82
## Residuals 160 61366067  383538               
## 
## Error: PID:HomType:REPS
##              Df   Sum Sq Mean Sq F value Pr(>F)
## HomType:REPS  1   774896  774896    1.53   0.22
## Residuals    80 40642642  508033               
## 
## Error: PID:BIAS:HomType:REPS
##                    Df   Sum Sq Mean Sq F value Pr(>F)
## BIAS:HomType:REPS   2  2110180 1055090    2.33    0.1
## Residuals         160 72354271  452214
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

