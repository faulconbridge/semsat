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

download.file("https://github.com/faulconbridge/semsat/raw/master/Semsat2/semsat2.csv", 
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


Now we can run two 3x2 ANOVAs on the NN/NV data separately:

```r
NN <- subset(raw.mean.stdev, HomType == "NN", select = c(PID:STDEV))
nn.aov <- aov(STDEV ~ BIAS * REPS + Error(PID/(BIAS * REPS)), data = NN)
summary(nn.aov)
```

```
## 
## Error: PID
##           Df   Sum Sq  Mean Sq F value Pr(>F)
## Residuals 80 8.52e+08 10655556               
## 
## Error: PID:BIAS
##            Df   Sum Sq Mean Sq F value  Pr(>F)    
## BIAS        2 12323290 6161645      13 5.8e-06 ***
## Residuals 160 75722977  473269                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:REPS
##           Df   Sum Sq Mean Sq F value Pr(>F)
## REPS       1    51498   51498    0.07   0.79
## Residuals 80 59910336  748879               
## 
## Error: PID:BIAS:REPS
##            Df   Sum Sq Mean Sq F value Pr(>F)
## BIAS:REPS   2   922466  461233    1.14   0.32
## Residuals 160 64990966  406194
```

```r

NV <- subset(raw.mean.stdev, HomType == "NV", select = c(PID:STDEV))
nv.aov <- aov(STDEV ~ BIAS * REPS + Error(PID/(BIAS * REPS)), data = NV)
summary(nv.aov)
```

```
## 
## Error: PID
##           Df   Sum Sq  Mean Sq F value Pr(>F)
## Residuals 80 8.21e+08 10262790               
## 
## Error: PID:BIAS
##            Df   Sum Sq Mean Sq F value  Pr(>F)    
## BIAS        2 13848640 6924320    13.7 3.3e-06 ***
## Residuals 160 81015920  506350                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:REPS
##           Df   Sum Sq Mean Sq F value Pr(>F)  
## REPS       1  2166308 2166308    5.55  0.021 *
## Residuals 80 31247666  390596                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: PID:BIAS:REPS
##            Df   Sum Sq Mean Sq F value Pr(>F)
## BIAS:REPS   2  1337792  668896    1.56   0.21
## Residuals 160 68729372  429559
```


And some t-tests to look at the ambiguity effect:

```r
dom <- subset(raw.mean.stdev, BIAS == "dominant", select = c(STDEV))
sub <- subset(raw.mean.stdev, BIAS == "subordinate", select = c(STDEV))
unrel <- subset(raw.mean.stdev, BIAS == "unrelated", select = c(STDEV))

NVsub3 <- subset(raw.mean.stdev, BIAS == "subordinate" & HomType == "NV" & REPS == 
    "short", select = c(STDEV))
NVsub30 <- subset(raw.mean.stdev, BIAS == "subordinate" & HomType == "NV" & 
    REPS == "long", select = c(STDEV))
NVunrel3 <- subset(raw.mean.stdev, BIAS == "unrelated" & HomType == "NV" & REPS == 
    "short", select = c(STDEV))
NVunrel30 <- subset(raw.mean.stdev, BIAS == "unrelated" & HomType == "NV" & 
    REPS == "long", select = c(STDEV))

t.test(dom$STDEV, sub$STDEV)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  dom$STDEV and sub$STDEV
## t = -2.388, df = 628.9, p-value = 0.01726
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -502.42  -48.93
## sample estimates:
## mean of x mean of y 
##      2043      2319
```

```r
t.test(NVsub3$STDEV, NVunrel3$STDEV)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  NVsub3$STDEV and NVunrel3$STDEV
## t = -0.3524, df = 158, p-value = 0.725
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -550.5  383.8
## sample estimates:
## mean of x mean of y 
##      2317      2400
```

```r
t.test(NVsub30, NVunrel30)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  NVsub30 and NVunrel30
## t = 0.137, df = 151.1, p-value = 0.8912
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -420.1  482.7
## sample estimates:
## mean of x mean of y 
##      2441      2410
```

