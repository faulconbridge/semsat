Semantic Satiation among Lexically Ambiguous Words: Experiment 1b
=================================================================

Acquiring the Data
------------------

We will first download the raw CSV from github and do a bit of preprocessing on it:


```r
download.file("https://github.com/faulconbridge/semsat/raw/master/Semsat1b/Semsat1b_compiled.csv", 
    "semsat1b.csv", "wget", extra = "--no-check-certificate")
semsat1b <- read.csv("semsat1b.csv", header = TRUE, sep = ",")

# Subset the data to remove all filler and incorrectly-answered items
raw <- subset(semsat1b, ((BIAS == "dominant" | BIAS == "subordinate") & ANSWER == 
    "M") | (BIAS == "unrelated" & ANSWER == "C"), select = c(PID, BIAS, REPS, 
    Elapsed, MS, STDEV, IQR))

# Remove participants who encountered errors
raw <- subset(raw, PID != 7 & PID != 20, select = c(PID:IQR))

# Identify which variables should be treated as factors
raw <- within(raw, {
    BIAS <- factor(BIAS)
    REPS <- factor(REPS)
    PID <- factor(PID)
})

# Subset the data to include all MS response times
raw.MS <- subset(raw, select = c(PID:MS))
raw.MS <- na.omit(raw.MS)

# Subset the data to include all MS response times with outliers (more than
# 3sd deviation) removed
raw.stdev <- subset(raw, select = c(PID:Elapsed, STDEV))
raw.stdev <- na.omit(raw.stdev)
```


We can now fit a linear mixed-effects model to our data:

```
## Loading required package: mvtnorm
## Loading required package: survival
## Loading required package: splines
## Loading required package: TH.data
```


```r
raw.stdev$REPS <- factor(raw.stdev$REPS)

time.linear <- lme(STDEV ~ BIAS * REPS * Elapsed, random = list(PID = pdDiag(~Elapsed)), 
    na.action = na.omit, data = raw.stdev)
```

This model gives:

```
## Linear mixed-effects model fit by REML
##  Data: raw.stdev 
##     AIC   BIC logLik
##   37435 37521 -18703
## 
## Random effects:
##  Formula: ~Elapsed | PID
##  Structure: Diagonal
##         (Intercept) Elapsed Residual
## StdDev:       341.6   30.89    742.6
## 
## Fixed effects: STDEV ~ BIAS * REPS * Elapsed 
##                                 Value Std.Error   DF t-value p-value
## (Intercept)                    1167.6    104.27 2260  11.198  0.0000
## BIASsubordinate                 117.9    138.24 2260   0.853  0.3939
## BIASunrelated                   561.9    130.73 2260   4.298  0.0000
## REPS30                          -81.6    133.78 2260  -0.610  0.5422
## Elapsed                          19.1     24.34 2260   0.786  0.4322
## BIASsubordinate:REPS30           75.8    202.13 2260   0.375  0.7079
## BIASunrelated:REPS30            -21.7    192.35 2260  -0.113  0.9103
## BIASsubordinate:Elapsed          15.6     34.69 2260   0.450  0.6524
## BIASunrelated:Elapsed            15.5     33.44 2260   0.464  0.6429
## REPS30:Elapsed                   26.1     33.56 2260   0.778  0.4365
## BIASsubordinate:REPS30:Elapsed  -14.9     50.27 2260  -0.296  0.7672
## BIASunrelated:REPS30:Elapsed      3.4     48.56 2260   0.070  0.9442
##  Correlation: 
##                                (Intr) BIASsb BIASnr REPS30 Elapsd
## BIASsubordinate                -0.594                            
## BIASunrelated                  -0.626  0.472                     
## REPS30                         -0.611  0.465  0.490              
## Elapsed                        -0.820  0.596  0.629  0.613       
## BIASsubordinate:REPS30          0.409 -0.684 -0.323 -0.662 -0.412
## BIASunrelated:REPS30            0.426 -0.322 -0.682 -0.697 -0.427
## BIASsubordinate:Elapsed         0.555 -0.922 -0.441 -0.434 -0.657
## BIASunrelated:Elapsed           0.574 -0.432 -0.916 -0.449 -0.679
## REPS30:Elapsed                  0.571 -0.435 -0.458 -0.924 -0.676
## BIASsubordinate:REPS30:Elapsed -0.386  0.636  0.305  0.617  0.457
## BIASunrelated:REPS30:Elapsed   -0.395  0.299  0.634  0.640  0.468
##                                BIASs:REPS30 BIASn:REPS30 BIASs:E BIASn:E
## BIASsubordinate                                                         
## BIASunrelated                                                           
## REPS30                                                                  
## Elapsed                                                                 
## BIASsubordinate:REPS30                                                  
## BIASunrelated:REPS30            0.460                                   
## BIASsubordinate:Elapsed         0.630        0.301                      
## BIASunrelated:Elapsed           0.296        0.625        0.475         
## REPS30:Elapsed                  0.612        0.644        0.478   0.495 
## BIASsubordinate:REPS30:Elapsed -0.927       -0.429       -0.689  -0.328 
## BIASunrelated:REPS30:Elapsed   -0.422       -0.922       -0.329  -0.692 
##                                REPS30: BIASs:REPS30:E
## BIASsubordinate                                      
## BIASunrelated                                        
## REPS30                                               
## Elapsed                                              
## BIASsubordinate:REPS30                               
## BIASunrelated:REPS30                                 
## BIASsubordinate:Elapsed                              
## BIASunrelated:Elapsed                                
## REPS30:Elapsed                                       
## BIASsubordinate:REPS30:Elapsed -0.668                
## BIASunrelated:REPS30:Elapsed   -0.693   0.461        
## 
## Standardized Within-Group Residuals:
##     Min      Q1     Med      Q3     Max 
## -2.7473 -0.5550 -0.1858  0.2871  7.7498 
## 
## Number of Observations: 2328
## Number of Groups: 57
```


```
##                   numDF denDF F-value p-value
## (Intercept)           1  2260   889.2  <.0001
## BIAS                  2  2260   140.8  <.0001
## REPS                  1  2260     0.5  0.4718
## Elapsed               1  2260    11.0  0.0009
## BIAS:REPS             2  2260     0.1  0.9172
## BIAS:Elapsed          2  2260     0.2  0.7955
## REPS:Elapsed          1  2260     1.3  0.2598
## BIAS:REPS:Elapsed     2  2260     0.1  0.9316
```


We can perform post-hoc tests on item bias:

```r
l2 <- glht(time.linear, linfct = mcp(BIAS = "Tukey"))
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
## Fit: lme.formula(fixed = STDEV ~ BIAS * REPS * Elapsed, data = raw.stdev, 
##     random = list(PID = pdDiag(~Elapsed)), na.action = na.omit)
## 
## Linear Hypotheses:
##                              Estimate Std. Error z value Pr(>|z|)    
## subordinate - dominant == 0       118        138    0.85    0.670    
## unrelated - dominant == 0         562        131    4.30   <0.001 ***
## unrelated - subordinate == 0      444        138    3.21    0.004 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
```


Finally, we can visualize these two main effects (i.e., those of meaning bias and of time elapsed between study and test phases). Unrelated items are illustrated in blue; subordinate in red; and dominant in green.

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

