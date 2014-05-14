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
View(raw.stdev)
```


We can now fit a linear mixed-effects model to our data:



```r
time.linear <- lme(STDEV ~ BIAS * REPS * Elapsed, random = list(PID = pdDiag(~Elapsed)), 
    na.action = na.omit, data = raw.stdev)
```

This model gives:

```
## Linear mixed-effects model fit by REML
##  Data: raw.stdev 
##     AIC   BIC logLik
##   38599 38686 -19284
## 
## Random effects:
##  Formula: ~Elapsed | PID
##  Structure: Diagonal
##         (Intercept) Elapsed Residual
## StdDev:       357.5   29.02    745.1
## 
## Fixed effects: STDEV ~ BIAS * REPS * Elapsed 
##                                 Value Std.Error   DF t-value p-value
## (Intercept)                    1153.8    103.89 2329  11.106  0.0000
## BIASsubordinate                 142.3    137.22 2329   1.037  0.2999
## BIASunrelated                   530.4    129.18 2329   4.106  0.0000
## REPS30                          -67.0    132.29 2329  -0.507  0.6125
## Elapsed                          19.5     23.86 2329   0.818  0.4134
## BIASsubordinate:REPS30           28.5    200.31 2329   0.142  0.8869
## BIASunrelated:REPS30             -8.7    190.45 2329  -0.046  0.9634
## BIASsubordinate:Elapsed          11.7     34.27 2329   0.342  0.7320
## BIASunrelated:Elapsed            23.6     32.95 2329   0.716  0.4740
## REPS30:Elapsed                   23.0     33.03 2329   0.697  0.4862
## BIASsubordinate:REPS30:Elapsed   -6.9     49.68 2329  -0.139  0.8892
## BIASunrelated:REPS30:Elapsed     -3.5     47.96 2329  -0.073  0.9417
##  Correlation: 
##                                (Intr) BIASsb BIASnr REPS30 Elapsd
## BIASsubordinate                -0.586                            
## BIASunrelated                  -0.622  0.470                     
## REPS30                         -0.606  0.463  0.491              
## Elapsed                        -0.814  0.595  0.630  0.614       
## BIASsubordinate:REPS30          0.404 -0.685 -0.322 -0.661 -0.411
## BIASunrelated:REPS30            0.423 -0.320 -0.681 -0.696 -0.428
## BIASsubordinate:Elapsed         0.547 -0.922 -0.438 -0.431 -0.654
## BIASunrelated:Elapsed           0.568 -0.428 -0.916 -0.447 -0.678
## REPS30:Elapsed                  0.565 -0.432 -0.457 -0.924 -0.675
## BIASsubordinate:REPS30:Elapsed -0.380  0.636  0.303  0.615  0.454
## BIASunrelated:REPS30:Elapsed   -0.391  0.296  0.633  0.638  0.466
##                                BIASs:REPS30 BIASn:REPS30 BIASs:E BIASn:E
## BIASsubordinate                                                         
## BIASunrelated                                                           
## REPS30                                                                  
## Elapsed                                                                 
## BIASsubordinate:REPS30                                                  
## BIASunrelated:REPS30            0.458                                   
## BIASsubordinate:Elapsed         0.631        0.298                      
## BIASunrelated:Elapsed           0.293        0.624        0.470         
## REPS30:Elapsed                  0.611        0.644        0.473   0.491 
## BIASsubordinate:REPS30:Elapsed -0.927       -0.426       -0.689  -0.325 
## BIASunrelated:REPS30:Elapsed   -0.419       -0.923       -0.325  -0.691 
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
## BIASsubordinate:REPS30:Elapsed -0.665                
## BIASunrelated:REPS30:Elapsed   -0.691   0.457        
## 
## Standardized Within-Group Residuals:
##     Min      Q1     Med      Q3     Max 
## -2.7211 -0.5583 -0.1904  0.2794  7.7168 
## 
## Number of Observations: 2399
## Number of Groups: 59
```

<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Wed May 14 13:26:47 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> numDF </TH> <TH> denDF </TH> <TH> F-value </TH> <TH> p-value </TH>  </TR>
  <TR> <TD align="right"> (Intercept) </TD> <TD align="right">   1 </TD> <TD align="right"> 2329.00 </TD> <TD align="right"> 845.45 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> BIAS </TD> <TD align="right">   2 </TD> <TD align="right"> 2329.00 </TD> <TD align="right"> 140.16 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> REPS </TD> <TD align="right">   1 </TD> <TD align="right"> 2329.00 </TD> <TD align="right"> 0.29 </TD> <TD align="right"> 0.59 </TD> </TR>
  <TR> <TD align="right"> Elapsed </TD> <TD align="right">   1 </TD> <TD align="right"> 2329.00 </TD> <TD align="right"> 11.66 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> BIAS:REPS </TD> <TD align="right">   2 </TD> <TD align="right"> 2329.00 </TD> <TD align="right"> 0.06 </TD> <TD align="right"> 0.94 </TD> </TR>
  <TR> <TD align="right"> BIAS:Elapsed </TD> <TD align="right">   2 </TD> <TD align="right"> 2329.00 </TD> <TD align="right"> 0.41 </TD> <TD align="right"> 0.67 </TD> </TR>
  <TR> <TD align="right"> REPS:Elapsed </TD> <TD align="right">   1 </TD> <TD align="right"> 2329.00 </TD> <TD align="right"> 0.97 </TD> <TD align="right"> 0.32 </TD> </TR>
  <TR> <TD align="right"> BIAS:REPS:Elapsed </TD> <TD align="right">   2 </TD> <TD align="right"> 2329.00 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> 0.99 </TD> </TR>
   </TABLE>


Finally, we can visualize these two main effects (i.e., those of meaning bias and of time elapsed between study and test phases). Unrelated items are illustrated in blue; subordinate in red; and dominant in green.

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

