########################################
## Meta-Analysis of Semantic Satiation
########################################

## load required libraries
library(metagen)
library(meta)

########################################
##            SOME NOTES              ##
########################################
## Cohene (1978): SD was estimated from
##   given SE * sqrt(n). Exp 2 not used
##   as utilized identical word, not
##   associate paradigm
## 
## Smith (1984): SD is for mean median
##   (need to confirm) in Exp. 1/2;
##   no SD given for Exp. 3
## 
## Smith & Klein (1990): No SD given for
##   Exp. 1; Exp. 2 unused as funky
##   paradigm was employed
## 
## Balota & Black (1997): No SD given
##   for Exp. 1/2. Exp. 3 unused as
##   employed rhyming words
## 
## Black (2001): No issues; all
##   experiments used
## 
## Tian & Huber (2010): Exp. 1a/b and 2
##   used; Exp. 3 unused given identical
##   target, not associate. SD estimated
##   from given SE * sqrt(n) for all Exp
## 
## Black, et al. (2013): Exp. 1a/b used
##   where 1a = concordant LF and
##   1b = concordand HF words
########################################

## Experimental items are the satiated related items
## Control items are the primed related items
data <- data.frame(Author=c("Balota & Black", "Balota & Black",
                            "Black", "Black", "Black et al.",
                            "Balota et al.", "Cohene, Smith, & Klein",
                            "Cohene, Smith, & Klein", "Smith",
                            "Smith", "Smith", "Smith & Klein",
                            "Tian & Huber", "Tian & Huber",
                            "Tian & Huber"), 
                 Year=c(1997, 1997, 2001, 2001, 2013, 2013, 
                        1978, 1978, 1984, 1984, 1984, 1990, 
                        2010, 2010, 2010), 
                 Study=c("1", "2", "1", "2", "1a", "1b", "1", 
                         "3", "1", "2", "3", "1", "1a", "1b", "2"), 
                 ExpN = c(36, 48, 72, 60, 144, 144, 10, 20, 16,
                          32, 32, 45, 43, 41, 40),
                 ExpMean = c(913, 801, 1300, 1296, 924, 851,
                             473, 551, 670, 708, 627, 910, 620,
                             604, 585),
                 ExpSD = c( NA, NA, 383, 248, 230, 189, 85, 76,
                            3, 5, NA, NA, 111, 77, 89),
                 CtrlN = c( 36, 48, 72, 60, 144, 144, 10, 20, 16,
                            32, 32, 45, 43, 41, 40),
                 CtrlMean = c(906, 796, 1296, 1208, 861, 805, 452,
                              554, 639, 630, 622, 865, 595, 579, 555),
                 CtrlSD = c( NA, NA, 385, 196, 213, 174, 57, 72, 2, 5,
                             NA, NA, 92, 77, 76))

## Order studies by year, experiment
data <- data[order(data$Year, data$Study), ]

## Create an object of class meta
## Specify random-effects model
## Use mean difference, maximum likelihood estimation
fit <- metacont(ExpN, ExpMean, ExpSD, CtrlN, CtrlMean, CtrlSD,
                studlab = paste(Author, Year, Study),
                data = na.omit(data), comb.random = TRUE,
                comb.fixed = FALSE, prediction = TRUE,
                sm = "MD", method.tau = "ML",
                method.bias = "rank", hakn = TRUE)

## Summarize model
summary(fit)

## Evaluate model bias using rank measure
metabias(fit, method.bias = "rank", plotit = TRUE,
         correct = TRUE)

## Perform cumulative meta-analysis
metacum(fit, pooled = "random")

## Construct forest plot of metacum
forest(fit, comb.fixed = FALSE, comb.random = TRUE,
       overall = TRUE, prediction = FALSE)
