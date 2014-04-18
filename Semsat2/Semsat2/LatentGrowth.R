source('http://openmx.psyc.virginia.edu/getOpenMx.R')
require(OpenMx)

############################################################################

# IMPORTANT!
# If you want to test the fit of your model,
# include the flag "cov" as the final argument
# given to the makeGrowthModel function.
# OpenMx will not compute the saturated model (and
# thus will not return fit statistics) if you input
# raw data. This flag lets the function automatically
# extract a covariance matrix and column means from
# your data, allows OpenMx to automatically
# create the saturated model, and returns fit
# statistics.

# The relevant statistics are: 
#   -chi-square
#   -CFI
#   -TLI
#   -RMSEA

# You want to see a chi-square with a small magnitude
# (closer to 0 is better!; note: this statistic is
# heavily influenced by sample size.). You can normalize
# this statistic by dividing it by the degrees of freedom
# to produce a normed chi-square. This value should
# be generally below 5 for a well-fitted model.
# 
# Comparative Fit Index (CFI) compares performance
# on your model to performance on baseline ("null"
# or "independence") model; baseline model assumes
# zero correlation between all observed variables.
# Again, values closer to 0 are preferred.
# 
# Tucker-Lewis Index (TLI), also non-normed fit
# index, is similar to the CFI: compares specified
# model to a theoretical baseline, but adjusts for
# degrees of freedom.
# 
# Both CFI and TFI can have values not contained in
# the 0-1 range; however, that should be considered
# suspect.
# 
# Root mean square error of approximation (RMSEA)
# gives lack of fit of our model to population data
# when parameters are optimally chosen. A value less
# than 0.05 is optimal; anything above 0.10
# constitutes a poorly-fitted model.

############################################################################

# User-defined function to create a growth curve model
# given an arbitrary dataset

getMxData <- function(dataset, type=NULL) {
  if(missing(type)) {
    temp <- mxData(
      dataset,
      type = "raw"
      )
  } else if(type=="cov") {
    temp <- mxData(
      cov(dataset),
      type = "cov",
      numObs = length(dataset[,1]),
      means = colMeans(dataset)
      )
  } else {
    temp <- mxData (
      dataset,
      type = "raw"
      )
  }
  return(temp)
}

makeGrowthModel <- function(name = NULL, dataset = NULL, slope = NULL, type = NULL) {
  if(missing(name)) {
    stop(sprintf("You forgot to specify a model name! Do so with name = \"myModel\""))
  } else if(missing(dataset)) {
    stop(sprintf("You forgot to specify a dataset! Do so with dataset = myData"))
  }
  
  if(missing(slope)) {
    slope <- c(seq(from = 0, to = length(dataset[1,])-1))
    print("You didn\'t specify a slope. Was this intentional? We defaulted")
    print("to fixed-interval constants. If your data were not collected")
    print("at even intervals, this may impact the results of your analysis.")
  }
  
  temp <- mxModel(name,
                  # Defines type of model: Path or RAM
                  # Path models are specified with explicit
                  # matrices; here, we use RAM
                  type="RAM",
                  getMxData(dataset, type),
                  # Manifest variables are the dependent measures
                  # Here, they are daily measurements of rat weight
                  manifestVars=colnames(dataset),
                  
                  # Our latent variables are the 'hidden' variables
                  # that we are attempting to infer
                  latentVars=c("intercept","slope"),
                  
                  # residual variances
                  mxPath(
                    # Specify the sources of the new paths
                    from=colnames(dataset),
                    
                    # Specify single- or double-headed arrows
                    arrows=2,
                    
                    # Paths are free (TRUE) or fixed (FALSE)
                    free=TRUE,
                    
                    # Numeric vector for the starting parameters
                    # Here, freely estimated but held to a constant
                    # value across all measurement occasions
                    values = c(rep(1, length(colnames(dataset)))),
                    labels=c(rep("residual", length(colnames(dataset))))
                  ),
                  
                  # latent variances and covariance:
                  # This results in three paths: from intercept
                  # to intercept (the variance of the interecpts),
                  # from intercept to slope (the covariance of
                  # the latent variables), and from slope to
                  # slope (the variance of the slopes).
                  mxPath(
                    from=c("intercept","slope"),
                    arrows=2,
                    
                    # Create all unique paths from and to
                    # all latent variables
                    connect="unique.pairs",
                    free=TRUE,
                    values=c(1, 1, 1),
                    labels=c("vari", "cov", "vars")
                  ),
                  
                  # The third and fourth mxPath functions 
                  # specify the factor loadings. As these 
                  # are defined to be a constant value of 
                  # 1 for the intercept factor and the set 
                  # [0, 1, 2, ... n-1] for the slope factor,
                  # these functions have no free parameters
                  
                  # intercept loadings
                  mxPath(
                    from="intercept",
                    to=colnames(dataset),
                    arrows=1,
                    free=FALSE,
                    values=c(rep(1,length(colnames(dataset))))
                  ),
                  # slope loadings
                  mxPath(
                    from="slope",
                    to=colnames(dataset),
                    arrows=1,
                    free=FALSE,
                    
                    # Values for the slope loadings may be arbitrary
                    # but consistently-incrementing numbers (e.g., 0,1,2,...n)
                    # if your time interval of measurement was held
                    # constant. Otherwise, you may need to think
                    # more carefully about how you define this.
                    # For the sample data, I computed variable-interval
                    # values that result in equal areas beneath the
                    # curve of a probability density function of the data
                    # being sampled.
                    values=slope
                  ),
                  
                  # The last two mxPath functions specify the means. 
                  # The manifest variables are not regressed on the
                  # constant, and thus have intercepts of zero. The
                  # observed means are entirely functions of the
                  # means of the intercept and slope. To specify
                  # this, the manifest variables are regressed on
                  # the constant (denoted "one") with a fixed value
                  # of zero, and the regressions of the latent
                  # variables on the constant are estimated as
                  # free parameters.
                  
                  # manifest means
                  mxPath(
                    from="one",
                    to=colnames(dataset),
                    arrows=1,
                    free=FALSE,
                    values=c(rep(0,length(colnames(dataset))))
                  ),
                  # latent means
                  mxPath(
                    from="one",
                    to=c("intercept", "slope"),
                    arrows=1,
                    free=TRUE,
                    values=c(1, 1),
                    labels=c("meani", "means")
                  )
  ) # close model
  return(temp)
}

############################################################################

# This function will plot out time series data for each of
# the experimental conditions in the sample dataset

plotTimeSeries <- function(plotTitle,ylabel,xlabel,dataset) {
  plot(c(seq(from=0,to=7,length.out=9)), c(seq(from=min(dataset),to=6000,
                                               length.out=9)), type="n", xlab=xlabel,
       ylab=ylabel)
  
  participant <- data.frame(X=c(1:9),Time=c(1:9),MS=c(1:9))
  
  for (i in 1:length(dataset[,1])) { 
    participant["Time"] <- c(seq(from=0,to=7,length.out=9))
    participant["MS"] <- c(dataset[i,1],dataset[i,2],dataset[i,3],dataset[i,4],
                         dataset[i,5],dataset[i,6],dataset[i,7],dataset[i,8],dataset[i,9])
    if(max(participant$MS)<6000) {
      lines(participant$Time, participant$MS, type="o", lwd=.5)
    }
  }
  
  participant["Time"] <- c(seq(from=0, to=7,length.out=9))
  participant["MS"] <- colMeans(dataset)
  #lines(participant$Time, participant$MS, type="o", lwd=2, col="blue")
  
  title(plotTitle)
}

############################################################################

# Subset the original data by independent variables

Dom3 <- subset(LatentGrowth, BIAS=="dominant"
               & REPS==3, select=T0:T8)

Dom30 <- subset(LatentGrowth, BIAS=="dominant" 
                & REPS==30, select=T0:T8)

Sub3 <- subset(LatentGrowth, BIAS=="subordinate" 
               & REPS==3, select=T0:T8)

Sub30 <- subset(LatentGrowth, BIAS=="subordinate" 
                & REPS==30, select=T0:T8)

Unrel3 <- subset(LatentGrowth, BIAS=="unrelated" 
                 & REPS==3, select=T0:T8)

Unrel30 <- subset(LatentGrowth, BIAS=="unrelated" 
                  & REPS==30, select=T0:T8)

############################################################################

# Construct time series plots for each experimental condition

layout(matrix(c(1:4),2,2))

plotTimeSeries("Participant Response Time by Study--Test Interval, Dominant, 3 Reps",
               "Response Time (MS)","Approx Time Between Study and Test (mins)",Dom3)
plotTimeSeries("Participant Response Time by Study--Test Interval, Subordinate, 3 Reps",
               "Response Time (MS)","Approx Time Between Study and Test (mins)",Sub3)
plotTimeSeries("Participant Response Time by Study--Test Interval, Unrelated, 3 Reps",
               "Response Time (MS)","Approx Time Between Study and Test (mins)",Unrel3)
plotTimeSeries("Participant Response Time by Study--Test Interval, Dominant, 30 Reps",
               "Response Time (MS)","Approx Time Between Study and Test (mins)",Dom30)
plotTimeSeries("Participant Response Time by Study--Test Interval, Subordinate, 30 Reps",
               "Response Time (MS)","Approx Time Between Study and Test (mins)",Sub30)
plotTimeSeries("Participant Response Time by Study--Test Interval, Unrelated, 30 Reps",
               "Response Time (MS)","Approx Time Between Study and Test (mins)",Unrel30)

############################################################################

# Generate growth curve models for each experimental condition

Dom3Growth <- makeGrowthModel(name = "Dom3Growth", dataset = Dom3,
                              slope = c(0,1.823618,2.566402,3.121861,3.619235,4.116609,
                                        4.672067,5.414852,6.543333), type = "cov")
Dom30Growth <- makeGrowthModel("Dom30Growth",Dom30,
                               c(0,2.004083,2.729832,3.27255,3.758517,4.244484,
                                 4.787203,5.512951,6.450278), type="cov")
Sub3Growth <- makeGrowthModel("Sub3Growth",Sub3,
                              c(0,1.870324,2.614923,3.171738,3.670328,4.168918,
                                4.725733,5.470332,6.453611), type="cov")
Sub30Growth <- makeGrowthModel("Sub30Growth",Sub30,
                               c(0,2.167211,2.854884,3.369129,3.8296,4.290071,
                                 4.804316,5.491988,6.391944), type="cov")
Unrel3Growth <- makeGrowthModel("Unrel3Growth",Unrel3,
                                c(0,1.727652,2.494975,3.068783,3.582588,4.096393,
                                  4.670201,5.437524,6.525), type="cov")
Unrel30Growth <- makeGrowthModel("Unrel30Growth",Unrel30,
                                 c(0,2.062077,2.768994,3.29763,3.770987,4.244344,
                                   4.772981,5.479897,6.481111), type="cov")

############################################################################

# Optimize growth curve models to give fitted model

Dom3Fit <- mxRun(Dom3Growth)
Dom30Fit <- mxRun(Dom30Growth)
Sub3Fit <- mxRun(Sub3Growth)
Sub30Fit <- mxRun(Sub30Growth)
Unrel3Fit <- mxRun(Unrel3Growth)
Unrel30Fit <- mxRun(Unrel30Growth)

############################################################################

# Summarize the results of the fitted models

summary(Dom3Fit)
summary(Dom30Fit)
summary(Sub3Fit)
summary(Sub30Fit)
summary(Unrel3Fit)
summary(Unrel30Fit)

bivHetModel <- mxModel("bivHet",
                       Dom3Fit, Dom30Fit,
                       mxAlgebra(Dom3Fit.objective + Dom30Fit.objective, name="h12"),
                       mxAlgebraObjective("h12")
)

bivHetFit <- mxRun(bivHetModel)

############################################################################

# Visualize equation fitted by latent growth curve,
# compare to original data

layout(matrix(c(1:2),2,1))

plotTimeSeries("Participant Response Time by Study--Test Interval, Dominant, 3 Reps",
               "Response Time (MS)","Time Between Study and Test (mins)",Dom3)
abline(h=1237.60339, col="blue", lwd=3, lty="dashed")
abline(a=1237.60339, b=20.16425, col="red", lwd=3, lty="dashed")
abline(a=1258.68139, b=14.34525, col="green", lwd=3, lty="dashed")

plotTimeSeries("Participant Response Time by Study--Test Interval, Dominant, 30 Reps",
               "Response Time (MS)","Time Between Study and Test (mins)",Dom30)
abline(h=1171.55197, col="blue", lwd=3, lty="dashed")
abline(a=1171.55197,b=51.13003,col="red", lwd=3, lty="dashed")
abline(a=1177.82295, b=46.10317, col="green", lwd=3, lty="dashed")