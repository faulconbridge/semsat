elapsed$BIAS.numeric <- as.numeric(elapsed$BIAS)
View(elapsed)

Dom3 <- subset(elapsed, BIAS=="dominant"
               & REPS==3, select=Elapsed)

Dom30 <- subset(elapsed, BIAS=="dominant" 
                & REPS==30, select=Elapsed)

Sub3 <- subset(elapsed, BIAS=="subordinate" 
               & REPS==3, select=Elapsed)

Sub30 <- subset(elapsed, BIAS=="subordinate" 
                & REPS==30, select=Elapsed)

Unrel3 <- subset(elapsed, BIAS=="unrelated" 
                 & REPS==3, select=Elapsed)

Unrel30 <- subset(elapsed, BIAS=="unrelated" 
                  & REPS==30, select=Elapsed)

findProbs <- function(dataset) {
  plot(density(dataset$Elapsed))
  probs <- c(seq(from=0.125, to=0.875,by=0.125))
  times <- NULL
  for(i in probs){
    times <- cbind(times,c(qnorm(i,mean(dataset$Elapsed),sd(dataset$Elapsed))))
  }
  print(times)
  for(i in 1:length(times)) {
    points(times[1,i],0.1)
    abline(v=times[1,i])
  }
  print(max(dataset$Elapsed))
}

findProbs(Dom3)
findProbs(Dom30)
findProbs(Sub3)
findProbs(Sub30)
findProbs(Unrel3)
findProbs(Unrel30)
