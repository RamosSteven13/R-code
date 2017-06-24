#Survival Analysis

weibSurv <- function(t, shape, scale){ pweibull(t, shape=shape, scale=scale, lower.tail=F) }

curve(weibSurv(x, shape=1.5, scale=1/0.03), from=0, to=80, ylim=c(0,1), 
      ylab='Survival probability', xlab='Time')

weibHaz <- function(x, shape, scale) {
  dweibull(x, shape=shape, scale=scale)/pweibull(x, shape=shape, scale=scale, lower.tail=F) }

curve(weibHaz(x, shape=1.5, scale=1/0.03), from=0, to=80, ylab='Hazard', xlab='Time', col="red"
      ,add=T)
curve(weibHaz(x, shape=1, scale=1/0.03), from=0, to=80, ylab='Hazard', xlab='Time', col="black"
      ,add=T)
curve(weibHaz(x, shape=0.75, scale=1/0.03), from=0, to=80,ylab='Hazard', xlab='Time', col="blue"
      ,add=T)

#Regression analysis
library(asaur);library(survival)
plsimple <- function(beta) { psi <- exp(beta) 
result <- log(psi) - log(3*psi + 3) -log(3*psi + 1) - log(2*psi + 1) 
result }

result <- optim(par=0, fn = plsimple, method = "L-BFGS-B",
           control=list(fnscale = -1), lower = -3, upper = 1) 
result$par

result

result.cox <- coxph(Surv(tt, status) ~ grp) 
summary(result.cox)

tt <- c(6, 7, 10, 15, 19, 25)
status <- c(1,0,1,1,0,1)
grp <- c("c","c","t","c","t","t")
result.cox <- coxph(Surv(tt, status) ~ grp)
summary(result.cox)

#5.6 Handling tied survival times
tt <- c(7, 6, 6, 5, 2, 4, 4, 1, 3, 1) 
status <- c(0, 1, 0, 0, 1, 1, 1, 1, 0, 1) 
grp <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1)

loglikContinuous <- function(b) { result <- 3*b + log(exp(b) + 9) - log(4*exp(b) + 6) -
  log(3*exp(b) + 6) - log(2*exp(b) + 6) -log(exp(b) + 5) - log(exp(b) + 4) 
    result }

loglikDiscrete <- function(b) { resultA <- exp(2*b)/(6*exp(2*b) + 24*exp(b) + 15) 
  resultB <- 1/(6 + 2*exp(b)) 
    resultC <- exp(b)/(10+5*exp(b)) 
      result <- log(resultA) + log(resultB) + log(resultC) 
          result }

result.optim.continuous <- optim(par=1.4, fn=loglikContinuous,method="BFGS", 
                                 control=list(fnscale = -1) )
result.optim.continuous$par

result.optim.discrete <- optim(par=1.4, fn=loglikDiscrete, method="BFGS", 
                               control=list(fnscale = -1) )
result.optim.discrete$par

result.coxph <- coxph(Surv(tt, status) ~ grp, ties="exact") 
result.coxph$coef


