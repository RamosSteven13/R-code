# Markov-Gauss Mean estimation

c <- matrix(c(0.27958,0.18899,0.18899,0.24621), nrow = 2, ncol=2) #covariance matrix
cInv <- solve(c) #Inverse of Covariance Matrix

#########################################################
x <-matrix( c(1,1,-0.64176,-0.20155), nrow = 2, ncol=2) #X matrix
xprim <- t(x) #X'

xPrimcInv <- xprim%*%cInv #X'C^-1
data <-matrix( c(7.956,7.976), nrow=2,ncol=1) #data matrix

####################################################

xPrimcInvData <- xPrimcInv%*%data #X'*C^-1*Data
xPrimcINvx <- xPrimcInv%*%x      #X'*C^-1*X
xPrimcINvxInv <- solve(xPrimcINvx) #(X'C^-1X)^-1

######################################################

mean <- xPrimcINvxInv%*%xPrimcInvData #(X'C-1X)-1*X'C-1Data
exp(mean)


