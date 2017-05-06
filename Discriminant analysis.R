#Linear Discriminat analysis

#Creating the data set
Mowers <- " Mowers Income Lot.Size
Non		33.00	18.80
Non		43.20	20.40
Non		47.40	16.40
Non		49.20	17.60
Non		52.80	20.80
Non		59.40	16.00
Riding	60.00	18.40
Riding	61.50	20.80
Riding	64.80	21.60
Non		64.80	17.20
Non		66.00	18.40
Riding	69.00	20.00
Non		75.00	19.60
Riding	82.80	22.40
Non		84.00	17.60
Riding	85.50	16.80
Riding	87.00	23.60
Riding	93.00	20.80
Riding	108.00	17.60
Riding	110.10	19.20
"
attach(Mowers)
plot(Income,Lot.Size,col=Mowers$Mowers)

Mowers<- read.table(textConnection(Mowers), header = TRUE) 
closeAllConnections() 

#save(Mowers,file="Mowers.Rda")
#load("Mowers.Rda") Do every pasted data set

# Linear Discriminant Analysis with Jacknifed Prediction 
library(MASS)
fit <- lda(Mowers ~ Income+Lot.Size, data=Mowers)
fit # show results

# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(Mowers$Mowers, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

fit$posterior


library(klaR)
partimat(Mowers ~ Income+Lot.Size, data=Mowers,method="lda")

predict(fit,newdata=data.frame(Income=65,Lot.Size=20))



