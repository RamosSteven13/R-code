#The Data for Painted Turtles
turtle <- "Gender	Length	Width	Height
Female	98	81	38
Female	103	84	38
Female	103	86	42
Female	105	86	42
Female	109	88	44
Female	123	92	50
Female	123	95	46
Female	133	99	51
Female	133	102	51
Female	133	102	51
Female	134	100	48
Female	136	102	49
Female	138	98	51
Female	138	99	51
Female	141	105	53
Female	147	108	57
Female	149	107	55
Female	153	107	56
Female	155	115	63
Female	155	117	60
Female	158	115	62
Female	159	118	63
Female	162	124	61
Female	177	132	67
Male	93	74	37
Male	94	78	35
Male	96	80	35
Male	101	84	39
Male	102	85	38
Male	103	81	37
Male	104	83	39
Male	106	83	39
Male	107	82	38
Male	112	89	40
Male	113	88	40
Male	114	86	40
Male	116	90	43
Male	117	90	41
Male	117	91	41
Male	119	93	41
Male	120	89	40
Male	120	93	44
Male	121	95	42
Male	125	93	45
Male	127	96	45
Male	128	95	45
Male	131	95	46
Male	135	106	47"
turtle <- read.table(textConnection(turtle), header = TRUE) 
closeAllConnections() 
turtle



install.packages(c("readxl","car","rgl"))
library("readxl");library("car");library("rgl");



attach(turtle)

scatter3d(Height~Length+Width | as.factor(Gender), surface=FALSE,data=turtle)


cor(turtle[-1], use="complete.obs")



male <- subset(turtle, Gender=="Male")
cor(male[-1], use="complete.obs")
a <- cov(male[-1])
eigen(a)
determinant(a)
m <- princomp(male[-1], cor = FALSE)
summary(m)
plot(m)
biplot(m)
male <- cbind(male,m$scores)


female <- subset(turtle, Gender=="Female")
cor(female[-1], use="complete.obs")
b <- cov(female[-1])
eigen(b)
f <- princomp(female[-1])
plot(f)
biplot(f)
female <- cbind(female,f$scores)
Hare <- rbind(male,female)
attach(Hare)
fsub <- female[-1]
msub <- male[-1]
library("Hotelling")
hotelling.test(msub,fsub)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(f, obs.scale = 1, var.scale = 1,
         groups = wine.class , ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

scatter3d(Comp.3~Comp.2+Comp.1 | as.factor(Gender), surface=FALSE,data=Hare)




####################################################################################################

Nitrous <- "Nox Humid Temp Bpres
.81 74.92 78.36 29.08                                                           
.96 44.64 72.48 29.37                                                           
.96 34.30 75.22 29.28                                                           
.94 42.36 67.28 29.29                                                           
.99 10.12 76.45 29.78                                                           
1.11 13.22 67.07 29.40                                                          
1.09 17.07 77.79 29.51                                                          
.77 73.70 77.36 29.14                                                           
1.01 21.54 67.62 29.50                                                          
1.03 33.87 77.20 29.36                                                          
.96 47.85 86.57 29.35                                                           
1.12 21.89 78.05 29.63                                                          
1.01 13.14 86.54 29.65                                                          
1.10 11.09 88.06 29.65                                                          
.86 78.41 78.11 29.43                                                           
.85 69.15 76.66 29.28                                                           
.70 96.50 78.10 29.08                                                           
.79 108.72 87.93 28.98                                                          
.95 61.37 68.27 29.34                                                           
.85 91.26 70.63 29.03                                                           
.79 96.83 71.02 29.05                                                           
.77 95.94 76.11 29.04                                                           
.76 83.61 78.29 28.87                                                           
.79 75.97 69.35 29.07                                                           
.77 108.66 75.44 29.00                                                          
.82 78.59 85.67 29.02                                                           
1.01 33.85 77.28 29.43                                                          
.94 49.20 77.33 29.43                                                           
.86 75.75 86.39 29.06                                                           
.79 128.81 86.83 28.96                                                          
.81 82.36 87.12 29.12                                                           
.87 122.60 86.20 29.15                                                          
.86 124.69 87.17 29.09                                                          
.82 120.04 87.54 29.09                                                          
.91 139.47 87.67 28.99                                                          
.89 105.44 86.12 29.21                                                          
.87 90.74 86.96 29.17                                                           
.85 142.20 87.08 28.99                                                          
.85 136.52 84.96 29.09                                                          
.70 138.90 85.91 29.16                                                          
.82 89.69 86.69 29.15                                                           
.84 92.59 85.27 29.18                                                           
.84 147.63 87.25 29.10                                                          
.85 141.35 86.34 29.06"

Nitrous <- read.table(textConnection(Nitrous), header = TRUE) 
closeAllConnections() 
Nitrous

cor(Nitrous,use="complete.obs")
attach(Nitrous)
model <- lm(Nox~Humid+Temp+Bpres)
summary(model)
plot(model)
plot(Nox~model$fitted.values)
vif(model)
