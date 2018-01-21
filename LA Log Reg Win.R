
library(tidyverse)
library(stringr)
library(rvest)

html.LA <- read_html("https://www.basketball-reference.com/players/a/aldrila01/gamelog/2018/")

LA <- html.LA %>%
  html_nodes("table") %>%
  .[[8]] %>%
  html_table(fill = TRUE)

LA %>% head()

#removing unneeded columns

LA <- LA[,-31]

#removing observations
LA %>% names()
LA <- LA[LA$GS=="1",] #LA starts every game 
colnames(LA)[6]<-("Home")
colnames(LA)[8]<-("Win")
colnames(LA)[30]<-("PlusMinus")
#convert easy vars to numeric
attach(LA)
LA[,c(11:30)] = apply(LA[,c(11:30)], 2, function(x) as.numeric(as.character(x)));
LA$Win <- substring(LA$Win,1,1)

LA$Win <-  as.factor(LA$Win)
contrasts(LA$Win)

model <- glm(Win~FG+FGA+BLK+TOV+PTS+DRB+ORB+AST+STL,family=binomial(link='logit'),data=LA)
summary(model)
anova(model, test="Chisq")
