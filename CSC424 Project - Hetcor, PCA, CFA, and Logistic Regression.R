###########################################
# Hetcor, PCA, CFA, and Logistic Regression
###########################################

# importing data
ds = read.csv("Final dataset_ver2.csv",stringsAsFactors = FALSE)

# looking at structure 
str(ds)

# separting independent variables by taking out PURCHASE and state
x = ds[-c(1,20)]

#taking out PROP_GAR, Presence.of.Children and Appended.Age.1
x = x[-c(33,20,12)]

#separting response variable
y = ds[1]
head(y)

# looking at distribution and skewness of numeric data
library(e1071)
library(ggplot2)
library(gridExtra)
g1 = ggplot(x, aes(x=PROP_ASSED_VAL)) + geom_histogram(bins=60)
skewness(x$PROP_ASSED_VAL)
g2 = ggplot(x, aes(x=PROP_MRKTVAL)) + geom_histogram(bins=60)
skewness
g3 = ggplot(x, aes(x=PROP_LANDSQFT)) + geom_histogram(bins=60)
skewness(x$PROP_LANDSQFT)
g4 = ggplot(x, aes(x=PROP_LIVINGSQFT)) + geom_histogram(bins=60)
skewness(x$PROP_LIVINGSQFT)
g5 = ggplot(x, aes(x=PROP_TAXAMT)) + geom_histogram(bins=60)
skewness(x$PROP_TAXAMT)
g6 = ggplot(x, aes(x=PROP_SALEAMT)) + geom_histogram(bins=60)
skewness(x$PROP_SALEAMT)
g7 = ggplot(x, aes(x=PROP_LOANTOVAL)) + geom_histogram(bins=60)
skewness(x$PROP_LOANTOVAL)
grid.arrange(g1,g2,g3,g4)
grid.arrange(g5,g6,g7)

# applying log transformation and looking at skewness and distribution again
x[1:7] = log(x[1:7])
g1 = ggplot(x, aes(x=PROP_ASSED_VAL)) + geom_histogram(bins=60)
skewness(x$PROP_ASSED_VAL)
g2 = ggplot(x, aes(x=PROP_MRKTVAL)) + geom_histogram(bins=60)
skewness
g3 = ggplot(x, aes(x=PROP_LANDSQFT)) + geom_histogram(bins=60)
skewness(x$PROP_LANDSQFT)
g4 = ggplot(x, aes(x=PROP_LIVINGSQFT)) + geom_histogram(bins=60)
skewness(x$PROP_LIVINGSQFT)
g5 = ggplot(x, aes(x=PROP_TAXAMT)) + geom_histogram(bins=60)
skewness(x$PROP_TAXAMT)
g6 = ggplot(x, aes(x=PROP_SALEAMT)) + geom_histogram(bins=60)
skewness(x$PROP_SALEAMT)
g7 = ggplot(x, aes(x=PROP_LOANTOVAL)) + geom_histogram(bins=60)
skewness(x$PROP_LOANTOVAL)
grid.arrange(g1,g2,g3,g4)
grid.arrange(g5,g6,g7)

# changing values of ordinal variables from alphabet to numeric
# Note: however, the variables will still be treated as ordinal in analysis
# PROP_BEDRMS and PROP_FULLBATHS are already numeric and ordinal
# discretizing Length.of.Residence

library(arules)
x$Length.of.Residence = discretize(x$Length.of.Residence, method="interval", categories = 3, labels=c(1,2,3))
x$PROP_MTGTERM = discretize(x$PROP_MTGTERM, method="interval", categories = 3, labels=c(1,2,3))

# Making Ordinal variables as factors.
x$Home.Market.Value = as.factor(x$Home.Market.Value)
x$Estimated.Income = as.factor(x$Estimated.Income)
x$Wealth.Score = as.factor(x$Wealth.Score)
x$Purchasing.Power.Score = as.factor(x$Purchasing.Power.Score)
x$Education = as.factor(x$Education)
x$Number.of.Children = as.factor(x$Number.of.Children)
x$PROP_BEDRMS = as.factor(x$PROP_BEDRMS)
x$PROP_FULLBATHS = as.factor(x$PROP_FULLBATHS)
x$Length.of.Residence = as.factor(x$Length.of.Residence)
x$PROP_MTGTERM = as.factor(x$PROP_MTGTERM)

# Making nominal variables with more than 2 levels as dummies
library(dummies)
x2 = x[c(18:23)]

#making PROP_IND and PROP_SALESTRANSCD as character so it's dummies are created.
x2["PROP_IND"][x2["PROP_IND"]== 10] = "10"
x2["PROP_IND"][x2["PROP_IND"]== 11] = "11"
x2["PROP_IND"][x2["PROP_IND"]== 21] = "21"
x2["PROP_IND"][x2["PROP_IND"]== 22] = "22"
x2["PROP_SALESTRANSCD"][x2["PROP_SALESTRANSCD"] == 1] = "1"
x2["PROP_SALESTRANSCD"][x2["PROP_SALESTRANSCD"] == 2] = "2"
x2["PROP_SALESTRANSCD"][x2["PROP_SALESTRANSCD"] == 3] = "3"
x2["PROP_SALESTRANSCD"][x2["PROP_SALESTRANSCD"] == 6] = "6"
x2["PROP_SALESTRANSCD"][x2["PROP_SALESTRANSCD"] == 7] = "7"
x2["PROP_SALESTRANSCD"][x2["PROP_SALESTRANSCD"] == 9] = "9"
dummy = dummy.data.frame(x2)

# adding dummies to original
x = data.frame(x,dummy)

# removing the original nominal and keeping their dummies
x = x[-c(18:23)]

# Making the dummies and binary variables as factors.
for (i in 18:ncol(x)){
  x[,i]=as.factor(x[,i])
}

# Doing hetcor
library(polycor)
library(psych)
str(x)
h = hetcor(x)
options(max.print=100000)
cor = h$correlations

# PROP_IND10; PROP_SALESDEEDCSX; PROP_SALESDEEDCDU; PROP_SALESTRANSCD2 and PROP_MTGLOANCDCNV
# caused some correlations to be NA
# Below I re-ran the correlation after taking them out and now I don't have any NA
xx = x[-c(34,40,41,43,48)]
h2 = hetcor(xx)
cor_2 = h2$correlations

# However, removing those dummies and keeping their other dummies in the data is not right.
# So I am going to remove all dummies related to the above dummy variables
# 34 to 37 is PROP_IND
# 38 to 41 is PROP_SALESDEEDCD
# 42 to 47 is PROP_SALESTRANSCD
# 48 to 52 is PROP_MTGLOANCD
xx2 = x[-c(34:37,38:41,42:47, 48:52)]
h3 = hetcor(xx2)
cor_3 = h3$correlations

# Running PCA
p = princomp(covmat = h3$correlations, cor=F)
summary(p)
plot(p)

# PCA showed that 23 components gave us 90 % variance
# Running CFA
p2 = principal(h3$correlations, nfactors = 23)
summary(p2)
print(p2$loadings, cutoff=.4)

# However, there are some components in CFA which only have one variable in them.
# we going to reduce the number of components to 20
p3 = principal(h3$correlations, nfactors = 20)
summary(p3)
print(p3$loadings, cutoff=.4)

# 20 didn't work. Lets try 15
p4 = principal(h3$correlations, nfactors = 15)
summary(p4)
print(p4$loadings, cutoff=.4)

# we still have one component with just one variable contributing
p5 = principal(h3$correlations, nfactors = 13)
summary(p5)
print(p5$loadings, cutoff=.4)

# There are some loadings in 0.4s. I am going to use a cuttoff of 0.5
# Using 13 components with 72% variance for better interpretation
print(p5$loadings, cutoff=.5)


# Getting the scores
#converting factors to numeric
for (i in 8:ncol(xx2)){
  xx2[,i]=as.numeric(xx2[,i])
}
str(xx2)

# getting scores and scaling them
p5$scores <- factor.scores(xx2,p5)
print("")
print("Sample of calculated factor scores:")
print("")
print(head(p5$scores$scores))

# putting the scores of components and response in one dataframe for logistic regression
data = data.frame(p5$scores$scores,y)
data$PURCHASE = as.factor(data$PURCHASE)

#logistic regression
# 13 components
# it showed that none of the components were significant
model = glm(PURCHASE~.,family=binomial(link='logit'),data=data)
summary(model)

# lets use the all components we got at the beginning from PCA of hetcor
# all components are significant but we have too many components
p2$scores <- factor.scores(xx2,p2)
data2 = data.frame(p2$scores$scores,y)
data2$PURCHASE = as.factor(data2$PURCHASE)
model2 = glm(PURCHASE~.,family=binomial(link='logit'),data=data2)
summary(model2)

# Using model selection
# model selection
model_step = step(model2, direction = "backward")
summary(model_step)

# we are going to use less components because the prof asked us to show less predictors
#Lets try 20 components
p3$scores <- factor.scores(xx2,p3)
data3 = data.frame(p3$scores$scores,y)
data3$PURCHASE = as.factor(data3$PURCHASE)

#All are significant
model3 = glm(PURCHASE~.,family=binomial(link='logit'),data=data3)
summary(model3)

#Lets use 18 variables
p33 = principal(h3$correlations, nfactors = 18)
summary(p33)
print(p33$loadings, cutoff=.5)
p33$scores <- factor.scores(xx2,p33)
data4 = data.frame(p33$scores$scores,y)
data4$PURCHASE = as.factor(data4$PURCHASE)
model4 = glm(PURCHASE~.,family=binomial(link='logit'),data=data4)
summary(model4)

#Lets use 15 variables
p44 = principal(h3$correlations, nfactors = 13)
summary(p44)
print(p44$loadings, cutoff=.4)
p44$scores <- factor.scores(xx2,p44)
data5 = data.frame(p44$scores$scores,y)
data5$PURCHASE = as.factor(data5$PURCHASE)
model5 = glm(PURCHASE~.,family=binomial(link='logit'),data=data5)
summary(model5)

#Lets use 16 variables
p55 = principal(h3$correlations, nfactors = 16)
summary(p55)
print(p55$loadings, cutoff=.4)
p55$scores <- factor.scores(xx2,p55)
data6 = data.frame(p55$scores$scores,y)
data6$PURCHASE = as.factor(data6$PURCHASE)
model6 = glm(PURCHASE~.,family=binomial(link='logit'),data=data6)
summary(model6)

#Lets use 17 variables
p66 = principal(h3$correlations, nfactors = 17)
summary(p66)
print(p66$loadings, cutoff=.4)
p66$scores <- factor.scores(xx2,p66)
data7 = data.frame(p66$scores$scores,y)
data6$PURCHASE = as.factor(data6$PURCHASE)
model7 = glm(PURCHASE~.,family=binomial(link='logit'),data=data7)
summary(model7)

#Running backward selection on final model
model_step = step(model4, direction = "backward")
summary(model_step)

# Cross validation method with final 18 components
library(ggplot2)
library(caret)
train_control<- trainControl(method="cv", number=10)
cvmodel<- train(PURCHASE~., data=data4, trControl=train_control, method="glm", family=binomial())
summary(cvmodel)
cvmodel$results

#Interpreting the factors of final model
print(p33$loadings, cutoff=.6)
