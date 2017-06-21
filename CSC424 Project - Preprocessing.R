###################################################################
# Comparision of distribution for original and sample data set
###################################################################

# Without missing cases
house2 = read.csv("Final dataset_ver2.csv",stringsAsFactors = FALSE)

# With missing cases
house1 = read.csv("mydata5.csv", head=T, na.strings = c("","NA"), stringsAsFactors = FALSE)

# Percentage of missing cases for each variable
sapply(house1, function(x) round(sum(is.na(x))/nrow(house1),2))
missing = house1[8:23]
missing = missing[-c(12,13)]
sapply(missing, function(x) round(sum(is.na(x))/nrow(missing),2))

# Distribution for dependent variable
house1_missing = house1 
for (i in 2:ncol(house1_missing)){
  house1_missing$PURCHASE[is.na(house1_missing[i]) & house1_missing$PURCHASE == "0"] = "2"
  house1_missing$PURCHASE[is.na(house1_missing[i]) & house1_missing$PURCHASE == "1"] = "3"
}
plot(house1$PURCHASE, main = "Distribution for Purchase in the original data", xlab = "Count", ylab = "Purchase")
plot(house1_missing$PURCHASE, main = "Distribution for Purchase in the original data", xlab = "Count", ylab = "Purchase")
plot(house2$PURCHASE, main = "Distribution for Purchase in the sample data", xlab = "Count", ylab = "Purchase")

# distribution for numeric
summary(log(house1$PROP_LOANTOVAL))
summary(log(house2$PROP_LOANTOVAL))
boxplot(log(house1$PROP_LOANTOVAL)~house1$PURCHASE, main = "PROP_LOANTOVAL Distribution in the original data",
        xlab = "PURCHASE", ylab = "Loan to Value")
boxplot(log(house2$PROP_LOANTOVAL)~house2$PURCHASE, main = "PROP_LOANTOVAL Distribution in the sample data",
        xlab = "PURCHASE", ylab = "Loan to Value")

# distribution for categorical
counts_cat1 = table(house1$Wealth.Score)
counts_cat1
counts_cat2 = table(house2$Wealth.Score)
counts_cat2
barplot(counts_cat1, main = "Wealth Score Distribution in the original data",
        xlab = "Wealth Score")
barplot(counts_cat2, main = "Wealth Score Distribution in the sample data",
        xlab = "Wealth Score")




###################################################################
# Data transformation 
# Check for multicollinearity on simple logistic regresssion model
###################################################################

numeric = house2[2:8]
head(numeric)
summary(numeric)

library(corrplot)
c = cor(numeric)
corrplot(c, method="circle")

# you may see the rorrelation plot for ordinal later
# multicollinearity
x = house2[-c(1,20)]
x = x[-c(33,20,12)]
y = house2[1]
x[1:7] = log(x[1:7])

library(arules)
x$Length.of.Residence = discretize(x$Length.of.Residence, method="interval", categories = 3, labels=c(1,2,3))
x$PROP_MTGTERM = discretize(x$PROP_MTGTERM, method="interval", categories = 3, labels=c(1,2,3))

x["PROP_IND"][x["PROP_IND"]== 10] = "10"
x["PROP_IND"][x["PROP_IND"]== 11] = "11"
x["PROP_IND"][x["PROP_IND"]== 21] = "21"
x["PROP_IND"][x["PROP_IND"]== 22] = "22"

x["PROP_SALESTRANSCD"][x["PROP_SALESTRANSCD"] == 1] = "1"
x["PROP_SALESTRANSCD"][x["PROP_SALESTRANSCD"] == 2] = "2"
x["PROP_SALESTRANSCD"][x["PROP_SALESTRANSCD"] == 3] = "3"
x["PROP_SALESTRANSCD"][x["PROP_SALESTRANSCD"] == 6] = "6"
x["PROP_SALESTRANSCD"][x["PROP_SALESTRANSCD"] == 7] = "7"
x["PROP_SALESTRANSCD"][x["PROP_SALESTRANSCD"] == 9] = "9"

for (i in 8:ncol(x)){
  x[,i] = as.factor(x[,i])
}
xx = x[-c(18:23)]
xd = x[c(18:23)]

library(dummies)
dummy = dummy.data.frame(xd)
dummy = dummy[-c(4,14,19,21,25)]
final_x = data.frame(xx,dummy)
data= data.frame(y,final_x)
simple.model = glm(PURCHASE~., family=binomial(link='logit'),data=data)
summary(simple.model)

library(car)
vif_score = vif(simple.model)


