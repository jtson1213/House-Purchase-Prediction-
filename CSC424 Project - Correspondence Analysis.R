###########################################
# Correspondence Analysis
###########################################
fp = read.csv("Final dataset_ver2.csv", head=T)

# Create table with variables Marital Status and income
mitable = table(as.array(fp$Marital.Status),as.array(fp$Estimated.Income),exclude = "null")
head(mitable)
# round and create Correspondence analysis with Marital Status and income
MaritalIncome = round(prop.table(mitable),4)
MaritalIncome
fitMI = ca(MaritalIncome)
mosaicplot(MaritalIncome,main='mosaic', shade = TRUE, Legend = TRUE)
summary(fitMI)
plot(fitMI)
plot(fitMI, mass=T, contrib="absolute", map="rowgreen", arrows=c(F, T))

# round and create Correspondence analysis with wealth score and purchasing power score
Wealthpower = round(prop.table(wptable),4)
Wealthpower
fit = ca(Wealthpower)
fit
mosaicplot(Wealthpower,main='mosaic', shade = TRUE, Legend = TRUE)
summary(fit)
plot(fit)
plot(fit, mass=T, contrib="absolute", map="rowgreen", arrows=c(F, T))
