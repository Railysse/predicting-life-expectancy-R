#Using multiple imputation to deal with missing values

if (!require("olsrr")) install.packages("olsrr")
if (!require("leaps")) install.packages("leaps")
if (!require("mice")) install.packages("mice")
if (!require("readr")) install.packages("readr")
if (!require("corrplot")) install.packages("corrplot")
if (!require("faraway")) install.packages("faraway")
if (!require("DTK")) install.packages("DTK")

library('readr')
library('olsrr')
library('leaps')
library('mice')
library("corrplot")
library("faraway")
library("DTK")

#import data
Life_Expectancy_Data1 <- read.csv("E:/Downloads/Life_Expectancy_Data1 (1).csv")

#check the data
head(Life_Expectancy_Data1)
dim(Life_Expectancy_Data1)
md.pattern(Life_Expectancy_Data1)

#change names
names.equiv=rbind(names(Life_Expectancy_Data1), c("Country_Name", "Country_Code", "Continent", "y","c1","c2","c3", "c4","c5","c6","c7", "c8","c9","c10","c11", "c12","c13","c14","c15", "c16","c17","c18","c19", "c20", "c21", "c22", "c23", "c24", "c25"))
names.equiv
names(Life_Expectancy_Data1)=c("Country_Name", "Country_Code", "Continent", "y","c1","c2","c3", "c4","c5","c6","c7", "c8","c9","c10","c11", "c12","c13","c14","c15", "c16","c17","c18","c19", "c20", "c21", "c22", "c23", "c24", "c25")


boxplot(Life_Expectancy_Data1$y ~ Life_Expectancy_Data1$Continent, xlab="Continent ", col="steelblue3",ylab = "Life Expectancy at birth (Years)", names = c("Africa", "Asia", "Australia", "Europe","N America", "S America"))

ggplot(Life_Expectancy_Data1, aes(x = y, y = Continent)) +
  geom_boxplot(fill = "steelblue")+theme_bw()+scale_y_discrete(labels = c("Africa","Asia","Oceania","Europe","N America","S America"))+xlab("Life Expectancy") +scale_x_continuous(breaks = round(seq(min(cont$y), max(cont$y), by = 5),0))

median(unlist(Life_Expectancy_Data1[!is.na(Life_Expectancy_Data1$y),4]))
mean(unlist(Life_Expectancy_Data1[!is.na(Life_Expectancy_Data1$y),4]))
sd(unlist(Life_Expectancy_Data1[!is.na(Life_Expectancy_Data1$y),4]))
hist(Life_Expectancy_Data1$y, col="steelblue", breaks = 20, ylim=c(0,30), main="", xlab = "Life Expectancy at birth (years)", ylab="Number of countries")

barplot(table(Life_Expectancy_Data1$Continent), names.arg = c("Africa", "Asia", "Australia", "Europe","N America", "S America"), col="steelblue", ylim = c(0,60))
plot(Life_Expectancy_Data1$c3, Life_Expectancy_Data1$c2, ylim= c(-3.5,11) ,xlim = c(-5,10), pch = 20, cex=1.4, col="steelblue", ylab="Adjusted net national income (% growth)", xlab="Adjusted net national income per capita (% growth)")

#------------------pre-processing of the data---------------------------
S=0
for (i in 1:nrow(Life_Expectancy_Data1)){
  for (j in 4:ncol(Life_Expectancy_Data1)){
    if (is.na(Life_Expectancy_Data1[i,j])){
      S=S+1
    }
  }
}
S/((ncol(Life_Expectancy_Data1)-3)*nrow(Life_Expectancy_Data1))*100 #percentage of missing values excluding the first three columns

#check number of missing values per row
r_missing=matrix(0,nrow(Life_Expectancy_Data1),1)
for (i in 1:nrow(Life_Expectancy_Data1)){
  #r_missing[i,1]=round(100*length(which(is.na(Life_Expectancy_Data1[i,])))/ncol(Life_Expectancy_Data1),2)
  r_missing[i,1]=round(length(which(is.na(Life_Expectancy_Data1[i,]))),2)
}
r_missing 
mean(r_missing)
sd(r_missing)
full_obs=0
for (i in 1:length(r_missing)){
  if (r_missing[i,1]==0){
    full_obs=full_obs+1
  }
}
full_obs #number of complete observations
#check percentage of missing values per column
missing=matrix(0,3,ncol(Life_Expectancy_Data1))
missing[1,]=names(Life_Expectancy_Data1)
for (i in 1:ncol(Life_Expectancy_Data1)){
  missing[2,i]=round(100*length(which(is.na(Life_Expectancy_Data1[,i])))/nrow(Life_Expectancy_Data1),2)
  missing[3,i]=length(which(is.na(Life_Expectancy_Data1[,i])))
}
missing 


#deleting the columns where more than 60% values are missing
col_removed=c()
for (i in 1:ncol(Life_Expectancy_Data1)){
  if (as.numeric(missing[2,i])>=60){
    col_removed=cbind(col_removed, c(i))
  }
}
col_removed
Life_Expectancy_Data1=Life_Expectancy_Data1[,-c(col_removed)]


#---------------------------------------------------------------------------
#Stage 1:Imputation Stage
imputations <- mice(Life_Expectancy_Data1, method ="cart", seed = 7448) 
print(imputations) #the imputed data is sorted into class mids

complete(imputations) #checking the complete dataset
complete_data=complete(imputations,5) #complete(imp,m) where m is the number of iterations

imputations$imp #diagnosing issues e.g. negative numbers


#response variable is life expectancy 
xyplot(imputations, y ~ c8| .imp, col = c("dodgerblue4", "orangered1"), pch = 20, cex = 1.3,ylab="Life Expectancy at birth (years)", xlab="Mortality rate, infant (per 1,000 live births)")



#Stage 2:Analysis
model.fit <- with(imputations, lm(y ~ c1+c2+c3+c4+c5+c8+c9+c11+c12+c13+c14+c15+c16+c17+c18+c19+c20+c22+c23+c25))
summary(model.fit)


#Stage 3:Pooling
pooled.model<-pool(model.fit)
summary(pooled.model) #most features don't have significantly small p-values

pool.r.squared(model.fit)
#------------------------------------------------------------------------------
#Q3 - Checking for collinearity
if (!require("corrplot")) install.packages("corrplot")
if (!require("faraway")) install.packages("faraway")
library("corrplot")
library("faraway")


reduced_df<-complete(imputations,5)[,-c(1,2,3)] #remove the character columns that we don't want to check

#corrplot(cor(reduced_df), method="circle")
corrplot(cor(reduced_df), method = 'square', type = 'upper', diag = FALSE, tl.pos = "td", tl.cex = 1, tl.offset = 0.3,tl.col = "indianred4")
corrplot.mixed(cor(reduced_df), lower="number", lower.col = "black", upper = "square", tl.pos = "d", number.cex = .6, tl.col = "indianred4") #check the pairwise correlation
vif(reduced_df[,-1]) #check the VIF of each predictor

#Visualize the VIF
barplot(vif(reduced_df[,-1]), horiz = TRUE, las=1, col = "steelblue", main ="VIF of each variable before column removal", xlab = "VIF", ylab = "Variable Name",xlim = c(0,11))
axis(1, at = seq(1, 11, by = 1), las=1)
abline(v = 10, lwd = 2, lty = 2, col = "brown2")
abline(v = 5, lwd = 1, lty = 2)

reduced_df=subset(reduced_df, select = -c(c20)) #we remove column c20, since it has the highest VIF
corrplot(cor(reduced_df), method = 'square', type = 'upper', diag = FALSE, tl.pos = "td", tl.cex = 1, tl.offset = 0.3,tl.col = "indianred4")
vif(reduced_df[,-1]) 

#c8 is highly correlated with our predictor, so we keep it into the model despite having a rather high VIF
plot(reduced_df$c2,reduced_df$c3, xlim = c(-5,11), ylim = c(-10,10), xlab = "Adjusted net national income", ylab = "Adjusted net national income per capita", col= "Red")
reduced_df=subset(reduced_df, select = -c(c3)) #not only is c2 correlated to c3, but they also have a high VIF, so we remove c3


#From the plot, C1 seems to be correlated with multiple variables. It also has a high VIF, so we remove it
reduced_df=subset(reduced_df, select = -c(c1)) 


#---test
#reduced_df=subset(reduced_df, select = -c(c9, c22, c19)) 
#reduced_df=subset(reduced_df, select = -c( c15)) 

#Visualizing VIF after removing c20, c3 and c1
barplot(vif(reduced_df[,-1]), horiz = TRUE, las=1, col = "steelblue", main ="VIF of each variable after column removal", xlab = "VIF", ylab = "Variable Name", xlim = c(0,11))
axis(1, at = seq(1, 11, by = 1), las=1)
abline(v = 10, lwd = 2, lty = 2, col = "brown2")
abline(v = 5, lwd = 1, lty = 2)
#Conclusion: We don't want c20, c3 and c1 in our model, since they cause multicollinearity

#------------------------------------------------------------------------------
#Q4 - Finding the best model

feature.selection1 = expression(null.model1 <- lm(y ~ 1),
                                 model2 <- step(null.model1, scope = ~c2+c4+c5+c8+c9+c11+c12+c13+c14+c15+c16+c17+c18+c19+c22+c23+c25 ))

#feature.selection1 = expression(null.model1 <- lm(y ~ c2+c4+c5+c8+c9+c11+c12+c13+c14+c15+c16+c17+c18+c19+c22+c23+c25),
#                               model2 <- step(null.model1, scope = ~c2+c4+c5+c8+c9+c11+c12+c13+c14+c15+c16+c17+c18+c19+c22+c23+c25 ))

#Using the with function to evaluate the above expression
step.fit = with(imputations, feature.selection1)

step.fit.models = lapply(step.fit$analyses, formula)
step.fit.features = lapply(step.fit.models, terms)
feature.frequency = unlist(lapply(step.fit.features, labels))
feature.frequency
sort(table(feature.frequency),decreasing=TRUE)
summary(lm(data=complete(imputations,5), y ~ c8 + c23 + c15 + c22))

model.fit <- with(imputations, lm(y ~ c8+ c23+c15))
summary(model.fit)
pooled.model<-pool(model.fit) 
summary(pooled.model) #get coefficients for the pooled model

library("car")
#test for normality of the residuals
qqnorm(residuals(lm(formula = y ~ c8 + c23 + c15, data = complete(imputations, 5))), pch = 20, cex=1.4, col="dodgerblue3", main="Original dataset",ylab="", xlab="", xaxt="n")
qqline(residuals(lm(formula = y ~ c8 + c23 + c15, data = complete(imputations, 5))), lt = 1)
shapiro.test(residuals(lm(formula = y ~ c8 + c23 + c15, data = Life_Expectancy_Data1))) 
#summary(lm(data=complete(imputations, 1), y~c8+c23+c15))

y_pred=rep(0, nrow(Life_Expectancy_Data1))
for (i in 1:5){
  model=lm(data=complete(imputations,i), y ~ c8 + c15 +c23)
  model$coefficients=c(74.1975189815, -0.2637189928, 0.0006708319, 0.0492452611)
  y_pred=y_pred+predict(model, complete(imputations,i))
}
y_pred=y_pred/5

pred_df=cbind(Life_Expectancy_Data1$Country_Name, Life_Expectancy_Data1$y, y_pred, Life_Expectancy_Data1$c8, Life_Expectancy_Data1$c15, Life_Expectancy_Data1$c23)

# CONTINENT means
cont=data.frame(Country_Name=Life_Expectancy_Data1$Country_Name, Continent=Life_Expectancy_Data1$Continent, y=Life_Expectancy_Data1$y )
cont=na.omit(cont)

one_way=aov(cont$y ~ cont$Continent)
summary(one_way)
#normality of the residuals
qqnorm(one_way$residuals, pch=19)
shapiro.test(one_way$residuals)
hist(one_way$residuals)

#homogeneity of variance
#sub=cont[(cont$Continent!="Africa") & (cont$Continent!="Australia/Oceania"),]
#bartlett.test(sub$y ~ sub$Continent)
bartlett.test(cont$y ~ cont$Continent)

one_way=aov(sub$y ~ sub$Continent)
summary(one_way)

tapply(cont$y,cont$Continent,mean)
#tests for differences
pairwise.t.test(cont$y, cont$Continent, p.adj = "bonferroni") #pairwise comparisons
TukeyHSD(one_way)




vars <- names(complete_data)[-c(1,2,3,4)]
models <- list()

for (i in 1:5){
  vc <- combn(vars,i)
  for (j in 1:ncol(vc)){
    model <- as.formula(paste0("y ~", paste0(vc[,j], collapse = "+")))
    models <- c(models, model)
  }
}
