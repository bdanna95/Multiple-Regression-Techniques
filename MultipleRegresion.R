df <- read.csv("~/data_470479.csv")
#Check normality assumption of Y
#Adjust number of plots to display
par(mfrow=c(2,2))

#create Q-Q plot for Y
qqnorm(df$Y)

#add a straight diagonal line to the plot
qqline(df$Y) 

#Create density plot of Y
plot(density(df$Y),main = "Density Plot of Depedent Variable")

#Y is skewed, use Box Cox transformation to find optimal lambda
require(MASS)    
bc <- boxcox(df$Y ~ 1)
(lambda <- bc$x[which.max(bc$y)])
lambda <- 0.5

#Re-evaluate normality assumption
#Adjust number of plots to display
par(mfrow=c(1,2))

#create Q-Q plot for residuals
qqnorm(((df$Y^lambda-1)/lambda))

#add a straight diagonal line to the plot
qqline(((df$Y^lambda-1)/lambda)) 

#Create density plot of residuals
plot(density(((df$Y^lambda-1)/lambda)),main = "Density Plot of Transformed Depedent Variable")

df$Y = ((df$Y^lambda-1)/lambda)
#Plot Y against continuous environmental variables to determine if a linear relationship exists
require(ggplot2)

ggplot(df, aes(x=E1, y=Y)) + geom_point()
ggplot(df, aes(x=E3, y=Y)) + geom_point()
ggplot(df, aes(x=E6, y=Y)) + geom_point()

#Adjust number of plots to display
par(mfrow=c(1,3))
plot(df$E1,df$Y,xlab="E1",ylab="Y",main = "Scatter of E1 vs Y")
plot(df$E3,df$Y,xlab="E3",ylab="Y",main = "Scatter of E3 vs Y")
plot(df$E6,df$Y,xlab="E1",ylab="Y",main = "Scatter of E6 vs Y")
#E1, E3, E6

#Check for missing values
sum(is.na(df))
#No missing data was found

#Perform correlation test on each of the three environmental variables that showed a linear association
cor.test(df$Y,df$E1) #0.39 with sig p-value
cor.test(df$Y,df$E3) #0.32 with sig p-value
cor.test(df$Y,df$E6) #0.27 with sig p-value

#Perform t-tests for mean of y conditioned on each indicator variable
for (i in colnames(df)[10:39]){
  print(t.test(subset(df, df[[i]] == 1,select=c(Y)),subset(df, df[[i]] == 0,select=c(Y)))$p.value)
}
#G2,G14,G21,G23,G24 appear to be significant between 0 and 1 indicator on/off

#Fit multiple regression of Y on all 38 IV
lm.fit.full <- lm(Y ~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10
                  + G11 + G12 + G13 + G14 + G15 + G16 + G17 + G18 + G19 + G20 + G21 + G22 + G23 + G24
                  + G25 + G26 + G27 + G28 + G29 + G30,
                  data = df)
summary(lm.fit.full)

#Backward selection, check F stat for each of two models with one deleting variables
lm.fit.minus1 <- lm(Y ~ E1 + E3 + E6 + G2 + G6 + G14 + G23,
                  data = df)
lm.fit.minus2 <- lm(Y ~ E1 + E3 + E6 + G2 + G14 + G23,
                    data = df)
summary(lm.fit.minus2)

#Compare each iteration of two models
anova(lm.fit.minus1,lm.fit.minus2)

#Check all 2 way interaction terms
lm.fit.fullint <- lm(Y ~ (E1 + E2 + E3 + E4 + E5 + E6 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10
                  + G11 + G12 + G13 + G14 + G15 + G16 + G17 + G18 + G19 + G20 + G21 + G22 + G23 + G24
                  + G25 + G26 + G27 + G28 + G29 + G30)^2,
                  data = df)
summary(lm.fit.fullint)

#Stepwise forward selection to determine if there are any significant interaction terms
intercept_only <- lm(Y ~ 1, data = df) 
all <- lm(Y ~.,data = df)
forward <- step(intercept_only,direction = "forward",scope = formula(lm.fit.fullint),trace = 0)
forward$anova

#Correlation matrix of the data set
cor(df)

#Calculate Mallow's CP
require(olsrr)
ols_mallows_cp(lm.fit.minus2,lm.fit.full)

#Create vector of residuals
res <- resid(lm.fit.minus2)

#produce residual vs. fitted plot
plot(fitted(lm.fit.minus2), res, main = "Residuals vs. Fitted Plot",xlab = "Fitted Values",ylab = "Residuals")

#add a horizontal line at 0 
abline(0,0)

#Adjust number of plots to display
par(mfrow=c(1,2))

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

#Create density plot of residuals
plot(density(res),main = "Density Plot of Residuals")

#create custom function to calculate the PRESS statistic
PRESS <- function(model) {
  i <- residuals(model)/(1 - lm.influence(model)$hat)
  sum(i^2)
}

#calculate PRESS for full model
PRESS(lm.fit.full)

#calculate PRESS for model 2
PRESS(lm.fit.minusint)

#calculate PRESS for final model
PRESS(lm.fit.minus2)

#Final model has smaller PRESS