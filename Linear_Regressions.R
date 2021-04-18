library(dplyr)
library(ggplot2)
library(readxl)

## 1. Basic Regression
# Import data from Sales Data tab
sales <- read_excel("C:\\hS\\GMMA\\860 Acquistion and Management of Data\\Session 3\\MMA 860 Assessing and Testing Data File v1.0.xlsx",
                    sheet = "Sales Data")

#Run linear regression model to explain order size in terms of Ad budget and distance 
reg <- lm(Order_Size ~ Ad_Budget + Distance, data  = sales)
summary(reg)
#Results of reg - P value for Distance is 0.49. Hence, the variable Distance does not belong to the model


#Run linear regression for order size with ad budget
reg1 <- lm(Order_Size ~ Ad_Budget, data  = sales)
summary(reg1)
plot(reg1)

## 2. Bimodal Error 1
# Import data from Bimodal Error 1 tab
be1 <- read_excel("C:\\hS\\GMMA\\860 Acquistion and Management of Data\\Session 3\\MMA 860 Assessing and Testing Data File v1.0.xlsx",
                    sheet = "Bimodal Error 1")

# Run linear regression for y in terms of x1 and x2

reg2 <- lm(Y ~ X1 + X2, data = be1)
summary(reg2)
plot(reg2)

#Destiny plot of errors shows 2 different normal distributions. Can be inferred that two distinct groups exist in the data
plot(density(resid(reg2)))


## 3. Bimodal Error 2
# Import data from Bimodal Error 2 tab
be2 <- read_excel("C:\\hS\\GMMA\\860 Acquistion and Management of Data\\Session 3\\MMA 860 Assessing and Testing Data File v1.0.xlsx",
                  sheet = "Bimodal Error 2")


# Run linear regression for y in terms of x1,x2 and US

reg3 <- lm(Y ~ X1 + X2 + US, data = be2)
summary(reg3)
plot(reg3)

#Destiny plot of errors shows a normal distributions. Addition of Variable 'US' resolves the error from previous case
plot(density(resid(reg3)))


## 4. Non-Normal Errors
# Import data from Nonlinear tab
nl <- read_excel("C:\\hS\\GMMA\\860 Acquistion and Management of Data\\Session 3\\MMA 860 Assessing and Testing Data File v1.0.xlsx",
                  sheet = "Nonlinear")

# Run linear regression for y in terms of x1

reg4 <- lm(Y ~ X1, data = nl)
summary(reg4)
plot(reg4)
plot(density(resid(reg4)))

reg5 <- lm(Y ~ X1 + X1Squared, data = nl)
summary(reg5)
plot(reg5)
plot(density(resid(reg5)))


## 5. Outliers
# Import data from Outliers tab
outlier <- read_excel("C:\\hS\\GMMA\\860 Acquistion and Management of Data\\Session 3\\MMA 860 Assessing and Testing Data File v1.0.xlsx",
                 sheet = "Outliers")

#Run a linear regression model on Y in terms of X1 and Outlier
reg6 <- lm(Y ~ X + Outlier, data = outlier)
summary(reg6)
plot(reg6)
# The Cook's Distance (or leverage) plot shows a single data point well outside the Cook's Distance line.

## 6. Heteroskedasticity
# Import the data from Heteroskedasticity tab.

hk <- read_excel("C:\\hS\\GMMA\\860 Acquistion and Management of Data\\Session 3\\MMA 860 Assessing and Testing Data File v1.0.xlsx",
                      sheet = "Heteroskedasticity")

#Run a linear regression model on Y in terms of X1 and X2 

reg7 <- lm(Y ~ X1 + X2, data = hk)
summary(reg7)
plot(reg7)
# A cone shape pattern in the residuals vs fitted plot implies that the data is hetereoskedastic 

## 7. Collinearity
#Import data from collinear tab

collinear <- read_excel("C:\\hS\\GMMA\\860 Acquistion and Management of Data\\Session 3\\MMA 860 Assessing and Testing Data File v1.0.xlsx",
                 sheet = "Collinear")

#Regression of Y in terms of exp and height
reg8 <- lm(Y ~ Experience + Height, data = collinear)
summary(reg8)

#Regression of Y in terms of exp and weight
reg9 <- lm(Y ~ Experience + Weight, data = collinear)
summary(reg9)

#Regression of Y in terms of exp, height and weight
reg10 <- lm(Y ~ Experience + Height + Weight, data = collinear)
summary(reg10)


plot <- ggplot(collinear, aes(x = Height, y = Weight)) + 
  geom_point()
plot

## As we can see from plot, height and weight are correlated. When we include highly correlated in a single model
## it can be hard to determine which one is actually driving change in your dependent variable. 
## When that happens, we find that variables can become ‘jointly significant’ – that is, 
## individually they appear significant,
## but don’t together because the predictive power gets split over two variables.

## 8. Diminishing Returns
#Import data from collinear tab

dr <- read_excel("C:\\hS\\GMMA\\860 Acquistion and Management of Data\\Session 3\\MMA 860 Assessing and Testing Data File v1.0.xlsx",
                        sheet = "Diminishing Returns")

reg11 <- lm(Sales ~ Price + Ad_Budget,data = dr)
plot(reg11)
plot(density(resid(reg11)))

dr$log_Ad_Budget <- log(dr$Ad_Budget)
dr

reg12 <- lm(Sales ~ Price + log_Ad_Budget,data = dr)
plot(reg12)
plot(density(resid(reg12)))
