library(stringr) # load this library to use str_c function
good <- read.csv(str_c(getwd(), "/good.csv"),header = T)
good
sum(is.na(good$mathraw97))
mean(good$mathraw97, na.rm = T)
median(good$mathraw97, na.rm = T)
sd(good$mathraw97, na.rm = T)
min(good$mathraw97, na.rm = T)
max(good$mathraw97, na.rm = T)
##### linear regression
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
install.packages("car")
library(car)
good2 <- c("AGE97","WICpreg","mathraw97","faminc97","bthwht","HOME97")
goody <- good[good2]
good1 <- na.omit(good[good2])
matrix <- cor(good1[,c("AGE97","WICpreg","mathraw97","faminc97","bthwht","HOME97")])
install.packages("corrplot")
library("corrplot")
corrplot(matrix, method="circle")

#####scatterplot
library(car)
scatterplotMatrix(good1[,c("mathraw97", "AGE97","logfaminc")],
                  cex = .5,
                  pch = 16,
                  col = rgb(0,0,0,1/32),
                  diagonal=list(method ="histogram", 
                                breaks = 20),
                  cex.labels = 0.5,
                  regLine=list(method = lm,
                               lty = 1,
                               lwd = 1,
                               col = 1),
                  smooth = list(method = "loessLine",
                                lty.smooth = 2,
                                lwd.smooth = 1,
                                col.smooth = 2,
                                lty.spread = 3,
                                lwd.spread = 1,
                                col.spread = 2))
 #run our regression model
reg1 <- lm(mathraw97 ~ AGE97  + faminc97 + bthwht +WICpreg , data = goody)

summary(reg1)
###### hist of regression
good.res <- resid(reg1)
########outlier
row <- good1[1416,]
row
hist(good.res, breaks = 50)
plot(reg1, main = "Cook's distance", which = 5)
summary(reg2)
cor.test(good1$HOME97,good1$mathraw97, use = "complete.obs")
#######corrected model
good1$logfaminc <- pmax(0, log1p(good1$faminc97))
reg2 <- lm(mathraw97 ~ AGE97 + logfaminc + bthwht  + WICpreg, data = good1)
####residue vs fitted
plot(reg1, which = 1)
plot(reg2, which =1)
scatterplot(fitted(reg1), resid(reg1))
scatterplot(fitted(reg2), resid(reg2))
####hist
good.res <- resid(reg2)
hist(good.res,breaks=50)
summary(reg2)
plot(reg2, which=5)
row <- goody[715,]
row
####outlier
good2$rstudent <- rstudent(reg1)
plot(good2$rstudent,
     xlab = "Index",
     ylab = "Standardized residuals",
     pch = 19)
abline(h=0, col = "red")
abline(h=2, col = "blue")
abline(h=-2, col = "blue")
abline(h=3, col = "green")
abline(h=-3, col = "green")
good1$leverage <- hatvalues(reg1)
plot(good1$leverage, 
     xlab = "Index",
     ylab = "Leverage",
     pch = 19)
abline(h=.00588, col="blue")
logfaminc 
good1$AGE97c <- good1$AGE97 - mean(good1$AGE97)
good1$cooksd <- cooks.distance(reg1)
reg2 <- lm(mathraw97 ~ logfaminc + AGE97 +  bthwht+WICpreg, 
           data = good1[good1$cooksd <= (4/2042),])
summary(reg2)
AIC(reg2)
hist(good1$cooksd[good1$cooksd > 4/2042])
round(quantile(good1$cooksd[good1$cooksd > 4/2042], 
               probs = seq(0, 1, 0.05)), 4)
reg3 <- lm(mathraw97 ~ logfaminc + AGE97 + bthwht+WICpreg, 
           data = good1[good1$cooksd < 0.0069,])
summary(reg3)
######reg4

reg4 <- lm(mathraw97 ~ logfaminc + AGE97 + bthwht+WICpreg + HOME97, 
           data = good1[good1$cooksd < 0.0069,])
summary(reg4)
AIC(reg4)
AIC(reg3)
plot(reg4, which = 5)
nrow(good1[good1$cooksd < 0.0069,])
scatterplotMatrix(good1[,c("mathraw97", "AGE97c","loginc97","WICpreg","bthwht","HOME97")],
                  cex = .5,
                  pch = 16,
                  col = rgb(0,0,0,1/32),
                  diagonal=list(method ="histogram", 
                                breaks = 20),
                  cex.labels = 0.5,
                  regLine=list(method = lm,
                               lty = 1,
                               lwd = 1,
                               col = 1),
                  smooth = list(method = "loessLine",
                                lty.smooth = 2,
                                lwd.smooth = 1,
                                col.smooth = 2,
                                lty.spread = 3,
                                lwd.spread = 1,
                                col.spread = 2))
