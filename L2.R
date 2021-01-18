## ----setup, include=TRUE,echo=FALSE----------------------------------------
suppressPackageStartupMessages(library(knitr))
knitr::opts_chunk$set(echo = FALSE, message=FALSE,warning = FALSE, error = FALSE)
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(reticulate))
#reticulate::use_python("/usr/bin/python3",required=TRUE)
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(magick))
suppressPackageStartupMessages(library(pheatmap))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(bestglm))
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(nortest))


## --------------------------------------------------------------------------
#http://zevross.com/blog/2017/06/19/tips-and-tricks-for-working-with-images-and-figures-in-r-markdown-documents/
# options in r chunk settings
# out.width="100%"
# dpi=72

# ny figur? office hours + mer om shrinkage + optimism Cov(yhat_i,y_i)
include_graphics("../overviewv2.png")


## ----fig.cap="Figure from An Introduction to Statistical Learning, with applications in R (Springer, 2013) with permission from the authors: G. James, D. Witten, T. Hastie and R. Tibshirani.",out.width="50%"----
#http://zevross.com/blog/2017/06/19/tips-and-tricks-for-working-with-images-and-figures-in-r-markdown-documents/
# options in r chunk settings
# out.width="100%"
# dpi=72

# ny figur? office hours + mer om shrinkage + optimism Cov(yhat_i,y_i)
include_graphics("./ILS34.png")


## ----echo=TRUE-------------------------------------------------------------
ds <- read.table("./sniffer.dat",header=TRUE)
x <- apply(ds[,-5],2,scale)
y <- ds[,5]-mean(ds[,5])
print(dim(x))
dss=data.frame(y,x)
ggpairs(dss)


## ----echo=TRUE-------------------------------------------------------------
cov(dss)


## ----echo=TRUE-------------------------------------------------------------
full <- lm(y~.,dss)
summary(full)
confint(full)
ggplot(full, aes(.fitted, .stdresid)) + geom_point(pch = 21) + geom_hline(yintercept = 0, 
    linetype = "dashed") + geom_smooth(se = FALSE, col = "red", size = 0.5, 
    method = "loess") + labs(x = "Fitted values", y = "Standardized residuals", 
    title = "Fitted values vs standardized residuals", subtitle = deparse(full$call))
ggplot(full, aes(sample = .stdresid)) + stat_qq(pch = 19) + geom_abline(intercept = 0, 
    slope = 1, linetype = "dotted") + labs(x = "Theoretical quantiles", 
    y = "Standardized residuals", title = "Normal Q-Q", subtitle = deparse(full$call))
ad.test(rstudent(full))


## ----echo=TRUE-------------------------------------------------------------
bests <- regsubsets(x,y)
sumbests <- summary(bests)
print(sumbests)
which.min(sumbests$cp) 


## ----echo=TRUE-------------------------------------------------------------
red <- lm(y~GasTemp+TankPres+GasPres,data=dss)
summary(red)
confint(red)


## ----fig.cap="Figure from An Introduction to Statistical Learning, with applications in R (Springer, 2013) with permission from the authors: G. James, D. Witten, T. Hastie and R. Tibshirani.",out.width="40%"----
#http://zevross.com/blog/2017/06/19/tips-and-tricks-for-working-with-images-and-figures-in-r-markdown-documents/
# options in r chunk settings
# out.width="100%"
# dpi=72

# ny figur? office hours + mer om shrinkage + optimism Cov(yhat_i,y_i)
include_graphics("./ILS67ridge.png")


## ----echo=TRUE-------------------------------------------------------------
start=glmnet(x=x,y=y,alpha=0)
autolambda=start$lambda # automatic choice of lambda had smallest lambda 0.96 - but I added more small values to also be able to see that LS-solution is for lambda=0
newlambda=c(autolambda,0.5,0.3,0.2,0.1)
fit.ridge=glmnet(x,y,alpha=0,lambda=newlambda)
plot(fit.ridge,xvar="lambda",label=TRUE)
#plot(fit.ridge,xvar="norm",label=TRUE)


## ----echo=TRUE-------------------------------------------------------------
cv.ridge=cv.glmnet(x,y,alpha=0,lambda=newlambda)
print(paste("The lamda giving the smallest CV error",cv.ridge$lambda.min))
print(paste("The 1sd err method lambda",cv.ridge$lambda.1se))

plot(cv.ridge)

# use 1sd error rule default
plot(fit.ridge,xvar="lambda",label=TRUE);
abline(v=log(cv.ridge$lambda.1se));

coef(cv.ridge)
full$coeff
red$coeff


## ----fig.cap="Figure from An Introduction to Statistical Learning, with applications in R (Springer, 2013) with permission from the authors: G. James, D. Witten, T. Hastie and R. Tibshirani.",out.width="50%"----
#http://zevross.com/blog/2017/06/19/tips-and-tricks-for-working-with-images-and-figures-in-r-markdown-documents/
# options in r chunk settings
# out.width="100%"
# dpi=72

# ny figur? office hours + mer om shrinkage + optimism Cov(yhat_i,y_i)
include_graphics("./ILS67lasso.png")


## ----fig.cap="Figure from An Introduction to Statistical Learning, with applications in R (Springer, 2013) with permission from the authors: G. James, D. Witten, T. Hastie and R. Tibshirani.",out.width="50%"----
#http://zevross.com/blog/2017/06/19/tips-and-tricks-for-working-with-images-and-figures-in-r-markdown-documents/
# options in r chunk settings
# out.width="100%"
# dpi=72

# ny figur? office hours + mer om shrinkage + optimism Cov(yhat_i,y_i)
include_graphics("./ILS610.png")


## --------------------------------------------------------------------------
# Now we fit a lasso model; for this we use the default `alpha=1`
fit.lasso=glmnet(x,y)#,lambda=newlambda)
plot(fit.lasso,xvar="lambda",label=TRUE)

cv.lasso=cv.glmnet(x,y)
#which.min(cv.lasso$cvm)

plot(cv.lasso)
plot(fit.lasso,xvar="lambda",label=TRUE);
abline(v=log(cv.lasso$lambda.1se))

coef(cv.lasso)


## --------------------------------------------------------------------------
include_graphics("logo.png")

