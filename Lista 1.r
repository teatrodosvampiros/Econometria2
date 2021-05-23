library(plm)
library(lmtest)
library(foreign)
library(sandwich)
library(stargazer)
library(tidyverse)
library(haven)

#QUESTÃO 1
Y <- matrix(c(800,1160,1580,2010,1890,2600,2070,1890,1830,1740,1380,1060), ncol=1, nrow=12, byrow=F)
Y
X2 <- matrix(c(2,4,6,8,7,12,11,10,9,8,6,4), ncol=1, nrow=12, byrow=F)
X3 <- matrix(c(.8,.7,.5,.4,.2,.2,.8,.7,.6,.1,.5,.4), ncol=1, nrow=12, byrow=F)
X <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,X2,X3), ncol=3, nrow=12, byrow=F)
X

#item A:
XT <- t(X)
XTX <- XT %*% X
XTY <- XT %*% Y
YT <- t(Y)
invXTX <- solve(XTX)
betas <- invXTX %*% XTY
betas
#conferindo:
regressao1<-(lm(Y~X2+X3))
coef(regressao1)
summary(regressao1)
#a regressão é dada, portanto, por Y = 789,33 + 149,56.X2 - 419,26.X3

#item B:
XBETA <- X %*% betas
ECHAPEU <- Y - XBETA
ECHAPEU
#conferindo
cbind(resid(regressao1))

ybarra <- mean(Y)
n <- nrow(Y)
SQE <- t(betas)%*%XTY-n*(ybarra^2)
YT <- t(Y)
YTY<-YT%*%Y
SQR <- YTY-t(betas)%*%XTY
SQR
#SQR = 173444

#item C:
SQT <- YTY-n*(ybarra^2)
cbind(SQE, SQT, SQR)
R2 <- SQE/SQT
R2
#R2 = 0,9369

#item D:
ECHAPEUT <- t(ECHAPEU)
k <- 2
SIGMA2 <- as.numeric((ECHAPEUT%*%ECHAPEU)/(n-k-1))
mavcov <- SIGMA2*invXTX
mavcov

#item E:
ep <- sqrt(diag(mavcov))
ep_beta_1<-ep[1]
ep_beta_2<-ep[2]
ep_beta_3<-ep[3]
cbind(ep_beta_1, ep_beta_2, ep_beta_3)

gl <- n-k-1
abs(qt(0.025, gl))

tbeta1<-((betas[1])/ep_beta_1)
tbeta1
tbeta2<-((betas[2])/ep_beta_2)
tbeta2
tbeta3<-((betas[3])/ep_beta_3)
tbeta3
#conferindo
coeftest(regressao1)
#todas são significantes a 95%

#--------------------------------

#QUESTÃO 2
X4 <- matrix(c(1,1,1,1,1,1,0,0,0,0,0,0), ncol=1, nrow=12, byrow=F)
X <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,X2,X3,X4), ncol=4, nrow=12, byrow=F)
X

#item A:
XT <- t(X)
XTX <- XT %*% X
XTY <- XT %*% Y
YT <- t(Y)
invXTX <- solve(XTX)
betas <- invXTX %*% XTY
betas
#conferindo:
regressao1<-(lm(Y~X2+X3+X4))
summary(regressao1)
coef(regressao1)
#a regressão é dada, portanto, por Y = 536,093 + 161,866.X2 - 327,778.X3 + 238,076.D

#item B:
XBETA <- X %*% betas
ECHAPEU <- Y - XBETA
ECHAPEU
#conferindo
cbind(resid(regressao1))

ybarra <- mean(Y)
n <- nrow(Y)
SQE <- t(betas)%*%XTY-n*(ybarra^2)
YT <- t(Y)
YTY<-YT%*%Y
SQR <- YTY-t(betas)%*%XTY
SQR
#conferindo
t(ECHAPEU) %*% ECHAPEU 
#SQR = 19854,22

#item C:
SQT <- YTY-n*(ybarra^2)
cbind(SQE, SQT, SQR)
R2 <- SQE/SQT
R2
#R2 = 0,9928

#item D:
ECHAPEUT <- t(ECHAPEU)
k <- 3
SIGMA2 <- as.numeric((ECHAPEUT%*%ECHAPEU)/(n-k-1))
mavcov <- SIGMA2*invXTX
mavcov

#item E:
ep <- sqrt(diag(mavcov))
ep_beta_1<-ep[1]
ep_beta_2<-ep[2]
ep_beta_3<-ep[3]
ep_beta_4<-ep[4]
cbind(ep_beta_1, ep_beta_2, ep_beta_3, ep_beta_4)

gl <- n-k-1
abs(qt(0.025, gl))

tbeta1<-((betas[1])/ep_beta_1)
tbeta1
tbeta2<-((betas[2])/ep_beta_2)
tbeta2
tbeta3<-((betas[3])/ep_beta_3)
tbeta3
tbeta4<-((betas[4])/ep_beta_4)
tbeta4
#conferindo
coeftest(regressao1)
#todas são significantes a 95%


#--------------------------------

#QUESTÃO 6

Y1 <- matrix(c(8,9,8,4,7,1,5,7,5,4,5,10,5,10,2), ncol=1, nrow=15, byrow=F)
Y1
Y2 <- matrix(c(6,5,4,3,2,1,4,6,4,5,2,3,4,5,1), ncol=1, nrow=15, byrow=F)
Y2
D  <- matrix(c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0), ncol=1, nrow=15, byrow=F)
D

#item A:
ATE <- mean(Y1)-mean(Y2)
ATE

#item B: (D=1)
ATT1 <- matrix(c(9,4,1,7,4,10,10), ncol=1, nrow=7, byrow=F)
ATT2 <- matrix(c(5,3,1,6,5,3,5), ncol=1, nrow=7, byrow=F)
ATT <- mean(ATT1) - mean(ATT2)
ATT

#item C: (D=0)
ATU1 <- matrix(c(8,8,7,5,5,5,5,2), ncol=1, nrow=8, byrow=F)
ATU2 <- matrix(c(6,4,2,4,4,2,4,1), ncol=1, nrow=8, byrow=F)
ATU <- mean(ATU1) - mean(ATU2)
ATU

#Como já possuímos os estados factuais e contrafactuais, podemos calcular o ATE (item A).
#Assim, o medicamento oferece na média 2,333 anos de vida a mais que não tomá-lo