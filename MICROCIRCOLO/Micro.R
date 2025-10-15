library(readxl)
data <- read_excel("full.xlsx")
dataset <- data[, c(1,2,3,6,7,12,84,85,90)]

#rimozione outlier
dataset <- dataset[-which(dataset$`Dose cum` < 2), ]
dim(dataset)


# Flow --------------------------------------------------------------------

dataF <- dataset[, c(1,2,3,4,7)]
colSums(is.na(dataF))
dataF <- dataF[-which(is.na(dataF$Flow)), ]
dataF$var_flow <- ((dataF$Flow / dataF$B_Flow) * 100) - 100 #var percentuale
summary(dataF)

m = lm(dataF$var_flow ~ dataF$`Dose cum`+ dataF$B_Flow , data=dataF)
summary(m)

funzione_non_lineare <- function(x, a, b) {
  a * (x^b) 
}

# Adattamento del modello non lineare ai dati
modello_non_lineare <- nls(dataF$var_flow ~ funzione_non_lineare(dataF$`Dose cum`, a, b), data = dataF, start = list(a = 1, b = 1), nls.control(maxiter=100))

summary(modello_non_lineare)
atot <- coef(modello_non_lineare)[1]
atot
btot <- coef(modello_non_lineare)[2]
btot

#residui del modello
residui <- resid(modello_non_lineare)

#Mean Squared Error
mse <- mean(residui^2)

# Stampa il MSE
print(mse)
sqrt(mse)

par(mfrow=cbind(1,1))
plot(dataF$`Dose cum`, dataF$var_flow, xlab='Dose Cumulata', ylab='Var Flow')
x <- seq(0,65)
lines(x, funzione_non_lineare(x,atot,btot), col='orange', lwd = 3)

#tento con intercetta e guardo mse

funzione_non_lineare2 <- function(x, a, b, c) {
  a * (x^b) + c
}

modello_non_lineare2 <- nls(dataF$var_flow ~ funzione_non_lineare2(dataF$`Dose cum`, a, b, c), data = dataF, start = list(a = 1, b = 1, c = 0), nls.control(minFactor=1e-010, maxiter=1000))

#non va :((     

#USIAMO INFO B_FLOW

color.position <- ifelse(dataF$B_Flow < 370.3, 'black', 
                         ifelse(dataF$B_Flow >= 370.3 & dataF$B_Flow < 508.5, 'yellow',
                                ifelse(dataF$B_Flow >= 508.5 & dataF$B_Flow < 783.9, 'blue', 
                                       ifelse(dataF$B_Flow >= 783.9, 'red', NA))))

layout(cbind(c(1, 1), c(2, 3)), widths=c(2, 1), heights=c(1, 1))
plot(dataF$`Dose cum`, dataF$var_flow, asp=1, col=color.position, pch=16, xlim = c(0,66))
hist(dataF$`Dose cum`, prob=T)
hist(dataF$var_flow, prob=T)

#ora divido per blocchi
dataF_black <- dataF[dataF$B_Flow < 370.3, ]
dataF_yellow <- dataF[dataF$B_Flow > 370.3 & dataF$B_Flow< 508.5, ]
dataF_blue <- dataF[dataF$B_Flow > 508.5 & dataF$B_Flow < 783.9, ]
dataF_red <- dataF[dataF$B_Flow > 783.9, ]

par(mfrow=cbind(2,2))

#RED
modello_red <- nls(dataF_red$var_flow ~ funzione_non_lineare(dataF_red$`Dose cum`, a, b), data = dataF_red, start = list(a = 1, b = 1))
summary(modello_red)
a <- coef(modello_red)[1]
a
b <- coef(modello_red)[2]
b
residui <- resid(modello_red)
mse <- mean(residui^2)
print(mse)
sqrt(mse)
plot(dataF_red$`Dose cum`, dataF_red$var_flow, col='black', xlab='Dose Cumulata', ylab='Var.Flow')
x <- seq(0,66)
lines(x, funzione_non_lineare(x,a,b), col='red', lwd = 3)
lines(x, funzione_non_lineare(x,atot,btot), col='orange', lwd = 3)

#BLUE
modello_blue <- nls(dataF_blue$var_flow ~ funzione_non_lineare(dataF_blue$`Dose cum`, a, b), data = dataF_blue, start = list(a = a, b = b))
summary(modello_blue)
a <- coef(modello_blue)[1]
a
b <- coef(modello_blue)[2]
b
residui <- resid(modello_blue)
mse <- mean(residui^2)
print(mse)
sqrt(mse)
plot(dataF_blue$`Dose cum`, dataF_blue$var_flow, col='black', xlab='Dose Cumulata', ylab='Var.Flow')
x <- seq(0,66)
lines(x, funzione_non_lineare(x,a,b), col='blue', lwd = 3)
lines(x, funzione_non_lineare(x,atot,btot), col='orange', lwd = 3)

#YELLOW
modello_yellow <- nls(dataF_yellow$var_flow ~ funzione_non_lineare(dataF_yellow$`Dose cum`, a, b), data = dataF_yellow, start = list(a = a, b = b))
summary(modello_yellow)
a <- coef(modello_yellow)[1]
a
b <- coef(modello_yellow)[2]
b
residui <- resid(modello_yellow)
mse <- mean(residui^2)
print(mse)
sqrt(mse)
plot(dataF_yellow$`Dose cum`, dataF_yellow$var_flow, col='black', xlab='Dose Cumulata', ylab='Var.Flow')
x <- seq(0,66)
lines(x, funzione_non_lineare(x,a,b), col='yellow', lwd = 3)
lines(x, funzione_non_lineare(x,atot,btot), col='orange', lwd = 3)

#BLACK 
modello_black <- nls(dataF_black$var_flow ~ funzione_non_lineare(dataF_black$`Dose cum`, a, b), data = dataF_black, start = list(a = a, b = b))
summary(modello_black)
a <- coef(modello_black)[1]
a
b <- coef(modello_black)[2]
b
residui <- resid(modello_black)
mse <- mean(residui^2)
print(mse)
sqrt(mse)
plot(dataF_black$`Dose cum`, dataF_black$var_flow, col='black', xlab='Dose Cumulata', ylab='Var.Flow')
x <- seq(0,66)
lines(x, funzione_non_lineare(x,a,b), col='black', lwd = 3)
lines(x, funzione_non_lineare(x,atot,btot), col='orange', lwd = 3)

#DOSE e B_FLOW

funzione2 <- function(x1, x2, a, b, c) {
  a*(x1^b)+c*x2
}

nlm <- nls(dataF$var_flow ~ funzione2(dataF$`Dose cum`, dataF$B_Flow, a, b, c), 
                           data = dataF, start = list(a = 1, b = 1, c = 0), nls.control(maxiter=5000))

summary(nlm)
atot <- coef(nlm)[1]
atot
btot <- coef(nlm)[2]
btot
ctot <- coef(nlm)[3]
ctot

residui <- resid(nlm)
mse1 <- mean(residui^2)
print(mse1)
sqrt(mse1) #migliora rispetto a usare solo la dose

#plot
x1F <- seq(2.64, 65, length.out=100)
x2F <- seq(107, 1514, length.out=100)
z <- matrix(NA, nrow = length(x1F), ncol=length(x2F))
for(i in 1:length(x1F)) {
  for(j in 1:length(x2F)) {
    z[i, j] <- funzione2(x1F[i], x2F[j], atot, btot, ctot)
  }
}

par(mfrow=c(1, 2))
#image(x1, x2, z)
#contour(x1, x2, z, add=T)
persp(x1F, x2F, z, col='green', xlab='Dose', ylab='', zlab='Var. Flow', ticktype = "detailed",
       nticks = 4)
persp(x1F, x2F, z, col='green', theta=90, phi=0, shade=.05, xlab='', ylab='B_Flow',
      zlab='Var. Flow', ticktype = "detailed", nticks = 4)

#più bello
library(rgl)
options(rgl.printRglwidget = TRUE)
persp3d(x1F, x2F, z, col='green', alpha=1, xlab='Dose', ylab='B_Flow', zlab='Var. Flow', main='3D Plot')

#######LMM

###########################LMM 
y <- dataF$var_flow
x1 <- atot*(dataF$`Dose cum`)^btot
x2 <- ctot*(dataF$B_Flow)
eps <- dataF$Name

library(lme4)
library(nlme) 

mod <- lmer(y ~ x1 + x2  + (1|eps))

summary(mod)
residui <- resid(mod)
mse <- mean(residui^2)
sqrt(mse)    #migliora rispetto NLS

#Devo controllare omoschedasticità
library(ggplot2)

ggplot(data.frame(predetti = predict(mod), residui = residui), aes(x = predetti, y = residui)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Valori Predetti", y = "Residui") +
  ggtitle("Grafico dei Residui per Valutare l'Omoschedasticità")

######PROVO ALTRI MODI
x1 <- (dataF$`Dose cum`)
x2 <- dataF$B_Flow
mod2 <- lmer(y ~ x1 + x2 +  (1|eps))
summary(mod2)
residui2 <- resid(mod2)
mse2 <- mean(residui2^2)
sqrt(mse2)
AIC(mod2)

x1 <- log(dataF$`Dose cum`)
mod3 <- lmer(y ~ x1 + x2  +  (1|eps))

summary(mod3)
residui3 <- resid(mod3)
mse3 <- mean(residui3^2)
sqrt(mse3)

x1 <- (dataF$`Dose cum`)^(2/50)
mod4 <- lmer(y ~ x1 + x2  +  (1|eps))

summary(mod4)
residui4 <- resid(mod4)
mse4 <- mean(residui4^2)
sqrt(mse4)

AIC_model1 <- AIC(mod)
AIC_model2 <- AIC(mod2)
AIC_model3 <- AIC(mod3)
AIC_model4 <- AIC(mod4)

# Akaike tiene conto della massima vcerosimiglianza e del numero dei parametri
#−2×log-verosimiglianza massima+2×numero di parametri del modello
print(AIC_model1)
print(AIC_model2)
print(AIC_model3)
print(AIC_model4) 

#migliore modelllo 1 ?

summary(mod)#random effects: variance of the b_i (sigma_b ^2) and variance of the residuals

confint(mod,oldNames=TRUE)

## Var-Cov matrix of fixed-effects
vcovb <- vcov(mod) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(mod)), 3)
rownames(corb) <- nms
corb

library(nlmeU)
library(insight)
library(corrplot)
library(lattice)
library(plot.matrix)

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(mod), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(mod))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(mod))
sigma2_b

## Let's compute the conditional and marginal var-cov matrix of Y
sgma <- summary(mod)$sigma

A <- getME(mod, "A") # A  --> N x n, A represents the D (not italic)
I.n <- Diagonal(ncol(A)) # IN  --> n x n

## the conditional variance-covariance matrix of Y (diagonal matrix)
SigmaErr = sgma^2 * (I.n)
SigmaErr[29:34, 29:34]  ## visualization of individual HN029
# Conditioned to the random effects b_i, we observe the var-cov of the errors
# that are independent and homoscedastic



## we visualize the first 20 rows/columns of the matrix
plot(as.matrix(SigmaErr[1:20,1:20]), main = 'Conditional estimated Var-Cov matrix of Y')

## the marginal variance-covariance matrix of Y (block-diagonal matrix)
V <- sgma^2 * (I.n + crossprod(A)) # V = s^2*(I_N+A*A) --> s^2*(I_N) is the error part, s^2*(A*A) is the random effect part
V[29:34, 29:34]  #-> V is a block-diagional matrix, the marginal var-cov matrix

# visualization of the first 20 rows/columns
plot(as.matrix(V[1:20,1:20]), main = 'Marginal estimated Var-Cov matrix of Y')

# Another way to interpret the variance output is to note percentage of the subject variance out 
# of the total, i.e. the Percentage of Variance explained by the Random Effect (PVRE).
# This is also called the intraclass correlation (ICC), because it is also an estimate of the within 
# cluster correlation.

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE #5%

## visualization of the random intercepts with their 95% confidence intervals
# Random effects: b_0i for i=1,...,234
ranef(mod, condVar=T) #stime puntuali dei b_i
dotplot(ranef(mod, condVar=T)) 

# The dotplot shows the point estimate and 95% PREDICTION intervals for this estimate for the random effects, 
# ordering them and highlighting which are significantly different from the mean (0)


# Prediction

# Let's now examine standard predictions vs. subject-specific predictions.
# As with most R models, we can use the predict function on the model object.

# Prediction from classical linear model
x1 <- atot*(dataF$`Dose cum`)^btot
x2 <- ctot*(dataF$B_Flow)
y <- dataF$var_flow
lm1 <- lm(y ~ x1 + x2) 
summary(lm1)

predict_lm <- predict(lm1)
head(predict_lm)

# Prediction from mixed model on the training set:
# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(mod, re.form=NA) 
head(predict_no_re) # (almost) same predictions of classical linear model
# 2) With random effects
predict_re <- predict(mod)
head(predict_re)#quite different from the classical ones




# Density -----------------------------------------------------------------

dataD <- dataset[, c(1,2,3,5,8)]
colSums(is.na(dataD))
dataD <- dataD[-which(is.na(dataD$`Total density`)), ]
dataD <- dataD[-which(dataD$`Total density` == 0), ]
dataD$var_density <- ((dataD$`Total density`/dataD$`B_Total density`)*100)-100
summary(dataD)

funzione_non_lineare <- function(x, a, b) {
  a * (x^b) 
}

# Adattamento del modello non lineare ai dati
modello_non_lineare <- nls(dataD$var_density ~ funzione_non_lineare(dataD$`Dose cum`, a, b),
                           data = dataD, start = list(a = 1, b = 1), 
                           nls.control(minFactor=1e-10, maxiter=100))

summary(modello_non_lineare)
atot <- coef(modello_non_lineare)[1]
atot
btot <- coef(modello_non_lineare)[2]
btot

#residui del modello
residui <- resid(modello_non_lineare)

#Mean Squared Error
mse <- mean(residui^2)

# Stampa il MSE
print(mse)
sqrt(mse)

par(mfrow=cbind(1,1))
plot(dataD$`Dose cum`, dataD$var_density, xlab='Dose Cumulata', ylab='Var Density')
x <- seq(0,65)
lines(x, funzione_non_lineare(x,atot,btot), col='orange', lwd = 3)

#USIAMO INFO B_TOTAL_DENS
summary(dataD)
color.position <- ifelse(dataD$`B_Total density` < 189.5, 'black', 
                         ifelse(dataD$`B_Total density` >= 189.5 & dataD$`B_Total density` < 251.9, 'yellow',
                                ifelse(dataD$`B_Total density` >= 251.9 & dataD$`B_Total density` < 375.7, 'blue', 
                                       ifelse(dataD$`B_Total density` >= 375.7, 'red', NA))))

layout(cbind(c(1, 1), c(2, 3)), widths=c(2, 1), heights=c(1, 1))
plot(dataD$`Dose cum`, dataD$var_density, asp=1, col=color.position, pch=16, xlim = c(0,66))
hist(dataD$`Dose cum`, prob=T)
hist(dataD$var_density, prob=T)

#DOSE e B_TOTAL_DENS

funzione2 <- function(x1, x2, a, b, c) {
  a*(x1^b) + c*x2
}

nlm <- nls(dataD$var_density ~ funzione2(dataD$`Dose cum`, dataD$`B_Total density`, a, b, c), data=dataD,
          start = list(a = atot, b = btot, c = 0), nls.control(maxiter=5000))

summary(nlm)
atot <- coef(nlm)[1]
atot
btot <- coef(nlm)[2]
btot
ctot <- coef(nlm)[3]
ctot

residui <- resid(nlm)
mse1 <- mean(residui^2)
print(mse1)
sqrt(mse1) 

#plot
x1D <- seq(2, 66, length.out=100)
x2D <- seq(74, 731, length.out=100)
z <- matrix(NA, nrow = length(x1D), ncol=length(x2D))
for(i in 1:length(x1D)) {
  for(j in 1:length(x2D)) {
    z[i, j] <- funzione2(x1D[i], x2D[j], atot, btot, ctot)
  }
}

par(mfrow=c(1, 2))
# image(x1, x2, z)
# contour(x1, x2, z, add=T)
persp(x1D, x2D, z, col='green', xlab='Dose', ylab='',
      zlab='Var. Density',  ticktype = "detailed", nticks = 3)
persp(x1D, x2D, z, col='green', theta=90, phi=0, shade=.05, xlab='', ylab='B_Density',
      zlab='Var. Density',  ticktype = "detailed", nticks = 3)

#più bello
library(rgl)
options(rgl.printRglwidget = TRUE)
persp3d(x1, x2, z, col='red', alpha=1, xlab='Dose', ylab='B_Total_Density', zlab='Var. Density', main='3D Plot', xlim=c(2,66), ylim=c(74,731))

#######LMM

###########################LMM 
y <- dataD$var_density
x1 <- atot*(dataD$`Dose cum`)^btot
x2 <- ctot*(dataD$`B_Total density`)
eps <- dataD$Name

library(lme4)
library(nlme) 

mod <- lmer(y ~ x1 + x2  + (1|eps))

summary(mod)
residui <- resid(mod)
mse <- mean(residui^2)
sqrt(mse)    #migliora rispetto NLS

#Devo controllare omoschedasticità
library(ggplot2)

ggplot(data.frame(predetti = predict(mod), residui = residui), aes(x = predetti, y = residui)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Valori Predetti", y = "Residui") +
  ggtitle("Grafico dei Residui per Valutare l'Omoschedasticità")

######PROVO ALTRI MODI
x1 <- dataD$`Dose cum`
x2 <- dataD$`B_Total density`
mod2 <- lmer(y ~ x1 + x2 +  (1|eps))
summary(mod2)
residui2 <- resid(mod2)
mse2 <- mean(residui2^2)
sqrt(mse2)

AIC(mod2)
AIC(mod)

#1 migliore sia per AIC che per mse

summary(mod)#random effects: variance of the b_i (sigma_b ^2) and variance of the residuals

confint(mod,oldNames=TRUE)

## Var-Cov matrix of fixed-effects
vcovb <- vcov(mod) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(mod)), 3)
rownames(corb) <- nms
corb

library(nlmeU)
library(insight)
library(corrplot)
library(lattice)
library(plot.matrix)

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(mod), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(mod))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(mod))
sigma2_b

## Let's compute the conditional and marginal var-cov matrix of Y
sgma <- summary(mod)$sigma

A <- getME(mod, "A") # A  --> N x n, A represents the D (not italic)
I.n <- Diagonal(ncol(A)) # IN  --> n x n

## the conditional variance-covariance matrix of Y (diagonal matrix)
SigmaErr = sgma^2 * (I.n)
SigmaErr[30:35, 30:35]  ## visualization of individual HN029
# Conditioned to the random effects b_i, we observe the var-cov of the errors
# that are independent and homoscedastic



## we visualize the first 20 rows/columns of the matrix
plot(as.matrix(SigmaErr[1:20,1:20]), main = 'Conditional estimated Var-Cov matrix of Y')

## the marginal variance-covariance matrix of Y (block-diagonal matrix)
V <- sgma^2 * (I.n + crossprod(A)) # V = s^2*(I_N+A*A) --> s^2*(I_N) is the error part, s^2*(A*A) is the random effect part
V[30:35, 30:35]  #-> V is a block-diagional matrix, the marginal var-cov matrix

# visualization of the first 20 rows/columns
plot(as.matrix(V[1:20,1:20]), main = 'Marginal estimated Var-Cov matrix of Y')

# Another way to interpret the variance output is to note percentage of the subject variance out 
# of the total, i.e. the Percentage of Variance explained by the Random Effect (PVRE).
# This is also called the intraclass correlation (ICC), because it is also an estimate of the within 
# cluster correlation.

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE #51% very high

## visualization of the random intercepts with their 95% confidence intervals
# Random effects: b_0i for i=1,...,234
ranef(mod, condVar=T) #stime puntuali dei b_i
dotplot(ranef(mod, condVar=T)) 

# The dotplot shows the point estimate and 95% PREDICTION intervals for this estimate for the random effects, 
# ordering them and highlighting which are significantly different from the mean (0)


# Prediction

# Let's now examine standard predictions vs. subject-specific predictions.
# As with most R models, we can use the predict function on the model object.

# Prediction from classical linear model
x1 <- atot*(dataD$`Dose cum`)^btot
x2 <- ctot*(dataD$`B_Total density`)
y <- dataD$var_density
lm1 <- lm(y ~ x1 + x2) 
summary(lm1)

predict_lm <- predict(lm1)
head(predict_lm)

# Prediction from mixed model on the training set:
# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(mod, re.form=NA) 
head(predict_no_re) # (almost) same predictions of classical linear model
# 2) With random effects
predict_re <- predict(mod)
head(predict_re)#quite different from the classical ones


# Pbr ---------------------------------------------------------------------


dataP <- dataset[, c(1,2,3,6,9)]
colSums(is.na(dataP))
dataP <- dataP[-which(is.na(dataP$`PBR4-25`)), ]
dataP$var_pbr <- ((dataP$`PBR4-25`/dataP$`B_PBR4-25`)*100)-100
summary(dataP)

funzione_non_lineare <- function(x, a, b) {
  a * (x^b) 
}

# Adattamento del modello non lineare ai dati
modello_non_lineare <- nls(dataP$var_pbr ~ funzione_non_lineare(dataP$`Dose cum`, a, b), data = dataP, start = list(a = 1, b = 1), nls.control(minFactor=1e-10, maxiter=100))

summary(modello_non_lineare)
atot <- coef(modello_non_lineare)[1]
atot
btot <- coef(modello_non_lineare)[2]
btot

#residui del modello
residui <- resid(modello_non_lineare)

#Mean Squared Error
mse <- mean(residui^2)

# Stampa il MSE
print(mse)
sqrt(mse)

par(mfrow=cbind(1,1))
plot(dataP$`Dose cum`, dataP$var_pbr, xlab='Dose Cumulata', ylab='Var PBR')
x <- seq(0,65)
lines(x, funzione_non_lineare(x,atot,btot), col='orange', lwd = 3)

#USIAMO INFO B_PBR
summary(dataP)
color.position <- ifelse(dataP$`B_PBR4-25` < 2.08, 'black', 
                         ifelse(dataP$`B_PBR4-25` >= 2.08 & dataP$`B_PBR4-25` < 2.184, 'yellow',
                                ifelse(dataP$`B_PBR4-25` >= 2.184 & dataP$`B_PBR4-25` < 2.31, 'blue', 
                                       ifelse(dataP$`B_PBR4-25` >= 2.31, 'red', NA))))

layout(cbind(c(1, 1), c(2, 3)), widths=c(2, 1), heights=c(1, 1))
plot(dataP$`Dose cum`, dataP$var_pbr, asp=1, col=color.position, pch=16, xlim = c(0,66))
hist(dataP$`Dose cum`, prob=T)
hist(dataP$var_pbr, prob=T)

#DOSE e B_PBR

funzione2 <- function(x1, x2, a, b, c) {
  a*(x1^b) + c*x2
}

nlm <- nls(dataP$var_pbr ~ funzione2(dataP$`Dose cum`, dataP$`B_PBR4-25`, a, b, c), data=dataP,
           start = list(a = 1, b = 1, c = 1), nls.control(maxiter=5000))

summary(nlm)
atot <- coef(nlm)[1]
atot
btot <- coef(nlm)[2]
btot
ctot <- coef(nlm)[3]
ctot

residui <- resid(nlm)
mse1 <- mean(residui^2)
print(mse1)
sqrt(mse1) 

#plot
x1P <- seq(2, 66, length.out=100)
x2P <- seq(1.52, 2.61, length.out=100)
z <- matrix(NA, nrow = length(x1P), ncol=length(x2P))
for(i in 1:length(x1P)) {
  for(j in 1:length(x2P)) {
    z[i, j] <- funzione2(x1P[i], x2P[j], atot, btot, ctot)
  }
}

par(mfrow=c(1, 2))
# image(x1, x2, z)
# contour(x1, x2, z, add=T)
persp(x1P, x2P, z, col='green', xlab='Dose', ylab='', zlab='Var. PBR',
      ticktype = "detailed", nticks = 3)
persp(x1P, x2P, z, col='green', theta=90, phi=0, shade=.05, xlab='',
      ylab='B_PBR', zlab='Var. PBR',ticktype = "detailed", nticks = 3)

#purtroppo con rgl non viene


#######LMM

###########################LMM 
y <- dataP$var_pbr
x1 <- atot*(dataP$`Dose cum`)^btot
x2 <- ctot*(dataP$`B_PBR4-25`)
eps <- dataP$Name

library(lme4)
library(nlme) 

mod <- lmer(y ~ x1 + x2  + (1|eps))

summary(mod)
residui <- resid(mod)
mse <- mean(residui^2)
sqrt(mse)    #migliora rispetto NLS

#Devo controllare omoschedasticità
library(ggplot2)

ggplot(data.frame(predetti = predict(mod), residui = residui), aes(x = predetti, y = residui)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Valori Predetti", y = "Residui") +
  ggtitle("Grafico dei Residui per Valutare l'Omoschedasticità")

######PROVO ALTRI MODI
x1 <- dataP$`Dose cum`
x2 <- dataP$`B_PBR4-25`
mod2 <- lmer(y ~ x1 + x2 +  (1|eps))
summary(mod2)
residui2 <- resid(mod2)
mse2 <- mean(residui2^2)
sqrt(mse2)

AIC(mod2)
AIC(mod)

#2 migliore sia per AIC , 1 migliore per mse

summary(mod)#random effects: variance of the b_i (sigma_b ^2) and variance of the residuals

confint(mod,oldNames=TRUE)

## Var-Cov matrix of fixed-effects
vcovb <- vcov(mod) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(mod)), 3)
rownames(corb) <- nms
corb

library(nlmeU)
library(insight)
library(corrplot)
library(lattice)
library(plot.matrix)

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(mod), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(mod))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(mod))
sigma2_b

## Let's compute the conditional and marginal var-cov matrix of Y
sgma <- summary(mod)$sigma

A <- getME(mod, "A") # A  --> N x n, A represents the D (not italic)
I.n <- Diagonal(ncol(A)) # IN  --> n x n

## the conditional variance-covariance matrix of Y (diagonal matrix)
SigmaErr = sgma^2 * (I.n)
SigmaErr[30:35, 30:35]  ## visualization of individual HN029
# Conditioned to the random effects b_i, we observe the var-cov of the errors
# that are independent and homoscedastic



## we visualize the first 20 rows/columns of the matrix
plot(as.matrix(SigmaErr[1:20,1:20]), main = 'Conditional estimated Var-Cov matrix of Y')

## the marginal variance-covariance matrix of Y (block-diagonal matrix)
V <- sgma^2 * (I.n + crossprod(A)) # V = s^2*(I_N+A*A) --> s^2*(I_N) is the error part, s^2*(A*A) is the random effect part
V[30:35, 30:35]  #-> V is a block-diagional matrix, the marginal var-cov matrix

# visualization of the first 20 rows/columns
plot(as.matrix(V[1:20,1:20]), main = 'Marginal estimated Var-Cov matrix of Y')

# Another way to interpret the variance output is to note percentage of the subject variance out 
# of the total, i.e. the Percentage of Variance explained by the Random Effect (PVRE).
# This is also called the intraclass correlation (ICC), because it is also an estimate of the within 
# cluster correlation.

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE #27%  high

## visualization of the random intercepts with their 95% confidence intervals
# Random effects: b_0i for i=1,...,234
ranef(mod, condVar=T) #stime puntuali dei b_i
dotplot(ranef(mod, condVar=T)) 

# The dotplot shows the point estimate and 95% PREDICTION intervals for this estimate for the random effects, 
# ordering them and highlighting which are significantly different from the mean (0)


# Prediction

# Let's now examine standard predictions vs. subject-specific predictions.
# As with most R models, we can use the predict function on the model object.

# Prediction from classical linear model
x1 <- atot*(dataP$`Dose cum`)^btot
x2 <- ctot*(dataP$`B_PBR4-25`)
y <- dataP$var_pbr
lm1 <- lm(y ~ x1 + x2) 
summary(lm1)

predict_lm <- predict(lm1)
head(predict_lm)

# Prediction from mixed model on the training set:
# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(mod, re.form=NA) 
head(predict_no_re) # (almost) same predictions of classical linear model
# 2) With random effects
predict_re <- predict(mod)
head(predict_re)#quite different from the classical ones


