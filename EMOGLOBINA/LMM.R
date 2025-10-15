#### LIBRERIE ####

library(readxl)
library(lme4)
library(nlme) 
library(corrplot)
library(lattice)
library(plot.matrix)
library(nlmeU)
library(insight)


#PULIZIA

dataset <- read_excel("PatioAlbi.xlsx")
summary(dataset)
dim(dataset)

dataset$Name<-factor(dataset$Name)
# rimuovo le colonne che non sono necessarie
dataset <- dataset[ , c( "Name", "BMI", "dose alla fraz mim", "Hb base", "Hb" )]
summary(dataset)
dim(dataset)

# rimozione dello stato iniziale
dataset <- dataset[-which(dataset$`dose alla fraz mim` == 0), ]
dim(dataset)

dataset <- dataset[-which(dataset$`dose alla fraz mim` < 2.65), ]
dim(dataset)

# rimozione dei valori di hb_base <1
dataset <- dataset[-which(dataset$`Hb base` < 1), ]
dim(dataset)

#variazione con segno di Hb, ossia traslo di 100 in negativo
dataset$Hb <- dataset$Hb - 100

# controllo NA
any(is.na(dataset))
colSums(is.na(dataset))
#tolgo righe senza BMI
dataset <- dataset[-which(is.na(dataset$BMI)),]
# controllo NA
any(is.na(dataset))
colSums(is.na(dataset))

###########################LMM 
y <- dataset$Hb
x1 <- (dataset$`dose alla fraz mim`)^2
x2 <- dataset$`Hb base`
eps <- dataset$Name

mod <- lmer(y ~ x1 + x2  +  (1|eps), 
             data = dataset)

summary(mod)
residui <- resid(mod)
mse <- mean(residui^2)
sqrt(mse)

#Devo controllare omoschedasticità
library(ggplot2)

ggplot(data.frame(predetti = predict(mod), residui = residui), aes(x = predetti, y = residui)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Valori Predetti", y = "Residui") +
  ggtitle("Grafico dei Residui per Valutare l'Omoschedasticità")

#sembrano omoschedastici

######PROVO ALTRE POTENZE
x1 <- (dataset$`dose alla fraz mim`)^(1/2)
mod2 <- lmer(y ~ x1 + x2  +  (1|eps), 
            data = dataset)

summary(mod2)
residui2 <- resid(mod2)
mse2 <- mean(residui2^2)
sqrt(mse2)

x1 <- dataset$`dose alla fraz mim`
mod3 <- lmer(y ~ x1 + x2  +  (1|eps), 
             data = dataset)

summary(mod3)
residui3 <- resid(mod3)
mse3 <- mean(residui3^2)
sqrt(mse3)

x1 <- (dataset$`dose alla fraz mim`)^(3/2)
mod4 <- lmer(y ~ x1 + x2  +  (1|eps), 
             data = dataset)

summary(mod4)
residui4 <- resid(mod4)
mse4 <- mean(residui4^2)
sqrt(mse4)

#PROVO con prodotto misto
x1 <- (dataset$`dose alla fraz mim`)^(1/2)
x2 <- (dataset$`dose alla fraz mim`)*(dataset$`Hb base`)
mod5 <- lmer(y ~ x1 + x2  +  (1|eps), 
             data = dataset)

summary(mod5)
residui5<- resid(mod5)
mse5 <- mean(residui5^2)
sqrt(mse5)

#using best NLS coefficients
x1 <- (dataset$`dose alla fraz mim`)^(2/7)
x2 <- ((dataset$`dose alla fraz mim`)*(dataset$`Hb base`))^(3/17)
mod6 <- lmer(y ~ x1 + x2  +  (1|eps), 
             data = dataset)

summary(mod6)
residui6<- resid(mod6)
mse6 <- mean(residui6^2)
sqrt(mse6)

AIC_model1 <- AIC(mod)
AIC_model2 <- AIC(mod2)
AIC_model3 <- AIC(mod3)
AIC_model4 <- AIC(mod4)
AIC_model5 <- AIC(mod5)
AIC_model6 <- AIC(mod6)

# Akaike tiene conto della massima vcerosimiglianza e del numero dei parametri
#−2×log-verosimiglianza massima+2×numero di parametri del modello
print(AIC_model1)
print(AIC_model2)
print(AIC_model3)
print(AIC_model4)
print(AIC_model5) 
print(AIC_model6) #migliore

summary(mod6)#random effects: variance of the b_i (sigma_b ^2) and variance of the residuals

confint(mod6,oldNames=TRUE)

## Var-Cov matrix of fixed-effects
vcovb <- vcov(mod6) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(mod6)), 3)
rownames(corb) <- nms
corb

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(mod6), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(mod6))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(mod6))
sigma2_b

## Let's compute the conditional and marginal var-cov matrix of Y
sgma <- summary(mod6)$sigma

A <- getME(mod6, "A") # A  --> N x n, A represents the D (not italic)
I.n <- Diagonal(ncol(A)) # IN  --> n x n

## the conditional variance-covariance matrix of Y (diagonal matrix)
SigmaErr = sgma^2 * (I.n)
SigmaErr[83:89, 83:89]  ## visualization of individual B005
# Conditioned to the random effects b_i, we observe the var-cov of the errors
# that are independent and homoscedastic

## we visualize the first 20 rows/columns of the matrix
plot(as.matrix(SigmaErr[1:20,1:20]), main = 'Conditional estimated Var-Cov matrix of Y')

## the marginal variance-covariance matrix of Y (block-diagonal matrix)
V <- sgma^2 * (I.n + crossprod(A)) # V = s^2*(I_N+A*A) --> s^2*(I_N) is the error part, s^2*(A*A) is the random effect part
V[83:89, 83:89]  #-> V is a block-diagional matrix, the marginal var-cov matrix

# visualization of the first 20 rows/columns
plot(as.matrix(V[1:20,1:20]), main = 'Marginal estimated Var-Cov matrix of Y')

# Another way to interpret the variance output is to note percentage of the subject variance out 
# of the total, i.e. the Percentage of Variance explained by the Random Effect (PVRE).
# This is also called the intraclass correlation (ICC), because it is also an estimate of the within 
# cluster correlation.

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 32% is  high!

## visualization of the random intercepts with their 95% confidence intervals
# Random effects: b_0i for i=1,...,234
ranef(mod6, condVar=T) #stime puntuali dei b_i
dotplot(ranef(mod6, condVar=T)) 

# The dotplot shows the point estimate and 95% PREDICTION intervals for this estimate for the random effects, 
# ordering them and highlighting which are significantly different from the mean (0)


# Prediction
#-------------
# Let's now examine standard predictions vs. subject-specific predictions.
# As with most R models, we can use the predict function on the model object.

# Prediction from classical linear model
x1 <- (dataset$`dose alla fraz mim`)^(2/7)
x2 <- ((dataset$`dose alla fraz mim`)*(dataset$`Hb base`))^(3/17)
y <- dataset$Hb
lm1 <- lm(y ~ x1 + x2, data = dataset) 
summary(lm1)

predict_lm <- predict(lm1)
head(predict_lm)

# Prediction from mixed model on the training set:
# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(mod6, re.form=NA) 
head(predict_no_re) # (almost) same predictions of classical linear model
# 2) With random effects
predict_re <- predict(mod6)
head(predict_re)#quite different from the classical ones

