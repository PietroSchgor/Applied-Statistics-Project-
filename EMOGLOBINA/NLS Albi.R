

#### LIBRERIE ####

library(readxl)

#### NLS ####

dataset <- read_excel("PatioAlbi.xlsx")
summary(dataset)
dim(dataset)
# rimuovo le colonne che non sono necessarie
dataset <- dataset[ , c( "BMI", "dose alla fraz mim", "Hb base", "Hb" )]
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

summary(dataset)

# Definizione della funzione non lineare (ad esempio, modello polinomiale)

funzione_non_lineare <- function(x, a, b) {
  a * (x^b)
}

# Adattamento del modello non lineare ai dati
modello_non_lineare <- nls(dataset$Hb ~ funzione_non_lineare(dataset$`dose alla fraz mim`, a, b), data = dataset, start = list(a = 1, b = 1))

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
plot(dataset$`dose alla fraz mim`, dataset$Hb, xlab='Dose Cumulata', ylab='Var. Hb')
x <- seq(0,46)
lines(x, funzione_non_lineare(x,atot,btot), col='orange', lwd = 3)

#ora divido per blocchi
dataset_black <- dataset[dataset$`Hb base` < 3.58, ]
dataset_yellow <- dataset[dataset$`Hb base` > 3.58 & dataset$`Hb base`< 5.61, ]
dataset_blue <- dataset[dataset$`Hb base` > 5.61 & dataset$`Hb base` < 7.62, ]
dataset_red <- dataset[dataset$`Hb base` > 7.62, ]

par(mfrow=cbind(2,2))

#RED
modello_red <- nls(dataset_red$Hb ~ funzione_non_lineare(dataset_red$`dose alla fraz mim`, a, b), data = dataset_red, start = list(a = 1, b = 1))
summary(modello_red)
a <- coef(modello_red)[1]
a
b <- coef(modello_red)[2]
b
residui <- resid(modello_red)
mse <- mean(residui^2)
print(mse)
sqrt(mse)
plot(dataset_red$`dose alla fraz mim`, dataset_red$Hb, col='black', xlab='Dose Cumulata', ylab='Var. Hb')
x <- seq(0,37)
lines(x, funzione_non_lineare(x,a,b), col='red', lwd = 3)
lines(x, funzione_non_lineare(x,atot,btot), col='orange', lwd = 3)

#BLUE
modello_blue <- nls(dataset_blue$Hb ~ funzione_non_lineare(dataset_blue$`dose alla fraz mim`, a, b), data = dataset_blue, start = list(a = a, b = b))
summary(modello_blue)
a <- coef(modello_blue)[1]
a
b <- coef(modello_blue)[2]
b
residui <- resid(modello_blue)
mse <- mean(residui^2)
print(mse)
sqrt(mse)
plot(dataset_blue$`dose alla fraz mim`, dataset_blue$Hb, col='black', xlab='Dose Cumulata', ylab='Var. Hb')
x <- seq(0,37)
lines(x, funzione_non_lineare(x,a,b), col='blue', lwd = 3)
lines(x, funzione_non_lineare(x,atot,btot), col='orange', lwd = 3)

#YELLOW
modello_yellow <- nls(dataset_yellow$Hb ~ funzione_non_lineare(dataset_yellow$`dose alla fraz mim`, a, b), data = dataset_yellow, start = list(a = a, b = b))
summary(modello_yellow)
a <- coef(modello_yellow)[1]
a
b <- coef(modello_yellow)[2]
b
residui <- resid(modello_yellow)
mse <- mean(residui^2)
print(mse)
sqrt(mse)
plot(dataset_yellow$`dose alla fraz mim`, dataset_yellow$Hb, col='black', xlab='Dose Cumulata', ylab='Var. Hb')
x <- seq(0,45)
lines(x, funzione_non_lineare(x,a,b), col='yellow', lwd = 3)
lines(x, funzione_non_lineare(x,atot,btot), col='orange', lwd = 3)

#BLACK 
modello_black <- nls(dataset_black$Hb ~ funzione_non_lineare(dataset_black$`dose alla fraz mim`, a, b), data = dataset_black, start = list(a = a, b = b))
summary(modello_black)
a <- coef(modello_black)[1]
a
b <- coef(modello_black)[2]
b
residui <- resid(modello_black)
mse <- mean(residui^2)
print(mse)
sqrt(mse)
plot(dataset_black$`dose alla fraz mim`, dataset_black$Hb, col='black', xlab='Dose Cumulata', ylab='Var. Hb')
x <- seq(0,37)
lines(x, funzione_non_lineare(x,a,b), col='black', lwd = 3)
lines(x, funzione_non_lineare(x,atot,btot), col='orange', lwd = 3)


####ora provo con dose e hb iniziale, vediamo in 3D cosa succede ####

funzione2 <- function(x1, x2, a, b, c, d) {
  a*(x1^b)+c*((x1*x2)^d)
}

modello_non_lineare <- nls(dataset$Hb ~ funzione2(dataset$`dose alla fraz mim`, dataset$`Hb base`, a, b, c, d), 
                           data = dataset, start = list(a = atot, b = btot, c = 1, d = 1), nls.control(minFactor=1e-010,maxiter=100))

summary(modello_non_lineare)
atot <- coef(modello_non_lineare)[1]
atot
btot <- coef(modello_non_lineare)[2]
btot
ctot <- coef(modello_non_lineare)[3]
ctot
dtot <- coef(modello_non_lineare)[4]
dtot

residui <- resid(modello_non_lineare)
mse1 <- mean(residui^2)
print(mse1)
sqrt(mse1)

#plot
x1 <- seq(4.2, 37, length.out=100)
x2 <- seq(1.2, 18.3, length.out=100)
z <- matrix(NA, nrow = length(x1), ncol=length(x2))
for(i in 1:length(x1)) {
  for(j in 1:length(x2)) {
    z[i, j] <- funzione2(x1[i], x2[j], atot, btot, ctot, dtot)
  }
}

par(mfrow=c(1, 1))
image(x1, x2, z)
contour(x1, x2, z, add=T)
persp(x1, x2, z, col='red', xlab='Dose', ylab='Hb Init.', zlab='Var. Hb', main='3D Plot')
persp(x1, x2, z, col='red', theta=30, phi=30, shade=.05, xlab='Dose', ylab='Hb Init.', zlab='Var. Hb', main='3D Plot')

#più bello
library(rgl)
options(rgl.printRglwidget = TRUE)
persp3d(x1, x2, z, col='red', alpha=1, xlab='Dose', ylab='Hb Init.', zlab='Var. Hb', main='3D Plot')

#### funzione coi due quadrati - no prodotto mistro ####
funzione2 <- function(x1, x2, a, b, e, f) {
  a*(x1^b)+e*(x2^f)
}

modello2<- nls(dataset$Hb ~ funzione2(dataset$`dose alla fraz mim`, dataset$`Hb base`, a, b, e, f), 
                           data = dataset, start = list(a = 1, b = 1, e = 1, f = 1), nls.control(minFactor=1e-010,maxiter=100))

summary(modello2)
atot <- coef(modello2)[1]
atot
btot <- coef(modello2)[2]
btot
etot <- coef(modello2)[3]
etot
ftot <- coef(modello2)[4]
ftot

residui <- resid(modello2)
mse2 <- mean(residui^2)
print(mse2)
sqrt(mse2)

mse2<mse1
#peggio

#### funzione completa #NON LA METTIAMO perchè vogliamo che quando x1=0 la variazione sia uguale a zero

#funzione3 <- function(x1, x2, a, b, c, d, e, f) {
  #a*(x1^b) + c*((x1*x2)^d) + e*(x2^f)
#}

###############################################

# funzione con BMI --------------------------------------------------------

funzione3 <- function(x1, x2, x3, a, b, c, d, e, f) {
  a*(x1^b) + c*((x1*x2)^d) + e*((x1*x3)^f)
}

atot <- coef(modello_non_lineare)[1]
btot <- coef(modello_non_lineare)[2]
modello3 <- nls(dataset$Hb ~ funzione3(dataset$`dose alla fraz mim`, dataset$`Hb base`, dataset$`BMI`,  a, b, c, d, e, f), 
                           data = dataset, start = list(a = atot, b = btot, c = ctot, d = dtot, e = 1, f = 1), nls.control(minFactor=1e-010,maxiter=200))

summary(modello3)
atot <- coef(modello_non_lineare)[1]
atot
btot <- coef(modello_non_lineare)[2]
btot
ctot <- coef(modello_non_lineare)[3]
ctot
dtot <- coef(modello_non_lineare)[4]
dtot
htot <- coef(modello_non_lineare)[3]
htot
gtot <- coef(modello_non_lineare)[4]
gtot

residui <- resid(modello3)
mse3 <- mean(residui^2)
print(mse3)
sqrt(mse3)

#NON SERVE AGGIUGERE BMI, INFATTI
mse3<mse1 #falso

