library(readxl)
library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(rgl)
library(dplyr)

dataset <- read_excel("PatioAlbi.xlsx")
summary(dataset)
dim(dataset)

# rimozione dei valori di hb_base <1 (outliers)
dataset <- dataset[-which(dataset$`Hb base` < 1), ]
dim(dataset)

# controllo NA
any(is.na(dataset))
colSums(is.na(dataset))

#trasformiamo le varibili da numeriche a categoriche
dataset$zona<-factor(dataset$zona,levels=c(1,2,3))
dataset$ALCOHOL<-factor(dataset$ALCOHOL,levels=c(0,1,2,3,7))
dataset$SMOKER<-factor(dataset$SMOKER,levels=c(0,1,2,3))
dataset$menopausal<-factor(dataset$menopausal,levels=c(1,2,3))
dataset$ipertensione<-factor(dataset$ipertensione)
dataset$malattie_cardiache<-factor(dataset$malattie_cardiache)
dataset$boost<-factor(dataset$boost)

str(dataset)

nrows<-dim(dataset)[1]
ncols<-dim(dataset)[2]
nrows
ncols

## Controllo se ho missing values
print(sapply(dataset,function(x) any(is.na(x))))
sum(is.na(dataset))

# riempiamo i valori di menopausal in base ai valori medi della popolazione italiana
dataset$menopausal <- ifelse(dataset$age > 55 & is.na(dataset$menopausal), 2 , dataset$menopausal )
dataset$menopausal <- ifelse(dataset$age < 45 & is.na(dataset$menopausal), 1 , dataset$menopausal )
dataset$menopausal <- ifelse(dataset$age <= 55 & is.na(dataset$menopausal) & dataset$age >= 45, 3 , dataset$menopausal )
dataset$menopausal<-factor(dataset$menopausal,levels=c(1,2,3))

print(sapply(dataset,function(x) any(is.na(x))))
sum(is.na(dataset))

#faccio blocchi per paziente per visualizzare variazione di emoglobina in funzione dei dati

blocks<-list()
start=1;
end=1;
for (i in seq(1,nrows)){
  if (dataset[start,1]!=dataset[i,1]){
    block <- dataset[start:i-1, 1:ncols]
    blocks<-append(blocks, list(block))
    start=i+1}
}

#grafico per capire variazione di emoglobina in funzione della dose per ogni paziente,
#avendo colarato le linee in modo diverso a seconda della quantità iniziale di emoglobina 
#osservando i quartili

n<-length(blocks)
par(mfrow=cbind(1,1))
plot(0,100,xlim=c(0,35),ylim=c(0,450), xlab="Dose", ylab="Hb")
legend("topleft", legend=c("1 quart","2 quart.","3 quart"," 4 quart"), 
       col=c("black", "yellow", "blue", "red"), lty=c(1, 1, 1, 1), title="baseline Hb quartiles")

for(i in seq(2,232)){
  block<-blocks[[i]]
  
  x<-block[,2]
  x<-unlist(x)
  y<-block[,4]
  y<-unlist(y)
  if(block[1,3]<3.7)
    lines(x,y,add=TRUE,col='black')
  if(block[1,3]>3.7 & block[1,19]<5.7)
    lines(x,y,add=TRUE,col='yellow')
  if(block[1,3]>5.7 & block[1,19]<7.8)
    lines(x,y,add=TRUE,col='blue')
  if(block[1,3]>7.8)
    lines(x,y,add=TRUE,col='red')
}

#valori bassi di Hb init portano variazioni più alte


#ora creo dataset in cui rimuovo misurazioni di cui mi mancano valori di alcohol o fumo
NoSmoke <- dataset %>% filter(!is.na(SMOKER))
NoAlcohol<- NoSmoke %>% filter(!is.na(ALCOHOL))
print(sapply(NoAlcohol,function(x) any(is.na(x))))
sum(is.na(NoAlcohol))

final <- NoAlcohol[ ,-21] #tolgo info chemio

print(sapply(final,function(x) any(is.na(x))))
sum(is.na(final))

#ora siamo pronti, abbiamo dataset con tutte le info

#REGRESSIONE LINEARE
library(ggplot2)
numeriche <- final[ , c( "height","weight","BMI","age","dose alla fraz mim", "Hb base", "Hb" )]
View(numeriche)

# rimozione dello stato iniziale poichè studiamo l'evoluzione dei valori di emoglobina
#e già sappiamo che allo stato iniziale la variazione è uguale a zero (dato inutile)
numeriche <- numeriche[-which(numeriche$`dose alla fraz mim` == 0), ]
ggpairs (data = numeriche, title = "GGPairs",lower = list(continuous=wrap("points",alpha = 0.5,size = 0.1)) )

#BOXPLOT
par( mfrow = c( 2, 4) )
boxplot( numeriche$`dose alla fraz mim`, main = "Boxplot of dose", pch = 16, col = 'forestgreen' )
boxplot( numeriche$`Hb base`, main = "Boxplot of `Hb base`", pch = 16, col = 'orange' )
boxplot( numeriche$Hb, main = "Boxplot of Hb", pch = 16, col = 'yellow' )
boxplot( numeriche$age, main = "Boxplot of age", pch = 16, col = 'purple' )
boxplot( numeriche$BMI, main = "Boxplot of bmi", pch = 16, col = 'red' )
boxplot( numeriche$height, main = "Boxplot of height", pch = 16, col = 'blue' )
boxplot( numeriche$weight, main = "Boxplot of weight", pch = 16, col = 'brown' )

mod <- lm(Hb ~`dose alla fraz mim` + `Hb base` + age + 
            BMI + height + weight , data=numeriche)
summary(mod)
vif(mod)

mod2 <- lm(Hb ~`dose alla fraz mim` + `Hb base` + age + 
            BMI + height , data=numeriche)
summary(mod2)
vif(mod2)

#ora siamo a posto col vif, guardiamo omoschedastità e selezioniamo il modello in base al miglior R^2adj

# omoschedasticità
par(mfrow=c(1,1))
plot( mod2$fit, mod2$res, xlab = "Fitted", ylab = "Residuals",
      main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )

x = model.matrix( mod2 ) [ , -1 ]
y = numeriche$Hb

adjr = leaps( x, y, method = "adjr2" )
names(adjr)
adjr

#Select the model with the greatest adjusted R^2 and then display the best models from a leaps object.

bestmodel_adjr2_ind = which.max( adjr$adjr2 )
adjr$which[ bestmodel_adjr2_ind, ] 

# Displays the best 5 models from a leaps object
maxadjr( adjr, 5 ) 


#secondo me conviene fare un'ulteriore selezione a mano
#tolgo age perchè non significativa

mod3 <- lm(Hb ~`dose alla fraz mim` + `Hb base` + 
             BMI + height , data=numeriche)
summary(mod3)
vif(mod3)

#tolgo height perchè non significativa

mod4 <- lm(Hb ~`dose alla fraz mim` + `Hb base` + 
             BMI , data=numeriche)
summary(mod4)
vif(mod4)


# omoschedasticità
par(mfrow=c(1,1))
plot( mod4$fit, mod4$res, xlab = "Fitted", ylab = "Residuals",
      main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )

#normalità
qqnorm( mod4$res, ylab = "Raw Residuals", pch = 16 )
qqline( mod4$res )
shapiro.test(mod4$res)

#no normalità
x = model.matrix( mod4 ) [ , -1 ]
y = numeriche$Hb

b = boxcox( mod4, lambda = seq(-3,3,by=0.01) )
best_lambda = b$x[ which.max( b$y ) ]
best_lambda 
mod5 = lm(log(Hb) ~ `dose alla fraz mim` + `Hb base` + BMI, data=numeriche)
summary(mod5)

#verifico omoschedasticità e normalità post boxcox
mod5_res = mod5$residuals/summary( mod5 )$sigma
plot( mod5$fitted, mod5_res, xlab = 'Fitted values', ylab = 'Standarzized residuals' )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )
qqnorm( mod5_res )
abline( 0, 1, col = 'red' )
shapiro.test( mod5$res )

#non va ancora bene, coda sinistra molto pesante vediamo graficamente

influencePlot(mod5, id.method = "identify", main = "influential Plot",
              sub = "Circle size is proportial to Cook's Distance" )

#proviamo con la rimozione outliers

gs = summary(mod5) 
res_std = mod5$res/gs$sigma
watchout_ids_rstd = which( abs( res_std ) > 2 )
watchout_ids_rstd

watchout_rstd = res_std[ watchout_ids_rstd ]
watchout_rstd
plot( mod3$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals" )
abline( h = c(-2,2), lty = 2, col = 'orange' )
points( mod3$fitted.values[watchout_ids_rstd], 
        res_std[watchout_ids_rstd], col = 'red', pch = 16 )
legend('bottomleft', col = c('red'), 
       c('Standardized Residuals'), pch = rep( 16, 2 ), bty = 'n' )
gDef=lm(log(Hb) ~ `dose alla fraz mim` + `Hb base` + BMI, numeriche, subset = ( abs(res_std)<2))
summary( gDef )

#verifico normalità e proprietà ultimo modello

gDef_res = gDef$residuals/summary( gDef )$sigma
plot( gDef$fitted, gDef_res, xlab = 'Fitted values', ylab = 'Standarzized residuals' )
qqnorm( gDef_res )
abline( 0, 1, col = 'red' )
shapiro.test( gDef_res ) #accetto normalità al 2%
summary(gDef)
hist(gDef$fitted.values)
hist(gDef$residuals)

gDef$coefficients

## 2. Confidence Intervals and Regions
### Confidence Intervals

#__2.a__ 
#Compute the $98\%$ confidence intervals 

alpha = 0.02
n = dim(model.matrix(gDef))[1]
n
p=4
t_alpha2 = qt( 1-alpha/2, n-p )

beta_hat_dose = gDef$coefficients[2]
se_beta_hat_dose = summary( gDef )[[4]][2,2]
se_beta_hat_dose
IC_dose = c( beta_hat_dose - t_alpha2 * se_beta_hat_dose, 
           beta_hat_dose + t_alpha2 * se_beta_hat_dose )
IC_dose

beta_hat_hbinit = gDef$coefficients[3]
se_beta_hat_hbinit = summary( gDef )[[4]][3,2]
se_beta_hat_hbinit
IC_hbinit = c( beta_hat_hbinit - t_alpha2 * se_beta_hat_hbinit, 
           beta_hat_hbinit + t_alpha2 * se_beta_hat_hbinit )
IC_hbinit

beta_hat_bmi = gDef$coefficients[4]
se_beta_hat_bmi = summary( gDef )[[4]][4,2]
se_beta_hat_bmi
IC_bmi = c( beta_hat_bmi - t_alpha2 * se_beta_hat_bmi, 
               beta_hat_bmi + t_alpha2 * se_beta_hat_bmi )
IC_bmi

IC_dose
IC_hbinit
IC_bmi
gDef$coefficients






