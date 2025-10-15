library(readxl)

#### DATASET ####
dati <- read_excel("dati_clinici_170pts_emo.xlsx")
head(dati)
dim(dati)

colSums(is.na(dati))
na.bmi <- dati[which(is.na(dati$BMI)),]

dim(na.bmi)
sum(is.na(na.bmi$`Volume [cm3]`))

#creo un nuovo dataset con i dati utili per fare la stima (senza buchi in BMI e volume)
new_dataset <- dati[,c(5,6,17)]
new_dataset<-new_dataset[-which(is.na(new_dataset$BMI)), ]
new_dataset<-new_dataset[-which(is.na(new_dataset$`Volume [cm3]`)), ]
head(new_dataset)
dim(new_dataset)

# Verifica se ci sono NA in tutto il dataset
any(is.na(new_dataset))

modello <- lm(BMI ~age+ `Volume [cm3]`, data = new_dataset)
summary (modello)
a_lm <-modello$coefficients[1]
a_lm
b_lm <-modello$coefficients[3]
b_lm

plot (x=new_dataset$`Volume [cm3]`,y= new_dataset$BMI)
xtilde = c(14200: 35000)
f_lin <- function(x, a, b) {
  a * x+b
}
lines(x=xtilde, y = f_lin(xtilde,b_lm,a_lm), col='orange', lwd = 2)
res1 <-modello$residuals
mse1 <-mean(res1^2)
mse1

#provo a effettuare la stima con una funzione non lineare e tolgo l'età

plot (x=new_dataset$`Volume [cm3]`,y= new_dataset$BMI)

x1 <- new_dataset$`Volume [cm3]`
y <- new_dataset$BMI

f <- nls(new_dataset$BMI ~ a * new_dataset$`Volume [cm3]`^b+ c, data = new_dataset, start = list(a = b_lm, b = 1,c = 0))
summary (f)
res2 <- resid(f)
mse2 <- mean(res2 ^2)
mse2 
a1 <-coef(f)[1]
b1<-coef(f)[2]
c1<-coef(f)[3]

f_nonlin <- function(x, a, b,c) {
  a * x^b+c
}
lines(x=xtilde, y = f_nonlin(xtilde,a1,b1,c1), col='orange', lwd = 2)

#confronto gli MSE: quale tengo? io direi il secondo anche se c'è davvero poca differenza  
mse1
mse2

#sostituisco gli NA del bmi con i valori della funzione non lineare
dati$BMI <- ifelse(is.na(dati$BMI), a1*dati$`Volume [cm3]`^b1 +c1, dati$BMI )
#controllo gli NA rimasti -> sono effettivamente quelli a cui corrisponde un na del volume
na.bmi <- dati[which(is.na(dati$BMI)),]
dim(na.bmi)


s1<-sum(is.na(dati$BMI) & is.na (dati$`Volume [cm3]`))
s2<-sum(is.na(dati$BMI))
s1
s2
#ho ancora dei buchi nel BMI ma sono dati per cui non ho neanche i volume

#salvo il dataset riempito come file txt
write.table(dati, "dataset_filled_bmi.txt", sep = "\t", row.names = FALSE)

