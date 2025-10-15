library(readxl)
library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(rgl)
library(dplyr)
library( rms )
library(arm)
library(ResourceSelection)
library(pROC)
library(PRROC)
library(Hmisc)

#abbiamo tolto dal file excel i pazienti che hanno Hb base < 1
#perchè mostrano variazioni troppo grandi (outliers)
#e qualche valore che ci sembrava strano

#### DATASET ####
dataset <- read_excel("PatioAlbi.xlsx")
head(dataset)
summary(dataset)

#trasformiamo le varibili da numeriche a categoriche
dataset$zona<-factor(dataset$zona,levels=c(1,2,3))
dataset$ALCOHOL<-factor(dataset$ALCOHOL,levels=c(0,1,2,3,7))
dataset$SMOKER<-factor(dataset$SMOKER,levels=c(0,1,2,3))
dataset$chirurgia<-factor(dataset$chirurgia)
dataset$menopausal<-factor(dataset$menopausal,levels=c(1,2,3))
dataset$ipertensione<-factor(dataset$ipertensione)
dataset$malattie_cardiache<-factor(dataset$malattie_cardiache)
dataset$boost<-factor(dataset$boost)

str(dataset)

nrows<-dim(dataset)[1]
ncols<-dim(dataset)[2]

## Controllo se ho missing values: Sì
print(sapply(dataset,function(x) any(is.na(x))))
sum(is.na(dataset))

#riempio i vuoti di BMI con il valore medio

m <- mean(dataset$BMI, na.rm = TRUE)
dataset$BMI <- replace(dataset$BMI, is.na(dataset$BMI), m)
summary(dataset)

# riempiamo i valori di menopausal in base ai valori medi della popolazione italiana
dataset$menopausal <- ifelse(dataset$age > 55 & is.na(dataset$menopausal), 2 , dataset$menopausal )
dataset$menopausal <- ifelse(dataset$age < 45 & is.na(dataset$menopausal), 1 , dataset$menopausal )
dataset$menopausal <- ifelse(dataset$age <= 55 & is.na(dataset$menopausal) & dataset$age >= 45, 3 , dataset$menopausal )
dataset$menopausal<-factor(dataset$menopausal,levels=c(1,2,3))

print(sapply(dataset,function(x) any(is.na(x))))
sum(is.na(dataset))

# #divido in zone
# 
# zona1 <- dataset[which(dataset$zona==1), ]
# zona2 <- dataset[which(dataset$zona==2), ]
# zona3 <- dataset[which(dataset$zona==3), ]
# 


#### PLOT BLOCCHI ####

#faccio blocchi per paziente per visualizzare variazione di emoglobina in funzione dei dati

#creo 4 differenti gruppi di blocks uno per ogni quartile
dataset1=dataset[which(dataset$`Hb base`<3.7), ]
nrows<-dim(dataset1)[1]
blocks1<-list()
start=1;
end=1;
for (i in seq(1,nrows)){
  if (dataset1[start,1]!=dataset1[i,1]){
    block <- dataset1[start:i-1, 1:ncols]
    blocks1<-append(blocks1, list(block))
    start=i+1}
}

dataset2=dataset[which(dataset$`Hb base`>3.7 & dataset$`Hb base`< 5.7),]
nrows<-dim(dataset2)[1]
blocks2<-list()
start=1;
end=1;
for (i in seq(1,nrows)){
  if (dataset2[start,1]!=dataset2[i,1]){
    block <- dataset2[start:i-1, 1:ncols]
    blocks2<-append(blocks2, list(block))
    start=i+1}
}

dataset3=dataset[which(dataset$`Hb base`>5.7 & dataset$`Hb base`<7.8),]
nrows<-dim(dataset3)[1]
blocks3<-list()
start=1;
end=1;
for (i in seq(1,nrows)){
  if (dataset3[start,1]!=dataset3[i,1]){
    block <- dataset3[start:i-1, 1:ncols]
    blocks3<-append(blocks3, list(block))
    start=i+1}
}

dataset4=dataset[which(dataset$`Hb base`>7.8),]
nrows<-dim(dataset4)[1]
blocks4<-list()
start=1;
end=1;
for (i in seq(1,nrows)){
  if (dataset4[start,1]!=dataset4[i,1]){
    block <- dataset4[start:i-1, 1:ncols]
    blocks4<-append(blocks4, list(block))
    start=i+1}
}


#creo oggetto funzione per ogni blocco f(x=dose_cumulata,i=indice_blocco)
#ho dovuto creare 4 tipi di funzione uno per ogni quartile

funzione <- function(x,i) {
  block<-blocks[[i]]
  x_interpolazione<-block[,2]
  x_interpolazione<-unlist(x_interpolazione)
  y_interpolazione<-block[,4]
  y_interpolazione<-unlist(y_interpolazione)
  n_misurazioni<-length(y_interpolazione)
  if (x <= x_interpolazione[1]) {
    return(y_interpolazione[1])
  }
  if (x >= x_interpolazione[n_misurazioni]) {
    return(y_interpolazione[n_misurazioni])
  }
  for (k in 2:n_misurazioni) {
    if (x <= x_interpolazione[k]) {
      m <- (y_interpolazione[k] - y_interpolazione[k-1]) / (x_interpolazione[k] - x_interpolazione[k-1])
      q <- y_interpolazione[k-1] - m * x_interpolazione[k-1]
      return(m * x + q)
    }
  }
}
  funzione1 <- function(x,i) {
    block<-blocks1[[i]]
    x_interpolazione<-block[,2]
    x_interpolazione<-unlist(x_interpolazione)
    y_interpolazione<-block[,4]
    y_interpolazione<-unlist(y_interpolazione)
    n_misurazioni<-length(y_interpolazione)
    if (x <= x_interpolazione[1]) {
      return(y_interpolazione[1])
    }
    if (x >= x_interpolazione[n_misurazioni]) {
      return(y_interpolazione[n_misurazioni])
    }
    for (k in 2:n_misurazioni) {
      if (x <= x_interpolazione[k]) {
        m <- (y_interpolazione[k] - y_interpolazione[k-1]) / (x_interpolazione[k] - x_interpolazione[k-1])
        q <- y_interpolazione[k-1] - m * x_interpolazione[k-1]
        return(m * x + q)
      }
    }
  }
  funzione2 <- function(x,i) {
    block<-blocks2[[i]]
    x_interpolazione<-block[,2]
    x_interpolazione<-unlist(x_interpolazione)
    y_interpolazione<-block[,4]
    y_interpolazione<-unlist(y_interpolazione)
    n_misurazioni<-length(y_interpolazione)
    if (x <= x_interpolazione[1]) {
      return(y_interpolazione[1])
    }
    if (x >= x_interpolazione[n_misurazioni]) {
      return(y_interpolazione[n_misurazioni])
    }
    for (k in 2:n_misurazioni) {
      if (x <= x_interpolazione[k]) {
        m <- (y_interpolazione[k] - y_interpolazione[k-1]) / (x_interpolazione[k] - x_interpolazione[k-1])
        q <- y_interpolazione[k-1] - m * x_interpolazione[k-1]
        return(m * x + q)
      }
    }
  }
  
  funzione3 <- function(x,i) {
    block<-blocks3[[i]]
    x_interpolazione<-block[,2]
    x_interpolazione<-unlist(x_interpolazione)
    y_interpolazione<-block[,4]
    y_interpolazione<-unlist(y_interpolazione)
    n_misurazioni<-length(y_interpolazione)
    if (x <= x_interpolazione[1]) {
      return(y_interpolazione[1])
    }
    if (x >= x_interpolazione[n_misurazioni]) {
      return(y_interpolazione[n_misurazioni])
    }
    for (k in 2:n_misurazioni) {
      if (x <= x_interpolazione[k]) {
        m <- (y_interpolazione[k] - y_interpolazione[k-1]) / (x_interpolazione[k] - x_interpolazione[k-1])
        q <- y_interpolazione[k-1] - m * x_interpolazione[k-1]
        return(m * x + q)
      }
    }
  }
  funzione4 <- function(x,i) {
    block<-blocks4[[i]]
    x_interpolazione<-block[,2]
    x_interpolazione<-unlist(x_interpolazione)
    y_interpolazione<-block[,4]
    y_interpolazione<-unlist(y_interpolazione)
    n_misurazioni<-length(y_interpolazione)
    if (x <= x_interpolazione[1]) {
      return(y_interpolazione[1])
    }
    if (x >= x_interpolazione[n_misurazioni]) {
      return(y_interpolazione[n_misurazioni])
    }
    for (k in 2:n_misurazioni) {
      if (x <= x_interpolazione[k]) {
        m <- (y_interpolazione[k] - y_interpolazione[k-1]) / (x_interpolazione[k] - x_interpolazione[k-1])
        q <- y_interpolazione[k-1] - m * x_interpolazione[k-1]
        return(m * x + q)
      }
    }
  }
  
  
  


#vorrei fare manova con g=4 e p=2...

n1<-length(blocks1)
mat1 <- matrix(nrow = n1, ncol = 2)

for(i in 1:n1){
  mat1[i,1]<-funzione1(14.31,i)
  mat1[i,2]<-funzione1(23.37,i)
}

n2<-length(blocks2)
mat2 <- matrix(nrow = n2, ncol = 2)

for(i in 1:n2){
  mat2[i,1]<-funzione2(14.31,i)
  mat2[i,2]<-funzione2(23.37,i)
}

n3<-length(blocks3)
mat3 <- matrix(nrow = n3, ncol = 2)

for(i in 1:n3){
  mat3[i,1]<-funzione3(14.31,i)
  mat3[i,2]<-funzione3(23.37,i)
}

n4<-length(blocks4)
mat4 <- matrix(nrow = n4, ncol = 2)

for(i in 1:n4){
  mat4[i,1]<-funzione4(14.31,i)
  mat4[i,2]<-funzione4(23.37,i)
}

mat<-rbind(mat1,mat2,mat3,mat4)

label1<-rep("1", times = n1)
label2<-rep("2", times = n2)
label3<-rep("3", times = n3)
label4<-rep("4", times = n4)
label<-c(label1,label2,label3,label4)


data_manova<-data.frame(
  Y1=mat[,1],
  Y2=mat[,2],
  X=label
)

manova_result <- manova(cbind(Y1, Y2) ~ X, data = data_manova)

# Visualizza i risultati
summary(manova_result)
