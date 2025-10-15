library(readxl)
library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(rgl)
library(dplyr)
#setwd("C:/Users/Alberto/Desktop/progetto applied")
setwd("C:/Users/alber/OneDrive/Desktop/progetto applied")
dataset <- read_excel("full_nuovo.xlsx")

# grafici flow ------------------------------------------------------------

###grafici flow###
#Divisione in blocchi flow
library(dplyr)
data <-dataset[!is.na(dataset$Flow),]
data <-   data %>%
  select(Name, `Dose cum`, Flow, B_Flow, sex, age, BMI, ipertensione)
summary(data)
nrows=nrow(data)

blocks<-list()
start=1;
end=1;
for (i in seq(1,nrows)){
  if (data[start,1]!=data[i,1]){
    block <- data[start:i-1, ]
    blocks<-append(blocks, list(block))
    start=i+1}
}

#plotto i blocks con colori diversi UOMINI E DONNE
blocks[[1]]
n<-length(blocks)
x<-block[,2]
x<-unlist(x)
y<-block[,3]
y<-unlist(y)
y1<-block[,4]
y1<-unlist(y1)
y<-(y/y1)*100-100

par(mfrow = c(2, 2))
plot(x,y,xlim=c(0,36),ylim=c(-30,400),xlab='dose_cum', ylab='flow_variation')

for(i in seq(1,n)){
  block<-blocks[[i]]
  
  
  x<-block[,2]
  x<-unlist(x)
  x<-c(0,x)
  y<-block[,3]
  y<-unlist(y)
  y1<-block[,4]
  y1<-unlist(y1)
  y<-((y/y1)*100)-100
  y<-c(0,y)
  if(block[1,5]=='M')
    lines(x,y,add=TRUE,col='blue',lwd = 3)
  if(block[1,5]=='F' )
    lines(x,y,add=TRUE,col='#C71585',lwd = 3)

  
  
}
legend("topright", legend=c('MALE','FEMALE'), col=c('blue','#C71585'),pch=16, cex=0.4)

#plotto i blocks con colori diversi per BMI
blocks[[1]]
n<-length(blocks)
x<-block[,2]
x<-unlist(x)
y<-block[,3]
y<-unlist(y)
y1<-block[,4]
y1<-unlist(y1)
y<-(y/y1)*100

plot(x,y,xlim=c(0,36),ylim=c(-30,400),xlab='dose_cum', ylab='flow_variation')

for(i in seq(1,n)){
  block<-blocks[[i]]
  
  
  x<-block[,2]
  x<-unlist(x)
  x<-c(0,x)
  y<-block[,3]
  y<-unlist(y)
  y1<-block[,4]
  y1<-unlist(y1)
  y<-((y/y1)*100)-100
  y<-c(0,y)
  if(!is.na(block[1,7])){
    if(block[1,7]<26.5)
      lines(x,y,add=TRUE,col='yellow',lwd = 3)
    if(block[1,7]>26.5 )
      lines(x,y,add=TRUE,col='green',lwd = 3)
    }
 
}

legend("topright", legend=c('BMI<26.5','BMI>26.5'), col=c('yellow','green'),pch=16, cex=0.4)


#plotto i blocks con colori diversi per AGE
blocks[[1]]
n<-length(blocks)
x<-block[,2]
x<-unlist(x)
y<-block[,3]
y<-unlist(y)
y1<-block[,4]
y1<-unlist(y1)
y<-(y/y1)*100-100

plot(x,y,xlim=c(0,36),ylim=c(-30,400), xlab='dose_cum', ylab='flow_variation')

for(i in seq(1,n)){
  block<-blocks[[i]]
  
  
  x<-block[,2]
  x<-unlist(x)
  x<-c(0,x)
  y<-block[,3]
  y<-unlist(y)
  y1<-block[,4]
  y1<-unlist(y1)
  y<-((y/y1)*100)-100
  y<-c(0,y)
  if(!is.na(block[1,6])){
    if(block[1,6]<56)
      lines(x,y,add=TRUE,col='purple',lwd = 3)
    if(block[1,6]>56 )
      lines(x,y,add=TRUE,col='grey', lwd = 3)
  }
 
}

legend("topright", legend=c('age<56','age>56'), col=c('purple','grey'),pch=16, cex=0.4)

#plotto i blocks con colori diversi in base IPERTENSIONE
blocks[[1]]
n<-length(blocks)
x<-block[,2]
x<-unlist(x)
y<-block[,3]
y<-unlist(y)
y1<-block[,4]
y1<-unlist(y1)
y<-(y/y1)*100-100

plot(x,y,xlim=c(0,36),ylim=c(-30,400),xlab='dose_cum', ylab='flow_variation')

for(i in seq(1,n)){
  block<-blocks[[i]]
  
  
  x<-block[,2]
  x<-unlist(x)
  x<-c(0,x)
  y<-block[,3]
  y<-unlist(y)
  y1<-block[,4]
  y1<-unlist(y1)
  y<-((y/y1)*100)-100
  y<-c(0,y)
  if(block[1,8]==0)
    lines(x,y,add=TRUE,col='blue',lwd = 3)
  if(block[1,8]==1 )
    lines(x,y,add=TRUE,col='#C71585',lwd = 3)
  
  
  
}

legend("topright", legend=c('senza ipert','con ipert'), col=c('blue','#C71585'),pch=16, cex=0.4)














# grafici density ---------------------------------------------------------


#Divisione in blocchi density
library(dplyr)
data <-dataset[!is.na(dataset$`Total density`),]
data <-   data %>%
  select(Name, `Dose cum`, `Total density` , `B_Total density`, sex, age, BMI, ipertensione)
summary(data)
nrows=nrow(data)

blocks<-list()
start=1;
end=1;
for (i in seq(1,nrows)){
  if (data[start,1]!=data[i,1]){
    block <- data[start:i-1, ]
    blocks<-append(blocks, list(block))
    start=i+1}
}

#plotto i blocks con colori diversi UOMINI E DONNE
blocks[[1]]
n<-length(blocks)
x<-block[,2]
x<-unlist(x)
y<-block[,3]
y<-unlist(y)
y1<-block[,4]
y1<-unlist(y1)
y<-(y/y1)*100

par(mfrow = c(2, 2))
plot(x,y,xlim=c(0,36),ylim=c(-200,300))

for(i in seq(1,n)){
  block<-blocks[[i]]
  
  
  x<-block[,2]
  x<-unlist(x)
  x<-c(0,x)
  y<-block[,3]
  y<-unlist(y)
  y1<-block[,4]
  y1<-unlist(y1)
  y<-((y/y1)*100)-100
  y<-c(0,y)
  if(block[1,5]=='M')
    lines(x,y,add=TRUE,col='blue',lwd = 3)
  if(block[1,5]=='F' )
    lines(x,y,add=TRUE,col='#C71585',lwd = 3)
  
  
  
}
legend("topright", legend=c('MALE','FEMALE'), col=c('blue','#C71585'),pch=16, cex=0.6)

#plotto i blocks con colori diversi per BMI
blocks[[1]]
n<-length(blocks)
x<-block[,2]
x<-unlist(x)
y<-block[,3]
y<-unlist(y)
y1<-block[,4]
y1<-unlist(y1)
y<-(y/y1)*100

plot(x,y,xlim=c(0,36),ylim=c(-200,300))

for(i in seq(1,n)){
  block<-blocks[[i]]
  
  
  x<-block[,2]
  x<-unlist(x)
  x<-c(0,x)
  y<-block[,3]
  y<-unlist(y)
  y1<-block[,4]
  y1<-unlist(y1)
  y<-((y/y1)*100)-100
  y<-c(0,y)
  if(!is.na(block[1,7])){
    if(block[1,7]<26.5)
      lines(x,y,add=TRUE,col='yellow',lwd = 3)
    if(block[1,7]>26.5 )
      lines(x,y,add=TRUE,col='green',lwd = 3)
  }
  
}

legend("topright", legend=c('BMI<26.5','BMI>26.5'), col=c('yellow','green'),pch=16, cex=0.6)


#plotto i blocks con colori diversi per AGE
blocks[[1]]
n<-length(blocks)
x<-block[,2]
x<-unlist(x)
y<-block[,3]
y<-unlist(y)
y1<-block[,4]
y1<-unlist(y1)
y<-(y/y1)*100

plot(x,y,xlim=c(0,36),ylim=c(-200,300), xlab='dose_cum', ylab='density_variation')

for(i in seq(1,n)){
  block<-blocks[[i]]
  
  
  x<-block[,2]
  x<-unlist(x)
  x<-c(0,x)
  y<-block[,3]
  y<-unlist(y)
  y1<-block[,4]
  y1<-unlist(y1)
  y<-((y/y1)*100)-100
  y<-c(0,y)
  if(!is.na(block[1,6])){
    if(block[1,6]<56)
      lines(x,y,add=TRUE,col='purple',lwd = 3)
    if(block[1,6]>56 )
      lines(x,y,add=TRUE,col='grey', lwd = 3)
  }
  
}

legend("topright", legend=c('age<56','age>56'), col=c('purple','grey'),pch=16, cex=0.6)

#plotto i blocks con colori diversi in base IPERTENSIONE
blocks[[1]]
n<-length(blocks)
x<-block[,2]
x<-unlist(x)
y<-block[,3]
y<-unlist(y)
y1<-block[,4]
y1<-unlist(y1)
y<-(y/y1)*100

plot(x,y,xlim=c(0,36),ylim=c(-200,300))

for(i in seq(1,n)){
  block<-blocks[[i]]
  
  
  x<-block[,2]
  x<-unlist(x)
  x<-c(0,x)
  y<-block[,3]
  y<-unlist(y)
  y1<-block[,4]
  y1<-unlist(y1)
  y<-((y/y1)*100)-100
  y<-c(0,y)
  if(block[1,8]==0)
    lines(x,y,add=TRUE,col='blue',lwd = 3)
  if(block[1,8]==1 )
    lines(x,y,add=TRUE,col='#C71585',lwd = 3)
  }
legend("topright", legend=c('senza ipertensione','con ipertensione'), col=c('blue','#C71585'),pch=16, cex=0.6)
#end
  

# grafici PBR -------------------------------------------------------------

#Divisione in blocchi density
library(dplyr)
data <-dataset[!is.na(dataset$`PBR4-25`),]
data <-   data %>%
  select(Name, `Dose cum`, `PBR4-25` , `B_PBR4-25`, sex, age, BMI, ipertensione)
summary(data)
nrows=nrow(data)

blocks<-list()
start=1;
end=1;
for (i in seq(1,nrows)){
  if (data[start,1]!=data[i,1]){
    block <- data[start:i-1, ]
    blocks<-append(blocks, list(block))
    start=i+1}
}

#plotto i blocks con colori diversi UOMINI E DONNE
blocks[[1]]
n<-length(blocks)
x<-block[,2]
x<-unlist(x)
y<-block[,3]
y<-unlist(y)
y1<-block[,4]
y1<-unlist(y1)
y<-(y/y1)*100-100

par(mfrow = c(2, 2))
plot(x,y,xlim=c(0,36),ylim=c(-50,100))

for(i in seq(1,n)){
  block<-blocks[[i]]
  
  
  x<-block[,2]
  x<-unlist(x)
  x<-c(0,x)
  y<-block[,3]
  y<-unlist(y)
  y1<-block[,4]
  y1<-unlist(y1)
  y<-((y/y1)*100)-100
  y<-c(0,y)
  if(block[1,5]=='M')
    lines(x,y,add=TRUE,col='blue',lwd = 3)
  if(block[1,5]=='F' )
    lines(x,y,add=TRUE,col='#C71585',lwd = 3)
  
  
  
}
legend("topright", legend=c('MALE','FEMALE'), col=c('blue','#C71585'),pch=16, cex=0.6)

#plotto i blocks con colori diversi per BMI
blocks[[1]]
n<-length(blocks)
x<-block[,2]
x<-unlist(x)
y<-block[,3]
y<-unlist(y)
y1<-block[,4]
y1<-unlist(y1)
y<-(y/y1)*100-100

plot(x,y,xlim=c(0,36),ylim=c(-50,100))

for(i in seq(1,n)){
  block<-blocks[[i]]
  
  
  x<-block[,2]
  x<-unlist(x)
  x<-c(0,x)
  y<-block[,3]
  y<-unlist(y)
  y1<-block[,4]
  y1<-unlist(y1)
  y<-((y/y1)*100)-100
  y<-c(0,y)
  if(!is.na(block[1,7])){
    if(block[1,7]<26.5)
      lines(x,y,add=TRUE,col='yellow',lwd = 3)
    if(block[1,7]>26.5 )
      lines(x,y,add=TRUE,col='green',lwd = 3)
  }
  
}

legend("topright", legend=c('BMI<26.5','BMI>26.5'), col=c('yellow','green'),pch=16, cex=0.6)


#plotto i blocks con colori diversi per AGE
blocks[[1]]
n<-length(blocks)
x<-block[,2]
x<-unlist(x)
y<-block[,3]
y<-unlist(y)
y1<-block[,4]
y1<-unlist(y1)
y<-(y/y1)*100-100

plot(x,y,xlim=c(0,36),ylim=c(-50,100), xlab='dose_cum', ylab='density_variation')

for(i in seq(1,n)){
  block<-blocks[[i]]
  
  
  x<-block[,2]
  x<-unlist(x)
  x<-c(0,x)
  y<-block[,3]
  y<-unlist(y)
  y1<-block[,4]
  y1<-unlist(y1)
  y<-((y/y1)*100)-100
  y<-c(0,y)
  if(!is.na(block[1,6])){
    if(block[1,6]<56)
      lines(x,y,add=TRUE,col='purple',lwd = 3)
    if(block[1,6]>56 )
      lines(x,y,add=TRUE,col='grey', lwd = 3)
  }
  
}

legend("topright", legend=c('age<56','age>56'), col=c('purple','grey'),pch=16, cex=0.6)

#plotto i blocks con colori diversi in base IPERTENSIONE
blocks[[1]]
n<-length(blocks)
x<-block[,2]
x<-unlist(x)
y<-block[,3]
y<-unlist(y)
y1<-block[,4]
y1<-unlist(y1)
y<-(y/y1)*100-100

plot(x,y,xlim=c(0,36),ylim=c(-50,100))

for(i in seq(1,n)){
  block<-blocks[[i]]
  
  
  x<-block[,2]
  x<-unlist(x)
  x<-c(0,x)
  y<-block[,3]
  y<-unlist(y)
  y1<-block[,4]
  y1<-unlist(y1)
  y<-((y/y1)*100)-100
  y<-c(0,y)
  if(block[1,8]==0)
    lines(x,y,add=TRUE,col='blue',lwd = 3)
  if(block[1,8]==1 )
    lines(x,y,add=TRUE,col='#C71585',lwd = 3)
}
legend("topright", legend=c('senza ipertensione','con ipertensione'), col=c('blue','#C71585'),pch=16, cex=0.6)
#end
  




# distribuzioni D -----------------------------------------------------------
library(dplyr)
data <-dataset[!is.na(dataset$Flow),]
data <-   data %>%
  select(Name, `N MEASURE.x`, `Dose cum`, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14, D15, D16, D17,D18,D19,D20,D21,D22,D23,D24)
summary(data)
nrows=nrow(data)


data$sum<-data$D4+data$D5+data$D6+data$D7+data$D8+data$D9+data$D10+data$D11+data$D12+data$D13+data$D14+data$D15+data$D16+data$D17+data$D18+data$D19+data$D20+data$D21+data$D22+data$D23+data$D24

data_perc<-c(data$D4/data$sum,data$D5/data$sum,data$D6/data$sum,data$D7/data$sum,data$D8/data$sum,data$D9/data$sum,data$D10/data$sum,data$D11/data$sum,data$D12/data$sum,data$D13/data$sum,data$D14/data$sum,data$D15/data$sum,data$D16/data$sum,data$D17/data$sum,data$D18/data$sum,data$D19/data$sum,data$D20/data$sum,data$D21/data$sum,data$D22/data$sum,data$D23/data$sum,data$D24/data$sum)
data_perc<-matrix(data_perc,c(nrows,21))
data_perc<-data.frame(data_perc)
data_perc$Name<-data$Name

nrows=nrow(data)
data_orig<-data
data<-data_perc
blocks<-list()
start=1;
end=1;
for (i in seq(1,nrows)){
  if (data[start,22]!=data[i,22]){
    block <- data[start:i-1, 1:21 ]
    blocks<-append(blocks, list(block))
    start=i+1}
  
  
}
block <- data[106:107, 1:21 ]
blocks<-append(blocks, list(block))
blocks[[35]]

###inserire qui il paziente di cui si vogliono seguire le distribuzioni prova paziente 13 usando 
#par(mfrow=c(2,nmeas/2))
#for(j in seq (1,nmeas-1)){
#  barplot(block[j,],ylim = c(0,0.2), xlab='paziente' )
#} 
dev.off()
npaz=3

for(i in seq(npaz,npaz)){
  block<-blocks[[i]][1:21]
  nmeas<-nrow(block)
  block<-unlist(block)
  
  nmeas
  block<-as.numeric(block)
  block<-matrix(block,c(nmeas,21))
  par(mfrow=c(1,nmeas))
  for(j in seq (1,nmeas)){
    barplot(block[j,], xlab='paziente' )
  }
}
par(mfrow=c(1,nmeas))




#parametric model for distribution of capillary density among different sizes of capillaries.
# alpha: size of capillaries with major proportion
#beta: proportion of capillaries of size alpha
#we want to study how these two parameters evolve over time
mispaz<-rep(0,0)
alpha<-rep(0,0)
beta<-rep(0,0)
media<-rep(0,0)
var<-rep(0,0)
npaz<-length(blocks)
for(i in seq(1,npaz)){
  block<-blocks[[i]][1:21]
  
  nmeas<-nrow(block)
  block<-unlist(block)
  nmeas
  block<-as.numeric(block)
  block<-matrix(block,c(nmeas,21))
 
  for(j in seq (1,nmeas)){
    alpha<-c(alpha, which.max(block[j,]))
    beta<-c(beta, max(block[j,]))
    m<-0
    v<-0
    for (i in seq (4,24)){
      m<-m+i*block[j,i-3]
      v<-v+(i^2)*block[j,i-3]
    }
    
    v<-v-m^2
    media<-c(media,m)
    var<-c(var,v)
  }
  mispaz<-c(mispaz,nmeas)
}
media
var
scale<-var/media
shape<-media/scale
rate<-1/scale
data<-data_orig
mispaz#misurazioni per ogni paziente
count<-1
suminit<-rep(0,0)
alphainit<-rep(0,0)
betainit<-rep(0,0)
mediainit<-rep(0,0)
varinit<-rep(0,0)
scaleinit<-rep(0,0)
rateinit<-rep(0,0)
for(i in seq(1,npaz)){
  suminit<-c(suminit,rep(data$sum[count],mispaz[i]))
  alphainit<-c(alphainit,rep(alpha[count],mispaz[i]))
  betainit<-c(betainit,rep(beta[count],mispaz[i]))
  mediainit<-c(mediainit,rep(media[count],mispaz[i]))
  varinit<-c(varinit,rep(var[count],mispaz[i]))
  scaleinit<-c(scaleinit,rep(scale[count],mispaz[i]))
  rateinit<-c(rateinit,rep(rate[count],mispaz[i]))
  
  count<-count+mispaz[i]
  
  
  
  
}
suminit

reg<-data.frame(dosecum=data$`Dose cum`,alpha=alpha,beta=beta,alphainit=alphainit,betainit=betainit, suminit=suminit,media=media,var=var,mediainit=mediainit,varinit=varinit)

m.alpha<-lm(reg$alpha ~ reg$dosecum+reg$alphainit+reg$betainit+reg$suminit)
summary(m.alpha)
sd(m.alpha$residuals)
vif(m.alpha)

par(mfrow=c(1,1))
plot( m.alpha$fit, m.alpha$res, xlab = "Fitted", ylab = "Residuals",
      main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )




m.beta<-lm(reg$beta ~ reg$dosecum+reg$alphainit+reg$betainit+reg$suminit)
summary(m.beta)
sd(m.beta$residuals)
vif(m.beta)

par(mfrow=c(1,1))
plot( m.beta$fit, m.beta$res, xlab = "Fitted", ylab = "Residuals",
      main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )

###media e varianza stimate

m.media<-lm(reg$media ~ reg$mediainit+reg$varinit+reg$dosecum)
summary(m.media)
shapiro.test(m.media$residuals)

sd(m.media$residuals)
vif(m.media)

par(mfrow=c(1,1))
plot( m.media$fit, m.media$res, xlab = "Fitted", ylab = "Residuals",
      main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )




m.var<-lm(reg$var ~ reg$varinit+reg$mediainit+reg$dosecum)
summary(m.var)
shapiro.test(m.var$residuals)
sd(m.var$residuals)
vif(m.var)

par(mfrow=c(1,1))
plot( m.var$fit, m.var$res, xlab = "Fitted", ylab = "Residuals",
      main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )

m.scale<-lm(scale ~ scaleinit+rateinit+reg$dosecum)
summary(m.scale)
m.scale<-lm(scale ~ rateinit+reg$dosecum)
summary(m.scale)


m.rate<-lm(rate ~ scaleinit+rateinit+reg$dosecum)
summary(m.rate)
m.scale<-lm(scale ~ rateinit+reg$dosecum)
summary(m.scale)



x <- seq(4, 24, length.out = 1000)

# Calcola la densità della distribuzione Gamma per i valori di x
###grafici sovrapposti
library(RColorBrewer)
colors <- c('black','red','orange','yellow')
dev.off()
for(i in seq(22:25)){
y <- dgamma(x, shape = shape[i], rate = rate[i])

# Crea il plot della densità
par(new = TRUE)
plot(x, y, type = "l", col =colors[i], lwd = 4, 
     main = "Distribuzione Gamma", 
     xlab = "x", ylab = "Densità",xlim=c(4,24),ylim=c(0,0.2))
par(new = TRUE)
plot(seq(4,24),data_perc[i,1:21],type='l', axes = FALSE, xlab = "", ylab = "", col=colors[i],xlim=c(4,24),ylim=c(0,0.2))
}
legend("topright",legend=c("mis1","mis2","mis3","mis4"),fill=c(colors[1],colors[2],colors[3],colors[4]))
###grafici in sequenza
library(RColorBrewer)
colors <- c('black','red','orange','yellow')
dev.off()
par(mfrow=c(2,2))
for(i in seq(22:25)){
  y <- dgamma(x, shape = shape[i], rate = rate[i])
  
  # Crea il plot della densità
  plot(x, y, type = "l", col =colors[i], lwd = 4, 
       main = "Distribuzione Gamma", 
       xlab = "x", ylab = "Densità",xlim=c(4,24),ylim=c(0,0.2))
  par(new = TRUE)
  plot(seq(4,24),data_perc[i,1:21],type='l', axes = FALSE, xlab = "", ylab = "", col=colors[i],xlim=c(4,24),ylim=c(0,0.2))
}
# distribuzione PBR -------------------------------------------------------
library(dplyr)
data <-dataset[!is.na(dataset$Flow),]
data <-   data %>%
  select(Name, `N MEASURE.x`, `Dose cum`, PBR_04, PBR_05, PBR_06, PBR_07, PBR_08, PBR_09, PBR_10, PBR_11, PBR_12, PBR_13, PBR_14, PBR_15, PBR_16, PBR_17,PBR_18, PBR_19, PBR_20, PBR_21, PBR_22, PBR_23, PBR_24)
summary(data)
nrows=nrow(data)


data$sum<-data$PBR_04+data$PBR_05+data$PBR_06+data$PBR_07+data$PBR_08+data$PBR_09+data$PBR_10+data$PBR_11+data$PBR_12+data$PBR_13+data$PBR_14+data$PBR_15+data$PBR_16+data$PBR_17+data$PBR_18+data$PBR_19+data$PBR_20+data$PBR_21+data$PBR_22+data$PBR_23+data$PBR_24

data_perc<-c(data$Name,data$PBR_04/4,data$PBR_05/5,data$PBR_06/6,data$PBR_07/7,data$PBR_08/8,data$PBR_09/9,data$PBR_10/10,data$PBR_11/11,data$PBR_12/12,data$PBR_13/13,data$PBR_14/14,data$PBR_15/15,data$PBR_16/16,data$PBR_17/17,data$PBR_18/18,data$PBR_19/19,data$PBR_20/20,data$PBR_21/21,data$PBR_22/22,data$PBR_23/23,data$PBR_24/4)
data_perc<-matrix(data_perc,c(nrows,15))


nrows=nrow(data)
data_orig<-data
data<-data_perc
blocks<-list()
start=1;
end=1;
for (i in seq(1,nrows)){
  if (data[start,1]!=data[i,1]){
    block <- data[start:i-1, 2:15 ]
    blocks<-append(blocks, list(block))
    start=i+1}
}

block <- data[106:107, 1:14 ]
blocks<-append(blocks, list(block))
blocks[[36]]
npaz<-length(blocks)

###inserire qui il paziente di cui si vogliono seguire le distribuzioni prova paziente 13 usando 
#par(mfrow=c(2,nmeas/2))
#for(j in seq (1,nmeas-1)){
#  barplot(block[j,],ylim = c(0,0.2), xlab='paziente' )
#} 
npaz=5
for(i in seq(npaz,npaz)){
  block<-blocks[[i]]
  nmeas<-nrow(block)
  block<-as.numeric(block)
  block<-matrix(block,c(nmeas,14))
  par(mfrow=c(1,nmeas))
  for(j in seq (1,nmeas)){
    barplot(block[j,],ylim = c(0,0.2), xlab='paziente' )
  }
}
par(mfrow=c(1,nmeas))


block<-blocks[[13]]
nmeas<-nrow(block)
block
block<-as.numeric(block)
block<-matrix(block,c(nmeas,14))
barplot(block[1,],ylim=2)

