# Projet Série temporelle  
# Marzook Cyril, Rida Adam 

#Importation des librairies
library(forecast)
library(xlsx)
data=read.csv(file="data_climat.csv", sep =",") #on importe notre dataset 

series =ts(data = data$Value,  frequency = 12, start=c(2000,1), end = c(2020,12))
series
#X11()  Pour afficher le graphique dans une autre fenetre
# par(mfcol=c(1,1)) Pour afficher un graphique par fenetre
plot(series)
saison = decompose(series)$seasonal
saison
series.des = decompose(series)$trend
series.des


plot(series, ,main="Degrés-jours aux Etats-Unis de 2000 à 2020 en F",ylab="Température",xlab="")
points(series.des,type="l",col="red") 
monthplot(series,main="degres-jour aux Etats-Unis de 2000 à 2016 en F",ylab="Température",xlab="") 


#Partie lissage exponentiel simple
series.LES=window(series,start=c(2000,1),end=c(2020,12)) 
series.LES.des=decompose(series.LES)$trend 
plot(series.LES,main="degres-jour aux Etats-Unis de 2000 à 2016 en F",ylab="Température",xlab="") 
points(series.LES.des,type="l",col="red") 
series.LES.app=window(series.des,start=c(2000,7),end=c(2015,7)) # on coupe le data set en 80/20 
series.LES.val=window(series.des,start=c(2015,7),end=c(2020,6))
LES 
LES=ets(series.LES.app,model="ANN",additive.only=TRUE)
summary(LES) 
prev=forecast(LES,h=60) 
plot(prev) 
points(series.LES.val,type="l",col="red") 
#legend('bottom',c("Valeurs observées","Prédictions"),col=c("red","blue"),lty=rep(1,1),lwd = rep(1,1))



series.LES.app=window(series,start=c(2000,1),end=c(2016,10))  # on coupe le data set en 80/20 
series.LES.val=window(series,start=c(2016,10),end=c(2020,12))


#Holt Winters
HW=ets(series.LES.app,model="AAA",additive.only=TRUE)
summary(HW)
prevHW=forecast(HW,h=52) 
plot(prevHW)
points(series.LES.val,type='l',col='red')
#legend('top',c("Valeurs observées","Prédictions"),col=c("red","blue"),lty=rep(1,2),lwd = rep(2,2))


