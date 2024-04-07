#czyszczenie wszystkich obiektow
rm(list=ls(all=TRUE))

# import bibliotek
install.packages("tseries")
install.packages("stats")
install.packages("vars")

# deklaracje bibliotek
library(tseries)
library(stats)
library(vars)


#zaimportowanie danych
dane=read.csv(file="/Users/polajankowska/Desktop/UG/III semestr/Ćwiczenia Lech/dane/bfc_d.csv", header=TRUE)
dane2=read.csv(file="/Users/polajankowska/Desktop/UG/III semestr/Ćwiczenia Lech/dane/wig20_d.csv", header=TRUE)
dane[1:5,]
dane[(nrow(dane)-5):nrow(dane),]
dane2[1:5,]
dane2[(nrow(dane2)-5):nrow(dane2),]

# dlugosci szeregow
dim(dane2)
dim(dane)

#wyrownanie dlugosci szeregow
wig=NULL
for (i in 1:(nrow(dane)))
{ # pocz i
  #i=1
  data1=dane[i,1]
  data2=dane2[,1]
  ind01=(data1==data2)
  if (sum(ind01)==1) wig=c(wig,dane2[ind01,5])
} # kon i


dane=data.frame(dane, wig)
dane[1:5,] #zamknięcie 
attach(dane)
y=Zamkniecie #notowania bfc
x=wig #notowania wig (oba na zamknięcie)

length(x)
length(y) 


# wykresy
# wykres szeregu czasowego
par(mfrow=c(2,2))
plot(y, type="l", main="Wykresz szeregu czasowego notowań akcji BFC", xlab="obserwacje dzienne")
# histogram #dlugi ogon - czy nie
hist(y, 7, main="Histogram notowań akcji BFC", freq=FALSE, xlab=NULL, ylab=NULL)
# wariancja w oknie
ww=5
od=1
do=od+ww
war=NULL
while (do<=nrow(dane))
{ # poczatek while
  war=c(war, sd(y[od:do])^2)
  od=od+1
  do=od+ww
} # koniec while
# wykres wariancji
plot(war, type="l", main="Wykres wariancji akcji BFC", xlab=paste("szerokosc okna ",ww, sep=""))
# efekty dnia tygodnia
y.dzien="czwartek"
dzien=weekdays(as.Date(Data))
wyb.dzien=y[dzien==y.dzien]
plot(wyb.dzien, type="l", main=paste("Wykres szeregu czasowego notowań akcji BFC w dniu: ",y.dzien, sep=""), xlab="")

des.stats=function(x)
{ # pocz des.stats
  x=as.numeric(x)
  sa=sum(x)/length(x)
  me.1=sort(x) 
  me=me.1[floor(length(me.1)/2)]
  os2=sum((x-sa)^2)/length(x)
  os=os2^0.5
  v=os/sa*100
  mc3=sum((x-sa)^3)/length(x)
  mw3=mc3/os^3
  mc4=sum((x-sa)^4)/length(x)
  mw4=mc4/os^4
  wyniki=data.frame(matrix(c(sa,me,os,v,mw3,mw4),6,1),row.names=c("Średnia","mediana","odch.std","wsp.zmienności","wsp.asymetrii", "wsp.kurtozy"))
  names(wyniki)=""
  des.stats=wyniki
} # koniec des.stats

# statystyki opisowe #OPIS
statystyki.opisowe=des.stats(y)
statystyki.opisowe

# badanie stacjonarnosci/pierwiastka jednostkowego szeregu notowań
# test adf

# k=rzad procesu autoregresyjnego w tescie adf
# l=rzad badanej autokorelacji w rownaniu pomocniczym testu adf
nasze.adf=function(x, ww, t, k, l)
{ # pocz nasze adf
  #k=3
  #ww=1
  #t=0
  x=as.numeric(x)
  dx=x[2:length(x)]-x[(1:length(x)-1)]
  RHS=matrix(c(NA,x[1:(length(x)-1)]), length(x),1)
  if (k>=1)
  {# pocz if
    for (i in 1:k)
    { # pocz i
      RHS=cbind(RHS, c(rep(NA, i+1), dx[1:(length(dx)-(i+0))]))
    } # kon i
    
  } # kon if
  if (t==1) RHS=cbind(RHS, seq(1, nrow(RHS), by=1))
  if (ww==1) RHS=cbind(RHS, 1)
  
  RHS[1:10,]
  y=c(NA,dx)
  
  mod01=lm(y~RHS-1)
  summary(mod01)
  rr=residuals(mod01)
  bl=Box.test(rr, lag=l, type=c("Box-Pierce", "Ljung-Box"), fitdf = 0)
  nasze.adf=list(reg.pom=summary(mod01), acor=bl)
} # kon nasze adf

# testowanie poziomu zmiennej y testem ADF i Leybourne'a
y.rev=rev(y)
K=1
L=1
nasze.adf.wynik=nasze.adf(x=y, ww=1, t=0, k=K, l=L)
nasze.adf.wynik.l=nasze.adf(x=y.rev, ww=1, t=0, k=K, l=L)
nasze.adf.wynik
nasze.adf.wynik.l
adf.res=adf.test(x=y,  alternative = "stationary", k=K)
adf.res.l=adf.test(x=y.rev,  alternative = "stationary", k=K)
adf.res
adf.res.l
# wynik tesu adf wskazuje na niestacjonarnosc (nie odrzucam H0 o niestacjonarnosci) - pvalue= 0.994

# testowanie pierwszych przyrostow y
# liczenie przyrostu
K=1
L=1
dy=y[2:length(y)]-y[(1:length(y)-1)]
dy.rev=rev(dy)
nasze.adf.wynik.dy=nasze.adf(x=dy, ww=1, t=0, k=K, l=L)
nasze.adf.wynik.dy.rev=nasze.adf(x=dy.rev, ww=1, t=0, k=K, l=L)
nasze.adf.wynik.dy
nasze.adf.wynik.dy.rev
adf.res.dy=adf.test(x=dy,  alternative = "stationary", k=K)
adf.res.dy.rev=adf.test(x=dy.rev,  alternative = "stationary", k=K)
adf.res.dy
adf.res.dy.rev

#czy są stacjonarne????

# testowanie poziomu zmiennej x
K=1
L=1
x.rev=rev(x)
nasze.adf.wynik.x=nasze.adf(x=x, ww=1, t=0, k=K, l=L)
nasze.adf.wynik.x.rev=nasze.adf(x=x.rev, ww=1, t=0, k=K, l=L)
nasze.adf.wynik.x
nasze.adf.wynik.x.rev
adf.res.x=adf.test(x=x,  alternative = "stationary", k=K)
adf.res.x.rev=adf.test(x=x.rev,  alternative = "stationary", k=K)
adf.res.x
adf.res.x.rev

# wynik tesu adf i leybourne'a wskazuje na niestacjonarnosc (nie odrzucam H0 o niestacjonarnosci)

# testowanie pierwszych przyrostow x
# liczenie przyrostu
K=1
L=1
dx=x[2:length(x)]-x[(1:length(x)-1)]
dx.rev=rev(dx)
nasze.adf.wynik.dx=nasze.adf(x=dx, ww=1, t=0, k=K, l=L)
nasze.adf.wynik.dx.rev=nasze.adf(x=dx.rev, ww=1, t=0, k=K, l=L)
nasze.adf.wynik.dx
nasze.adf.wynik.dx.rev
adf.res.dx=adf.test(x=dx,  alternative = "stationary", k=K)
adf.res.dx.rev=adf.test(x=dx.rev,  alternative = "stationary", k=K)
adf.res.dx
adf.res.dx.rev

# notowanania BCM oraz WIG sa niestacjonarne, ale pierwsze przyrosty sa stacjonarne

# test Johansena
# testowanie rzedu procesu VAR
yu=(max(y)-y)/(max(y)-min(y))
xu=(max(x)-x)/(max(x)-min(x))
plot(yu, type="l", col="black", lwd=2)
lines(xu, type="l", col="red", lwd=2)

#kointegracje: gzie sie pokrywaja to maja wspolnyc ..stocchastycznej- czyli kointegrowanie - 

# jesli beda skointegrowane, to kointegracja z wyrazem wolny (nieograniczonym) i bez trendu
VARselect(y=cbind(y,x), type="const")


# z kryterium SC rzad procesu VAR p=2 (dlaczego dla rzedu 2?)
# wykonac test johansena dla p=2-1 

test.johansena=ca.jo(x=data.frame(cbind(y,x)), type = c("eigen", "trace"),
                     ecdet="const", K=2, spec="longrun", season = NULL, dumvar = NULL)
summary(test.johansena)

# y, x są skointegrowane, "jeden" wektror kointegrujacy
model01=cajools(test.johansena)
summary(model01)

# szacowanie VECM do zastosowania w funkcji predict
model01vec2var=vec2var(z=test.johansena, r=1)
# prognoza z wyprzedzeniem 5 okresowym
prog=predict(object=model01vec2var, n.ahead=5, ci=0.95, dumvar=NULL)
prog

attributes(prog)


# Otwarcie obszaru roboczego wykresu
od=1500
do=length(y)
yy=c(y[od:do], prog$fcst$y[,3])
plot(yy, type="n", col="black", lwd=2)
lines(y[od:do], type="l", col="black", lwd=2)
lines(c(rep(NA,length(y))[od:(do-1)], y[do], prog$fcst$y[,1]), type="l", col="red", lwd=2)


#
# Zakładam, że 'y' to wektor danych czasowych, a 'prog' to struktura zawierająca prognozy.

# Otwarcie obszaru roboczego wykresu
od = 1500
do = length(y)
yy = c(y[od:do], prog$fcst$y[,3])

# Rysowanie wykresu
plot(yy, type="n", col="black", lwd=2)
lines(y[od:do], type="l", col="black", lwd=2)
lines(c(rep(NA, length(y))[od:(do-1)], y[do], prog$fcst$y[,1]), type="l", col="red", lwd=2)

# Dodanie legendy i tytułu
legend("topright", legend=c("Dane", "Prognoza"), col=c("black", "red"), lwd=2)
title("Wykres danych i prognozy")

predykcje <- model01vec2var$datamat[,1]


plot(y, type="l", col="black", lwd=2,xlab="obserwacje dzienne", ylab="cena po zamknięciu")
lines(predykcje, type="l", col="red", lwd=2)
length(y)
length(predykcje)
#y <- y[3:length(y)]
length(y)
mse <- mean((y - predykcje)^2)
rmse <- sqrt(mean((y - predykcje)^2))
v <- rmse / (sum(y) / length(y)) * 100
mae <- mean(abs(y - predykcje))
mape <- mean(abs((y - predykcje) / y))* 100
bledy<-data.frame(matrix(c(mse,rmse,v,mae,mape),5,1),row.names=c("MSE","RMSE","V","MAE","MAPE"))
bledy






