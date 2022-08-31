library(readr)
library(stats)
a <- read_csv("C:/Users/Onur/OneDrive/Masaüstü/data_final (3).csv")
summary(a)

#Ham Veri Hali#
y1 <- 1:1350
y2 <- a$y2[1:1350]
b <- data.frame(y1,y2)

#in-sample#
y01 <- 1:1300
y02 <- a$y2[1:1300]
b0 <- data.frame(y01,y02)

#Out of sample#
y11 <- 1301:1350
y12 <- a$y2[1301:1350]
b1 <- data.frame(y11,y12)

#1.SORU CEVABI

###############################################################

#Zaman Seri Yapısı Kontrol#
par(mfrow=c(1,3))
plot.ts(b0$y02)
#Bu şekilde hareket etmesi ortalamasının zaman içinde sabit olduğu gösteriyor#
#Çünkü onun etrafında dalganıyor
#Durağan  bir süreç#
#Farkını almış gibi davranyor

#Correlogram#

acf(b0$y02, lag.max=20)
acf(b0$y02, lag.max=20, plot=F)
#Geometrik olarak azalıyor otokorelasyon fonksiyonu
#7.Dönemden sonra anlamsız oluyor

pacf(b0$y02, lag.max=20)
pacf(b0$y02, lag.max=20, plot=F)
#Kısmıotokorelasyon fonksiyonu 1. LAG YUKARDA 2.LAG AŞAĞIDA YER ALIYOR
#1.LAG sonra anlamını kaybediyor
#1.lag sonra hızlı bir düşüş oluyor
#Arma(1,0) modelidir

#Box-Test#

Box.test(b0$y02, lag = 4 ,type="Ljung")

#Zaman serisi yapısı olduğu için anlamlı


#LB TEST
LB.test <- lapply(1:20, function(i) Box.test(b0$y02, type="Ljung-Box", i))
LB.results <- data.frame(
  lag=1:20,
  xsquared=sapply(LB.test, "[[", "statistic"),
  pvalue=sapply(LB.test, "[[", "p.value")
)
head(LB.results, 24)
#Zaman serisi kontrolü 1'den 20 kadar yapıldı ve anlamlı
#Veri durağan olduğunu ve zaman seri olduğu belli

#kontrol(diagnostic check)
arma_00 <- arima(b0$y02, order = c(1,0,0))
arma_00  

arma_02 <- arima(b0$y02, order = c(2,0,0))
arma_02  

arma_12 <- arima(b0$y02, order = c(1,0,1))
arma_12

#6.381766 red bölgesi
#H0 hipotezini red ettik
#t value < 1.96 düşük çıkması lazım
#sıfırdan farklı bu
#aic(101>200) daha yüksek
#Model arima(1,0,1)

#Artıkların Kontrolü
res <- residuals(arma_12)
par(mfrow=c(1,3))
plot.ts(res)
acf(res, lag.max=20)
pacf(res, lag.max=20)


LB.test <- lapply(1:20, function(i) Box.test(res, type="Ljung-Box", i))
LB.results <- data.frame(
  lag=1:20,
  xsquared=sapply(LB.test, "[[", "statistic"),
  pvalue=sapply(LB.test, "[[", "p.value")
)
head(LB.results, 24)

#artık arna değerlendir
library(aTSA)
identify(b0$y02, p=3, q=3, nlag=6, intercept = T, stat.test = F, output = T)




##############################################################

#2.SORU CEVABI

###############################################################


#TAHMİN
install.packages("devtools")
library(devtools)
library(forecast)
arma <- arima(b0$y02, order=c(1,0,1))#arma(1,0,1)
arma

tahmini <- forecast(arma,h=50)
KA <-data.frame(b1$y12,tahmini)
KA$diff <- KA$b1.y12-KA$Point.Forecast

ong <- snaive(b0$y02,h=50,model ="arma")
s1 <- accuracy(ong,b1$y12)
s1
#Başarılı bir tahminleme ACF1 sıfıra yakın çıkmıştır

###############################################################

#3.SORU CEVABI

###############################################################

#Modelin Başarısını Yazı Tura Atma İle Karşılaştırılması
KA$sign <- sign(KA$diff) 
KA$sign[KA$sign  == "-1"] <- 0
table(KA$sign)
binom.test(27,50,p=0.5,alternative=c("two.sided","less","greater"),
           conf.level=0.95)
yazıtura <- c(0.3932420,0.6818508)
arma <- c(046,0.54)
comp <- data.frame(arma,yazıtura)
rownames(comp) <- c(0,1)
show(comp)
#Bir keree random 