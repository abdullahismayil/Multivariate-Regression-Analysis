#Verinin tanımlanması
veri= read.table("C:/Users/HP/Desktop/STATISTICS/3.sınıf/II dönem/Regresyon Çözümlemesi/Odevverisi.txt", header=TRUE)
names(veri)
attach(veri)

#2(Tanımlayıcı İstatistikler)
veri$x4 = as.factor(veri$x4)
summary(veri)

#3 Normalliğin İncelenmesi

#Normallik Testi
qqnorm(veri$y)
qqline(veri$y)
library(nortest)
lillie.test(veri$y)
ad.test(veri$y)

#Normallik Dönüşümleri

#Tersinin Alınması
veri$y_ters = 1/veri$y #tersinin alınması

lillie.test(veri$y_ters)
ad.test(veri$y_ters)

#sign fonksiyonu, mutlak değer ve log birleşik dönüşümü
veri$y_sign = sign(veri$y)*log(1 + abs(veri$y))

lillie.test(veri$y_sign)
ad.test(veri$y_sign)

#sign fonksiyonu, mutlak değer ve üstlü ifade birleşik dönüşümü
n=2
veri$y_üstel = sign(veri$y) * abs(veri$y)^n #sign fonksiyonu, mutlak değer ve üstlü ifade birleşik dönüşümü

lillie.test(veri$y_üstel)
ad.test(veri$y_üstel)

#Yeo-Johnson Dönüşümü
library(car)
y <- veri$y
yj_transformation <- powerTransform(y, family = "yjPower")
lambda <- 0.5
y_yeojohnson <- yjPower(y, lambda = lambda)
veri$y_yeojohnson <- y_yeojohnson

lillie.test(veri$y_yeojohnson)
ad.test(veri$y_yeojohnson)



# Boxplot çizimi
boxplot(veri$y, main = "y Değişkeninin Dağılımı", ylab = "y Değerleri")


# Aykırı değerlerin veriden çıkarılması
veri2 <- veri[-c(53, 58, 80, 93, 96), ]

# Aykırı Değerler Çıkarıldıkdan Sonra Normallik Testi)

lillie.test(veri2$y)
ad.test(veri2$y)

#Gereksiz Sütunların Silinmesi
veri2$y_ters <- NULL
veri2$y_üstel <- NULL
veri2$y_sign <- NULL
veri2$y_yeojohnson <- NULL


#Doğrusallık incelenmesi
pairs(veri2)

# Korelasyon matrisini hesaplanması
cor_matrix <- cor(veri2[, c("y", "x1", "x2", "x3")])
cor_matrix

#4 Çoklu Regresyon Modeli Kurulması ve Artıkların İncelenmesi

#Çoklu Regresyon Modeli
regresyon_modeli = lm(formula = y ~ x1 + x2 + x3 + x4, data=veri2 )
regresyon_modeli
summary(regresyon_modeli)


#Aykırı, Etkin Değer ve Uç Gözlem İncelemeleri
inf = ls.diag(regresyon_modeli)
inf
#Aykırı, Etkin Değer ve Uç Gözlemlerin Grafik Çizimleri

#Cook Uzaklığı Grafiği
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(regresyon_modeli)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(veri2$y)>50) 4/length(veri2$y) else 4/(length(veri2$y)-(length(veri2)-1)-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(veri2$y)>50) 4/length(veri2$y) else 4/(length(veri2$y)-(length(veri2)-1)-1),index(cooksd),""), col="red", pos=4)

#Gözlem Uzaklığı Grafiği(hat values)
hat1<-inf$hat
plot(hat1, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*length(veri2)/length(veri2$y) , col="red")
text(x=1:length(hat1)+1, y=hat1, labels=ifelse(hat1>2*length(veri2)/length(veri2$y),index(hat1),""), col="red", pos=4)

# Standartlaştırılmış Hata Grafiği
standart_artık <- rstandard(regresyon_modeli)
plot(standart_artık, pch="*", cex=2, main="Standart Artıklar")
abline(h = c(-2, 2), col="red")
text(x=1:length(standart_artık), y=standart_artık, labels=ifelse(standart_artık > 2 | standart_artık < -2, 1:length(standart_artık), ""), col="red", pos=4)


# Student Türü Hata Grafiği
student_artık <- rstudent(regresyon_modeli)
plot(student_artık, pch="*", cex=2, main="Student Tipi Artıklar")
abline(h = c(-3, 3), col="red")
text(x=1:length(student_artık), y=student_artık, labels=ifelse(student_artık > 3 | student_artık < -3, 1:length(student_artık), ""), col="red", pos=4)


#Aykırı, Etkin Değer ve Uç Gözlem İncelemeleri Sonucunda Veriden ilgili değerlerin çıkarılması.
veri3 = veri2[-c(4, 5, 10, 18, 38, 46,  49, 79, 81, 86, 88, 91 ), ]

#Tanımlayıcı İstatistikler

summary(veri3)

#Normallik Testi
qqnorm(veri3$y)
qqline(veri3$y)
library(nortest)
lillie.test(veri3$y)
ad.test(veri3$y)

#Doğrusallık incelenmesi
pairs(veri3)

# Korelasyon matrisini hesaplanması
cor_matrix2 <- cor(veri3[, c("y", "x1", "x2", "x3")])
cor_matrix2


#Çoklu Regresyon Modeli
regresyon_modeli2 = lm(formula = y ~ x1 + x2 + x3 + x4, data=veri3 )
regresyon_modeli2
summary(regresyon_modeli2)

#Aykırı, Etkin Değer ve Uç Gözlem İncelemeleri
inf2 = ls.diag(regresyon_modeli2)
inf2

#Cook Uzaklığı Grafiği
cooksd2 <- cooks.distance(regresyon_modeli2)
plot(cooksd2, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(veri3$y)>50) 4/length(veri3$y) else 4/(length(veri3$y)-(length(veri3)-1)-1) , col="red")
text(x=1:length(cooksd2)+1, y=cooksd2, labels=ifelse(cooksd2>if (length(veri3$y)>50) 4/length(veri3$y) else 4/(length(veri3$y)-(length(veri3)-1)-1),index(cooksd2),""), col="red", pos = 4)

#Gözlem Uzaklığı Grafiği(hat values)
hat2<-inf2$hat
plot(hat2, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*length(veri3)/length(veri3$y) , col="red")
text(x=1:length(hat2)+1, y=hat2, labels=ifelse(hat2>2*length(veri3)/length(veri3$y),index(hat2),""), col="red", pos=4)

# Standartlaştırılmış Hata Grafiği
standart_artık2 <- rstandard(regresyon_modeli2)
plot(standart_artık2, pch="*", cex=2, main="Standart Artıklar")
abline(h = c(-2, 2), col="red")
text(x=1:length(standart_artık2), y=standart_artık2, labels=ifelse(standart_artık2 > 2 | standart_artık2 < -2, 1:length(standart_artık2), ""), col="red", pos=4)


# Student Türü Hata Grafiği
student_artık2 <- rstudent(regresyon_modeli2)
plot(student_artık2, pch="*", cex=2, main="Student Tipi Artıklar")
abline(h = c(-3, 3), col="red")
text(x=1:length(student_artık2), y=student_artık2, labels=ifelse(student_artık2 > 3 | student_artık2 < -3, 1:length(student_artık2), ""), col="red", pos=4)

#5
#^
#y = 6.3130 + 2.8992x1 + 3.9759x2 − 2.8127x3 − 5.0682x4_2 − 7.4811x4_3  1.256 
#   (2.7157) (0.2345)    (0.1988)   (0.1895)   (0.3648)     (0.3584)
#H0 reddedilir, model anlamlıdır.

#6
#Hepsinin Modele katkısı anlamlıdır

#7
#R^2= 95.16%
#Etkileşim terimlerinin çözümlemeye eklenmesi
regresyon_modeli3 = lm(y ~ x1+ x2 + x3 +x4 + (x1 +x2 +x3) * x4, data = veri3)
summary(regresyon_modeli3)

#8 Regresyon Modeli için güven aralığı
confint(regresyon_modeli2, level = 0.99)

#9 Değişen Varyanslılık Sorununun İncelenmesi

#Grafiksel Gösterim
plot(predict(regresyon_modeli2), inf2$stud.res, ylab="Studentized Residuals", xlab="Predicted Values")
segments(-18, -1, -5, 2.75, col="red", lwd = 2)
segments(-18, -2, 10, -2, col="red", lwd = 2)
# Hafif Sağa Doğru Megafon

#Bruge-Pagan Test
library(lmtest)
bptest(regresyon_modeli2)
#H0 reddedilemez varyanslar Homojendir


#10 Özilişki Sorunu İncelenmesi
dwtest(regresyon_modeli2)
#H0 reddedilemez özilişki yoktur.


#11 Çoklu Bağlantı Sorunu İncelenmesi
 cor_matrix2
#İlişki katsayıları çok da yüksek çıkmadı
 
#1.yöntem(VİF) A
library(DAAG)
vif(regresyon_modeli2)
#Vif değerleri düşük çoklu bağlantı yok

#1.yöntem (VİF) B
library(olsrr)
ols_vif_tol(regresyon_modeli2)
#vif değerleri düşük çoklu bağlantı yok 


#2.yöntem(Koşul sayısı)
ols_eigen_cindex(regresyon_modeli2)
# 47 değeri olduğundan 1 adet çoklu bağlantı vardır denir. 27 değeri olduğundan çoklu bağlantı var ama güçlü değil.
#Güçlü çoklu bağlantı ve çoklu bağlantılar için anlamlı katsayılar bulunamıyor.

#3.yöntem(Özdeğer ve Özvektörler)
library(fastDummies)
dummy<-dummy_cols(veri3$x4) # Nitel değişken için göstermelik değişkenlerin oluşturulması
x41<-dummy$.data_1
x42<-dummy$.data_2
x43<-dummy$.data_3


ort1<-mean(veri3$x1)
kt1<-sum((veri3$x1-ort1)^2)
skx1<-(veri3$x1-ort1)/(kt1^0.5)
ort2<-mean(veri3$x2)
kt2<-sum((veri3$x2-ort2)^2)
skx2<-(veri3$x2-ort2)/(kt2^0.5) 
ort3<-mean(veri3$x3) 
kt3<-sum((veri3$x3-ort3)^2) 
skx3<-(veri3$x3-ort3)/(kt3^0.5)
ort42<-mean(x42)
kt42<-sum((x42-ort42)^2) 
skx42<-(x42-ort42)/(kt42^0.5) 
ort43<-mean(x43) 
kt43<-sum((x43-ort43)^2)
skx43<-(x43-ort43)/(kt43^0.5)
x<-cbind(skx1,skx2,skx3,skx42,skx43)
sm<- eigen (t(x)%*%x) 
signif(sm$values,3)
signif(sm$vectors,3)
#Özdeğerlerden 0 a çok yakın olan bir değer yoktur. Çoklu bağlantı yoktur.
#Özdeğerlerden 0 a çok yakın değer olmadığından özvektörlerden anlamlı bir sonuca varılamaz.

V = sm$vectors
t(V)%*%V
# Özvektörlerin transpozunu alıp kendisiyle çarparak Birim matrise oldukça yakın bir matris. Köşegen Değerler 1 ve diğer değerler 0 a çok yakın, çoklu bağlantı sorunu yoktur.
V %*% diag(sm$values) %*% t(V)
#Yüksek derece bir ilişkinin saptanamadığı söylenebilir. Çoklu bağlantı yoktur. özvektörler ve özdeğerlerden bulunan Değişkenler arasındaki ililşki matrisi.

#12 Modelin Uyum Kestirimi

uyum_kestirimi <- predict(regresyon_modeli2, veri3)
print(uyum_kestirimi[2])

# 13 Modelin Ön Kestirimi

x1 = 8.5
x2 = 0.5
x3 = 11.0
x4_2 = 1
x4_3 = 0
önkestirim_değeri = 6.3130 + (2.8992 * x1) + (3.9759 * x2) -  (2.8127 * x3) - (5.0682 * x4_2) - (7.4811 * x4_3)
önkestirim_değeri


#         ^        ~
# 14  𝐸(y) ve 𝐸(y) için güven aralıklarının %95 güven düzeyinde bulunması ve yorumlanması:
  
kritik_t <- qt(0.975, df = 77)  # 0.975 çünkü %95 güven düzeyi için iki kuyruklu dağılım kullanıyoruz

# Uyum kestirimi için güven aralığı

uyum_kestirimi <- 3.28097  # Uyum kestiriminden bulunan değer

#(X'X)^-1 matrisinin hesaplanması

library(MASS)
X <- cbind(veri3[, c("x1", "x2", "x3")], dummy)
X <- data.frame(lapply(X, function(col) {
  if (is.factor(col) || is.character(col)) {
    as.numeric(as.character(col))
  } else {
    col
  }
}))
X <- as.matrix(X)
XtX <- t(X) %*% X
print(XtX)
XtX_inv <- ginv(XtX)
print(XtX_inv)
row_vector <- matrix(as.numeric(X[2, ]), nrow = 1)

#Uyum Kestirimi denkleminin standart sapması

S_y <- 1.256 * sqrt(row_vector %*% XtX_inv %*% t(row_vector))

#Güven Aralıkları

uyum_std_error <- S_y
uyum_kestirimi_lower <- uyum_kestirimi - kritik_t * uyum_std_error
uyum_kestirimi_upper <- uyum_kestirimi + kritik_t * uyum_std_error
print(paste("Uyum Kestirimi Güven Aralığı:", uyum_kestirimi_lower, "-", uyum_kestirimi_upper))

# Ön kestirim için güven aralığı

önkestirim_değeri <- -3.06375  # Örnek verilen değer

#Örnek değerlerin matrise dönüştürülmesi

Y = matrix(c(8.5, 0.5, 11.0, 2, 0, 1, 0), nrow = 1, ncol = 7)
YtY = t(Y) %*% Y

#(Y'Y)^-1 matrisin hesaplanması

Y <- data.frame(lapply(Y, function(col) {
  if (is.factor(col) || is.character(col)) {
    as.numeric(as.character(col))
  } else {
    col
  }
}))
Y <- as.matrix(Y)
print(YtY)
if (det(YtY) != 0) {
  YtY_inv <- solve(YtY)
} else {
  YtY_inv <- ginv(YtY)
}
print(YtY_inv)


#Ön Kestirim denkleminin standart sapması

S_y2 <- 1.256 * sqrt(1  + Y %*% YtY_inv %*% t(Y))


#Güven Aralıkları

önkestirim_std_error <- S_y2
önkestirim_değeri_lower <- önkestirim_değeri - kritik_t * önkestirim_std_error
önkestirim_değeri_upper <- önkestirim_değeri + kritik_t * önkestirim_std_error
print(paste("Ön Kestirim Güven Aralığı:", önkestirim_değeri_lower, "-", önkestirim_değeri_upper))



# 15 Değişken Seçimi Yöntemleri ile en iyi modelin  bulunması

## İleriye Doğru Seçim 
library(stats)
lm.null <- lm(veri3$y ~ 1)
forward <- step(lm.null,veri3$y~veri3$x1+veri3$x2+veri3$x3+veri3$x4,  direction = "forward")
forward
summary(forward)

## Geriye Doğru Seçim 
backward<-step(regresyon_modeli2,direction="backward")
summary(backward)

## Adımsal Seçim Yöntemi 
library(MASS)
step.model <- stepAIC(regresyon_modeli2, direction = "both", trace = FALSE)
summary(step.model)


#16 Ridge Regresyon Modeli

ridge <- lm.ridge(veri3$y~x1+x2+x3+x4, data = veri3 ,lambda = seq(0,1,0.05))
matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),
        ylab=expression(hat(beta)))
abline(h=0,lwd=2)
ridge$coef
select(ridge)
ridge$coef[, ridge$lam == 0.5]



