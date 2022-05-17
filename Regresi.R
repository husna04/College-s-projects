#REGRESI LINIER SEDERHANA#
#respon satu var, prediktor satu var

#setwd("~/")
ad.data<-read.csv("C:\\Users\\asus\\OneDrive\\Documents\\Kuliah\\Semester 6\\Data sains\\P6 - Supervised (Regression)\\c3-CARPR.csv")

### CLEAN UP WORKSPACE DAN IMPORT DATA (PREPROCESSING DATA)
rm(list=ls())   # clean up workspace
ad.data <- read.csv(file.choose(), sep = ",", header = T)  # read data from csv file
ad.data[1:10,]  # inspect the first 10 rows of the loaded data

class(ad.data)  # inspect the type of the loaded data 

lapply(ad.data, class)  # inspect the type of each column 
any(is.na(ad.data)) # check for missing values


###2 ANALISIS DESKRIPTIF DAN VISUALISASI DATA
attach(ad.data) #separating the data
#kalo belum di attach income ga kepanggil
names(ad.data) #check col labels
summary(price)
summary(income)

mean(price)
mean(income)

sd(price)
sd(income)

quantile(t(price),c(0.05,0.95)) # the 5-th and 95-th percentiles of price
quantile(t(income),c(0.05,0.95)) # the 5-th and 95-th percentiles of income

##Kita ingin melakukan pemodelan linier dari variabel price dan variabel income tentu akan tidak masuk apabila kedua variabel tersebut tidak memiliki hubungan linier
cor(price,income) # Pearson correlation coefficient

#install.packages('gridExtra')#
library(ggplot2)

#visualisasi dari histogram
require(gridExtra)

hist.income=ggplot(ad.data, aes(x=income/1000)) +
  geom_histogram(color="yellow",fill="white",binwidth = 7)+
  ggtitle("Histogram Income Per $1000")+
  labs(x="income (in $1000's)",y="freq")
hist.price=ggplot(ad.data, aes(x=price)) +
  geom_histogram(color="black",fill="white",binwidth = 2000)+
  ggtitle("Histogram of Price")+
  labs(x="Price",y="freq")
grid.arrange(hist.price, hist.income, ncol=2)
#scatter plot
ggplot(data=ad.data, aes(x=income, y=price)) + 
  geom_point(aes(color="red", size=8, alpha=0.5))
#plot biasa gaperlu packages(?)

###3 TRAIN THE MODEL

#taksiran kuadrat terkecil
lm.income <- lm(price ~ income) #lm itu linier model
ggplot(data=ad.data, aes(x=income, y=price)) + 
  geom_point(aes(color="red", size=8, alpha=0.5))+
  geom_smooth(method='lm', formula= y~x)

#Menganalisis model regresi income dan price
summary(lm.income)


###4 PREDIKSI (TEST THE MODEL)
# example of a prediction
new.income <-data.frame(income=c(75000))
new.income
predict(lm.income,new.income,interval="prediction")

##plot untuk estimasi garis regresi dengan interval prediksinya
# prediction intervals
new <- data.frame(income=seq(min(income),max(income),5))
predinc.plim <- predict(lm.income,new,interval="prediction")
View(predinc.plim) #fit itu hasil y nya
matplot(new$income,predinc.plim,col=c("black","red","red"),lty=c(1,2,2),type="l",xlab="income",
        ylab="purchase price",main="The estimated regression line with prediction intervals")
points(income,price,col="blue",cex=1.5)

###UJI ASUMSI

#uji normalitas residual
shapiro.test(lm.income$residuals)
#pvalue 2.2e-16 (data tidak berdistribusi normal)
#pvalue 0.5 artinya residual normal, karena pvalue>alpha(5%). 


#uji autokorelasi
dwtest(lm.income)
#pvalue 0.9161
#pvalue > alpha ,maka tidak ada autokorelasi

#uji homoskedastisitas
bptest(lm.income, studentize=FALSE)
#pvalue 2.2e-16
#pvalue > alpha, tidak terjadi heter 
install.packages('olsrr')
library(olsrr)

#multiko
vif(lm.income)
ols_vif_tol(reg1)
#tidak terjadi jika nilai tol > 0.1

#linearitas(sudah diasumsikan linier biasanya)
resettest(lm.income)
#pvalue 0.08158
