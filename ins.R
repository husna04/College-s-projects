ad.data <- read.csv(file.choose(), sep = ";", header = T)  # read data from csv file
ad.data[1:10,]  # inspect the first 10 rows of the loaded data
attach(ad.data) #separating the data
class(ad.data)  # inspect the type of the loaded data 

lapply(ad.data, class)  # inspect the type of each column 
any(is.na(ad.data)) # check for missing values

reg1 <- lm(GDP ~ GFCF+TR+Trade+IVA)
summary(reg1)
#jika nilai signifikansi lebih kecil dari 0.05 maka var x secara sendiri sendiri berpengaruh terhadao var y
#dik bahwa untuk var x1 pva<alpha artinya var1 signifikan berpengaruh terhadap var y
#nilai p value dari f stat < alpha, maka secara simultan var x berpengaruh terhadap var y
#var independen sign berpengaruh terhadap var dependen sebesar 99.91%
reg1$coefficients

###hasil pvalue signifikan, karena pvalue < alpha=5%Ada pengaruh dari var ***
#terhadap prestasi

#uji f
anova(reg1)
#Pr(>F) <0.05 maka ada pengaruh var x secara simultan terhadap variabel y


plot (GFCF,GDP)
plot (TR,GDP)
plot (Trade,GDP)
plot (IVA,GDP)


install.packages("lmtest")
library(lmtest)
library(car)
#analisis

#uji asumsi
#uji normalitas residual
shapiro.test(reg1$residuals)
#pvalue 0.5 artinya residual normal, karena pvalue>alpha(5%). 
ks.test(reg1$residuals, ecdf(reg1$residuals))
ks.test()

#uji autokorelasi
dwtest(reg1)
#pvalue > alpha ,maka tidak ada autokorelasi

#uji homoskedastisitas
bptest(reg1, studentize=FALSE)
#pvalue > alpha, tidak terjadi heter 
install.packages('olsrr')
library(olsrr)
#multiko
vif(reg1)
ols_vif_tol(reg1)
#tidak terjadi jika nilai tol > 0.1

resettest(reg1)
#pvalue>alpha, h0 diterima 