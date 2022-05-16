data=read.csv(file.choose(), sep = ";", header = T)
data
head(data)
attach(data)
summary(data)
library(plm)
library(panelr)
attach(data)
d<-long_panel(data,begin=0,end=3,label_location="end") 
head(d)
attach(d)
ols<-lm(d$T ~ d$perlakuan, data = d)

#FEM
fe=plm(d$T~ d$perlakuan + d$x, data = d, index = c("id","wave"),
       effect = "individual",model = "within")
summary(fe)


pFtest(fe, ols)

#random-effect
re=plm(d$T~ d$perlakuan + d$x, data = d, index = c("id","wave"),
       effect = "individual",model = "random")
summary(re)

#cek mana yang dipake
###UJI HAUSMAN
phtest(fe,re)

###Uji autokorelasi
pdwtest(re)

###UJI HOMOSKEDASTISITAS
library(lmtest)
bptest(re)

###UJI MULTIKOL
library(car)
vif(re)

##uji linearitas ramsey's reset 
resettest(d$T~d$perlakuan+d$x)
