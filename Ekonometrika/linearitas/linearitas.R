data=read.csv(file.choose(), sep = ",", header = T)
y=as.matrix(data[,7])
x0=as.matrix(data[,c(3,6,8,9,10)])
class(data)  # inspect the type of the loaded data 
lapply(data, class)  # inspect the type of each column 
any(is.na(ad.data)) # check for missing values
reg1 <- lm(GDP ~ GFCF+TR+Trade+IVA) #train the model
summary(reg1)
attach(data)
data1 <- lm(lprice ~ bdrms+colonial+lassess+llotsize+lsqrft)
summary(data1)
ols_vif_tol(data1)


##Menentukan banyak unit pengamata (baris dari vektor y) 
n	<- nrow(y)
##Membentuk vektor unit konstan
u	<- matrix(c(1),n,1)

X	<- cbind(u,x0)
#Taksiran standar error
#Matriks Hat (H)
H	<- X%*%solve(t(X)%*%X)%*%t(X)

##Membentuk matriks identitas (nxn)
I	<- diag(c(1),n,n)

##Menghitung Sum Square Residual
SSE	<- t(y)%*%(I-H)%*%y

r20=1-SSE/(n-1)/var(y)

#Q=2
yhat2=(H%*%y)^2

#auxilary regression
x=cbind(X,yhat2)

#Matriks Hat (H)
h	<- x%*%solve(t(x)%*%x)%*%t(x)
##Menghitung Sum Square Residual
sse	<- t(y)%*%(I-h)%*%y

#menghitung R2 baru dibawah h1
r21=1-sse/(n-1)/var(y)
##Menghitung banyak regresor+1
p	<- ncol(x)
#menghitung stat uji
F=((r21-r20)/1)/((1-r21)/(n-(p+1+1)))

#menghitung crit value
Fcrit=qf(0.95,1,n-p)

test_significant=
  if (F >= Fcrit) {
    print("H0 ditolak")
  } else { print("H0 diterima")
  }

#cek pake syntax jadi
model=lm(y~x0)
summary(model)

