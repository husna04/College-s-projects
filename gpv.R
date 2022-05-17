#Input2 data
peluang=read.csv(file.choose(),sep=",")
peluang1=peluang[,-1]
peluang1
pxT=1-peluang1
b=as.numeric(readline(prompt="Tingkat suku bunga = "))
x=as.integer(readline(prompt="Usia saat ini = "))
B1=as.integer(readline(prompt="Besar Manfaat 1="))
B2=as.integer(readline(prompt="Besar Manfaat 2="))
B3=as.integer(readline(prompt="Besar Manfaat 3="))
cat("Jenis Kelamin? \na.Laki-Laki \nb.Perempuan")
jk=readline(prompt="Jenis kelamin = ")

GPV=function(b,x,jk,B1,B2,B3){
  J=matrix(1,71-x,1)
  for (t in x:70){
    n=70-t
    n
    
    #matriks q
    if (jk=="a") {
      qx1=0.2*peluang1[,1]
      qx2=0.8*peluang1[,1]
    }else {
      qx1=0.2*peluang1[,2]
      qx2=0.8*peluang1[,2]
    }
    qx1
    
    #matriks tpx
    tpx=function(n,t,jk) {
      A=matrix(1,n+1,1)
      if (n==0) {
        A[1,1]=1
      } else {
        A[1,1]=1
        if (jk=="a") {
          A[2,1]=A[1,1]*pxT[t+1,1]
          if (t<=68) {
            for (i in 2:n){
              A[i+1,1]=A[i,1]*pxT[t+i,1]
            }
          }
        } else {
          A[2,1]=A[1,1]*pxT[t+1,2]
          if (t<=68) {
            for (i in 2:n){
              A[i+1,1]=A[i,1]*pxT[t+i,2]
            }
          }
        }
      }
      return(A)
    }
    
    tpx(n,t,jk)
    
    #APV decrement 1
    A1=function(b,n,t,jk) {
      if (n>0) {
        C=matrix(1,n,1)
        for (i in 0:(n-1)){
          C[i+1,1]=((1+b)^-(i+1))*tpx(n,t,jk)[i+1,1]*qx1[t+1+i]
        }
        return(C)
      }
      return(0)
    }
    A1(b,n,t,jk)
    
    #APV decrement 2
    A2=function(b,n,t,jk) {
      if (n>0) {
        D=matrix(1,n,1)
        for (i in 0:(n-1)){
          D[i+1,1]=((1+b)^-(i+1))*tpx(n,t,jk)[i+1,1]*qx2[t+1+i]
        }
        return(D)
      }
      return(0)
    }
    A2(b,n,t,jk)
    
    #APV decrement 3
    A3=function(b,n,t,jk) {
      W=((1+b)^(-n))*tpx(n,t,jk)[n+1,1]
      return(W)
    }
    A3(b,n,t,jk)*B3
    
    #anuitas
    a1=function(b,n,t,jk) {
      if (n>0) {
        E=matrix(1,n,1)
        for (i in 0:(n-1)) {
          E[i+1,1]=((1+b)^(-i))*tpx(n,t,jk)[i+1,1]
        }
        return(E)
      }
      return(0)
    }
    
    a1(b,n,t,jk)
    
    
    #Matriks Biaya
    d1=matrix(c(0.8),ncol=1,nrow=1)
    d2=matrix(c(0.3),ncol=1,nrow=19)
    d3=matrix(c(0.1),ncol=1,nrow=(50-x))
    d4=matrix(c(400000),ncol=1,nrow=1)
    d5=matrix(c(600000),ncol=1,nrow=19)
    d6=matrix(c(200000),ncol=1,nrow=(50-x))
    H=cbind(rbind(d1,d2,d3),rbind(d4,d5,d6))
    
    #Gross Premi
    F=H*as.matrix(cbind(a1(b,70-x,x,jk),a1(b,70-x,x,jk)))
    K=colSums(F)
    L=matrix(c(a1(b,70-x,x,jk)[1:20,]),nrow=20,ncol=1)
    M=colSums(L)
    GP=(colSums(rbind(((B1*A1(b,70-x,x,jk))+(B2*A2(b,70-x,x,jk))),(B3*A3(b,70-x,x,jk))))+K[2])/(M-K[1])
    
    if (t==x) {
      BA=rbind(((B1*A1(b,n,t,jk))+(B2*A2(b,n,t,jk))),(B3*A3(b,n,t,jk)))
      O=BA+rbind((((H[,1]*GP)+H[,2])*a1(b,n,t,jk)),matrix(0,1,1))
      I=GP*rbind(L,matrix(0,21,1))
      J[1,1]=round(colSums(as.matrix(O-I)))
    }else {
      if (t<=69){
        if ((t-x)<=19) {
          BA=rbind(((B1*A1(b,n,t,jk))+(B2*A2(b,n,t,jk))),(B3*A3(b,n,t,jk)))
          Z=((as.matrix(H[-1:(x-t),1])*GP)+as.matrix(H[-1:(x-t),2]))*a1(b,n,t,jk)
          O=BA+rbind(Z,matrix(0,1,1))
          L=matrix(c(a1(b,n,t,jk)[1:(20-t+x),]),nrow=(20-t+x),ncol=1)
          I=GP*rbind(L,matrix(0,21,1))
          J[t-x+1,1]=round(colSums((O-I)))
        } else {
          BA=rbind(((B1*A1(b,n,t,jk))+(B2*A2(b,n,t,jk))),(B3*A3(b,n,t,jk)))
          Z=((as.matrix(H[-1:(x-t),1])*GP)+as.matrix(H[-1:(x-t),2]))*a1(b,n,t,jk)
          Z
          O=BA+rbind(Z,matrix(0,1,1))
          J[t-x+1,1]=round(colSums(O))
        }
      }else {
        J[t-x+1,1]=B3*A3(b,n,t,jk)
      }
    }
  }
  return(J)
}


GPV(b,x,jk,B1,B2,B3)
Z=GPV(b,x,jk,B1,B2,B3)
