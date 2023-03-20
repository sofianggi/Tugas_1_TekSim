Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("az(i-1)+c","Xi","Ui")
  for (i in 1:n)
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[1,2]/m
    z0<-xi[1,2]
  }
  hist(xi[,3])
  View(xi)
}
Additive_RNG(45, 21139, 437, 417, 150)

Bernouli_2<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-(X<=p+0)
  (tabel<-table(Y)/length(Y))
}
Bernouli_2(150, 0.83)
