raw_df<-read.csv('X:/Dido/Drive/Didos_self/Career/Future/kaggle/datasets/bottle.csv')
dep<-raw_df$Depthm
dep_o<-dep[dep==0]
length(dep_o)
L<-0.5
hist(dep[dep<L], col = 'blue', breaks=4, xlim = c(0,L) )
dep_50<-dep[dep==0]
length(dep_50)
ind<-0
for (i in 1:length(dep)) {
  if(dep[i]==0){
    ind<-c(ind,i)
  }
}
ind<-ind[ind!=0]

#Using the data to work on objects with measurements done at depth <50m
dep_L_raw<-raw_df$Depthm[ind]
sa_L_raw<-raw_df$Salnty[ind]
Tem_L_raw<-raw_df$T_degC[ind]
sum(is.na(sa_L_raw))
sum(is.na(Tem_L_raw))
mat<-matrix(c(dep_L_raw, sa_L_raw, Tem_L_raw), nrow = 3, byrow = T)
dim(mat)
row.names(mat)<-c('depth', 'salinity', 'temperature')

#start using a loop to clean data

for (i in 1:dim(mat)[2]) {
if(is.na(mat[2,i])==TRUE | is.na(mat[3,i])==TRUE){
  mat[,i]<-NA
}  
}

#subset the numeric data and then create the cleaned matrix
dep_L_raw<-mat[1,][is.na(mat[1,])==F]
sa_L_raw<-mat[2,][is.na(mat[2,])==F]
Tem_L_raw<-mat[3,][is.na(mat[3,])==F]
mat<-matrix(c(dep_L_raw, sa_L_raw, Tem_L_raw), nrow = 3, byrow = T)
row.names(mat)<-c('depth', 'salinity', 'temperature')
dim(mat)
sum(is.na(mat[3,]))

#plot the matrix
plot(mat[3,], mat[2,], xlab = 'temp', ylab = 'salinity', main = 'depth=0m', col='green', xlim = c(0, 1.1*max(mat[3,])), ylim = c(0, 1.1*max(mat[2,])))
dev.off()
dim(m)
dim(mat)

#Call the DataAnalyze library and fit the fit_2D function
x<-mat[3,]
f<-function(v){
  y<-v[1]*x+v[2]
  return(y)
}
library(DataAnalyze1.0)
dat<-matrix(c(mat[2,],mat[3,]), nrow = 2, byrow = T)
v<-c(2,0)
lines(x,f(v))
help("lines")
lines(x,x^2)
line(x,x^2)
lines(x,f(v), col='red')
plot(x,f(v))
min(x)
fit_2D(f,op_v = v, it=20000, dat = dat )
v<-c(0.136, 31.206)
lines(x,f(v))
