raw_df<-read.csv('X:/Dido/Drive/Didos_self/Career/Future/kaggle/datasets/bottle.csv')
dep<-raw_df$Depthm
hist(dep[dep<50], col = 'blue', breaks=4, xlim = c(0,200) )
dep_50<-dep[dep<50]
length(dep_50)
ind<-0
for (i in 1:length(dep)) {
  if(dep[i]<=50){
    ind<-c(ind,i)
  }
}
ind<-ind[ind!=0]

#Using the data to work on objects with measurements done at depth <50m
dep_50_raw<-raw_df$Depthm[ind]
sa_50_raw<-raw_df$Salnty[ind]
Tem_50_raw<-raw_df$T_degC[ind]
sum(is.nan(sa_50_raw))
sum(is.nan(Tem_50_raw))
mat<-matrix(c(dep_50_raw, sa_50_raw, Tem_50_raw), nrow = 3, byrow = T)
dim(mat)
row.names(mat)<-c('depth', 'salinity', 'temperature')



