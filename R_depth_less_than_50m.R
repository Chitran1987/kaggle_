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

#start using a loop to clean data

for (i in 1:dim(mat)[2]) {
if(is.na(mat[2,i])==TRUE | is.na(mat[3,i])==TRUE){
  mat[,i]<-NA
}  
}

#subset the numeric data and then create the cleaned matrix
dep_50_raw<-mat[1,][is.na(mat[1,])==F]
sa_50_raw<-mat[2,][is.na(mat[2,])==F]
Tem_50_raw<-mat[3,][is.na(mat[3,])==F]
mat<-matrix(c(dep_50_raw, sa_50_raw, Tem_50_raw), nrow = 3, byrow = T)
row.names(mat)<-c('depth', 'salinity', 'temperature')
dim(mat)
sum(is.na(mat[3,]))
plot(mat[2,], mat[3,], xlab = 'salinity', ylab = 'temp')
