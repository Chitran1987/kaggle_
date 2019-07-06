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
plot(mat[2,], mat[3,], xlab = 'salinity', ylab = 'temp', main = 'depth=0m', col='green')
dev.off()
