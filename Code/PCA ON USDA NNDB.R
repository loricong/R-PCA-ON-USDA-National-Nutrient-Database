nndb_flat <- read.csv("~/Desktop/GRADUATE/5703/100980213-YC-ASSIGNMENT2/Data/nndb_flat.csv")
#check for highly correlated features
cor(nndb_flat[,8:45])
#delete _USRDA & name
nndb_du<-nndb_flat[-c(1:7,31:45)]
View(nndb_du)
library(car)
library(plyr)
library(psych)
#explore distribution of data
multi.hist(nndb_du)

#transformation
library(rcompanion)
#square root transformation
nndb_sq=sqrt(nndb_du)
View(nndb_sq)
multi.hist(nndb_sq)
cor(nndb_sq)
colMeans(nndb_sq)
apply(nndb_sq, 2, sd)
#cube root transformation
nndb_cub=sign(nndb_du)*abs(nndb_du)^(1/3)
View(nndb_cub)
multi.hist(nndb_cub)
colMeans(nndb_cub)
apply(nndb_cub, 2, sd)
#log transformation
nndb_log=log(nndb_du)
View(nndb_log)
multi.hist(nndb_log)
colMeans(nndb_log)
apply(nndb_log, 2, sd)
#cub is better

#to account for different scales of measurement, standardize
f.data.std<-function(data) {
  data<-as.matrix(data)
  bar<-apply(data,2,mean)
  s<-apply(data,2,sd)
  t((t(data)-bar)/s)
}

nndb_c_s=f.data.std(nndb_cub)


colMeans(nndb_cub)
apply(nndb_cub, 2, sd)

round(colMeans(nndb_c_s))
apply(nndb_c_s, 2, sd)


#pca
library(stats)

#calculate eigenvalues & eigenvectors.
##(nndb_cov<-cov(nndb_c_s))
##(nndb_eigen<-eigen(nndb_cov)) #same as prcomp, eigenvalue=std^2, eigenvector=rotation

(pc.nndb_c_s<-prcomp(nndb_c_s)) 
(pc.nndb_c_sr<- -pc.nndb_c_s$rotation)


##plot(pc.nndb_c_s,col=heat.colors(23))
##abline(h=1,lty=2)

summary(pc.nndb_c_s)


(pc_var<-pc.nndb_c_s$sdev^2) #variance
(pc.varex<-pc_var/sum(pc_var)) #proportion of variance explained
(cumsum(pc.varex)) #cumulative proportion of variance explained
plot(pc.varex,xlab="Principal Component", 
    ylab="Proportion of Vatiance Explained", type="b")
plot(cumsum(pc.varex),xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained",
    type="b")

#library(devtools)
#install_github("vqv/ggbiplot")
#library(ggbiplot)
#ggbiplot(pc.nndb_c_s,scale=0)

library(hornpa)
hornpa(k=23,size=8618, seed=1234)
#keep first 5 components. account for 72.61% of the variance.

