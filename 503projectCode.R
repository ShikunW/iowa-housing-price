library(reshape2)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(MASS)
require(class)
setwd("/Users/shikunwang/Desktop/STATS503/final project")
train=read.csv("train.csv",sep=",",header=T)
test=read.csv("test.csv",sep=",",header=T)
imp=read.csv("code/implement.csv")
set.seed(503)

#https://www.kaggle.com/prise6/house-prices-advanced-regression-techniques/stacking-lasso-gbm-rf-stacking-0-118/code
#################################### preprocess ################################################################
preprocess=function(x){
  x$age=x$YrSold-x$YearBuilt
  x$Cost_of_living=NULL
  x$Income=NULL
  x$Annual_property_tax=NULL
  x$School=NULL
  x$Crime=NULL
  x$lSalePrice=log(x$SalePrice)
  for(j in 1:nrow(x)){
    i=match(x$Neighborhood[j],table=imp$Neighborhood)
    x$Cost_of_living[j]     =ifelse(is.na(imp$Cost_of_living[i]),-18,imp$Cost_of_living[i])
    x$Income[j]             =ifelse(is.na(imp$Income[i]),46358,imp$Income[i])
    x$Owners[j]             =ifelse(is.na(imp$Owners[i]),43,imp$Owners[i])
    x$Annual_property_tax[j]=ifelse(is.na(imp$Annual_property_tax[i]),2479,imp$Annual_property_tax[i])
    x$School[j]             =ifelse(is.na(imp$School[i]),8,imp$School[i])
    x$Crime[j]              =ifelse(is.na(imp$Crime[i]),-28,imp$Crime[i])
    x$LotFrontage[j]        =ifelse(is.na(x$LotFrontage[j]),0,x$LotFrontage[j])
    x$GrLivArea[j]          =ifelse(is.na(x$GrLivArea[j]),0,x$GrLivArea[j])
    x$TotalBsmtSF[j]        =ifelse(is.na(x$TotalBsmtSF[j]),0,x$TotalBsmtSF[j])
    x$MasVnrArea[j]        =ifelse(is.na(x$MasVnrArea[j]),0,x$MasVnrArea[j])
    
  }
  
  l1=sort(x$lSalePrice)[nrow(x)*.1]
  l2=sort(x$lSalePrice)[nrow(x)*.25]
  l3=sort(x$lSalePrice)[nrow(x)*.5]
  l4=sort(x$lSalePrice)[nrow(x)*.75]
  l5=sort(x$lSalePrice)[nrow(x)*.9]
  
  x$class=NULL
  for(i in 1:nrow(x)){
    p=x$lSalePrice[i]
    if(p<l1) x$class[i]=1
    else if (p<l2) x$class[i]=2
    else if (p<l3) x$class[i]=3
    else if (p<l4) x$class[i]=4
    else if (p<l5) x$class[i]=5
    else x$class[i]=6
  }
  
  for(i in 1:nrow(x)){
    p=x$lSalePrice[i]
    if(p<l1) x$class[i]="poor"
    else if (p<l2) x$class[i]="lesspoor"
    else if (p<l3) x$class[i]="moderate"
    else if (p<l4) x$class[i]="good"
    else if (p<l5) x$class[i]="superb"
    else x$class[i]="best"
  }
  
  y=NULL
  for(i in 1:ncol(x)){
    if(is.numeric(x[,i])==T)
      y=cbind(y,i)
  }
  

  return(x)
}

x=preprocess(train)
z=data.frame(x[,y])
z=z[,-c(1,2,7,8,20,21,26,36,37,38,40,46)]
z$class=x$class

test=preprocess(test)

####################################  summary  ##########################################
## price data
hist(x$SalePrice)
hist(log(x$SalePrice))

boxplot(x$SalePrice)
boxplot(log(x$SalePrice))

qqnorm(x$SalePrice)
qqnorm(log(x$SalePrice))

str(x)
summary(x)

#hist

for(i in 1:1){
  q=ggplot(data=x,aes(factor(x$Street),fill=x$Street))+theme(legend.position="none")
  print(q+geom_bar()+xlab("")+ylab("")+theme_bw()+theme(axis.text = element_blank()))
}
for(i in 1:ncol(x)){p<-ggplot(x,aes(x=x[,i],fill=x[,i])) +  geom_histogram(stat="count")+xlab("")+
  ggtitle(names(x)[i])+theme(legend.position="none",
                             axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 45));print(p)
}

#boxplot

ggplot(data=x,aes(x=x$Street,y=x$SalePrice,fill=x$Street))+
 geom_boxplot()+theme_bw()+theme(legend.position="bottom")+theme(axis.text = element_blank())

#heat plot

t=z[,-c(1,2,3,4,15,12,17,28,27,22,26,14,23,24,25,30,31,32,33,34,35)]
co=cor(t)
co[upper.tri(co)] = NA
m=melt(co)
m$Var1=factor(m$Var1);m$Var2=factor(m$Var2)
g = ggplot(m, aes(x=Var1, y=Var2, fill=value))+xlab('X-labels')+ylab("Y-labels")+
  geom_tile(col="white")+scale_fill_distiller(limits=c(-1, 1), palette='RdBu', na.value='white',
                                              name='Correlation')+
  ggtitle('Correlations')+
  coord_fixed(ratio=1) +
  theme_minimal() +
  scale_y_discrete(position="right") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        legend.position=c(0.1,0.9),
        legend.justification=c(0,1))
g

# show level of price
summary(x$lSalePrice)

hist(x$lSalePrice,nclass=50)
abline(v=sort(x$lSalePrice)[nrow(z)*.1],col=2)
abline(v=sort(x$lSalePrice)[nrow(z)*.25],col=2)
abline(v=sort(x$lSalePrice)[nrow(z)*.5],col=2)
abline(v=sort(x$lSalePrice)[nrow(z)*.75],col=2)
abline(v=sort(x$lSalePrice)[nrow(z)*.9],col=2)

#################################### classification #######################################

#
# linear regression
#
# Note: class=1,2,3,4,5,6

lmm=lm(class~.,z)
summary(lmm)
(which(abs(lmm$residuals)>10))
plot(residuals(lmm))
hist(residuals(lmm))


#
# PCA
#

X=as.matrix(z[,-35])
pca = princomp(X, cor=T)
loadings(pca)
pca$sdev^2
require(ggplot2)
plot(pca)
scree = qplot(1:length(pca$sdev), pca$sdev, geom='line',
              ylab='Component Standard Deviation', xlab='Component')
scree
require(ggbiplot)

ggbiplot(pca)

#
# FA
#

data_FA = factanal(covmat = cor(X), factors = 2)


#
# classification
#


#
# Classification trees
#

control = rpart.control(cp = 0.000, xxval = 100, minsplit = 2)
election_tr = rpart(class ~ ., data = z, control = control)
plotcp(election_tr)

printcp(election_tr)

selected_tr <- prune(election_tr, cp= election_tr$cptable[which.min(election_tr$cptable[,"xerror"]),"CP"])
quartz()
fancyRpartPlot(selected_tr)

printcp(selected_tr)


class.pred <- table(predict(selected_tr, type="class"), z$class)
1-sum(diag(class.pred))/sum(class.pred)  # error = 0.2315068

#
# Random forest
#

library(randomForest)
z$class=factor(z$class)
election_rf = randomForest(class~., mtry= 10, ntree= 500, data=z)
election_rf
varImpPlot(election_rf)

class.pred <- table(predict(election_rf, type="class"), z$class)
1-sum(diag(class.pred))/sum(class.pred)  # error = 0.3136986

plot(election_rf)
tuneelection_rf = tuneRF(x =z[,-35], y = z$class)

#
# LDA
#

irislda = lda(data=z,class~.)
irispred = predict(irislda, z[,-35])
ta=table(z$class, irispred$class)
1-sum(diag(ta))/sum(ta)  # error = 0.3424658

#
# Diagonal LDA (Naive Bayes)
#

library(sparsediscrim)
irisdlda <- dlda(data=z,class~.)
irispred = predict(irisdlda, z[,-35])
ta=table(z$class, irispred$class)
1-sum(diag(ta))/sum(ta)  # error = 0.3910959

#
# Multinomial Logistic Regression
#

iris_train = iris[train,]
iris_test = iris[-train,]

mlogit = multinom(Species ~ ., data=iris_train)

#
# KNN
#

autoknn = knn(train = z[,-35], cl = z$class, test = z[,-35], k = 5)
summary(autoknn)
ta=table(z$class, autoknn)
1-sum(diag(ta))/sum(ta)  # error = 0.3390411



