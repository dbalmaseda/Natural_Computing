require(tree)
library(randomForest)
library(dplyr)

df<-read.csv('breast-cancer-wisconsin.data',header = FALSE) %>% select(V2,V3,V4,V5,V6,V7,V8,V9,V10,V11)
df$V11<-as.factor(df$V11)
df$V2<-as.numeric(df$V2)
df$V3<-as.numeric(df$V3)
df$V4<-as.numeric(df$V4)
df$V5<-as.numeric(df$V5)
df$V6<-as.numeric(df$V6)
df$V7<-as.numeric(df$V7)
df$V8<-as.numeric(df$V8)
df$V9<-as.numeric(df$V9)
df$V10<-as.numeric(df$V10)

##Impute NA values for PCA
library(mice)
imp<-mice(df)
df_i<-complete(imp)

##Feature selection
library(factoextra)
variables<-df_i %>% select(V2,V3,V4,V5,V6,V7,V8,V9,V10)
res.pca <- prcomp(variables, scale = F)
fviz_eig(res.pca)
fviz_pca_biplot(res.pca,
                col.var = "contrib", # Color by contributions to the PC
                col.ind = 'grey',
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE     # Avoid text overlapping
)
df_f<-df_i %>% select(V2,V3,V4,V5,V6,V7,V8,V9,V11)
colnames(df_f)<-c('a','b','c','d','e','f','g','h','y')

##Split data
set.seed(123)
ta<-round(length(df_f$y)*80/100)
index<-sample(1:nrow(df_f),ta)
train<-df_f[index,]
test<-df_f[-index,]

##Parameters
acc<-c()
for(i in 1:80){
  rtree<-randomForest(train$y~.,data=train, ntree=i)
  pred<-predict(rtree, test, type='class')
  t<-table(pred, true=test$y)
  acc<-c(acc, t[1,1]+t[2,2]/140)
}
plot(acc,xlab = 'Number of trees',ylab = 'Accuracy')
  
##Decision Tree
dtree<-tree(train$y~.,data=train)
pred<-predict(dtree, test, type='class')
table(pred, true=test$y)

##Random Forest
rtree<-randomForest(train$y~.,data=train, ntree=30)
pred<-predict(rtree, test, type='class')
table(pred, true=test$y)
