
movies<-read.csv("http://files.grouplens.org/datasets/movielens/ml-100k/u.item", header = FALSE, sep = "|")

str(movies)
head(movies)
colnames(movies)=c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary","Drama","Fantasy","FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller","War","Western")
str(movies)
movies$ID=NULL
movies$ReleaseDate=NULL
movies$VideoReleaseDate=NULL
movies$IMDB=NULL
movies[,c("ID","ReleaseDate","VideoReleaseDate", "IMDB")]<-NULL
movies[,c(1,3,4,5)]<-NULL
which(table(movies$Title)>1)
class(movies)
str(movies)
movies<-unique(movies)
which(table(movies$Title)>1)
str(movies)
library(caTools)
set.seed(50)
spl<-sample.split(movies$Title,SplitRatio = 0.8)
table(spl)
train<-subset(movies,spl==T)
test<-subset(movies,spl==F)
# Quick Question 

table(movies$Comedy)
table(movies$Western)
table(movies$Romance,movies$Drama)
distance<-dist(movies[2:20],method = "euclidean")
distance_train<-dist(train[2:20],method = "euclidean")
head(distance_train)
str(distance)
head(distance)
summary(distance)
clusterMovies<-hclust(d = distance,method="ward.D")
clusterTrain<-hclust(d=distance_train,method = "ward.D")
plot(clusterMovies)
plot(clusterTrain)
clusterGroups<-cutree(clusterMovies,k=10)
GroupTrain<-cutree(clusterTrain,k=10)
table(GroupTrain)
table(clusterGroups)
table(clusterGroups,movies$Action)
tapply(movies$Action,clusterGroups,mean)
tapply(movies$Romance,clusterGroups,mean)
round(tapply(movies$Comedy,clusterGroups,mean),2)
colMeans(subset(movies[2:20], clusterGroups == 1))
agg<-aggregate(movies[2:20],list(clusterGroups),mean)
agg_train<-aggregate(train[2:20],list(GroupTrain),mean)
agg_train<-data.frame(agg_train,row.names = 1)
agg<-data.frame(agg,row.names = 1)
trans<-data.frame(t(agg),check.names = T)
trans_train<-data.frame(t(agg_train),check.names = T)
movies$cluster<-clusterGroups
SelectMovie<-sqldf("Select cluster from movies[,2:20] group by cluster")
library(ggplot2)
qplot(clusterGroups,geom="bar")
match("Men in Black (1997)",table = movies$Title)
grep("men in black",x = movies$Title,ignore.case = T)
movies[257,] 
spl = split(movies[2:20], clusterGroups)
spl[[1]]
lapply(spl, colMeans)

movies$clusterGroups<-clusterGroups
# Recommendation Systems
library(sqldf)
grep(pattern = "men in black",x = movies$Title,ignore.case = T)
sqldf("select * from movies where Title like '%Men in Black%' ")
#OR 
subset(movies,Title=="Men in Black (1997)")
clusterGroups[257]
movies2<- subset(movies,clusterGroups==2)
movies2$Title
head(movies2$Title,10)
library(flexclust)
hclust.KCCA<-as.kcca(object = clusterTrain,k = 10,data = train[2:20])
pred<-predict(hclust.KCCA,newdata=test[2:20])
table(pred)
test_final<-cbind(test,pred)
verify<-subset(test,pred==10)
# Quick Question
clusterGroups2<-cutree(clusterMovies,k=2)
colMeans(subset(movies[2:20], clusterGroups2 == 2))
colMeans(subset(movies[2:20], clusterGroups2 == 1))
agg2<-aggregate(movies[2:20],list(clusterGroups2),mean)

# Applying kmeans
set.seed(50)
KMC<-kmeans(x = train[2:20],centers = 10,iter.max = 100)
KMC$iter
KMC$centers
table(KMC$cluster)
agg_KMC<-aggregate(x = train[2:20],by = list(KMC$cluster),FUN = mean)
KMC_KCCA<-as.kcca(object = KMC,data = train[2:20])
summary(KMC_KCCA)
pred_KMC<-predict(object = KMC_KCCA,newdata=test[2:20])
table(pred_KMC)
test_KMC<-cbind(test,pred_KMC)
verify<-subset(test,pred_KMC==9)
SumWithinss = sapply(2:15, function(x) {kmeans(train[2:20], centers=x, iter.max=1000)$tot.withinss})
SumWithinss
NumClusters<-seq(2,15,1)
plot(NumClusters,SumWithinss,type="b")


# Applying kmeans
set.seed(50)
KMC<-kmeans(x = movies[2:20],centers = 10,iter.max = 100)
KMC$cluster[257]
table(KMC$cluster)
head(subset(movies$Title,KMC$cluster==1),10)
KMC$size
KMC$centers
KMC$iter
KMC$totss
KMC$withinss
KMC$tot.withinss
KMC$betweenss
set.seed(50)
KMC2<-kmeans(x = movies[2:20],centers = 2,iter.max = 100)
KMC2$tot.withinss
agg_KMC<-aggregate(x = movies[2:20],by = list(KMC$cluster),FUN = mean)
KMC_KCCA<-as.kcca(object = KMC,data = train[2:20])
pred_KMC<-predict(object = KMC_KCCA,newdata=test[2:20])
test_KMC<-cbind(test,pred_KMC)
verify<-subset(test,pred_KMC==8)

SumWithinss = sapply(2:20, function(x) {set.seed(50) ; kmeans(movies[2:20], centers=x, iter.max=1000)$tot.withinss})
SumWithinss
NumClusters<-seq(2,20,1)
plot(NumClusters,SumWithinss,type="b")



