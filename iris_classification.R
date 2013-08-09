#Predicting the type of a flower
library(rpart)
data(iris)

#create model
model=rpart(Species~Petal.Width+Petal.Length,data=iris);

#set x-y coordinate
x=seq(min(iris$Petal.Length),max(iris$Petal.Length),length=50);
y=seq(min(iris$Petal.Width),max(iris$Petal.Width),length=50);
feature = expand.grid(Petal.Length=x,Petal.Width=y);

#create predict object
class = apply(predict(model,feature),1,function(one_row) return(which(one_row==max(one_row))));

#display result
plot(iris[3:4],pch=21,bg=c("red","green","blue")[unclass(iris$Species)]);
contour(x,y,matrix(class,length(x)),levels=c(1.5,3),add=T,labex=0)

