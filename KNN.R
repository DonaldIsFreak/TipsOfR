group=rbind(c(1.0, 1.1), c(1.0, 1.0), c(0, 0), c(0, 0.1));
labels = c('A', 'A', 'B', 'B'); 

dataset = data.frame(group,labels);
names(dataset)=c('x','y','label');

classify = function(inX,K){
  n=nrow(dataset);
  distance=sqrt(apply((matrix(rep(inX,n),nrow=n)-dataset[1:2])^2,1,sum));
  sorted=sort(distance,index.return=T)$ix;
  voteLabel=dataset[sorted[1:K],]$label;
  tb = table(voteLabel);
  return(names(tb[which(tb[1:length(tb)]==max(tb))]));
}

result = classify(c(0.1,0),3);
result;