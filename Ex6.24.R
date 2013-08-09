#Exercise 6.24

f=function(x,K,n,N)(choose(K,x)*choose(N-K,n-x))/choose(N,n);

#Q1:P(X>=1)=1-P(0)
Q1=1-f(0,3,4,50); Q1;

n=0;
repeat{
	if((1-f(0,3,n,50))>0.5) break;
	n=n+1;
};
n;