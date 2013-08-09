#Exercise 6.5

x=seq(20,90,by=10);

f=c(0.05,0.1,0.1,0.1,0.25,0.25,0.1,0.05);

#E(X)
Ex=sum(x*f); Ex;

#V(X)
Vx=sum((x-Ex)^2*f); Vx;