#Exercise 6.11

x=seq(0,3);
f=c(1/2,1/4,1/8,1/8);

#E(X)
Ex=sum(x*f); Ex;

#V(X)
Vx=sum(x^2*f)-Ex^2; Vx;