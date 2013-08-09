#Exercise 6.10

x=seq(220,160,by=-10);

f=c(0.04,0.1,0.2,0.27,0.21,0.1,0.08);

#E(X)
Ex=sum(x*f); Ex;

#V(X)
Vx=sum(x^2*f)-Ex^2; Vx;

