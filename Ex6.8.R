#Exercise 6.8

x=seq(10,50,by=10);
f=c(0.35,0.25,0.2,0.1,0.1);

#E(X)
Ex=sum(x*f); Ex;

#V(X)
Vx=sum(x^2*f)-Ex^2; Vx;

y=c(15,28,41,54,67);

#E(Y)
Ey=sum(y*f); Ey;

#V(Y)
Vy=sum(y^2*f)-Ey^2; Vy;