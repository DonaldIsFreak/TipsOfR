#Exercise 8.3

#X
x = c(-2,-1,0,1,2); x;

#f(X)
f = c(2,4,3,1,2)/12; f;

#E(X)
Ex = sum(x*f); Ex;

#E(X^2)
Ex2 = sum(x^2*f); Ex2;

#Y=(2X-1)^2
y = (2*x-1)^2; y;

#E(Y)
Ey = sum(y*f); Ey;

#E(Y^2)
Ey2 = sum(y^2*f); Ey2;

#f(x,y)
fxy = as.vector(outer(f,f)); fxy;

#Cov(X,Y)
CovXY = sum(x*y*fxy)-(Ex*Ey); CovXY;

#stdX
stdX = sqrt(sum(x^2*f)-Ex^2); stdX;

#stdY
stdY = sqrt(sum(y^2*f)-Ey^2); stdY;

#\rhoXY
rhoXY = CovXY/(stdX*stdY); rhoXY;



