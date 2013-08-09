#Exercise 8.8

#X
x = 1:4; x;

#Y
y = 0:1;

#f(X,Y=0)
fy_0 = c(0.1,0.1,0.1,0.2); fy_0;

#f(X,Y=1)
fy_1 = c(0.1,0.2,0.05,0.15); fy_1;

#E(X|Y=1)
Exy_1 = sum((fy_1/0.5)*x); Exy_1;

#Std(X|Y=1)
Stdxy_1 =  sqrt(sum(x^2*(fy_1/0.5))-2.5^2); Stdxy_1;

#fx
fx = fy_0+fy_1; fx;

#fy
fy = c(sum(fy_0),sum(fy_1)); fy;

#E(X)
Ex = sum(x*fx); Ex;

#E(Y)
Ey = sum(y*fy); Ey;

#Exy
Exy = sum(y[1]*x*fy_0+y[2]*x*fy_1);

#Cov(X,y)
Covxy = Exy-Ex*Ey; Covxy;

#Stdx
Stdx = sqrt(sum(x^2*fx)-Ex^2); Stdx;

#Stdy
Stdy = sqrt(sum(y^2*fy)-Ey^2); Stdy;

#Pxy
Pxy = Covxy/(Stdx*Stdy); Pxy;