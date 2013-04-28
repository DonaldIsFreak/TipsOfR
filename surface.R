# Draw the 3D surface 
library(rgl);

funs1 = function(x,y){
  return (8*x^3+2*x*y-3*x^2+y^2-1);
}


funs2 = function(x,y){
  return(x+2*exp(1)*y-exp(1)^x-exp(1)^(2*y));
}
x = seq(-1,1,by=0.01);

y = seq(-1,1,by=0.01);

z = outer(x,y,funs1)

surface3d(x,y,z)