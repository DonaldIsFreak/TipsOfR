#Practice the regressive algo.
x1 = c(0,0,0,0,0);
x2 = c(0,1,0,0,1);
x3 = c(0,0,0,1,0);
x4 = c(0,0,1,0,0);
x5 = c(0,1,0,0,1);

alpha = 0.2;
phi = 0.01;
lambda = alpha/phi;

w = diag(c(1,1,1,1,1));
thelta = runif(5,0,1);
#thelta = c(0.4940,0.2661,0.0907,0.9478,0.0737);

x = cbind(x1,x2,x3,x4,x5);

#Tuning the weight component
for (i in 1:20){
  y = rowSums(t(x)*w) - thelta;
  delta_w = phi*y*(lambda*x-w);
  w = w + delta_w;
}

X = c(1,0,0,0,1);

Y = sign(w%*%X-thelta);

