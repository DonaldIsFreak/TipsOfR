#Exercise 4.18

n = 2000;
x = c(25,75,125,175,225,275,350,450,600);
f = c(101,434,563,414,240,124,80,20,24);

#Q1
mean = sum(x*f)/n; mean;

median = 100+(1000-535)*50/563; median;

mode = (100+150)/2; mode;

#Q2
Q1 = 50+(500-101)*50/434; Q1;

Q3 = 150+(1500-1098)*50/414; Q3;

#Q3
D1 = 50+(200-101)*50/434; D1;

D9 = 250+(1800-1752)*50/124; D9;

#Q4
range = 600-25; range;

IQR = Q3-Q1; IQR;

MAD = sum(abs(x-mean)*f)/n; MAD;

#Q5
var = sum((x-mean)^2*f)/(n-1); var;

sd = sqrt(var); sd;