#Exercise 6.20

x=seq(0,10);

f=choose(10,x)/1024;

#P(X>=7)
p7=sum(f[7:10]); p7;

#P(X<=4)
p4=sum(f[0:4]); p4;