#Exercise 6.23

x=seq(0,2);
f=function(x)(choose(15,x)*choose(85,20-x))/choose(100,20)

#p(x<=2)
p2=round(sum(f(x)),3); p2;

#1-p(x<=2)
1-p2;
