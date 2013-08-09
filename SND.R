#Draw Standard Normal Distribution Graph

N=1000;

x=seq(-5,5,length=N);

y=dnorm(x,mean=0,sd=1);

plot(y~x,type="l",lwd="2",col="red",lab=c(20,20,10));

#abline(v=-5:5);

#The above code same as
curve(dnorm,-5,5,lwd="1",add=T,col="green");