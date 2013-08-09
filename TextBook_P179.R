# Textbook: 統計學方法與應用4ed(v2)
# Table 14.12 P.179

n=5;
k=3;

df1=(k-1);
df2=(n*k-k);

#Median of absolute deviation on each vectors.
a=c(2,24,0,6,14);
b=c(5,0,3,8,5);
c=c(0,4,5,0,6);

y.mean=sapply(list(a,b,c),function(x)sum(x)/n); y.mean;

y.var= sapply(list(a,b,c),function(x)var(x)); y.var;

y_mean2= (n*sum(y.mean))/(k*n); y_mean2;


ssf=n*sum((y.mean-y_mean2)^2); ssf;

sse=(n-1)*sum(y.var); sse;

msf=ssf/df1; msf;

mse=sse/df2; mse;

f.v=msf/mse; f.v;

f.tlv=qf(.05,df1,df2,lower.tail=F); f.tlv;

f.p=pf(f,df1,df2,lower.tail=F); f.p;