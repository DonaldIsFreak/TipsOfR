# Textbook: 統計學方法與應用4ed,v2
# Table 14.14 P.184
k = 4;
n = 23;
alpha = 0.05;
df1 = k-1;
df2 = n-k;

m1 = c(65,87,73,79,81,69);
m2 = c(75,69,83,81,72,79,90);
m3 = c(59,78,67,62,83,76);
m4 = c(94,89,80,88);

v_n = sapply(list(m1,m2,m3,m4),length);
y_mean = sapply(list(m1,m2,m3,m4),mean); y_mean;
s_var = sapply(list(m1,m2,m3,m4),var); s_var;

y_mean2 = sum(c(m1,m2,m3,m4))/n; y_mean2;

ssf = sum(v_n*(y_mean-y_mean2)^2); ssf;
msf = ssf/df1; msf;

sse = sum((v_n-1)*s_var); sse;
mse = sse/df2; mse;

f_value = msf/mse; f_value;

f_tlv = qf(alpha,df1,df2,lower.tail = F); f_tlv;

if (f_value>f_tlv) cat("No accept H0") else cat("No reject H0");

p_value = pf(alpha,df1,df2,lower.tail = F); p_value;




