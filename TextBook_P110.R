# Textbook: 統計學方法與應用4ed,v2
# Example 13.11 P.110
# 改用ANOVA驗證t^2=F
s_mean = c(6.24,5.996);
s_var = c(0.9908,0.9904);
s_n = c(25,25);

y_mean2 = round(sum(s_mean*s_n)/sum(s_n),2); y_mean2;

ssf = 25*sum((s_mean-y_mean2)^2); ssf;
sse = sum((s_n-1)*s_var); sse;

msf = ssf/1; msf;
mse = sse/48; mse;

f_v = msf/mse; f_v;