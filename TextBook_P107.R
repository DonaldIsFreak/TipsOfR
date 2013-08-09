# Textbook: 統計學方法與應用4ed,v2
# Example 13.10 P.107
# 改用ANOVA驗證t^2=F
y_mean2 = round((77.73*15+86.5*12)/27,2); y_mean2;

ssf = round(15*(77.73-y_mean2)^2+12*(86.5-y_mean2)^2,2); ssf;
sse = round(14*4.42^2+11*4.27^2,2); sse;

msf = round(ssf/1,2); msf;
mse = round(sse/25,2); mse;

f = round(msf/mse,2); f;

Sp = round(sqrt(14*4.42^2+11*4.27^2)/25,2); Sp;

t = round((77.73-86.5)/(Sp*sqrt(1/15+1/12)),2); t;