# Textbook: 統計學方法與應用4ed,v2
# Table 14.29 P.201
alpha<-0.05;
r = 2;
c = 3;
n = r*c;

all_data=array(c(30,23,38,29,42,36),dim=c(r,c));

df_ssfa = r-1;
df_ssfb = c-1;
df_sse = df_ssfa*df_ssfb;

y_sum_i = sapply(1:r,function(i)sum(all_data[i,]));
y_mean_i = y_sum_i/c; y_mean_i;

y_sum_j = sapply(1:c,function(j)sum(all_data[,j]));
y_mean_j = y_sum_j/r; y_mean_j;

y_mean2 = sum(all_data)/n; y_mean2;

sst = sum(all_data^2)-n*y_mean2^2; sst;
ssfa = c*sum(y_mean_i^2)-n*y_mean2^2; ssfa;
ssfb = r*sum(y_mean_j^2)-n*y_mean2^2; ssfb;
sse = sst-ssfa-ssfb; sse;

msfa = ssfa/df_ssfa; msfa;
msfb = ssfb/df_ssfb; msfb;
mse = sse/df_sse; mse;
          
fv_a = msfa/mse; fv_a;
fv_b = msfb/mse; fv_b;

f_tlv_a = qf(alpha,df_ssfa,df_sse,lower.tail=F); f_tlv_a;
f_tlv_b = qf(alpha,df_ssfb,df_sse,lower.tail=F); f_tlv_b;

pv_a = pf(alpha,df_ssfa,df_sse,lower.tail=F); pv_a;
pv_b = pf(alpha,df_ssfb,df_sse,lower.tail=F); pv_b;


