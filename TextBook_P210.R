# Textbook: 統計學方法與應用4ed,v2
# Table 14.36 P.210
alpha = 0.05;
r = 2;
c = 3;
n = 4;
tn = r*c*n;

df_ssfa = r-1;
df_ssfb = c-1;
df_ssi = df_ssfa*df_ssfb;
df_sse = r*c*(n-1);

all_data=array(c(20.91,12.27,11.82,15.1,25.91,26.82,25,25.95,
                 30.46,27.73,25,27.76,13.19,12.27,10,11.8,20.01,26.36,
                 19.09,21.75,19.1,27.27,21.82,22.07),dim=c(n,c,r));

y_mean_ij = sapply(1:r,function(j) sapply(c(1:c),function(i) mean(all_data[,i,j])));
y_mean_i = round(sapply(1:r,function(i) (n*sum(y_mean_ij[,i]))/(n*c)),2);
y_mean_j = round(sapply(1:c,function(j) (n*sum(y_mean_ij[j,]))/(n*r)),2);

y_mean2 = round(sum(all_data)/tn,2); y_mean2;

sst = round(sum(all_data^2)-tn*y_mean2^2,2); sst;
ssfa = round(((c*n)*sum(y_mean_i^2))-tn*y_mean2^2,2); ssfa;
ssfb = round(((r*n)*sum(y_mean_j^2))-tn*y_mean2^2,2); ssfb;
sse = round(sum(all_data^2) - (n*sum(y_mean_ij^2)),2); sse;
ssi = sst-ssfa-ssfb-sse; ssi;

msfa = round(ssfa/df_ssfa,2); msfa;
msfb = round(ssfb/df_ssfb,2); msfb;
msi = round(ssi/df_ssi,2); msi;
mse = round(sse/df_sse,2); mse;

fv_a = round(msfa/mse,2); fv_a;
fv_b = round(msfb/mse,2); fv_b;
fv_i = round(msi/mse,2); fv_i;

f_tlv_a = qf(alpha,df_ssfa,df_sse,lower.tail=F); f_tlv_a;
f_tlv_b = qf(alpha,df_ssfb,df_sse,lower.tail=F); f_tlv_b;
f_tlv_i = qf(alpha,df_ssi,df_sse,lower.tail=F); f_tlv_i;