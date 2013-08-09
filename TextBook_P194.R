# Textbook: 統計學方法與應用4ed,v2
# Table 14.24 P.194
all_data = array(c(76,78,73,79,80,67,72,75,71,80,78,75,77,86,73,76,69,65,83,78,79,80,87,81,80,82,81,79),dim = c(7,4)); all_data;

alpha = 0.05;
n = length(all_data);
b = 7;
k = 4;

df_ssf = k-1;
df_ssbk = b-1;
df_sse = df_ssf*df_ssbk;

#y_sum_i = sapply(1:7,function(i)sum(all_data[i,]));
y_sum_i = apply(all_data,1,sum); y_sum_i;
y_mean_i = y_sum_i/k; y_mean_i;

#y_sum_j = sapply(1:4,function(j)sum(all_data[,j]));
y_sum_j = apply(all_data,2,sum); y_sum_j;
y_mean_j = y_sum_j/b; y_mean_j;

y_mean2 = sum(all_data)/(b*k); y_mean2;

sst = sum((all_data-y_mean2)^2); sst;

ssf = b*sum((y_mean_j-y_mean2)^2); ssf;

ssbk = k*sum((y_mean_i-y_mean2)^2); ssbk;

sse = sst-ssbk-ssf; sse;

msf = ssf/df_ssf; msf;

msbk = ssbk/df_ssbk; msbk;

mse = sse/df_sse; mse;

f_value = msf/mse; f_value;

fbk_value = msbk/mse; fbk_value;

f_tlv = qf(alpha,df_ssf,df_sse,lower.tail = F); f_tlv;

if (f_value>f_tlv) cat("No accept H0") else cat("No reject H0");

p_value = pf(alpha,df_ssf,df_sse,lower.tail = F); p_value;



