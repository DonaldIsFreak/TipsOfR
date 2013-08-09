# Textbook: 統計學方法與應用4ed,v2
# Table 16.1 P.313
n = 5;
k = 2;

x = c(6,9,12,16,18);
z = c(7,5,14,8,6);
y = c(38,40,53,50,55);

sumsq = function(d) sum((d-mean(d))^2);
cov_ab = function(a,b) sum((a-mean(a))*(b-mean(b)));
all_data = data.frame(x=x,y=y,z=z);

# ggplot(all_data,aes(x=x))+
#   geom_point(aes(y=y),color='red')+
#   geom_density(aes(y))+
#   geom_point(aes(y=z),color='blue');

# Formula 16.13~16.5 P.317
beta = (sumsq(z)*cov_ab(x,y)-cov_ab(x,z)*cov_ab(z,y))/(sumsq(x)*sumsq(z)-(cov_ab(x,z))^2);
beta = round(beta,2); beta;
gamma = (sumsq(x)*cov_ab(z,y)-cov_ab(z,x)*cov_ab(x,y))/(sumsq(x)*sumsq(z)-(cov_ab(x,z))^2); 
gamma =round(gamma,2); gamma;
alpha = mean(y)-beta*mean(x)-gamma*mean(z);
alpha = round(alpha,2); alpha;

yi = alpha+beta*x+gamma*z; yi;
e = y-yi; e;

# Formula 16.16~16.8 P.319
beta_var= (sumsq(z)*var(e))/(sumsq(x)*sumsq(z)-(cov_ab(x,z))^2);
beta_sd = sqrt(beta_var); beta_sd;
gamma_var = (sumsq(x)*var(e))/(sumsq(x)*sumsq(z)-(cov_ab(x,z))^2);
gamma_sd = sqrt(gamma_var); gamma_sd;
alpha_var = ((1/n)+((mean(x)^2*sumsq(z)+mean(z)^2*sumsq(x)-2*mean(x)*mean(y)*cov_ab(x,z))/(sumsq(x)*sumsq(z)-(cov_ab(x,z))^2)))*var(e);
alpha_sd = sqrt(alpha_var); alpha_sd;

# Table 16.7 P.324
sst = sum((y-mean(y))^2); sst;
ssr = sum((yi-mean(y))^2); ssr;
sse = sum((y-yi)^2); sse;

R2 = round(ssr/sst,2); R2;

adjust_R2 = 1-((sum(e^2)/(n-3))/(sumsq(y)/(n-1))); adjust_R2;

msr = round(ssr/k,2); msr;
mse = round(sse/(n-k-1),2); mse;

f_value = round(msr/mse,2); f_value;

# Table 16.8 P.326
beta_x = round(cov(x,y)/var(x),2); beta_x;
alpha_x = round(mean(y) - beta_x*mean(x)); alpha_x;

yi_x = alpha_x + beta_x*x;
sst_x = sum((y-mean(y))^2); sst_x;
ssr_x = sum((yi_x-mean(y))^2); ssr_x;
sse_x = sum((yi_x-y)^2); sse_x;

df_ssr_x = 1;
df_sse_x = n-2;
msr_x = round(ssr_x/df_ssr_x,2); msr_x;
mse_x = round(sse_x/df_sse_x,2); mse_x;

f_value_x = msr_x/mse_x; f_value_x;

f_value_particle = ((ssr-ssr_x)/1)/(sse/(5-2-1)); f_value_particle;
