# Textbook: 統計學方法與應用4ed,v2

# Table 15.4 P.245
sumsq = function(n) return(sum((n-mean(n))^2));

name = c("大通","大德","大信","大道","大方","大立","大興","大展");
ad = c(300,400,500,500,800,1000,1000,1300);
income = c(9500,10300,11000,12000,12400,13400,14500,15300);

ad_income = data.frame(name=name,ad=ad,income=income,stringsAsFactors=F);

plot(ad_income$ad,ad_income$income);


x = ad_income$ad;
y = ad_income$income;
n = length(x);

x.mean = mean(x); x.mean;
y.mean = mean(y); y.mean;

beta = round(sum((x-x.mean)*(y-y.mean))/sumsq(x),2); beta;
alpha = y.mean-beta*x.mean; alpha;

Y = function(x) alpha+beta*x;
yi = Y(x);

e = y-yi; e;
beta_var = var(e)/sumsq(x); beta_var; sqrt(beta_var);
alpha_var = (sum(x^2)*var(e)) / (n*sumsq(x)); alpha_var; sqrt(alpha_var);

# Table 15.5 P.250
s_var = sum(e^2)/(n-2); s_var; 
s_sd = sqrt(s_var); s_sd;

h = 1/n+(x-x.mean)^2/sumsq(x); h;
s_e = s_sd*sqrt(1-h); s_e;
std_e = e/s_e; std_e;

# Table 15.9 P.261
sst = sumsq(y); sst;
sse = sum((y-yi)^2); sse;
ssr = sst - sse; ssr;

R2 = ssr/sst; R2;

df_r = 1;
df_e = 6;
msr = ssr/df_r; msr;
mse = sse/df_e; mse;

f_v = msr/mse; f_v;

# Example 15.4 P.263

# test beta
s_beta_var = (s_var/sumsq(x)); s_beta_var;
s_beta_sd = sqrt(s_beta_var); s_beta_sd;
t = (beta-0)/s_beta_sd; t;

t_ee = s_beta_sd*qt(0.05,n-2,lower.tail=F); t_ee;
beta_confidence_interval = beta + c(-t_ee,t_ee); beta_confidence_interval;

# Example 15.5 P.269
x0 = 1400; x0;
y0 = alpha+x0*beta; y0;
s2_y0 = s_var*((1/n)+((x0-x.mean)^2/sumsq(x))); s2_y0;
s_y0 = sqrt(s2_y0); s_y0;
t_ee = qt(0.025,6,lower.tail=F)*s_y0; t_ee;
y0_confidence_interval = y0+c(-t_ee,t_ee); y0_confidence_interval;

# Example 15.6 P270
e0 = 16000;
x0 = 1500; x0;
y0 = alpha + x0*beta; y0;
s2_y0 = s_var*((1/n)+((x0-x.mean)^2/sumsq(x))); s2_y0;
s_y0 = sqrt(s2_y0); s_y0;
t = (y0 - e0)/s_y0; t;

# Example 15.7 P272
x0 = 1400; x0;
y0 = alpha + beta*x0; y0;
s2_e0 = s_var*(1+1/n+(x0-x.mean)^2/sumsq(x)); s2_e0;
s_e0 = sqrt(s2_e0); s_e0;
t_ee = qt(0.025,6,lower.tail=F)* s_e0; t_ee;
y0_confidence_interval = y0+c(-t_ee,t_ee); y0_confidence_interval;

# Example 15.9 P.283
r_xy = cov(x,y)/(sd(x)*sd(y)); r_xy;

# Example 15.10 P.285
t = r_xy/sqrt((1-r_xy^2)/(n-2)); t;

# test alpha
s_alpha_var = (s_var*sqrt(sum(x^2)/(n*sumsq(x)))); s_alpha_var;
s_alpha_sd = sqrt(s_alpha_var); s_alpha_sd;
t = (alpha-0)/s_alpha_sd; t;

t_ee = s_alpha_sd*qt(0.05,n-2,lower.tail=F); t_ee;
alpha_confidence_interval = alpha + c(-t_ee,t_ee); alpha_confidence_interval;


# Draw residual plots
default_par = par(no.readonly=T);
par(mfrow=c(3,1));
plot(x,e,col="red");
abline(0,0);

plot(yi,scale(e),col="red");
abline(0,0);
par(default_par);