# Practice to z-estimate. Try to find out how many number of values not on confidence interval 
# when the confidence level in 90%,95%,99%.
#

check_sample<-function(sample_mean){
percent90=sample_mean[sample_mean-0.281>3.5|sample_mean+0.281<3.5];
percent95=sample_mean[sample_mean-0.335>3.5|sample_mean+0.335<3.5];
percent99=sample_mean[sample_mean-0.440>3.5|sample_mean+0.440<3.5];
return(list("90"=percent90,"95"=percent95,"99"=percent99));
}

# Simulation to throw a thousand times of dice as the population of dice.
population=round(runif(1000,1,6));

# Random to catch sample size of 100 as 40 samples.
sample.mean=sapply(1:40,function(a)mean(sample(x,100)));

# Determine the sample mean is between in lower and upper confidence limit.
notOnInterval=check_sample(sample.mean);

# Display values are not on the confidence interval.
notOnInterval;

# Display number of the values.
sapply(notOnInterval,length);