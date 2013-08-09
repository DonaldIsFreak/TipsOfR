#Exercise 4.10
#Q1
nums = c(41.0,40.9,39.8,42.6,41.6,42.5,42.4,40.8,42.5,42.8,39.7,41.8,42.2,42.5,42.6,40.0,41.0,42.4,42.7,43.6);

nums.n = length(nums);

nums.mean = mean(nums); nums.mean;

nums.mode = table(nums)[table(nums)==max(table(nums))]; nums.mode;

nums.median = median(nums); nums.median;

#Q2
nums.P1 = quantile(nums,0.1); nums.P1;

nums.P9 = quantile(nums,0.9); nums.P9;

#Q3
nums.range = max(nums)-min(nums); nums.range;

nums.mad = sum(abs(nums-nums.mean))/nums.n; nums.mad;

nums.iqr = quantile(nums,0.75)-quantile(nums,0.25); nums.iqr;

#Q4
nums.var = var(nums); nums.var;

nums.sd = sd(nums);

nums.cv = nums.sd/nums.mean; nums.cv;
