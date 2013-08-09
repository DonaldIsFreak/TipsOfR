#Exercise 4.19

nums = c(141.2,259.9,263.3,206,113.6,451,278.7,168.7,73.1,270,606.5,582.7,346.6,1030,362.4,236.3,431,286.2,587.3,240.8,513.5,114.5,407.5);

boxplot(nums,horizontal=TRUE,col="blue");

#Check outliters whether in the sequence.
nums.Q1 = as.integer(quantile(nums,0.25));

nums.Q3 = as.integer(quantile(nums,0.75));

nums.IQR =  as.integer(nums.Q3 - nums.Q1) ; nums.IQR;

nums[nums<(nums.Q1-1.5*nums.IQR) | nums>nums.Q3+1.5*nums.IQR]