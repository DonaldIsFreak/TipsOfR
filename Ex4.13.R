#Exercise 4.13

nums = c(540,680,503,558,490,609,379,601,559,495,562,580,510,623,477,574,588,497,527,570,495,590,602,541);

boxplot(nums,horizontal=TRUE,col="blue");

nums.summary = summary(nums); nums.summary;

#Check outliters whether in the sequence.
nums.Q1 = as.integer(quantile(nums,0.25));

nums.Q3 = as.integer(quantile(nums,0.75));

nums.IQR =  as.integer(nums.Q3 - nums.Q1) ; nums.IQR;

nums[nums<(nums.Q1-1.5*nums.IQR) | nums>nums.Q3+1.5*nums.IQR]