#Exercise 4.1
#Q1
nums = c(9,8,8,7,6,5,5,5,4,3);

nums.range = max(nums)-min(nums); nums.range;

nums.mean = mean(nums); nums.mean;

nums.median = median(nums); nums.median;

nums.mode = table(nums)[table(nums)==max(table(nums))]; nums.mode;

#Q2
nums = c(50,8,8,7,6,5,5,5,4,3);

nums.mean = mean(nums); nums.mean;

nums.median = median(nums); nums.median;

nums.mode = table(nums)[max(table(nums))]; nums.mode;
