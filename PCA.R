#Using PCA to practice the dimensionality reduction.
dat = as.matrix(read.table("marks.dat",head=T));

dim(dat);
plot(dat);

covMat = cov(dat);

eig = eigen(covMat);

eigVals = eig$values;
eigVects = eig$vectors;

redEigVects = eigVects[,1];
lowDDataMat = dat %*% redEigVects;
dim(lowDDataMat);

plot(lowDDataMat%*%redEigVects);
