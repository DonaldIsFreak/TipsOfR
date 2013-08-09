#學習使用simulated Annealing Optimization演算法
# 
#各節點至各節點距離
DisMatrix =matrix(c(0,19,92,29,49,78,6,19,0,21,85,45,16,26,92,21,0,24,26,87,47,29,85,
               24,0,76,17,8,49,45,26,76,0,90,27,78,16,87,17,90,0,55,6,26,47,8,27,
               55,0),nrow=7,ncol=7);

#狀態函數
State = function(x){
  len = length(x);
  result = 0;
  for (i in 1:len){
    j = i+1;
    if (j>len) j=1;
    result= result+DisMatrix[x[i],x[j]];
  }
  return(result);
};

#波茲曼函數
PR = function(delta,T) min(1,exp(1)**(delta/T));

#降溫α值
alpha = 0.95;

#執行回合數
Index = 0;
MaxIndex = 100000;

X = c(1:7);
XBest = X;
StateBest = State(X);

#溫度
TC = 1000;

while (Index < MaxIndex){
  #沒有使用鄰域搜尋法，而採用亂數抓取。
  XTemp = sample(X);
  State_XTemp = State(XTemp);
  State_X = State(X);
  #如果新的值比原來還低，就assign成Best
  if (State_XTemp<=State_X){
    XBest = XTemp;
    StateBest = State_XTemp;
  }else{
    #雖然，新的值很大，若超過随機機率，則可以跳出
    if (PR(-(State_X-State_XTemp),TC)>=runif(1)){
      XBest = XTemp;
      StateBest = State_XTemp;
    }
  }
  Index = Index+1;
  TC = TC*alpha;
}

XBest;
StateBest;