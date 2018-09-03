sbdeep=function(data,parts,xiaoz){
  parts<-parts         #分几个箱
  xiaoz<-xiaoz         #极小值
  value<-quantile(data,probs = seq(0,1,1/parts))  #这里以data等比分为4段，步长为1/4
  number<-mapply(function(x){
    for (i in 1:(parts-1))
    {
      if(x>=(value[i]-xiaoz)&x<value[i+1])
      {
        return(i)
      }
    }
    if(x+xiaoz>value[parts])
    {
      return(parts)
    }
    return(-1)
  },data)
  #打标签L1L2L3L4
  return(list(degree=paste("L",number,sep=""),degreevalue=number,value=table(value),number=table(number)))               #将连续变量转化成定序变量，此时为L1,L2,L3,L4...根据parts
}
