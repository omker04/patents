spatial.rank=function(data,x)
{
  v=array(0,ncol(data))
  for(j in 1:ncol(data))
  {
    for(i in 1:nrow(data))
    {
      if(sqrt(sum((x-data[i,])^2))!=0)
        v[j]=v[j]+((x[j]-data[i,j])/sqrt(sum((x-data[i,])^2)))
    }
    v[j]=v[j]/nrow(data)
  }
  v
}



