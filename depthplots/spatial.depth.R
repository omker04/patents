spatial.depth=function(data,x){
  if(ncol(as.matrix(x))==1)
    x<-t(x)
  if(ncol(data)!=ncol(as.matrix(x))){
    sd<-"Dimensions do not match"
  }else{
    spd<-function(data,x)
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
      sd=1-sqrt(sum(v^2))
    }
    sd<-apply(x,1,function(y){spd(data,y)})
  }
  sd
}