dd.plot<-function(data1,data2=rmvnorm(nrow(data1),array(0,ncol(data1)),diag(1,ncol(data1),ncol(data1))),
                  main="Normal DD-plot",xlab="Theoretical Depths",ylab="Sample Depths"){
  spatial.depth=function(data,x){
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
    sd
  }
  something1<-c(spatial.depth(data1,data1),spatial.depth(data1,data2))
  something2<-c(spatial.depth(data2,data1),spatial.depth(data2,data2))
  x<-cbind(something1,something2)
  matplot(x[,1],x[,2],main=main,xlab=xlab,ylab=ylab)
}
