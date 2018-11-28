asax<-function(x,cat,gam=0.0000000000001){
  x<-x[!is.na(x)]

  #initialization of the breakpoints
  bb<-discretize(x,method = "frequency",breaks=cat,onlycuts = TRUE)
  # bb<-c(min(x),bb,max(x))
  
  bbno<-bb
  # D<-Inf
  f<-1
  it<-0
  r<-rep(0,cat)
  SSE<-rep(0,cat)
  while (f==1)
  {
    it<-it+1
    for(i in 1:(cat))#per each interval
    {
      
      if(i==cat){t<-x[x>=bb[i]&x<=bb[i+1]]}
      else{t<-x[x>=bb[i]&x<bb[i+1]]}
      #centre of mass (r) calculation
      r[i]<-sum(t)/length(t)
      SSE[i]<-sum((t-r[i])^2)
    }
    
    #new breakpoints computation
    for(i in 2:(cat)){bb[i]<-(r[(i-1)]+r[i])/2}
    
    #SSE calculation
    D1<-sum(SSE)
    if (it==1){D<-D1}else
    {if (((D-D1)/D)<gam){f<-0}else{D<-D1}}
    
  }
  
  return(list(bb,it,bbno))
}
