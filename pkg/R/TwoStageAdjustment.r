#Solving for q given r in the New Two Stage procedure (shrinking by q/(1+q))
solve.q=function(sorted,m,j,r=0){
  a=sorted*(m-r)/(1:m)
  stopifnot(a>0 , j>=1 , j<=m , r>=0 , r<=m)
  adjusted=ifelse(a>0.5, 1 , a/(1-a) )
  
  temp.min=adjusted[m]
  for(i in m:j){
    if(adjusted[i]<=temp.min) temp.min=adjusted[i]
    else adjusted[i]=temp.min
    }
  return(adjusted)
}# Close solve.q function

####################################3
two.stage.adjust=function(sorted,r=0,patience=4){
  m=length(sorted)
  adjusted=rep(0,m)  
   
  # Adjusting sorted p-values
  adjusted.q=solve.q(sorted=sorted,m=m,j=1,r=0)
  checking=adjusted.q

  #Has the procedure rejected everything at the first stage?
  if(sum(bh(sorted,adjusted.q[1]/(1+adjusted.q[1]),m=m)$Pvals[['rejected']])==m){
    adjusted.q=rep(adjusted.q[1],m)
    return(adjusted.q)
  }
                                              
  else{
    for (j in 1:m) { 
      delta.r=1;delta.q=1
      new.q=adjusted.q[j]
      r.new=sum(bh(sorted,new.q/(1+new.q),m=m)$Pvals[['rejected']])
      counter=0;max.q=0
      
      while(delta.r>0 & delta.q>0){
        old.q=new.q
        r.old=r.new
        new.q=solve.q(sorted=sorted,m=m,j=j,r=r.old)[j]
        r.new=sum(bh(sorted,new.q/(1+new.q),m=m)$Pvals[['rejected']])
        delta.r=abs(r.new-r.old)  
        delta.q=abs(new.q-old.q)
        #cat('j=',j,'Old r=',r.old,'New r=',r.new,'r change=',delta.r,'new q=',new.q,'old q=',old.q,'q change=',delta.q,'Counter=',counter,'\n',sep='');flush.console()
        counter=counter+1
        if(counter>patience & max.q!=new.q) max.q=max(max.q,new.q)
        else if(counter>patience & max.q==new.q ) break
        } #Close interations inside q[j]
            
      adjusted.q[j]=min(new.q,1)
      adjusted.q[min(j+1,m)]=adjusted.q[j]
      stopifnot(any(adjusted.q[j]<=checking[j]))
      }#Close looping over j.    
    
    temp.min=adjusted.q[m]
    for(i in m:1){
      if(adjusted.q[i]<=temp.min) temp.min=adjusted.q[i]
      else adjusted.q[i]=temp.min
      }
    return(adjusted.q)      
    }#Close 'else' clause
    
}# Close two.stage.adjust



