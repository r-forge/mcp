adaptive.bh=function(sorted,q,m){
  #Stage I- estimating m0
  stage.one=bh(sorted,q,adjust=TRUE,m=m)
  r=sum(stage.one$Pvals[['rejected']])
  #Anything to reject?                                          
  if( r==0 ) return(stage.one)  
  #Stage II- estimate m0
  else m0=bh.m0.estimate(sorted=sorted,m=m)  

  output=bh(sorted=sorted,q=q,m0=m0,m=m)  
  output$Pvals[['adjusted.pvals']]=bh.adjust(sorted,m=m,m0=m0)
  return(output)  

}# Closing adaptive.bh

