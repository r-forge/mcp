adaptive.st=function(sorted,q,m,lambda){
  #Stage I- estimating m0
  r=sum(sorted<=lambda)
  m0=(m-r+1)/(1-lambda)

  #Stage II- calling bh with estimated m0
  output=bh(sorted=sorted,q=q,m0=m0,m=m)  
  output$Pvals[['adjusted.pvals']]=bh.adjust(sorted,m=m,m0=m0)
  output$Pvals[['rejected']]=output$Pvals[['rejected']]*(output$Pvals[['original.pvals']]<=lambda)
  return(output)  
}


