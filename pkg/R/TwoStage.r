two.stage=function(sorted,q,m){
  #Stage I- estimating m0
  q1=q/(1+q)
  stage.one=bh(sorted,q1,adjust=TRUE,m=m)
  r=sum(stage.one$Pvals[['rejected']])
  if (r==0) {
    stage.one$Pvals[['adjusted.pvals']]=1
    return(stage.one)  
  }
   else if (r==m) {
    stage.one$Pvals[['adjusted.pvals']]=stage.one$Pvals[['adjusted.pvals']][1]
    return(stage.one)  
  }
    
  #Stage II- updating q using m0
  else {
    m0=m-r
    output=bh(sorted=sorted,q=q1,m0=m0,m=m)#oracle(sorted,q1,m0)
    output$Pvals[['adjusted.pvals']]=two.stage.adjust(sorted,q)
    return(output)
  }
}

