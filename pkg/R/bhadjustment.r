bh.adjust=function(sorted,m,m0,constant=1){
  adjusted=rep(NA,m)
  temp.min=sorted[m]
  min.ind=rep(0,m)    
  for (i in m:1) {
      temp= min(m0*sorted[i]*constant / i,1)
      if  ( temp <= temp.min  ) {
      temp.min = temp
      min.ind[i]=1
      }
     adjusted[i]=temp.min
     }
  return(adjusted)
}
    

