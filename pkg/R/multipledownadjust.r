multiple.down.adjust=function(sorted,m){
  adjusted=rep(NA,m)
  temp.max=sorted[1]
  max.ind=rep(0,m)

  for (i in 1:m) {
      temp= min(sorted[i]*(m+1-i)/(i*(1-sorted[i])),1)
      if  ( temp >= temp.max  ) {
      temp.max = temp
      max.ind[i]=1
      }
     adjusted[i]=temp.max
     }

  return(adjusted)
}


