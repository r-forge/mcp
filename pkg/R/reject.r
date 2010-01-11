reject=function(sorted,criticals){
  m=length(sorted)
  stopifnot( length(criticals) == m )
  indicators= sorted<criticals # Marking p-values below the critical values
  
  if(!any(indicators)) return(list(cutoff=0,cut.index=0))

  cut.index=max((1:m)[indicators])

  cutoff=sorted[cut.index] #The highest rejected p-value
  
  return( list(cutoff=cutoff,cut.index=cut.index) )
}


