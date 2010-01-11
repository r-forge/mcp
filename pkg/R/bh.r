bh=function(sorted,q,m,adjust=FALSE,m0=m,pi0,constant=1){
  if(missing(m0) & !missing(pi0)) m0=pi0*m
  else{
  
  criticals=(1:m)*q/(m0*constant)
  cutoff=reject(sorted,criticals)
  rejected= sorted<=cutoff$cutoff
  
  adjusted=rep(NA,m)
  
  if(adjust) adjusted=bh.adjust(sorted,m=m,m0=m0,constant=constant)

  multiple.pvals=data.frame(original.pvals=sorted,criticals=criticals,rejected=rejected,adjusted.pvals=adjusted)
  output=list(Cutoff=cutoff,Pvals=multiple.pvals)
  return(output)
  }# Closing else caluse
  
}

