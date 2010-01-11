fdr=function(x,q,method='BH',pi0,lambda=0.5, m0){
  #Input checking
  stopifnot( is.numeric(x) , is.numeric(q) , any(x<1) , any(x>0) , q<1 , q>0 )
  m=length(x)
  if(!missing(m0)) stopifnot( is.numeric(m0) , m0<m , m0>=0 )
  else if(!missing(m0) & !missing(pi0)) stop("Only m0 or pi0 can be specified.")
  else if (method!='Oracle' && !missing(m0) ) stop("m0 can only be specified for the Oracle method.")
  else if(missing(m0) & !missing(pi0)) m0=pi0*m
                                                                                                    
  ranks=rank(x)
  sorted=sort(x)
         
  # Selecting the propoer procedure
  output=switch(method,
    'BH'=bh(sorted=sorted,q=q,adjust=TRUE,m=m),
    'General Dependency'=bh(sorted=sorted,q=q,m=m,constant=log(m),adjust=TRUE), 
    'Oracle'=bh(sorted=sorted,q=q,m=m,m0=m0,adjust=TRUE),
    'BH Adaptive'=adaptive.bh(sorted=sorted,q=q,m=m),
    'ST Adaptive'=adaptive.st(sorted=sorted,q=q,m=m,lambda),
    'Two Stage'=two.stage(sorted=sorted,q=q,m=m),
    'Multiple Step Down'=multiple.down(sorted=sorted,q=q,m=m),    
    'Multiple Step Up'=stop("Not implemented yet.") ,#look in file "multiple up (unused).r",    
    'Smoother'=stop("Not implemented yet.") ,   
    'Bootstrap'=stop("Not implemented yet."),
    'Median ST Adaptive'=stop("Not implemented yet.")
  )                
  
  #Arranging output
  output$Pvals=output$Pvals[ranks,]
  output=list(method=method,q=q,Cutoff=output[['Cutoff']],Pvals=output[['Pvals']])
  class(output)=c(class(output),'fdr')
  return(output)
}

