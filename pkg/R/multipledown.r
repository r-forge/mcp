multiple.down=function(sorted,q,m){
  if(q>0.5) warning('FDR is not controlled when q>0.5')
   
  criticals=sapply(1:m,function(i) q*i/(m-i*(1-q)+1))
  indicators= sorted<criticals # Marking p-values below the critical values
  
  if(!indicators[1]) cutoff=list(cutoff=0,cut.index=0)
  else if(all(indicators)) cutoff=list(cutoff=sorted[m],cut.index=m)
  else{ 
    cut.index=min((1:m)[!indicators])-1
    cutoff=list(cutoff=sorted[cut.index],cut.index=cut.index) 
    }
           
  rejected= sorted<=cutoff$cutoff
  
  #adjusted=NA
  adjusted=multiple.down.adjust(sorted,m)  

  multiple.pvals=data.frame(original.pvals=sorted,criticals=NA,rejected=rejected,adjusted.pvals=adjusted)
  output=list(Cutoff=cutoff,Pvals=multiple.pvals)
  
  return(output)

}

#test=runif(100)^1.4;s.test=sort(test)
#multiple.1=multiple.down(s.test,0.49,length(s.test));multiple.1
