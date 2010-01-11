summary.fdr=function(fdr)  {
  r=sum(fdr$Pvals[['rejected']])
  m=length(fdr$Pvals[['rejected']])
  cat('There were',r,'rejections out of',m,'hypothsis tested which are',r/m, 'percent \n')
  cat('Using method',fdr$method,'at FDR level of',fdr$q,'\n')
  
}

