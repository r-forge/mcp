plot.fdr=function(fdr){
  data=fdr$Pvals
  ord=order(data$original.pvals)
  m=length(ord)
  data=data[ord,]
  
  devAskNewPage(ask = TRUE)
  
  plot(data$original.pvals,cex=50/m,col=as.numeric(data$rejected)+1,xlab="Order of P-values",ylab="",lty=2,main='Sorted pvals')
  lines(data$criticals,col='red')
  abline(h=fdr$Cutoff)
                      
  ylabel=paste(fdr$method,'adjusted p-values')
  plot(data$adjusted.pvals~data$original.pvals,cex=50/m,main='Adjusted p-vals',col=as.numeric(data$rejected)+1,ylim=c(0,1),xlab='Original P-values',ylab=ylabel)
  abline(h=fdr$q)
  axis(side=2,at=fdr$q,labels='q',las=2)
    
  devAskNewPage(ask = FALSE)
}

#plot.fdr(bh.1)
