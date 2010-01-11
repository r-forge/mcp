st.m0.estimate=function(sorted,m){
  k=2:m
  m0.k=(m+1-k)/(1-sorted[-1])
  diffs=diff(m0.k,lag=1)
  indicators= diffs>0
  optimal.k=min( k[indicators] )
  m0=min(ceiling(m0.k[optimal.k]),m)
  stopifnot(m0 <=m ,m0 > 0)
  return(m0)
}