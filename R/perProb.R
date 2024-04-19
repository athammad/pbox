


perProb<-function(bpx,vecQuery){
  perCop<-copy(bpx)
  perCop@copula@paramMargins<-perturbate_params(bpx@copula@paramMargins)
  pMvdc(vecQuery,perCop@copula)
}
