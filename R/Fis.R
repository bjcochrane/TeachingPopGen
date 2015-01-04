Fis <-function(genos){
  p <-genos[1]+.5*genos[2]
  Hexp <-2*p*(1-p)
  f <- (Hexp-genos[2])/Hexp
  f
}