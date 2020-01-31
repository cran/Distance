# from mrds
covn <- function (lvec, groups1, groups2, type){
  # covn - computes covariance of encounter rate of clusters and individuals as
  #        called from dht.  modeled after varn.
  L <- sum(lvec)
  n1 <- sum(groups1)
  er1 <- n1/L
  n2 <- sum(groups2)
  er2 <- n2/L

  k <- length(lvec)
  if(type=="R3"){
    varer <- sum(lvec * (groups1/lvec - er1)*(groups2/lvec-er2))/(L*(k-1))
  }else{
    varer <- k*sum(lvec^2 * (groups1/lvec - er1)*(groups2/lvec-er2))/(L^2*(k-1))
  }

  return(varer)
}
