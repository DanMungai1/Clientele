sample_size <- function(z=1.96, q=0.5,p=0.5, e=0.05, N) {
  up <- (z^2*p*q*N)
  low <- (e^2*(N-1) + z^2*p*q) 
  n <- up/low
  n
}

sample_size(N=659)

