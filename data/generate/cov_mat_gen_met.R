
generate_lamb_q <- function(
  n_matrices,
  dim,
  lambda_dist = "exp",
  ...
) {
  psd_list <- vector("list", n_matrices)

  for (i in seq_len(n_matrices)) {

    rfun <- get(paste0("r", lambda_dist), mode = "function")
    lambda <- rfun(dim, ...)
    lambda <- sort(lambda, decreasing = TRUE)
    Lambda <- diag(lambda)

    Z <- matrix(rnorm(dim * dim), dim, dim)
    qrZ <- qr(Z)
    Q <- qr.Q(qrZ)
    R <- qr.R(qrZ)


    D <- diag(sign(diag(R)))
    Q <- Q %*% D

    psd_list[[i]] <- Q %*% Lambda %*% t(Q)
  }

  return(psd_list)
}
