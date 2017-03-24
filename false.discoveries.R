false.discoveries <- function(df, T, mixed = TRUE){
  if (mixed) o <- lapply(df, function(x) order(x$PValue.Mixed))
  else o <- lapply(df, function(x) order(x$PValue))
  x <- NULL
  for (i in 1:length(T)) {
    x[[i]] <- cumsum(1 - T[[i]][o[[i]]])
  }
  NFD <- Reduce("+", x)/length(x) # Number of false discoveries are averaged over 100 simulations.
  FDR <- NFD/nDEsets
  return(list(NFD = NFD, FDR = FDR))
}
