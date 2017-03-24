true.discoveries <- function(df, T, mixed = TRUE){
  if (mixed) o <- lapply(df, function(x) order(x$PValue.Mixed))
  else o <- lapply(df, function(x) order(x$PValue))
  x <- NULL
  for (i in 1:length(T)) {
    x[[i]] <- cumsum(T[[i]][o[[i]]])
  }
  NTD <- Reduce("+", x)/length(x) # Number of true discoveries are averaged over 100 simulations.
  TDR <- NTD/nDEsets
  return(list(NTD = NTD, TDR = TDR))
}
