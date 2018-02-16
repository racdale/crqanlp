#' Generate a sequence of numeric identifiers
#' 
#' @param RP recurrence matrix, output from crqa library
#' @param xlab x label on plot's x-axis
#' @param ylab y label on plot's y-axis
#' @param cex size of marker
#' @return generates a plot; no value returned

#' @examples
#'
#' txt = 'here is a raw raw raw string, literally'
#' res = text_rqa(txt,typ='string')
#' plot_rp(res$RP)

.packageName <- 'crqa_nlp'

plot_rp = function(RP,xlab='i',ylab='j',cex=.1) {
  if (!is.matrix(RP)) { RP = as.matrix(RP) }
  ij = which(RP==1,arr.ind=T)
  plot(ij[,1],ij[,2],cex=cex,xlab=xlab,ylab=ylab,pch=16)
}
