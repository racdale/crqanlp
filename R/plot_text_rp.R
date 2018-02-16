#' Generate a sequence of numeric identifiers
#' 
#' @param RP recurrence matrix, output from crqa library
#' @param rsrc location of file or resource, or string literal
#' @param typ specify whether 'file', 'url', or 'string'
#' @param xlab x label on plot's x-axis
#' @param ylab y label on plot's y-axis
#' @param cex size of marker
#' @param removeStopwords omit closed-class words - 'stopwords'
#' @return generates a plot; no value returned

#' @examples
#'
#' txt = 'here is a raw raw raw string, literally'
#' res = text_rqa(txt,typ='string')
#' plot_rp(res$RP)

.packageName <- 'crqa_nlp'

plot_text_rp = function(RP,rsrc,typ='file',xlab='i',ylab='j',cex=.1,removeStopwords=F) {
  if (!is.matrix(RP)) { RP = as.matrix(RP) }
  ij = which(RP==1,arr.ind=T)
  plot(ij[,1],ij[,2],cex=cex,xlab=xlab,ylab=ylab,pch=16,col='white')

  if (typ=='file') { # it's a file name
    rawText = readChar(rsrc,file.info(path)$size)
  } else if (typ=='url') { # it's a url, get the file
    rawText = getURL(rsrc, ssl.verifypeer = F)
  } else if (typ=='string') { # use literal string
    rawText = rsrc
  }
  rawText = clean_text(rawText,removeStopwords)$content # why does this, when run first, produce time zone warning?
  words = unlist(strsplit(rawText, ' ')) # get list of words
  text(ij[,1],ij[,2],words[ij[,1]],cex=cex)
}
