#' Generate a sequence of numeric identifiers
#' 
#' @param rsrc location of file or resource, or string literal
#' @param typ specify whether 'file', 'url', or 'string'
#' @param removeStopwords omit closed-class words - 'stopwords'
#' @return a vector of numeric identifiers of lexical items

#' @examples
#'
#' txt = 'here is a raw raw raw string, literally'
#' ts = get_text_series(txt,typ='string')
#' # called by text_rqa
#' res = text_rqa(txt,typ='string')
#' plot_rp(res$RP)

.packageName <- 'crqa_nlp'

get_text_series = function(rsrc,typ='file',removeStopwords=F) {
  if (typ=='file') { # it's a file name
    rawText = readChar(rsrc,file.info(rsrc)$size)
  } else if (typ=='url') { # it's a url, get the file
    rawText = getURL(rsrc, ssl.verifypeer = F)
  } else if (typ=='string' | typ=='raw_chars') { # use literal string
    rawText = rsrc
  } else if (typ=='tibble') { # is this a tidytext tibble
    rawText = paste(rsrc$word,collapse=' ')
  }
  if (typ!='raw_chars') {
    rawText = clean_text(rawText,removeStopwords)$content # why does this, when run first, produce time zone warning?
    words = unlist(strsplit(rawText, ' ')) # get list of words
  } else {
    words = unlist(strsplit(rawText, ''))
  }
  ts = assign_codes(words,unique(words)) # assign the numeric codes 
  return(ts)
}
