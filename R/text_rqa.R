#' Generate a sequence of numeric identifiers
#' 
#' @param rsrc location of file or resource, or string literal
#' @param typ specify whether 'file', 'url', or 'string'
#' @param removeStopwords omit closed-class words - 'stopwords'
#' @return a vector of numeric identifiers of lexical items

#' @examples
#'
#' txt = 'here is a raw raw raw string, literally'
#' res = text_rqa(txt,typ='string')
#' plot_rp(res$RP)

.packageName <- 'crqa_nlp'

text_rqa = function(rsrc,typ='file',removeStopwords=F,embed=1,tw=1,limit=-1,shuffle=F) {
  ts = get_text_series(rsrc,typ=typ,removeStopwords=removeStopwords)
  if (limit>-1 & length(ts)>limit) {
    ts = ts[1:limit]
  }
  if (shuffle==T) {
    ts = sample(ts,length(ts))
  }
  rqa_res = crqa(ts,ts,delay=1,embed=embed,radius=.001,rescale=F,normalize=F,mindiagline=2,minvertline=2,tw=tw)
  return(rqa_res)
}
