#' Assign a code with an existing list of words (indices used)
#' 
#' @param words vector of strings (tokens)
#' @param wordCode vector of strings (types)
#' @return a vector of numeric identifiers of lexical items

.packageName <- 'crqa_nlp'

assign_codes = function(words,wordCode) {
  series = c()
  for (word in words) {
    series = c(series, which(word==wordCode))
  }
  return(series)
}
