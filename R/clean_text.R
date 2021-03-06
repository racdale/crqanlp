#' Clean raw text 
#' 
#' @param rawText a single string literal
#' @param removeStopwords omit closed-class words - 'stopwords'
#' @return a new string literal, with sentence marker, no punctation, etc.

.packageName <- 'crqa_nlp'

clean_text = function(rawText,removeStopwords=F) {
  rawText = Corpus(VectorSource(rawText))    
  # eliminate extra whitespace; requires tm
  rawText = tm_map(rawText, tolower)
  # eliminate punctuation
  sentencebreak = function(x) { return(gsub("\\.",' .',x)) }
  removepunct = function(x) { return(gsub("[[:punct:]]","",x)) }
  rawText = tm_map(rawText, sentencebreak) 
  rawText = tm_map(rawText, removepunct)
  if (removeStopwords) {
    rawText = tm_map(rawText, removeWords, stopwords('english'))
  }
  rawText = tm_map(rawText, stripWhitespace)
  return(rawText[[1]])
}
