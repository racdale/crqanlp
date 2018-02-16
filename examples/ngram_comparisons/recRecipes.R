library(tm)
library(crqa)
library(entropy)
setwd('~/Dropbox/new.projects/recurrence-recipes-2/')
source('functions.R')
text1 = c(rep("a",10),rep("b",10),rep("c",10),rep("d",10),rep("e",10))
uniqChars = unique(text1)

nyt1 = readChar('nyt1.txt',file.info('nyt1.txt')$size) # let's first load in the raw data
nyt2 = readChar('nyt2.txt',file.info('nyt2.txt')$size) # let's first load in the raw data
nyt3 = readChar('nyt3.txt',file.info('nyt3.txt')$size) # let's first load in the raw data
nyt = paste(nyt1,nyt2,nyt3)
# split up the text by space and get all unique words
cleanTxt = PlainTextDocument(removeWords(cleanText(cleanSpecialChars(nyt)),stopwords(kind="en")))$content
charCode = unique(unlist(strsplit(cleanTxt,'')))
seq = assignCharCodes(text1,charCode)
myRqa = crqa(seq,seq,delay=1,embed=3,radius=.001,rescale=F,normalize=F,mindiagline=2,minvertline=2,tw=1)
plotRP(myRqa$RP,xlab='word',ylab='word',cex=.05)

mergembed = function(tbl) {
  return(unlist(apply(tbl,1,function(x) {
    return(paste(x,collapse="-"))
  })))
}

# number of unique ngrams NOT CONTAINED by some longer ngram
# this returns a distribution from 2 to length(seq)-1
ngrammers = function(seq) {
  distrib = c()
  ngramList = list()
  for (j in (length(seq)-1):2) {
    ngrams = mergembed(embed(seq,j)[,j:1])
    ngramTable = table(ngrams)
    embeddedUncorrected = ngramTable^2-ngramTable
    correction = unlist(lapply(names(ngramTable),function(ngram) {
      #ixes = grep(x,names(ngramList))
      ixes = gregexpr(paste("(?=",gsub('-','\\\\-',ngram),")",sep=''),names(ngramList),perl=T)
      vals = 0
      if (length(ixes)>0) {
        for (i in 1:length(ixes)) {
          totPat = sum(ixes[[i]]>0)
          if (totPat>0) {
            vals = vals + totPat * unlist(ngramList)[i]
          }
        }
      }
      return(vals)
    }))
    embedded = embeddedUncorrected - correction 
    distrib = c(distrib,sum(embedded[embedded>0]))
    ngramList = c(ngramList,embedded[embedded>0])    
  }
  distrib[distrib<0]=0
  return(distrib)
}

simpleEnt = function(distrib) {
  distrib = distrib / sum(distrib)
  distrib = distrib[distrib!=0]
  return(-sum(log(distrib)*distrib))
}

computeTrend = function(seq,skip) {
  diag_rec = drpdfromts(seq,seq,datatype='categorical',ws=(length(seq)))$profile # get the full profile
  diag_rec = rev(diag_rec[skip:floor(length(diag_rec)/2)]) # use floor; exclude line of identity; reverse this for the entropy summation; skip = avoid skip-1# corner spots for inclusion
  # see: http://www.recurrence-plot.tk/rqa.php
  numerator = sum((1:length(diag_rec)-length(diag_rec)/2)*(100*diag_rec-mean(100*diag_rec))) 
  trend = numerator / sum((1:length(diag_rec)-length(diag_rec)/2)^2)
  return(trend)
}

lapply(list.files('crqa_nlp/R'),function(x) { source(paste('crqa_nlp/R/',x,sep='')) })
nlp_dat = c()
rqa_dat = c()
for (i in 1:10) {
  print(i)
  seq = get_text_series(paste(sample(text1,100,replace=T),collapse=' '),typ='string')
  ngramDist = ngrammers(seq)
  
  term_freq_sq = sum(table(seq)^2)-sum(table(seq)) ## RR proportional to sum(f^2-f)
  ngram_variability = simpleEnt(ngramDist) ## ENT equal to Shannon entropy of n-gram distribution under square assumption
  proportion_on_repeat_ngrams = sum(((length(seq)-1):2)*ngramDist)/term_freq_sq[length(term_freq_sq)] ## proportional to DET
  mean_ngram_length = sum((((length(seq)-1):2)*ngramDist)/sum(ngramDist))
  longest_repeated_subsequence = length(seq)-which(ngramDist>0)[1]
  nlp_dat = rbind(nlp_dat,data.frame(term_freq_sq,ngram_variability,proportion_on_repeat_ngrams,mean_ngram_length,longest_repeated_subsequence))
  trend = computeTrend(seq,11)
  # note that these require this SQUARED assumption over the scores; may be interesting to consider this as a kind of squared divergence from unity (single)
  
  myRqa = crqa(seq,seq,delay=1,embed=1,radius=.001,rescale=F,normalize=F,mindiagline=2,minvertline=2,tw=1)
  rqa_dat = rbind(rqa_dat,data.frame(rr=myRqa$RR,ent=myRqa$ENT,det=myRqa$DET,meanl=myRqa$L,maxl=myRqa$maxL,trend=trend))
  
}

cor.test(rqa_dat$maxl,nlp_dat$longest_repeated_subsequence)








