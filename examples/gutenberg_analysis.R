
setwd("~/Dropbox/new.projects/recurrence-recipes-2/crqanlp/examples/") 

library(parallel) # to use multiple cores
library(gutenbergr) # where the books are hiding
 
analyze_book = function(x) { # analyze one of the book's texts
  require(crqa) # need these for each core
  require(tidytext)
  require(R.utils)
  require(tm)
  require(dplyr)
  require(stringr)
  # crqanlp library is here:
  lapply(list.files('crqa_nlp/R',pattern='R$'),function(x) { source(paste('crqa_nlp/R/',x,sep='')) })   
  work_id = x['gutenberg_id']
  print(work_id)
  tryCatch({
    work_info = gutenberg_works(gutenberg_id==work_id)
    author_id = work_info$gutenberg_author_id 
    author_info = gutenberg_authors[gutenberg_authors$gutenberg_author_id==author_id,]
    if (is.na(author_id)) {
      author_info = author_info[1,]
    }
    subj = paste(x['subject'][[1]],collapse=',')
    
    if (dim(work_info)[1]>0) { # let's keep it viable... sometimes functions are slow -- skips massive books
      tryCatch({
        txt = withTimeout({
          gutenberg_download(gutenberg_id=work_id)
        }, timeout=5)
      }, TimeoutException = function(ex) {
        txt = data.frame()
      })
      
      if (dim(txt)[1]>0) { # same--oddly strip sometimes takes more time
        tryCatch({
          txt = withTimeout({
            gutenberg_strip(txt)
          }, timeout=5)
        }, TimeoutException = function(ex) {
          txt = data.frame()
        })
      }
      
      if (dim(txt)[1]>0) { # only if we got some data back!
        txt = txt %>%
          unnest_tokens(word, text) # let's tidy this up
        lenDL = length(txt$word) # how many words are there?
        if (lenDL>10000) {
          #txt = paste(txt$word[5000:min(10000,lenDL)],collapse=' ') # let's only take 100 words for now (testing)
          txt = paste(txt$word[5000:10000],collapse=' ') # let's only take 100 words for now (testing)
          write.table(txt,file=paste('books/',work_id,'.txt',sep=''))
          res = text_rqa(txt,typ="string") # time to run rqa
          resToSave = data.frame(res[1:9],
                                 as.data.frame(author_info),
                                 as.data.frame(work_info),subj,lenDL=lenDL)
          #allDat = rbind(allDat,resToSave)
          return(resToSave)
          #bookDeets = rbind(bookDeets,data.frame(id=work_id,len=length(txt$word)))
        }
      }
    }
  }, error = function(ex) {
    write(file='error.log',paste(str(ex),'error! gutenberg ID',work_id),append=T)
  })
}

# let's get a list of the viable gutenbergr books; some cause issues with strip, etc.
subs = sort(table(paste(gutenberg_subjects$subject[gutenberg_subjects$subject_type=='lcsh'])),
            decreasing=T)[1:20] # top 20 categories
subs = names(subs)
works = gutenberg_subjects[gutenberg_subjects$subject %in% subs,] # let's get all the books relevant to these 20
works = aggregate(subject~gutenberg_id,data=works,paste) # some books are under several categories; merge

ncores = detectCores() - 1
#clst = makeCluster(ncores)
#clst = makeCluster(2)
t1=proc.time()
#a=parApply(clst,works[1:200,],1,analyze_book)
a=apply(works,1,analyze_book)
print(proc.time()-t1)
results = do.call(rbind,a)
#stopCluster(clst)
save(file='results.Rd',results)

#dat = read.table('data.txt')
#load('results.Rd')
#colnames(dat) = colnames(results)[1:(ncol(results)-1)]
#results = dat
dim(results)
results = results[!is.na(results$DET),] # must have at least 1 diagonal line
dim(results)
topicCodes = matrix(0,nrow=nrow(results),ncol=length(subs))
for (i in 1:nrow(results)) {
  subjs = unlist(strsplit(as.character(results[i,]$subj),','))
  topicCodes[i,which(subs %in% subjs)] = 1
}

# large majority of works have just 1 category from our 20
hist(rowSums(topicCodes))
mean(rowSums(topicCodes)==2)
# let's test on the most frequent category, on the small batch
which.max(colSums(topicCodes))
colSums(topicCodes)
colnames(topicCodes) = subs
summary(lm(RR~topicCodes,data=results))
summary(lm(DET~topicCodes,data=results))
summary(lm(maxL~topicCodes,data=results))
summary(lm(ENTR~topicCodes,data=results))
summary(lm(DET~birthdate,data=results))

categ = 'Poetry'
mean(results$RR[topicCodes[,which(subs==categ)]==1])
mean(results$RR[topicCodes[,which(subs==categ)]==0])

results$rDET = resid(lm(DET~RR,data=results))
pdf(file='gutenberg_max_det.pdf',width=4.5,height=5)
for (s in subs) {
  ix = which(subs == s)
  n = sum(topicCodes[,ix])
  subdat = results[topicCodes[,ix]==1,]
  if (s=='Poetry') {
    subdat = subdat[subdat$maxL<1000,] # ripple rhymes has repeat poem!
  }
  
  mrr = mean(subdat$RR)
  serr = 2.5*sd(subdat$RR)/sqrt(n)
  mdet = mean(subdat$DET)
  sedet = 2.5*sd(subdat$DET)/sqrt(n)
  mmaxl = mean(subdat$maxL)
  semaxl = 2.5*sd(subdat$maxL)/sqrt(n)
  sdmaxl = sd(subdat$maxL)
  ment = mean(subdat$ENTR)
  sdent = sd(subdat$ENTR)
  
  if (s==subs[1]) {
    #RRplot(mrr,mdet,pch=16,type='p',xlim=c(.7,1.4),ylim=c(4,7),col='gray',cex=.5)
    #RRpoints(c(mrr,mrr),c(mdet-sedet,mdet+sedet),type='l',col='gray')
    #RRpoints(c(mrr-serr,mrr+serr),c(mdet,mdet),type='l',col='gray')
    #ENT plot(ment,mdet,pch=16,type='p',xlim=c(0.23,.27),ylim=c(5.4,5.85),col='black')
    plot(mmaxl,mdet,pch=16,type='p',xlim=c(6,18),ylim=c(3,7),col='gray',xlab='MAXLINE',ylab='DET')
    points(c(mmaxl,mmaxl),c(mdet-sedet,mdet+sedet),type='l',col='gray')
    points(c(mmaxl-semaxl,mmaxl+semaxl),c(mdet,mdet),type='l',col='gray')
  } else {
    points(mmaxl,mdet,pch=16,type='p',col='gray',cex=.75)
    points(c(mmaxl,mmaxl),c(mdet-sedet,mdet+sedet),type='l',col='gray')
    points(c(mmaxl-semaxl,mmaxl+semaxl),c(mdet,mdet),type='l',col='gray')
  }
  text(mmaxl,mdet,paste(s,'\n'),cex=.5,pos=4,offset=.05)
}
dev.off()
summary(lm(maxL~topicCodes,data=results))

# table
library(doBy)
library(stargazer)
forDisplay = c()
for (s in sort(subs)) {
  dat=results[,c(1,2,4,5,6)]
  ix = which(subs == s)
  if (s=='Poetry') {
    dat = dat[dat$maxL<1000,] # ripple rhymes has repeat poem!
  }
  n = sum(topicCodes[,ix])
  res = data.frame(s,n,summaryBy(.~.,dat[topicCodes[,ix]==1,]))
  forDisplay = rbind(forDisplay,res)
}
library(latex2exp)
s = '$R^2$ of measure predicted'
Rsquareds = c()
for (i in c(1,2,4,5,6)) {
  r2 = summary(lm(results[,i]~topicCodes))$r.squared
  Rsquareds = c(Rsquareds,r2)
}
names(Rsquareds) = names(forDisplay[,3:7])
res = data.frame(s=s,n='',t(Rsquareds))
forDisplay = rbind(forDisplay,res)
write(file='gutenberg_table.txt',stargazer(forDisplay,summary=F,rownames=F,digits=2))

#genre dendrogram
dendroDat = scale(forDisplay[1:(nrow(forDisplay)-1),3:7])
row.names(dendroDat) = forDisplay$s[1:nrow(dendroDat)]
dendroDist = dist(dendroDat)
hClust = hclust(dendroDist)
pdf(file = "gutenberg_dendrogram.pdf", width = 7, height = 7)
plot(hClust,main='',ylab='',yaxt='none',xlab='')
dev.off()


pcaDat = princomp(dendroDat)
plot(pcaDat$scores[,1],pcaDat$scores[,2],col='white')
text(pcaDat$scores[,1],pcaDat$scores[,2],row.names(dendroDat),cex=.8)


#### sample RP
# let's use 12977
# Ambrose Bierce, Black Beetles in Amber
# subset(dat[dat$subj=='Poetry',],,c(gutenberg_id,title,author))[1:20,]
work_id = 12977
txt = withTimeout({
  gutenberg_download(gutenberg_id=work_id)
}, timeout=5)
txt = withTimeout({
  gutenberg_strip(txt)
}, timeout=5)
txt = txt[258:311,] # clean... will have to do this for all of 'em... ack....
txt = txt %>%
  unnest_tokens(word, text) # let's tidy this up
res = text_rqa(txt,typ="string") # time to run rqa
plot_rp(res$RP,cex=1)




