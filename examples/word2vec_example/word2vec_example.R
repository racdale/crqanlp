
setwd('~/Dropbox/new.projects/recurrence-recipes-2/crqanlp/examples')
library(wordVectors)
library(magrittr)
#perl -ne 's/[^A-Za-z_0-9 \n]/ /g; print lc $_;' cookbooks/*.txt > cookbooks.txt
prep_word2vec(origin="some_gutenberg_books",destination="transcripts.txt",lowercase=T,bundle_ngrams=1)
model = train_word2vec("transcripts.txt","vectors.bin",vectors=200,threads=4,window=12,iter=5,negative_samples=0,force=T)
model %>% closest_to("fresh")
txt = as.character(read.table('14127.txt')$x)
txt = unlist(strsplit(txt,' '))
textVecs = c()
for (wd in txt) {
  textVecs = rbind(textVecs,model[[wd]])
}
textVecs = scale(textVecs)
D = textVecs[1:2000,] %*% t(textVecs[1:2000,])
D = abs(D)
thresh = sort(D)[.05*(2000^2)]
RP = RP = 1*(D<thresh)
res = crqa(RP,tw=1,recpt=T,mindiagline=2,minvertline=2)
pdf(file='semantic_rp.pdf',height=7,width=7)
plot_rp(RP,cex=.1,xlab='i (word)',ylab='j (word)')
dev.off()
#sRP = as.matrix(dist(textVecs))
#RP = 1*(sRP<mean(sRP,na.rm=T)-4*sd(sRP,na.rm=T))
#
#res$RR
#
