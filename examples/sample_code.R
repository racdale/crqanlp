
setwd("~/Dropbox/new.projects/recurrence-recipes-2/crqanlp/examples")
# in place of just loading the future library:
library(tm)
library(crqa)
library(RCurl)
lapply(list.files('../R'),function(x) { source(paste('../R/',x,sep='')) })

a = text_rqa('14127.txt',typ='file',embed=1,tw=0)
#pdf(file='sample_rp.pdf',height=7,width=7)
plot_rp(a$RP,cex=.25,xlab='i (word)',ylab='j (word)')
a = text_win_rqa('14127.txt',typ='file',winsz=500,wshft=20)
#dev.off()
#plot_text_rp(a$RP,rsrc=url,typ='url',cex=.5)

txt = 'a b c d e d c b a'
a = text_rqa(txt,typ='string',tw=0)
plot_rp(a$RP,cex=2)
plot_text_rp(a$RP,rsrc=txt,typ='string',cex=2.5)

