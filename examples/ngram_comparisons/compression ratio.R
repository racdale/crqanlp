
text1 = c(rep("a",10),rep("b",10),rep("c",10),rep("a b",10),rep("b c",10),rep("a b c",10))

dat=c()
for (i in 1:1000) {
  print(i)
  txt = paste(base::sample(text1,100,replace=T),collapse=' ')
  res = text_rqa(txt,typ='string')
  detres = res$DET
  dv2 = res$NRLINE
  
  write(txt,file='temp.txt')
  zip(zipfile='temp.txt.zip',files=c('temp.txt'))
  cRatio = file.info('temp.txt.zip')$size/file.info('temp.txt')$size
  
  dat = rbind(dat,c(detres,1-cRatio))
}

library(latex2exp)
pdf(file='det_cratio.pdf',height=5,width=4)
plot(dat[,1],dat[,2],xlab=TeX('$DET$'),ylab=TeX('compressibility ratio'),pch=16,col='gray',cex=0.75)
lmo = lm(dat[,2]~dat[,1])
abline(lmo)
lmo$coefficients = round(lmo$coefficients*1000)/1000
model_eq = paste('Y = ',lmo$coefficients[2],'X +',lmo$coefficients[1])
model_txt = paste(model_eq,'$R^2 =',round(summary(lmo)$r.sq*1000)/1000,'$')
text(73,.35,TeX(model_txt),cex=.75)
cor.test(dat[,1],dat[,2])
dev.off()


