necpro<-read.csv(file="NEC_pro.csv", header=TRUE)
necproS<-read.csv(file="NecS.csv", header=TRUE)

par(mfrow=c(3,3))
par(mar=c(.5,.5,0,0), oma=c(4,9,2,2))
for(i in 3:11)
{
  barplot(height=necpro[,i], ann=F, axes=F, ylim=c(0,1.3), col=c("#08306b","#313695","#4575b4","#74add1","#abd9e9","#e0f3f8","#ffffbf","#fee090","#fdae61","#f46d43","#d73027","#a50026"))
  title(colnames(necpro)[i], line=-3.5, cex.main=2.8)
  #box(col="gray50")
  
  if(i %in% c(3,6,9)){ axis(side=2, xlab="Average Goede Score", at=c(0,.5,1.0), las=1, cex.axis=2)}
  if(i %in% c(4,5,7,8,10,11)){axis(side=2, labels= FALSE, tick=TRUE, at=c(0,.5,1.0), cex.axis=2)}
  if(i %in% c(3:11)){axis(side=1, labels=F, tick=T, lwd.ticks=0)}
  #if(i %in% c(4,7,10)){
  # text(x = seq(1, 12, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45, pos = 1, xpd = TRUE)
  #}
}
#mtext("Abnormality occurence", side=3, outer=TRUE, line=.1)
mtext("Proportion of fish affected", side=2, outer=TRUE, line=5, cex=2.3)
dev.off()
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("center",c("Coulter", "Rock Creek", "Issaquah","Church",
                  "Harris", "May", "Jenkins",
                  "Woodland", "Longfellow", "Mercer", 
                  "Swamp", "Thorton"), title="Site",  xpd = TRUE,
       inset = c(0, 0),cex=2.3, bty = "n", ncol=1, fill=c("#08306b","#313695","#4575b4","#74add1","#abd9e9","#e0f3f8","#ffffbf","#fee090","#fdae61","#f46d43","#d73027","#a50026"))

?legend
###Necrospy September Scores###
dev.off()
par(mfrow=c(3,3))
par(mar=c(.5,.5,0,0), oma=c(4,9,2,2))
for(i in c(3:11)) {
  barplot(height=necproS[,i], ann=F, axes=F, ylim=c(0,1.2), col=c("#08306b","#4575b4","#abd9e9","#e0f3f8","#ffffbf","#d73027"))
  title(colnames(necpro)[i], line=-3.5, cex.main=2.8)
  #box(col="gray50")
  
  if(i %in% c(3,6,9)){ axis(side=2, xlab="Average Goede Score", las=1, cex.axis=2, at=c(0,.5,1.0))}
  if(i %in% c(4,5,7,8,10,11)){axis(side=2, labels= FALSE, tick=TRUE, at=c(0,.5,1.0), cex.axis=2)}
  if(i %in% c(3:11)){axis(side=1, labels=F, tick=T, lwd.ticks=0)}
  #if(i %in% c(4,7,10)){
  # text(x = seq(1, 12, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45, pos = 1, xpd = TRUE)
  #}
}
#mtext("Abnormality occurence", side=3, outer=TRUE, line=.1)
mtext("Proportion of fish affected", side=2, outer=TRUE, line=5, cex=2.3)
dev.off()
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("center",c("Coulter", "Issaquah",
                  "Harris", "May", "Jenkins",
                  "Swamp") , title="Site",  xpd = TRUE,
       inset = c(0, 0),cex=2.3, bty = "n", fill=c("#08306b","#4575b4","#abd9e9","#e0f3f8","#ffffbf","#d73027"))

