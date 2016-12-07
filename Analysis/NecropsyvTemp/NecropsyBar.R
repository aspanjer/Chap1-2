#Need to change to proportion of occurance and add an axis with either site names and color coded from low urban to high
necavg<-read.csv(file="NEC.csv")
lab<-necavg[,1]
par(mfcol=c(3,3))
par(mar=c(.5,.5,0,0), oma=c(4,4,2,2))
for(i in 2:10)
{
  barplot(height=necavg[,i], ann=F, axes=F, ylim=c(0,30), col=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928'))
  title(colnames(necavg)[i], line=-1)
  box(col="gray50")
    
  if(i %in% c(2,3,4)){ axis(side=2, xlab="Average Goede Score")}
  #if(i %in% c(4,7,10)){
   # text(x = seq(1, 12, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45, pos = 1, xpd = TRUE)
    #}
}
mtext("Coho Goede Gross Abnormality Necropsy Scores", side=3, outer=TRUE, line=.1)
mtext("Average Goede Score", side=2, outer=TRUE, line=2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom",c("Coulter", "Rock Creek", "Issaquah","Church",
                  "Harris", "May", "Jenkins",
                  "Woodland", "Longfellow", "Mercer", 
                  "Swamp", "Thorton") , xpd = TRUE,
       inset = c(0, 0), bty = "n", lty=1, lwd=10, ncol=6, col=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928'))


necavg$Site


c("Coulter", "Rock Creek")
legend("bottom", necavg[,1],lty=1,col=terrain.colors(12), ncol=2, bty="n")
dev.off()
)


