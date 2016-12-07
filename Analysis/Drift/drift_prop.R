drift<-read.csv(file="drift_prop_r.csv")
diet<-read.csv(file="diet_prop.csv")
#Stacked barplot
co.c<-paste(c("#53bf00", "#4EBF00",	"#5BBF00",	"#B7BF00",	"#BFBC00",	"#BFA000", "#BF8100",	"#BF5B00",	"#BF2300"))
par(mar=c(9.5, 5.1, 1, 15), xpd=TRUE)
barplot(t(diet[1:10,2:10]),
        ylab="", ylim=c(0,1), 
        col=co.c,axes=F, names.arg= sites.s, las=2, cex.names=2)
sites.s<-drift[1:10,1]
title(ylab="Proportion of Diet", line=6, cex.lab=2)
#for(i in 1:6){
#barplot(t(food[i,4:6]), space=i+1, add=TRUE, main="Diet Energy Proportion",
#       ylab="Proportion of Diet", ylim=c(0,1.1), 
#       col=color[i])
#}

axis(side=2, at=c(0,.25,.5,.75,1),cex.axis=1.8, las=1)
legend("topright", inset=c(-.52,0), c("Coleoptera", "Hemiptera", 
                                    "Hymenoptera", "Other Adult", 
                                    "Aquatic larvae \n(rigid-bodied)", 
                                    "Winged insect", "Aquatic Nymphs",
                                    "Aquatic Other","Aquatic larvae \n(soft-bodied)"), 
       xpd=TRUE, fill= rev(co.c), bty="n", cex=2)
