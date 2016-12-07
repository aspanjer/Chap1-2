###PAUASE OUT TO MAKE GRAPHS###

coho.p.J<-read.csv(file="Model_results_coho_J.csv", header=T)
?boxplot
par(cex.lab=1.5, cex.axis=1.7)
boxplot(coho.p.J$Final.Weight~coho.p.J$site.level, names=coho.9, ylab="Final Weight (g)", xaxs="i", cex.names=5, col="blue")
coho.9<-c("Coulter","E.F. Dariy","Issaquah","Harris","Church","May","Woodland","Jenkins","Longfellow","Swamp")
coho.p.S<-read.csv(file="Model_results_coho_S.csv", header=T)
boxplot(coho.p.S$Final.Weight~coho.p.S$site.level, names=coho.6, ylab="% of maximum consumption", xaxs="i", col="blue")
coho.6<-c("Coulter","Issaquah", "Harris", "May", "Jenkins", "Swamp")
?plot
