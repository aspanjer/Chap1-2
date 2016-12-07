Sim.1 <- RUN.FISH.BIOENERGETICS("10AL.txt", Weight.Initial=.2, 
                                Weight.Final=8.3, Spawning.Day=0, Gonad.Loss=0, Con.Equation=3, 
                                EG.EX.Equation=3, Resp.SDA.Equation=2, Pred.ED.Equation=2) 

#Easiest to set your working directory to where the source code is stored. Simple to set the working directory  
#in R-Studio---On main menue---Session-->Set Working Directory--->To Source File Location. 
write.csv(x=Sim.1, file="19AE.csv")