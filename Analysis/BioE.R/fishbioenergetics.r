#===============================================================================================================
         ####<><><><><><><><><><><>---Wisconsin Bioenergetics Model---<><><><><><><><><><><>####
#===============================================================================================================

#Coded by Adam G. Hansen --> Updated 5-20-13.
#Washington Cooperative Fish and Wildlife Research Unit, School of Aquatic and Fishery
#Sciences, University of Washington, BOX: 355020, Seattle, Washington 98105.

#Code follows Hanson et al. (1997).

#This script has been validated against Fish Bioenergetics 3.0.

#The model: Consumption = (Respiration + Active Metabolism + Specific Dynamic Action) 
#           + (Egestion + Excretion) + (Somatic Growth + Gonad Growth)
#           C = (R + A + S) + (F + U) + (DeltaB + G)

#This script is designed to fit to observed growth only, not consumption and 
#only runs simulations for an individual fish (i.e., no population 
#scaling or cohort mortality).

#Input file should follow heading strucure outlined below (space-deliminted in 
#.txt file):

#SimDay Temp Diet1 Diet2 Diet3 Diet4 Diet5 Diet6 Diet7 Diet8 Diet9 Diet10
#X X X X X X X X X X X X 
#X X X X X X X X X X X X 
 
#Description of main data frame headings: Simulation day; temperature experienced 
#on each day; proportion of each diet category (1 --> 10) on each day.  
#Must put zeros in for diet items not being used in your analysis.   

#===============================================================================================================
              #-----START OF PRIMARY FISH BIOENERGETICS FUNCTION: {RUN.FISH.BIOENERGETICS}-----#
#===============================================================================================================

#Default equations are for cool-cold water species:
#to put into a for loop you can add a vector of initial and 
#final weights and days of the simulation based on the differently 
#determined emergence dates, then feed into the eqution below. Using 
#this method we can acheive the ability to incorporate error terms 
#on the model outputs for Cmax resulting in a better estimate of 
#consumption for the stream. 

RUN.FISH.BIOENERGETICS <- function(DataFile,Weight.Initial=200,Weight.Final=10,Spawning.Day=0,Gonad.Loss=0,Con.Equation=3,
                                   EG.EX.Equation=2, Resp.SDA.Equation=2, Pred.ED.Equation=2) #equation selection based on the work of Willey 2004
{
#Read in the user defined data:
TheData <- read.csv(DataFile,header=T)

#Name of simulation?
Sim.Name <- "Example"

#What species are we modeling?
Modeled.Species <- "Coho salmon"                      

#===============================================================================================================
                                    #-----INPUT PHYSIOLOGICAL PARAMETERS-----#
#===============================================================================================================
#REMEMBER: If the equation numbers selected when calling {Run.FISH.BIOENERGETICS} are not appropriate for the 
#species being modeled, this script may not run (i.e., inappropriate equations will likely be undefined for 
#some parameter values.

#Consumption related parameters (user defined):
CA <- 0.303 #Hanson et al. 1997 
CB <- -0.275 #Hanson et al. 1997
CQ <- 5 #Hanson et al. 1997
CTO <- 15 #Hanson et al. 1997
CTM <- 18 #Hanson et al. 1997
CTL <- 26 #Willey 2004
CK1 <- 0.42 #Willey 2004
CK4 <- 0.03 #Willey 2004

#Respiration related parameters (user defined):
RA <- 0.0046 #White and Li 1985
RB <- -0.217 #Hanson et al. 1997
RQ <- 2.1 #Hanson et al. 1997
RTO <- 18 #Brett 1952
RTM <- 26 #Brett 1952
RTL <- 25# Not sure this is needed for the juvenile coho function, need to determine if there is more to this
RK1 <- 1
RK4 <- 0.13
ACT <- 2 #Kitchell et al. 1997
BACT <- 0.0405 #not sure
SDA <- 0.172 #Hanson et al. 1997
OxyConv <- 13560 #J/gram of O2 in respiration conversions (Elliot and Davidson 1975).   

#Egestion and excretion parameters (user defined):
FA <- 0.212 #Hanson et al. 1997
FB <- -0.522 #Willey 2004
FG <- 0.631 #Hanson et al. 1997
UA <- 0.0214 #Willey 2004
UB <- 0.380 #Willey 2004
UG <- -0.299 #Hanson et al. 1997

#Predator energy densities for EQUATION 1 (user defined):
Predator.Energy <- 5000

#Predator energy densities for EQUATION 2 (user defined):
AlphaI <- 4111
BetaI <- 155
Thresh <- 10
AlphaII <- 7602
BetaII <- 0.5266 

#Prey energy densities: script can handle TEN diet items.  Must put ZERO's in 
#for diet slots not being used, otherwise sript won't run.  These slots must 
#line up with diet proportion columns outlined in the input data file.
Prey1 <- 5778 
Prey2 <- 3454.5 
Prey3 <- 3700 
Prey4 <- 0 
Prey5 <- 0 
Prey6 <- 0
Prey7 <- 0
Prey8 <- 0
Prey9 <- 0
Prey10 <- 0

#Prey indigestibilites in terms of proportions of prey: make sure prey 
#proportions/densities correspond (values should be < 1). 
IndPrey1 <- 0.03
IndPrey2 <- 0.17
IndPrey3 <- 0.17
IndPrey4 <- 0
IndPrey5 <- 0
IndPrey6 <- 0
IndPrey7 <- 0
IndPrey8 <- 0
IndPrey9 <- 0
IndPrey10 <- 0

#Consumer inputs (grams):
Weight0 <- Weight.Initial #Intial consumer weight.
WeightF <- Weight.Final   #Final weight of consumer after simulation period.
Gonad.Loss <- Gonad.Loss  #As a proportion of fish body mass.  
                   
Gonad.Loss.Day <- Spawning.Day   #Day of simulation that corresponds to spawning and gamete 
                                 #loss. Use zero in inital function call if simulation period does 
                                 #not include spawning. Modeled such that gamete release occurs after 
                                 #growth on this day.

#===============================================================================================================
#FUNCTION NUMBER 1: {Bioenergetics}          

#Runs bioenergetics model for a single day in a given simulation and spits out
#vector of bioenergetics quantities --> Used for fitting the p-value and for
#generating consumption once p-value has been fit --> For looping below.
#===============================================================================================================

#"i" is a row in the input data file, "WO" is the consumer weight, and "P" is an arbitrary
#p-value that will eventually be fitted to observed growth using this function.

Bioenergetics <- function(i,W0,P)    
{  
  
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
        #-----CONSUMPTION: Choice among 3 temperature dependent functions [f(t)] for CONSUMPTION-----# 
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#Exponential---useful when temperatures are always below physiological optimum for species:  
if (Con.Equation == 1) {ft.con <- exp(CQ*TheData$Temp[i])}

#Warm-water species:
if (Con.Equation == 2) {
                        V.con <- (CTM-TheData$Temp[i])/(CTM-CTO) 
                        Z.con <- log(CQ)*(CTM-CTO)  
                        Y.con <- log(CQ)*(CTM-CTO+2)
                        X.con <- (Z.con^2*(1+(1+40/Y.con)^0.5)^2)/400
                        ft.con <- (V.con^X.con)*exp(X.con*(1-V.con)) 
                        } 

#Cool and cold-water species:
if (Con.Equation == 3) {
                        G1 <- (1/(CTO-CQ))*log((0.98*(1-CK1))/(CK1*0.02))   
                        G2 <- (1/(CTL-CTM))*log((0.98*(1-CK4))/(CK4*0.02)) 
                        L1 <- (exp(G1*(TheData$Temp[i]-CQ)))
                        L2 <- (exp(G2*(CTL-TheData$Temp[i]))) 
                        KA <- (CK1*L1)/(1+CK1*(L1 - 1)) 
                        KB <- (CK4*L2)/(1+CK4*(L2 - 1))
                        ft.con <- (KA*KB)
                        }

  Consumption <- (CA*W0^CB)*P*ft.con #Units --> Specific (g/g/d).

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
          #-----EGESTION (solid) and EXCRETION (nitrogenous): Choice among 3 waste loss functions-----# 
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#Both types of waste loss are simply a constant fraction of consumption: 
if (EG.EX.Equation == 1) {
                          Egestion <- FA*Consumption 
                          Excretion <- UA*(Consumption-Egestion)
                          } 

#Waste loss is dependent on mass, temperature, and ration:
if (EG.EX.Equation == 2) {
                          Egestion <- FA*(TheData$Temp[i]^FB)*exp(FG*P)*Consumption
                          Excretion <- UA*(TheData$Temp[i]^UB)*exp(UG*P)*(Consumption-Egestion)
                          }

#Similar to equation 2, but with correction for indigestible prey (PFF):
if (EG.EX.Equation == 3) {
                          PFF <-((IndPrey1*TheData$Diet1[i])+ (
                                  IndPrey2*TheData$Diet2[i])+ (
                                  IndPrey3*TheData$Diet3[i])+ (
                                  IndPrey4*TheData$Diet4[i])+ (
                                  IndPrey5*TheData$Diet5[i])+ (
                                  IndPrey6*TheData$Diet6[i])+ (
                                  IndPrey7*TheData$Diet7[i])+ (
                                  IndPrey8*TheData$Diet8[i])+ (
                                  IndPrey9*TheData$Diet9[i])+ (  
                                  IndPrey10*TheData$Diet10[i]))
                          PE <- FA*(TheData$Temp[i]^FB)*exp(FG*P) 
                          PF <- ((PE-0.1)/0.9)*(1-PFF)+PFF
                          Egestion <- PF*Consumption
                          Excretion <- UA*(TheData$Temp[i]^UB)*exp(UG*P)*(Consumption-Egestion)
                          }

                          #Units --> Specific (g/g/d).

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                  #-----RESPIRATION and SDA: Choice between 2 functions for RESPIRATION-----# 
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#Exponential with swimming speed--dependent on mass and water temperature below a cuttoff (RTL):
if (Resp.SDA.Equation == 1) {
                             ifelse(TheData$Temp[i] > RTL, VEL <- RK1*W0^RK4, VEL <- ACT*(W0^RK4)*exp(BACT*TheData$Temp[i]))
                             ft.metabolism <- exp(RQ*TheData$Temp[i])
    	                       ACTIVITY <- exp(RTO*VEL)                           
                             } 

#Temperature dependent with activity multiplier:
if (Resp.SDA.Equation == 2) {
                             V.resp <- (RTM-TheData$Temp[i])/(RTM-RTO) 
                             Z.resp <- log(RQ)*(RTM-RTO)  
                             Y.resp <- log(RQ)*(RTM-RTO+2)
                             X.resp <- (Z.resp^2*(1+(1+40/Y.resp)^0.5)^2)/400 
                             ft.metabolism <- (V.resp^X.resp)*exp(X.resp*(1-V.resp))
                             ACTIVITY <- ACT                          
                             }
         
  Respiration <- RA*(W0^RB)*ft.metabolism*ACTIVITY #Units --> Specific (g O2/g/d). 
  
  SDAction <- SDA*(Consumption-Egestion) #Units --> Specific (g/g/d) 
 
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                          #-----PREDATOR ENERGY (J/g): Choice between 2 Equations-----# 
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> 

if (Pred.ED.Equation == 1) {Pred.Energy <- Predator.Energy}
                           
if (Pred.ED.Equation == 2 & W0 < Thresh) {Pred.Energy <- AlphaI+(BetaI*W0)}

if (Pred.ED.Equation == 2 & W0 >= Thresh) {Pred.Energy <- AlphaII+(BetaII*W0)}

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                   #-----Convert grams to joules and calculate GROWTH for simulation day i-----# 
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

 Growth <-           ((((Consumption)*((
                                  Prey1*TheData$Diet1[i])+ (
                                  Prey2*TheData$Diet2[i])+ (
                                  Prey3*TheData$Diet3[i])+ (
                                  Prey4*TheData$Diet4[i])+ (
                                  Prey5*TheData$Diet5[i])+ (
                                  Prey6*TheData$Diet6[i])+ (
                                  Prey7*TheData$Diet7[i])+ (
                                  Prey8*TheData$Diet8[i])+ (
                                  Prey9*TheData$Diet9[i])+ (  
                                  Prey10*TheData$Diet10[i])))-(
                      (Egestion+Excretion+SDAction)*((
                                  Prey1*TheData$Diet1[i])+ (
                                  Prey2*TheData$Diet2[i])+ (
                                  Prey3*TheData$Diet3[i])+ (
                                  Prey4*TheData$Diet4[i])+ (
                                  Prey5*TheData$Diet5[i])+ (
                                  Prey6*TheData$Diet6[i])+ (
                                  Prey7*TheData$Diet7[i])+ (
                                  Prey8*TheData$Diet8[i])+ (
                                  Prey9*TheData$Diet9[i])+ (  
                                  Prey10*TheData$Diet10[i])) + (Respiration*OxyConv)))/Pred.Energy)*W0    #In terms of daily weight gain (g/day).
                                                                                                          #Divide this value by W0 to get back 
                                                                                                          #to a specific rate (g/g/d).

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
          #-----Update GROWTH if spawning occured and return vector of bioenergetics quantities-----# 
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
 
 Consumption.Specific <- Consumption #Units --> g/g/d
 Consumption.total <- Consumption.Specific*W0 #Units --> g/d
          
 #If spawning occured: need to update total growth on spawning day:
 if (i == Gonad.Loss.Day & Gonad.Loss > 0)          
 {Growth <- Growth-(Gonad.Loss*(Growth+W0))} 
 else
 {Growth <- Growth} #Units --> g       
          
 Growth.Specific <- Growth/W0 #Units --> g/g/d

 #Weight of fish at end of simulation day:
 Weight.End <- Growth+W0 #Units --> g   

 #Egestion, Excretion, Respiration, and SDA are in specific terms.
          
 #Return a vector of all bionergetics quantities from the days simulation: 
 return(c(Growth,Growth.Specific,Weight.End,Consumption.total,Consumption.Specific,
          Egestion,Excretion,Respiration,SDAction))
}

#===============================================================================================================
                                      #---End of FUNCTION NUMBER 1---#
#===============================================================================================================

#===============================================================================================================
#FUNCTION NUMBER 2: {FIT.PVALUE}

#Primary function that computes the deviation between observed and estimated growth based on some defined P-value. 
#This function will be optimized below (i.e., P-value that makes function output equal zero).        
#===============================================================================================================

FIT.PVALUE <- function(P)
{                
P1 <- P[1]
                                                                                                                    
 #Matrix of zeros to fill with bioenergetics quantities once looping is initiated.
 Results <- matrix(0,ncol=9,nrow=(length(TheData$SimDay))) 

 #Need a starting point to intitiate looping --> compute bioenergetics quantities for simulation day 1: 
 Results[1,] <- Bioenergetics (1,Weight0,P1)

 #Loop through data for every day of simulation period defined by length of input .txt file.
 #Growth accruing from previous days are continuously added to the consumers weight throughout 
 #the simulation period.
 for (i in 2:length(TheData$SimDay))
 {
 Results[i,] <- Bioenergetics (i,Results[i-1,3],P1)
 }

 #How much did the fish grow over the course of the simulation period? 
 G.total <- sum(Results[,1])
 
 #Total observed growth:
 Total.Growth <- WeightF-Weight0
 
 #Return the deviation between observed and predicted growth.  The following accounts for all 
 #possible growth responses (i.e., posotive, negative, static).  This quantity will be minimized
 #once the optimization routine below is called.
 return(abs(G.total-Total.Growth))
}

#===============================================================================================================
                                        #---End of FUNCTION NUMBER 2---#
#===============================================================================================================

#===============================================================================================================
#FUNCTION NUMBER 3: {GET.CONSUMPTION}

#Repeat simulations, extract total consumption, and split consumption into individual diet components using best
#fit P-value (P1):
#===============================================================================================================

GET.CONSUMPTION <- function (P1)
{  

 #Matrix of zeros to fill with bioenergetics quantities once looping is initiated.
 Results <- matrix(0,ncol=9,nrow=(length(TheData$SimDay)))

 #Need a starting point to intitiate looping --> compute bioenergetics quantities for simulation day 1: 
 Results[1,] <- Bioenergetics (1,Weight0,P1)

 #Loop through data for every day of simulation period defined by length of input .txt file.
 #Growth accruing from previous days are continuously added to the consumers weight throughout 
 #the simulation period.
 for (i in 2:length(TheData$SimDay))
 {
 Results[i,] <- Bioenergetics (i,Results[i-1,3],P1) 
 }

#===============================================================================================================
                                #-----BUILD TABLE OF KEY RESULTS FOR USER-----#
#===============================================================================================================
 
 #How many days were in the simulation period? 
 Day <- data.frame(seq(1,length(TheData$SimDay),by=1)) #Units --> days

 #How much prey did the fish consume in total on every day of simulation(summed across diet items)? 
 Daily.Total.Con <- Results[,4] #Units --> g/day

 #Partition total daily consumption into different diet items based on diet proportions specified 
 #by the user: 
 Daily.Total.Con.Diet1 <- Daily.Total.Con*TheData$Diet1 #Units --> g/day
 Daily.Total.Con.Diet2 <- Daily.Total.Con*TheData$Diet2 #Units --> g/day
 Daily.Total.Con.Diet3 <- Daily.Total.Con*TheData$Diet3 #Units --> g/day
 Daily.Total.Con.Diet4 <- Daily.Total.Con*TheData$Diet4 #Units --> g/day
 Daily.Total.Con.Diet5 <- Daily.Total.Con*TheData$Diet5 #Units --> g/day
 Daily.Total.Con.Diet6 <- Daily.Total.Con*TheData$Diet6 #Units --> g/day
 Daily.Total.Con.Diet7 <- Daily.Total.Con*TheData$Diet7 #Units --> g/day
 Daily.Total.Con.Diet8 <- Daily.Total.Con*TheData$Diet8 #Units --> g/day
 Daily.Total.Con.Diet9 <- Daily.Total.Con*TheData$Diet9 #Units --> g/day
 Daily.Total.Con.Diet10 <- Daily.Total.Con*TheData$Diet10 #Units --> g/day

 #Combine consumption of all diet items into a new data frame: 
 All.Diet <- data.frame (cbind(Daily.Total.Con.Diet1,
                        Daily.Total.Con.Diet2, 
                        Daily.Total.Con.Diet3, 
                        Daily.Total.Con.Diet4, 
                        Daily.Total.Con.Diet5, 
                        Daily.Total.Con.Diet6, 
                        Daily.Total.Con.Diet7, 
                        Daily.Total.Con.Diet8, 
                        Daily.Total.Con.Diet9, 
                        Daily.Total.Con.Diet10)) 

 #How much did the fish weigh at the end of each simulation day?
 Daily.Weight <- Results[,3] #Units --> g
 
 #How much did the fish grow on every day of the simulation? 
 Daily.Weight.Gain <- Results[,1] #Units --> g/day

 #Determine which diet items to include in output summary table (i.e., which
 #of the ten possible slots were actually used by the user?)
 Diet.Check <- rep(0,12) 

 for (i in 3:12)
 {
 Diet.Check[i] <- if (sum(TheData[,i])>0)
                 {1}
                 else
                 {0}
 }

 Include.Diet <- matrix(0,ncol=sum(Diet.Check),nrow=length(TheData$SimDay))
 
 for (i in 1:sum(Diet.Check))
 {
 Include.Diet[,i] <- All.Diet[,i]   
 }

 #Combine all these lists of quanitites together into a streamlined output table to return to the user: 
 Output <- cbind(Day, Include.Diet,                            
                      Daily.Total.Con,
                      Daily.Weight.Gain,
                      Daily.Weight)
                            
 #Give each column of the output table a meaningful name: 
 colnames(Output) <- c("Sim.Day",
                      if ((TheData$Diet1[1])>0){"D1.Con.(g/d)"},
                      if ((TheData$Diet2[1])>0){"D2.Con.(g/d)"},
                      if ((TheData$Diet3[1])>0){"D3.Con.(g/d)"},
                      if ((TheData$Diet4[1])>0){"D4.Con.(g/d)"},
                      if ((TheData$Diet5[1])>0){"D5.Con.(g/d)"},
                      if ((TheData$Diet6[1])>0){"D6.Con.(g/d)"},
                      if ((TheData$Diet7[1])>0){"D7.Con.(g/d)"},
                      if ((TheData$Diet8[1])>0){"D8.Con.(g/d)"},
                      if ((TheData$Diet9[1])>0){"D9.Con.(g/d)"},
                      if ((TheData$Diet10[1])>0){"D10.Con.(g/d)"},
                      "Total.Con.(g/d)",
                      "W.Gain.(g/d)",
                      "Daily.Weight.(g)")                      
 return (Output)   
}

#===============================================================================================================
                                       #---End of FUNCTION NUMBER 3---#
#===============================================================================================================

#===============================================================================================================
#FUNCTION NUMBER 4 (starts at top of script --> {RUN_FISH_BIOENERGETICS}):

#Function based on input file that returns consumption resulting from user Specifications. For those unfamiliar 
#with R, use the following commands --> load functions written in this script into R by running the command: 

#source("C:\\fishbioenergetics.r") 

#in the R console.  THE SCRIPT MUST BE PLACED ON LOCAL DISK (C:) FOR THIS COMMAND TO WORK. This command only 
#needs to be run once every time R is opened.

#To run the model given what has been outlined in the .txt input file which must be named 'bio_input_data.txt' 
#and also placed on Local Disk (C:), type the command with user defined start & end weights, spawning day, and 
#gonad loss:

#RUN.FISH.BIOENERGETICS("C:\\bio_input_data.txt",Weight.Initial, Weight.Final, Spawning.Day, Gonad.Loss)

#Consumption output will be printed to the R console.  Cut and paste into Excel, manipulate further in R, or
#save as a .csv file to working directory using the "Write" funciton.
#===============================================================================================================

 #Fit the p-value.  An upper limit of 2.0 is set for the P-value. 
 Pvalue.Opt <- optimize(f=FIT.PVALUE,lower=0,upper=2,tol=0.0000001) 

 #Extract the fitted p-value: 
 Best.Pvalue <- data.frame(Pvalue.Opt[1])

 #What is the quantity of the function FIT.PVALUE that corresponds  
 #with the optimal P-value? 
 Objective <- data.frame(Pvalue.Opt[2])
 
 #Use the best P-value and model consumption: 
 Results.Final <- GET.CONSUMPTION(Best.Pvalue[1,1]) 

 #Print some important information for the user at the top of the output table: 
 print ("Name of Simulation")
 print (Sim.Name)
 print("----------------------------")
 print ("Modeled Species")
 print (Modeled.Species)
 print("----------------------------")
 print ("Initial Weight")
 print (Weight0) 
 print("----------------------------")
 print ("Final Weight")                                         
 print (WeightF)
 print("----------------------------") 
 print ("Best fit P-value")
 print (Best.Pvalue[1,1]) 
 print("----------------------------")
 print("Value of the function FIT.PVALUE coresponding to optimal P")
 print (Objective[1,1])
 print("<><><><><><><><><><><><><><><>")
 print("Results of Simulation") 
 print("<><><><><><><><><><><><><><><>")

 #Return the final results to the user: 
 return (Results.Final)
}

#===============================================================================================================
                                         #---End of FUNCTION NUMBER 4---#
#===============================================================================================================

TheData <- read.csv("3ACO.csv",header=T)

#A useful way to save your simulation is to use the {write.csv} function, which will save the results returned from
#the function {RUN.FISH.BIOENERGETICS} as a .csv file to your working directory.  To do this, you must name your 
#simulation as an object.  The example below names the simulation "Sim.1", and the output file 
#"Bioenergetics results.csv":
Sim.1 <- RUN.FISH.BIOENERGETICS("3ACO.csv", Weight.Initial=200, 
                                Weight.Final= 1100, Spawning.Day=0, Gonad.Loss=0, Con.Equation=3, 
                                EG.EX.Equation=3,Resp.SDA.Equation=1, Pred.ED.Equation=2) 

Sim.1<-RUN.FISH.BIOENERGETICS("3ACO.csv",Weight.Initial=200,Weight.Final=1100,Spawning.Day=0,Gonad.Loss=0,Con.Equation=3,
                                   EG.EX.Equation=2, Resp.SDA.Equation=2, Pred.ED.Equation=2)

#Easiest to set your working directory to where the source code is stored. Simple to set the working directory  
#in R-Studio---On main menue---Session-->Set Working Directory--->To Source File Location. 
write.csv(x=Sim.1, file="Bioenergetics results.csv")
