#Code to calculate Hellinger Distance measures
#Required data files include:
#Global land area: "land.shp"
# Data files:
# Dornelas et al. points: [see authors for data]
# Vellend et al. points: "vellendpts.shp"
# Raster files:
#   IUCN Marine Biodiveristy: "AllTaxa1.tif"
#   Halpern et al. Marine Impacts: "nceas_wgs.tif"
#   Keft & Jetz Terrestrial Plant Richness: "kj_tr.tif"
#Also needed: Need 'rand' and 'vellend' outputs from Chi-Sq.R #which are modfied output from extraction of Hansen et al. Forest #Cover Change data to vellend pts/random pts, done in Google #Earth Engine. See GEE_hansen.txt file for the JavaScript code #for GEE. 
    

  
#packages
library(rgdal)
library(dismo)
library(maptools)
library(distr)
library(distrEx)

#working directory:
setwd("~/GitHub/Biodiversity")
#h ome working directory:
#setwd("~/Biodiversity/Biodiversity")

#load land shapefile
land<- readShapePoly('land.shp')

#####SET UP DATA###############################################
#LOAD POINT FILES 
Dornpts<-readShapePoints("Dornelas_mar.shp")
names(Dornpts)[names(Dornpts)=="Latitude"]<- "y"
names(Dornpts)[names(Dornpts)=="Longitude"]<- "x"
#reorder x & y
Dornxy<- Dornpts[,c(2,1)]

setwd("~/GitHub/Biodiversity")
Vellendpts<-readShapePoints("vellendpts.shp")
names(Vellendpts)[names(Vellendpts)=="Latitude"]<- "y"
names(Vellendpts)[names(Vellendpts)=="Longitude"]<- "x"
#reorder x & y
Vellendxy<- Vellendpts[,c(2,1)]

#LOAD RASTERS:
#Marine Biodiversity from IUCN 
rnorm <- raster("marineAllNorm.tif") #normalized for each taxon then averaged for "mean diversity"
rnd<- disaggregate(rnorm, fact=10)
#r<- raster("marineBiodivAll.tif") #all taxa with 0s on land
r<-raster("AllTaxa1.tif")  #same as above but export w NAs instead of 0s
rd<-disaggregate(r, fact=10)
#
#MAKE A MASK
mat3<- c(0, 0, NA, 1, 3410,1) #assign 0 to NA
na.mat2<-matrix(mat3, ncol=3, byrow=TRUE) #transf to matrix
r01<- reclassify(rd, na.mat2)  #apply to raster to reclassify pixels to NA or 1 to make a mask
r.masked<- mask(rd, r01, maskvalue=0)
#use r.masked to generate Random Points
#save(r.masked, file="r.masked.RData") #exported to take upstairs

#RECLASSIFY RASTERS TO QUANTILES:     
#secies richness
rquants    <- c(0,85,1, 85,113,2,  113,368,3, 368,3410, 4) #assign breaks
rclmat <- matrix(rquants,ncol=3,byrow=TRUE)  #transf to matrix
rq <- reclassify(r.masked, rclmat) #apply it to the raster to reclassify values according to matrix
####USE rq  when plotting random pts to ensure they are only on water##############

#normalized richenss   NOTE: did not impact assessment so not used in final figs.
#MAKE A MASK
nmat3<- c(0, 0, NA, 0.000001, 1,1) #assign 0 to NA
na.nmat2<-matrix(nmat3, ncol=3, byrow=TRUE) #transf to matrix
nr01<- reclassify(rnd, na.nmat2)  #apply to raster to reclassify pixels to NA or 1 to make a mask
nr.masked<- mask(rnd, nr01, maskvalue=0)
#quantile(rnd)
rnquants<- c(0,0.14,1, 0.14,0.26,2, 0.26,0.33,3, 0.33,0.78,4)
rnclmat<- matrix(rnquants, ncol=3, byrow=TRUE)
rnq<- reclassify(nr.masked, rnclmat)

######NCEAS Marine Impacts DATA - Halpern et al.###############################
nceas<- raster("nceas_wgs.tif")
#MAKE A MASK
mat4<- c(0, 0, NA, 0.00001, 80, 1) #assign 0 to NA
na.mat4<-matrix(mat4, ncol=3, byrow=TRUE) #transf to matrix
n01<- reclassify(nceas, na.mat4)  #apply to raster to reclassify pixels to NA or 1 to make a mask
n.masked<- mask(nceas, n01, maskvalue=0)
#use r.masked to generate Random Points
# reclass to quantiles
quantile(nceas)
nquants <- c(0,0,0, 0.0001,3.957,1, 3.957,6.99,2,  6.99,9.13,3, 9.13,80, 4) 
rcln <- matrix(nquants,ncol=3,byrow=TRUE)
nq <- reclassify(n.masked, rcln) #reclassed to quantiles
####USE nq  when plotting random pts to ensure they are only on water

#NCEAS DATA LOG TRANSFORMED: to compensate for long tail
log.nceas<-log(nceas)
quantile(log.nceas)
# it doesn't have 0s n.log.mat <- c(-8,0,0, 0.00001,1.38,1, 1.38,1.94,2,  1.94,2.21,3, 2.21,4.36, 4)
n.log.quant <- c(-8,1.38,1, 1.38,1.94,2,  1.94,2.21,3, 2.21,4.36, 4)
rcln.log <- matrix(n.log.quant,ncol=3,byrow=TRUE)
n.log.q <- reclassify(log.nceas, rcln.log)#reclass to quantiles

#####KREFT & JETZ TERR PLANT RICHNESS RASTER##############
setwd("U:/GitHub/Biodiversity/spatialdata")

ter.rich<- raster("kj_tr.tif")
summary(ter.rich)
trd<-disaggregate(ter.rich, fact=10)
tmat<-c(0,0,NA, 1,11625,1)
na.tmat<- matrix(tmat, ncol=3, byrow=TRUE)
tr01<-reclassify(trd, na.tmat)
tr.masked<- mask(trd, tr01, maskvalue=0)
quantile(ter.rich)
trquants<-c(0,682,1, 682,1085,2, 1085,1846,3, 1846,6229,4)
trclmat<-matrix(trquants, ncol=3, byrow=TRUE)
trq<- reclassify(tr.masked, trclmat)
#####use trq    



##############GENERATE RANDOM POINTS########################################
#function to generate the points:

pts<- function(r, p ){  #r=raster, p=number of pts
  randpts<- randomPoints(r,p)              
}

#n=10 #REPeTITIONS
p=100  #NO. OF PTS.


#generate random points over two types of raster separately, to ensure that 
#you are not casting points over pixels classified as water in any of these rasters.
#with richness
#Prob will hve to do each separately insted of writing a function to loop it bc of memory issues
rr<- pts(#rasterr , #set number of pts)   #rnormalized can use the same points
rmi<-pts(#rasterr , #set number of pts)   #mi.log can use the same points

#plot them to check that they are not on NAs
plot(rq, colNA='red')
points(rr, col="black")
plot(nq, colNA='red')
points(rmi, col='black')

######PUT THEM TOGETHER###########
setwd("~/GitHub/Biodiversity/data/rr")
load(file='rr1.RData')
load(file='rr2.RData')
load(file='rr3.RData')
load(file='rr4.RData')
load(file='rr5.RData')
load(file='rr6.RData')
load(file='rr7.RData')
load(file='rr8.RData')
load(file='rr9.RData')#this loads as rrpoints
load(file='rmi9.RData')#this loads as rr9
rr10<-rrpoints #showing up as same place as rr9.....
#SO DO NEED A NEW rr10, which is being run as rr9 upstairs

rr.pts= rbind(rr,rr2,rr3,rr4,rr5,rr6,rr7,rr8, rr9, rr10)


#ALSO NEED RMI 9 and RMI10.
setwd("~/GitHub/Biodiversity/data/rmi")
load(file='rmi8.RData')#loads as rmi
load(file='rmi2.RData')
load(file='rmi3.RData')
load(file='rmi4.RData')
load(file='rmi5.RData')
load(file='rmi6.RData')
load(file='rmi7.RData')
load(file='rmi9.RData')#loads as rmi8
load(file='rmi11.RData')
#NEED THIS ONE STILL load(file="rmi12.RData)
rmi.pts= rbind(rmi,rmi2,rmi3,rmi4,rmi5,rmi6,rmi7,rmi8, rmi11) 
setwd("~/GitHub/Biodiversity")

#######for terrestrial richness measures w Vellend/K&Jraster

#n=10 #REPeTITIONS  <--- old
p=346000  #NO. OF PTS. 346*1000=346,000

#FORM: name<-pts(#raster , #set number of pts) 
#trr<- pts(trq, p)
setwd("~/GitHub/Biodiversity/data")
load(file="trr2.RData")


##################EXTRACT DATA TO POINTS#####################################
#1. Extract SpRichness to Dornelas Sample Points          
d.sr.extr<- extract(rq, Dornxy) #need o calculate how many overlap w NA

#2. Extract normalized richness to Dornelas points
d.nr.extr<- extract(rnq, Dornxy) 

#3. Extract Marine Impacts to Dornelas Sample Points
d.mi.extr<- extract(nq, Dornxy)

#4. Extract logged marine impacts to Dornelas Ppoints
d.milog.extr<- extract(n.log.q, Dornxy)

#CONVERT TO A MATRIX
drex<-matrix(d.sr.extr, ncol=430324, byrow=TRUE)
dnex<-matrix(d.nr.extr, ncol=430324, byrow=TRUE)
dmex<-matrix(d.mi.extr, ncol=430324, byrow=TRUE)

#5. Extract TerrRichness to Vellend Sample Pts
v.tr.extr<-extract(trq, Vellendxy)
vrex<-matrix(v.tr.extr, ncol=346, byrow=TRUE)

####################################################
#BATCH EXTRACTION FOR RANDOM POINTS
#function to extract the data from raster and send to matrix
extr.random<- function(m, r, row, col){ #m=x/y matrix, r=raster file, row/col=# in end matrix, 
  extr<- extract(r, m) #returns a vector
  mat2<- matrix(extr,row,col) #convert to matrix
}
########################################################################
#####random points generated above are called: 'rrdata' and 'rmidata'###########
########################################################################
#marine richness
r=1000
c=10000
r.sr.extr<- extr.random(rr.pts, rq, r, c) #set the #row/col to match specified n,p above

#normalized richness
r.nr.extr<- extr.random(rrdata, rnq, r,c)

r2=500
r=900
c=10000
#marine impacts
r.mi.extr<- extr.random(rmi.pts, nq,r,c) 

#log marine impacts
r.milog.extr<- extr.random(rmidata, n.log.q, r2,c)               

#terrestrial richness
r=1000
c=346
r.tr.extr<-extr.random(trr2, trq,r,c)

########################################################
#HELLINGER'S DISTANCE CALCULATIONS 

#THIS WILL CREATE OBJECTS OF DISTRIBUTION CLASS TO USE FOR HELLINGERS DISTANCE CALS FOR:
#Dornelas Sample points: DSR (Dorn sp richness), DNR (normalized richness) and DMI (Dorn marine impacts)
#Random Sample points: RSR, RNR, RMI
#Uniform distribution to test against:  UD

#USING tHE INPUTS pulled from above:
#1. Matrices of x/y values for Dornelas sample points, or randomly generated points
#2. Extracted data from rasters. [In matrix form for random pts.]
 
#BASIC STRUCTURE of analysis:
#a<-DiscreteDistribution(categories, probabilities) #both vectors, calc probs from actual data 
#b<- HellingerDist( e1,e2)

quants<-c(1,2,3,4)

#uniform dist to test against
UD<- DiscreteDistribution(quants, c(0.25,0.25,0.25,0.25))

#THIS IS THe GENERAL FORM:
#Need to update each step with the correct extracted data  matrix
DSR<-apply(drex, 1, function(x){         #insert name of extracted points object
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE) 
  h$counts= h$counts/sum(h$counts)
  DD<- DiscreteDistribution(quants, h$counts)
  HD<- HellingerDist(UD, DD)
})

# DNR<-apply(dnex, 1, function(x){
#   h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
#   h$counts= h$counts/sum(h$counts)
#   DD<- DiscreteDistribution(quants, h$counts)
#   HD<- HellingerDist(UD, DD)
# })

DMI<-apply(dmex, 1, function(x){
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
  h$counts= h$counts/sum(h$counts)
  DD<- DiscreteDistribution(quants, h$counts)
  HD<- HellingerDist(UD, DD)
})

VTR<-apply(vrex, 1, function(x){         #insert name of extracted points object
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE) 
  h$counts= h$counts/sum(h$counts)
  DD<- DiscreteDistribution(quants, h$counts)
  HD<- HellingerDist(UD, DD)
})


RSR<-apply(r.sr.extr, 1, function(x){
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
  prob= h$counts/sum(h$counts)
  DD<- DiscreteDistribution(quants, prob)
  HD<- HellingerDist(UD, DD)
})

# RNR<-apply(r.nr.extr, 1, function(x){
#   h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
#   h$prop= h$counts/sum(h$counts)
#   DD<- DiscreteDistribution(quants, h$prop)
#   HD<- HellingerDist(UD, DD)
# })

RMI<-apply(r.mi.extr, 1, function(x){
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
  h$prop= h$counts/sum(h$counts)
  DD<- DiscreteDistribution(quants, h$prop)
  HD<- HellingerDist(UD, DD)
})

RTR<-apply(r.tr.extr, 1, function(x){
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
  prob= h$counts/sum(h$counts)
  DD<- DiscreteDistribution(quants, prob)
  HD<- HellingerDist(UD, DD)
})
######################################################################
#HOW TO DO THIS WITH THE EXPORTED Vellend/Hansen Data:
#the counts have already been calculated. 
#create a discrete distribution object
#then run against Expected Dist
total<-apply(rand, 1, sum)
as.matrix(toal, ncol=1, byrow=TRUE)
hfc<- cbind(rand, total)

#1. pull in [rand] and [vellend] from Chi-sq file:
#both are matrix

#SET UP EXPECTED DISTRIBUTION BASED ON ACTUAL PROP OF MAP AREA IN EACH CATEGORY
ED<- DiscreteDistribution(quants, c(0.979, 0.015, 0.0047, 0.0013))

#matrix of proportions
prop<- apply(rand, 1, function(x){
  tot<-sum(x)
  pct<-x/tot
})###why is it transforming the matrix?????????
pct<-t(prop)


RFCD<-apply(pct,1,function(x){
quants<-c(1:4)
DD<-DiscreteDistribution(quants, x)
ED<- DiscreteDistribution(quants, c(0.979, 0.015, 0.0047, 0.0013))
HD<-HellingerDist(ED,DD)
})




# CUT THIS:
# 
# RHFC<- apply(rand, 1, function(x){
#   quants<-c(0,1,2,3)
#   probs<-x/sum(x)
#   DD<- DiscreteDistribution(quants, probs)
#   HD<-HellingerDist(UD,DD)
# })


VHFC<- apply(vellend, 1, function(x){
  quants<-c(1:4)
  probs<-x/sum(x)
  DD<- DiscreteDistribution(quants, probs)
  ED<- DiscreteDistribution(quants, c(0.9, 0.04, 0.04, 0.02))
  HD<-HellingerDist(ED,DD)   #USE EXPECTED DISTRIBUTION
})

###############################################
####pulling in results from GLOBE##############
###############################################
sd(RSR)
(DSR-(mean(RSR)))/(sd(RSR)) 
(DMI-(mean(RMI)))/(sd(RMI))
(VHFC-(mean(RFCD)))/(sd(RFCD))
