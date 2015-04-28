# To DO:
#   1. mask out land area from the analysis


#packages
library(rgdal)
library(dismo)
#library(rJava)
library(maptools)

#work working directory:
setwd("~/GitHub/Biodiversity")
#home working directory:
setwd("~/Biodiversity/Biodiversity")

#load land shapefile
land<- readShapePoly('land.shp')
#Read point files
Dornpts<-readShapePoints("Dornelas_terrt.shp")

# load rasters 
rnorm <- raster("marineAllNorm.tif") #normalized for each taxon then averaged for "mean diversity"
r<- raster("marineBiodivAll.tif") #all taxa

#examine the distribution of pixel values
h <- hist(r, breaks = 5, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h)
q<- quantile(r) 

# reclassify to categories or actual values?
#m <- c(0,4,1,4,9,2,9,18,3,18,64,4)
mat    <- c(0,0,NA, 0.01,64,1, 64,104,2,  104,310,3, 310,3410, 4) #0 to NA for use in mask later
rclmat <- matrix(mat,ncol=3,byrow=TRUE)
rq <- reclassify(r, rclmat)



# kfold sampling for training and testing
fold <- kfold(Dornpts, k=3) #best practices for #?
ptstest <- Dornpts[fold == 1, ] #one subsample for validation
ptstrain <- Dornpts[fold != 1, ] #the rest of the subsamples for model training
points(ptstest, col="blue")

# Option 2: do a balanced random sample instead
AtoNA <- c(0, NA, 1, 1)
PtoNA <- c(0, 1, 1, NA)
Pmat <- matrix(AtoNA,ncol=2,byrow=TRUE)
Amat <- matrix(PtoNA,ncol=2,byrow=TRUE)
pmask <- reclassify(r,Pmat)
amask <- reclassify(r,Amat)
prespts <- randomPoints(pmask, 300)  
abspts <- randomPoints(amask, 300)
plot(r)
points(prespts, col="blue")
points(abspts, col="red")
pts <- rbind(prespts,abspts)
############## MY EDIT OF ABOVE:  NOT WORKING, STILL 0 not NA in rmask/rand pts on land.
rmask<- reclassify(r, rclmat)
randoPts<- randomPoints(rmask, 300)
plot(r)
points(Dornpts, col='blue')
points(randoPts, col='red')





#extract dependent and predictor values from rasters for training and testing
dornextr <- cbind(Dornpts,extract(r, Dornpts))
#asign column name to div data
colnames(dornextr)[3] <- "sp.div"
#convert to dataframe
dornextr <- data.frame(dornextr)

randoextr <- cbind(randoPts,extract(r, randoPts))
colnames(randoextr)[3] <- "sp.div"
randoextr<- data.frame(randoextr)

#GLM
glm <- glm(oakb~dem+sandp+coarse+slope+topowet, family=binomial,data=patrain)
response(glm)
summary(glm)
anova(glm)

#make within- and outside-sample prediction for GLM
lYhattrain <- predict(glm, newdata=patrain) #calculate probabilities training data
lYhattest <- predict(glm, newdata=patest) #calculate probabilities testing data

#make prediction raster from GLM
sdf <- data.frame(getValues(s))
predict <- predict(glm, newdata=sdf)
p = raster(nrows=nrow(s), ncols=ncol(s), ext=extent(s), vals=predict)
plot(p)

#GAM
gam <- gam(oakb~s(sandp)+s(dem)+s(topowet)+s(slope)+coarse, family=binomial, data=patrain)
summary(gam)
plot(gam, se=TRUE)
anova(gam)      #test for non-linearity in effects

#make within- and outside-sample predictions with GAM
Yhattest <- predict(gam, newdata=patest)       #predict odds
lYhattest <- exp(Yhattest) / (1 + exp(Yhattest))  #logit transformation

Yhattrain <- predict(gam, newdata=patrain)       #predict odds
lYhattrain <- exp(Yhattrain) / (1 + exp(Yhattrain))  #logit transformation

#neural net
ann <- nnet(oakb~sandp+dem+topowet+slope+coarse, patrain, patrain$oakb, 10, rang=0.1)
print(ann)
summary(ann)

#make within- and outside-sample predictions with ANN
lYhattrain <- predict(ann, patrain, type="raw") #calculate probabilities training data
lYhattest <- predict(ann, patest) #calculate probabilities testing data

# MaxEnt - Create presence-only data
pa <- cbind(pts,extract(r, pts))
colnames(pa)[3] <- "oakb"
pa <- subset(pa, pa[,3]==1)
occ <- pa
occ[,3] <- NULL
plot(r)
points(occ)
sme <- dropLayer(s, 2)
sme <- dropLayer(sme, 3)

#MaxEnt model
me <- maxent(sme, occ)
plot(me)
response(me)
show(me)

# choose a threshold for dichotomizing according to predicted probability
thresh <- 0.5
YhatFac <- cut(lYhattrain, breaks=c(-Inf, thresh, Inf), labels=c("lo", "hi"))
# contingency table and marginal sums
cTab <- table(patrain$oakb, YhatFac)
addmargins(cTab)
# percentage correct for training data
sum(diag(cTab)) / sum(cTab)

#ROC
ROC <- roc(patest$oakb, lYhattest)
plot.roc(ROC)

#make prediction raster from GAM
sdf <- data.frame(getValues(s))
predict <- predict(gam, newdata=sdf)
p = raster(nrows=nrow(s), ncols=ncol(s), ext=extent(s), vals=predict)
lp = exp(p) / (1 + exp(p))
plot(lp)

#make prediction raster from MaxEnt
p <- predict(me, s)
plot(p)

#ROC for maxent
paxy <- cbind(pa$x, pa$y)
pa <- cbind(pa, extract(p, paxy))
colnames(pa)[4] <- "pred"
pa <- data.frame(pa[complete.cases(pa),])

ROC <- roc(pa$oakb, pa$pred)
plot.roc(ROC)

#MCE DATA SET
#read layers from MCE lab
setwd("h:/projects/nre534/mce/4R")
s <- stack("annarbor", "clayp", "elevation", "lu95", "public", "roads", "sandp", "stream", "veg_1850")
plot(s)

#calculate distances
zerotoNA <- c(0, NA, 1, 1)
rclmat <- matrix(zerotoNA,ncol=2,byrow=TRUE)

s$rddist <- distance(reclassify(s$roads,rclmat))
s$strdist <- distance(reclassify(s$stream, rclmat))
s$aadist <- distance(reclassify(s$annarbor, rclmat))
s$slope <- terrain(s$elevation, unit='degrees')

plot(s) 

lu2dev <- c(110,139,1,140,625,0) # reclass dev,  minus trans, extraction, cemetaries
lumat <- matrix(lu2dev,ncol=3,byrow=TRUE)

d <- reclassify(s$lu95, lumat)


#sample
d2 <- aggregate(d, 20)
pts <- as.data.frame(d2, xy=TRUE)
pts$layer <- NULL
plot(d)
points(pts, col='red')

