#Analyses needed to Create Fig. 2: Chi-Sq calcs

#need:  extracted data files 
setwd("~/GitHub/Biodiversity")

#Counts of total # per bin for all 1000 randomized extractions:
#(creates a distribution of counts per class to get expected)
###########MARINE SPECIES RICHNESS#################
###################################################
b<-apply(r.sr.extr, 1, function(x){              
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
  c<-h$counts
  })
b<-t(b)
colnames(b) <- c(1,2,3,4)  #CHANGE THESE BELOW TO LABEL AS THE ACTUALY RANGE OF CATEGORIES

##########NORMALIZED RICHNESS###############
################################################
# n<-apply(r.nr.extr, 1, function(x){              
#   h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
#   c<-h$counts
# })
# n<-t(n)
# colnames(b) <- c(1,2,3,4) 


############MARINE IMPACTS#######################
################################################
c<-apply(r.mi.extr, 1, function(x){            
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
  c<-h$counts
})
c<-t(c)
colnames(c) <- c(1,2,3,4)

######## Terr [PLANT] Sp Richness #############
###########################################
p<- apply(r.tr.extr,1 ,function(x){            
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
  c<-h$counts
})
p<-t(p)
colnames(p) <- c(1,2,3,4)
########Normalized Terr [PLANT] Sp Richness #############
###########################################
np<- apply(r.ltr.extr,1 ,function(x){            
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
  c<-h$counts
})
np<-t(np)
colnames(np) <- c(1,2,3,4)




#############TERRESTRIAL [RED LISTED]SPECIES RICHNESS######### [import GLOBE results...]
######################################################
setwd("~/GitHub/Biodiversity/GLOBE/vellend")  #random terrestrial points
tr1<-(read.csv(file="sample-0.csv", header=FALSE,stringsAsFactors=FALSE))
tr2<-(read.csv(file="sample-1.csv", header=FALSE,stringsAsFactors=FALSE))
tr3<-(read.csv(file="sample-2.csv", header=FALSE,stringsAsFactors=FALSE))
tr4<-(read.csv(file="sample-3.csv", header=FALSE,stringsAsFactors=FALSE))
t<-rbind(tr1[,1],tr2[,1],tr3[,1],tr4[,1])
comb<-matrix(t, ncol=346, byrow=TRUE)
#42.63295  69.73010 129.36024 364.13199
comb[comb<= 43]<- 1
comb[comb>43&comb<=70]<- 2
comb[comb> 70&comb<=130]<- 3
comb[comb> 130&comb<400]<- 4

h<- hist(comb[1,], breaks=c(0,1,2,3,4))
h2<- hist(comb[2,], breaks=c(0,1,2,3,4))
h3<- hist(comb[3,], breaks=c(0,1,2,3,4))
h4<- hist(comb[4,], breaks=c(0,1,2,3,4))
h<-h$counts
h2<-h2$counts
h3<-h3$counts
h4<-h4$counts
h
tr<-rbind(h,h2,h3,h4)

colnames(tr) <- c(1,2,3,4)
vsum<- apply(v,1,sum)  

###########VELLEND COLLECTION FOR TERRESTRIAL [REDLIST] SPECIES RICHNESS#########
#######################################################################
v<-(read.csv(file="vellend.csv", header=FALSE,stringsAsFactors=FALSE))
vrex<-matrix(v[,1], ncol=346, byrow=TRUE)
vrex[vrex<= 43]<- 1
vrex[vrex>43&vrex<=70]<- 2
vrex[vrex> 70&vrex<=130]<- 3
vrex[vrex> 130&vrex<400]<- 4
vh<- hist(vrex[1,], breaks=c(0,1,2,3,4))
vct<-vh$counts
vct<-matrix(vct, ncol=4, byrow=TRUE)
colnames(vct) <- c(1,2,3,4)

#################VELLEND COLLECTION FOR TERRESTRIAL [PLANT] SPECIES RICHNESS###########
#############################################################################
vp<-apply(vrex, 1, function(x){                   #Vellend terrestrial 
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
  c<-h$counts
})

vp<-t(vp)
colnames(vp) <- c(1,2,3,4)

#################VELLEND COLLECTION FOR normalized TERRESTRIAL [PLANT] SPECIES RICHNESS###########
#############################################################################
vnp<-apply(vlrex, 1, function(x){                   #Vellend terrestrial 
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
  c<-h$counts
})

vnp<-t(vnp)
colnames(vnp) <- c(1,2,3,4)



#############DORNELAS COLLECTION FOR MARINE SPECIES RICHNESS##########
#####################################################################
d<-apply(drex, 1, function(x){                  #Dornelas marine collection data
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
  c<-h$counts
})

d<-t(d)
colnames(d) <- c(1,2,3,4)

#############DORNELAS COLLECTION FOR ___normalized__  MARINE SPECIES RICHNESS##########
#####################################################################
# dn<-apply(dnex, 1, function(x){                  #Dornelas marine collection data
#   h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
#   c<-h$counts
# })
# 
# dn<-t(dn)
# colnames(dn) <- c(1,2,3,4)



############DORNELAS COLLECTION FOR MARINE IMPACTS###################
####################################################################
dm<-apply(dmex, 1, function(x){                  #Dornelas marine collection data
  h<- hist(x, breaks=c(0,1,2,3,4), plot=FALSE)
  c<-h$counts
})

dm<-t(dm)
colnames(dm) <- c(1,2,3,4)

###############################################################
CLASS.NAMES.str = c('one', 'two', 'three', 'four')
CLASS.NAMES = c('1', '2', '3', '4')
CLASS.NAMES.msr =c('0-85', '85-113',  '113-368', '368-3410')


##########accounting for diff in sample size#####
#(x/10000)=(val for cat/totalDornpts) 
#total Dorn pts= 424193
###########
#adjusted observed totals to 10k RANDOM observations:
#Dornelas MARINE SP RICHNESS ADJUSTED
rstan<- c(1211, 700,3660,4428) 
drstan<- matrix(rstan, ncol=4, byrow=TRUE)
colnames(drstan) <- c(1,2,3,4)

#Dornelas mARINE IMPACTS ADJUSTED
mstan<- c(977,1735,2117,5170)
dmstan<- matrix(mstan, ncol=4, byrow=TRUE)
colnames(dmstan) <- c(1,2,3,4)

#CAN USE SOURCE VALS FOR TERREST RICHNESS BC OBS AND RANDOM BOTH =346 OBS.
######################################################
# Analysis
######################################################
#
#EXPECTED VALUES:
srexpected <- apply(b, 2, function (x) { round(mean(x), 0) })
nrexpected<- apply(n, 2, function (x)  {round(mean(x), 0)})
miexpected <- apply(c, 2, function (x) { round(mean(x), 0) })
tsrexpected <-apply(tr, 2, function (x) { round(mean(x), 0) })
tprexpected<-apply(p, 2, function (x) {round(mean(x),0)})
tlprexpected<-apply(np, 2, function (x) {round(mean(x), 0) })
# Create the contingency tables for the GLOBE "representedness" statistic

#first for species richness:
tabrr <- cbind(t(drstan), srexpected)   #sprichness
colnames(tabrr)<- c('observed', 'expected')
r.c.tab <- matrix(ncol=2, nrow=0)
for (i in 1:dim(tabrr)[1]) {
  r.c.tab <- rbind(r.c.tab, matrix(c(tabrr[i,], apply(tabrr[-i,], 2, sum)),
                               nrow=2, ncol=2, byrow=TRUE))
}
rownames(r.c.tab) <- c('1', '-1', '2', '-2',
                     '3', '-3', '4', '-4')

#norm species richness:
# tabrn <- cbind(t(dstan), nrexpected)   #nsprichness
# colnames(tabrn)<- c('observed', 'expected')
# nr.c.tab <- matrix(ncol=2, nrow=0)
# for (i in 1:dim(tabrn)[1]) {
#   nr.c.tab <- rbind(nr.c.tab, matrix(c(tabrn[i,], apply(tabrn[-i,], 2, sum)),
#                                    nrow=2, ncol=2, byrow=TRUE))
# }
# rownames(nr.c.tab) <- c('1', '-1', '2', '-2',
#                        '3', '-3', '4', '-4')

####################################################################
#then for marine impacts:
tabrmi <- cbind(t(dmstan), miexpected)   #marine impacts
colnames(tabrmi)<- c('observed', 'expected')
mi.c.tab <- matrix(ncol=2, nrow=0)
for (i in 1:dim(tabrmi)[1]) {
  mi.c.tab <- rbind(mi.c.tab, matrix(c(tabrmi[i,], apply(tabrmi[-i,], 2, sum)),
                                     nrow=2, ncol=2, byrow=TRUE))
}
rownames(mi.c.tab) <- c('1', '-1', '2', '-2',
                        '3', '-3', '4', '-4')

####################################################################
#terr plant richness: obs=vp, exp=tprexpected
tabpr <- cbind(t(vp), tprexpected)   #sprichness
colnames(tabpr)<- c('observed', 'expected')
tpr.c.tab <- matrix(ncol=2, nrow=0)
for (i in 1:dim(tabpr)[1]) {
  tpr.c.tab <- rbind(tpr.c.tab, matrix(c(tabpr[i,], apply(tabpr[-i,], 2, sum)),
                                   nrow=2, ncol=2, byrow=TRUE))
}
rownames(tpr.c.tab) <- c('1', '-1', '2', '-2',
                       '3', '-3', '4', '-4')

#normalized terr plant richness:  obs=vnp, exp= tlprexpected;
tabnpr<- cbind(t(vnp), tlprexpected)
colnames(tabnpr)<- c('observed', 'expected')
tnpr.c.tab<-matrix(ncol=2, nrow=0)
for (i in 1:dim(tabnpr)[1]){
  tnpr.c.tab<- rbind(tnpr.c.tab, matrix(c(tabnpr[i,], apply(tabnpr[-i,],2, sum)),
                                        nrow=2, ncol=2, byrow=TRUE))
}
rownames(tnpr.c.tab)<-c('1', '-1', '2', '-2',
                        '3', '-3', '4', '-4')


#########################################################################
###Vellend terr sp. richness: REDLIST
#observed= tr, expected = tsrexpected
tabtr <- cbind(t(vp), tprexpected)   #sprichness
colnames(tabtr)<- c('observed', 'expected')
tr.c.tab <- matrix(ncol=2, nrow=0)
for (i in 1:dim(tabtr)[1]) {
  tr.c.tab <- rbind(tr.c.tab, matrix(c(tabtr[i,], apply(tabtr[-i,], 2, sum)),
                                   nrow=2, ncol=2, byrow=TRUE))
}
rownames(tr.c.tab) <- c('1', '-1', '2', '-2',
                       '3', '-3', '4', '-4')


#####################################################################
# Get p-values for each category 
####sp rich:##########
rp.values <- data.frame()
for (i in seq.int(1, dim(r.c.tab)[1], by=2)) {
  rp.values <- rbind(rp.values, chisq.test(r.c.tab[c(i, i+1),])$p.value)
}
colnames(rp.values) <- c('p.value')
rownames(rp.values) <- CLASS.NAMES
rp.values$reject <- rp.values$p.value < 0.05
#####norm rich:############
# np.values <- data.frame()
# for (i in seq.int(1, dim(nr.c.tab)[1], by=2)) {
#   np.values <- rbind(np.values, chisq.test(nr.c.tab[c(i, i+1),])$p.value)
# }
# colnames(np.values) <- c('p.value')
# rownames(np.values) <- CLASS.NAMES
# np.values$reject <- np.values$p.value < 0.05

#######################################################################
# Get p-values for 
#####marine impacts#######
mip.values <- data.frame()
for (i in seq.int(1, dim(mi.c.tab)[1], by=2)) {
  mip.values <- rbind(mip.values, chisq.test(mi.c.tab[c(i, i+1),])$p.value)
}
colnames(mip.values) <- c('p.value')
rownames(mip.values) <- CLASS.NAMES
mip.values$reject <- mip.values$p.value < 0.05

####ter Plant rich:####
tprp.values<- data.frame()
for (i in seq.int(1, dim(tpr.c.tab)[1], by=2)) {
  tprp.values <- rbind(tprp.values, chisq.test(tpr.c.tab[c(i, i+1),])$p.value)
}
colnames(tprp.values) <- c('p.value')
rownames(tprp.values) <- CLASS.NAMES
tprp.values$reject <- tprp.values$p.value < 0.05
######normalized terr Plant rich: 
tnprp.values<- data.frame()
for (i in seq.int(1, dim(tnpr.c.tab)[1], by=2)) {
  tnprp.values <- rbind(tnprp.values, chisq.test(tnpr.c.tab[c(i, i+1),])$p.value)
}
colnames(tnprp.values) <- c('p.value')
rownames(tnprp.values) <- CLASS.NAMES
tnprp.values$reject <- tnprp.values$p.value < 0.05

#########################################################################
# P-values for terrestrial richness [RED LIST]
#########################################################################
trp.values <- data.frame()
for (i in seq.int(1, dim(tr.c.tab)[1], by=2)) {
  trp.values <- rbind(trp.values, chisq.test(tr.c.tab[c(i, i+1),])$p.value)
}
colnames(trp.values) <- c('p.value')
rownames(trp.values) <- CLASS.NAMES
trp.values$reject <- trp.values$p.value < 0.05



#######################################################################
#########call the function:
# Assumes expected frequency is in tab[1,2] and observed is in tab[1,1]
#
repr <- function (tab) {
  test <- chisq.test(tab)
  if (0 == tab[1,2] & 0 == tab[1,1]) {
    return(0)
  } else {
    if (tab[1,2] < tab[1,1]) {
      return(1 - test$p.value)
    } else {
      if (tab[1,2] >= tab[1,1]) {
        return(-(1 - test$p.value))
      } else {
        if (tab[1,2] == 0 & tab[1,1] != 0) {
          return(NA)
        }
      }
    }
  }
}


#####################################################
###Representativeness statistic##########################
###########general form:####################################
r.values <- data.frame()
for (i in seq.int(1, dim(c.tab)[1], by=2)) {
  r.values <- rbind(r.values, repr(c.tab[c(i, i+1),]))
}
colnames(r.values) <- c('r.value')
rownames(r.values) <- CLASS.NAMES

########################################################
#############marine sp richness: #########################
msr.r.values <- data.frame()
for (i in seq.int(1, dim(r.c.tab)[1], by=2)) {
  msr.r.values <- rbind(msr.r.values, repr(r.c.tab[c(i, i+1),]))
}
colnames(msr.r.values) <- c('r.value')
rownames(msr.r.values) <- CLASS.NAMES
########################################################
#############___norm______marine sp richness: #########################
nsr.r.values <- data.frame()
for (i in seq.int(1, dim(nr.c.tab)[1], by=2)) {
  nsr.r.values <- rbind(nsr.r.values, repr(nr.c.tab[c(i, i+1),]))
}
colnames(nsr.r.values) <- c('r.value')
rownames(nsr.r.values) <- CLASS.NAMES
#######################################################
###########marine impacts:###############################
mi.r.values <- data.frame()
for (i in seq.int(1, dim(mi.c.tab)[1], by=2)) {
  mi.r.values <- rbind(mi.r.values, repr(mi.c.tab[c(i, i+1),]))
}
colnames(mi.r.values) <- c('r.value')
rownames(mi.r.values) <- CLASS.NAMES

#####################################################
################terr plant richness#################
tpr.r.values <- data.frame()
for (i in seq.int(1, dim(tpr.c.tab)[1], by=2)) {
  tpr.r.values <- rbind(tpr.r.values, repr(tpr.c.tab[c(i, i+1),]))
}
colnames(tpr.r.values) <- c('r.value')
rownames(tpr.r.values) <- CLASS.NAMES

##################
########normalized plant richness: ##########
tnpr.r.values <- data.frame()
for (i in seq.int(1, dim(tnpr.c.tab)[1], by=2)) {
  tnpr.r.values <- rbind(tnpr.r.values, repr(tnpr.c.tab[c(i, i+1),]))
}
colnames(tnpr.r.values) <- c('r.value')
rownames(tnpr.r.values) <- CLASS.NAMES


########################################################
#########terrestrial species richness: REDLIST###################
tr.r.values <- data.frame()
for (i in seq.int(1, dim(tr.c.tab)[1], by=2)) {
  tr.r.values <- rbind(tr.r.values, repr(tr.c.tab[c(i, i+1),]))
}
colnames(tr.r.values) <- c('r.value')
rownames(tr.r.values) <- CLASS.NAMES

################
# Visualization 
#Step 1: create the final dataframes.  
#Step 2: see ChiSqfigures.R

# There are four (4) land cover classes
unif.dist <- dunif(seq.int(1, 4), min=0, max=4)

# Expected land/marine area in each class from expected proportions and total land
# area (148,326,000 km^2) from:
# http://www.nationsonline.org/oneworld/earth.htm
total.land.area <- 148326000
land.areas <- apply(p / apply(p, 1, sum), 2, mean) * total.land.area

cum.land.area <- land.areas
for (i in seq.int(2, length(land.areas))) {
  cum.land.area[i] <- sum(land.areas[1:i])
}
#FOR MARINE SP RICHNESS: TOTAL AREA = 426,475,185 lm^2 : larger than just total ocean/water 
#bc it includes cells that are partially covered by land, partially water.
total.mar.area <- 426475185
marine.area <- apply(b / apply(b, 1, sum), 2, mean) * total.mar.area
mi.area<-apply(c/apply(c, 1, sum), 2, mean)*total.mar.area
cum.marine.area <- marine.area
for (i in seq.int(2, length(marine.area))) {
  cum.marine.area[i] <- sum(marine.area[1:i])
}
cum.mi.area<-mi.area
for (i in seq.int(2, length(mi.area))){
  cum.mi.area[i]<- sum(mi.area[1:i])
}
#marine sp richness:
msr.dat <- data.frame(label=factor(row.names(tabrr), ordered=TRUE,
                               levels=row.names(tabrr),
                               labels=c(">85", "85-113", "113-368", "368-3410")), #c('1', '2', '3', '4')),
                  cum.marine.area.sq.km=cum.marine.area,
                  marine.area.sq.km=marine.area,
                  unifsites=floor(unif.dist * 430324), # Number of study sites? 443000
                  r.value=msr.r.values)
srobs.prop<-c(0.121, 0.070, 0.366, 0.443)
unif.prop<- c(0.25, 0.25,0.25,0.25)
msr.dat<-cbind(msr.dat, tabrr, srobs.prop, unif.prop)

#norm richness:
# nsr.dat<-data.frame(label=factor(row.names(tabrn), ordered=TRUE,
#                                  levels=row.names(tabrn),
#                                  labels=c('<0.14', '0.14-0.26',  '0.26-0.33', '0.33-0.78')), #c('1', '2', '3', '4')),
#                     cum.marine.area.sq.km=cum.marine.area,
#                     marine.area.sq.km=marine.area,
#                     unifsites=floor(unif.dist * 430324), # Number of study sites? 443000
#                     r.value=nsr.r.values)
# 
# 


#marine impacts:
imarine.area <- apply(c / apply(c, 1, sum), 2, mean) * total.mar.area

cum.imarine.area <- imarine.area
for (i in seq.int(2, length(imarine.area))) {
  cum.imarine.area[i] <- sum(imarine.area[1:i])
}

mi.dat<-data.frame(label=factor(row.names(tabrmi), ordered=TRUE,
                                levels=row.names(tabrmi),
                                labels=c('<4', '4-7','7-9','9-80')),
                   cum.marine.area.sq.km=cum.imarine.area,
                   imarine.area.sq.km=imarine.area,
                   unifsites=floor(unif.dist * 430324), # Number of study sites? 443000
                   r.value=mi.r.values)
miobs.prop<-c(0.098, 0.174, 0.212,0.517)
unif.prop<- c(0.25, 0.25,0.25,0.25)
mi.dat<-cbind(mi.dat, tabrmi, miobs.prop, unif.prop)


#terrestrial species richness: (replicating GLOBE fig)
total.land.area <- 148326000
land.areas <- apply(tr / apply(tr, 1, sum), 2, mean) * total.land.area

cum.land.area <- land.areas
for (i in seq.int(2, length(land.areas))) {
  cum.land.area[i] <- sum(land.areas[1:i])
}

tr.dat <- data.frame(label=factor(row.names(tabtr), ordered=TRUE,
                               levels=row.names(tabtr),
                               labels=c("1", "2", "3", "4")),
                  cum.land.area.sq.km=cum.land.area,
                  land.area.sq.km=land.areas,
                  uniform=floor(unif.dist * 346), # Number of study sites (or other?)
                  r.value=tr.r.values)
trobs.prop<- c(0.28,0.21,0.43,0.08)
tr.dat<-cbind(tr.dat, tabtr, trobs.prop, unif.prop)

####Terrestrial PLANT species richness
total.land.area <- 148326000
land.areas <- apply(p / apply(p, 1, sum), 2, mean) * total.land.area

cum.land.area <- land.areas
for (i in seq.int(2, length(land.areas))) {
  cum.land.area[i] <- sum(land.areas[1:i])
}
tpr.dat <- data.frame(label=factor(row.names(tabpr), ordered=TRUE,
                                  levels=row.names(tabpr),
                                  #labels=c("1", "2", "3", "4")),
                                  labels=c("225-682", "683-1085", "1086-1846", "1847-6229")),
                     cum.land.area.sq.km=cum.land.area,
                     land.area.sq.km=land.areas,
                     uniform=floor(unif.dist * 346), # Number of study sites (or other?)
                     r.value=tpr.r.values)
tprobs.prop<- c(0.09,0.09,0.39,0.28)
unif.prop<- c(0.25, 0.25,0.25,0.25)

tpr.dat<-cbind(tpr.dat, tabpr, tprobs.prop, unif.prop)

#normalized:
total.land.area <- 148326000
land.areas <- apply(np / apply(np, 1, sum), 2, mean) * total.land.area

cum.land.area <- land.areas
for (i in seq.int(2, length(land.areas))) {
  cum.land.area[i] <- sum(land.areas[1:i])
}

tnpr.dat <- data.frame(label=factor(row.names(tabnpr), ordered=TRUE,
                                   levels=row.names(tabnpr),
                                   labels=c("1", "2", "3", "4")),
                      cum.land.area.sq.km=cum.land.area,
                      land.area.sq.km=land.areas,
                      uniform=floor(unif.dist * 346), # Number of study sites (or other?)
                      r.value=tpr.r.values)
tnprobs.prop<- c(0.18, 0.23,0.27,0.32)
tnpr.dat<-cbind(tnpr.dat, tabnpr, tnprobs.prop, unif.prop)