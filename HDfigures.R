#Code to generate Figure 1.
#
#outputs of hellinger distances to plot disributions:

#random marine sp rich
rdf<- as.data.frame(RSR)  
names(rdf)[1]<-paste("hd")

#random marine impacts
ndf<-as.data.frame(RMI)  
names(ndf)[1]<-paste("hd")

#random forest cover change
rhfc<-as.data.frame(RFCD) 
names(rhfc)[1]<-paste("hd")

#random [terrestrial] RedList species richness
setwd("C:/Users/gallingt/Google Drive/Biodiversity/GLOBEoutputs/vellend only")
rrr<- read.csv(file="sampleHD.csv")
RRR<-as.matrix(rrr)

#random terrestrial plant species richness
rtr<-as.data.frame(RTR)
names(rtr)[1]<-paste("hd")

rhfc$sample<-c('rhfc')
rtr$sample<-c('rtr')
ndf$sample<-c('ndf')
rdf$sample<-c("rdf")
rrr$sample<-c('rrr')

histogram<- rbind(rdf, ndf,rhfc,rtr) #rrr#GLOBEVelend and/or GLOBE
histogram<- rbind(rdf, ndf,rhfc,rtr)


#Dornelas & Vellend Points
# sprrv<- 0.17   #terrestrial Red List sp rich just Vellend Pts
# sprtvd<-####   #terrestrial Red List sp rich Vellend + Dornelas Pts
y<-c(0,0,0,0)
x<-c(DSR,DMI, VHFC, VTR)
u<- c(mean(RSR), mean(RMI), mean(RFCD), mean(RTR))
sd<-c(sd(RSR), sd(RMI), sd(RFCD),sd(RTR))
collection<-c('Marine SpRichness ', 'Marine Impacts ', 'Forest Cover Change ', 'Terrestrial SpRichness ')
#collectionsds<-c('Marine:SpRichness', 'Marine:Impacts', 'Forest Cover Change', 'Terrestrial:SpRichness')

CPoints<- data.frame(collection,x,y)
uPoints<- data.frame(collection, u,y)




palette<- c('#018571','#80cdc1','#dfc27d','#a6611a')   #color: dkgr, ltgr, ltbrwn, dkbrwn
hexpalette<- c('#cccccc', '#969696', '#525252', "#252525")  #bw

setwd("~/GitHub/Biodiversity/output plots")
#####BLACK & WHITE##################################HD<- 

HD.bw<- 
  ggplot(histogram, aes(x=hd, y=..density.., color=sample))+
  geom_histogram(data=rdf, fill= "#cccccc", alpha=0.5,binwidth=0.02)+   #marine richness-random pts
  geom_histogram(data=ndf, fill="#969696", alpha=0.75, binwidth=0.02)+     #marine impacts-random pts
  geom_histogram(data=rhfc, fill="#525252", alpha=0.75, binwidth=0.02)+     #forest cover change- random pts
  geom_histogram(data=rtr, fill= "#252525", alpha=0.5,binwidth=0.02)+  #terrestrial richness
  xlim(0,0.5)+
  theme_bw()+
  scale_shape_manual(values=c(21,21,21,21))+
  scale_color_manual(values=hexpalette)+
  geom_point(data=Points, aes(x=x, y=y),color='black', size=4 , show_guide = FALSE )+
  geom_point(data=Points, aes(x=x, y=y, color=collection), size=3)+
  theme(axis.text.x=element_text(size=12,vjust=-0.35))+
  theme(axis.text.y=element_text(size=12, vjust=-0.35))+
  theme(axis.text=element_text(size=8),axis.title=element_text(size=12))+
  theme(legend.text=element_text(size=8))+
  
  labs(color="Collection")+
  labs(x="hellinger distance", y="percent")
HD.bw
#
###########COLOR#########################   
HD2<- ggplot(histogram, aes(x=hd, y=..density..))+
  geom_histogram(data=rdf, , fill= "#dfc27d", alpha=0.5,binwidth=0.02)+   #marine richness-random pts
  geom_histogram(data=ndf, fill="#80cdc1", alpha=0.75, binwidth=0.02)+     #marine impacts-random pts
  geom_histogram(data=rhfc, fill="#018571", alpha=0.75, binwidth=0.02)+     #forest cover change- random pts
  geom_histogram(data=rtr, fill= "#a6611a", alpha=0.75,binwidth=0.02)+  #terrestrial richness
  xlim(0,0.5)+
  theme_bw()+
  scale_color_manual(values=palette)+
  geom_point(data=uPoints, aes(x=u, y=y),color='black', size=4.5 ,shape=16, show_guide = FALSE)+
  #geom_point(data=uPoints, aes(x=u, y=y,color=collection), size=4 , show_guide = TRUE)+
  scale_shape_manual(values=c(18,18,18,18),labels=c('Marine:SpRichness (36.4)', 
                                                    'Marine:Impacts(43.6)', 
                                                    'Forest Cover Change (12.38)', 
                                                    'Terrestrial:SpRichness (Cmb50.838)'))+
  geom_point(data=CPoints, aes(x=x, y=y),color='dark grey', size=4.5 , show_guide = FALSE)+
  geom_point(data=CPoints, aes(x=x, y=y, color=collection),size=4)+
  theme(axis.text.x=element_text(size=12,vjust=-0.35))+
  theme(axis.text.y=element_text(size=12, vjust=-0.35))+
  theme(axis.text=element_text(size=8),axis.title=element_text(size=12))+
  theme(legend.text=element_text(size=8), legend.title=element_text(color='black',size=8, face='bold'))+
  labs(color="Collection")+
  labs(x="hellinger distance", y="percent")
HD2




###############################################
##########export figs##########################
################################################
setwd("~/GitHub/Biodiversity/output plots/Fig.2")

ggsave(HD.bw, file="HD.bw-Fig.jpg", height=4, width=6, dpi=200)

ggsave(HD2 , file="HD-Fig-wPlantRichness2.jpg", height=4, width=6, dpi=200)




##################################################################
####### make a table of results #########################
#################################################

hdmeans<- c(mean(RSR), mean(RMI), mean(RHFC))
hdsd<- c(sd(RSR), sd(RMI), sd(RHFC))
hdnum<- c()
hdcol<- c("random sp richness", "random marine impacts", "random forest change")
hdtable<- cbind(hdcol, hdmeans)

#####################################
########################################
HD3<- ggplot(histogram, aes(x=hd, y=..density..))+
  geom_histogram(data=rdf, , fill= "#dfc27d", alpha=0.5,binwidth=0.02)+   #marine richness-random pts
  geom_histogram(data=ndf, fill="#80cdc1", alpha=0.75, binwidth=0.02)+     #marine impacts-random pts
  geom_histogram(data=rhfc, fill="#018571", alpha=0.75, binwidth=0.02)+     #forest cover change- random pts
  geom_histogram(data=rtr, fill= "#a6611a", alpha=0.75,binwidth=0.02)+  #terrestrial richness
  xlim(0,0.5)+
  theme_bw()+
  scale_color_manual(values=palette)+
  geom_point(data=uPoints, aes(x=u, y=y),color='black', size=4.5 ,shape=16, show_guide = FALSE)+
  geom_point(data=uPoints, aes(x=u, y=y,color=collection), size=4 , show_guide = TRUE)+
  scale_shape_manual(values=c(18,18,18,18),labels=c('Marine:SpRichness (36.4)', 
                                                    'Marine:Impacts(43.6)', 
                                                    'Forest Cover Change (12.38)', 
                                                    'Terrestrial:SpRichness (Cmb50.838)'))+
  geom_point(data=CPoints, aes(x=x, y=y),color='dark grey', size=4.5 , show_guide = FALSE)+
  geom_point(data=CPoints, aes(x=x, y=y, color=collection),size=4)+
  theme(axis.text.x=element_text(size=12,vjust=-0.35))+
  theme(axis.text.y=element_text(size=12, vjust=-0.35))+
  theme(axis.text=element_text(size=8),axis.title=element_text(size=12))+
  theme(legend.text=element_text(size=8), legend.title=element_text(color='black',size=8, face='bold'))+
  labs(color="Collection")+
  labs(x="hellinger distance", y="percent")
HD3
ggsave(HD3 , file="HD-Fig-colormean.jpg", height=4, width=6, dpi=200)
