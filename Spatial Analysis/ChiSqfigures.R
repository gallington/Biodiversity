#Code to generate Fig. 2

library(ggplot2)
library(grid)
setwd("~/GitHub/Biodiversity/output plots")

#CairoPNG(width=2200, height=1600, file='Rplot.png', dpi=200)
msr<-ggplot(msr.dat) +
  geom_bar(mapping=aes(x=label, y=unif.prop), fill='grey',color="black", 
           stat='identity', width=1, lwd=0.5) +
  geom_point(mapping=aes(x=label, y=(srobs.prop)),
             color='white', size=14) +
  geom_point(mapping=aes(x=label, y=(srobs.prop),
                         color=r.value),
             size=12) +
  scale_color_gradient2(limits=c(-1, 1)) +
  #scale_fill_grey(start=0.2, end=0.2,labels=c('1     ', '2     ',
  #'3     ', '4'), guide=FALSE) +
  scale_fill_grey(start=0.6, end=0.6,labels=c('0-85', '85-113',  '113-368', '368-3410'), guide=FALSE) +
  scale_y_continuous(limits=c(0, 1)) +
  ylab('Proportion of Cases') +
  xlab('Sp. Richness') +
  labs(title='Marine Species Richness', size=16) +
  theme(axis.text.y=element_text(size=18, vjust=-0.35))+
  guides(color=guide_legend(title='Representativeness:')) +
  theme_bw() +
  theme(text=element_text(size=16),
        legend.position='bottom',
        legend.margin=unit(0, 'cm'),
        legend.key=element_rect(color='white'),
        legend.key.size=unit(1, 'cm'),
        plot.title=element_text(vjust=2),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_line(color='black'),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(color='black', size=0.5))
msr
ggsave(msr, file="msr-chisq.png", width=8, height=6, dpi=200)
#dev.off()

#normalized marine species richness:
# nsr<-ggplot(nsr.dat) +
#   geom_bar(mapping=aes(x=label, y=unifsites), color="black", fill="16", 
#            stat='identity', width=1, lwd=0.5) +
#   geom_point(mapping=aes(x=label, y=((r.value * unifsites) + unifsites)),
#              color='white', size=14) +
#   geom_point(mapping=aes(x=label, y=((r.value * unifsites) + unifsites),
#                          color=r.value),
#              size=12) +
#   scale_color_gradient2(limits=c(-1, 1)) +
#   #scale_fill_grey(start=0.2, end=0.2,labels=c('1     ', '2     ',
#   #'3     ', '4'), guide=FALSE) +
#   scale_fill_grey(start=0.6, end=0.6,labels=c('<0.14', '0.14-0.26',  '0.26-0.33', '0.33-0.78'), guide=FALSE) +
#   scale_y_continuous(limits=c(0, with(msr.dat, 2*unifsites[1]))) +
#   ylab('Number of Cases') +
#   xlab('') +
#   #ggtitle("Marine Species Richness",size=16)
#   labs(title='Normalized Marine Species Richness', size=16) +
#   theme(axis.text.y=element_text(size=18, vjust=-0.35))+
#   guides(color=guide_legend(title='Representativeness:')) +
#   theme_bw() +
#   theme(text=element_text(size=16),
#         legend.position='bottom',
#         legend.margin=unit(0, 'cm'),
#         legend.key=element_rect(color='white'),
#         legend.key.size=unit(1, 'cm'),
#         plot.title=element_text(vjust=2),
#         panel.border=element_blank(),
#         axis.ticks=element_blank(),
#         #axis.line=element_line(color='black'),
#         panel.grid.major.x=element_blank(),
#         panel.grid.major.y=element_line(color='black', size=0.5))
# nsr
# ggsave(nsr, file="nsr-chisq.png", width=8, height=6, dpi=200)





#marine impacts:
mi<-ggplot(mi.dat) +
  geom_bar(mapping=aes(x=label, y=unif.prop),fill='grey', color='black', 
           stat='identity', width=1, lwd=0.5) +
  geom_point(mapping=aes(x=label, y=(miobs.prop)),
             color='white', size=14) +
  geom_point(mapping=aes(x=label, y=(miobs.prop),
                         color=r.value),
             size=12) +
  scale_color_gradient2(limits=c(-1, 1)) +
  #scale_fill_grey(start=0.2, end=0.2,labels=c('1     ', '2     ',
  #'3     ', '4'), guide=FALSE) +
  scale_fill_grey(start=0.6, end=0.6,labels=c('<4', '4-7','7-9','9-80'), guide=FALSE) +
  scale_y_continuous(limits=c(0, 1)) +
  ylab('Proportion of Cases') +
  xlab('No. Human Impacts') +
  labs(title='Marine Impacts', size=16) +
  theme(axis.text.y=element_text(size=18, vjust=-0.35))+
  guides(color=guide_legend(title='Representativeness:')) +
  theme_bw() +
  theme(text=element_text(size=16),
        legend.position='bottom',
        #legend.margin=unit(0, 'cm'),
        legend.key=element_rect(color='white'),
        #legend.key.size=unit(1, 'cm'),
        plot.title=element_text(vjust=2),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_line(color='black'),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(color='black', size=0.5))
mi
ggsave(mi, file="mi-chisq.png", width=8, height=6, dpi=200)

#terrestrial richness:
tr<-ggplot(tr.dat) +
  geom_bar(mapping=aes(x=label, y=unif.prop), fill='grey', color='black', 
           stat='identity', width=1, lwd=0.5) +
  geom_point(mapping=aes(x=label, y=(trobs.prop)),
             color='white', size=14) +
  geom_point(mapping=aes(x=label, y=(trobs.prop),
                         color=r.value),
             size=12) +
  scale_color_gradient2(limits=c(-1, 1)) +
  #scale_fill_grey(start=0.2, end=0.2,labels=c('1     ', '2     ',
  #'3     ', '4'), guide=FALSE) +
  scale_fill_grey(start=0.6, end=0.6,labels=c('1','2','3','4'), guide=FALSE) +
  scale_y_continuous(limits=c(0, 1)) +
  ylab('Proportion of Cases') +
  xlab('Quartiles of species richness') +
  labs(title='Terrestrial Richness of Red List Species', size=16) +
  theme(axis.text.y=element_text(size=18, vjust=-0.35))+
  guides(color=guide_legend(title='Representativeness:')) +
  theme_bw() +
  theme(text=element_text(size=16),
        legend.position='bottom',
        #legend.margin=unit(0, 'cm'),
        legend.key=element_rect(color='white'),
        #legend.key.size=unit(1, 'cm'),
        plot.title=element_text(vjust=2),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_line(color='black'),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(color='black', size=0.5))
tr
ggsave(tr, file="tr-chisq.png", width=8, height=6, dpi=200)

#forest cover change
fcc<-ggplot(dat) +
  geom_bar(mapping=aes(x=label, y=expected), fill='grey', color='black',
           stat='identity', width=1, lwd=0.5) +
  # You need to put the observed counts here for y; where `vellend` is
  geom_point(mapping=aes(x=label, y=vellend), color='white', size=14) +
  # You need to put the observed counts here for y; where `vellend` is
  geom_point(mapping=aes(x=label, y=vellend, color=r.value), size=12) +
  scale_color_gradient2(limits=c(-1, 1)) +
  scale_fill_grey(labels=c('No Change     ', 'Loss     ',
                           'Gain     ', 'Loss & Gain'), guide=FALSE) +
  scale_y_log10(breaks=c(1, 10, 1e2, 1e3, 1e4, 1e5)) +
  ylab('Number of Observations (Pixels)') +
  xlab('') +
  labs(title='Forest Cover Change', size=16) +
  theme(axis.text.y=element_text(size=18, vjust=-0.35))+
  guides(color=guide_legend(title='Representedness:')) +
  theme_bw() +
  theme(text=element_text(size=16),
        legend.position='bottom',
        #legend.margin=unit(0, 'cm'),
        legend.key=element_rect(color='white'),
        #legend.key.size=unit(1, 'cm'),
        plot.title=element_text(vjust=2),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_text(vjust=1.2),
        axis.line=element_line(color='black'),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(color='black', size=0.5))
fcc

ggsave(fcc, file="fcc-chisq.png", width=8, height=6, dpi=200)

###terrestrial PLANT species richness#############
#terrestrial richness:
tpr<-ggplot(tpr.dat) +
  geom_bar(mapping=aes(x=label, y=unif.prop), fill='grey', color='black', 
           stat='identity', width=1, lwd=0.5) +
  geom_point(mapping=aes(x=label, y=(trobs.prop)),
             color='white', size=14) +
  geom_point(mapping=aes(x=label, y=(trobs.prop),
                         color=r.value),
             size=12) +
  scale_color_gradient2(limits=c(-1, 1)) +
  #scale_fill_grey(start=0.2, end=0.2,labels=c('1     ', '2     ',
  #'3     ', '4'), guide=FALSE) +
  scale_fill_grey(start=0.6, end=0.6,labels=c('1','2','3','4'), guide=FALSE) +
  scale_y_continuous(limits=c(0, 1)) +
  ylab('Proportion of Cases') +
  xlab('Quartiles of species richness') +
  labs(title='Species Richness of Terrestrial Plant Species', size=16) +
  theme(axis.text.y=element_text(size=18, vjust=-0.35))+
  guides(color=guide_legend(title='Representativeness:')) +
  theme_bw() +
  theme(text=element_text(size=16),
        legend.position='bottom',
        #legend.margin=unit(0, 'cm'),
        legend.key=element_rect(color='white'),
        #legend.key.size=unit(1, 'cm'),
        plot.title=element_text(vjust=2),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_line(color='black'),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(color='black', size=0.5))
tpr
ggsave(tpr, file="trPLANT-chisq.png", width=8, height=6, dpi=200)