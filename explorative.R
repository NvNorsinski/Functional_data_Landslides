rm(list = ls(all = TRUE))
library(ggplot2)
library(reshape)
library(cowplot)
library(ggpubr)
# read data
response = readRDS(file = "Daten/Paldau/Samples/response.rds")

slope = readRDS(file = "Daten/Paldau/Samples/slope.rds")
dgm = readRDS(file = "Daten/Paldau/Samples/dgm.rds")
aspect_ns = readRDS(file = "Daten/Paldau/Samples/aspect_ns.rds")
aspect_ow = readRDS(file = "Daten/Paldau/Samples/aspect_ow.rds")

genCurvature = readRDS(file = "Daten/Paldau/Samples/genCurvature.rds")
catchmant_area = readRDS(file = "Daten/Paldau/Samples/catchmantArea.rds")

twi = readRDS(file = "Daten/Paldau/Samples/twi.rds")

adju = 0.5

plot(density(slope[1,], adjust = adju), ylim = c(1,4), xlab = "Slope [degree]")
lines (density(slope[10,], adjust = adju))
lines (density(slope[20,], adjust = adju))


dat = as.data.frame(slope[c(1,10,20),])
dat = t(dat)
long = reshape::melt(dat)
long$X2 = as.factor(long$X2)

as_ns = as.data.frame(aspect_ns[c(1,10,20),])
as_ns = t(as_ns)
as_ns = reshape::melt(as_ns)
as_ns$X2 = as.factor(as_ns$X2)

as_ow = as.data.frame(aspect_ow[c(1,10,20),])
as_ow = t(as_ow)
as_ow = reshape::melt(as_ow)
as_ow$X2 = as.factor(as_ow$X2)

genCurv = as.data.frame(genCurvature[c(1,10,20),])
genCurv = t(genCurv)
genCurv = reshape::melt(genCurv)
genCurv$X2 = as.factor(genCurv$X2)

catch = as.data.frame(catchmant_area[c(1,10,20),])
catch = t(catch)
catch = reshape::melt(catch)
catch$X2 = as.factor(catch$X2)

tw = as.data.frame(twi[c(1,10,20),])
tw = t(tw)
tw = reshape::melt(tw)
tw$X2 = as.factor(tw$X2)





p1 = ggplot(long, aes(x = value, colour = X2)) +
  geom_density(adjust = 1/2, show_guide=FALSE, size = 0.7)+
  stat_density(geom="line", position="identity", adjust = 1/2)+
  labs(color = "radius of moving window [m]: ")+
  xlim(0, 1)+
  ylim(0, 4)+
  xlab("Slope [Degree]") +
 # labs(title="Slope")+
  #theme(legend.position="bottom")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  scale_color_manual(values=c("blue", "green", "red"))+
  geom_hline(yintercept=0, colour="grey", size=0.01)+
  theme(axis.text=element_text(size=12),
       axis.title=element_text(size=14))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))

p1

p2 = ggplot(as_ns, aes(x = value, colour = X2)) +
  geom_density(adjust = 1/2, show_guide=FALSE, size = 0.7)+
  stat_density(geom="line", position="identity", adjust = 1/2)+
  labs(color = "radius moving window [m]")+
  xlim(-1, 1)+
  ylim(0, 4)+
  xlab("Aspect NS [radiant]") +
  #labs(title="Slope")+
  #theme(legend.position="bottom")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  scale_color_manual(values=c("blue", "green", "red"))+
  geom_hline(yintercept=0, colour="grey", size=0.01)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))


p2



p3 = ggplot(as_ow, aes(x = value, colour = X2)) +
  geom_density(adjust = 1/2, show_guide=FALSE, size = 0.7)+
  stat_density(geom="line", position="identity", adjust = 1/2)+
  labs(color = "radius moving window [m]")+
  xlim(-1, 1)+
  ylim(0, 4)+
  xlab("Aspect EW [radiant]") +
  #labs(title="Slope")+
  #theme(legend.position="bottom")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  scale_color_manual(values=c("blue", "green", "red"))+
  geom_hline(yintercept=0, colour="grey", size=0.01)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))


p3

p4 = ggplot(genCurv, aes(x = value, colour = X2)) +
  geom_density(adjust = 1/2, show_guide=FALSE, size = 0.7)+
  stat_density(geom="line", position="identity", adjust = 1/2)+
  labs(color = "radius moving window [m]")+
  xlim(-1, 1)+
  ylim(0, 4)+
  xlab("General Curvature [radiant]") +
  #labs(title="Slope")+
  #theme(legend.position="bottom")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  scale_color_manual(values=c("blue", "green", "red"))+
  geom_hline(yintercept=0, colour="grey", size=0.01)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))


p4


p5 = ggplot(catch, aes(x = value, colour = X2)) +
  geom_density(adjust = 1/2, show_guide=FALSE, size = 0.7)+
  stat_density(geom="line", position="identity", adjust = 1/2)+
  labs(color = "radius moving window [m]")+
  xlim(-0.1, 5.4)+
  ylim(0, 2)+
  xlab("Catchmant Area") +
  #labs(title="Catchmant Area")+
  #theme(legend.position="bottom")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  scale_color_manual(values=c("blue", "green", "red"))+
  geom_hline(yintercept=0, colour="grey", size=0.01)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))


p5


p6 = ggplot(tw, aes(x = value, colour = X2)) +
  geom_density(adjust = 1/2, show_guide=FALSE, size = 0.7)+
  stat_density(geom="line", position="identity", adjust = 1/2)+
  labs(color = "radius moving window [m]")+
  xlim(1, 18)+
  ylim(0, 1)+
  xlab("Topographic Wetness Index") +
  #labs(title="Catchmant Area")+
  #theme(legend.position="bottom")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.5))+
  theme_minimal()+
  theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values=c("blue", "green", "red"))+
  geom_hline(yintercept=0, colour="grey", size=0.01)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))


p6


#plot_grid(p1, p2, p3, p4, p5, p6, labels = "AUTO")

img = ggarrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3, common.legend = TRUE, legend="bottom")

img

ggsave("density2.png", plot = img, device = "png", dpi = 300, height =25, units = "cm")

