library(RColorBrewer)
library(ggpubr)
library(ggalt)
library(mgcv)
library(scales)

library(boot)
library(readxl)
library(metafor)
library(ggplot2)

# time series of carbon and nitrogen budgets
budget$ssp = factor(budget$ssp,levels=c("SSP","SSP2","SSP2-4.5","SSP1","SSP1-2.6"))

P5 <-ggplot(budget, aes(x = year)) +
  geom_ribbon(aes(ymin = Accumulation-Accumulation_SD, ymax = Accumulation+Accumulation_SD, fill = ssp), 
              alpha = 0.2) +        
  theme_bw()+
  ylab(' N Accumulation (Tg N)')+xlab('Year')+
  theme(legend.position = "none",
        text = element_text(family = "Arial"),
        axis.title.x = element_text(size=9), axis.title.y = element_text(size=9))+
  geom_xspline(aes(y = Accumulation, color = ssp),spline_shape = 1,size=1)


p4 <- ggplot(budget, aes(x = year)) +
  geom_ribbon(aes(ymin = Csink - Csink_SD, ymax = Csink + Csink_SD, fill = ssp), alpha = 0.2) +
  theme_bw() +
  ylab('Carbon Sink (Pg C)') + xlab('Year') +
  theme(legend.position = "none",
        text = element_text(family = "Arial"),
        axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 9)) +
  geom_xspline(aes(y = Csink, color = ssp), spline_shape = 1, size = 1) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5), labels = c('0.5', '1.0', '1.5'), expand = expansion(add = c(0.24, 0.05)))


p0 <-ggplot(budget, aes(x = year)) +
  geom_ribbon(aes(ymin = area- area_SD, ymax = area+area_SD, fill = ssp), 
              alpha = 0.2) +        
  theme_bw()+
  ylab('Forest Area (Billion ha)')+xlab('Year')+
  theme(legend.position = "none",
        text = element_text(family = "Arial"),
        axis.title.x = element_text(size=9), axis.title.y = element_text(size=9))+
  geom_xspline(aes(y = area, color = ssp),spline_shape = 1,size=1)


p1 <-ggplot(budget, aes(x = year)) +
  geom_ribbon(aes(ymin = Input-Input_SD, ymax = Input+Input_SD, fill = ssp), 
              alpha = 0.2) +        
  theme_bw()+
  ylab('Nitrogen Input (Tg N)')+xlab('Year')+
  theme(legend.position = "none",
        text = element_text(family = "Arial"),
        axis.title.x = element_text(size=9), axis.title.y = element_text(size=9))+
  geom_xspline(aes(y = Input, color = ssp),spline_shape = 1,size=1)

p2 <-ggplot(budget, aes(x = year)) +
  geom_ribbon(aes(ymin = Pro-Pro_SD, ymax = Pro+Pro_SD, fill = ssp), 
              alpha = 0.2) +        
  theme_bw()+
  ylab('Forest Products (Tg N)')+xlab('Year')+
  theme(legend.position = "none",  
        text = element_text(family = "Arial"),
        axis.title.x = element_text(size=9), axis.title.y = element_text(size=9))+
  geom_xspline(aes(y = Pro, color = ssp),spline_shape = 1,size=1)+
  scale_y_continuous(breaks = c(20,30,40,50),labels = c('20','30','40','50'))

p3 <-ggplot(budget, aes(x = year)) +
  geom_ribbon(aes(ymin = Nrloss-Nrloss_SD, ymax = Nrloss+Nrloss_SD, fill = ssp), 
              alpha = 0.2) +        
  theme_bw()+
  ylab('Nr Loss (Tg N)')+xlab('Year')+
  theme(legend.position = "none",  
        text = element_text(family = "Arial"),
        axis.title.x = element_text(size=9), axis.title.y = element_text(size=9))+
  geom_xspline(aes(y = Nrloss, color = ssp),spline_shape = 1,size=1)+
  scale_y_continuous(breaks = c(10,30,50),labels = c('10','30','50'))


ggarrange(p0 + scale_colour_manual(values = c("SSP" = "gray", "SSP1" = "green1", "SSP1-2.6" = "green4", "SSP2" = "deepskyblue", "SSP2-4.5" = "royalblue")) + scale_fill_manual(values = c("SSP" = "gray", "SSP1" = "lightgreen", "SSP1-2.6" = "green", "SSP2" = "deepskyblue", "SSP2-4.5" = "royalblue")),
          p4 + scale_colour_manual(values = c("SSP" = "gray", "SSP1" = "green1", "SSP1-2.6" = "green4", "SSP2" = "deepskyblue", "SSP2-4.5" = "royalblue")) + scale_fill_manual(values = c("SSP" = "gray", "SSP1" = "lightgreen", "SSP1-2.6" = "green", "SSP2" = "deepskyblue", "SSP2-4.5" = "royalblue")),
          p1 + scale_colour_manual(values = c("SSP" = "gray", "SSP1" = "green1", "SSP1-2.6" = "green4", "SSP2" = "deepskyblue", "SSP2-4.5" = "royalblue")) + scale_fill_manual(values = c("SSP" = "gray", "SSP1" = "lightgreen", "SSP1-2.6" = "green", "SSP2" = "deepskyblue", "SSP2-4.5" = "royalblue")),
          p2 + scale_colour_manual(values = c("SSP" = "gray", "SSP1" = "green1", "SSP1-2.6" = "green4", "SSP2" = "deepskyblue", "SSP2-4.5" = "royalblue")) + scale_fill_manual(values = c("SSP" = "gray", "SSP1" = "lightgreen", "SSP1-2.6" = "green", "SSP2" = "deepskyblue", "SSP2-4.5" = "royalblue")),
          P5 + scale_colour_manual(values = c("SSP" = "gray", "SSP1" = "green1", "SSP1-2.6" = "green4", "SSP2" = "deepskyblue", "SSP2-4.5" = "royalblue")) + scale_fill_manual(values = c("SSP" = "gray", "SSP1" = "lightgreen", "SSP1-2.6" = "green", "SSP2" = "deepskyblue", "SSP2-4.5" = "royalblue")),
          p3 + scale_colour_manual(values = c("SSP" = "gray", "SSP1" = "green1", "SSP1-2.6" = "green4", "SSP2" = "deepskyblue", "SSP2-4.5" = "royalblue")) + scale_fill_manual(values = c("SSP" = "gray", "SSP1" = "lightgreen", "SSP1-2.6" = "green", "SSP2" = "deepskyblue", "SSP2-4.5" = "royalblue")),
          labels = letters[1:6],
          ncol = 3, nrow = 2,
          common.legend = FALSE, 
          font.label = list(size = 10))  


#biomass
bio$Climatezone2 = factor(bio$Climatezone2,levels=c("Arid","Tropical","Temperate","Cold","Polar"))
bio$Response_variable = factor(bio$Response_variable,levels=c("Rootbiomass","Leafbiomass","Stembiomass"))

pbio_cli <- ggplot(bio, aes(y = RR, x = Climatezone2, fill = Response_variable)) +
  geom_boxplot(position = position_dodge(width = 0.8), color = "gray10", width = 0.7, size = 0.6, alpha=0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Climate zone", y = "Response ratio of plant biomass (%)") +
  scale_fill_manual(values = c("sandybrown", "lightgreen", "forestgreen"),
                    guide = guide_legend(title = ""),
  )  
print(pbio_cli)


bio$Fumigation = factor(bio$Fumigation,levels=c("FACE","OTC","GC"))

pbio_f <- ggplot(bio, aes(y = RR, x = Fumigation, fill = Response_variable)) +
  geom_boxplot(position = position_dodge(width = 0.8), color = "gray10", width = 0.7, size = 0.6, alpha=0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Fumigation methods", y = "Response ratio of plant biomass (%)") +
  scale_fill_manual(values = c("sandybrown", "lightgreen", "forestgreen"),
                    guide = guide_legend(title = ""),
  )  
print(pbio_f)


# biomass vs duration, by groups
ggplot(bio, aes(x = Duration, y = RR, size = weight, color = Response_variable, fill = Response_variable)) +
  geom_point(shape = 21, alpha = 0.3) +
  labs(x = expression(paste("Duration (month)")), y = "Response ratio of plant biomass (%)") +
  facet_wrap(~Response_variable, scales = "free_y", ncol = 1, strip.position = "top") +
  scale_fill_manual(values = c("sandybrown", "lightgreen", "forestgreen"))+
  theme(legend.position = "none")

# biomass vs vegetation age, by groups

bio <- subset(dat, Response_variable %in% c("Rootbiomass","Leafbiomass","Stembiomass"))
bio$age = factor(bio$age,levels=c("1","2","3"))

pbio_age <- ggplot(bio, aes(y = RR, x = age, fill = Response_variable)) +
  geom_boxplot(position = position_dodge(width = 0.8), color = "gray10", width = 0.7, size = 0.6, alpha=0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Vegetation age", y = "Response ratio of plant biomass (%)") +
  scale_fill_manual(values = c("sandybrown", "lightgreen", "forestgreen"),
                    guide = guide_legend(title = ""),
  )  
print(pbio_age)

# regression between NPP and Duration
regNPP_dur <- rma.mv(RR,
                     v,
                     data = NPP,
                     method = "ML",
                     mods = ~ Duration,
                     random = ~ 1 | site/Obs
)
summary(regNPP_dur)

regplot(regNPP_dur,xlab=(expression(paste("Duration (month)"))),ylab= "Response ratio of NPP (%)",
        psize=NPP$weight,col="gray", bg=rgb(0,204,0,85, maxColorValue=255),lcol=("transparent"))

# NPP vs duration
ggplot(NPP, aes(x = Duration, y = RR, size = weight)) +
  geom_point(bg=rgb(0,204,0,85, maxColorValue=255), shape = 21, alpha = 0.3) +
  labs(x = expression(paste("Duration (month)")), y = "Response ratio of NPP (%)") +
  theme_minimal()+
  theme(legend.position = "none")


# boxplot by forest age
page <- ggplot(NPP, aes(y = RR,group = age)) +
  geom_boxplot(position = position_dodge(width = 3),fill = "lightblue", color = "darkblue",width=1.5,size=0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Vegetation age", y = "Response ratio of NPP (%)")
print(page)


# boxplot by duration
pdur <- ggplot(NPP, aes(y = RR,group = dur)) +
  geom_boxplot(position = position_dodge(width = 3),fill = "mediumpurple4", color = "purple",width=1.5,size=0.6,alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Experimental duration", y = "Response ratio of NPP (%)") 
print(pdur)


# boxplot by study
pstu <- ggplot(NPP, aes(y = RR,group = study)) +
  geom_boxplot(position = position_dodge(width = 3),fill = "khaki", color = "orange",width=2,size=0.6,alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Study site", y = "Response ratio of NPP (%)") +
  scale_y_continuous(labels = c("-20", "0", "20","40","60"), breaks = c(-20, 0, 20, 40,60))+
  scale_x_discrete(labels = c("Duke FACE","Oak Ridge FACE","EUROFACE","Aspen FACE","EucFACE","China OTC"), breaks = c(1,2,3,4,5,6))
print(pstu)

pstu <- ggplot(NPP, aes(y = RR,group = study)) +
  geom_boxplot(position = position_dodge(width = 3),fill = "khaki", color = "orange",width=1.5,size=0.6,alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Study site", y = "Response ratio of NPP (%)")
print(pstu)

#box NPP by climate zone
NPP <- subset(dat, Response_variable == "NPP" )
NPP$Climatezone2 = factor(NPP$Climatezone2,levels=c("Temperate","Cold"))
NPP_c <- ggplot(NPP, aes(x = Climatezone2, y = RR)) +
  geom_boxplot(position = position_dodge(width = 0.8),fill = "green3", color = "black", width = 0.46, size = 0.6, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Climate zone", y = "Response ratio of NPP (%)") 
print(NPP_c)

#box NUE by climate zone
NUE <- subset(dat, Response_variable == "NUE" )
NUE$Climatezone2 = factor(NUE$Climatezone2,levels=c("Tropical","Temperate","Cold"))
NUE_c <- ggplot(NUE, aes(x = Climatezone2, y = RR)) +
  geom_boxplot(position = position_dodge(width = 0.8),fill = "royalblue", color = "black", width = 0.46, size = 0.6, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Climate zone", y = "Response ratio of NUE (%)") 
print(NUE_c)

pBNF_sym <- ggplot(BNF, aes(y = RR,group = category)) +
  geom_boxplot(position = position_dodge(width = 3),fill = "khaki", color = "orange",width=1.5,size=0.6,alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "N-fixing species", y = "Response ratio of BNF (%)")
print(pBNF_sym)

BNF$Climatezone2 = factor(BNF$Climatezone2, levels = c("Tropical", "Temperate", "Cold"))
BNF$category = factor(BNF$category, levels = c("symbiotic", "non-symbiotic"))

pBNF_c <- ggplot(BNF, aes(x = Climatezone2, y = RR, group = interaction(Climatezone2, category), fill = category)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.46, size = 0.6, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Climate zone", y = "Response ratio of BNF (%)") +
  scale_fill_manual(values = c("khaki", "orange"), 
                    guide = guide_legend(title = ""),
                    labels = c("Symbiotic", "Asymbiotic")
  )

print(pBNF_c)

# BNF by fumigation
BNF <- subset(dat, Response_variable %in% c("BNF"))
BNF$Fumigation = factor(BNF$Fumigation,levels=c("FACE","OTC","GC"))

pBNF_f <- ggplot(BNF, aes(x = Fumigation, y = RR)) +
  geom_boxplot(position = position_dodge2(width = 0.8),fill = "khaki", color = "gray18", width = 0.6, size = 0.6, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Fumigation method", y = "Response ratio of BNF (%)")

print(pBNF_f)


#[N] by climate zone
subset_N <- subset(dat, Response_variable %in% c("Leaf[N]","Stem[N]","Root[N]", "TIN"))
subset_N$Climatezone2 = factor(subset_N$Climatezone2,levels=c("Tropical","Temperate","Cold","Polar"))
subset_N$Response_variable = factor(subset_N$Response_variable,levels=c("Leaf[N]","Stem[N]","Root[N]", "TIN"))

pN_c <- ggplot(subset_N, aes(x = Climatezone2, y = RR, group = interaction(Climatezone2, Response_variable), fill = Response_variable)) +
  geom_boxplot(position = position_dodge2(width = 0.8), width = 0.7, size = 0.6, alpha = 0.8, aes(color = Response_variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Climate zone", y = "Response ratio of nitrogen content (%)") +
  scale_fill_manual(values = c("sandybrown", "lightgreen", "forestgreen","brown"), 
                    guide = guide_legend(title = ""),
                    labels = c("Leaf[N]","Stem[N]","Root[N]", "TIN")
  ) +
  scale_color_manual(values = c("salmon4", "steelblue", "darkgreen", "black"), 
                     guide = guide_legend(title = "", override.aes = list(fill = c("sandybrown", "lightgreen", "forestgreen","brown")))
  ) +  
  guides(fill = guide_legend(override.aes = list(color = c("salmon4", "steelblue", "darkgreen", "black"))))  # ????????????

print(pN_c)


#[N] by climate zone
subset_N <- subset(dat, Response_variable %in% c("Leaf[N]","Stem[N]","Root[N]"))
subset_N$Climatezone2 = factor(subset_N$Climatezone2,levels=c("Tropical","Temperate","Cold","Polar"))
subset_N$Response_variable = factor(subset_N$Response_variable,levels=c("Leaf[N]","Stem[N]","Root[N]"))

pN_c <- ggplot(subset_N, aes(x = Climatezone2, y = RR, group = interaction(Climatezone2, Response_variable), fill = Response_variable)) +
  geom_boxplot(position = position_dodge2(width = 0.8), width = 0.7, size = 0.6, alpha = 0.8, aes(color = Response_variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Climate zone", y = "Response ratio of nitrogen content (%)") +
  scale_fill_manual(values = c("sandybrown", "lightgreen", "forestgreen"), 
                    guide = guide_legend(title = ""),
                    labels = c("Leaf[N]","Stem[N]","Root[N]", "TIN")
  ) +
  scale_color_manual(values = c("salmon4", "steelblue", "gray50"), 
                     guide = guide_legend(title = "", override.aes = list(fill = c("sandybrown", "lightgreen", "forestgreen")))
  ) +  
  guides(fill = guide_legend(override.aes = list(color = c("salmon4", "steelblue", "gray50"))))  # ????????????

print(pN_c)



