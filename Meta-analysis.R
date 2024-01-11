install.packages("boot")
library(boot)
library(readxl)
library(metafor)
library(ggplot2)

dat <- read.csv("rawdata.csv",na.strings=c("",NA))

subset_ANPP <- subset(dat, Response_variable == "ANPP")
LnR_ANPP <- rma.mv(R,
                   v,
                   data = subset_ANPP,
                   method = "ML",
                   random = ~ 1 | site/obs
)
LnR_ANPP

subset_BNPP <- subset(dat, Response_variable %in% c("BNPP", "NFRP"))
LnR_BNPP <- rma.mv(R,
                   v,
                   data = subset_BNPP,
                   method = "ML",
                   random = ~ 1 | site/obs
)
LnR_BNPP

subset_Leafbiomass <- subset(dat, Response_variable %in% c("Leafbiomass"))
LnR_Leafbiomass <- rma.mv(R,
                          v,
                          data = subset_Leafbiomass,
                          method = "ML",
                          random = ~ 1 | site/obs
)
LnR_Leafbiomass

subset_Stembiomass <- subset(dat, Response_variable %in% c("Stembiomass"))
LnR_Stembiomass <- rma.mv(R,
                          v,
                          data = subset_Stembiomass,
                          method = "ML",
                          random = ~ 1 | site/obs
)
LnR_Stembiomass

subset_Rootbiomass <- subset(dat, Response_variable %in% c("Rootbiomass"))
LnR_Rootbiomass <- rma.mv(R,
                          v,
                          data = subset_Rootbiomass,
                          method = "ML",
                          random = ~ 1 | site/obs
)
LnR_Rootbiomass

subset_SOC <- subset(dat, Response_variable %in% c("SOC"))
LnR_SOC <- rma.mv(R,
                  v,
                  data = subset_SOC,
                  method = "ML",
                  random = ~ 1 | site/obs
)
LnR_SOC

subset_DOC <- subset(dat, Response_variable %in% c("DOC"))
LnR_DOC <- rma.mv(R,
                  v,
                  data = subset_DOC,
                  method = "ML",
                  random = ~ 1 | site/obs
)
LnR_DOC

subset_RS <- subset(dat, Response_variable %in% c("Rs"))
LnR_RS <- rma.mv(R,
                 v,
                 data = subset_RS,
                 method = "ML",
                 random = ~ 1 | site/obs
)
LnR_RS

subset_BNF <- subset(dat, Response_variable %in% c("BNF"))
LnR_BNF <- rma.mv(R,
                  v,
                  data = subset_BNF,
                  method = "ML",
                  random = ~ 1 | site/obs
)
LnR_BNF

# select subset of Fumigation="FACE" for regression between NPP and MAP^2
subset_NPP <- subset(NPP, Fumigation == "FACE")

regNPP_MAP <- rma.mv(RR,
                     v,
                     data = subset_NPP,
                     method = "ML",
                     mods = ~ MAP2,
                     random = ~ 1 | site/Obs
)
summary(regNPP_MAP)

regplot(regNPP_MAP,xlab=(expression(paste("MAP"^"2","(mm"^"2",")"))),ylab= "Response ratio of NPP (%)",
        psize=subset_NPP$weight,col="gray", bg=rgb(0,204,0,85, maxColorValue=255),lcol=("gray"))

# select subset of  Rs vs MAP
subset_Rs2 <- subset(dat, Response_variable == "Rs" & Fumigation %in% c("FACE"))

regRs2 <- rma.mv(RR,
                 v,
                 data = subset_Rs2,
                 method = "ML",
                 mods = ~ MAP,
                 random = ~ 1 | site/Obs
)
summary(regRs2)

regplot(regRs2,xlab=(expression(paste("MAP (mm)"))),ylab= "Response ratio of Rs (%)",
        psize=subset_Rs2$weight,col="gray", bg=rgb(139,0,0,85, maxColorValue=255),lcol=("gray"))

regBNF <- rma.mv(RR,
                 v,
                 data = BNF,
                 mods = ~ soilCN,
                 method = "ML",
                 random = ~ 1 | site/Obs
)
summary(regBNF)

regplot(regBNF, xlab = expression(paste("Soil C:N ratio")), ylab = "Response ratio of BNF (%)",
        psize = BNF$weight, col = "gray", bg = rgb(255, 204, 0, 85, maxColorValue = 255), lcol = "gray") +
  scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 50))

regNUE <- rma.mv(RR,
                 v,
                 data = NUE,
                 mods = ~ dco2,
                 method = "ML",
                 random = ~ 1 | site/Obs
)
summary(regNUE)
regplot(regNUE, xlab=(expression(paste("??CO"["2"]," (ppm)"))),ylab= "Response ratio of NUE (%)",
        psize=NUE$weight,col="gray", bg=rgb(65,105,225,85, maxColorValue=255),lcol=("gray"))

#regression N2O vs dCO2
N2O <- subset(dat, Response_variable == "N2O" )
regN2O <- rma.mv(RR,
                 v,
                 data = N2O,
                 mods = ~ dco2,
                 method = "ML",
                 random = ~ 1 | site/Obs
)
summary(regN2O)

regplot(regN2O,xlab=(expression(paste("??CO"["2"]," (ppm)"))),ylab= (expression(paste("Response ratio of N"[" 2"],"O (%)"))),
        psize=N2O$weight,col="gray", bg=rgb(148,0,211,85, maxColorValue=255),lcol=("gray"))

#regression WUE vs dCO2
WUE <- subset(dat, Response_variable == "WUE" )
regWUE <- rma.mv(RR,
                 v,
                 data = WUE,
                 mods = ~ dco2,
                 method = "ML",
                 random = ~ 1 | site/Obs
)
summary(regWUE)

regplot(regWUE,xlab=(expression(paste("??CO"["2"]," (ppm)"))),ylab= "Response ratio of WUE (%)",
        psize=WUE$weight,col="gray", bg=rgb(0,206,209,90, maxColorValue=255),lcol=("gray"))