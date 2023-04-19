#Error bars for Input Emissions charts

#Install and upload packages
library(data.table)
library(zoo)
library(sandwich)
library(plm)
library(lmtest)
library(data.table)
library(plotly)
library(jtools) 
library(ggstance)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)
library(plotly)
library(reshape2)
library(naniar)
library(reshape)
library(wesanderson)
library(stringr)
install.packages("ggplot2")
#Read in Data
GHG <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-Ag_tbls/GHG_emis.csv")
Energy_emis <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Energy and GHG Analysis (2019)/Analysis/energy_ag_analysis/Results/Energy_emissions.csv")
lwr_N <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-Ag_tbls/lwr_N.csv")
lwr_S <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-Ag_tbls/lwr_S.csv" )
lwr_C <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-Ag_tbls/lwr_C.csv" )
lwr_W <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-Ag_tbls/lwr_W.csv" )
lwr_E <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-Ag_tbls/lwr_E.csv" )

upr_N <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-Ag_tbls/upr_N.csv" )
upr_S <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-Ag_tbls/upr_S.csv" )
upr_C <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-Ag_tbls/upr_C.csv" )
upr_W <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-Ag_tbls/upr_W.csv" )
upr_E <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-Ag_tbls/upr_E.csv" )

#Input Emissions data - prepare for plotting
#Melt data for ggplot
Energy_emis <- melt(Energy_emis)
colnames(Energy_emis) <- c("Energy_Scenario", "Region", "Value")
Energy_emis$Value <- Energy_emis$Value * Gg_to_Gt

#separate scenario from energy source into 2 columns
Energy_emis$Energy_Scenario <- str_replace(Energy_emis$Energy_Scenario, "synN_use", "synNuse") ##change "syn_use_" to synuse to make it easier to separate values into 2 cols
Energy_emis$Energy_Scenario <- str_replace(Energy_emis$Energy_Scenario, "h_alt", "alt")
Energy_emis <- Energy_emis %>% separate(Energy_Scenario, c("Energy Source", "Scenario"), "_")

#Change naming conventions to match that of error bars to making joining easier
Energy_emis$`Energy Source` <- str_replace(Energy_emis$`Energy Source`, "synNuse","Manufacturing Fertilizers")
Energy_emis$`Energy Source` <- str_replace(Energy_emis$`Energy Source`, "pest","Manufacturing Pesticides")
Energy_emis$`Energy Source` <- str_replace(Energy_emis$`Energy Source`, "m","Embodied Energy in Machinery")
Energy_emis$`Energy Source` <- str_replace(Energy_emis$`Energy Source`, "synN","Synthetic N")
Energy_emis$`Energy Source` <- str_replace(Energy_emis$`Energy Source`, "fuel","On-Farm Fuel")

Energy_emis$Scenario <- str_replace(Energy_emis$Scenario, "base","Baseline")
Energy_emis$Scenario <- str_replace(Energy_emis$Scenario, "proj","Projected")
Energy_emis$Scenario <- str_replace(Energy_emis$Scenario, "high","High")
Energy_emis$Scenario <- str_replace(Energy_emis$Scenario, "alt","High_alt")

#Split data by region
Energy_emis_E <- filter(Energy_emis, Region == "East")
Energy_emis_W <- filter(Energy_emis, Region == "West")
Energy_emis_S <- filter(Energy_emis, Region == "Southern")
Energy_emis_C <- filter(Energy_emis, Region == "Central")
Energy_emis_N <- filter(Energy_emis, Region == "North")
Energy_emis_Afr <- filter(Energy_emis, Region == "Africa")
Energy_emis_N$Region <- NULL
Energy_emis_S$Region <- NULL
Energy_emis_W$Region <- NULL
Energy_emis_E$Region <- NULL
Energy_emis_C$Region <- NULL
Energy_emis_Afr$Region <- NULL


#Input Emissions for Africa (Chart 2) Error Bars 
Energy_emis_Afr$Scenario <- factor(Energy_emis_Afr$Scenario ,levels = c("Baseline", "Projected", "High_alt"))

# Machinery emissions - add in error bars using Dan's code 
# margin <- c((Energy_emis_S$Value[Energy_emis_S$`Energy Source` == "Embodied Energy in Machinery"] - lwr_S$lwr[lwr_S$Energy.Source == "Embodied Energy in Machinery"]),
#              (Energy_emis_N$Value[Energy_emis_N$`Energy Source` == "Embodied Energy in Machinery"] -  lwr_N$lwr[lwr_N$Energy.Source == "Embodied Energy in Machinery"]),
#              (Energy_emis_C$Value[Energy_emis_C$`Energy Source` == "Embodied Energy in Machinery"] - lwr_C$lwr[lwr_C$Energy.Source == "Embodied Energy in Machinery"]),
#              (Energy_emis_W$Value[Energy_emis_W$`Energy Source` == "Embodied Energy in Machinery"] - lwr_W$lwr[lwr_W$Energy.Source == "Embodied Energy in Machinery"]),
#              (Energy_emis_E$Value[Energy_emis_E$`Energy Source` == "Embodied Energy in Machinery"] - lwr_E$lwr[lwr_E$Energy.Source == "Embodied Energy in Machinery"]))
# t <- data.frame(mid = c(Energy_emis_S$Value[Energy_emis_S$`Energy Source` == "Embodied Energy in Machinery"],Energy_emis_N$Value[Energy_emis_N$`Energy Source` == "Embodied Energy in Machinery"],
#                       Energy_emis_C$Value[Energy_emis_C$`Energy Source` == "Embodied Energy in Machinery"], Energy_emis_W$Value[Energy_emis_W$`Energy Source` == "Embodied Energy in Machinery"],
#                       Energy_emis_E$Value[Energy_emis_E$`Energy Source` == "Embodied Energy in Machinery"]), margin = margin )
# t$scenario <- c("Baseline", "Projected", "High", "Baseline", "Projected", "High", "Baseline", "Projected", "High","Baseline", "Projected", "High","Baseline", "Projected", "High")
# t$region <- c("S", "S", "S",
# 
# quadrature <- function(x){ #this is a function for calculating the square root of the summed squares of a vector
#   summed_squares <- 0 #set to 0 at first
#   for (i in 1:length(x)){ #for each observation...
#     summed_squares = summed_squares + x[i]^2 #...add the sum of the squared value to all the previous squared values
#   }
#   sqrt(summed_squares) #return the square root of the summed values
# }
# 
# tot = t %>% summarize(m = sum(mid), #sum of central values
#                       margin = quadrature(margin)) %>% #quadrature of the margin of error values
#   mutate(upper_b = m+margin, lower_b = m-margin) #properly calculated confidence interval, propagating error using quadrature
# tot

#Calculate upper and lower bounds based on margin of error
lwr_S$margin <- 
Energy_emis_Afr$upr <- sqrt((Energy_emis_S$upr^2) + (Energy_emis_N$upr^2) + (Energy_emis_C$upr^2) + (Energy_emis_W$upr^2) + (Energy_emis_E$upr^2))
Energy_emis_Afr$lwr <-  sqrt((Energy_emis_S$lwr^2) + (Energy_emis_N$lwr^2) + (Energy_emis_C$lwr)^2 + (Energy_emis_W$lwr^2) + (Energy_emis_E$lwr^2))

#multiply by Africa cropland 
Energy_emis_Afr$cropland[Energy_emis_Afr$Scenario == "Baseline"] <- area_b$harv_area[area_b$region == "Africa"]
Energy_emis_Afr$cropland[Energy_emis_Afr$Scenario == "Projected"] <- area_p$harv_area[area_p$region == "Africa"]
Energy_emis_Afr$cropland[Energy_emis_Afr$Scenario == "High_alt"] <- area_h_alt$harv_area[area_h_alt$region == "Africa"]
Energy_emis_Afr$lwr <- Energy_emis_Afr$lwr * Energy_emis_Afr$cropland 
Energy_emis_Afr$upr <- Energy_emis_Afr$upr * Energy_emis_Afr$cropland 

#Convert fertilizer & pesticides application to manufacturing emissions with EFs
Energy_emis_Afr$lwr[Energy_emis_Afr$`Energy Source` == "Manufacturing Fertilizers"] <- Energy_emis_Afr$lwr[Energy_emis_Afr$`Energy Source` == "Manufacturing Fertilizers"] * fert_ef  * kg_to_Gg
Energy_emis_Afr$lwr[Energy_emis_Afr$`Energy Source` == "Manufacturing Pesticides"] <- Energy_emis_Afr$lwr[Energy_emis_Afr$`Energy Source` == "Manufacturing Pesticides"] * pest_ef  * tons_to_Gg
Energy_emis_Afr$upr[Energy_emis_Afr$`Energy Source` == "Manufacturing Fertilizers"] <- Energy_emis_Afr$upr[Energy_emis_Afr$`Energy Source` == "Manufacturing Fertilizers"] * fert_ef  * kg_to_Gg
Energy_emis_Afr$upr[Energy_emis_Afr$`Energy Source` == "Manufacturing Pesticides"] <- Energy_emis_Afr$upr[Energy_emis_Afr$`Energy Source` == "Manufacturing Pesticides"] * pest_ef  * tons_to_Gg

#Convert all lower and upper estimates to Gigatons to match the units of the fitted values
Energy_emis_Afr$lwr <- Energy_emis_Afr$lwr * Gg_to_Gt
Energy_emis_Afr$upr <- Energy_emis_Afr$upr * Gg_to_Gt


Energy_emis_S$upr <- Energy_emis_S$upr/1000000000 
Energy_emis_N$upr <- Energy_emis_N$upr/1000000000 
Energy_emis_C$upr <- Energy_emis_C$upr/1000000000 
Energy_emis_W$upr <- Energy_emis_W$upr/1000000000 
Energy_emis_E$upr <- Energy_emis_E$upr/1000000000 
Energy_emis_S$lwr <- Energy_emis_S$lwr/1000000000 
Energy_emis_N$lwr <- Energy_emis_N$lwr/1000000000 
Energy_emis_C$lwr <- Energy_emis_C$lwr/1000000000 
Energy_emis_W$lwr <- Energy_emis_W$lwr/1000000000 
Energy_emis_E$lwr <- Energy_emis_E$lwr/1000000000 

Energy_emis_Afr$upr <- sqrt((Energy_emis_S$upr^2) + (Energy_emis_N$upr^2) + (Energy_emis_C$upr)^2 + (Energy_emis_W$upr^2) + (Energy_emis_E$upr^2))
Energy_emis_Afr$lwr <-  sqrt((Energy_emis_S$lwr^2) + (Energy_emis_N$lwr^2) + (Energy_emis_C$lwr)^2 + (Energy_emis_W$lwr^2) + (Energy_emis_E$lwr^2))


Energy_Afr_plot <- ggplot(Energy_emis_Afr, aes(fill = `Energy Source` , y = Value, x = Scenario)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle(expression(paste("Energy Input Use Emissions by Scenario for Africa"))) + xlab("Scenario") + ylab("GHG Emissions (Gt CO2eq)") + labs(fill = "Energy Inputs") + geom_errorbar(aes(ymin= lwr , ymax= upr), width=.2,
                                                                                                                                                                                                                                     position=position_dodge(.9))  +  scale_x_discrete(labels= x.axis) + theme(axis.title.x = element_text(color="black",size=12),axis.title.y = element_text(color="black", size=12) ,panel.background = element_rect(fill = NA),
                                                                                                                                                                                                                                                                                                               panel.grid.major.y = element_line(colour="grey68",linetype="dotted"),
                                                                                                                                                                                                                                                                                                               panel.grid.major.x = element_line(colour="grey68",linetype="dotted"),axis.ticks.y = element_blank(),axis.ticks.x=element_blank(),
                                                                                                                                                                                                                                                                                                               plot.title = element_text(size=13, face="bold"))
Energy_Afr_plot  <- Energy_Afr_plot  +  scale_fill_manual(values=wes_palette(n=5, name="Darjeeling1"))
ggsave(filename = "~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Graphs/New Graphs/Energy_Afr.png", plot = Energy_Afr_plot, width = , height = ) 




