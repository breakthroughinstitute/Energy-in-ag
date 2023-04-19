#Graph 2 with error bars
#read in Energy_emis_Afr file
#read in upper and lower error bars (Calculated in Google Sheets)
Afr_err_bars <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-ag_tbls/Africa_emis_err-bars.csv")
colnames(Afr_err_bars) <- NULL
colnames(Afr_err_bars) <- c("Source", "Scenario", "Value (m)", "Summed squares(MoE)", "sqrt(MoE)", "upr", "lwr")
Afr_err_bars <- Afr_err_bars[c(2:16),]
lwr_Afr <- as.numeric(as.matrix(unlist(Afr_err_bars$lwr)))
upr_Afr <- as.numeric(as.matrix(unlist(Afr_err_bars$upr)))
x.axis <- c("Baseline", "Projected", "High")
Energy_Afr_plot <- ggplot(Energy_emis_Afr, aes(fill = `Energy Source` , y = Value, x = Scenario)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle(expression(paste("Energy Input Use Emissions by Scenario for Africa"))) + xlab("Scenario") + ylab("GHG Emissions (Gt CO2eq)") + labs(fill = "Energy Inputs") + geom_errorbar(aes(ymin= lwr_Afr , ymax= upr_Afr), width=.2,
                                                                                                                                                                                                                                     position=position_dodge(.9))  +  scale_x_discrete(labels= x.axis) + theme(axis.title.x = element_text(color="black",size=12),axis.title.y = element_text(color="black", size=12) ,panel.background = element_rect(fill = NA),
                                                                                                                                                                                                                                                                                                               panel.grid.major.y = element_line(colour="grey68",linetype="dotted"),
                                                                                                                                                                                                                                                                                                               panel.grid.major.x = element_line(colour="grey68",linetype="dotted"),axis.ticks.y = element_blank(),axis.ticks.x=element_blank(),
                                                                                                                                                                                                                                                                                                               plot.title = element_text(size=13, face="bold"))          
Energy_Afr_plot 
Energy_Afr_plot  <- Energy_Afr_plot  +  scale_fill_manual(values=wes_palette(n=5, name="Darjeeling1"))
Energy_Afr_plot 
ggsave(filename = "~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Graphs/New Graphs/Energy_Afr.png", plot = Energy_Afr_plot, width = , height = ) 

#Graph 3
#read in Energy_emis_all
#read in upper and lower error bars for input emissios by region and scenario
input_err_bars <- read.csv("~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Energy-ag_tbls/input_emis_errbar.csv")
input_err_bars <-input_emis_errbar
lwr_region <-  as.numeric(as.matrix(unlist(input_err_bars$lower)))
upr_region <-  as.numeric(as.matrix(unlist(input_err_bars$upper)))
Energy_emis_all
Energy_emis_all_reg <- ggplot(Energy_emis_all, aes(fill = Region , y = `sum(Value)`, x = Scenario)) + 
  geom_bar(position="dodge", stat="identity" ) + ggtitle(expression(paste("Energy Input Use Emissions by Region"))) + xlab("Scenario") + ylab("GHG Emissions (GtCO2eq)") + labs(fill = "Region") + geom_errorbar(aes(ymin= lwr_region , ymax= upr_region), width=.2,
                                                                                                                                                                                                                position=position_dodge(.9)) + theme_bti()

Energy_emis_all_reg <-Energy_emis_all_reg +  scale_fill_manual(values=bti_colors)
ggsave(filename = "~/Google Drive File Stream/My Drive/Food & Farming/Energy in ag/Results_2020/Graphs/New Graphs/Energy_all_reg.png", plot = Energy_emis_all_reg, width = , height = )
Energy_emis_all_reg
