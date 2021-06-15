install.packages("Rmisc")
                 

library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(haven)
library(ggplot2)
library(Rmisc)

#TODO 20190906
# remove outliers
# add error bars
# turn into line or bar plots (non-EVT vs. EVT version ;)

## USER-DEFINED FUNCTIONS ##

# label me! What do I do?
calc_residual_activity <- function(enz.data, col.start, level.pressure, count.enz){
  for(n.enz in seq(1, count.enz)){
    colnum.0psi <- col.start + ((n.enz - 1) * levels.pressure)
    for(level.pressure in rev(seq(1, levels.pressure))){
      colnum.pressure <- col.start + level.pressure - 1 + ((n.enz - 1) * levels.pressure)
      colname.0psi <- colnames(enz.data)[colnum.0psi]
      colname.pressure <- colnames(enz.data)[colnum.pressure]
      #colname.residact <- paste("resid", colname.pressure, sep='_')
      colname.residact <- colname.pressure
      print(c(colnum.pressure, colnum.0psi, colname.residact)) #TEST
      # this should replace the raw data column with residual rate
      enz.data <- enz.data %>% mutate(!!colname.residact := .[[colnum.pressure]] / .[[colnum.0psi]])
    }
  }
  return(enz.data)
}

## MAIN ANALYSIS ##

data_file <- "C:/Users/Tiffa/Documents/DeepC/DeepC R Data/Master_CteneR_spellchecked.csv"

#load Master Ctene Data
enz.data <- read.csv(data_file)

#calculate residual rates
col.start <- 6
levels.pressure <- 5 #including 1 atm and recovery
count.enz <- (ncol(enz.data) + 1 - col.start) / levels.pressure
enz.data.resid <- calc_residual_activity(enz.data, col.start, level.pressure, count.enz)

#put dataframe in long form
enz.data.resid.long <- enz.data.resid %>% gather(key = "enz.pressure", value = "rate", 
  -"Sample ID", -"Species", -"Mass (g)", -"Volume (m)", -"Coversion") %>% 
mutate(enzyme= substr(enz.pressure, 1, str_locate(enz.pressure, "[0-9]")-1))%>% 
mutate(pressure= substr(enz.pressure, str_locate(enz.pressure, "[0-9]"),
                        str_length(enz.pressure))) %>% 
select(-enz.pressure)

## PLOTS ##

# make sure x-axis plots in the right order
enz.data.resid.long$pressure <- factor(enz.data.resid.long$pressure, levels = c("0psi", "3000psi", "6000psi", "9000psi", "0Recovery"), ordered = TRUE)

# residual activity by enzyme
enz.data.resid.long %>% ggplot(aes(x = pressure, y = rate, color = Species)) + 
  facet_grid(~enzyme) +
  geom_line()

# residual activity by species
enz.data.resid.long %>% ggplot(aes(x = pressure, y = rate, color = Species)) + 
  facet_grid(~Species) +
  geom_line()

# residual activity by enzyme x species
enz.data.resid.long %>% ggplot(aes(x = pressure, y = rate, color = Species)) + 
  facet_grid(~enzyme*Species) + 
  geom_line()

tgc <- summarySE(enz.data.resid.long, measurevar="rate", groupvars=c("Species"))

## Bar plot for individual Species
enz.data.resid.long %>% 
  filter(Species %in% c( "Tjalfiella pink")) %>%
  ggplot(aes(x = pressure, y = rate, c = Species, fill = Species)) +
  scale_x_discrete(labels = c('1','200','400','600','(recovery)'))+
  geom_bar(stat="identity") +
  facet_wrap(~enzyme)+
  scale_fill_manual(values=c("grey"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = 'cornsilk2'), strip.text.x = element_text(size = 18))
  
 
##Box plot to Compare species##
enz.data.resid.long %>% 
  filter(Species %in% c("Lampea shallow", "Lampea deep")) %>%
  ggplot(aes(x = pressure, y = rate, c = Species, fill = Species)) +
  scale_x_discrete(labels = c('1','200','400','600','(recovery)'))+
  scale_y_continuous(trans = 'log10')+
  geom_boxplot(lwd=0.7) +
  facet_wrap(~enzyme) +
  xlab("Pressure (bar)")+
  ylab("Resisdual Activity (log10)")+
  scale_fill_manual(values=c("yellow3", "hotpink4"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = 'cornsilk2'),
        axis.line = element_line(colour = "white",size = 1, linetype = "solid"),
        axis.text.x = element_text(face="bold", color="#993333", size=7),axis.text.y = element_text(face="bold", color="#993333",size=7))
  
##Scatter plot of all species##

enz.data.resid.long %>% 
  filter(Species %in% c("Lampea deep","Bathocyroe fosteri", "Beroe abyssicola", "Tjalfiella pink")) %>%
  ggplot(aes(x = pressure, y = rate, c = Species, fill = Species)) +
  scale_x_discrete(labels = c('1','200','400','600','(recovery)'))+
  scale_y_continuous(trans = 'log10')+
  geom_boxplot(lwd=0.7)+
  facet_wrap(~enzyme) +
  xlab("Pressure (bar)")+
  ylab("Resisdual Activity (log10)")+
  scale_fill_manual(values=c("salmon", "tomato","coral", "darkred","indianred1"))+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = 'cornsilk2')),                                   axis.line = element_line(colour = "white",size = 1, linetype = "solid"))

