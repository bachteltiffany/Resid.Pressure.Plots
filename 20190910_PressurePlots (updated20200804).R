install.packages("gridExtra")
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
return(tidyverse)


#NOTES
# all the `dplyr::` stuff is to specify which package the function is supposed to be
# called from. Previously thought I was having some masking issues.
# Package specification probably not necessary anywhere except dplyr::select(),
# but it won't hurt.

#TODO 20190906
# remove outliers √ 20190910
# add error bars √ 20190910
# turn into line or bar plots √ 20190910 (non-EVT vs. EVT version ;)

## USER-DEFINED FUNCTIONS ##

# label me! What do I do?
calc_residual_activity <- function(enz.data, col.start, levels.pressure, count.enz){
  for(n.enz in seq(1, count.enz)){
    colnum.0psi <- col.start + ((n.enz - 1) * levels.pressure)
    for(level.pressure in rev(seq(1, levels.pressure))){
      colnum.pressure <- col.start + level.pressure - 1 + ((n.enz - 1) * levels.pressure)
      colname.0psi <- colnames(enz.data)[colnum.0psi]
      colname.pressure <- colnames(enz.data)[colnum.pressure]
      #colname.residact <- paste("resid", colname.pressure, sep='_')
      colname.residact <- colname.pressure
      #print(c(colnum.pressure, colnum.0psi, colname.residact)) #TEST
      # this should replace the raw data column with residual rate
      enz.data <- enz.data %>% mutate(!!colname.residact := .[[colnum.pressure]] / .[[colnum.0psi]])
    }
  }
  return(enz.data)
}

## MAIN ANALYSIS ##

data_file <- "C:/Users/Tiffa/Documents/DeepC/DeepC R Data/Master_CteneR_spellchecked.csv"

# load Master Ctene Data
enz.data <- readr::read_csv(data_file)

# calculate residual rates
col.start <- 6
levels.pressure <- 5 #including 1 atm and recovery
count.enz <- (ncol(enz.data) + 1 - col.start) / levels.pressure
enz.data.resid <- calc_residual_activity(enz.data, col.start, levels.pressure, count.enz)

# initial QC step: scrap any run with a residual rate value <0
enz.data.resid.good <- enz.data.resid %>% 
  dplyr::filter_all(all_vars(. >= 0))

# put dataframe in long form and group it for downstream stats
enz.data.resid.long <- enz.data.resid.good %>% 
  tidyr::gather(key = "enz.pressure", value = "rate", 
    -"Sample ID", -"Species", -"Mass (g)", -"Volume (m)", -"Coversion") %>% 
  dplyr::mutate(enzyme= substr(enz.pressure, 1, str_locate(enz.pressure, "[0-9]")-1)) %>% 
  dplyr::mutate(pressure= substr(enz.pressure, str_locate(enz.pressure, "[0-9]"), str_length(enz.pressure))) %>% 
  dplyr::select(-enz.pressure) %>% 
  tidyr::drop_na(rate) %>% 
  dplyr::group_by(Species, enzyme, pressure)

## OUTLIER REMOVAL ##

enz.data.resid.long.clean <- enz.data.resid.long %>% 
  # remove negative residual activities
  dplyr::filter(rate > 0) %>% 
  # 2*sigma outlier filter
  #filter(!(abs(rate - median(rate)) > 2*sd(rate)))
  # 1.5*IQR outlier filter
  dplyr::filter(!(abs(rate - median(rate)) > 1.5*IQR(rate, na.rm=TRUE)))

## SUMMARY STATS ##

enz.data.resid.clean.summary <- enz.data.resid.long.clean %>% 
  dplyr::summarise(mean = mean(rate), stderr = sd(rate)/sqrt(n()), n = n())

## PLOTS ##

# make sure x-axis plots in the right order
levels.factor.pressure <- c("0psi", "3000psi", "6000psi", "9000psi", "0Recovery")
enz.data.resid.clean.summary$pressure <- factor(enz.data.resid.clean.summary$pressure, levels = levels.factor.pressure, ordered = TRUE)
enz.data.resid.long$pressure <- factor(enz.data.resid.long$pressure, levels = levels.factor.pressure, ordered = TRUE)

# select species to display
# be sure to check your spelling!

# get all species with >=3 biological replicates
Species.todisplay <- enz.data.resid.good %>% 
  group_by(Species) %>% 
  add_tally() %>% 
  filter(n >= 3) %>% 
  pull(Species) %>% 
  unique()

# OR specify manually
#Species.todisplay <- c("Hormiphora californensis", "Tjalfiella pink")
#Species.todisplay <- c("Beroe forskalii", "Lampea shallow", "Undescribed spC", "Tjalfiella pink")

# vanilla residual activity plots
enz.data.resid.clean.summary %>% 
  filter(Species %in% Species.todisplay) %>%
  ggplot(aes(x = pressure, y = mean, group = Species, color = Species)) + 
  scale_y_log10() +
  facet_grid(~enzyme) + 
  geom_line() +
  geom_errorbar(aes(ymin = mean-stderr, ymax = mean+stderr), color = "black", position = "dodge", width = 0.3)

# outlier check!
dirty <- enz.data.resid.long %>% 
  filter(Species %in% Species.todisplay) %>%
  ggplot(aes(x = pressure, y = rate, color = Species)) + 
  facet_grid(~enzyme) + 
  geom_point()

clean <- enz.data.resid.long.clean %>% 
  filter(Species %in% Species.todisplay) %>%
  ggplot(aes(x = pressure, y = rate, color = Species)) + 
  facet_grid(~enzyme) + 
  geom_point()

grid.arrange(dirty, clean, nrow=1)

## BOXPLOTS ##

enz.data.resid.long %>% 
  filter(Species %in% c("Undescribed spW")) %>%
  ggplot(aes(x = pressure, y = rate, c = Species, fill = Species)) +
  scale_x_discrete(labels = c('1','200','400','600','(Rec)'))+
  #scale_y_continuous(trans = 'log10')+
  geom_boxplot(lwd=0.4, color="black") +
  facet_wrap(~enzyme) +
  xlab("Pressure (bar)")+
  ylab("Resisdual Activity (log10)")+
  scale_fill_manual(values=c("cornflowerblue"))+
  guides(colour = guide_legend(title.position = "top",
                               keywidth = unit(1, "cm"),
                               keyheight = unit(0.5, "cm"),
                               override.aes = list(shape = 22,size = 10)))+
  theme(legend.position=c(0.9,0.8),
        legend.title = element_text(size=10,face="bold"),
        legend.text=element_text(size=10),
        legend.background = element_rect(fill="white"),
        #plot.background = element_rect(fill ="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.background = element_rect(fill = 'black'),
        panel.spacing.x = unit(0,"line"),
        panel.border=element_rect(fill=NA),
        strip.text.x = element_text(size = 12),
        strip.placement = "inside",
        axis.line = element_line(colour = "black",size = 1, linetype = "solid"),axis.text.x = element_text(angle = 45,size=12, colour="black"),axis.text.y = element_text( color="black",size=12))

##ANOVA##
species.aov<-aov(pressure ~ rate, data = enz.data.resid.long)


#Mean +/- Standard deviation#
enz.data.resid.long %>% 
  filter(Species %in% c("Undescribed spW")) %>%
  ggerrorplot(aes(x = pressure, y = rate, c = Species, fill = Species,desc_stat = "mean_sd")) +
  scale_x_discrete(labels = c('1','200','400','600','(Rec)'))+
  #scale_y_continuous(trans = 'log10')+
  geom_boxplot(lwd=0.4, color="black") +
  facet_wrap(~enzyme)


