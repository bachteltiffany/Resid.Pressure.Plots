library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(haven)
library(ggplot2)

data_file <- "C:/Users/Tiffa/Documents/DeepC/Enzyme Rates/Master_CteneR.csv"

#clean up Master Ctene Data
enz.data <- read_csv(data_file)

#caculate residual rate

gather(key = "enz.pressure", value = "rate", 
  -"Sample ID", -"Species", -"Mass (g)", -"Volume (m)", -"Coversion") %>% 
mutate(enzyme= substr(enz.pressure, 1, str_locate(enz.pressure, "[0-9]")-1))%>% 
mutate(pressure= substr(enz.pressure, str_locate(enz.pressure, "[0-9]"),
                        str_length(enz.pressure))) %>% 
select(-enz.pressure)

#caculate fold change
initial.rate <- unite(enz.data, "samp.enz", c("Sample ID", "enzyme")) %>% 
  filter(pressure == "0psi") %>% 
  select(samp.enz, rate)
initial.rate.dict <- list(initial.rate$rate)
setNames(initial.rate.dict, list(initial.rate$samp.enz))
mutate(foldchg = filter(enz.data, ("Sample ID" == "Sample ID" && 
                                     "enzyme" == "enzyme" && 
                                     "pressure" == "0psi"))$rate/rate)
#enz.data$pressure <- as_factor(enz.data$pressure, levels = c("0psi", "3000psi", "6000psi", "0Recovery"), ordered = TRUE)
#ggplot(enz.data, aes(x = pressure, y = rate, color = Species)) + 
  #facet_wrap(~enzyme) + 
  #geom_point()
View(enz.data)