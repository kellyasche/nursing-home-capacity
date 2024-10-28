# library -----------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggrepel)
library(scales)
library(ggiraph)
library(rmapshaper)
library(cowplot)
library(RColorBrewer)
library(readxl)
library(lubridate)
library(readxl)
library(systemfonts)
reset_font_cache()
library(ggtext)
library(janitor)
library(cowplot)


rm(list = ls())

# themes ------------------------------------------------------------------
theme_bar <- theme_bw() +
  theme(panel.grid.major = element_line(color = "grey70", size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(1, "lines"),
        legend.margin = margin(0,0,0,0),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 2)),
        text = element_text(family = "Avenir") ,
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

theme_line <- theme_bw() +
  theme(legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.text = element_text(margin = margin(l = 2)),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey70", size = 0.1),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(1, "lines"),
        text = element_text(family = "Avenir") ,
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))


theme_sf <- theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 2)),
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(1, "lines"),
        text = element_text(family = "Avenir") ,
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

regions <- read_csv("/Users/kellyasche/Library/CloudStorage/GoogleDrive-kasche@ruralmn.org/My Drive/Data Prep/R Projects/Join docs/county_regions.csv") %>%
  select(5,6) %>%
  unique() %>%
  mutate(edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))

counties.regions <- read_csv("/Users/kellyasche/Library/CloudStorage/GoogleDrive-kasche@ruralmn.org/My Drive/Data Prep/R Projects/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Name = str_replace(Name, "Mcleod", "McLeod"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc) ,
         edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))

color.ruca <- c("Entirely rural" = "#009933", "Town/rural mix" = "#99CC33", "Urban/town/rural mix" = "#CC9966", "Entirely urban" = "#754C29", "Minnesota" = "black")

color.pr <- c("Northwest" = "#4575b4","Northeast" = "grey", "Central" = "#fee090", "Seven County Mpls-St Paul" = "#d73027", "Southwest" = "#91bfdb", "Southeast" = "#fc8d59", "Minnesota" = "black")

color.edr <- c("EDR 1 - Northwest" = "#b3cde3", "EDR 2 - Headwaters" = "#8c96c6", "EDR 3 - Arrowhead" = "#fe9929", "EDR 4 - West Central" = "#8856a7", "EDR 5 - North Central" = "#810f7c", "EDR 6E- Southwest Central" = "#e5f5f9", "EDR 6W- Upper Minnesota Valley" = "#bdc9e1", "EDR 7E- East Central" = "#99d8c9", "EDR 7W- Central" = "#2ca25f", "EDR 8 - Southwest" = "#74a9cf", "EDR 9 - South Central" = "#0570b0", "EDR 10 - Southeast" = "#d7301f", "EDR 11 - 7 County Twin Cities" = "#d8b365", "Minnesota" = "black")

color.pr.edr <- c ("Northwest" = "#4575b4","Northeast" = "#e0f3f8", "Central" = "#fee090", "Seven County Mpls-St Paul" = "#d73027", "Southwest" = "#91bfdb", "Southeast" = "#fc8d59", "Minnesota" = "black", "EDR 1 - Northwest" = "#b3cde3", "EDR 2 - Headwaters" = "#8c96c6", "EDR 3 - Arrowhead" = "#fe9929", "EDR 4 - West Central" = "#8856a7", "EDR 5 - North Central" = "#810f7c", "EDR 6E- Southwest Central" = "#e5f5f9", "EDR 6W- Upper Minnesota Valley" = "#bdc9e1", "EDR 7E- East Central" = "#99d8c9", "EDR 7W- Central" = "#2ca25f", "EDR 8 - Southwest" = "#74a9cf", "EDR 9 - South Central" = "#0570b0", "EDR 10 - Southeast" = "#d7301f", "EDR 11 - 7 County Twin Cities" = "#d8b365")

color.six <- c("#009933", "#4575b4", "grey", "#fee090", "#fc8d59", "#d73027")

mn_counties <- st_read("/Users/kellyasche/Library/CloudStorage/GoogleDrive-kasche@ruralmn.org/My Drive/Data Prep/R Projects/Shapefiles/County shapefiles/MNCounties_MNDOT.shp", quiet = TRUE) %>%
  ms_simplify(keep = .01, keep_shapes = TRUE) %>%
  rename("countyfp" = "FIPS_CODE")

# Prep data ---------------------------------------------------------------

age.group.original <- read_csv("Data/Population/Age/Master-age-group-projections-county.csv") %>%
  left_join(counties.regions[,c(1,8)], by = "countyfp") %>%
  mutate(Dem_Desc = fct_relevel(Dem_Desc, "Entirely rural", "Town/rural mix", "Urban/town/rural mix", "Entirely urban"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"),
         edr.simple = fct_relevel(edr.simple, "EDR 1", "EDR 2", "EDR 3", "EDR 4", "EDR 5", "EDR 6E", "EDR 6W", "EDR 7E", "EDR 7W", "EDR 8", "EDR 9", "EDR 10", "EDR 11"),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"))

ltc.demand.county <- age.group.original %>%
  mutate(ltc.demand = ifelse(age.group == "65_74", pop * .01,
                             ifelse(age.group == "75_84", pop * .03, pop * .08))) %>%
  group_by(year, countyfp, Name, planning.region, edr, edr.simple, Dem_Desc) %>%
  summarize(pop.65plus = sum(pop),
            ltc.demand = sum(ltc.demand)) %>%
  ungroup() 

ltc.demand.ruca <- ltc.demand.county %>%
  group_by(year, Dem_Desc) %>%
  summarize(pop.65plus = sum(pop.65plus),
            ltc.demand = sum(ltc.demand)) %>%
  ungroup() %>%
  group_by(Dem_Desc) %>%
  mutate(ltc.demand.index = (ltc.demand - ltc.demand[year == min(year)]) / ltc.demand[year == min(year)],
         peak.demand = ltc.demand[which.max(ltc.demand)],
         peak.demand.year = year[which.max(ltc.demand)]) %>%
  ungroup() %>%
  mutate(data_id = seq(n()))

ltc.peak.demand.county <- ltc.demand.county %>%
  group_by(countyfp) %>%
  filter(ltc.demand == max(ltc.demand)) %>%
  mutate(year.bins = cut(year,
                         breaks = c(0, 2030, 2035, 2040, 2045, 3000),
                         labeles = c("2025 to 2030", "2030 to 2035", "2035 to 2040", "2040 to 2045", "After 2045"))) %>%
  left_join(mn_counties[,c(5,7)], by = "countyfp")

# Create chart ------------------------------------------------------------

names(ltc.demand.ruca)

ltc.demand.ruca.plot <- ggplot(ltc.demand.ruca, aes(year, ltc.demand, color = Dem_Desc)) +
  facet_wrap(~Dem_Desc, ncol = 2, scales = "free_y") +
  geom_line(size = 3) +
  geom_point_interactive(size = 2, aes(data_id = data_id, tooltip = paste(Dem_Desc, "\nYear: ", year, "\nPopulation 65+: ", comma(pop.65plus), "\nEstimated demand for long term care: ", comma(ltc.demand), "\nChange in demand since ", min(year), ": ", percent(ltc.demand.index), sep = ""))) +
  labs(x="", y = "", color="", title = "Demand for skilled nursing beds among\npopulation 65+",
       subtitle = "The trends in demand vary significantly by rural and\nurban-ness")+
  geom_label_repel(data = filter(ltc.demand.ruca, year == max(year)), aes(label = paste("Peak demand\n", peak.demand.year, "\n", comma(peak.demand, accuracy = 1))), show.legend = FALSE, size = 3) +
  scale_y_continuous(labels=scales::comma)+
  scale_x_continuous(breaks = seq(1900, 2050, 5)) +
  scale_color_manual(values = color.ruca,
                     guide = guide_legend(ncol = 2)) +
  theme_line+
  theme(legend.position = "bottom",
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 25, hjust = .9))

ltc.demand.ruca.plot

ggsave(filename = "Charts/Paper report/skilled-nursing-demand-ruca.png", dpi = "print", width = 6, height = 5)

ggsave(filename = "Charts/Paper report/skilled-nursing-demand-ruca.pdf", device = cairo_pdf, dpi = "print", width = 6, height = 5)

