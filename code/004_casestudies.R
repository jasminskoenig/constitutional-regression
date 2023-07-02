library(tidyverse)
library(hrbrthemes)
library(ggplot2)
library(patchwork)

# Set-up ----

theme_gridY <- theme_ipsum_rc(grid = "Y") +
  theme(
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y.left = element_text(size = 18),
    strip.text = element_text(size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.key.size = unit(0.5, "cm"),
    plot.margin = grid::unit(c(1, 0.5, 0.5, 0), "mm"),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
  )

theme_set(theme_gridY)

# Data ----

ccpc_vdem <- readRDS("data/ccpc_vdem_eu_la") 

# Hungary ----

ccpc_vdem %>% 
  filter(country == "Hungary" & year > 1990) -> 
  df

df %>% 
  filter(evnt==1) -> 
  dfevnt

## democratic quality ----

df %>%
  ggplot(aes(x = year, y = v2x_libdem)) +
  geom_vline(data = dfevnt, 
             aes(xintercept = year), 
             color = "darkgrey") +
  geom_vline(xintercept = 2010, 
             color = "#F8766D", 
             linetype = "dashed", 
             linewidth = 1.1) +
  geom_line(size = 1) +
  labs(y = "Liberal Democracy Index",
       x = "",
       title = "Hungary") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) -> 
  hungary_democracy

## populism score ----

df %>% 
  ggplot(aes(x = year, y = gov_popul_weighted)) +
  geom_vline(xintercept = 2010, color = "#F8766D", linetype = "dashed", lwd = 1.1) +
  geom_line(size = 1) +
  labs(x = "",
       y = "Weighted Populis Score") -> 
  hungary_populism

# Poland ----

ccpc_vdem %>% 
  filter(country == "Poland",
         year > 1990) -> 
  df_poland

df_poland %>% 
  filter(evnt == 1) -> 
  dfevnt_poland

## liberal democracy ----

df_poland %>% 
  ggplot(aes(x = year, y = v2x_libdem)) +
  geom_vline(xintercept = 2015, color = "#F8766D", linetype = "dashed", linewidth = 1.1) +
  geom_vline(data = dfevnt_poland, aes(xintercept = year), color = "darkgrey") +
  geom_line(size = 1) +
  labs(x = "",
       y = "",
       title = "Poland") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) -> 
  poland_democracy

## populism score ---- 

df %>% 
  ggplot(aes(x = year, y = gov_popul_weighted)) +
  geom_vline(xintercept = 2015, color = "#F8766D", linetype = "dashed", lwd = 1.1) +
  geom_line(size = 1) +
  labs(x = "Year",
       y = "")  ->
  poland_populism

patchworked_casestudies <- hungary_democracy + poland_democracy + hungary_populism + poland_populism

patchworked_casestudies

ggsave("results/graphs/casestudies.pdf", width = 10, height = 10, units = "in", device = cairo_pdf)
ggsave("results/graphs/casestudies.png", width = 10, height = 10, units = "in")
