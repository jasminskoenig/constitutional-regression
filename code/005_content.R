library(tidyverse)
library(hrbrthemes)
library(ggplot2)
library(ggrepel)

# Set-up ----

theme_gridY <- theme_ipsum_rc(grid = "Y") +
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y.left = element_text(size = 18),
    strip.text = element_text(size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 12),
    legend.key.size = unit(0.5, "cm"),
    plot.margin = grid::unit(c(1, 0.5, 0.5, 0), "mm"),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
  )

theme_set(theme_gridY)

# Data ----

ccpc_vdem <- readRDS("data/ccpc_vdem_eu_la.rds") 

pos <- position_jitter(width = 0.5, seed = 2)

rights_labs <- c("Rights of Executive", "Rights of Judiciary", "Political Rights", "Social Rights")
names(rights_labs) <- c("diff_executive", "diff_judiciary", "diff_rights_pol", "diff_rights_social")

ccpc_vdem %>% 
  dplyr::select(country, gov_popul_weighted, year, e_regiongeo, diff_rights_ind, diff_rights_social, diff_rights_political, diff_executive, diff_judiciary, v2x_libdem) %>%
  filter(lag(v2x_libdem) > v2x_libdem) %>% 
  filter(gov_popul_weighted > 0.5) %>%
  dplyr::select(-diff_rights_ind) %>%
  filter(year > 1990) %>% 
  dplyr::select(-v2x_libdem) |> 
  pivot_longer(cols = contains("diff"), values_to = "n", names_to = "rights") %>% 
  filter(n != 0) %>% 
  mutate(continent = case_when(
    e_regiongeo %in% c(17:29) ~ "Latinamerika",
    e_regiongeo %in% c(1:4) ~ "Europe",
    TRUE ~ NA)) %>%
  ggplot() +
  geom_hline(yintercept=0, color = "darkslategrey") +
  geom_jitter(aes(x = 0, 
                  y = n, 
                  color = continent,
                  shape = continent), 
              position = pos,
              size = 3) +
  geom_text_repel(aes(x = 0, 
                      y = n, 
                      label = paste(country, year)), 
                  position = pos, size = 5) +
  facet_wrap(~ rights,
             labeller = labeller(rights = rights_labs),
             ncol = 2) +
  ylim(-3, 8) +
  labs(x = "",
       y = "Change in Number of Rights",#
       color = "",
       shape = "") +
  scale_color_brewer(palette = "Dark2")

ggsave("results/graphs/rights_change.pdf", width = 20, height = 20, units = "cm", device = cairo_pdf)
ggsave("results/graphs/rights_change.png", width = 20, height = 20, units = "cm", device = "png")
