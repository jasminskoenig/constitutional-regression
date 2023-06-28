# Set-up ----

library(tidyverse)
library(lubridate)
library(rio)
library(stargazer)
library(rms)
library(interplot)
library(hrbrthemes)
library(lme4)
library(margins)
library(broom.mixed)
library(gridExtra)
library(margins)
library(grid)
library(patchwork)
extrafont::loadfonts()

theme_gridY <- theme_ipsum_rc(grid = "Y") +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    strip.text = element_text(size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.5, "cm"),
    plot.margin = grid::unit(c(1, 0.5, 0.5, 0), "mm"),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
  )

# There is an additional case in the org. data (Moldova 2009 is doubled)
df2 |>  filter(country == "Moldova" & year == 2009) -> add
df2 |>  bind_rows(add) -> df2_inspect

# set theme
theme_set(theme_gridY)

# Data ----

ccpc_vdem <- readRDS("data/ccpc_vdem_eu_la") 

ccpc_vdem|>
  # at time of analysis dataset only had data till 2020, 1990 democratization in ee
  filter(
    year <= 2020 & year > 1990,
    # exclude closed autocracies
    v2x_regime > 0,
    country != "Moldova"
  ) |>
  # year as factor for random intercepts
  mutate(year = as.factor(year)) ->
  ccpc_vdem

# filter European countries
ccpc_vdem %>%
  filter(e_regiongeo %in% c(1:4)) ->
  df2

# Inspect data
summary(df2)


# Analysis (only Europe) ----

## Preparation ----

### Prep: asses means for country and year ----

# liberal democracy
m0 <- lmer(lead(v2x_libdem, 1) ~ (1 | country) + (1 | year), data = df2)
summary(m0)

# participation
m0cs <- lmer(lead(v2x_cspart, 1) ~ (1 | country) + (1 | year), data = df2)
summary(m0cs)

# assess clustering structure of the data
performance::icc(m0)
performance::icc(m0cs)

## Run Regressions ----

### Liberal Democracy ----

# weighted government -> lib dem this would have a random slope as well
m1 <- lmer(lead(v2x_libdem, 1) ~ gov_popul_weighted + (gov_popul_weighted | country) + (1 | year), data = df2)
summary(m1)

# weighted government -> lib dem
m1 <- lmer(lead(v2x_libdem, 1) ~ gov_popul_weighted + (1 | country) + (1 | year), data = df2)
summary(m1)

# change in constitution in general -> lib dem
m2 <- lmer(lead(v2x_libdem, 1) ~ evnt + (1 | country) + (1 | year), data = df2)
summary(m2)

# interaction -> libdem
m3 <- lmer(lead(v2x_libdem, 1) ~ evnt * gov_popul_weighted + (1 | country) + (1 | year), data = df2)
summary(m3)

### Participation ----

# weighted government -> part
m1cs <- lmer(lead(v2x_cspart, 1) ~ gov_popul_weighted + (1 | country) + (1 | year), data = df2)
summary(m1cs)

# change in constitution in general -> part
m2cs <- lmer(lead(v2x_cspart, 1) ~ evnt + (1 | country) + (1 | year), data = df2)
summary(m2cs)

# interaction -> part
m3cs <- lmer(lead(v2x_cspart, 1) ~ evnt * gov_popul_weighted + (1 | country) + (1 | year), data = df2)
summary(m3cs)

## Regression Output ----

### Quantile calculations for text ----

# democracy index
qs <- quantile(df2$gov_popul_weighted, probs = c(0.05, 0.95), na.rm = TRUE)
qs[1] * -0.113 - qs[2] * -0.113

# civil society index
qs <- quantile(df2$gov_popul_weighted, probs = c(0.05, 0.95), na.rm = TRUE)
qs[1] * -0.197 - qs[2] * -0.197

### Regression Table ----

# make table
tidym1 <- broom::tidy(m1) %>% filter(effect == "fixed")
tidym2 <- broom::tidy(m2) %>% filter(effect == "fixed")
tidym3 <- broom::tidy(m3) %>% filter(effect == "fixed")
tidym1cs <- broom::tidy(m1cs) %>% filter(effect == "fixed")
tidym2cs <- broom::tidy(m2cs) %>% filter(effect == "fixed")
tidym3cs <- broom::tidy(m3cs) %>% filter(effect == "fixed")

# counts of random effects
num_year <- nrow(ranef(m1)$year)
num_country <- nrow(ranef(m1)$country)


# standard deviation of random effects
sd_year1 <- round(attributes(VarCorr(m1)$"year")$stddev, 3)
sd_country1 <- round(attributes(VarCorr(m1)$"country")$stddev, 3)

sd_year2 <- round(attributes(VarCorr(m2)$"year")$stddev, 3)
sd_country2 <- round(attributes(VarCorr(m2)$"country")$stddev, 3)

sd_year3 <- round(attributes(VarCorr(m3)$"year")$stddev, 3)
sd_country3 <- round(attributes(VarCorr(m3)$"country")$stddev, 3)

sd_year1cs <- round(attributes(VarCorr(m1cs)$"year")$stddev, 3)
sd_country1cs <- round(attributes(VarCorr(m1cs)$"country")$stddev, 3)

sd_year2cs <- round(attributes(VarCorr(m2cs)$"year")$stddev, 3)
sd_country2cs <- round(attributes(VarCorr(m2cs)$"country")$stddev, 3)

sd_year3cs <- round(attributes(VarCorr(m3cs)$"year")$stddev, 3)
sd_country3cs <- round(attributes(VarCorr(m3cs)$"country")$stddev, 3)

# calculate standard errors clustered by cabinet

tribble(
  ~stat, ~m1, ~m2, ~m3, ~m1cs, ~m2cs, ~m3cs,
  "Number of Years", num_year, num_year, num_year, num_year, num_year, num_year,
  "Number of Governments", num_country, num_country, num_country, num_country, num_country, num_country,
  "sd(Year)", sd_year1, sd_year2, sd_year3, sd_year1cs, sd_year2cs, sd_year3cs,
  "sd(Country)", sd_country1, sd_country2, sd_country3, sd_country1cs, sd_country2cs, sd_country3cs,
  "", NA, NA, NA, NA, NA, NA,
  "N", nobs(m1), nobs(m2), nobs(m3), nobs(m1cs), nobs(m2cs), nobs(m3cs)
) ->
mod_stats


# create table

stargazer(m1, m2, m3, m1cs, m2cs, m3cs,
  type = "latex",
  # Below: manually supply tidied coefficients and standard errors
  coef = list(tidym1$estimate, tidym2$estimate, tidym3$estimate, tidym1cs$estimate, tidym2cs$estimate, tidym3cs$estimate),
  se = list(tidym1$std.error, tidym2$std.error, tidym3$std.error, tidym1cs$std.error, tidym2cs$std.error, tidym3cs$std.error),
  omit = c("year", "country"),
  # Omit model statistics by default...
  omit.table.layout = "s",
  # ...but supply your own that you created (with random effects)
  add.lines = lapply(1:nrow(mod_stats), function(i) unlist(mod_stats[i, ])),
  dep.var.labels = c("Liberal Democracy", "Participation"),
  model.names = FALSE
)

### Plot for liberal democracy ----

interplot(m3,
  var1 = "evnt",
  var2 = "gov_popul_weighted",
  hist = TRUE
) +
  xlab("Government Populism Score") +
  ylab("Effect of Constitutional Change") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Liberal Democracy") ->
p1

p1

### Plot for participation ----

interplot(m3cs,
  var1 = "evnt",
  var2 = "gov_popul_weighted",
  hist = TRUE
) +
  xlab("Government Populism Score") +
  ylab("Effect of Constitutional Change") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Civil Society Index") ->
p2

p2

### European combined plot ----

european_effects <- p1 + p2

european_effects

ggsave("results/regression_europe.pdf", width = 15, height = 6, units = "in", device = cairo_pdf)
ggsave("results/regression_europe.png", width = 15, height = 6, units = "in", device = "png")

# Analysis (Latin America & Europe) ----

## Data ----

# filter data  frame to both continents
ccpc_vdem %>%
  filter(e_regiongeo %in% c(1:4, 17:18)) |>
  # create dummy for interaction effect
  mutate(latin = if_else(e_regiongeo %in% c(17, 18), 1, 0)) ->
  df4

## Run Regressions ----

### Liberal Democracy ----
m1lat <- lmer(lead(v2x_libdem, 1) ~ gov_popul_weighted + (1 | country) + (1 | year), data = df4)
summary(m1lat)

m2lat <- lmer(lead(v2x_libdem, 1) ~ evnt + (1 | country) + (1 | year), data = df4)
summary(m2lat)

m3lat <- lmer(lead(v2x_libdem, 1) ~ evnt * gov_popul_weighted * latin + (1 | country) + (1 | year), data = df4)
summary(m3lat)

### Participation ----

m1latcs <- lmer(lead(v2x_cspart, 1) ~ gov_popul_weighted + (1 | country) + (1 | year), data = df4)
summary(m1latcs)

m2latcs <- lmer(lead(v2x_cspart, 1) ~ evnt + (1 | country) + (1 | year), data = df4)
summary(m2latcs)

m3latcs <- lmer(lead(v2x_cspart, 1) ~ evnt * gov_popul_weighted * latin + (1 | country) + (1 | year), data = df4)
summary(m3latcs)

## Regression Output ----

### Create regression table ----

# make table

tidym1lat <- broom::tidy(m1lat) %>% filter(effect == "fixed")
tidym2lat <- broom::tidy(m2lat) %>% filter(effect == "fixed")
tidym3lat <- broom::tidy(m3lat) %>% filter(effect == "fixed")
tidym1latcs <- broom::tidy(m1latcs) %>% filter(effect == "fixed")
tidym2latcs <- broom::tidy(m2latcs) %>% filter(effect == "fixed")
tidym3latcs <- broom::tidy(m3latcs) %>% filter(effect == "fixed")

# counts of random effects

num_year <- nrow(ranef(m1lat)$year)
num_country <- nrow(ranef(m1lat)$country)

# standard deviation of random effects

sd_year1 <- round(attributes(VarCorr(m1lat)$"year")$stddev, 3)
sd_country1 <- round(attributes(VarCorr(m1lat)$"country")$stddev, 3)

sd_year2 <- round(attributes(VarCorr(m2lat)$"year")$stddev, 3)
sd_country2 <- round(attributes(VarCorr(m2lat)$"country")$stddev, 3)

sd_year3 <- round(attributes(VarCorr(m3lat)$"year")$stddev, 3)
sd_country3 <- round(attributes(VarCorr(m3lat)$"country")$stddev, 3)

sd_year1cs <- round(attributes(VarCorr(m1latcs)$"year")$stddev, 3)
sd_country1cs <- round(attributes(VarCorr(m1latcs)$"country")$stddev, 3)

sd_year2cs <- round(attributes(VarCorr(m2latcs)$"year")$stddev, 3)
sd_country2cs <- round(attributes(VarCorr(m2latcs)$"country")$stddev, 3)

sd_year3cs <- round(attributes(VarCorr(m3latcs)$"year")$stddev, 3)
sd_country3cs <- round(attributes(VarCorr(m3latcs)$"country")$stddev, 3)

# combine stats into table 

tribble(
  ~stat, ~m1lat, ~m2lat, ~m3lat, ~m1latcs, ~m2latcs, ~m3latcs,
  "Number of Years", num_year, num_year, num_year, num_year, num_year, num_year,
  "Number of Governments", num_country, num_country, num_country, num_country, num_country, num_country,
  "sd(Year)", sd_year1, sd_year2, sd_year3, sd_year1cs, sd_year2cs, sd_year3cs,
  "sd(Country)", sd_country1, sd_country2, sd_country3, sd_country1cs, sd_country2cs, sd_country3cs,
  "", NA, NA, NA, NA, NA, NA,
  "N", nobs(m1lat), nobs(m2lat), nobs(m3lat), nobs(m1latcs), nobs(m2latcs), nobs(m3latcs)
  ) -> 
  mod_stats


# output table with stargazer

stargazer(m1lat, m2lat, m3lat, m1latcs, m2latcs, m3latcs,
  type = "latex",
  # Below: manually supply tidied coefficients and standard errors
  coef = list(tidym1lat$estimate, tidym2lat$estimate, tidym3lat$estimate, tidym1latcs$estimate, tidym2latcs$estimate, tidym3latcs$estimate),
  se = list(tidym1lat$std.error, tidym2lat$std.error, tidym3lat$std.error, tidym1latcs$std.error, tidym2latcs$std.error, tidym3latcs$std.error),
  omit = c("year", "country"),
  # Omit model statistics by default...
  omit.table.layout = "s",
  add.lines = lapply(1:nrow(mod_stats), function(i) unlist(mod_stats[i, ])),
  dep.var.labels = c("Liberal Democracy", "Participation"),
  model.names = FALSE
)

# create sequence of 0.05 distance to calculate marginal effects
govpol_seq <- seq(
  min(df4$gov_popul_weighted, na.rm = TRUE),
  max(df4$gov_popul_weighted, na.rm = TRUE),
  0.05
)

### Plotting for Liberal Democracy ----

# marginal effects for liberal democracy based on interaction midel

m3lat %>%
  margins(
    variables = "evnt",
    at = list(gov_popul_weighted = govpol_seq, latin = c(0, 1))
  ) %>%
  summary() ->
meff

# calculate lower and upper confidence interval

meff %>%
  mutate(lower = AME - 1.96 * SE, 
         upper = AME + 1.96 * SE,
         # rename latin dummy for plot
         latin = if_else(latin == 0, "Europe", "Latin America")) %>%
  dplyr::select(gov_popul_weighted, latin, AME, upper, lower) ->
  plotdf_ld

## update theme of plots

theme_gridY <- theme_ipsum_rc(grid = "Y") +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    strip.text.x = element_text(size = 10, hjust = 0),
    strip.placement = "inside",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.5, "cm"),
    plot.title = element_text(size = 12, hjust = -0.101),
    plot.subtitle = element_text(size = 12, face = "plain", hjust = -0.077),
    plot.margin = grid::unit(c(1, 0.5, 0.5, 0), "mm"),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
  )

theme_set(theme_gridY)

## Final Plot for Liberal Democracy 

ggplot(plotdf_ld, aes(x = gov_popul_weighted, y = AME)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#6C6F7F") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_line() +
  facet_grid(~latin) +
  labs(caption = "") +
  labs(title = "",
       subtitle = "Liberal Democracy",
       x = "Government Populism Score",
       y = "") +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    limits = c(0, 1)
  ) +
  ylim(-0.1, 0.1)  ->
plot_ld

plot_ld

ggsave("results/liberaldem_interaction.pdf", width = 14, height = 6, units = "in", dev = cairo_pdf)
ggsave("results/liberaldem_interaction.png", width = 14, height = 6, units = "in", dev = "png")

### Plotting for Participation ----

m3latcs %>%
  margins(
    variables = "evnt",
    at = list(gov_popul_weighted = govpol_seq, latin = c(0, 1))
  ) %>%
  summary() ->
  meff_cs

# calculate confidence intervals 

meff_cs |> 
  mutate(lower = AME - 1.96 * SE, 
         upper = AME + 1.96 * SE,
         # rename dummy for plots
         latin = if_else(latin == 0, "Europe", "Latin America")) |>  
  dplyr::select(gov_popul_weighted, latin, AME, upper, lower) ->
  plotdf_cs

# Final Plot Participation 

ggplot(
  plotdf_cs,
  aes(x = gov_popul_weighted, y = AME)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#6C6F7F") +
  geom_line() +
  facet_grid(~latin) +
  labs(title = "Effect of Constitutional Change on...",
       subtitle = "Civil Society",
       x = "",
       y = "Marginal Effect")+
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    limits = c(0, 1)
  ) +
  ylim(-0.1, 0.1) +
  ## update theme of plots (different to liberal democracy for combination of plots later)
  theme(panel.spacing = unit(1, "lines")) +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    strip.text = element_text(size = 10, vjust = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.5, "cm"),
    plot.title = element_text(size = 12, face = "plain", hjust = -0.101),
    plot.subtitle = element_text(size = 12, face = "plain", hjust = -0.077),
    plot.margin = grid::unit(c(1, 0.5, 0.5, 0), "mm"),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) ->
plot_cs

plot_cs

ggsave("results/participationdem_interaction.pdf", width = 14, height = 6, units = "in", dev = cairo_pdf)
ggsave("results/participation_interaction.png", width = 14, height = 6, units = "in", dev = "png")

### Combined Final Plots for Paper ----

patchworked <- plot_cs + plot_ld + plot_layout(ncol = 1)

patchworked

ggsave("results/interaction.pdf", width = 8, height = 8, units = "in", dev = cairo_pdf)
ggsave("results/interaction.png", width = 8, height = 8, units = "in", dev = "png")
