library(tidyverse)
library(lubridate)
library(vdemdata)

# VParty ----

vparty %>% 
  filter(year>1989) -> 
  vparty2

vparty2 %>% 
  select(v2paenname,v2paid,country_name,year,
         country_id,e_regiongeo,v2xpa_antiplural,v2xpa_popul,
         v2paseatshare,v2patotalseat,v2pavote,v2pagovsup,
         ep_type_populism,ep_type_populist_values, 
         ep_v8_popul_rhetoric,ep_v9_popul_saliency, v2pariglef, pf_party_id) -> 
  vparty2

# partyfacts ----

file_name <- "partyfacts-mapping.csv"
if( ! file_name %in% list.files()) {
  url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  download.file(url, file_name)
}
partyfacts_raw <- read_csv(file_name, guess_max = 50000)

partyfacts <- partyfacts_raw |> 
  filter(!is.na(partyfacts_id))

partyfacts |> 
  select(partyfacts_id, dataset_party_id, country, dataset_key, name_english) ->
  partyfacts_lookup

# match with popuList ----

popuList <- readxl::read_xlsx("data/popuList.xlsx")

# partyfacts

partyfacts_lookup |> 
  filter(dataset_key == "parlgov") |> 
  mutate(dataset_party_id = as.integer(dataset_party_id)) |> 
  distinct(partyfacts_id, .keep_all = TRUE) ->
  partyfacts_parlgov

# change start and end of populist "area" of each party to interval
popuList |> 
  left_join(partyfacts_parlgov, by = c("parlgov_id" = "dataset_party_id")) |> 
  mutate(partyfacts_id = if_else(
    is.na(partyfacts_id.x), partyfacts_id.y, partyfacts_id.x
  )) |> 
  select(-partyfacts_id.x, partyfacts_id.y) |> 
  mutate(populist_start = if_else(populist_start < 1990, 1990, populist_start),
         populist_start = if_else(populist_start == 2100, 2019, populist_start),
         populist_end = if_else(populist_end == 2100, 2020, populist_end),
         populist_interval = interval(ymd(populist_start, truncated = 2L),
                                      ymd(populist_end, truncated = 2L))) ->
  popuList

# select columns for matching
popuList_short <- popuList |> 
  select(partyfacts_id, party_name_english, populist_interval, country_name)

popuList_shorter <- popuList_short |> 
  select(partyfacts_id, populist_interval) |> 
  distinct(partyfacts_id, .keep_all = TRUE)

# popuList included countries

popuList |> 
  group_by(country_name) |> 
  distinct(country_name) |> 
  mutate(dataset = "PopuList") ->
  popuList_countries

# match with vparty2 on party-country base
vparty2 |> 
  left_join(popuList_short,
            by = c("v2paenname" = "party_name_english",
                   "country_name" = "country_name")) |>
  select(-partyfacts_id) |> 
  # match on partyfacts id base if names didn't match
  left_join(popuList_shorter, 
            by = c("pf_party_id" = "partyfacts_id"),
            na_matches = "never") |>  
  # choose the interval that's not NA
  mutate(populist_interval = if_else(
    !is.na(populist_interval.x), populist_interval.x, populist_interval.y)
  ) |> 
  select(-populist_interval.x, -populist_interval.y) |> 
  # now check for each year of party in vparty dataset whether it is included in pop interval
  mutate(rooduijn = if_else(
    # if there is no interval for the party check whether country is in popuList
    is.na(populist_interval), if_else(
      country_name %in% popuList$country_name, 0, NA # 0 if country included, NA if it isnt
    ), if_else(
      ymd(year, truncated = 2L) %within% populist_interval, 1, 0)
    ),
    rooduijn_senior = case_when(
      rooduijn == 1 & v2pagovsup == 0 ~ 1,
      is.na(rooduijn) ~ NA,
      TRUE ~ 0
      )) ->
  vparty2

# Coding the V-Party Dataset ----

# calculate populism score of government
vparty2 -> 
  df

# party in government
df$gov_party <- ifelse(df$v2pagovsup %in% c(0, 1, 2), 1, 0)

# exclude parties that only support government, but are not respresented
df$gov_party_con <- ifelse(df$v2pagovsup %in% c(0, 1), 1, 0)

# prime minister
df$primemin_party <- ifelse(df$v2pagovsup == 0, 1, 0)

df %>% 
  filter(gov_party == 1) ->
  vparty_governments

# calculate government information
vparty_governments %>% 
  group_by(country_name,year) %>% 
  # seat share of government
  mutate(gov_seatshare = sum(v2paseatshare), 
         # calculate weight of government parties
         weight = v2paseatshare/gov_seatshare,
         rooduijn_government = if_else(sum(rooduijn) > 0, "Populist", "Non-Populist"),
         rooduijn_government_senior = if_else(sum(rooduijn_senior) > 0, "Populist", "Non-Populist")) %>% 
  group_by(country_name, year, gov_seatshare, e_regiongeo, rooduijn_government, rooduijn_government_senior) %>% 
  # calulcate mean of populism score and weighted populism score per year of country (= per government)
  summarise(gov_popul_mean = mean(v2xpa_popul),
            # calculate weighted pop score per party and then add as weighted pop score per government
            gov_popul_weighted = sum(v2xpa_popul*weight),
            # calculate weighted left - right econ
            gov_ideol_mean = mean(v2pariglef),
            gov_ideol_weighted = sum(v2pariglef*weight),
            no_govparties = n(),
            .groups = "drop") |>
  # dummies for economic left-right
  mutate(econ_right = if_else(gov_ideol_weighted > 0.5, 1, 0),
         econ_left = if_else(gov_ideol_weighted < 0.5, 1, 0)) -> 
  vparty_governments_populism

# populism score of party of president in vparty
df %>% 
  filter(primemin_party == 1) |>  
  group_by(country_name, year, rooduijn) %>% 
  mutate(gov_popul_prime = v2xpa_popul) %>%
  # Only very few countries have heads of governments from two parties (conflict resolution)
  # for these we calculate the mean
  summarize(gov_popul_prime = mean(gov_popul_prime),
            .groups = "drop") ->
  vparty_prime

# join together as new dataframe that includes information on populism in government
partydat <- left_join(vparty_governments_populism,
                      vparty_prime,
                      by=c("country_name","year")) 
glimpse(partydat)

saveRDS(partydat, "data/party_populism.rds")

