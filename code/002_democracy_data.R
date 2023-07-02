library(tidyverse)
library(lubridate)
library(vdemdata)
library(stringi)
library(readtext)

# ADD VDEM AND CCPC DATA TO PARTYDATA

# Data Import

partydat <- readRDS("data/party_populism.rds")

# VDEM ----
vdem %>% 
  filter(year > 1989) -> 
  vdem2

vdem2 %>% 
  dplyr::select(country_name, year, country_id, e_regiongeo, v2x_polyarchy,
         v2x_libdem, v2x_delibdem, v2x_egaldem, v2x_liberal, v2xcl_rol, v2x_cspart,
         v2x_jucon, v2jureform, v2jupurge, v2jupoatck, v2jupack,
         v2juaccnt, v2jucorrdc, v2juhcind, v2juncind, v2juhccomp,
         v2jucomp, v2jureview, v2x_regime) |> 
  mutate(country_name = if_else(country_name == "Czechia", "Czech Republic", country_name)) -> 
  vdem2

joined_vdata <- left_join(vdem2,
                          partydat,
                          by=c("country_name", "year", "e_regiongeo")) 

glimpse(joined_vdata)

# add ruth data, as this isn't based on party but president

# Ruth 2022 ----

ruth <- read.csv("data/ruth2022.csv", header = FALSE)

ruth_smaller <- ruth |> 
  dplyr::select(V2, V4, V5, V7) |> 
  rename("country" = "V2",
         "year" = "V4",
         "president" = "V5",
         "ruth_populism" = "V7") |> 
  mutate(ruth_populism = case_when(
    ruth_populism == 1 ~ "Populist",
    ruth_populism == 0 ~ "Non-Populist",
    is.na(ruth_populism) ~ NA_character_
  ))

joined_vdata |> 
  left_join(ruth_smaller, by = c("country_name" = "country",
                                 "year" = "year")) ->
  joined_vdata

ruth_smaller |> 
  group_by(country) |> 
  distinct(country) |> 
  mutate(dataset = "Ruth-Lovell & Grahn") ->
  ruth_countries


# fill in vparty columns for the additional observations in vdem years
joined_vdata %>% 
  group_by(country_name) %>% 
  fill(gov_popul_mean,
       gov_popul_weighted,
       gov_seatshare,
       e_regiongeo,
       no_govparties,
       gov_popul_prime,
       rooduijn_government,
       rooduijn_government_senior) %>% 
  ungroup() -> 
  joined_vdata

# make table showing whether countries are included in datasets - needed for blog

joined_vdata %>%
  dplyr::select(country_name, rooduijn_government, rooduijn_government_senior, ruth_populism, president, gov_popul_prime, gov_popul_mean, gov_popul_weighted) %>% 
  group_by(country_name) %>%
  summarize_all(~ sum(!is.na(.x))) ->
  country_datasets

write_csv(country_datasets, "data/country_datasets.csv")
  
# was there a single party government
joined_vdata$singleparty_gov <- ifelse(
  joined_vdata$no_govparties == 1, 1, 0
)

# clean names
joined_vdata %>%
  rename(country = country_name) -> 
  joined_vdata

joined_vdata |> 
  pull(country) |> 
  unique() ->
  country_list

# CCPC ----

# This code was executed based on the original csv provided by ccp: 
# https://comparativeconstitutionsproject.org/download-data/
# Due to its size, we can not upload it in the repository - 
# please download this datset before running code

# DOWNLOAD FIRST (LINK ABOVE)
ccpc <- read.csv("data/ccpcnc_v3_small.csv")

ccpc |> 
  mutate(country = case_when(
    country == "Slovak Republic" ~ "Slovakia",
    TRUE ~ country
  )) -> 
  ccpc

ccpc |> 
  filter(country %in% country_list)->
  ccpc

## Count Rights Changes ----

# number of rights in constitution
# import names of rights in dataset

# use the codebook of ccp to create a list of rights in constitutions
rightsindex <- readtext("codebooks/rightsindex_ccp.pdf") %>%
  select(-doc_id) %>%
  mutate(text = str_remove_all(text, "ID\\b")) %>%
  mutate(text = str_extract_all(text, "[:upper:]{2,}\\d{0,}")) %>%
  unlist(text) %>%
  str_to_lower() %>%
  stringi::stri_replace_all_regex(c("equal\\B","artspec", "debtrght", "health", "indrght", "intprop\\B", "socsec1", "socsec(?=\\d)"), c("equalgr_", "artspec_1", "debtors", "healthr", "indpolgr_", "intprop_", "socsec", "finsup_"), vectorize_all = FALSE)%>%
  str_remove("appeal|envref|intprop$|solissuf") %>%
  stri_remove_empty()

# only keep ccp colums that refer to rights
ccpc %>%
  select(one_of(rightsindex), rghtapp) %>%
  select(-arms) %>% 
  mutate(across(.cols = everything(), ~ifelse(.==1, 1, 0))) ->
  ccpc_rights

# sum of all rights in constitution
ccpc_rights %>% 
  mutate(rights_sum = rowSums(across(everything()))) %>% 
  pull(rights_sum) ->
  ccpc$rights_sum

# add rights from an area to an index 
ccpc_rights %>%
  # criminal rights
  mutate(rights_crim = jury, vicright, excrim, prerel, habcorp, wolaw, rghtapp, corppun, dueproc, falseimp, fairtri, speedtri, presinoc, trilang, juvenile, doubjep, miranda, couns, debtors,
  # rule of law index
  rights_ruleolaw = rowSums(select(., "citren", 
                                          contains("equal"), 
                                          "infoacc", 
                                          "libel", 
                                          "freerel", "seprel", # religion
                                          "exprop", # enteignung möglich
                                          "remuner", "socsec", "standliv", contains("finsup"), "shelter", "healthr", # faire entlohnung, soziale sicherheit und lebensstandard, finanzielle unterstützung bestimmtr gruppen, recht auf wohnen, gesundheitsversorgung
                                          "jointrde", "strike", "occupate", "safework", "childwrk", #gewerkschaft und streik, eigene berufswahl, sichere arbeitsumgebung, verbot v kinderarbeit
                                          "testate", "transfer", "inherit", contains("intprop"), "proprght", # property and inheritance
                                          "busines", "freecomp", # establish busines, free market
                                          "conright", # consumer rights
                                          "scifree", "acfree",  # wissenschaftsfreiheit
                                          "achighed", # bildung
                                          "marriage", "fndfam", "matequal", "childpro", 2, # family
                                          "selfdet", # selfdetermination
                                          "life", 
                                          "slave", "torture", "cruelty",  
                                          "privacy",
                                          "freemove", 
                                          "opinion", "express", "petition", "censor", "press", "assem", "assoc",  # politische freiheit
                                          contains("intrght"), # international human rights declarations
                                          "devlpers", # persönliche freiheit
                                          "nomil", # verweigerung militär
                                          "asylum",
                                          "artspec_1")),
         # political rights
         rights_political = rowSums(select(., "infoacc", "scifree", "acfree", "opinion", "express", "petition", "censor", "press", "assem", "assoc", contains("intrght"), "asylum","jointrde", "strike")),
         # economic rights
         rights_econ = rowSums(select(., "busines", "freecomp", "conright", "testate", "transfer", "inherit", contains("intprop"), "proprght")),
         # individual rights
         rights_ind = freerel, citren, selfdet, life, privacy, freemove, devlpers,
         # social rights
         rights_social = rowSums(select(., "remuner", "socsec", "standliv", contains("finsup"), "shelter", "healthr", "safework", "childwrk", "achighed"))) %>% 
  select(contains("rights_")) ->
  ccpc_rights_sums

ccpc %>%
  cbind(ccpc_rights_sums) ->
  ccpc

ccpc %>%
  # remove all 99s and 0
  mutate(across(starts_with("con"), ~ str_remove_all(., "\\D")),
         hosterm = str_remove_all(hosterm, "\\D"),
         # 99 NA, 0 not specified, 100 constitutional monarchies
         hosterm = if_else(hosterm %in% c("99", "0", "100", ""), NA, hosterm),
         # 99 NA, 0 not specified
         conterm = if_else(conterm %in% c("99", "0", ""), NA, conterm),
         # 90 non-constitutional law, 96 other, 98 not specified, 99 NA
         hosterml = if_else(hosterml %in% c("90", "96", "98", "99", ""), NA, hosterml)) %>% 
  group_by(country) %>% 
  # numeric for calculations of rights
  mutate(hosterm = as.numeric(hosterm),
         across(starts_with("con"), ~ as.numeric(.)),
         across(starts_with("jud"), ~as.numeric(.)),
         across(starts_with("rights_"), 
                .fns = ~ . - lag(.),
                .names = "diff_{.col}"),
         across(c("conterm", "judind", "hosterm"),
                .fns = ~ . - lag(.),
                .names = "diff_{.col}"),
         # decree power head of state
         decree = ifelse(hosdec == 1 | hogdec == 1, 1, 0),
         # declare state of emergency by head of state
         emergency = ifelse(emdecl < 5, 1, 0),
         # removal of legislature by anyone but legislature themselved
         removal_leg = ifelse(legdiss < 5, 1, 0),
         # does any institution have veto power against the legislature
         veto = ifelse(legapp < 5, 1, 0),
         # does the head of state or government have the standing to challenge the constitutionality of legislation?
         review = ifelse(challeg_1 == 1 | challeg_2 == 1| challeg_3 == 1, 1, 0),
         # who can propose an amendment to the constitution?
         amendement = ifelse(amndprop_1 == 1 | amndprop_2 == 1| amndprop_3 == 1, 1, 0),
         # power of the executive
         executive = decree + emergency + removal_leg + veto + review + amendement,
         # number if institutions included in the selection of judges
         selection = ifelse(connom_1 + connom_2 + connom_3 + connom_4 + connom_5 + connom_7 + conap_1 + conap_2 + conap_3 + conap_4 + conap_5 + conap_7 > 1 | connom_6 + conap_6 > 0, 1, 0),
         # only one term for judges allowed
         appointment = ifelse(conlim == 1, 1, 0),
         # number of institutions that can propose the dismissal of judges
         removal = ifelse(jrempro_1 + jrempro_2 + jrempro_3 + jrempro_4 + jrempro_5 + jrempro_7 + jrempro_9 + jremap_1 + jremap_2 + jremap_3 + jremap_4 + jremap_5 + jremap_7 > 1 | jrempro_8 > 0 | jremap_6 + jrempro_6 > 0 | jrem == 2, 1, 0),
         # are judges protected against random dismissal (general dissatisfactin 1, incapacitation 5, other 96)
         removal_reason = ifelse(jremcon_1 != 1 & jremcon_5 != 1 & jremcon_96 != 1, 1, 0),
         # are judicial salaries protected from government interference
         salary = ifelse(judsal == 1, 1, 0),
         # rights of the judiciary
         judiciary = judind + selection + appointment + appointment + removal + removal_reason + salary,
         across(c("judiciary", "executive"),
                .fns = ~ . - lag(.),
                .names = "diff_{.col}"),
         # power change of executive
         diff_executive = executive - lag(executive),
         # increase in terms allowance for head of state
         term_change = case_when(
           hosterml != 1 & lag(hosterml) == 1 ~ 1,
           lag(hosterml) == 2 & hosterml != 3 & hosterml != 1 & hosterml != 2 ~ 1,
           lag(hosterml) == 3 & hosterml != 1 & hosterml != 3  ~ 1,
           lag(hosterml) == 4 & hosterml == 5 & hosterml != 4 ~ 1,
           is.na(hosterml) | is.na(lag(hosterml)) ~ NA_real_,
           TRUE ~ 0)) %>%
  ungroup() ->
  ccpc

# only relevant columns 
ccpc %>%
  as_tibble() %>%
  select(
    country, year, syst, evnt, evnttype, contains("rights_"), starts_with("diff_")
  ) ->
ccpc

glimpse(ccpc)

ccpc_vdem <- left_join(joined_vdata,
                       ccpc,
                       by=c("country","year"))

glimpse(ccpc_vdem)

# Save Data ----

## Special Cases ----

# Bolsonaro's party is coded as antipluralist but not populist
ccpc_vdem %>% 
  filter(country == "Brazil") 

## Save ----

saveRDS(ccpc_vdem, "data/ccpc_vdem.rds")

## Save Data Europe and Latin America ----

ccpc_vdem |> 
  filter(e_regiongeo %in% c(1:4, 17:18)) |> 
  arrange(country, year) |> 
  filter(country != "German Democratic Republic" & country != "Suriname") |> 
  mutate(date = ymd(year, truncated = 2L)) ->
  ccpc_vdem_eu_la

saveRDS(ccpc_vdem_eu_la, "data/ccpc_vdem_eu_la.rds")

## Loop Blog Data ----

ccpc_vdem_eu_la |> 
  group_by(country) |> 
  distinct(country) |> 
  mutate(dataset = "V-Party") ->
  vparty_countries

# start and end date of year for plotting
ccpc_vdem_eu_la |>
  mutate(year_start = ymd(year, truncated = 2L)-184,
         year_end = ymd(year, truncated = 2L)+181,
         year_interval = interval(year_start, year_end),
         year_start = if_else(year_start < ymd("1990-01-01"), ymd("1990-01-01"), year_start),
         year_end = if_else(year_end > ymd("2020-12-31"), ymd("2020-12-31"), year_end)) |> 
  select(-starts_with("v2j"), -starts_with("jud"), -starts_with("int")) |> 
  filter(year < 2022) ->
  ccpc_vdem_eu_la

write_csv(ccpc_vdem_eu_la, "data/blog/ccpc_vdem_eu_la_blog.csv")
write_csv(vparty_countries, "data/blog/country_overview.csv")
