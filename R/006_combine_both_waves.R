library(tidyverse)
library(rstan)
library(here)

here::i_am("R/006_combine_both_waves.R")

### Get in wave 1 data
### In wave 1, MP A is much more left-wing translates to a dv of 1
### In wave 2, MP A is much more economically conservative translates to a dv of 1
### So we need to flip the scale for wave 1
w1 <- readRDS(here::here("data",
                         "tidied_data_wave1.rds"))

w1 <- w1 |>
    mutate(dv = case_when(dv == 5 ~ 1,
                          dv == 4 ~ 2,
                          dv == 3 ~ 3,
                          dv == 2 ~ 4,
                          dv == 1 ~ 5))

### Add on the wikidata identifier
w1_aux <- read.csv(here::here("data",
                              "previous_exercise_with_wikidata_idents.csv"))

w1 <- left_join(w1,
                w1_aux |> dplyr::select(Person.ID, wikidata),
                by = join_by(Person.ID.A == Person.ID))
                
w1 <- left_join(w1,
                w1_aux |> dplyr::select(Person.ID, wikidata),
                by = join_by(Person.ID.B == Person.ID),
                suffix = c(".A", ".B"))

### Get in wave 2 data
### Read in the data
p1 <- readRDS(here::here("working", "p1_tidy_data.rds"))
p2 <- readRDS(here::here("working", "p2_tidy_data.rds"))
p1$dim <- "econ"
p2$dim <- "cult"

w2 <- bind_rows(p1, p2)

### Add on the wikidata identifier
w2_aux <- read.csv(here::here("data", "canonical_representation.csv")) |>
    mutate(DisplayName = paste0(Name, " (", Constituency, ")"))

w2 <- left_join(w2,
                w2_aux |> dplyr::select(DisplayName, wikidata = wikidata_id, Constituency),
                by = join_by(MP_A == DisplayName))

w2 <- left_join(w2,
                w2_aux |> dplyr::select(DisplayName, wikidata = wikidata_id, Constituency),
                by = join_by(MP_B == DisplayName),
                 suffix = c(".A", ".B"))

ni_seats <- c("Belfast East", "Belfast North",
              "Belfast South and Mid Down",
              "Belfast West",
              "East Antrim",
              "East Londonderry",
              "Fermanagh and South Tyrone",
              "Foyle",
              "Lagan Valley",
              "Mid Ulster",
              "Newry and Armagh",
              "North Antrim",
              "North Down",
              "South Antrim",
              "South Down",
              "Strangford",
              "Upper Bann",
              "West Tyrone")

w2 <- w2 |>
    filter(!is.element(Constituency.A, ni_seats)) |>
    filter(!is.element(Constituency.B, ni_seats)) |>
    dplyr::select(-Constituency.A, -Constituency.B)
              
### Combine the two data-sets
w1$wave <- 1
w2$wave <- 2
w1$dim <- "econ"

dat <- full_join(w1 |> dplyr::select(wikidata.A, wikidata.B,
                                     dv, dim, wave),
                 w2 |> dplyr::select(wikidata.A, wikidata.B,
                                     dv, dim, wave),
                 by = join_by(wikidata.A, wikidata.B, dv, dim, wave))

mp_levels <- c(as.character(dat$wikidata.A),
               as.character(dat$wikidata.B))
mp_levels <- sort(unique(mp_levels))


dat <- dat |>
    mutate(wikidata.A = factor(wikidata.A,
                               levels = mp_levels,
                               ordered = TRUE),
           wikidata.B = factor(wikidata.B,
                               levels = mp_levels,
                               ordered = TRUE),
           MP_A.num = as.numeric(wikidata.A),
           MP_B.num = as.numeric(wikidata.B),
           dim.num = ifelse(dim == "econ", 1, 2))

saveRDS(dat, file = here::here("working",
                               "combined_data.rds"))

### What is the corresponding party for each MP?
aux <- full_join(w1_aux, w2_aux,
                 by = join_by(Name, Constituency, Party)) |>
    mutate(Party = dplyr::recode(Party,
                                 "Conservative" = "Con",
                                 "Labour" = "Lab",
                                 "Liberal Democrat" = "LD",
                                 "Plaid Cymru" = "PC",
                                 "Scottish National Party" = "SNP")) |>
    mutate(wikidata = ifelse(!is.na(wikidata),
                             wikidata,
                             wikidata_id)) |>
    dplyr::select(wikidata, Party) |>
    distinct()

mp <- match(mp_levels, aux$wikidata)
if (any(mp == 0)) {
    stop("Match problem")
}
if (any(is.na(mp))) {
    stop("Match problem")
}

mp_party <- aux$Party[mp]

