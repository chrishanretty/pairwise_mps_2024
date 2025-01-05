library(tidyverse)
library(openxlsx)
library(readr)
library(glue)

here::i_am("R/008_prepare_both_waves_outputs.R")

dat <- readRDS(here::here("working", "bothwaves_thetas.rds"))

## Need to flip econ around so left is on the left
dat <- dat |>
    mutate(value = value * -1)

minmax_sc <- function(x) {
    x <- x - min(x, na.rm = TRUE)
    x <- x / max(x, na.rm = TRUE)
    return(x)
}

### Save some summary statistics
theta_floor <- min(dat$value)
theta_ceiling <- max(dat$value) - theta_floor
save(theta_floor,
     theta_ceiling,
     file = here::here("working",
                       "minmax_scaling.rds"))

dat <- dat |>
    mutate(value.sc = minmax_sc(value),
           value.sc = value.sc * 100)

### dat$value.sc <- round(dat$value.sc)
### dat$value.sc <- as.integer(dat$value.sc)

w1_aux <- read.csv(here::here("data", "previous_exercise_with_wikidata_idents.csv")) |>
    dplyr::select(DisplayName = Name,
                  wikidata,
                  Party,
                  PCON22NM = Constituency,
                  PCON22CD = PCON22CD) 

w2_aux <- read.csv(here::here("data", "canonical_representation.csv")) |>
    dplyr::select(DisplayName = Name,
                  wikidata = wikidata_id,
                  Party,
                  PCON24NM = Constituency,
                  PCON24CD = ons_id)

### Amend some names
w1_aux <- w1_aux |>
    mutate(DisplayName = dplyr::recode(DisplayName,
                                       "Liz SAVILLE-ROBERTS" = "Liz SAVILLE ROBERTS",
                                       "Naseem SHAH" = "Naz SHAH",
                                       "Nusrat GHANI" = "Nus GHANI",
                                       "Thomas TUGENDHAT" = "Tom TUGENDHAT",
                                       "Edward DAVEY" = "Ed DAVEY",
                                       "Preet Kaur GILL" = "Preet GILL",
                                       "Andrew SLAUGHTER" = "Andy SLAUGHTER",
                                       "John Martin MCDONNELL" = "John MCDONNELL",
                                       "Diana R. JOHNSON" = "Diana JOHNSON"))

aux <- full_join(w1_aux,
                 w2_aux,
                 by = join_by(DisplayName, wikidata),
                 suffix = c(".22", ".24"))


dat <- left_join(dat, aux,
                 by = join_by(wikidata))

dat <- dat |>
    dplyr::select(iter, dimension,
                  wikidata, DisplayName,
                  value, value.sc,
                  Party.22, PCON22CD, PCON22NM,
                  Party.24, PCON24CD, PCON24NM)

write_csv(dat,
          file = here::here("outputs",
                                 "mpsleftright_full.csv.gz"))

### Now something for Shiny
### This should have the two dimensions in separate columns
### and a smaller number of iterations
### it should also be restricted to the 2024 MPs
shiny <- dat |>
    filter(as.integer(iter) <= 500) |>
    filter(!is.na(Party.24)) |>
    filter(Party.24 %in% c("Con", "Green", "Ind", "Lab", "LD", "PC", "RUK", "SNP")) |>
    dplyr::select(iter, DisplayName, value.sc, dimension, iter, Constituency = PCON24NM, Party = Party.24) |>
    mutate(dimension = case_when(dimension == 2 ~ "Cultural",
                                 dimension == 1 ~ "Economic",
                                 TRUE ~ NA_character_)) |>
    pivot_wider(id_cols = c(iter, DisplayName, Constituency, Party),
                values_from = value.sc,
                names_from = dimension) |>
    arrange(Constituency) |>
    mutate(iter = as.integer(as.character(iter)),
           DisplayName = factor(DisplayName),
           Constituency = factor(Constituency),
           Party = factor(Party)) 


saveRDS(shiny,
        file = here::here("shiny_pairwise_mps",
                          "shiny_theta_iters.rds"))

### Now something for export, not the full Excel version

###
dat2 <- dat |>
    group_by(DisplayName, wikidata, PCON22CD, PCON22NM, Party.22, PCON24CD, PCON24NM, Party.24, dimension) |>
    summarize(value = mean(value.sc),
              value_lo = quantile(value.sc, 0.05),
              value_hi = quantile(value.sc, 0.95)) |>
    pivot_wider(id_cols = c(DisplayName, wikidata, PCON22CD, PCON22NM, Party.22, PCON24CD, PCON24NM, Party.24),
                names_from = c(dimension),
                values_from = c(value, value_lo, value_hi)) |>
    dplyr::select(DisplayName, wikidata,
                  PCON22CD, PCON22NM, Party.22,
                  PCON24CD, PCON24NM, Party.24, 
                  econ_mean = value_1,
                  econ_lo = value_lo_1,
                  econ_hi = value_hi_1,
                  cult_mean = value_2,
                  cult_lo = value_lo_2,
                  cult_hi = value_hi_2)

ggplot(dat2,
       aes(x = econ_mean, xmin = econ_lo, xmax = econ_hi,
           y = cult_mean, ymin = cult_lo, ymax = cult_hi)) +
    geom_pointrange() +
    facet_wrap(~Party.24) +
    theme_bw()

write.csv(dat2,
          file = here::here("outputs",
                            "summary_statistics_file.csv"),
          row.names = FALSE)

### Get positions in 2023
dat <- readRDS(here::here("working", "bothwaves_thetas.rds"))

## Need to flip econ around so left is on the left
dat <- dat |>
    mutate(value = value * -1)

minmax_sc <- function(x) {
    x <- x - min(x, na.rm = TRUE)
    x <- x / max(x, na.rm = TRUE)
    return(x)
}

increments <- readRDS(file = here::here("working", "increments.rds")) |>
    dplyr::select(iter, wikidata,
                  incr = value)

dat <- dat |>
    left_join(increments,
              by = join_by(iter, wikidata))

dat <- dat |>
    ungroup() |>
    filter(dimension == 1) |>
    mutate(old_value = value + incr,
           value.sc = minmax_sc(value),
           value.sc = value.sc * 100,
           old_value.sc = minmax_sc(old_value),
           old_value.sc = old_value.sc * 100)

saveRDS(dat, file = here::here("working", "overtime_comparison.rds"))

