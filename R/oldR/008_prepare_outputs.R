library(tidyverse)
library(openxlsx)
library(readr)
here::i_am("R/008_prepare_outputs.R")

dat <- readRDS(here::here("working", "partyonly_theta_iters.rds"))

### Add iter and rescale
dat <- dat |>
    mutate(iter = rep(1:8000, each = length(unique(dat$Name))))

## Need to flip econ around so left is on the left
dat <- dat |>
    mutate(value = value * -1)

dat <- dat |>
    group_by(iter, dimension) |>
    mutate(value.sc = value - min(value),
           value.sc = value.sc / max(value.sc),
           value.sc = value.sc * 100)

### dat$value.sc <- round(dat$value.sc)
### dat$value.sc <- as.integer(dat$value.sc)

### Add in the codes
canon <- read.csv(here::here("data", "canonical_representation.csv"))

canon <- canon |>
    dplyr::select(Name, Constituency, wikidata_id, ons_id)

dat <- left_join(dat, canon,
                 by = join_by(Name, Constituency))

write_csv(dat,
          file = here::here("outputs",
                                 "mpsleftright_full.csv.gz"))

### Now something for export, not the full Excel version
###
dat2 <- dat |>
    group_by(Name, wikidata_id, Constituency, ons_id, First.party, dimension) |>
    summarize(value = mean(value.sc),
              value_lo = quantile(value.sc, 0.05),
              value_hi = quantile(value.sc, 0.95)) |>
    pivot_wider(id_cols = c(Name, wikidata_id, Constituency, ons_id, First.party),
                names_from = c(dimension),
                values_from = c(value, value_lo, value_hi)) |>
    dplyr::select(Name, wikidata_id,
                  Constituency, ons_id,
                  Party = First.party,
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
    facet_wrap(~Party) +
    theme_bw()


write.csv(dat2,
          file = here::here("outputs",
                            "summary_statistics_file.csv"),
          row.names = FALSE)

### Add on Conservative party leadership endorsements
endorsements <- read.csv(here::here("data", "conservative_party_leadership_endorsements.csv"))

endorsements <- endorsements |>
    dplyr::select(wikidata_id, starts_with("endorsement_"))

endorsements <- left_join(endorsements,
                          dat2)

p1 <- ggplot(endorsements, aes(x = endorsement_mv,
                               y = econ_mean)) +
    geom_boxplot() +
    geom_point(position = "jitter") +
    theme_bw()

p2 <- ggplot(endorsements, aes(x = endorsement_mv,
                               y = cult_mean)) +
    geom_boxplot() +
    geom_point(position = "jitter") +
    theme_bw()

cowplot::plot_grid(p1, p2)

write.csv(endorsements,
          file = here::here("outputs",
                            "endorsement_file.csv"),
          row.names = FALSE)

## Precision-weighted regression
endorsements <- endorsements |>
    mutate(ci_width = econ_hi - econ_lo,
           sd = ci_width / (1.645 * 2),
           var = sd^2,
           prec = 1/var)

summary(lm(econ_mean ~ endorsement_mv, data = endorsements, weights = endorsements$prec))

## ### Export as Excel
## ### (1) Create the workbook
## wb <- createWorkbook()
## modifyBaseFont(wb, fontSize = 10, fontName = "Arial Narrow")

## ### Create an initial sheet
## addWorksheet(wb, sheetName = "Frontispiece", gridLines = FALSE)

## if (!file.exists(here::here("outputs", "rhul_logo.jpg"))) {
##     download.file("https://intranet.royalholloway.ac.uk/staff/assets/img/brand-toolkit/logo-small-london-cmyk.jpg",
##                   destfile = here::here("outputs", "rhul_logo.jpg"),
##                   method = "wget", extra = "--no-check-certificate")
## }


## insertImage(wb,
##             "Frontispiece",
##             here::here("outputs", "rhul_logo.jpg"),
##             width = 3, height = 1.5,
##             startRow = 1, startCol = 1)

## if (!file.exists(here::here("outputs", "survation_logo.png"))) {
##     download.file("https://cdn.survation.com/wp-content/theme/images/logo.png",
##                   destfile = here::here("outputs", "survation_logo.png"))
## }

## insertImage(wb,
##             "Frontispiece",
##             width = 3, height = .6,
##             here::here("outputs", "survation_logo.png"),
##             startRow = 2, startCol = 6)

## ### Add a table
## tab <- tribble(~Column, ~Explanation,
##                "Name", "MP's name in format FirstName LASTNAME",
##                "Person.ID", "TheyWorkForYou identifier code for the MP (hidden)",
##                "Constituency", "MP's constituency",
##                "PCON22CD", "ONS identifier code for the constituency (hidden)",
##                "Value", "Value from 0-100, where higher numbers mean MP is more right-wing",
##                "Could be as low as", "Lower end of a credible interval. There's a very high probability (97.5%) that the MP's true value is greater than this number",
##                "Could be as high as", "Upper end of a credible interval. There's a very high probability (97.5%) that the MP's true value is lower than this number",
##                "Rank", "The MP's rank, from 1st most-left wing to 609th most-left-wing",
##                "Could be as low as ", "Lower end of a credible interval. There's a very high probability (97.5%) that the MP's true rank is greater than this number",
##                "Could be as high as ", "Upper end of a credible interval. There's a very high probability (97.5%) that the MP's true rank is lower than this number") |>
##     as.data.frame()

## writeDataTable(wb, sheet = 1, tab, startRow = 10)

## setColWidths(wb, 1, 1, 
##              widths = 20)

## ### Create the data worksheet
## addWorksheet(wb, sheetName = "MP values", gridLines = FALSE)
## freezePane(wb, sheet = 2, firstRow = TRUE, firstCol = FALSE)

## writeDataTable(wb, sheet = 2, dat, startRow = 1)

## ### Hide column 2 and 4
## setColWidths(wb, 2, 2, 
##              widths = 8.43, hidden = TRUE)
## setColWidths(wb, 2, 4, 
##              widths = 8.43, hidden = TRUE)

## setColWidths(wb, 2, 1, 
##              widths = 24, hidden = FALSE)
## setColWidths(wb, 2, 3, 
##              widths = 24, hidden = FALSE)

## ### party column
## setColWidths(wb, 2, 5, 
##              widths = 18, hidden = FALSE)

## ### could be... columns
## setColWidths(wb, 2, 7, 
##              widths = 24, hidden = FALSE)
## setColWidths(wb, 2, 8, 
##              widths = 24, hidden = FALSE)

## ### could be... columns
## setColWidths(wb, 2, 10, 
##              widths = 24, hidden = FALSE)
## setColWidths(wb, 2, 11, 
##              widths = 24, hidden = FALSE)
## ### 
## ### Export it

## saveWorkbook(wb,
##              here::here("outputs", "mpsleftright_excel.xlsx"),
##              overwrite = TRUE)
