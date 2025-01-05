library(tidyverse)
library(sf)
library(spdep)
### Which UTLAs contain a single constituency?

lu <- read.csv("data/Ward_to_Westminster_Parliamentary_Constituency_to_LAD_to_UTLA_(July_2024)_Lookup_in_UK.csv")

wmins_per_utla <- lu |>
    group_by(UTLA24CD, UTLA24NM) |>
    summarize(n_wmin = length(unique(PCON24CD)))

problems <- wmins_per_utla |>
    filter(n_wmin == 1)

##    UTLA24CD  UTLA24NM            n_wmin
##    <chr>     <chr>                <int>
##  1 E06000001 Hartlepool               1
##  2 E06000017 Rutland                  1
##  3 E06000053 Isles of Scilly          1
##  4 E09000001 City of London           1
##  5 S12000011 East Renfrewshire        1
##  6 S12000013 Na h-Eileanan Siar       1
##  7 S12000018 Inverclyde               1
##  8 S12000019 Midlothian               1
##  9 S12000023 Orkney Islands           1
## 10 S12000027 Shetland Islands         1
## 11 S12000030 Stirling                 1
## 12 S12000035 Argyll and Bute          1
## 13 S12000039 West Dunbartonshire      1
## 14 W06000001 Isle of Anglesey         1
## 15 W06000008 Ceredigion               1
## 16 W06000019 Blaenau Gwent            1
## 17 W06000020 Torfaen                  1
## 18 W06000021 Monmouthshire            1
## 19 W06000024 Merthyr Tydfil           1

### For each of these UTLAs, find contiguous UTLAs
### work out whch has the fewest PCONs contained within
### 
map <- st_read("data/Upper_Tier_Local_Authorities_December_2022_Boundaries_UK_BFE_1067518925926936612",
        "UTLA_MCTY_DEC_2022_UK_BFE")

st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
queen_neighbours <- st_queen(map)
res <- lapply(seq_len(length(queen_neighbours)), function(i) {
    nb <- queen_neighbours[[i]]
    if (length(nb) < 1) {
        return(NULL)
    } else {
        return(data.frame(UTLA22CD = map$UTLA22CD[i],
                          Neighbour = map$UTLA22CD[nb]))
    }  
})
res <- bind_rows(res)
res <- res |>
    filter(UTLA22CD %in% problems$UTLA24CD)

res <- left_join(res,
                 wmins_per_utla,
                 by = join_by(Neighbour == UTLA24CD))
res <- res |>
    group_by(UTLA22CD) |>
    arrange(n_wmin) |>
    slice(1)

### And on the codes
res <- res |>
    left_join(wmins_per_utla |> dplyr::select(UTLA24CD, Name = UTLA24NM),
              by = join_by(UTLA22CD == UTLA24CD))
res <- res |>
    dplyr::select(UTLA22CD, Name, Neighbour, UTLA24NM)

### For each neighbour, concatenate the names involved
tmp <- res |>
    group_by(Neighbour) |>
    mutate(new_label = str_c(unique(c(UTLA22CD, Neighbour)), collapse = "+"),
              new_name = str_c(unique(c(Name, UTLA24NM)), collapse = "+"))

# We need a lookup between the original UTLA code and the new one for both UTLAs
tmp <- tmp |> 
    select(UTLA22CD, Neighbour, new_label, new_name) |>
    pivot_longer(cols = c(UTLA22CD, Neighbour),
                 names_to = "type",
                 values_to = "geogcode") |> 
    select(geogcode, new_label, new_name) |> 
    distinct(geogcode, new_label, new_name) |> 
    # We have Blaenau Gwent+Torfaen in a 2 and 3 combination, so we'll only keep the 3
    filter(new_label != "W06000019+W06000020") |> 
    # Some manual adjustment to remove dupes and create a 4x Scottish UTLA
    mutate(new_label = case_when(
                geogcode %in% c("S12000018", "S12000030", "S12000035", "S12000039") ~ "S12000018+S12000030+S12000035+S12000039",
                TRUE ~ new_label),
           new_name = case_when(
               geogcode %in% c("S12000018", "S12000030", "S12000035", "S12000039") ~ "Inverclyde+Stirling+Argyll and Bute+West Dunbartonshire",
               TRUE ~ new_name)) |> 
    distinct(geogcode, new_label, new_name) 

### Read in the original look up and overwrite
lu <- left_join(lu, tmp,
                by = join_by(UTLA24CD == geogcode)) |>
    mutate(grouped_code = ifelse(is.na(new_label),
                             as.character(UTLA24CD),
                             as.character(new_label)),
           grouped_name = ifelse(is.na(new_name),
                                 as.character(UTLA24NM),
                                 as.character(new_name)))

write.csv(lu,
          file = "data/hacked_lookup.csv",
          row.names = FALSE)
