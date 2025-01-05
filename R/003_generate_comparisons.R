
### ########################################################################################################
### Load libraries
### ########################################################################################################
library(tidyverse)

### ########################################################################################################
### Read in the current MPs
### with ONS identifiers
### ########################################################################################################

hoc_url <- "https://researchbriefings.files.parliament.uk/documents/CBP-10009/HoC-GE2024-results-by-constituency.csv"
results_locn <- "data/HoC-GE2024-results-by-constituency.csv"
if (!file.exists(results_locn)) {
    download.file(hoc_url, results_locn)
}
mps <- read.csv(results_locn)

mps$DisplayName <- paste(mps$Member.first.name,
                         " ",
                         toupper(mps$Member.surname),
                         " (",
                         mps$Constituency.name,
                         ")",
                         sep = "")

starmer_text <- "Keir STARMER (Holborn and St Pancras)"
sunak_text <- "Rishi SUNAK (Richmond and Northallerton)"

### ########################################################################################################
### Generate combinations at the grouped UTLA level
### ########################################################################################################

# lu <- read.csv("Ward_to_Westminster_Parliamentary_Constituency_to_LAD_to_UTLA_(July_2024)_Lookup_in_UK.csv")
lu <- read.csv("data/final_lookup.csv")

lu <- lu |>
 distinct(geogcode, geogname,
          PCON24CD, PCON24NM,
          grouped_code, grouped_name,
          type)

# Remove Farnham and Bordon from the Hampshire UTLA
lu <- lu |>
 filter(!(PCON24NM == "Farnham and Bordon" & grouped_name == "Hampshire"))

set.seed(24926)

### For each grouped UTLA, get an exhaustive list of the pairwise comparisons between constituencies
grouped_utlas <- unique(lu$grouped_code)

grouped_utla <- grouped_utlas[25]

holder <- lapply(grouped_utlas, function(grouped_utla) {
 wmins_in_that_grouped_utla <- unique(lu$PCON24CD[which(lu$grouped_code == grouped_utla)])
 ### Look up the corresponding MPs
 mpos <- match(wmins_in_that_grouped_utla, mps$ONS.ID)
 mps_in_that_grouped_utla <- mps$DisplayName[mpos]
 mps_in_that_grouped_utla <- c(mps_in_that_grouped_utla,
                               starmer_text,
                               sunak_text)
 ### Add on two anchors
 combs <- expand.grid(A = mps_in_that_grouped_utla,
                      B = mps_in_that_grouped_utla)
 
 ### We don't want comparisons between identical things
 combs <- combs |>
  filter(A != B)
 
 ### Are they in alphabetical order?
 combs <- t(apply(combs, 1, sort))
 combs <- as.data.frame(combs)
 names(combs) <- c("A", "B")
 combs <- unique(combs)
 
 keep <- apply(combs, 1, function(x) all(sort(x) == x))
 combs <- combs[keep,]
 
 ### We want to keep *all* of the comparisons involving MPs other than Sunak and Starmer
 part_a <- combs |>
  filter(A != sunak_text) |>
  filter(A != starmer_text) |>
  filter(B != sunak_text) |>
  filter(B != starmer_text)
 
 ### None of the comparisons which involve *both* Sunak and Starmer
 part_b <- combs |>
  filter((A == sunak_text & B == starmer_text) |
          (B == sunak_text & A == starmer_text))
 
 ### and some of the comparisons which involve either one
 part_c <- combs |>
  filter((A == sunak_text & B != starmer_text) |
          (A == starmer_text & B != sunak_text) |
          (B == sunak_text & A != starmer_text) |
          (B == starmer_text & A != sunak_text))
 
 ### Let's filter this down to 2
 part_c <- part_c |>
  sample_n(2)
 
 combs <- full_join(part_a,
                    part_c,
                    by = join_by(A, B))
 
 ### Check if the grouped_utla is of type LAD using all()
 if (any(lu$type[which(lu$grouped_code == grouped_utla)] == "LAD")) {
   lads_in_that_grouped_utla <- unique(lu$geogcode[which(lu$grouped_code == grouped_utla)])
   out <- lapply(lads_in_that_grouped_utla, function(j) {
     combs |>
       mutate(geogcode = j)
   })
   out <- bind_rows(out)
   out <- out |>
     mutate(grouped_code = grouped_utla)
   return(out)
 } else {
   combs <- combs |> 
     mutate(grouped_code = grouped_utla)
   return(combs)
 }
})

holder <- bind_rows(holder)

holder <- left_join(holder,
                    lu |> dplyr::select(grouped_code,
                                        grouped_name,
                                        geogcode,
                                        geogname) |>
                     distinct(),
                    relationship = "many-to-one")

### Ensure unique combinations within each grouped_code/grouped_name
tmp <- holder |>
 group_by(geogcode) |>
 distinct(A, B, geogname, .keep_all = TRUE) |>
 ungroup()

check <- tmp |> 
  group_by(geogname) |> 
  summarise(n = n()) |>
  arrange(n) 

holder |> 
  select(A, B) |>
  pivot_longer(cols = c(A, B), names_to = "MP", values_to = "name") |> 
  select(-MP) |> 
  group_by(name) |>
  count() |> 
  arrange(desc(n))

write.csv(holder, file = "data/2024_comparisons.csv.gz")
