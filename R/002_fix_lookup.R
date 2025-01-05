# Check how the Councils in the councillor database match with the councils in the pairwise data
library(tidyverse)
library(openxlsx)

# Load the lookup
lu <- read.csv("data/hacked_lookup.csv")

lu <- lu |>
 distinct(LAD24CD, LAD24NM,
          UTLA24CD, UTLA24NM,
          PCON24CD, PCON24NM,
          grouped_code, grouped_name)

n_distinct(lu$LAD24CD) # 361
n_distinct(lu$UTLA24NM) # 218 (?) 

# Load the councillor database
cl <- read.xlsx("councillors/councillor_database_202409.xlsx") |> 
 select(Council) |> 
 distinct()

n_distinct(cl$Council) # 382 total - This includes Northern Ireland, but so do the MPs

# Check the councils in the councillor database against the lookup
mis <- cl |>
 left_join(lu, by = c("Council" = "LAD24NM")) |>
 filter(is.na(LAD24CD))

# So we have 26 mismatches, let's check if those are in the UTLA24NM column
mis |>
 select(Council) |>
 left_join(lu, by = c("Council" = "UTLA24NM")) |>
 filter(is.na(UTLA24CD)) |> 
 pull(Council)

# Now we have 5 missing. These all correspond to LADs
# "Bristol"  = "Bristol, City of"
# "Durham" = "County Durham"
# "Edinburgh" = "City of Edinburgh"
# "Herefordshire" = "Herefordshire, County of"
# "Kingston upon Hull" = "Kingston upon Hull, City of"

# Let's fix these
cl <- cl |>
 mutate(Council = case_when(
   Council == "Bristol" ~ "Bristol, City of",
   Council == "Durham" ~ "County Durham",
   Council == "Edinburgh" ~ "City of Edinburgh",
   Council == "Herefordshire" ~ "Herefordshire, County of",
   Council == "Kingston upon Hull" ~ "Kingston upon Hull, City of",
   TRUE ~ Council
 ))

# Check the councils in the councillor database against the lookup
lads <- cl |>
 left_join(lu, by = c("Council" = "LAD24NM")) |>
 filter(!is.na(LAD24CD)) |> 
 mutate(type = "LAD")

utlas <- cl |>
 left_join(lu, by = c("Council" = "LAD24NM")) |>
 filter(is.na(LAD24CD)) |> 
 select(Council) |> 
 left_join(lu, by = c("Council" = "UTLA24NM")) |> 
 mutate(type = "UTLA")

# Let's now make a lookup table for those
lads <- lads |> 
 select(geogcode = LAD24CD, 
        geogname = Council, 
        grouped_code,
        grouped_name,
        PCON24CD,
        PCON24NM,
        type)

utlas <- utlas |> 
 select(geogcode = UTLA24CD, 
        geogname = Council, 
        grouped_code,
        grouped_name,
        PCON24CD,
        PCON24NM,
        type)

newlu <- bind_rows(lads, utlas)

# For consistency, let's add the old names from the councillor database
newlu <- newlu |> 
 left_join(cl |> 
            mutate(database_name = case_when(
              Council == "Bristol, City of" ~ "Bristol",
              Council == "County Durham" ~ "Durham",
              Council == "City of Edinburgh" ~ "Edinburgh",
              Council == "Herefordshire, County of" ~ "Herefordshire",
              Council == "Kingston upon Hull, City of" ~ "Kingston upon Hull",
              TRUE ~ Council
            )), by = c("geogname" = "Council")) 

# Now where we have a + present in the grouped_name, we should replace the geogname with the grouped_name (and same for code)
# newlu <- newlu |> 
#  mutate(grouped_name = case_when(
#    !grepl("\\+", grouped_name) ~ geogname,
#    TRUE ~ grouped_name
#  )) |> 
#  mutate(grouped_code = case_when(
#    !grepl("\\+", grouped_name) ~ geogcode,
#    TRUE ~ grouped_name
#  ))


# Save the lookup
write.csv(newlu, "data/final_lookup.csv", row.names = FALSE)



