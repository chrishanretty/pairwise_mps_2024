### PURPOSE OF THIS CODE: to take in the raw data supplied by
### Survation and produce a tidy data frame

here::i_am("R/005_tidy_survey_data.R")
suppressPackageStartupMessages(library(tidyverse))

### Read in the data
dat <- read.csv(here::here("data/ind", "cllr_p1_20241024.csv"),
                check.names = FALSE)

### Shorten remaining names
names(dat) <- dplyr::recode(names(dat),
                            "Response ID" = "id",
                            "Respondent Session ID" = "sessionid",
                            "Time Started" = "starttime",
                            "Date Submitted" = "submittime",
                            "Status" = "status",
                            "Invite Custom Field 1" = "council",
                            "Invite Custom Field 2" = "ward",
                            "Invite Custom Field 3" = "party")

### Save information that we don't have in part 2
demogs <- dat |> 
        select(sessionid, council, ward, party)

### Reshape the data
dat <- dat |>
 pivot_longer(cols = starts_with("Here are two"),
              names_to = "comparison",
              values_to = "a_beats_b",
              names_repair = "unique")

# Would need a lookup for this:
# ### Remove Northern Irish respondents
# dat <- dat |>
#  filter(ctry != "Northern Ireland")

### Remove missing responses
dat <- dat |>
 filter(a_beats_b != "")

### Tidy and split the comparisons
dat <- dat |>
 mutate(comparison = sub("Here are two MPs:", "", comparison, fixed = TRUE),
        comparison = sub("Which of these MPs is more economically conservative?", "", comparison, fixed = TRUE))

dat <- dat |>
 separate_wider_delim(comparison, delim = "|",
                      names = c("MP_A", "MP_B")) |>
 mutate(MP_A = str_trim(MP_A),
        MP_B = str_trim(MP_B))

### Remove D/K responses
nrow(dat)
dat <- dat |>
 filter(a_beats_b != "I don't know these MPs well enough")
nrow(dat)

### The structure is right, but some of the column names had errors
### Make sure there are parentheses at the end
dat <- dat |>
 mutate(MP_A = sub("([A-Za-z])$", "\\1)", MP_A),
        MP_B = sub("([A-Za-z])$", "\\1)", MP_B))

### Make sure no awkward characters
dat <- dat |>
 mutate(MP_A = sub(" ", "", MP_A),
        MP_B = sub(" ", "", MP_B))

### Make sure double spaces are replaced with single spaces
dat <- dat |>
 mutate(MP_A = gsub(" +", " ", MP_A)) |>
 mutate(MP_B = gsub(" +", " ", MP_B))

### Turn the answers into ordered sequence
dat <- dat |>
 rowwise() |>
 mutate(dv = case_when(grepl(MP_A, a_beats_b, fixed = TRUE) &
                        grepl("much more", a_beats_b) ~ 1L,
                       grepl(MP_A, a_beats_b, fixed = TRUE) &
                        grepl("somewhat more", a_beats_b) ~ 2L,
                       grepl(MP_A, a_beats_b, fixed = TRUE) &
                        grepl("about the same", a_beats_b) ~ 3L,
                       grepl(MP_B, a_beats_b, fixed = TRUE) &
                        grepl("somewhat more", a_beats_b) ~ 4L,
                       grepl(MP_B, a_beats_b, fixed = TRUE) &
                        grepl("much more", a_beats_b) ~ 5L,
                       TRUE ~ NA_integer_)) |> 
        ungroup()


### Tidy respondent information

dat <- dat |>
 mutate(party = dplyr::recode(party,
                              "Independent councillor" = "Independent",
                              "Independent group" = "Independent",
                              "Conservative and Unionist" = "Conservative",
                              "Green Party" = "Green Party",
                              "Labour Party" = "Labour",
                              "Scottish National Party (SNP)" = "SNP",
                              "Independent / Other" = "Independent / Other",
                              "Liberal Democrats" = "Liberal Democrats",
                              .default = "Independent / Other"))

### Make sure the start and submission times are properly formatted
dat <- dat |>
 mutate(starttime = as.POSIXct(starttime, format = "%d/%m/%Y %H:%M"),
        submittime = as.POSIXct(submittime, format = "%d/%m/%Y %H:%M"))

### Turn some character variables to factors
dat <- dat |>
 mutate(status = factor(status),
        council = factor(council),
        party = factor(party))

### Remove missing dependent variables
dat <- dat |>
 filter(!is.na(dv))

### Unclass
dat <- dat |> as.data.frame()

### Select the cols we need
dat <- dat |> 
        select(sessionid, starttime, submittime, council, ward, party, MP_A, MP_B, a_beats_b, dv)

saveRDS(dat, file = here::here("working", "p1_tidy_data.rds"))

### Check MP counts
p1mps <- dat |> 
 select(MP_A, MP_B) |> 
 pivot_longer(cols = c(MP_A, MP_B), values_to = "MP", names_to = "remove") |> 
 select(-remove) |> 
 count(MP) |> 
 arrange(desc(n))

### Find out which MPs are missing
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

p1mps <- mps |> 
        anti_join(p1mps, by = c("DisplayName" = "MP"))

comps <- readRDS(here::here("working", "comparisons.rds")) |> 
        filter(A %in% p1mps$DisplayName | B %in% p1mps$DisplayName)


table(comps$geogname)

### Clean part 2
p2 <- read.csv(here::here("data/ind", "cllr_p2_20241024.csv"),
               check.names = FALSE)

names(p2) <- dplyr::recode(names(p2),
                            "Respondent Session ID" = "sessionid",
                            "Time Started" = "starttime",
                            "Date Submitted" = "submittime")

p2 <- p2 |>
        pivot_longer(cols = starts_with("Here are two"),
                     names_to = "comparison",
                     values_to = "a_beats_b",
                     names_repair = "unique")

### Remove missing responses and tidy
p2 <- p2 |>
        filter(a_beats_b != "") |> 
        mutate(comparison = sub("Here are two MPs:", "", comparison, fixed = TRUE),
               comparison = sub("Which of these MPs is more socially conservative?", "", comparison, fixed = TRUE)) |> 
        separate_wider_delim(comparison, delim = "|",
                             names = c("MP_A", "MP_B")) |>
        mutate(MP_A = str_trim(MP_A),
               MP_B = str_trim(MP_B)) |> 
        filter(a_beats_b != "I don't know these MPs well enough")

nrow(p2)

# Fix dates
p2 <- p2 |>
        mutate(starttime = as.POSIXct(starttime, format = "%d/%m/%Y %H:%M"),
               submittime = as.POSIXct(submittime, format = "%d/%m/%Y %H:%M"))

### Fix the errors with characters
p2 <- p2 |>
        mutate(MP_A = sub("([A-Za-z])$", "\\1)", MP_A),
               MP_B = sub("([A-Za-z])$", "\\1)", MP_B)) |> 
        mutate(MP_A = sub(" ", "", MP_A),
               MP_B = sub(" ", "", MP_B)) |> 
        mutate(MP_A = gsub(" +", " ", MP_A)) |>
        mutate(MP_B = gsub(" +", " ", MP_B))

### Turn the answers into ordered sequence
p2 <- p2 |>
        rowwise() |>
        mutate(dv = case_when(grepl(MP_A, a_beats_b, fixed = TRUE) &
                                      grepl("much more", a_beats_b) ~ 1L,
                              grepl(MP_A, a_beats_b, fixed = TRUE) &
                                      grepl("somewhat more", a_beats_b) ~ 2L,
                              grepl(MP_A, a_beats_b, fixed = TRUE) &
                                      grepl("about the same", a_beats_b) ~ 3L,
                              grepl(MP_B, a_beats_b, fixed = TRUE) &
                                      grepl("somewhat more", a_beats_b) ~ 4L,
                              grepl(MP_B, a_beats_b, fixed = TRUE) &
                                      grepl("much more", a_beats_b) ~ 5L,
                              TRUE ~ NA_integer_)) |> 
        ungroup()

### Add demogs
p2 <- p2 |>
        left_join(demogs, by = "sessionid")

sum(is.na(p2$party))

### Turn some character variables to factors
p2 <- p2 |>
        mutate(council = factor(council),
               party = factor(party))

### Remove missing dependent variables
p2 <- p2 |>
        filter(!is.na(dv))

### Unclass
p2 <- p2 |> as.data.frame()

### Select the cols we need
p2 <- p2 |> 
        select(sessionid, starttime, submittime, council, ward, party, MP_A, MP_B, a_beats_b, dv)

saveRDS(p2, file = here::here("working", "p2_tidy_data.rds"))

### Check MP counts
p2mps <- p2 |> 
        select(MP_A, MP_B) |> 
        pivot_longer(cols = c(MP_A, MP_B), values_to = "MP", names_to = "remove") |> 
        select(-remove) |> 
        count(MP) |> 
        arrange(desc(n))

### Find out which MPs are missing
p2mps <- mps |> 
        anti_join(p2mps, by = c("DisplayName" = "MP"))

comps2 <- readRDS(here::here("working", "comparisons.rds")) |> 
        filter(A %in% p1mps$DisplayName | B %in% p1mps$DisplayName)

table(comps2$geogname)
