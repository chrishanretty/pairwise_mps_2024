# Generate the questions for Alchemer
library(tidyverse)
library(officer)

# Part I - left-right dimension

comps <- read.csv("data/2024_comparisons.csv.gz") |> 
 select(-X) |> 
 arrange(geogname)


# Open a file connection
outfile <- "working/economic_questions.txt"
file_conn <- file(outfile, "w")

 for (geogcode in unique(comps$geogcode)) {
  # Add a page name
  geogname <- comps$geogname[which(comps$geogcode == geogcode)[1]]
  writeLines(paste0("::NewPage:: ", geogname), file_conn)
  writeLines("\n", file_conn)
  geogdat <- comps %>% filter(geogcode == !!geogcode)
  
  for (i in 1:nrow(geogdat)) {
    writeLines(paste0("Here are two MPs:<br><br>",
                      geogdat$A[i], "  |  ", geogdat$B[i], "<br><br>",
                      "Which of these MPs is more <b>economically conservative</b>?<br><br>"), file_conn)
    
    writeLines(paste0("() ", geogdat$A[i], " is much more economically conservative"), file_conn)
    writeLines(paste0("() ", geogdat$A[i], " is somewhat more economically conservative"), file_conn)
    writeLines(paste0("() ", geogdat$A[i], " and ", geogdat$B[i], " are about the same"), file_conn)
    writeLines(paste0("() ", geogdat$B[i], " is somewhat more economically conservative"), file_conn)
    writeLines(paste0("() ", geogdat$B[i], " is much more economically conservative"), file_conn)
    writeLines("() I don't know these MPs well enough", file_conn)
    
    writeLines("\n", file_conn)
  }
 }
 

close(file_conn)



# Open a file connection
outfile <- "working/social_questions.txt"
file_conn <- file(outfile, "w")

for (geogcode in unique(comps$geogcode)) {
 # Add a page name
 geogname <- comps$geogname[which(comps$geogcode == geogcode)[1]]
 writeLines(paste0("::NewPage:: ", geogname), file_conn)
 writeLines("\n", file_conn)
 geogdat <- comps %>% filter(geogcode == !!geogcode)
 
 for (i in 1:nrow(geogdat)) {
  writeLines(paste0("Here are two MPs:<br><br>",
                    geogdat$A[i], "  |  ", geogdat$B[i], "<br><br>",
                    "Which of these MPs is more <b>socially conservative</b>?<br><br>"), file_conn)
  
  writeLines(paste0("() ", geogdat$A[i], " is much more socially conservative"), file_conn)
  writeLines(paste0("() ", geogdat$A[i], " is somewhat more socially conservative"), file_conn)
  writeLines(paste0("() ", geogdat$A[i], " and ", geogdat$B[i], " are about the same"), file_conn)
  writeLines(paste0("() ", geogdat$B[i], " is somewhat more socially conservative"), file_conn)
  writeLines(paste0("() ", geogdat$B[i], " is much more socially conservative"), file_conn)
  writeLines("() I don't know these MPs well enough", file_conn)
  
  writeLines("\n", file_conn)
 }
}

close(file_conn)

comps |> 
 select(A, B) |>
 pivot_longer(cols = c(A, B), names_to = "MP", values_to = "name") |> 
 select(-MP) |> 
 group_by(name) |>
 count() |> 
 arrange(desc(n))

comps |> 
 select(A, B) |>
 pivot_longer(cols = c(A, B), names_to = "MP", values_to = "name") |> 
 select(-MP) |> 
 group_by(name) |>
 count() |> 
 arrange(n)
