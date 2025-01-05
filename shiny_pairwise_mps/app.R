library(shiny)
library(tidyverse)
library(ggdist)
library(scales)
library(colorspace)

get_wep <- function(x) {
    wep <- case_when(x > .99 ~ "virtually certain to be to the ",
                     x > .90 ~ "very likely to be to the ",
                     x > .66 ~ "likely to be to the ",
                     x >= .50 ~ "more likely than not to be to the ",
                     x >= .45 ~ "only barely more likely than not to be to the ",
                     TRUE ~ " error! "
                     )
    return(wep)
}
        
party2col <- function(x) {
    dplyr::recode(x,
                  "Conservative" = "#0087DC",
                  "Plaid Cymru" = "#005B54",
                  "Scottish National" = "#FDF38E",
                  "Independents" = "#666666",
                  "Labour" = "#E4003B",
                  "Green" = "#02A95B",
                  "Liberal Democrats" = "#FAA61A",
                  "Reform UK" = "#12B6CF",
                  .default = "#DDDDDD")
}

### Read in the data we'll need
### We'll need to think about how to make this maximally efficient

dat <- readRDS("shiny_theta_iters.rds")


dat <- dat |>
    mutate(DisplayName = paste0(DisplayName, " (", Constituency, ")")) |>
    dplyr::select(DisplayName, Party, Economic, Cultural) |>
    ungroup()

### Change parties so they look nice
dat <- dat |>
    mutate(Party = dplyr::recode(Party,
                                 "Con" = "Conservative",
                                 "PC" = "Plaid Cymru",
                                 "SNP" = "Scottish National",
                                 "Green" = "Green",
                                 "Ind" = "Independents",
                                 "Lab" = "Labour",
                                 "LD" = "Liberal Democrats",
                                 "RUK" = "Reform UK"),
           party_col = party2col(Party),
           party_fill = colorspace::lighten(party_col, 0.3))


### Also read in cutoffs
cutoffs <- readRDS("mean_cutoffs.rds")

### ################################################################
### User interface
### ################################################################

ui <- fluidPage(
    theme = bslib::bs_theme(
                       ## to customize if necessary
                       
                   ),
    tags$link(rel = "stylesheet", type="text/css", href="https://www.survation.com/wp-content/themes/SurvationFromCDN/css/bootstrap.css?ver=4.9.15"),
  # App title ----
    titlePanel("Survation/Royal Holloway estimates of MP positions"),
    tabsetPanel(type = "tabs",
              tabPanel("Compare two MPs", {
                  sidebarLayout(
                      sidebarPanel(
                          selectizeInput(inputId = "mp_a",
                                         label = "First-named MP:",
                                         choices = unique(dat$DisplayName),
                                         options = list(placeholder = 'type an MP name')),
                          selectizeInput(inputId = "mp_b",
                                         label = "Second-named MP:",
                                         choices = unique(dat$DisplayName),
                                         options = list(placeholder = 'type an MP name')),
                          actionButton("compare", "Compare!")
                      ),
                      mainPanel(
                          htmlOutput("text_descr", inline = TRUE),
                          plotOutput("pairwise_plot")
                      )
                      )
              }),
              tabPanel("Compare an MP with their party", {
                  sidebarLayout(
                      sidebarPanel(
                          selectizeInput(inputId = "mp_party",
                                         label = "MP:",
                                         choices = unique(dat$DisplayName),
                                         options = list(placeholder = 'type an MP name')),
                          actionButton("compare_within_party", "Compare!")
                      ),
                      mainPanel(
                          htmlOutput("party_comparison", inline = TRUE),
                          plotOutput("party_comparison_plot")
                          
                      )
                  )
              }),
              tabPanel("Compare an MP with the whole Commons", {
                  sidebarLayout(
                      sidebarPanel(
                          selectizeInput(inputId = "mp_sole",
                                         label = "MP:",
                                         choices = unique(dat$DisplayName),
                                         options = list(placeholder = 'type an MP name')),
                          actionButton("compare_sole", "Compare!")
                      ),
                      mainPanel(
                          htmlOutput("sole_comparison", inline = TRUE),
                          plotOutput("sole_comparison_plot")
                      )
                  )
              }),
              tabPanel("FAQs", {
                  sidebarLayout(
                      sidebarPanel(
                      ),
                      mainPanel(
                          h3("Frequently asked questions"),
                          ###
                          h4("What are these figures based on?"),
                          p("These figures are based on two large surveys of local councillors. The first survey was done between August and September 2023; the second survey was done in October 2024. 1486 councillors responded to the first survey; 1027 to the second survey. These councillors were asked to compare MPs in their local area two at a time. We asked them to pick the MP that was 'more left-wing' (first survey wording) or 'more economically conservative' (second survey wording). The figures here are based on those pairwise comparisons. In total, we model 13,680 pairwise comparisons, or just over fourteen comparisons for each of the 953 MPs in the data. "),
                          ##
                          h4("What do 'left' and 'right' mean here?"),
                          p("We asked respondents to think about left and right in economic terms. When, in the second survey, we asked respondents to compare MPs on their positions on social issues, the figures we got were almost identical to the figures for MPs' positions on the economic left-right dimension. "),
                          ##
                          h4("What does 0 or 100 mean? Does a score of 50 mean someone is centrist?"),
                          p("Scores of zero and one hundred are the most left-wing and most right-wing we think it's possible for an MP in the 2019-2024 and 2024 - parliaments to be. These figures are relative figures. If we somehow added a communist or fascist MP to the survey, the numbers would have to be recalculated. A score of 50 just means that someone is roughly in the middle of the distribution of MPs for these two parliaments. It doesn't mean that they are centrist in any meaningful sense. "),
                          ##
                          h4("Where can I read more about the methodology?"),
                          p("You can find a full description of the methodology we used in this ",
                            a(href= 'https://github.com/chrishanretty/pairwise_mps_2024/blob/main/article/article.pdf', 'working paper'),
                            ". If you've ever heard of a Bradley-Terry model, or if you know about Elo scores in chess, you should be able to understand the key idea behind pairwise comparisons, even if the implementation is a bit more complicated than either of those models. "),
                          ## 
                          h4("Why are there no Northern Irish politicians?"),
                          p("We did survey councillors in Northern Ireland, but when we try to model their responses it's hard to fit the responses on the same scale that we use for MPs in England, Scotland and Wales. "),
                          ## 
                          h4("Why are some politicians missing?"),
                          p("Some politicians are not present in this data either because there were no councillors in their area who answered the survey, or because they did not feature in the (randomly-generated) comparisons between MPs in the area, or because all councillors who did see an MP in a comparison said that they didn't know whether that person was more or less left-wing or economically conservative than the other MP. "),
                          ## 
                          h4("I know MPs really well -- can I answer questions about them?"),
                          p("At the moment we survey councillors because we believe that they are particularly knowledgeable about MPs in their local area, and because we can get access to a large number of councillors through Survation's panel of councillors. At this point we don't want to recruit and manage a separate group of political experts"),
                          ## 
                          h4("I'm an MP -- can I have my figure removed from this website?"),
                          p("These estimates are a good faith attempt to describe the political positions of elected representatives. We think there's value in their publication. We won't be removing any figures, but we would be happy to meet or call you to discuss the findings or the research methodology in more depth. "),
                          ## 
                          h4("Can I use this data in my work?"),
                          p("You can use this data on the condition that you provide appropriate credit. Here, 'appropriate credit' means mentioning Survation, UK in a Changing Europe and Royal Holloway, University of London by name. We appreciate links, but understand that this may not always be possible. If you're using this data for academic work, you should cite the ",
                            a(href= 'https://github.com/chrishanretty/pairwise_mps_2024/blob/main/article/article.pdf', 'working paper')),
                          ##
                          h4("Can I download the data?"),
                          p("Yes: you can either download "),
                          tags$ul(
                                   tags$li(tags$a("this Excel spreadsheet", href = "https://github.com/chrishanretty/pairwise_mps_2024/blob/main/outputs/mpsleftright_excel.xlsx"),
                                           ", which gives MP names, each MPs' party, their average score, their average rank, and 'low' and 'high' scores and ranks (explained in the spreadsheet). The scores have been rescaled so that the lowest (most left-wing) score is 0, and the highest (most right-wing) score is 100. "),
                                   tags$li(tags$a("this comma separated values file", href = "https://github.com/chrishanretty/pairwise_mps_2024/blob/main/outputs/mpsleftright_full.csv.gz"),
                                           ", which includes the full output of the measurement model, with ONS constituency identifiers and Wikidata person codes")
                               ),
                          p("If you don't know which file to use, use the Excel spreadsheet. "),
                          h4("Where can I find the results for the 2019-2024 parliament?"),
                          p("These are available, like the release above, in...  "),
                          tags$ul(
                                   tags$li(tags$a("this Excel spreadsheet", href = "https://github.com/chrishanretty/pairwise_mps/blob/main/outputs/mpsleftright_excel.xlsx"),
                                           ", which gives MP names, each MPs' party, their average score, their average rank, and 'low' and 'high' scores and ranks (explained in the spreadsheet). The scores have been rescaled so that the lowest (most left-wing) score is 0, and the highest (most right-wing) score is 100. "),
                                   tags$li(tags$a("this comma separated values file", href = "https://github.com/chrishanretty/pairwise_mps/blob/main/outputs/mpsleftright_full.csv.gz"),
                                           ", which includes the full output of the measurement model, with ONS constituency identifiers and TheyWorkForYou person codes")
                               ),
                          ## 
                          h4("Who funded this research?"),
                          p("The first survey was the result of an ongoing partnership between Survation and Royal Holloway. The second survey was funded by ",
                            a(href = "https://ukandeu.ac.uk/", "UK in a Changing Europe"))
                      )
                  )   
              })
              ),

    div(style = "position: fixed; right: 2%; bottom: 2%;",
        HTML("&nbsp;"),
        img(src='https://www.royalholloway.ac.uk/images/logo-og-default.jpg', align = "right", width = "150"),
        HTML("&nbsp;"),
        span(style = "width: 2em; "),
        img(src='https://cdn.survation.com/wp-content/theme/images/logo.png', align = "right", width = "150", style = "margin-top: 26px; margin-right: 26px; "),
        HTML("&nbsp;")
        )
    
)


### ################################################################
### Server
### ################################################################

server <- function(input, output) {
    require(stringr)

    ### Declare some reactive variables
    theta_a <- eventReactive(input$compare, {
        dat |> filter(as.character(DisplayName) == input$mp_a)
    })
    theta_b <- eventReactive(input$compare, {
        dat |> filter(as.character(DisplayName) == input$mp_b)
    })

    theta_mp_party <- eventReactive(input$compare_within_party, {
        pt_a <- dat |> filter(as.character(DisplayName) == input$mp_party) |>
            mutate(selected = 1)
        pt_b <- dat |> filter(Party == pt_a$Party[1]) |>
            mutate(selected = 0)
        rbind(pt_a, pt_b)
    })

    theta_mp_sole <- eventReactive(input$compare_sole, {
        pt_a <- dat |> filter(DisplayName == input$mp_sole) |>
            mutate(selected = 1)
        pt_b <- dat |> filter(DisplayName != input$mp_sole) |>
            mutate(selected = 0)
        rbind(pt_a, pt_b)
    })
    ### Output elements
    output$text_descr <- renderUI({
        bar_a <- mean(theta_a()$Economic)
        bar_b <- mean(theta_b()$Economic)

        if (bar_a > bar_b) {
            direction <- "right"
            prob <- mean(theta_a()$Economic > theta_b()$Economic)
        } else {
            direction <- "left"
            prob <- mean(theta_b()$Economic > theta_a()$Economic)
        }


        MP_A <- theta_a()$DisplayName |> unique()
        MP_B <- theta_b()$DisplayName |> unique()
        MP_A_full <- MP_A
        MP_B_full <- MP_B
        MP_A <- sub(" \\(.*", "", MP_A)
        MP_B <- sub(" \\(.*", "", MP_B)
        wep <- get_wep(prob)
        bar_a <- round(bar_a)
        bar_b <- round(bar_b)
        prob <- round(prob * 100)

        delta <- theta_a()$Economic - theta_b()$Economic
        cumprobs <- plogis(mean(delta) - cutoffs)
        cum2prob <- function(cp) {
            pr <- rep(NA, length(cp) + 1)
            pr[1] <- 1 - cp[1]
            for (i in 2:(length(pr) - 1)) {
                pr[i] <- cp[i-1] - cp[i]
            }
            pr[length(pr)] <- cp[length(cp)]
            pr
        }
        
### prs[1] is the probability
        pr <- round(100 * cum2prob(cumprobs))
        
        str_for_glueing <- "
<p>&nbsp;</p>
<ul>
<li> <em>{MP_A_full}</em> is <strong>{wep}</strong> {direction} of <em>{MP_B_full}</em> on economic issues. </li>

<li> Our score for {MP_A}, on a scale from 0-100, is <texttt>{bar_a}</texttt>; the same score for {MP_B} is <texttt>{bar_b}</texttt> </li>

<li> The probability that {MP_A} is to the {direction} of {MP_B} is {prob}%. </li>

<li> If we asked 100 councillors to compare these two MPs (and all of them answered), we'd expect the following responses: 

<ul>
 <li> {pr[1]} councillors would say {MP_A} is much more left-wing than {MP_B} </li>
 <li> {pr[2]} councillors would say {MP_A} is somewhat more left-wing than {MP_B} </li>
 <li> {pr[3]} councillors would say the two MPs are about the same  </li>
 <li> {pr[4]} councillors would say {MP_B} is somewhat more left-wing than {MP_A} </li>
 <li> {pr[5]} councillors would say {MP_B} is much more left-wing than {MP_A} </li>
</ul>
</li>
</ul>

"
        
        print(HTML(stringr::str_glue(str_for_glueing)))
          
  })    

    output$pairwise_plot <- renderPlot({
        plot_df <- rbind(theta_a(), theta_b())

        plot_df <- plot_df |>
            mutate(DisplayName = sub(" \\(.*", "", DisplayName))
        
        ggplot(plot_df, aes(x = DisplayName, y = Economic,
                            colour = party_col,
                            fill = party_fill)) +
            scale_y_continuous("Left-right position") +
            scale_x_discrete("") + 
            ggdist::stat_halfeye() +
            scale_colour_identity() +
            scale_fill_identity() + 
            coord_flip() + 
            theme_bw(base_size = 18)
        })

    
    output$party_comparison <- renderUI({
###
        party <- theta_mp_party() |> pull(Party) |> unique()
        party_prefix <- case_when(party == "Plaid Cymru" ~ "",
                                           TRUE ~ "the")
        party_postfix <- case_when(party %in% c("Plaid Cymru", "Scottish National",
                                                "Independents", "Liberal Democrats") ~ ".",
                                   TRUE ~ " party.")
        bar_a <- mean(theta_mp_party() |> filter(selected == 1) |> pull(Economic))
        bar_party <- median(theta_mp_party() |> filter(selected == 0) |> pull(Economic))

        if (bar_a > bar_party) {
            direction <- "right"
            prob <- mean(theta_mp_party() |>
                         filter(selected == 1) |>
                         pull(Economic) >
                         theta_mp_party() |>
                         filter(selected == 0) |>
                         pull(Economic) |>
                         median())
        } else {
            direction <- "left"
            prob <- mean(theta_mp_party() |>
                         filter(selected == 1) |>
                         pull(Economic) <
                         theta_mp_party() |>
                         filter(selected == 0) |>
                         pull(Economic) |>
                         median())
        }

        MP_A <- theta_mp_party() |> filter(selected == 1) |> pull(DisplayName) |> unique()
        MP_A_full <- MP_A
        MP_A <- sub(" \\(.*", "", MP_A)
        
        wep <- get_wep(prob)
        bar_a <- round(bar_a)
        bar_party <- round(bar_party)
        prob <- round(prob * 100)
        
        str_for_glueing <- "
<p>&nbsp;</p>
<ul>
<li> <em>{MP_A_full}</em> is <strong>{wep}</strong> {direction} of the average (median) MP in the {party_prefix} {party} {party_postfix} </li>

<li> Our score for {MP_A}, on a scale from 0-100, is <texttt>{bar_a}</texttt>; the same score for the median MP from the same party is <texttt>{bar_party}</texttt> </li>

<li> The probability that {MP_A} is to the {direction} of the median {party} MP is {prob}%. </li>
</ul>

"
        n_comparisons <- nrow(theta_mp_party())
        if ((n_comparisons == 1) | (party == "Independents")) {
            print(HTML("This comparison doesn't make sense for independents. "))
        } else {
            print(HTML(stringr::str_glue(str_for_glueing)))
        }

    })

    output$party_comparison_plot <- renderPlot({
        party <- theta_mp_party() |> pull(Party) |> unique()
        plot_df <- theta_mp_party() |>
            mutate(y_label = ifelse(selected == 1,
                                    sub(" \\(.*", "", DisplayName),
                                    "Everyone else"))

        ggplot(plot_df, aes(x = y_label, y = Economic,
                            colour = party_col,
                            fill = party_fill)) +
            scale_y_continuous("Left-right position") +
            scale_x_discrete("") +
            scale_fill_identity() +
            scale_colour_identity() + 
            ggdist::stat_halfeye() +
            coord_flip() + 
            theme_bw(base_size = 18)
        })
    
    output$sole_comparison <- renderUI({
###
        bar_a <- mean(theta_mp_sole() |> filter(selected == 1) |> pull(Economic))
        bar_party <- median(theta_mp_sole() |> filter(selected == 0) |> pull(Economic))

        if (bar_a > bar_party) {
            direction <- "right"
            prob <- mean(theta_mp_sole() |>
                         filter(selected == 1) |>
                         pull(Economic) >
                         theta_mp_sole() |>
                         filter(selected == 0) |>
                         pull(Economic) |>
                         median())
        } else {
            direction <- "left"
            prob <- mean(theta_mp_sole() |>
                         filter(selected == 1) |>
                         pull(Economic) <
                         theta_mp_sole() |>
                         filter(selected == 0) |>
                         pull(Economic) |>
                         median())
        }

        MP_A <- theta_mp_sole() |> filter(selected == 1) |> pull(DisplayName) |> unique()
        MP_A_full <- MP_A
        MP_A <- sub(" \\(.*", "", MP_A)
        
        wep <- get_wep(prob)
        bar_a <- round(bar_a)
        bar_party <- round(bar_party)
        prob <- round(prob * 100)
        
        str_for_glueing <- "
<p>&nbsp;</p>
<ul>
<li> <em>{MP_A_full}</em> is <strong>{wep}</strong> {direction} of the average (median) MP in the 2019-2014 House of Commons. </li>

<li> Our score for {MP_A}, on a scale from 0-100, is <texttt>{bar_a}</texttt>; the same score for the median MP is <texttt>{bar_party}</texttt> </li>

<li> The probability that {MP_A} is to the {direction} of the median MP is {prob}%. </li>
</ul>

"
        print(HTML(stringr::str_glue(str_for_glueing)))

    })

    output$sole_comparison_plot <- renderPlot({
        plot_df <- theta_mp_sole() |>
            mutate(y_label = ifelse(selected == 1,
                                    sub(" \\(.*", "", DisplayName),
                                    "Everyone else"),
                   col = case_when(selected == 1 ~ party_col,
                                   selected == 0 ~ "#DDDDDD"),
                   fill = case_when(selected == 1 ~ party_fill,
                                    selected == 0 ~ "#DDDDDD"))

        ggplot(plot_df, aes(x = y_label, y = Economic,
                            colour = col,
                            fill = fill)) +
            scale_y_continuous("Left-right position") +
            scale_x_discrete("") + 
            ggdist::stat_halfeye() +
            scale_colour_identity() +
            scale_fill_identity() + 
            coord_flip() + 
            theme_bw(base_size = 18)
        })
    

}

shinyApp(ui = ui, server = server)

