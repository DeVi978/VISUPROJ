library(shiny)
library(shinydashboard)
library(tidyverse)
D <- read.csv("deliveries2.csv")
M <- read.csv("matches.csv")
D <- D %>% 
  filter(is_super_over == 0)
d1 <- data.frame(Variable = names(D),
                        Type = c("Numeric (discrete)","Binary","Nominal","Nominal","Numeric (discrete)","Numeric (discrete)","Nominal","Nominal","Nominal","Binary",rep("Numeric (discrete)", 8),rep("Nominal", 4)),
                        Description = c("match id number","1 denotes 1st innings, 2 denotes 2nd innings","name of the batting team","name of the bowling team","over number in a match","ball number in an over","name of the striking batsman in the delivery","name of the non-striking batsman in the delivery","name of the bowler","1 if it is a super over delivery else 0","runs for the batting team due to a wide ball delivery","bye runs","legbye runs","runs due to a no-ball delivery","penalty runs","runs scored by the batsman in the delivery","total extra runs scored in the delivery","total runs scored in the delivery","player dismissed in the delivery","type of dismissal","fielder who contributed in the dismissal","winner of the match"))
d2 <- data.frame(Variable = names(M),
                 Type = c("Numeric (discrete)","Numeric (discrete)","Nominal","Ordinal",rep("Nominal",3),"Binary","Nominal","Binary","Nominal","Numeric (discrete)","Numeric (discrete)",rep("Nominal", 5)),
                 Description = c("match id number","IPL season","city hosting the match","date of the match","first team","second team","team winning the toss","toss winning team's decision to field or bat","result type of the match is normal, tie or no result","1 if dls method applied else 0","winner of the match","run difference if won by runs","wicket difference if won by wickets","player of the match","venue where the match is played","1st umpire","2nd umpire","3rd umpire"))

ui <- dashboardPage(
  dashboardHeader(title = "IPL 2008-2019"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Description", tabName = "description"),
      menuItem("Player-wise EDA", tabName = "player"),
      menuItem("Team-wise EDA", tabName = "team"),
      menuItem("Year-wise EDA", tabName = "year"),
      menuItem("Miscellaneous", tabName = "other"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "description",
              h3("Introduction"),
              p("The Indian Premier League (", strong("IPL"), ") is a Men's T20 franchise cricket league of India.
                  The league was founded by the Board of Control for Cricket in India (", strong("BCCI"), ") in 2007.
                  In 2010, the IPL became the first sporting event in the world to be broadcast live on YouTube.
                  The brand value of the IPL in 2019 was ₹47,500 crore (US $5.9 billion).
                  According to BCCI, the 2015 IPL season contributed ₹1,150 crore (US $140 million) to the GDP of the Indian economy.
                  The IPL is the most-attended cricket league in the world and in 2014 was ranked sixth by average attendance among all sports leagues."),
              h3("Objective"),
              p("In this visualization project, I have performed Exploratory Data Analysis (", strong("EDA"), ") on IPL 2008-2019 dataset by exploring through various statistical queries that might interest an inquisitive cricket mind.
                I categorized the queries by players, teams, seasons and miscellaneous others and simultaneously answered them in a visual way for ease of capturing the data analysis and statistics."),
              h3("Dataset & Variables"),
              p(strong("Source:"), a(" IPL 2008-2019 Datasets from Kaggle", href = "https://www.kaggle.com/anuranroy/ipldatasets")),
              p("The dataset comprises of 2 csv files:"),
              p(strong("1. deliveries2.csv:"), "This dataset consists of 179078 rows and 22 columns. It contains ball-by-ball delivery-wise statistics of every match played in the period 2008-2019."),
              tableOutput("t1"),
              p(strong("2. matches.csv:"), "This dataset consists of 756 rows and 18 columns. It contains match-wise statistics of every match played in the period 2008-2019."),
              tableOutput("t2")
      ),
      tabItem(tabName = "player",
              tabsetPanel(
                tabPanel("Batsman",
                         fluidPage(
                           titlePanel(""),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("pq1",
                                           label = "Choose a Query to Explore",
                                           choices = c("Highest Run Scorers in IPL",
                                                       "Most Six Hitters in IPL",
                                                       "Most Boundary Hitters in IPL",
                                                       "Highest Individual Run Scorers in a Match in IPL",
                                                       "Top 5 Players with Most IPL Centuries",
                                                       "Top 10 Players with Most IPL Fifties",
                                                       "IPL Players with Most Sixes Hit in a Match"),
                               ),
                               actionButton("pa1", label = "Show")
                             ),
                             mainPanel(plotOutput(outputId = "pp1"), br(), textOutput("pt1"))
                           )
                         )
                         ),
                tabPanel("Bowler",
                         fluidPage(
                           titlePanel(""),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("pq2",
                                           label = "Choose a Query to Explore",
                                           choices = c("Highest Wicket Takers in IPL",
                                                       "Top 10 'Bowled' Wicket Takers in IPL",
                                                       "Top 10 Powerplay Bowlers in IPL",
                                                       "Top 10 Death Bowlers in IPL",
                                                       "Most Runs conceded by a Bowler in a Match in IPL",
                                                       "Most Dot Balls by a Bowler in IPL",
                                                       "Most No-balls by Bowlers in IPL"),
                               ),
                               actionButton("pa2", label = "Show")
                             ),
                             mainPanel(plotOutput(outputId = "pp2"), br(), textOutput("pt2"))
                           )
                         )
                         ),
                tabPanel("Other",
                         fluidPage(
                           titlePanel(""),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("pq3",
                                           label = "Choose a Query to Explore",
                                           choices = c("Most MoM Award Winners in IPL",
                                                       "Players with most Catches in IPL",
                                                       "Players with most Run Outs in IPL",
                                                       "Players with most Stumpings in IPL"),
                               ),
                               actionButton("pa3", label = "Show")
                             ),
                             mainPanel(plotOutput(outputId = "pp3"), br(), textOutput("pt3"))
                           )
                         )
                         )
              )
      ),
      tabItem(
        tabName = "team",
        fluidPage(
          titlePanel("Analysis by Team"),
          sidebarLayout(
            sidebarPanel(
              selectInput("tq",
                          label = "Choose a Query to Explore",
                          choices = c("Total Wins by Teams in IPL",
                                      "Total Runs Scored by Teams in IPL",
                                      "Highest Total by Teams in IPL",
                                      "Total Overboundaries by Teams in IPL",
                                      "Total Boundaries by Teams in IPL",
                                      "Total Wickets Taken by Teams in IPL",
                                      "Total No-balls Bowled by Teams in IPL",
                                      "Most Extra Runs conceded by Teams",
                                      "Best Chasing Team in IPL",
                                      "Best Defending Team in IPL"),
              ),
              actionButton("ta", label = "Show")
            ),
            mainPanel(plotOutput(outputId = "tp"), br(), textOutput("tt"))
          )
        )
      ),
      tabItem(tabName = "year",
              fluidPage(
                titlePanel("Analysis by Season"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("yq",
                                label = "Choose a Query to Explore",
                                choices = c("No. of Matches Played Each Season",
                                            "Highest Run Scorer Each Season",
                                            "Highest Wicket Taker Each Season",
                                            "Most Six Hitters Each Season",
                                            "Most Boundary Hitters Each Season",
                                            "Total Sixes Hit Each Season",
                                            "Total Boundaries Hit Each Season"),
                    ),
                    actionButton("ya", label = "Show")
                  ),
                  mainPanel(plotOutput(outputId = "yp"), br(), textOutput("yt"))
                )
              )
      ),
      tabItem(
        tabName = "other",
        fluidPage(
          titlePanel("Miscellaneous"),
          sidebarLayout(
            sidebarPanel(
              selectInput("oq",
                          label = "Choose a Query to Explore",
                          choices = c("Top 10 Venues with most IPL Matches Played",
                                      "Top 10 Umpires who officiated most number of Matches on-field"),
              ),
              actionButton("oa", label = "Show")
            ),
            mainPanel(plotOutput(outputId = "op"), br(), textOutput("ot"))
          )
        )
      )
    )
  )
)
  

server <- function(input, output) {
  output$t1 <- renderTable(d1)
  output$t2 <- renderTable(d2)
  cp1 <- eventReactive(
    input$pa1,
    {
      if(input$pq1 == "Highest Run Scorers in IPL"){
        "The 'Run-Machine' Virat Kohli tops the list with 5429 runs, followed closely by Suresh Raina with 5407 runs and then Rohit Sharma with 4914 runs. David Warner (4741), Chris Gayle (4535) and AB de Villers (4415) are the only foreign players in this list."
      }
      else if(input$pq1 == "Highest Wicket Takers in IPL"){
        "Lasith Malinga tops the most wicket takers list with 170 wickets, followed by Amit Mishra (156) and Harbhajan Singh (150). Dwayne Bravo (147) and Sunil Narine (122) are the only other Overseas players in this list."
      }
      else if(input$pq1 == "Most Six Hitters in IPL"){
        "Chris Gayle tops this list hitting a humongous number of 326 sixes in his IPL career 2008-2019, 'not-so-close'-ly followed by AB de Villers (212) and Mahendra Singh Dhoni (207). Other foreign players in this list are David Warner (181), Shane Watson (177) and Kieron Pollard (174)"
      }
      else if(input$pq1 == "Most Boundary Hitters in IPL"){
        "Shikhar Dhawan has hit the most boundaries (526) in IPL 2008-2019, followed by Suresh Raina (495) and Gautam Gambhir (492). David Warner (459) and Chris Gayle (373) are the only foreign players in this list."
      }
      else if(input$pq1 == "Highest Individual Run Scorers in a Match in IPL"){
        "Chris Gayle has scored the most runs, a humongous 175 in an inning, followed by Brendon McCullum (158) and AB de Villers (133). Rishabh Pant (130), Murli Vijay (127) and Virendra Sehwag (122) are the only Indian players in this list."
      }
      else if(input$pq1 == "Top 5 Players with Most IPL Centuries"){
        "Chris Gayle has hit a total of 7 IPL(2008-2019) Centuries in this 20 over format league, followed by Virat Kohli (5) being the only Indian player in the list."
      }
      else if(input$pq1 == "Top 10 Players with Most IPL Fifties"){
        "David Warner has been the most consistent player in IPL 2008-2019 with the most number of fifties (48), followed by Virat Kohli (42) and Suresh Raina (39). AB de Villers with 36 fifties and Chris Gayle with 34 fifties are the only foreign players in the list."
      }
      else{
        "Not to anyone's surprise, Chris Gayle tops the list of hitting the most number of sixes (17) in a single match, followed by Brendon McCullum (13), Chris Gayle (13) and then Chris Gayle (12). Chris Gayle appears 4 times in this list. Murli Vijay (11) and Shreyas Iyer (10) are the only Indians in the list."
      }
    }
  )
  dp1 <- eventReactive(
    input$pa1,
    {
      if(input$pq1 == "Highest Run Scorers in IPL"){
        D %>% 
          group_by(batsman) %>% 
          summarize(runs = sum(batsman_runs)) %>% 
          arrange(desc(runs)) %>% 
          head(10) %>% 
          ggplot(aes(x = reorder(batsman,runs), y = runs, fill = batsman))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = runs),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Runs", title = "Total Runs Scored by a Player", fill = "Batsman")+
          scale_y_continuous(limits = c(0,5600))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$pq1 == "Most Six Hitters in IPL"){
        D %>% 
          filter(batsman_runs == 6) %>% 
          group_by(batsman) %>% 
          summarize(sixes = n()) %>% 
          arrange(desc(sixes)) %>% 
          head(10) %>% 
          ggplot(aes(x = reorder(batsman, sixes), y = sixes, fill = batsman))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = sixes),size = 2.5, position = position_dodge(width = 0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Number of Sixes", title = "Total Sixes Hit by a Player", fill = "Batsman")+
          scale_y_continuous(limits = c(0,335))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$pq1 == "Most Boundary Hitters in IPL"){
        D %>% 
          filter(batsman_runs == 4) %>% 
          group_by(batsman) %>% 
          summarize(fours = n()) %>% 
          arrange(desc(fours)) %>% 
          head(10) %>% 
          ggplot(aes(x = reorder(batsman, fours), y = fours, fill = batsman))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = fours),size = 2.5, position = position_dodge(width = 0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Number of Fours", title = "Total Boundaries Hit by a Player", fill = "Batsman")+
          scale_y_continuous(limits = c(0,540))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$pq1 == "Highest Individual Run Scorers in a Match in IPL"){
        indv <- D %>% 
          group_by(match_id, batsman) %>% 
          summarize(inning_runs = sum(batsman_runs)) %>% 
          arrange(desc(inning_runs)) %>% 
          head(10)
        indv %>% 
          ggplot(aes(x = reorder(as.character(match_id), inning_runs), y = inning_runs, fill = batsman))+
          geom_bar(stat = "identity")+
          scale_x_discrete(labels = rev(indv$batsman))+
          geom_text(aes(label = inning_runs),size = 2.5, position = position_dodge(width = 0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Runs", title = "Total Runs Scored in an Inning by a Player", fill = "Batsman")+
          scale_y_continuous(limits = c(0,180))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$pq1 == "Top 5 Players with Most IPL Centuries"){
        D %>% 
          group_by(match_id, batsman) %>% 
          summarize(inning_runs = sum(batsman_runs)) %>% 
          arrange(desc(inning_runs)) %>% 
          filter(inning_runs >= 100) %>% 
          group_by(batsman) %>% 
          summarize(centuries = n()) %>%
          arrange(desc(centuries)) %>% 
          head(5) %>% 
          ggplot(aes(x = reorder(batsman, centuries), y = centuries, fill = batsman))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = centuries),size = 4.5, position = position_dodge(width = 0.4), vjust = 0.3, hjust = -0.4)+
          coord_flip()+
          labs(x = "Player", y = "Number of Centuries", title = "Total Number of Centuries Hit by a Player", fill = "Batsman")+
          scale_y_continuous(limits = c(0,7.2))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$pq1 == "Top 10 Players with Most IPL Fifties"){
        D %>% 
          group_by(match_id, batsman) %>% 
          summarize(inning_runs = sum(batsman_runs)) %>% 
          arrange(desc(inning_runs)) %>% 
          filter(inning_runs >= 50) %>% 
          group_by(batsman) %>% 
          summarize(fifties = n()) %>%
          arrange(desc(fifties)) %>% 
          head(10) %>% 
          ggplot(aes(x = reorder(batsman, fifties), y = fifties, fill = batsman))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = fifties),size = 2.5, position = position_dodge(width = 0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Number of Fifties", title = "Total Number of Fifties Hit by a Player", fill = "Batsman")+
          scale_y_continuous(limits = c(0,50))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else{
        six_freqs <- D %>% 
          filter(batsman_runs == 6) %>% 
          group_by(match_id, batsman) %>% 
          summarize(freq = n()) %>% 
          arrange(desc(freq)) %>% 
          head(12)
        six_freqs %>%
          ggplot(aes(x = reorder(as.character(match_id), freq), y = freq, fill = batsman))+
          geom_bar(stat = "identity")+
          scale_x_discrete(labels = rev(six_freqs$batsman))+
          geom_text(aes(label = freq), size = 2.5, position = position_dodge(width = 0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Number of Sixes", title = "Total Sixes Hit in a Match by a Player", fill = "Batsman")+
          scale_y_continuous(limits = c(0,18))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
    }
  )
  output$pp1 <- renderPlot(
    {
      dp1()
    }
  )
  output$pt1 <- renderText(
    {
      cp1()
    }
  )
  cp2 <- eventReactive(
    input$pa2,
    {
      if(input$pq2 == "Highest Wicket Takers in IPL"){
        "Lasith Malinga tops the most wicket takers list with 170 wickets, followed by Amit Mishra (156) and Harbhajan Singh (150). Dwayne Bravo (147) and Sunil Narine (122) are the only other Overseas players in this list."
      }
      else if(input$pq2 == "Top 10 'Bowled' Wicket Takers in IPL"){
        "Lasith Malinga has the most number of direct 'Bowled' dismissals numbering up to 63 taking a huge lead, followed by Piyush chawla (43), Sunil Narine (35) and Bhuvaneshwar Kumar (35). Dale Steyn (26) is the only other foreign player in this list."
      }
      else if(input$pq2 == "Top 10 Powerplay Bowlers in IPL"){
        "Zaheer Khan has taken 52 wickets in powerplay overs the most by any bowler, followed by Bhuvaneshwar Kumar (46) and Umesh Yadav (45). Lasith malinga (37) and Morne Morkel (36) are the only foreign players in this list."
      }
      else if(input$pq2 == "Top 10 Death Bowlers in IPL"){
        "Lasith Malinga tops the list by taking a total of 112 wickets in Death Overs only, followed by Dwayne Bravo (95) and Bhuvaneshwar Kumar (80). Dale Steyn (52) is the only other foreign player in this list."
      }
      else if(input$pq2 == "Most Runs conceded by a Bowler in a Match in IPL"){
        "Mujeeb Ur Rahman holds the record of conceding the most runs (73) in a single Match, followed by Basil Thampi (71) and Tim Southee (67)."
      }
      else if(input$pq2 == "Most Dot Balls by a Bowler in IPL"){
        "Harbhajan Singh has delivered 1290 dot balls in total the most by any bowler in IPL, followed by Lasith Malinga (1211) and Bhuvaneshwar Kumar (1175). Dale Steyn (1038) is the only other foreign player in this list."
      }
      else{
        "Sreesanth has bowled the most no-balls (23), followed by Jasprit Bumrah (22) and Ishant Sharma (21)."
      }
    }
  )
  dp2 <- eventReactive(
    input$pa2,
    {
      if(input$pq2 == "Highest Wicket Takers in IPL"){
        D %>% 
          filter(dismissal_kind %in% c("caught","bowled","caught and bowled","lbw","stumped","hit wicket")) %>% 
          group_by(bowler) %>% 
          summarize(wickets = n()) %>% 
          arrange(desc(wickets)) %>% 
          head(10) %>% 
          ggplot(aes(x = reorder(bowler,wickets), y = wickets, fill = bowler))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = wickets),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Number of Wickets", title = "Total Wickets Taken by a Player", fill = "Bowler")+
          scale_y_continuous(limits = c(0,175))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$pq2 == "Top 10 'Bowled' Wicket Takers in IPL"){
        D %>% 
          filter(dismissal_kind == "bowled") %>% 
          group_by(bowler) %>% 
          summarize(wickets = n()) %>% 
          arrange(desc(wickets)) %>% 
          head(10) %>% 
          ggplot(aes(x = reorder(bowler,wickets), y = wickets, fill = bowler))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = wickets),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Number of Direct Dismissals", title = "Total 'Bowled' Wickets Taken by a Player", fill = "Bowler")+
          scale_y_continuous(limits = c(0,65))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$pq2 == "Top 10 Powerplay Bowlers in IPL"){
        D %>% 
          filter(over <= 6) %>% 
          group_by(bowler) %>% 
          summarize(wickets = sum(dismissal_kind %in% c("caught","bowled","caught and bowled","lbw","stumped","hit wicket"))) %>% 
          arrange(desc(wickets)) %>% 
          head(10) %>% 
          ggplot(aes(x = reorder(bowler,wickets), y = wickets, fill = bowler))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = wickets),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Number of Wickets", title = "Total Powerplay Wickets Taken by a Player", fill = "Bowler")+
          scale_y_continuous(limits = c(0,55))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$pq2 == "Top 10 Death Bowlers in IPL"){
        D %>% 
          filter(over >= 15) %>% 
          group_by(bowler) %>% 
          summarize(wickets = sum(dismissal_kind %in% c("caught","bowled","caught and bowled","lbw","stumped","hit wicket"))) %>% 
          arrange(desc(wickets)) %>% 
          head(10) %>% 
          ggplot(aes(x = reorder(bowler,wickets), y = wickets, fill = bowler))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = wickets),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Number of Wickets", title = "Total Death Wickets Taken by a Player", fill = "Bowler")+
          scale_y_continuous(limits = c(0,120))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$pq2 == "Most Runs conceded by a Bowler in a Match in IPL"){
        concede <- D %>% 
          group_by(match_id, bowler) %>% 
          summarize(runs_conceded = sum(batsman_runs) + sum(wide_runs) + sum(noball_runs)) %>% 
          arrange(desc(runs_conceded)) %>% 
          head(10)
        concede %>% 
          ggplot(aes(x = reorder(as.character(match_id), runs_conceded), y = runs_conceded, fill = bowler))+
          geom_bar(stat = "identity")+
          scale_x_discrete(labels = rev(concede$bowler))+
          geom_text(aes(label = runs_conceded),size = 2.5, position = position_dodge(width = 0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Runs", title = "Total Runs conceded by a Bowler in a Match", fill = "Bowler")+
          scale_y_continuous(limits = c(0,75))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$pq2 == "Most Dot Balls by a Bowler in IPL"){
        D %>% 
          filter(wide_runs == 0 & noball_runs == 0 & batsman_runs == 0) %>%
          group_by(bowler) %>% 
          summarize(dot_balls = n()) %>% 
          arrange(desc(dot_balls)) %>% 
          head(10) %>% 
          ggplot(aes(x = reorder(bowler,dot_balls), y = dot_balls, fill = bowler))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = dot_balls),size = 2.5, position = position_dodge(width = 0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Runs", title = "Most Dot Balls by a Bowler overall", fill = "Bowler")+
          scale_y_continuous(limits = c(0,1350))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else{
        D %>% 
          filter(noball_runs != 0) %>% 
          group_by(bowler) %>% 
          summarize(noballs = n()) %>% 
          arrange(desc(noballs)) %>% 
          head(12) %>% 
          ggplot(aes(x = reorder(bowler, noballs), y = noballs, fill = bowler))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = noballs),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "Runs", title = "Total No-balls by a Bowler", fill = "Bowler")+
          scale_y_continuous(limits = c(0, 23))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
    }
  )
  output$pp2 <- renderPlot(
    {
      dp2()
    }
  )
  output$pt2 <- renderText(
    {
      cp2()
    }
  )
  cp3 <- eventReactive(
    input$pa3,
    {
      if(input$pq3 == "Most MoM Award Winners in IPL"){
        "Chris Gayle has been awarded Man of the Match 21 times, followed by AB de Villers (20) and Rohit Sharma (17). David Warner (17), Shane Watson (15) and Michael Hussey (12) are the only foreign players in this list."
      }
      else if(input$pq3 == "Players with most Catches in IPL"){
        "Dinesh Karthik has taken the most number of catches (109), followed by Suresh Raina (102) and MS Dhoni (98). AB de Villers (93), Kieron Pollard (82) and Dwayne Bravo (74) are the foreign players in this list."
      }
      else if(input$pq3 == "Players with most Run Outs in IPL"){
        "Wicketkeeper MS Dhoni (23) tops the list, followed by two non-wicketkeeping skilled throwers and excellent fielders, Ravindra Jadheja (20) and Virat Kohli (17). AB de Villers (14) and Dwayne Bravo (12) are the only foreign players in this list."
      }
      else{
        "MS Dhoni tops the list with most stumpings (38), followed by Robin Uthappa (32) and Dinesh Karthik (29). Adam Gilchrist (16) and QUinton de Kock (8) are the only foreign players in this list."
      }
    }
  )
  dp3 <- eventReactive(
    input$pa3,
    {
      if(input$pq3 == "Most MoM Award Winners in IPL"){
        M %>% 
          group_by(player_of_match) %>% 
          summarize(most_pom = n()) %>% 
          arrange(desc(most_pom)) %>% 
          head(12) %>% 
          ggplot(aes(x = reorder(player_of_match, most_pom), y = most_pom, fill = player_of_match))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = most_pom),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Player", y = "Number of Awards", title = "Most MoM Winner", fill = "Player")+
          scale_y_continuous(limits = c(0,22))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$pq3 == "Players with most Catches in IPL"){
        D %>% 
          mutate(fielder = ifelse(dismissal_kind == "caught and bowled", bowler, fielder)) %>% 
          filter(fielder != "" & (dismissal_kind == "caught" | dismissal_kind == "caught and bowled")) %>% 
          group_by(fielder) %>% 
          summarize(catches = n()) %>% 
          arrange(desc(catches)) %>% 
          head(10) %>% 
          ggplot(aes(x = reorder(fielder, catches), y = catches, fill = fielder))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = catches),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "Number of Catches", title = "Total Catches by a Player", fill = "Player")+
          scale_y_continuous(limits = c(0, 115))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$pq3 == "Players with most Run Outs in IPL"){
        D %>% 
          filter(fielder != "" & dismissal_kind == "run out") %>% 
          group_by(fielder) %>% 
          summarize(runouts = n()) %>% 
          arrange(desc(runouts)) %>% 
          head(12) %>% 
          ggplot(aes(x = reorder(fielder, runouts), y = runouts, fill = fielder))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = runouts),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "Number of Run Outs", title = "Total Run Outs by a Player", fill = "Player")+
          scale_y_continuous(limits = c(0, 24))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else{
        D %>% 
          filter(fielder != "" & dismissal_kind == "stumped") %>% 
          group_by(fielder) %>% 
          summarize(stumpings = n()) %>% 
          arrange(desc(stumpings)) %>% 
          head(10) %>% 
          ggplot(aes(x = reorder(fielder, stumpings), y = stumpings, fill = fielder))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = stumpings),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "Number of Stumpings", title = "Total Stumpings by a Wicketkeeper", fill = "Wicketkeeper")+
          scale_y_continuous(limits = c(0, 40))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
    }
  )
  output$pp3 <- renderPlot(
    {
      dp3()
    }
  )
  output$pt3 <- renderText(
    {
      cp3()
    }
  )
  ct <- eventReactive(
    input$ta,
    {
      if(input$tq == "Total Wins by Teams in IPL"){
        "Mumbai Indians is the most successful IPL Team in terms of Wins (107), followed by Chennai Super Kings (100) and Kolkata Knight Riders (92)."
      }
      else if(input$tq == "Total Runs Scored by Teams in IPL"){
        "Mumbai Indians has scored the most runs (29798) in IPL 2008-2019, followed by the 'not-so-successful' team Royal Challengers Bangalore (28096) and Kings XI Punjab (27868)."
      }
      else if(input$tq == "Highest Total by Teams in IPL"){
        "Royal Challengers Bangalore (263) holds the record of the highest team total by any team in an inning in IPL 2008-2019, followed by Kolkata Knight Riders (250) and Royal Challengers Bangalore (248). Royal Challengers Bangalore appears thrice in this list."
      }
      else if(input$tq == "Total Overboundaries by Teams in IPL"){
        "Having the top two six-hitters in the team, it comes as no surprise that Royal Challengers Bangalore has hit the most number of sixes in IPL 2008-2019, followed by Mumbai Indians (1095) and Kings XI Punjab (975)."
      }
      else if(input$tq == "Total Boundaries by Teams in IPL"){
        "Mumbai Indians has hit the most number of fours (2587), followed by Kings XI Punjab (2454) and Kolkata Knight Riders (2431)."
      }
      else if(input$tq == "Total Wickets Taken by Teams in IPL"){
        "Mumbai Indians has taken the most number of wickets (1035), followed by Royal Challengers Bangalore (935) and Chennai Super Kings (934)."
      }
      else if(input$tq == "Total No-balls Bowled by Teams in IPL"){
        "Mumbai Indians Playes has bowled the most number of no-balls (104) in IPL, followed by Kings XI Punjab (95) and Royal Challengers Bangalore (90)."
      }
      else if(input$tq == "Most Extra Runs conceded by Teams"){
        "Mumbai Indians has conceded the most number of extra runs (1608), followed by Royal Challengers Bangalore (1557) and Kings XI Punjab (1452)."
      }
      else if(input$tq == "Best Chasing Team in IPL"){
        "Kolkata Knight Riders has successfully chased the most number of matches (58), followed by Mumbai Indians (51), Royal Challengers Bangalore (49) and Chennai Super Kings (49)."
      }
      else{
        "Mumbai Indians has successfully defended the most number of matches (58), followed by Chennai Super Kings (51) and Kings XI Punjab (38)."
      }
    }
  )
  dt <- eventReactive(
    input$ta,
    {
      if(input$tq == "Total Wins by Teams in IPL"){
        M %>%
          filter(result == 'normal') %>% 
          group_by(winner)%>%
          summarize(total_win = n())%>%
          ggplot(aes(x = reorder(winner, total_win),y = total_win,fill = winner))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = total_win),size = 2.5, position = position_dodge(width=0.7), vjust=0.3, hjust=-0.2)+
          coord_flip()+
          labs(x = "Winning Team", y = "Number of wins", title = "Total Matches Won by a Team", fill = "IPL Team")+
          scale_y_continuous(limits = c(0,115))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"),
                legend.key.size = unit(0.3, "cm"),
                legend.text = element_text(size = 8))
      }
      else if(input$tq == "Total Runs Scored by Teams in IPL"){
        D %>% 
          group_by(batting_team) %>% 
          summarize(team_runs = sum(total_runs)) %>% 
          ggplot(aes(x = reorder(batting_team, team_runs), y = team_runs, fill = batting_team))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = team_runs),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "Runs", title = "Total Runs Scored by a Team", fill = "IPL Team")+
          scale_y_continuous(limits = c(0, 33000))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"),
                legend.key.size = unit(0.3, "cm"),
                legend.text = element_text(size = 8))
      }
      else if(input$tq == "Highest Total by Teams in IPL"){
        highest_total <- D %>% 
          group_by(match_id, batting_team) %>% 
          summarize(total = sum(total_runs)) %>% 
          arrange(desc(total)) %>% 
          head(10)
        highest_total %>% 
          ggplot(aes(x = reorder(as.character(match_id), total), y = total, fill = batting_team))+
          geom_bar(stat = "identity")+
          scale_x_discrete(labels = rev(highest_total$batting_team))+
          geom_text(aes(label = total),size = 2.5, position = position_dodge(width = 0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "Total Score", title = "Highest Team Totals", fill = "IPL Team")+
          scale_y_continuous(limits = c(0,280))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$tq == "Total Overboundaries by Teams in IPL"){
        D %>% 
          filter(batsman_runs == 6) %>% 
          group_by(batting_team) %>% 
          summarize(team_sixers = n()) %>% 
          ggplot(aes(x = reorder(batting_team, team_sixers), y = team_sixers, fill = batting_team))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = team_sixers),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "Number of Sixes", title = "Total Sixes Hit by a Team", fill = "IPL Team")+
          scale_y_continuous(limits = c(0, 1250))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"),
                legend.key.size = unit(0.3, "cm"),
                legend.text = element_text(size = 8))
      }
      else if(input$tq == "Total Boundaries by Teams in IPL"){
        D %>% 
          filter(batsman_runs == 4) %>% 
          group_by(batting_team) %>% 
          summarize(team_fours = n()) %>% 
          ggplot(aes(x = reorder(batting_team, team_fours), y = team_fours, fill = batting_team))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = team_fours),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "Number of Fours", title = "Total Fours Hit by a Team", fill = "IPL Team")+
          scale_y_continuous(limits = c(0, 2800))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"),
                legend.key.size = unit(0.3, "cm"),
                legend.text = element_text(size = 8))
      }
      else if(input$tq == "Total Wickets Taken by Teams in IPL"){
        D %>% 
          filter(dismissal_kind %in% c("caught","bowled","caught and bowled","lbw","stumped","hit wicket")) %>% 
          group_by(bowling_team) %>% 
          summarize(wickets_taken = n()) %>% 
          ggplot(aes(x = reorder(bowling_team, wickets_taken), y = wickets_taken, fill = bowling_team))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = wickets_taken),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "Number of Wickets", title = "Total Wickets Taken by a Team", fill = "IPL Team")+
          scale_y_continuous(limits = c(0, 1150))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"),
                legend.key.size = unit(0.3, "cm"),
                legend.text = element_text(size = 8))
      }
      else if(input$tq == "Total No-balls Bowled by Teams in IPL"){
        D %>% 
          filter(noball_runs != 0) %>% 
          group_by(bowling_team) %>% 
          summarize(noballs = n()) %>% 
          ggplot(aes(x = reorder(bowling_team, noballs), y = noballs, fill = bowling_team))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = noballs),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "number of No-balls", title = "Total No-balls by a Team", fill = "IPL Team")+
          scale_y_continuous(limits = c(0, 110))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"),
                legend.key.size = unit(0.3, "cm"),
                legend.text = element_text(size = 8))
      }
      else if(input$tq == "Most Extra Runs conceded by Teams"){
        D %>% 
          group_by(bowling_team) %>% 
          summarize(extras = sum(extra_runs)) %>% 
          arrange(desc(extras)) %>% 
          ggplot(aes(x = reorder(bowling_team, extras), y = extras, fill = bowling_team))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = extras),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "Extra Runs", title = "Total Extra Runs conceded by a Team", fill = "IPL Team")+
          scale_y_continuous(limits = c(0, 1750))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"),
                legend.key.size = unit(0.3, "cm"),
                legend.text = element_text(size = 8))
      }
      else if(input$tq == "Best Chasing Team in IPL"){
        toss <- M %>% 
          mutate(toss_loser = ifelse(team1 == toss_winner, team2, team1), .after = toss_winner)
        toss %>% 
          mutate(chasing = ifelse(toss_decision == "field", toss_winner, toss_loser), .before = winner) %>% 
          filter(chasing == winner) %>% 
          group_by(chasing) %>% 
          summarize(chaser = n()) %>%
          ggplot(aes(x = reorder(chasing, chaser), y = chaser, fill = chasing))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = chaser),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "Number of Matches", title = "Total Matches Won by Chasing", fill = "IPL Team")+
          scale_y_continuous(limits = c(0, 60))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"),
                legend.key.size = unit(0.3, "cm"),
                legend.text = element_text(size = 8))
      }
      else{
        toss %>% 
          mutate(defending = ifelse(toss_decision == "bat", toss_winner, toss_loser)) %>% 
          filter(defending == winner) %>% 
          group_by(defending) %>% 
          summarize(defender = n()) %>%
          ggplot(aes(x = reorder(defending, defender), y = defender, fill = defending))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = defender),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Team", y = "Number of Matches", title = "Total Matches Won by Defending", fill = "IPL Team")+
          scale_y_continuous(limits = c(0, 60))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"),
                legend.key.size = unit(0.3, "cm"),
                legend.text = element_text(size = 8))
      }
    }
  )
  output$tp <- renderPlot(
    {
      dt()
    }
  )
  output$tt <- renderText(
    {
      ct()
    }
  )
  cy <- eventReactive(
    input$ya,
    {
      if(input$yq == "No. of Matches Played Each Season"){
        "IPL-2013 has seen the most number of matches (76), followed by IPL-2012 (74) and IPL-2011 (73)."
      }
      else if(input$yq == "Highest Run Scorer Each Season"){
        "Among all IPL seasons, Virat Kohli has scored the most number of runs in a single season (973) in IPL-2016 scoring a total of 973 runs, followed by Kane Williamson (747) in IPL-2018, Michael Hussey (733) in IPL-2013 and Chris Gayle (733) in IPL-2012."
      }
      else if(input$yq == "Highest Wicket Taker Each Season"){
        "Among all seasons, Dwayne Bravo has been the leading wicket taker in a single season in IPL-2013 taking a total of 32 wickets, followed by Lasith Mailnga (28) in IPL-2011, Imran Tahir (26) in IPL-2019, Bhuvaneshwar Kumar (26) in IPL-2017 and Dwayne Bravo (26) in IPL-2015."
      }
      else if(input$yq == "Most Six Hitters Each Season"){
        "Among all seasons, Chris Gayle holds the record of hitting the most number of sixes in a single season in IPL-2012 hitting a total of 59 sixes, followed by Andre Russell hitting 51 sixes in IPL-2019. Chris Gayle is also the leading six hitter of three other seasons IPL-2011, IPL-2013 and IPL-2015."
      }
      else if(input$yq == "Most Boundary Hitters Each Season"){
        "Among all seasons, David Warner holds the record of being the most boundary hitter in a single season in IPL-2016 hitting a total of 88 boundaries, closely followed by Sachin Tendulkar hitting 86 boundaries in IPL-2010 and Michael Hussey hitting 81 boundaries in IPL-2013."
      }
      else if(input$yq == "Total Sixes Hit Each Season"){
        "IPL-2018 has seen the most number of overboundaries amounting to 869, followed by IPL-2019 (778) and IPL-2012 (733)."
      }
      else{
        "IPL-2013 has seen the most number of boundaries numbering up to 2052, followed by IPL-2011 (1916) and IPL-2012 (1911)."
      }
    }
  )
  dy <- eventReactive(
    input$ya,
    {
      full_data <- D %>%
        left_join(M, by = c("match_id" ="id"))
      if(input$yq == "No. of Matches Played Each Season"){
        Season <- as.character(sort(unique(M$season)))
        M %>% 
          group_by(season) %>%
          summarize(matches_played = n()) %>% 
          ggplot(aes(x = Season, y = matches_played, fill = Season))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = matches_played),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "IPL Season", y = "Number of Matches", title = "Total Matches Played per Season")+
          theme_bw()+
          scale_y_continuous(limits = c(0,80))+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(color = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$yq == "Highest Run Scorer Each Season"){
        full_data %>% 
          group_by(season, batsman) %>% 
          summarize(runs = sum(batsman_runs)) %>% 
          filter(runs == max(runs)) %>% 
          ggplot(aes(x = as.character(season), y = runs, fill = batsman))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = runs),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Season", y = "Runs", title = "Highest Run Scorer in a Season", fill = "Batsman")+
          scale_y_continuous(limits = c(0,1000))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$yq == "Highest Wicket Taker Each Season"){
        full_data %>% 
          filter(dismissal_kind %in% c("caught","bowled","caught and bowled","lbw","stumped","hit wicket")) %>%
          group_by(season, bowler) %>% 
          summarize(wickets = n()) %>%
          filter(wickets == max(wickets)) %>% 
          ggplot(aes(x = as.character(season), y = wickets, fill = bowler))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = wickets),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Season", y = "Number of Wickets", title = "Highest Wicket Taker in a Season", fill = "Bowler")+
          scale_y_continuous(limits = c(0,33))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$yq == "Most Six Hitters Each Season"){
        six_hitter <- full_data %>%
          filter(batsman_runs == 6) %>% 
          group_by(season, batsman) %>% 
          summarize(runs = n()) %>% 
          filter(runs == max(runs))
        six_hitter %>% 
          ggplot(aes(x = paste(season,batsman), y = runs, fill = batsman))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = runs),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          scale_x_discrete(labels = six_hitter$season)+
          coord_flip()+
          labs(x = "Season", y = "Number of Sixes", title = "Most Six Hitter in a Season", fill = "Batsman")+
          scale_y_continuous(limits = c(0,60))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$yq == "Most Boundary Hitters Each Season"){
        four_hitter <- full_data %>%
          filter(batsman_runs == 4) %>% 
          group_by(season, batsman) %>% 
          summarize(runs = n()) %>% 
          filter(runs == max(runs))
        four_hitter %>% 
          ggplot(aes(x = paste(season,batsman), y = runs, fill = batsman))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = runs),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          scale_x_discrete(labels = four_hitter$season)+
          coord_flip()+
          labs(x = "Season", y = "Number of Fours", title = "Most Boundary Hitter", fill = "Batsman")+
          scale_y_continuous(limits = c(0,90))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else if(input$yq == "Total Sixes Hit Each Season"){
        full_data %>%
          filter(batsman_runs == 6) %>% 
          group_by(season) %>% 
          summarize(sixes_hit = n()) %>% 
          ggplot(aes(x = Season, y = sixes_hit, fill = Season))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = sixes_hit),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Season", y = "Number of Overboundaries", title = "Total Sixes Hit", fill = "Season")+
          scale_y_continuous(limits = c(0,900))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
      else{
        full_data %>%
          filter(batsman_runs == 4) %>% 
          group_by(season) %>% 
          summarize(fours_hit = n()) %>% 
          ggplot(aes(x = Season, y = fours_hit, fill = Season))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = fours_hit),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Season", y = "Number of Boundaries", title = "Total Fours Hit", fill = "Season")+
          scale_y_continuous(limits = c(0,2100))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
    }
  )
  output$yp <- renderPlot(
    {
      dy()
    }
  )
  output$yt <- renderText(
    {
      cy()
    }
  )
  co <- eventReactive(
    input$oa,
    {
      if(input$oq == "Top 10 Venues with most IPL Matches Played"){
        "Highest number of matches were played in Eden Gardens Cricket Stadium (77) in Kolkata, followed by Wankhede Stadium (73) in Mumbai and M Chinnaswamy Stadium (73) in Bangalore."
      }
      else{
        "S Ravi has officiated the most number of 106 IPL matches, followed by Dharmasena (87) and Shamsuddin (73)."
      }
    }
  )
  do <- eventReactive(
    input$oa,
    {
      if(input$oq == "Top 10 Venues with most IPL Matches Played"){
        M %>% 
          group_by(venue) %>% 
          summarize(total_match = n()) %>%
          arrange(desc(total_match)) %>% 
          head(10) %>% 
          ggplot(aes(x = reorder(venue,total_match),y = total_match))+
          geom_bar(stat = "identity", fill = "#CCFF00")+
          geom_text(aes(label = total_match),size = 2.5,position=position_dodge(width=0.7), vjust=0.3, hjust=1.2)+
          coord_flip()+
          labs(x = "Venue", y = "Number of Matches", title = "Total Matches played in a Venue")+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"))
      }
      else{
        data.frame(umpire = union_all(M$umpire1, M$umpire2)) %>% 
          group_by(umpire) %>% 
          summarize(umpiring = n()) %>%
          arrange(desc(umpiring)) %>% 
          head(11) %>% 
          ggplot(aes(x = reorder(umpire, umpiring), y = umpiring, fill = umpire))+
          geom_bar(stat = "identity")+
          geom_text(aes(label = umpiring),size = 2.5, position = position_dodge(width=0.7), vjust = 0.3, hjust = -0.2)+
          coord_flip()+
          labs(x = "Umpire", y = "Number of Matches", title = "Total Matches officiated on-field", fill = "Umpire")+
          scale_y_continuous(limits = c(0,110))+
          theme_bw()+
          theme(plot.title = element_text(face = "italic", size = 12),
                plot.background = element_rect(colour = "black"),
                legend.background = element_rect(colour = "black", fill = "#FFFFCC"),
                panel.background = element_rect(fill = "#FFFFCC"))
      }
    }
  )
  output$op <- renderPlot(
    {
      do()
    }
  )
  output$ot <- renderText(
    {
      co()
    }
  )
}
  
shinyApp(ui, server)

