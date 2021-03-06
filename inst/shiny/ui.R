# americanRouletteV4 UI file
# Ignat Kulinka


# I. Package imports ------------------------------------------------------
require(DT)
require(shiny)
require(ggplot2)
require(stringr)
require(dplyr)
require(data.table)

fluidPage(
  # Busy pop-up!
  tags$head(tags$style(type="text/css", "#loadmessage { position: fixed;
                                                             top: 0px;
                                                            left: 0px;
                                                           width: 100%;
                                                         padding: 0px 0px 0px 0px;
                                                      text-align: center;
                                                     font-weight: bold;
                                                       font-size: 100%;
                                                           color: #000000;
                                                background-color: #ffffff;
                                                         z-index: 105; } ")),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Processing, please wait!", id="loadmessage")),


  # Application title
  headerPanel("American Roulette"),

  fluidRow(
    # Sidebar with a slider and selection inputs
    column(5,
           tabsetPanel(
             tabPanel("Betting",
                      br(),
                      ### Manual Betting
                      h4("Manual Betting"),
                      strong("Bet Amount:"),
                      br(),
                      # \10\25\50\100\250\ bet buttons
                      actionButton("bet1", "$10"),
                      actionButton("bet2", "$25"),
                      actionButton("bet3", "$50"),
                      actionButton("bet4", "$100"),
                      actionButton("bet5", "$250"),
                      br(),
                      br(),
                      h4("Chip Color"),
                      selectizeInput("chipColor", "Choose chip color:",
                                     choices = tolower(colors()[grepl("^[^0-9]*$", colors())]),
                                     selected = "navy"),
                      hr(),
                      ### Computer Assisted Betting
                      h4("Computer Assisted Betting"),
                      sliderInput("numBets", "Number of Random Bets:", min = 1, max = 35, value = 1),
                      actionButton("random", "Place random bet(s)"),
                      br(),
                      br(),
                      actionButton("session_status", "Print session_vals")
                      # COMING SOON
                      #strong("Other computer generated bets")
             ),
             tabPanel("Summary",
                      br(),
                      ### Manual Bet List
                      h4("Manual Bets"),
                      DTOutput("viewManual"),
                      hr(),
                      ### Computer Assisted Bets
                      h4("Computer Assisted Bets"),
                      DTOutput("viewCPU"),
                      hr(),
                      ###\Spin Roulette\Reset Bets\
                      actionButton("spin", "Spin Roulette"),
                      actionButton("reset", "Reset Bets"),
                      hr(),
                      ### Results
                      h4("Results"),
                      verbatimTextOutput("roulette", placeholder = FALSE),
                      DTOutput("result"),
                      verbatimTextOutput("total", placeholder = FALSE)))),
    column(7,
           tabsetPanel(
             tabPanel("Roulette Table",
                      plotOutput("rTable", click = "plot_click", width = "100%")),
             tabPanel("Summary Plots",
                      fluidRow(
                        column(12,
                               br(),
                               h4("Total Balance vs. Round Number"),
                               plotOutput("mainPlot", height = "250px"),
                               fluidRow(column(12,
                                               br(),
                                               h4("Winning Slots Distribution"),
                                               plotOutput("midPlot", height = "250px", width = "100%"),
                                               fluidRow(column(4,
                                                               br(),
                                                               h4("Manual vs CPU Amount Won"),
                                                               plotOutput("bottomLeftPlot", height = "180px", width = "100%")),
                                                        column(4,
                                                               br(),
                                                               h4("Manual vs CPU Bets Won"),
                                                               plotOutput("bottomMidPlot", height = "180px", width = "100%")),
                                                        column(4,
                                                               br(),
                                                               h4("Number of Bets Won vs Lost"),
                                                               plotOutput("bottomRightPlot", height = "180px", width = "100%")))))))),
             tabPanel("Data Output",
                      br(),
                      fluidRow(
                        column(8,
                               h4("Data Generated by the Simulation")),
                        column(4,
                               downloadButton("downloadData", "Download")),
                        fluidRow(
                          column(12,
                                 DTOutput("dataOutput"))
                        ))),
             tabPanel("Chat",
                      bootstrapPage(
                        # We'll add some custom CSS styling -- totally optional
                        includeCSS("shinychat.css"),

                        # And custom JavaScript -- just to send a message when a user hits "enter"
                        # and automatically scroll the chat window for us. Totally optional.
                        includeScript("sendOnEnter.js"),

                        div(
                          # Setup custom Bootstrap elements here to define a new layout
                          class = "container-fluid",
                          div(class = "row-fluid",

                              # Create the header
                              div(class="span6", style="padding: 10px 0px;",
                                  h3("Chat"))),
                          # The main panel
                          div(
                            class = "row-fluid",
                            mainPanel(
                              # Create a spot for a dynamic UI containing the chat contents.
                              uiOutput("chat"),

                              # Create the bottom bar to allow users to chat.
                              fluidRow(
                                div(class="span10",
                                    textInput("entry", "")
                                ),
                                actionButton("send", "Send")
                              )
                            ),
                            # The right sidebar
                            sidebarPanel(
                              # Let the user define his/her own ID
                              textInput("user", "Your User ID:", value=""),
                              tags$hr(),
                              h5("Connected Users"),
                              # Create a spot for a dynamic UI containing the list of users.
                              uiOutput("userList")
                            )
                          )
                        )
                      )
             )))))
