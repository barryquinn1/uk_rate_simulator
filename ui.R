fluidPage(
  theme = shinytheme("flatly"),
  # Application title
  fluidRow(
    headerPanel(
      tags$div(
        h1("UK Base Rate Simulator", style = "display: inline", class="center"),
        a(
          href = "https://github.com/barryquinn1",
          icon("github", class="fa-lg pull-right")
        ),
        img(src="rw_hex.png", align="left",width="5%",
            href="https://github.com/barryquinn1/tsfe")
      ), 
      windowTitle = "Interest Rate"
    )
  ),
  br(),
  fluidRow(
    column(
      width = 3,class = "text-center",
      wellPanel(h3("General Simulation Parameters"),br(),
                fluidRow(column(
            width = 6,sliderInput(
              "boot_sample",
              "Bootstrap Sample Size",
              min = 1,max = 5000,value = 100,ticks = FALSE)),
          column(width = 6,sliderInput(
              "h","Months Ahead",
              min = 1,max = 60,value = 60,ticks = FALSE)),
        br(),radioButtons(
          inputId = "type",
          label = "Type of Walk",
          choices = c("Cox-Ingersoll-Ross" = "cir","Bootstrap Resampling" = "bootstrap"),
          inline = TRUE,selected = "bootstrap"),
        br(),hr(style = "border-top: 1px solid #000"),
        conditionalPanel(
          'input.type == "cir"',
          h3("Cox-Ingersoll-Ross Parameters"),br(),
          numericInput(inputId = "reversion",
            label = "Mean Reversion Speed - a",
            value = 2,
            step = 0.1),
          numericInput(
            inputId = "ir_mean",
            label = "Interst Rate Mean - b",
            value = 3,
            step = 0.1),
          numericInput(
            inputId = "ir_sd",
            label = "Interest Rate SD - sigma",
            value = 2,
            step = 0.1)),
        conditionalPanel(
          'input.type == "bootstrap"',
          h3("Bootstrap Resampling"),
          br(),
          numericInput(
            inputId = "bs_0",
            label = "Initial Rate (yield at h=0)",
            value = base_rate$Value[1],
            step = 0.1),
          column(width = 6,
            selectInput(inputId="start",label="Sample Start Point",datelist,selected = datelist[400])),
          column(width = 6,
            selectInput(inputId="end",label="Sample End Point",datelist,selected = max))))))),
    column(
      width = 9,
      tabsetPanel(
        tabPanel("Plot",br(),highchartOutput("sims_chart", height = 600)),
        tabPanel("Table",br(),DT::dataTableOutput("sim_tbl")), 
        tabPanel("Distributon",br(),highchartOutput("sims_dist", height = 600)))),
  fluidRow(
    column(
      width = 12,
      class = "text-center",
      br(),br(),hr(),
      h1("Historical UK Base Rate"),
      h3("(For Your Reference in Making Parameter Selections in Above Walk)"),br()),
    column(
      width = 12,wellPanel(highchartOutput("ir_chart", height = 500)))))