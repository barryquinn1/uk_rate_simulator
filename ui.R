fluidPage(
  theme = shinytheme("flatly"),
  # Application title
  fluidRow(
    headerPanel(
      tags$div(
        h1("Interest Rate Forecasting Stories", style="display: inline",align="center"),
        img(src="rw_hex.png", align="left",width="3%",style="display: inline",
            href="https://github.com/barryquinn1/tsfe"),
      ), windowTitle = "Interest Rate"),
    p("Big-world prediction is hard but we can learn alot from the pyschology of forecasting.",
    a(" Phil Tetlock",hef="https://www.sas.upenn.edu/tetlock/"),
    "points out that its not brainpower, or statistical prowess that is the 
    most important attribute of a superforecaster, but the ability to rethink 
      and change our minds. ",a(" Adam Grant", href="https://www.adamgrant.net/"),
    "called this trait confident humulity in his latest book Think Again. 
      He defines confident humilty as:",
      strong("the ability to doubt our own judgement and 
           the curiousity to seek our new information that
           can be used to revise our predictions.")),
    br(),
    p("Forecasting interest rates is a hard big-world problem which I have been rethinking
    through discussions with my learned colleagues", 
        a("Mark Farrell",href="https://proactuary.com/about-proactuary/")," and",
        a("Ronan Gallagher.",
          href="https://www.business-school.ed.ac.uk/staff/ronan-gallagher"),
      "  As we talk about what the future holds for UK interest rates, 
      the reluctant statistican in me thought to build a small-world future possibilities ",em("story-teller"),
      "by simulation. Big-world prediction is hard as 
      the most important factors for accurate forecasts are usually the hardest to measure. 
      In the small-world we can use historical data, and the computer-age tools of simulation. 
      This app allows to to tell future stories using the parametric techniques of", 
      a("Cox–Ingersoll–Ross (CIR) model",href="https://en.wikipedia.org/wiki/Cox–Ingersoll–Ross_model"),", a one-factor market risk model"
      ,". The nonparametric stories evolve via a bootstrap of the change in the underlying historical interest rate.  
      As academic Dads our discussion focused on two interest rates important to our family's futures, 
      the Bank of England base rate and the 5 year fixed mortgage rate.  
      The data used is sourced from the Bank of England database.")
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
              "h","Months Ahead (h)",
              min = 1,max = 60,value = 60,ticks = FALSE)),
          br(),
          radioButtons(
            inputId = "rate",
            label = "Rate to Forecast",
            choices = c("UK Base Rate" = "base",
                        "UK Fixed 5 yr Interest Rate(LTV_75%)" = "fixed5yr_75",
                        "UK Fixed 5 yr Interest Rate(LTV_95%)" = "fixed5yr_95"),
            inline = TRUE,selected = "base"),
        br(),
        radioButtons(
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
          column(width = 6,
            selectInput(inputId="start",label="Sample Start Point",datelist,selected = min)),
          column(width = 6,
            selectInput(inputId="end",label="Sample End Point",datelist,selected = max)))))),
    column(
      width = 9,
      tabsetPanel(
        tabPanel("Plot",br(),highchartOutput("sims_chart", height = 600)),
        tabPanel("Table",br(),DT::dataTableOutput("sim_tbl")), 
        tabPanel("Distributon",br(),highchartOutput("sims_dist", height = 600))))),
  fluidRow(
    column(
      width = 12,
      class = "text-center",
      br(),br(),hr(),
      h1("Historical Interest Rate"),
      h3("(For Your Reference in Making Parameter Selections in Above Walk)"),br()),
    column(
      width = 12,wellPanel(highchartOutput("ir_chart", height = 500)))))