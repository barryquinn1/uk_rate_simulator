fluidPage(
  theme = shinytheme("flatly"),
  # Application title
  fluidRow(
    headerPanel(
      tags$div(
        h1("Interest Rate Forecasts", align="center"),
        img(src="rw_hex.png", align="left",width="10%",
            href="https://github.com/barryquinn1/tsfe"),
        h3("Simulation stories",align="center"),
      ), windowTitle = "Interest Rate"),
    p("These stories are inspired by discussions with my learned colleagues", 
        a("Mark Farrell",href="https://proactuary.com/about-proactuary/")," and",
        a("Ronan Gallagher.",
          href="https://www.business-school.ed.ac.uk/staff/ronan-gallagher"),
      " As we talk about what the future holds for UK interest rates, 
      I thought to build a simulations",em("story-teller"),
      "to see what the future may hold. This app allows to to tell future stories using the parametric techniques of", 
      a("Cox–Ingersoll–Ross (CIR) model",href="https://en.wikipedia.org/wiki/Cox–Ingersoll–Ross_model"),", a one-factor market risk model"
      ,"The nonparametric stories evolve via a bootstrap of the change in the underlying historical interest rate.  The app allows the choice of
      the Bank of England base rate and the average 5 year fixed mortgage rate for LTV95% or LTV75%.",align="center")
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
            inline = TRUE,selected = "fixed5yr_75"),
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
      h1("Historical Interest Rate"),
      h3("(For Your Reference in Making Parameter Selections in Above Walk)"),br()),
    column(
      width = 12,wellPanel(highchartOutput("ir_chart", height = 500)))))