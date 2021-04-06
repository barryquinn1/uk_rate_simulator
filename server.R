function(input, output, server) {
  sel_rate<- reactive({
    dat <- if (input$rate == "base") {
      base_rate_df %>% select(Index,..Close) %>% rename(rate=..Close)
    } 
    if (input$rate == "UK_95") {
      fixed5yr_df %>% select(Index,UK_95) %>% rename(rate=UK_95)
    }
    else {
      fixed5yr_df %>% select(Index,UK_75) %>% rename(rate=UK_75)
    }  
  })
  
  cir_sims <- reactive({
    set.seed(1234)
    obs <- input$boot_sample
    h <- input$h
    initial_b_rate<-sel_rate() %>%
    filter(Index==max)
    out <- vector("list", length = obs)
    for (i in seq_along(out)) {
      sim<-rcCIR(n=input$h, Dt = 0.1, x0 = initial_b_rate$rate, theta = c(input$ir_mean * input$reversion, input$reversion, input$ir_sd))
      out[[i]] <- sim
    }
    
    out
  })
  
  bs_sims <- reactive({
    set.seed(12345)
    br<-sel_rate() %>%
      filter(
        Index >= input$start,
        Index <= input$end) %>% 
      drop_na()
    br<-br$rate
    validate(
      need(
        length(br) >= 2, 
        label = "Sample Months", 
        message = "Not Enough Historical Yields in Sample"
      ), 
      errorClass = character(0)
    )
    
    obs <- input$boot_sample
    
    rate_changes <- diff(br) / br[-length(br)]
    rate_changes<-rate_changes[!is.na(rate_changes)]
    
    out <- vector("list", length = obs)
    for (i in seq_along(out)) {
      initial_b_rate<-sel_rate() %>%
        filter(Index==max) %>% select(rate) %>% unlist(use.names = FALSE)
      sim_changes <- 1 + sample(rate_changes, size = input$h, replace = TRUE)
      sim_changes <- cumprod(sim_changes)
      sim<-c(initial_b_rate,initial_b_rate * sim_changes)
      out[[i]] <- sim
    }
    
    out
    
  })
  
  sel_sim <- reactive({
    dat <- if (input$type == "cir") {
      cir_sims()
    } else {
      bs_sims()
    }
  })
  
  sims_chart_prep <- reactive({

    dat<- sel_sim()
    out <- vector("list", length = length(dat))
    for (i in seq_along(out)) {
      out[[i]]$data <- dat[[i]]
      out[[i]]$name <- paste0("V", i)
    }
    isolate({
      title <- if (input$type == "cir") {
        list(
          main = "Cox-Ingersoll-Ross Random Walk",
          sub = paste0("Parameters: a = ", input$reversion, ", b = ", input$ir_mean, 
                       ", sigma = ", input$ir_sd) 
        ) 
      } else {
        list(
          main = paste0("Bootstrap Resampling - Changes in ",input$rate), 
          sub = paste0("Parameters: Initial rate =", dat$rate[[1]], 
                       ", Sampled Changes from ", 
                       input$start, 
                       " to ",
                         input$end)
        )
      }
    })
    list(
      dat = out,
      titles = title
    )
  })
  
  output$sims_chart <- renderHighchart({
    dat <- sims_chart_prep()$dat
    titles <- sims_chart_prep()$titles  
    highchart() %>%
      hc_chart(zoomType = "y") %>%
      hc_title(text = titles$main) %>%
      hc_subtitle(text = titles$sub) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = tychobratools::hc_btn_options()) %>%
      hc_legend(
        enabled = FALSE,
        reversed = TRUE) %>%
      hc_plotOptions(
        series = list(
          tooltip = list(
            crosshairs = TRUE,
            pointFormat = 'Rate: <b>{point.y:,.2f}</b>'
          ),
          marker = list(enabled = FALSE))) %>%
      hc_xAxis(title = list(text = "Months Ahead")) %>%
      hc_yAxis(title = list(text = "Rate")) %>%
    hc_add_series_list(dat)
  })
  
  output$sims_dist <- renderHighchart({
    
    out <- sel_sim()
    names(out) <- paste0("Simulation", 1:length(out))
    out <- as_data_frame(out)
    out[nrow(out),] %>% as.numeric()->final
    probs<-fortify(ecdf(final)) %>%
      mutate(y=if_else(y>0.5,1-y,y))
    hchart(probs,hcaes(x=x,y=y),
           name="Proability",type="area") %>%
      hc_xAxis(title = list(text = "Final Interest Rate Forecast")) %>%
      hc_yAxis(title = list(text = "Probability")) 
  })
  
  output$sim_tbl <- DT::renderDataTable({
    out <- sel_sim()
    names(out) <- paste0("Simulation", 1:length(out))
    out <- as_data_frame(out)
    out <- cbind("t" = max + seq(1/12,input$h/12,by=1/12), out)
    
    DT::datatable(
      out,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom= "Btp",
        buttons = list("excel", "csv"),
        ordering = FALSE,
        scrollX = TRUE
      )
    ) %>%
      formatRound(
        columns = 2:length(out),
        digits = 2
      )
  })
  
  output$ir_chart <- renderHighchart({
    highchart() %>%
      hc_chart(
        zoomType = "y",
        type = "line"
      ) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = tychobratools::hc_btn_options()
      ) %>%
      hc_legend(
        enabled = TRUE,
        reversed = TRUE
      ) %>%
      hc_add_series(
        data = all_rates, "line",
        name = "UK Base Rate",
       hcaes(Index, base_rate)
      ) %>%
      hc_add_series(
        data = all_rates,"line",
        name = "5 Yr Fixed Rate (95%LTV)",
        hcaes(Index, UK_95)
      ) %>%
      hc_add_series(
        data = all_rates,"line",
        name = "5 Yr Fixed Rate (75%LTV)",
        hcaes(Index, UK_75)
      )
  })
}
