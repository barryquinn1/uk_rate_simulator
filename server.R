function(input, output, server) {
  cir_sims <- reactive({
    set.seed(1234)
    obs <- input$boot_sample
    h <- input$h
    out <- vector("list", length = obs)
    for (i in seq_along(out)) {
      out[[i]] <- rcCIR(n=input$h, Dt = 0.1, x0 = input$bs_0, theta = c(input$ir_mean * input$reversion, input$reversion, input$ir_sd))
    }
    
    out
  })
  
  bs_sims <- reactive({
    set.seed(12345)
    rates <- base_rate %>%
      filter(
        Date >= input$daterange[1] & Date <= input$daterange[2]
      )
    b_rate <- rates$Value
    
    obs <- input$boot_sample
    
    rate_changes <- diff(b_rate) / b_rate[-length(b_rate)]
    
    out <- vector("list", length = obs)
    for (i in seq_along(out)) {
      # find initial yield
      initial_b_rate <- input$bs_0
      sim_changes <- 1 + sample(rate_changes, size = input$h, replace = TRUE)
      sim_changes <- cumprod(sim_changes)
      out[[i]] <- c(initial_b_rate * sim_changes)
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
    dat <- sel_sim()
    
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
          sel_duration <- gsub("[^*]_", "", input$duration)
          
        list(
          main = paste0("Bootstrap Resampling - Changes in ", 
                       sel_duration, 
                       " "), 
          sub = paste0("Parameters: Initial rate = ", input$bs_0, 
                       ", Sampled Changes from ", 
                       input$daterange[1], 
                       " to ",
                       input$daterange[2])
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
      hc_chart(
        zoomType = "y"
      ) %>%
      hc_title(text = titles$main) %>%
      hc_subtitle(text = titles$sub) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = tychobratools::hc_btn_options()
      ) %>%
      hc_legend(
        enabled = FALSE,
        reversed = TRUE
      ) %>%
      hc_plotOptions(
        series = list(
          tooltip = list(
            crosshairs = TRUE,
            pointFormat = 'Rate: <b>{point.y:,.2f}</b>'
          ),
          marker = list(enabled = FALSE)
        )
      ) %>%
      hc_xAxis(
        categories = 1:isolate({input$h}),
        title = list(text = "Simulated Steps")
      ) %>%
      hc_yAxis(
        title = list(text = "Rate")
      ) %>%
      hc_add_series_list(
        dat
      )
  })
  
  output$sims_dist <- renderHighchart({
    
    out <- sel_sim()
    names(out) <- paste0("Simulation", 1:length(out))
    out <- as_data_frame(out)
    out[nrow(out),] %>% as.numeric()->final
    # den<-density(final)
    # dat<-tibble(y=den$y)
    hchart(density(final),name="Final Base Rate Distribution",type="area")
  })
  
  output$sim_tbl <- DT::renderDataTable({
    out <- sel_sim()
    names(out) <- paste0("Simulation", 1:length(out))
    out <- as_data_frame(out)
    out <- cbind("t" = 1:nrow(out), out)
    
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
  }, server = FALSE)
  
  output$ir_chart <- renderHighchart({
    highchart(type = "stock") %>%
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
        )  %>%
      hc_xAxis(
        type = 'datetime'
      ) %>%
      hc_yAxis(
        title = list(text = "Rate")
      ) %>%
      hc_add_series(
        base_rate_xts[index(base_rate_xts)<=input$daterange[2]
                      & index(base_rate_xts)>=input$daterange[1]]
      )
  })
}
