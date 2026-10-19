install.packages(c(
  "shiny",
  "plotly",
  "ggplot2",
  "DT"
))

shiny::runApp()

})

output$route_bar <- renderPlot({
  d <- route_data()
  validate(need(nrow(d) > 0, "No valid interstate route records available."))
  ggplot(d, aes(x = reorder(other_state, individuals), y = individuals)) +
    geom_col(fill = if (input$route_direction == "in") "#16887e" else "#dd8766",
             width = .75) + coord_flip() +
    labs(x = NULL, y = "Individuals represented on IRS returns (N2)") +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", trim = TRUE)) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          plot.margin = margin(10, 15, 10, 5))
})

output$route_table <- renderDT({
  d <- route_data()
  tbl <- data.frame(
    `From → To` = paste(d$origin_state, "→", d$destination_state),
    `Returns (N1)` = d$returns,
    `Individuals (N2)` = d$individuals,
    `AGI ($ millions)` = round(d$agi_thousands / 1000, 1),
    check.names = FALSE
  )
  datatable(tbl, rownames = FALSE,
            options = list(pageLength = 10, scrollX = TRUE, dom = "t"))
})

output$state_comparison <- renderDT({
  d <- view_data()
  a <- d[d$abbr == input$state_a, , drop = FALSE]
  b <- d[d$abbr == input$state_b, , drop = FALSE]
  keys <- c("net_rate", "in_rate", "out_rate", "net_people",
            "population_growth_pct", "housing_burden_pct_2024",
            "median_income", "median_home_value", "median_rent",
            "median_rent_pct", "unemployment_pct", "poverty_pct",
            "commute_minutes")
  label <- c(unname(metric_labels[c("net_rate", "in_rate", "out_rate")]),
             "Net IRS represented individuals", unname(metric_labels[keys[5:13]]))
  show_val <- function(d, k) {
    if (k == "net_people") return(fmt_signed(d$net_people, 0))
    fmt_metric(d[[k]], k)
  }
  tbl <- data.frame(
    Measure = label,
    `First state` = vapply(keys, function(k) show_val(a, k), character(1)),
    `Second state` = vapply(keys, function(k) show_val(b, k), character(1)),
    check.names = FALSE
  )
  names(tbl)[2:3] <- c(a$state, b$state)
  datatable(tbl, rownames = FALSE,
            options = list(pageLength = 15, dom = "t", ordering = FALSE))
})

trend_data <- reactive({
  chosen <- c(input$trend_state_a, input$trend_state_b)
  s <- pop[pop$abbr %in% chosen, , drop = FALSE]
  if (input$trend_metric == "pop_index") {
    do.call(rbind, lapply(seq_len(nrow(s)), function(i) {
      data.frame(state = s$state[i], year = 2020:2025,
                 value = 100 * (as.numeric(unlist(s[i, paste0("population_", 2020:2025)], use.names = FALSE)) /
                                  s$population_2020[i] - 1))
    }))
  } else {
    a <- merge(acs_all, pop[, c("fips", "state", "abbr")], by = "fips")
    a <- a[a$abbr %in% chosen, , drop = FALSE]
    data.frame(state = a$state, year = a$acs_year,
               value = a[[input$trend_metric]])
  }
})

output$trend_plot <- renderPlotly({
  d <- trend_data()
  d <- d[is.finite(d$value), , drop = FALSE]
  validate(need(nrow(d) > 0, "No matching annual values available."))
  d$tooltip <- paste0(d$state, " · ", d$year, ": ",
                      round(d$value, 2),
                      if (input$trend_metric == "pop_index") "%" else "")
  p <- ggplot(d, aes(x = year, y = value, color = state, group = state,
                     text = tooltip)) +
    geom_line(linewidth = 1.1) + geom_point(size = 2.9) +
    scale_color_manual(values = c("#16887e", "#dd8766")) +
    scale_x_continuous(breaks = sort(unique(d$year))) +
    labs(x = "Year", y = if (input$trend_metric == "pop_index")
      "Population change from 2020 (%)" else
        metric_labels[[input$trend_metric]], color = "State") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  ggplotly(p, tooltip = "text", source = "trends") %>%
    layout(margin = list(l = 80, r = 15, b = 55, t = 15),
           paper_bgcolor = "white", plot_bgcolor = "white")
})

output$download_master <- downloadHandler(
  filename = function() paste0("american_migration_", input$irs_period,
                               "_ACS", input$acs_year, ".csv"),
  content = function(file) write.csv(view_data(), file, row.names = FALSE)
)
}

shinyApp(ui = ui, server = server)







output$download_scatter <- downloadHandler(
  filename = function() paste0("scatter_", input$x_metric, "_vs_", input$y_metric, ".csv"),
  content = function(file) {
    d <- scatter_data()[, c("fips", "state", "abbr", "x", "y")]
    names(d)[4:5] <- c(input$x_metric, input$y_metric)
    write.csv(d, file, row.names = FALSE)
  }
)

route_data <- reactive({
  d <- irs_flows[irs_flows$period == input$irs_period, , drop = FALSE]
  s <- pop$fips[match(input$route_state, pop$abbr)]
  if (input$route_direction == "in") {
    d <- d[d$destination_fips == s, , drop = FALSE]
    d$other_state <- d$origin_state
  } else {
    d <- d[d$origin_fips == s, , drop = FALSE]
    d$other_state <- d$destination_state
  }
  head(d[order(d$individuals, decreasing = TRUE), , drop = FALSE], 10)
})

output$route_kpis <- renderUI({
  s <- view_data()
  s <- s[s$abbr == input$route_state, , drop = FALSE]
  if (!nrow(s)) return(NULL)
  div(class = "kpi-grid",
      stat_box("Incoming individuals (N2)", fmt_num(s$inflow_people)),
      stat_box("Outgoing individuals (N2)", fmt_num(s$outflow_people)),
      stat_box("Net represented individuals", fmt_signed(s$net_people, 0)),
      stat_box("Net rate per 1,000", fmt_signed(s$net_rate, 2))
  )
})

output$route_bar <- renderPlot({
  d <- route_data()
  validate(need(nrow(d) > 0, "No valid interstate route records available."))
  ggplot(d, aes(x = reorder(other_state, individuals), y = individuals)) +
    geom_col(fill = if (input$route_direction == "in") "#16887e" else "#dd8766",
             width = .75) + coord_flip() +
    labs(x = NULL, y = "Individuals represented on IRS returns (N2)") +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", trim = TRUE)) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          plot.margin = margin(10, 15, 10, 5))
})

output$route_table <- renderDT({
  d <- route_data()
  tbl <- data.frame(
    `From → To` = paste(d$origin_state, "→", d$destination_state),
    `Returns (N1)` = d$returns,
    `Individuals (N2)` = d$individuals,
    `AGI ($ millions)` = round(d$agi_thousands / 1000, 1),
    check.names = FALSE
  )
  datatable(tbl, rownames = FALSE,
            options = list(pageLength = 10, scrollX = TRUE, dom = "t"))
})

output$state_comparison <- renderDT({
  d <- view_data()
  a <- d[d$abbr == input$state_a, , drop = FALSE]
  b <- d[d$abbr == input$state_b, , drop = FALSE]
  keys <- c("net_rate", "in_rate", "out_rate", "net_people",
            "population_growth_pct", "housing_burden_pct_2024",
            "median_income", "median_home_value", "median_rent",
            "median_rent_pct", "unemployment_pct", "poverty_pct",
            "commute_minutes")
  label <- c(unname(metric_labels[c("net_rate", "in_rate", "out_rate")]),
             "Net IRS represented individuals", unname(metric_labels[keys[5:13]]))
  show_val <- function(d, k) {
    if (k == "net_people") return(fmt_signed(d$net_people, 0))
    fmt_metric(d[[k]], k)
  }
  tbl <- data.frame(
    Measure = label,
    `First state` = vapply(keys, function(k) show_val(a, k), character(1)),
    `Second state` = vapply(keys, function(k) show_val(b, k), character(1)),
    check.names = FALSE
  )
  names(tbl)[2:3] <- c(a$state, b$state)
  datatable(tbl, rownames = FALSE,
            options = list(pageLength = 15, dom = "t", ordering = FALSE))
})

trend_data <- reactive({
  chosen <- c(input$trend_state_a, input$trend_state_b)
  s <- pop[pop$abbr %in% chosen, , drop = FALSE]
  if (input$trend_metric == "pop_index") {
    do.call(rbind, lapply(seq_len(nrow(s)), function(i) {
      data.frame(state = s$state[i], year = 2020:2025,
                 value = 100 * (as.numeric(unlist(s[i, paste0("population_", 2020:2025)], use.names = FALSE)) /
                                  s$population_2020[i] - 1))
    }))
  } else {
    a <- merge(acs_all, pop[, c("fips", "state", "abbr")], by = "fips")
    a <- a[a$abbr %in% chosen, , drop = FALSE]
    data.frame(state = a$state, year = a$acs_year,
               value = a[[input$trend_metric]])
  }
})

output$trend_plot <- renderPlotly({
  d <- trend_data()
  d <- d[is.finite(d$value), , drop = FALSE]
  validate(need(nrow(d) > 0, "No matching annual values available."))
  d$tooltip <- paste0(d$state, " · ", d$year, ": ",
                      round(d$value, 2),
                      if (input$trend_metric == "pop_index") "%" else "")
  p <- ggplot(d, aes(x = year, y = value, color = state, group = state,
                     text = tooltip)) +
    geom_line(linewidth = 1.1) + geom_point(size = 2.9) +
    scale_color_manual(values = c("#16887e", "#dd8766")) +
    scale_x_continuous(breaks = sort(unique(d$year))) +
    labs(x = "Year", y = if (input$trend_metric == "pop_index")
      "Population change from 2020 (%)" else
        metric_labels[[input$trend_metric]], color = "State") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  ggplotly(p, tooltip = "text", source = "trends") %>%
    layout(margin = list(l = 80, r = 15, b = 55, t = 15),
           paper_bgcolor = "white", plot_bgcolor = "white")
})

output$download_master <- downloadHandler(
  filename = function() paste0("american_migration_", input$irs_period,
                               "_ACS", input$acs_year, ".csv"),
  content = function(file) write.csv(view_data(), file, row.names = FALSE)
)
}

shinyApp(ui = ui, server = server)
