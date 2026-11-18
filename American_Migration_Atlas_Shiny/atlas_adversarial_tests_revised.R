# American Migration Atlas — adversarial tests for the revised ZIP, 2026-11-18
# Run from the extracted project directory using:
#   source("/path/to/atlas_adversarial_tests_revised.R")
# NO APPLICATION FILES OR DATA ARE MODIFIED.
# This script uses base R and the packages already required by your app.
# It does NOT test browser layout, Plotly click behavior, or deployment.

report <- data.frame(test=character(), status=character(), detail=character(),
                     stringsAsFactors=FALSE)
check <- function(name, expr) {
  outcome <- tryCatch({force(expr); "PASS"}, error=function(e) conditionMessage(e))
  status <- if (identical(outcome, "PASS")) "PASS" else "FAIL"
  message(sprintf("[%s] %s%s", status, name,
                  if (status=="FAIL") paste0(" — ", outcome) else ""))
  report[nrow(report)+1L, ] <<- list(name, status, if (status=="FAIL") outcome else "")
  invisible(status == "PASS")
}

check("Standard Shiny project launcher app.R exists", {
  stopifnot(file.exists("app.R"))
})
check("Current project code, README and CSVs present", {
  needed <- c("atlas_server.R", "atlas_ui.R", "SHINY_SETUP.md",
              "state_population_growth_clean.csv",
              paste0("ACS_", 2019:2024, "_state_new.csv"),
              paste0("state_inflow_clean_", c("1920","2021","2122","2223"), ".csv"),
              paste0("state_outflow_clean_", c("1920","2021","2122","2223"), ".csv"))
  stopifnot(all(file.exists(needed)))
})

# Source only the underlying components; this loads data and defines server/ui.
check("Load app components", {
  source("atlas_server.R", local=FALSE)
  source("atlas_ui.R", local=FALSE)
  stopifnot(is.function(server), !is.null(ui))
})

if (exists("server", inherits=FALSE) && is.function(server)) {
  check("50 states, 300 ACS state-years, and 200 IRS state-period records", {
    stopifnot(nrow(state_population)==50L, nrow(acs_by_state_year)==300L,
              nrow(irs_state_totals)==200L)
  })
  check("State/ACS/IRS keys are unique", {
    stopifnot(!anyDuplicated(state_population$fips),
              !anyDuplicated(paste(acs_by_state_year$fips,acs_by_state_year$acs_year)),
              !anyDuplicated(paste(irs_state_totals$fips,irs_state_totals$irs_period)),
              !anyDuplicated(paste(irs_route_flows$origin_fips,
                                   irs_route_flows$destination_fips,
                                   irs_route_flows$irs_period)))
  })
  check("Every period contains 50 states", {
    stopifnot(all(table(irs_state_totals$irs_period)==50L),
              all(table(acs_by_state_year$acs_year)==50L))
  })
  check("Only valid two-character FIPS codes used in state tables", {
    stopifnot(all(nchar(state_population$fips)==2L),
              setequal(acs_by_state_year$fips, state_population$fips),
              setequal(irs_state_totals$fips, state_population$fips))
  })
  check("Population growth calculated correctly", {
    actual <- with(state_population, 100*(population_2025-population_2020)/population_2020)
    stopifnot(all(abs(actual-state_population$population_growth_pct)<0.011))
  })
  check("Nonzero population, income, and valid ratios", {
    stopifnot(all(state_population$population_2020>0),
              all(acs_by_state_year$median_household_income>0),
              all(is.finite(acs_by_state_year$median_home_value/
                              acs_by_state_year$median_household_income)))
  })
  check("ACS factor columns finite and sensible", {
    metrics <- c("acs_population", "median_household_income", "median_home_value",
                 "median_rent", "rent_share_of_income_pct", "unemployment_pct",
                 "poverty_pct", "avg_commute_minutes")
    stopifnot(all(vapply(acs_by_state_year[metrics],
                         function(x) all(is.finite(x)), logical(1))),
              all(acs_by_state_year$unemployment_pct >=0 & acs_by_state_year$unemployment_pct<=100),
              all(acs_by_state_year$poverty_pct >=0 & acs_by_state_year$poverty_pct<=100))
  })
  check("For every state/period: net = inbound minus outbound", {
    with(irs_state_totals, stopifnot(all(net_exemptions ==
                                           inflow_exemptions-outflow_exemptions)))
  })
  check("All per-1,000 measures use 2020 denominator", {
    x <- merge(irs_state_totals, state_population[, c("fips","population_2020")], by="fips")
    stopifnot(all(abs(x$net_per_1000-1000*x$net_exemptions/x$population_2020)<1e-9),
              all(abs(x$inflow_per_1000-1000*x$inflow_exemptions/x$population_2020)<1e-9),
              all(abs(x$outflow_per_1000-1000*x$outflow_exemptions/x$population_2020)<1e-9))
  })
  check("IRS missing route counters accurately track 50 possible partners", {
    for (period in irs_period_codes) {
      d <- irs_route_flows[irs_route_flows$irs_period==period,]
      s <- irs_state_totals[irs_state_totals$irs_period==period,]
      for(i in seq_len(nrow(s))) {
        f <- s$fips[i]
        ni <- sum(d$destination_fips==f & is.finite(d$exemptions))
        no <- sum(d$origin_fips==f & is.finite(d$exemptions))
        stopifnot(s$missing_inflow_routes[i]==50L-ni,
                  s$missing_outflow_routes[i]==50L-no,
                  s$has_missing_routes[i]==((ni+no)<100L))
      }
    }
  })
  check("South Carolina 2022–23 reference case", {
    x <- irs_state_totals[irs_state_totals$fips=="45" &
                            irs_state_totals$irs_period=="2223",]
    stopifnot(nrow(x)==1L, x$inflow_exemptions==178163,
              x$outflow_exemptions==119061, x$net_exemptions==59102,
              abs(x$net_per_1000-11.5164)<0.001)
  })
  check("Pearson r matches direct base-R correlation for default inputs", {
    a <- merge(state_population, acs_by_state_year[acs_by_state_year$acs_year==2024,],by="fips")
    a <- merge(a,irs_state_totals[irs_state_totals$irs_period=="2223",],by="fips")
    stopifnot(abs(safe_correlation(a$rent_share_of_income_pct,a$net_per_1000)-
                    cor(a$rent_share_of_income_pct,a$net_per_1000))<1e-12)
  })
  check("All 15 source CSV links resolve to unique existing files", {
    stopifnot(length(source_files)==15L, !anyDuplicated(source_files),
              all(file.exists(file.path(data_folder,source_files))),
              !anyDuplicated(vapply(source_files,csv_download_id,character(1))))
  })
  check("All historical and team images exist", {
    paths <- c("www/OverviewPic1.png", "www/OverviewPic2.png",
               "www/ExploreFactorsPic1.png", "www/ExploreFactorsPic2.png",
               "Head Portrait - Lucas Chacon.png", "SPIA Portraits 6-12-26-4917.jpg",
               "Diana_2.jpeg")
    stopifnot(all(file.exists(paths)))
  })
  
  # testServer requires an installed, compatible Shiny version.
  if ("testServer" %in% getNamespaceExports("shiny")) {
    check("Reactive checks across all 4 IRS periods × 6 ACS years", {
      shiny::testServer(server, {
        session$setInputs(page="overview", irs_period="2223", acs_year="2024",
                          map_metric="net_per_1000", x_metric="rent_share_of_income_pct",
                          y_metric="net_per_1000", route_state="SC", route_direction="in",
                          state_a="FL",state_b="CA",trend_metric="net_exemptions",
                          trend_state_a="FL",trend_state_b="CA")
        for(p in irs_period_codes) for(y in 2019:2024) {
          session$setInputs(irs_period=p,acs_year=as.character(y))
          v <- state_metrics()
          stopifnot(nrow(v)==50L,all(v$irs_period==p),all(v$acs_year==y),
                    !anyDuplicated(v$fips))
        }
      })
    })
    check("Explore Factors — all 8 × 5 X/Y combinations", {
      shiny::testServer(server, {
        session$setInputs(page="explore",irs_period="2223",acs_year="2024",
                          x_metric="median_household_income",y_metric="net_per_1000",
                          route_state="SC",route_direction="in",
                          trend_metric="net_exemptions",trend_state_a="FL",trend_state_b="CA")
        for(x in unname(x_axis_choices)) for(y in unname(y_axis_choices)) {
          session$setInputs(x_metric=x,y_metric=y)
          p <- scatter_data()
          stopifnot(nrow(p)==50L, all(is.finite(p$x_value)),all(is.finite(p$y_value)),
                    is.finite(safe_correlation(p$x_value,p$y_value)))
        }
      })
    })
    check("Migration routes — 4 periods × 2 directions for South Carolina", {
      shiny::testServer(server, {
        session$setInputs(page="routes",irs_period="2223",acs_year="2024",
                          route_state="SC",route_direction="in",x_metric="median_rent",
                          y_metric="net_per_1000",trend_metric="net_exemptions",
                          trend_state_a="FL",trend_state_b="CA")
        for(p in irs_period_codes) for(d in c("in","out")) {
          session$setInputs(irs_period=p,route_direction=d)
          f <- routes_for_selected_state()
          stopifnot(nrow(f)>0L,all(f$irs_period==p))
          target <- if (d=="in") "destination_fips" else "origin_fips"
          stopifnot(all(f[[target]]=="45"))
        }
      })
    })
    check("Trends — correct observation counts per selected measure", {
      shiny::testServer(server, {
        session$setInputs(page="trends",irs_period="2223",acs_year="2024",
                          route_state="SC",route_direction="in",x_metric="median_rent",
                          y_metric="net_per_1000",trend_state_a="FL",trend_state_b="CA",
                          trend_metric="net_exemptions")
        for (metric in unname(trend_choices)) {
          session$setInputs(trend_metric=metric)
          d <- trend_data()
          wanted <- if(metric=="pop_endpoints") 4L else if(metric %in% irs_metric_keys) 8L else 12L
          stopifnot(nrow(d)==wanted,all(is.finite(d$value)))
        }
        session$setInputs(trend_metric="pop_endpoints",trend_state_a="FL",trend_state_b="FL")
        stopifnot(nrow(trend_data())==2L)
      })
    })
  } else message("shiny::testServer() unavailable; reactive tests not executed.")
}

message("\nSummary: ", sum(report$status=="PASS"), " PASS; ", sum(report$status=="FAIL"), " FAIL.")
write.csv(report, "atlas_audit_test_results.csv", row.names=FALSE)
message("Saved test results: atlas_audit_test_results.csv")
if (any(report$status=="FAIL")) message("FAILURES are diagnostics, not proof that the underlying data is wrong. Review each case.")
