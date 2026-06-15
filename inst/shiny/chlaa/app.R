required_packages <- c("shiny", "ggplot2", "dplyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Install missing packages before running this app: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

app_file <- function(...) {
  candidates <- c(
    file.path(getwd(), ...),
    file.path(getwd(), "inst", "shiny", "chlaa", ...),
    system.file("shiny", "chlaa", ..., package = "chlaa")
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0) stop("Could not find app file: ", file.path(...), call. = FALSE)
  candidates[[1]]
}

repo_root <- function() {
  roots <- unique(normalizePath(c(
    getwd(),
    file.path(getwd(), "..", "..", ".."),
    file.path(getwd(), "..", "..")
  ), mustWork = FALSE))
  roots[file.exists(file.path(roots, "DESCRIPTION")) & file.exists(file.path(roots, "R"))][1]
}

load_chlaa <- function() {
  root <- repo_root()
  if (!is.na(root) && requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(root, quiet = TRUE)
    return(invisible(TRUE))
  }
  if (!requireNamespace("chlaa", quietly = TRUE)) {
    stop("Install chlaa or run the app from the package source tree.", call. = FALSE)
  }
  invisible(TRUE)
}

extdata_file <- function(...) {
  candidates <- c(
    system.file("extdata", ..., package = "chlaa"),
    file.path(getwd(), "inst", "extdata", ...),
    file.path(getwd(), "..", "..", "..", "inst", "extdata", ...)
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0) stop("Could not find extdata file: ", file.path(...), call. = FALSE)
  candidates[[1]]
}

load_chlaa()
kirotshe_bundle <- readRDS(extdata_file("kirotshe_particle_fit.rds"))
kirotshe_fit <- kirotshe_bundle$fit
kirotshe_base_pars <- kirotshe_bundle$pars
kirotshe_observed <- as.data.frame(kirotshe_bundle$observed)
kirotshe_burnin <- kirotshe_bundle$burnin
kirotshe_display_pars <- chlaa::chlaa_update_from_fit(
  fit = kirotshe_fit,
  pars = kirotshe_base_pars,
  draw = "median",
  burnin = kirotshe_burnin
)

no_intervention_modify <- function() {
  list(
    chlor_start = 0, chlor_end = 0, chlor_effect = 0,
    hyg_start = 0, hyg_end = 0, hyg_effect = 0,
    lat_start = 0, lat_end = 0, lat_effect = 0,
    cati_start = 0, cati_end = 0, cati_effect = 0,
    orc_start = 0, orc_end = 0, orc_capacity = 0,
    ctc_start = 0, ctc_end = 0, ctc_capacity = 0,
    vax1_start = 0, vax1_end = 0, vax1_doses_per_day = 0, vax1_total_doses = 0,
    vax2_start = 0, vax2_end = 0, vax2_doses_per_day = 0, vax2_total_doses = 0
  )
}

custom_intervention_aliases <- function() {
  c(
    chlorination = "chlor", chlor = "chlor", chlorine = "chlor",
    hygiene = "hyg", hyg = "hyg",
    latrine = "lat", latrines = "lat", lat = "lat",
    cati = "cati",
    orc = "orc", oral_rehydration_point = "orc",
    ctc = "ctc", cholera_treatment_centre = "ctc", cholera_treatment_center = "ctc",
    vax1 = "vax1", vaccine1 = "vax1", vaccination1 = "vax1", dose1 = "vax1", dose_1 = "vax1",
    vax2 = "vax2", vaccine2 = "vax2", vaccination2 = "vax2", dose2 = "vax2", dose_2 = "vax2"
  )
}

normalise_intervention_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

parse_intervention_numeric <- function(x, field, rows) {
  x_chr <- trimws(as.character(x))
  missing <- is.na(x) | !nzchar(x_chr) | tolower(x_chr) %in% c("na", "nan")
  out <- rep(NA_real_, length(x_chr))
  parsed <- suppressWarnings(as.numeric(x_chr[!missing]))
  bad <- !missing
  bad[!missing] <- is.na(parsed) | !is.finite(parsed)
  if (any(bad)) {
    bad_row <- rows[which(bad)[[1]]]
    bad_value <- x_chr[which(bad)[[1]]]
    stop(
      sprintf(
        "row %s column '%s' must be numeric or blank; found '%s'",
        bad_row, field, bad_value
      ),
      call. = FALSE
    )
  }
  out[!missing] <- parsed
  out
}

require_intervention_value <- function(value, row, field, intervention) {
  if (length(value) != 1 || is.na(value)) {
    stop(
      sprintf("row %s (%s) is missing required '%s'", row, intervention, field),
      call. = FALSE
    )
  }
  value
}

check_intervention_fraction <- function(value, row, field, intervention) {
  if (value < 0 || value > 1) {
    stop(
      sprintf("row %s (%s) column '%s' must be between 0 and 1", row, intervention, field),
      call. = FALSE
    )
  }
  value
}

check_intervention_nonnegative <- function(value, row, field, intervention) {
  if (!is.na(value) && value < 0) {
    stop(
      sprintf("row %s (%s) column '%s' must be non-negative", row, intervention, field),
      call. = FALSE
    )
  }
  value
}

custom_intervention_overrides <- function(dat) {
  if (is.null(dat)) {
    out <- list()
    attr(out, "n_interventions") <- 0L
    return(out)
  }
  if (!is.data.frame(dat) || nrow(dat) == 0) {
    out <- list()
    attr(out, "n_interventions") <- 0L
    return(out)
  }

  names(dat) <- normalise_intervention_name(names(dat))
  if (!"intervention" %in% names(dat)) {
    stop(
      "missing required 'intervention' column; expected columns are intervention,start,end,effect,capacity,total_doses,doses_per_day",
      call. = FALSE
    )
  }

  numeric_fields <- c("start", "end", "effect", "capacity", "total_doses", "doses_per_day")
  row_ids <- seq_len(nrow(dat))
  for (field in numeric_fields) {
    if (!field %in% names(dat)) dat[[field]] <- NA_character_
    dat[[field]] <- parse_intervention_numeric(dat[[field]], field, row_ids)
  }

  aliases <- custom_intervention_aliases()
  raw_names <- trimws(as.character(dat$intervention))
  empty_names <- !nzchar(raw_names) | is.na(raw_names)
  if (any(empty_names)) {
    stop(sprintf("row %s is missing required 'intervention'", which(empty_names)[[1]]), call. = FALSE)
  }

  lookup_names <- normalise_intervention_name(raw_names)
  unknown <- setdiff(unique(lookup_names), names(aliases))
  if (length(unknown) > 0) {
    stop(
      "unknown intervention name(s): ",
      paste(unknown, collapse = ", "),
      ". Supported names include chlorination, hygiene, latrine, CATI, ORC, CTC, vax1, and vax2.",
      call. = FALSE
    )
  }

  dat$intervention <- unname(aliases[lookup_names])
  duplicates <- unique(dat$intervention[duplicated(dat$intervention)])
  if (length(duplicates) > 0) {
    stop(
      "duplicate intervention row(s) are not supported for: ",
      paste(duplicates, collapse = ", "),
      call. = FALSE
    )
  }

  overrides <- list()
  for (i in seq_len(nrow(dat))) {
    row <- dat[i, , drop = FALSE]
    intervention <- row$intervention[[1]]
    start <- require_intervention_value(row$start[[1]], i, "start", intervention)
    end <- require_intervention_value(row$end[[1]], i, "end", intervention)
    if (end < start) {
      stop(sprintf("row %s (%s) has end before start", i, intervention), call. = FALSE)
    }

    if (intervention %in% c("chlor", "hyg", "lat", "cati")) {
      effect <- require_intervention_value(row$effect[[1]], i, "effect", intervention)
      effect <- check_intervention_fraction(effect, i, "effect", intervention)
      prefix <- c(chlor = "chlor", hyg = "hyg", lat = "lat", cati = "cati")[[intervention]]
      overrides[[paste0(prefix, "_start")]] <- start
      overrides[[paste0(prefix, "_end")]] <- end
      overrides[[paste0(prefix, "_effect")]] <- effect
    } else if (intervention %in% c("orc", "ctc")) {
      capacity <- check_intervention_nonnegative(row$capacity[[1]], i, "capacity", intervention)
      overrides[[paste0(intervention, "_start")]] <- start
      overrides[[paste0(intervention, "_end")]] <- end
      if (!is.na(capacity)) overrides[[paste0(intervention, "_capacity")]] <- capacity
    } else if (intervention %in% c("vax1", "vax2")) {
      total_doses <- check_intervention_nonnegative(row$total_doses[[1]], i, "total_doses", intervention)
      doses_per_day <- check_intervention_nonnegative(row$doses_per_day[[1]], i, "doses_per_day", intervention)
      campaign_days <- max(1, end - start)
      if (is.na(total_doses) && is.na(doses_per_day)) {
        stop(
          sprintf("row %s (%s) must provide total_doses or doses_per_day", i, intervention),
          call. = FALSE
        )
      }
      if (is.na(doses_per_day)) doses_per_day <- total_doses / campaign_days
      if (is.na(total_doses)) total_doses <- doses_per_day * campaign_days
      overrides[[paste0(intervention, "_start")]] <- start
      overrides[[paste0(intervention, "_end")]] <- end
      overrides[[paste0(intervention, "_total_doses")]] <- total_doses
      overrides[[paste0(intervention, "_doses_per_day")]] <- doses_per_day
    }
  }

  attr(overrides, "n_interventions") <- nrow(dat)
  overrides
}

intervention_schema_note <- function() {
  shiny::tags$div(
    class = "help-block",
    shiny::tags$p("Optional long-form intervention CSV schema:"),
    shiny::tags$pre(
      paste(
        "intervention,start,end,effect,capacity,total_doses,doses_per_day",
        "chlorination,126,238,0.20,,,",
        "ORC,77,231,,500,,",
        "vax1,140,168,,,40000,1428.57",
        sep = "\n"
      )
    ),
    shiny::tags$p(
      "Supported interventions: chlorination, hygiene, latrine, CATI, ORC, CTC, vax1, vax2. ",
      "Leave unused numeric columns blank."
    )
  )
}

clean_scenario_name <- function(x) {
  x <- trimws(x)
  if (!nzchar(x)) x <- paste0("scenario_", format(Sys.time(), "%H%M%S"))
  x <- gsub("[^A-Za-z0-9_ -]+", "", x)
  x <- gsub("[ -]+", "_", x)
  x
}

obs_interval_from_fit <- function(fit) {
  interval <- attr(fit, "obs_interval", exact = TRUE)
  if (is.null(interval) || !is.finite(interval)) 1 else as.numeric(interval)
}

obs_case_var <- function(interval) {
  if (identical(as.numeric(interval), 7)) "inc_symptoms_weekly" else "inc_symptoms"
}

first_trigger_time <- function(observed, threshold = 50) {
  if (!is.data.frame(observed) || !all(c("time", "cases") %in% names(observed))) return(7)
  idx <- which(observed$cases >= threshold)
  if (length(idx) == 0) return(min(observed$time, na.rm = TRUE))
  observed$time[[idx[[1]]]]
}

scenario_modify_from_inputs <- function(input, pars) {
  trigger <- input$trigger_day
  duration <- input$response_duration
  end <- trigger + duration
  out <- list()

  if (isTRUE(input$use_wash)) {
    start <- trigger + input$wash_offset
    finish <- start + duration
    out <- utils::modifyList(out, list(
      chlor_start = start, chlor_end = finish, chlor_effect = input$wash_effect,
      hyg_start = start, hyg_end = finish, hyg_effect = input$wash_effect,
      lat_start = start, lat_end = finish, lat_effect = input$latrine_effect,
      cati_start = start, cati_end = finish, cati_effect = input$cati_effect
    ))
  }

  if (isTRUE(input$use_care)) {
    start <- trigger + input$care_offset
    finish <- start + duration
    out <- utils::modifyList(out, list(
      orc_start = start, orc_end = finish, orc_capacity = input$orc_capacity,
      ctc_start = start, ctc_end = finish, ctc_capacity = input$ctc_capacity
    ))
  }

  if (isTRUE(input$use_vaccine)) {
    start <- trigger + input$vax_delay
    campaign_days <- max(1, input$vax_campaign_days)
    finish <- start + campaign_days
    total_doses <- input$vax_total_doses
    out <- utils::modifyList(out, list(
      vax1_start = start,
      vax1_end = finish,
      vax1_total_doses = total_doses,
      vax1_doses_per_day = total_doses / campaign_days,
      vax2_start = 0,
      vax2_end = 0,
      vax2_total_doses = 0,
      vax2_doses_per_day = 0
    ))
  }

  if (length(out) == 0) out <- list(note = "No intervention parameter changes selected")
  out[names(out) != "note"]
}

scenario_metrics <- function(entries, selected) {
  entries <- entries[names(entries) %in% selected]
  if (length(entries) == 0) return(data.frame())

  rows <- lapply(entries, function(entry) {
    fc <- entry$forecast
    end_time <- max(fc$time[fc$type == "absolute" & fc$variable == "cum_symptoms"], na.rm = TRUE)
    cum <- fc[fc$type == "absolute" & fc$variable == "cum_symptoms" & fc$time == end_time, , drop = FALSE]
    no_int <- cum$mean[cum$scenario == "no_interventions"][1]
    sc <- cum[cum$scenario == entry$name, , drop = FALSE]
    if (nrow(sc) == 0) return(NULL)
    reduction <- if (is.finite(no_int) && no_int > 0) 100 * (no_int - sc$mean[[1]]) / no_int else NA_real_
    data.frame(
      scenario = entry$name,
      cumulative_cases = round(sc$mean[[1]], 1),
      reduction_vs_no_intervention_pct = round(reduction, 1),
      n_draws = attr(fc, "n_draws"),
      created = entry$created,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

combined_end_cumulative <- function(forecast) {
  if (is.null(forecast)) return(data.frame())
  df <- forecast[
    forecast$type == "absolute" & forecast$variable == "cum_symptoms",
    ,
    drop = FALSE
  ]
  if (nrow(df) == 0) return(data.frame())

  end_time <- max(df$time, na.rm = TRUE)
  df <- df[df$time == end_time, , drop = FALSE]
  df$scenario <- factor(df$scenario, levels = unique(df$scenario))
  df
}

plot_cumulative_cases <- function(forecast) {
  df <- combined_end_cumulative(forecast)
  if (nrow(df) == 0) return(NULL)

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$scenario, y = .data$q0p5, colour = .data$scenario)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$q0p025, ymax = .data$q0p975),
      width = 0.18,
      linewidth = 0.7
    ) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Cumulative symptomatic cases",
      title = "Cumulative cases"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

plot_reduction_vs_no_intervention <- function(forecast) {
  df <- combined_end_cumulative(forecast)
  if (nrow(df) == 0) return(NULL)
  no_int <- df[df$scenario == "no_interventions", , drop = FALSE]
  if (nrow(no_int) == 0 || !is.finite(no_int$mean[[1]]) || no_int$mean[[1]] <= 0) return(NULL)

  df <- df[df$scenario != "no_interventions", , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  df$reduction_pct <- 100 * (no_int$mean[[1]] - df$mean) / no_int$mean[[1]]
  df$scenario <- factor(df$scenario, levels = rev(as.character(df$scenario)))

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$scenario, y = .data$reduction_pct, fill = .data$scenario)
  ) +
    ggplot2::geom_col(width = 0.65, alpha = 0.82) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Reduction vs no intervention (%)",
      title = "Cases averted"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

combine_forecasts <- function(entries, selected) {
  entries <- entries[names(entries) %in% selected]
  if (length(entries) == 0) return(NULL)

  first <- entries[[1]]$forecast
  baseline <- first[first$scenario %in% c("fitted_response", "no_interventions"), , drop = FALSE]
  custom <- lapply(entries, function(entry) {
    entry$forecast[entry$forecast$scenario == entry$name, , drop = FALSE]
  })
  out <- do.call(rbind, c(list(baseline), custom))
  attr(out, "baseline_name") <- "fitted_response"
  out
}

parameters_table <- function(entries) {
  if (length(entries) == 0) return(data.frame())
  rows <- lapply(entries, function(entry) {
    modify <- entry$modify
    if (length(modify) == 0) {
      return(data.frame(scenario = entry$name, parameter = NA_character_, value = NA_real_))
    }
    data.frame(
      scenario = entry$name,
      parameter = names(modify),
      value = unlist(modify, use.names = FALSE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

readme_tab <- function() {
  readme <- app_file("README.md")
  if (requireNamespace("markdown", quietly = TRUE)) {
    shiny::includeMarkdown(readme)
  } else {
    shiny::tags$pre(paste(readLines(readme, warn = FALSE), collapse = "\n"))
  }
}

scenario_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 3,
      shiny::wellPanel(
        shiny::uiOutput(ns("fit_summary")),
        shiny::textInput(ns("scenario_name"), "Scenario name", "early_combined_response"),
        shiny::numericInput(ns("trigger_day"), "Trigger day", value = 56, min = 0, step = 7),
        shiny::numericInput(ns("response_duration"), "Response duration (days)", value = 120, min = 1, step = 7),
        shiny::numericInput(ns("horizon_weeks"), "Extra forecast horizon (weeks)", value = 26, min = 0, step = 1),
        shiny::hr(),
        shiny::checkboxInput(ns("use_wash"), "WASH / CATI package", TRUE),
        shiny::numericInput(ns("wash_offset"), "WASH start offset from trigger", value = 0, step = 7),
        shiny::sliderInput(ns("wash_effect"), "Chlorination/hygiene effect", min = 0, max = 0.8, value = 0.2, step = 0.01),
        shiny::sliderInput(ns("latrine_effect"), "Latrine effect", min = 0, max = 0.8, value = 0.1, step = 0.01),
        shiny::sliderInput(ns("cati_effect"), "CATI effect", min = 0, max = 0.8, value = 0.1, step = 0.01),
        shiny::hr(),
        shiny::checkboxInput(ns("use_care"), "Case-management capacity", TRUE),
        shiny::numericInput(ns("care_offset"), "Care start offset from trigger", value = 0, step = 7),
        shiny::numericInput(ns("orc_capacity"), "ORC capacity", value = 500, min = 0, step = 25),
        shiny::numericInput(ns("ctc_capacity"), "CTC capacity", value = 100, min = 0, step = 25),
        shiny::hr(),
        shiny::checkboxInput(ns("use_vaccine"), "One-dose vaccination", FALSE),
        shiny::numericInput(ns("vax_delay"), "Vaccination delay from trigger", value = 14, step = 7),
        shiny::numericInput(ns("vax_campaign_days"), "Campaign duration (days)", value = 28, min = 1, step = 1),
        shiny::numericInput(ns("vax_total_doses"), "Total dose-1 supply", value = 100000, min = 0, step = 1000),
        shiny::hr(),
        shiny::numericInput(ns("n_draws"), "Posterior draws per run", value = 20, min = 1, step = 5),
        shiny::numericInput(ns("seed"), "Simulation seed", value = 12, min = 1, step = 1),
        shiny::checkboxInput(ns("deterministic"), "Deterministic process model", FALSE),
        shiny::actionButton(ns("run_scenario"), "Run and save scenario", class = "btn-primary"),
        shiny::br(),
        shiny::br(),
        shiny::downloadButton(ns("download_params"), "Download parameter sets"),
        shiny::downloadButton(ns("download_outputs"), "Download outputs")
      )
    ),
    shiny::column(
      width = 9,
      shiny::plotOutput(ns("fit_plot"), height = "430px"),
      shiny::fluidRow(
        shiny::column(width = 6, shiny::plotOutput(ns("cumulative_plot"), height = "260px")),
        shiny::column(width = 6, shiny::plotOutput(ns("reduction_plot"), height = "260px"))
      ),
      shiny::uiOutput(ns("saved_selector")),
      shiny::tableOutput(ns("metrics_table")),
      shiny::tableOutput(ns("parameter_table"))
    )
  )
}

scenario_server <- function(id, fit_state) {
  shiny::moduleServer(id, function(input, output, session) {
    saved <- shiny::reactiveVal(list())
    default_forecast <- shiny::reactiveVal(NULL)

    shiny::observeEvent(fit_state(), {
      fs <- fit_state()
      if (!isTRUE(fs$ready)) return()
      observed <- fs$observed
      interval <- obs_interval_from_fit(fs$fit)
      shiny::updateNumericInput(session, "trigger_day", value = first_trigger_time(observed))
      shiny::updateNumericInput(session, "orc_capacity", value = fs$display_pars$orc_capacity %||% 500)
      shiny::updateNumericInput(session, "ctc_capacity", value = fs$display_pars$ctc_capacity %||% 100)
      shiny::updateNumericInput(session, "vax_total_doses", value = floor(0.2 * fs$display_pars$N))

      fc <- try(chlaa::chlaa_forecast_from_fit(
        fit = fs$fit,
        pars = fs$pars,
        time = observed$time,
        vars = obs_case_var(interval),
        include_cases = TRUE,
        obs_interval = interval,
        obs_model = "mean",
        n_draws = 20,
        burnin = fs$burnin,
        seed = 10,
        dt = 1
      ), silent = TRUE)
      if (!inherits(fc, "try-error")) default_forecast(fc)
    }, ignoreNULL = FALSE)

    output$fit_summary <- shiny::renderUI({
      fs <- fit_state()
      if (!isTRUE(fs$ready)) {
        return(shiny::tags$p("Load or fit data to enable scenarios."))
      }
      shiny::tagList(
        shiny::tags$strong(fs$label),
        shiny::tags$p(sprintf(
          "%s observations, time %s to %s.",
          nrow(fs$observed), min(fs$observed$time), max(fs$observed$time)
        ))
      )
    })

    shiny::observeEvent(input$run_scenario, {
      fs <- fit_state()
      shiny::validate(shiny::need(isTRUE(fs$ready), "Load or fit a model before running scenarios."))
      scenario_name <- clean_scenario_name(input$scenario_name)
      interval <- obs_interval_from_fit(fs$fit)
      observed <- fs$observed
      horizon <- max(observed$time, na.rm = TRUE) + input$horizon_weeks * 7
      scenario_time <- seq(min(observed$time, na.rm = TRUE), horizon, by = interval)
      modify <- scenario_modify_from_inputs(input, fs$display_pars)
      scenarios <- list(
        chlaa::chlaa_scenario("no_interventions", no_intervention_modify()),
        chlaa::chlaa_scenario(scenario_name, modify)
      )

      forecast <- shiny::withProgress(message = "Running scenario forecast", value = 0.2, {
        chlaa::chlaa_forecast_scenarios_from_fit(
          fit = fs$fit,
          pars = fs$pars,
          scenarios = scenarios,
          baseline_name = "fitted_response",
          time = scenario_time,
          vars = c(obs_case_var(interval), "cum_symptoms", "cum_deaths"),
          include_cases = TRUE,
          obs_interval = interval,
          obs_model = "mean",
          n_draws = input$n_draws,
          burnin = fs$burnin,
          seed = input$seed,
          dt = 1,
          n_particles = 1,
          deterministic = input$deterministic
        )
      })

      entries <- saved()
      entries[[scenario_name]] <- list(
        name = scenario_name,
        modify = modify,
        forecast = forecast,
        created = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      saved(entries)
    })

    output$saved_selector <- shiny::renderUI({
      entries <- saved()
      choices <- names(entries)
      if (length(choices) == 0) {
        return(shiny::tags$p("No saved scenarios yet. Run a scenario to add it to the plot and tables."))
      }
      shiny::checkboxGroupInput(
        session$ns("show_scenarios"),
        "Saved scenarios shown on plot",
        choices = choices,
        selected = choices
      )
    })

    output$fit_plot <- shiny::renderPlot({
      fs <- fit_state()
      shiny::validate(shiny::need(isTRUE(fs$ready), "Load or fit a model to show the fit."))
      selected <- input$show_scenarios
      combined <- combine_forecasts(saved(), selected)
      if (!is.null(combined)) {
        chlaa::chlaa_plot_scenario_forecasts(
          combined,
          var = "cases",
          type = "absolute",
          data = fs$observed,
          data_y = "cases"
        )
      } else {
        fc <- default_forecast()
        shiny::validate(shiny::need(!is.null(fc), "Preparing the default model-fit plot."))
        chlaa::chlaa_plot_forecast(
          fc,
          var = "cases",
          data = fs$observed,
          data_y = "cases"
        )
      }
    })

    output$metrics_table <- shiny::renderTable({
      scenario_metrics(saved(), input$show_scenarios)
    }, striped = TRUE, bordered = TRUE)

    output$cumulative_plot <- shiny::renderPlot({
      combined <- combine_forecasts(saved(), input$show_scenarios)
      shiny::validate(shiny::need(!is.null(combined), "Run a scenario to compare cumulative cases."))
      p <- plot_cumulative_cases(combined)
      shiny::validate(shiny::need(!is.null(p), "Cumulative cases are not available for these runs."))
      p
    })

    output$reduction_plot <- shiny::renderPlot({
      combined <- combine_forecasts(saved(), input$show_scenarios)
      shiny::validate(shiny::need(!is.null(combined), "Run a scenario to compare reductions."))
      p <- plot_reduction_vs_no_intervention(combined)
      shiny::validate(shiny::need(!is.null(p), "No-intervention comparison is not available for these runs."))
      p
    })

    output$parameter_table <- shiny::renderTable({
      entries <- saved()
      if (length(entries) == 0) return(data.frame())
      parameters_table(entries[names(entries) %in% input$show_scenarios])
    }, striped = TRUE, bordered = TRUE)

    output$download_params <- shiny::downloadHandler(
      filename = function() paste0("chlaa_scenario_parameters_", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(parameters_table(saved()), file, row.names = FALSE)
    )

    output$download_outputs <- shiny::downloadHandler(
      filename = function() paste0("chlaa_scenario_outputs_", Sys.Date(), ".rds"),
      content = function(file) saveRDS(saved(), file)
    )
  })
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

custom_data_ui <- function() {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::wellPanel(
          shiny::fileInput("custom_file", "CSV case data", accept = c(".csv", "text/csv")),
          shiny::uiOutput("custom_columns"),
          shiny::fileInput("custom_interventions_file", "CSV interventions (optional)", accept = c(".csv", "text/csv")),
          intervention_schema_note(),
          shiny::selectInput("custom_obs_interval", "Observation interval", choices = c("weekly" = 7, "daily" = 1), selected = 7),
          shiny::numericInput("custom_population", "Population size", value = 100000, min = 1, step = 1000),
          shiny::numericInput("custom_initial_exposed", "Initial exposed", value = 10, min = 0, step = 1),
          shiny::numericInput("custom_n_steps", "MCMC steps", value = 200, min = 10, step = 50),
          shiny::numericInput("custom_n_particles", "Filter particles", value = 25, min = 1, step = 5),
          shiny::numericInput("custom_n_chains", "Chains", value = 1, min = 1, step = 1),
          shiny::numericInput("custom_fit_seed", "Fit seed", value = 101, min = 1, step = 1),
          shiny::checkboxInput("custom_deterministic_fit", "Deterministic fitting likelihood", TRUE),
          shiny::actionButton("custom_fit_button", "Fit model", class = "btn-primary"),
          shiny::br(),
          shiny::br(),
          shiny::verbatimTextOutput("custom_fit_status")
        )
      ),
      shiny::column(
        width = 9,
        shiny::plotOutput("custom_data_plot", height = "260px")
      )
    ),
    shiny::hr(),
    scenario_ui("custom_scenarios")
  )
}

ui <- shiny::navbarPage(
  title = "chlaa",
  shiny::tabPanel("README", readme_tab()),
  shiny::tabPanel("Kirotshe model fit", scenario_ui("kirotshe")),
  shiny::tabPanel("Fit your own data", custom_data_ui())
)

server <- function(input, output, session) {
  kirotshe_state <- shiny::reactive({
    list(
      ready = TRUE,
      label = "Bundled Kirotshe pMCMC fit",
      fit = kirotshe_fit,
      pars = kirotshe_base_pars,
      display_pars = kirotshe_display_pars,
      observed = kirotshe_observed,
      burnin = kirotshe_burnin
    )
  })

  scenario_server("kirotshe", kirotshe_state)

  custom_raw <- shiny::reactive({
    req <- input$custom_file
    if (is.null(req)) return(NULL)
    utils::read.csv(req$datapath, stringsAsFactors = FALSE)
  })

  custom_interventions_raw <- shiny::reactive({
    req <- input$custom_interventions_file
    if (is.null(req)) return(NULL)
    utils::read.csv(req$datapath, stringsAsFactors = FALSE, check.names = FALSE)
  })

  output$custom_columns <- shiny::renderUI({
    dat <- custom_raw()
    if (is.null(dat)) return(shiny::tags$p("Upload a CSV with time and case-count columns."))
    cols <- names(dat)
    shiny::tagList(
      shiny::selectInput("custom_time_col", "Time column", choices = cols, selected = cols[[1]]),
      shiny::selectInput("custom_cases_col", "Cases column", choices = cols, selected = cols[min(2, length(cols))])
    )
  })

  output$custom_data_plot <- shiny::renderPlot({
    dat <- custom_raw()
    shiny::validate(shiny::need(!is.null(dat), "Upload a CSV to preview observed cases."))
    shiny::validate(shiny::need(!is.null(input$custom_time_col), "Choose the time column."))
    shiny::validate(shiny::need(!is.null(input$custom_cases_col), "Choose the cases column."))
    prep <- chlaa::chlaa_prepare_data(dat, input$custom_time_col, input$custom_cases_col)
    ggplot2::ggplot(prep, ggplot2::aes(x = .data$time, y = .data$cases)) +
      ggplot2::geom_col(fill = "grey55") +
      ggplot2::labs(x = "Time", y = "Cases", title = "Uploaded case series") +
      ggplot2::theme_minimal()
  })

  custom_fit <- shiny::reactiveVal(NULL)
  custom_status <- shiny::reactiveVal("No custom fit has been run yet.")

  shiny::observeEvent(input$custom_fit_button, {
    dat <- custom_raw()
    shiny::validate(shiny::need(!is.null(dat), "Upload a CSV before fitting."))
    prep <- chlaa::chlaa_prepare_data(dat, input$custom_time_col, input$custom_cases_col)
    intervention_overrides <- tryCatch(
      custom_intervention_overrides(custom_interventions_raw()),
      error = function(e) e
    )
    if (inherits(intervention_overrides, "error")) {
      custom_status(paste("Intervention CSV error:", conditionMessage(intervention_overrides)))
      return()
    }
    n_interventions <- attr(intervention_overrides, "n_interventions", exact = TRUE) %||% 0L

    pars <- tryCatch(
      do.call(
        chlaa::chlaa_parameters,
        c(
          list(
            N = input$custom_population,
            E0 = input$custom_initial_exposed
          ),
          intervention_overrides
        )
      ),
      error = function(e) e
    )
    if (inherits(pars, "error")) {
      custom_status(paste("Parameter error:", conditionMessage(pars)))
      return()
    }
    interval <- as.numeric(input$custom_obs_interval)

    custom_status("Fitting model...")
    fit <- try(shiny::withProgress(message = "Fitting custom data", value = 0.1, {
      chlaa::chlaa_fit_pmcmc(
        data = prep,
        pars = pars,
        n_particles = input$custom_n_particles,
        n_steps = input$custom_n_steps,
        n_chains = input$custom_n_chains,
        seed = input$custom_fit_seed,
        obs_interval = interval,
        deterministic = input$custom_deterministic_fit
      )
    }), silent = TRUE)

    if (inherits(fit, "try-error")) {
      custom_status(paste("Fit failed:", conditionMessage(attr(fit, "condition"))))
      return()
    }

    display_pars <- try(chlaa::chlaa_update_from_fit(fit, pars, draw = "median", burnin = 0.25), silent = TRUE)
    if (inherits(display_pars, "try-error")) display_pars <- pars

    custom_fit(list(
      ready = TRUE,
      label = "Custom uploaded-data fit",
      fit = fit,
      pars = pars,
      display_pars = display_pars,
      observed = prep,
      burnin = 0.25
    ))
    custom_status(sprintf(
      "Fit complete: %s observations, %s MCMC steps, %s intervention row(s).",
      nrow(prep), input$custom_n_steps, n_interventions
    ))
  })

  output$custom_fit_status <- shiny::renderText(custom_status())

  custom_state <- shiny::reactive({
    fit <- custom_fit()
    if (is.null(fit)) {
      return(list(ready = FALSE, label = "No custom model fit", observed = data.frame()))
    }
    fit
  })

  scenario_server("custom_scenarios", custom_state)
}

shiny::shinyApp(ui = ui, server = server)
