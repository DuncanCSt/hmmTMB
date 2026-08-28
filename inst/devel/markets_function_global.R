run_hmm_analysis <- function(
  observation,
  obs_train,
  obs_test,
  cov_train,
  cov_test,
  hidden_states = 3,
  hid_formula = as.formula("~1"),
  horseshoe = FALSE,
  plot_folder = "tsx_only_plots"
) {
  source("markets_functions.R")

  plot_train_test_samples(
    obs_train = obs_train,
    obs_test = obs_test,
    y_column = "TSX_Composite",
    savefig = file.path(plot_folder, "train_test_samples.png")
  )

  # Step one: Create working parameters via log link
  obs_train_log <- lapply(obs_train, function(x) ln_transform(x))
  obs_test_log <- lapply(1:length(obs_test),  function(i) ln_transform(
      obs_test[[i]], previous_row = tail(obs_train[[i]], 1)
    )
  )

  n_samples <- length(obs_train_log)
  n_forecasts <- nrow(obs_test_log[[1]])

  for (i in 1:n_samples) {
    plot_train_test_zoomed(
      train_data = obs_train[[i]],
      test_data = obs_test[[i]],
      observation = observation,
      zoom_range = n_forecasts + 10,
      title = paste0("Train/Test Zoomed - Sample ", i),
      savefig = paste0(plot_folder, "/train_test_zoomed_sample_", i, ".png")
    )
  }

  hmms <- lapply(1:n_samples, function(i) {
    fit_markets_hmm(
      data = cbind(obs_train_log[[i]], cov_train[[i]]),
      n_states = hidden_states,
      obs_name = observation,
      hid_formula = hid_formula,
      horseshoe = horseshoe
    )
  })

  for (i in 1:n_samples) {
    plot_tsx_state_overview(
      hmm         = hmms[[i]],             # fitted HMM object
      train_data  = obs_train[[i]],      # data used to fit the model (needs Date + TSX_Composite)
      observation = "TSX_Composite", # defaults to this, change if you need another series
      show        = "both",          # options: "both", "states", "interval"
      title       = paste0("3 State model, sample ", i),  # optional heading
      savefig     = paste0(plot_folder, "/tsx_state_overview_sample_", i, ".png")
    )
  }

  forecasts <- lapply(1:n_samples, function(n) {
    hmm <- hmms[[n]]
    forecast <- hmmTMB::Forecast$new(
      hmm,
      n = n_forecasts,
      forecast_data = cov_test[[n]],
      preset_eval_range = list(TSX_Composite=seq(-0.05, 0.05, by = 0.001))
    )
  })
  forecast_eval_range <- lapply(forecasts, function(forecast) {
    forecast$eval_range()[[observation]]
  })
  forecast_dists <- lapply(forecasts, function(forecast) {
    forecast$forecast_dists()[[observation]]
  })
  forecast_dists_normalized <- lapply(seq(1:n_samples), function(n) {
    apply(forecast_dists[[n]], 2, function(col) col / sum(col))
  })

  for (n in 1:n_samples) {
    plot_ridge_data(
      # Timesteps ahead to be plotted
      forecast_timesteps = seq(60, n_forecasts, by=20),
      forecast_steps = forecast_eval_range[[n]],
      forecast_data = forecast_dists[[n]],
      true_values = obs_test_log[[n]][[observation]],
      title = paste0("Log Return forecast. Sample ", n),
      y_label = "Log Return",
      savefig = paste0(plot_folder, "/log_return_ridge_plot_sample_", n, ".png")
    )
  }

  log_return_traces <- compute_log_loss_traces(
    true_forecast=obs_test_log,
    forecast_dis=forecast_dists,
    forecast_range=forecast_eval_range,
    observation=observation
  )
  plot_log_loss_traces(
    log_loss_results=log_return_traces,
    title="Log Loss of Log Return",
    savefig=file.path(plot_folder, "log_return_loss_traces.png")
  )

  compute_real_forecast_probs <- function(forecast_dists_normalized, forecast_eval_range, n_forecasts) {
    # Create extended working range for convolution with a zero midpoint
    step_size <- diff(forecast_eval_range)[1]
    top_range <- max(forecast_eval_range) * sqrt(n_forecasts)
    remainder <- top_range %% step_size
    convolution_range <- round(seq(-top_range+remainder, top_range-remainder, by = step_size), 5)
    convolution_length <- length(convolution_range)
    # Create range which maps back to real prices after exponential transform
    out_length <- 500
    real_forecast_range <- log(seq(exp(-top_range), exp(top_range), length.out = out_length))
    real_forecast_probs <- array(0, dim = c(out_length, n_forecasts))

    # Factor applied to maintain normalized PDF after resampling
    first_step_probs <- zoo::na.fill(approx(
      x = forecast_eval_range,
      y = forecast_dists_normalized[, 1],
      xout = convolution_range)$y
      , 0)
    current_step_probs <- first_step_probs
    real_forecast_probs[, 1] <- zoo::na.fill(approx(
      x = convolution_range,
      y = current_step_probs/step_size,
      xout = real_forecast_range)$y
      , 0)

    for (i in 2:n_forecasts) {
      # after open convolution length is len(convolution_range)+len(forecast_eval_range)- 1
      current_step_probs <- convolve(current_step_probs, forecast_dists_normalized[, i], type = "open")
      start_out  <- floor((length(current_step_probs) - convolution_length) / 2) + 1
      stop_out   <- start_out + convolution_length - 1
      current_step_probs <- current_step_probs[start_out:stop_out]
      real_forecast_probs[, i] <- zoo::na.fill(approx(
        x = convolution_range,
        y = current_step_probs/step_size,
        xout = real_forecast_range)$y
        , 0)

    }
    real_forecast_probs <- apply(real_forecast_probs, 2, function(col) col / sum(col))
    list(probs = real_forecast_probs, range = real_forecast_range, conv_range = convolution_range)
  }
  real_forecast_probs_list <- lapply(1:n_samples, function(n) {
    compute_real_forecast_probs(
      forecast_dists_normalized[[n]],
      forecast_eval_range[[n]],
      n_forecasts
    )
  })
  last_true_values <- sapply(obs_train, function(train_data) {
    tail(train_data[[observation]], 1)
  })
  real_forecast_range <- lapply(1:n_samples, function(n) {
    last_true_values[[n]] * exp(real_forecast_probs_list[[n]]$range)
  })
  real_forecast_probs <- lapply(1:n_samples, function(n) {
    real_forecast_probs_list[[n]]$probs / diff(real_forecast_range[[n]])[1]
  })

  for (n in 1:n_samples) {
    plot_ridge_data(
      # Timesteps ahead to be plotted
      forecast_timesteps = seq(60, n_forecasts, by=20),
      forecast_steps = real_forecast_range[[n]],
      forecast_data = real_forecast_probs[[n]],
      true_values = obs_test[[n]][[observation]],
      title = paste0("True Forecast vs True Observations. Sample ", n),
      y_label = "Price"
      #savefig = paste0(plot_folder, "/price_forecast_ridge_plot_sample_", n, ".png")
    )
  }

  price_log_traces <- compute_log_loss_traces(
    true_forecast=obs_test,
    forecast_dis=real_forecast_probs,
    forecast_range=real_forecast_range,
    observation=observation
  )
  plot_log_loss_traces(
    log_loss_results=price_log_traces,
    title="Log Loss of Price",
    savefig=file.path(plot_folder, "price_log_loss_traces.png")
  )

  return(list(
    hmms = hmms,
    forecasts = forecasts,
    log_return_traces = log_return_traces,
    price_log_traces = price_log_traces
  ))
}