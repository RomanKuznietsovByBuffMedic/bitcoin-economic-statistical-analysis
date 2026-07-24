# Price-return charts -----------------------------------------------------
#
# This module contains only charts for prepared price and return data.
# Shared rendering and themes are defined in R/book_charts.R.

complete_hourly_price_return_grid <- function(data) {
  if (nrow(data) == 0L) {
    stop("Неможливо побудувати графік для порожнього набору даних.")
  }

  required_columns <- c(
    "open_time",
    "price_quote_per_btc",
    "simple_return_1h",
    "log_return_1h"
  )
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "Для графіка бракує полів: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  observed_values <- data |>
    dplyr::select(dplyr::all_of(required_columns)) |>
    dplyr::arrange(open_time) |>
    dplyr::distinct(open_time, .keep_all = TRUE)

  hourly_grid <- tibble::tibble(
    open_time = seq.POSIXt(
      from = min(observed_values$open_time),
      to = max(observed_values$open_time),
      by = "hour"
    )
  )

  hourly_grid |>
    dplyr::left_join(observed_values, by = "open_time") |>
    dplyr::mutate(
      simple_return_percent = 100 * simple_return_1h,
      log_return_percent = 100 * log_return_1h
    )
}

validate_display_btc_amount <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  satoshis <- value * 1e8

  if (
    length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 1e-8 ||
      value > 1 ||
      abs(satoshis - round(satoshis)) > 1e-6
  ) {
    stop(
      paste(
        "Масштаб ціни має бути від 0.00000001 до 1 BTC",
        "і містити цілу кількість сатоші."
      )
    )
  }

  value
}

btc_amount_in_satoshis <- function(value) {
  value <- validate_display_btc_amount(value)
  as.integer(round(value * 1e8))
}

format_btc_amount_uk <- function(value) {
  value <- validate_display_btc_amount(value)
  value_text <- format(
    value,
    scientific = FALSE,
    trim = TRUE,
    digits = 8
  )
  sub(".", ",", value_text, fixed = TRUE)
}

time_range_slider <- function() {
  list(
    visible = TRUE,
    thickness = 0.12,
    borderwidth = 1
  )
}

trace_toggle_menu <- function(
  label,
  trace_index,
  position
) {
  x_positions <- c(0.32, 0.68)
  if (!position %in% seq_along(x_positions)) {
    stop("Некоректна позиція кнопки графіка.")
  }

  list(
    name = paste0("book-series-toggle-", trace_index),
    type = "buttons",
    direction = "left",
    x = x_positions[[position]],
    xanchor = "center",
    y = 1.02,
    yanchor = "bottom",
    pad = list(r = 0, t = 0, l = 0, b = 0),
    font = list(size = 12),
    showactive = FALSE,
    active = 0,
    buttons = list(
      list(
        label = paste("●", label),
        method = "restyle",
        args = list("visible", TRUE, list(trace_index)),
        args2 = list("visible", FALSE, list(trace_index))
      )
    )
  )
}

make_price_widget <- function(
  data,
  display_amount_btc = 0.001,
  quote_currency = "USDT",
  market_label = "BTC/USDT"
) {
  chart_data <- complete_hourly_price_return_grid(data)
  display_amount_btc <- validate_display_btc_amount(
    display_amount_btc
  )
  display_satoshis <- btc_amount_in_satoshis(
    display_amount_btc
  )
  amount_label <- format_btc_amount_uk(display_amount_btc)
  satoshi_label <- format(
    display_satoshis,
    big.mark = " ",
    scientific = FALSE
  )

  chart_data <- chart_data |>
    dplyr::mutate(
      displayed_amount_value =
        price_quote_per_btc * display_amount_btc
    )
  one_btc_limits <- range(
    chart_data$price_quote_per_btc,
    na.rm = TRUE
  )
  price_padding <- diff(one_btc_limits) * 0.05
  if (!is.finite(price_padding) || price_padding <= 0) {
    price_padding <- one_btc_limits[[1]] * 0.05
  }
  one_btc_axis_range <- c(
    one_btc_limits[[1]] - price_padding,
    one_btc_limits[[2]] + price_padding
  )
  displayed_amount_axis_range <-
    one_btc_axis_range * display_amount_btc

  chart <- plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~price_quote_per_btc,
    type = "scattergl",
    mode = "lines",
    name = "1 BTC",
    visible = TRUE,
    connectgaps = FALSE,
    line = list(
      color = "#F2A900",
      width = 2
    ),
    hovertemplate = paste0(
      "<b>%{x|%Y-%m-%d %H:%M} UTC</b>",
      "<br>1 BTC: %{y:,.2f} ",
      quote_currency,
      "<extra></extra>"
    )
  ) |>
    plotly::add_trace(
      y = ~displayed_amount_value,
      type = "scattergl",
      mode = "lines",
      name = paste(amount_label, "BTC"),
      visible = TRUE,
      yaxis = "y2",
      connectgaps = FALSE,
      line = list(
        color = "#2E86DE",
        width = 1.6,
        dash = "solid"
      ),
      hovertemplate = paste0(
        "<b>%{x|%Y-%m-%d %H:%M} UTC</b>",
        "<br>",
        amount_label,
        " BTC: %{y:,.2f} ",
        quote_currency,
        "<extra></extra>"
      )
    ) |>
    plotly::layout(
      title = list(
        text = paste0(
          "Вартість 1 BTC і ",
          amount_label,
          " BTC",
          "<br><sup>",
          satoshi_label,
          " сатоші. Дві шкали, однакова форма руху на ринку ",
          market_label,
          "</sup>"
        ),
        x = 0,
        xanchor = "left"
      ),
      xaxis = list(
        title = list(text = "Час, UTC"),
        rangeslider = time_range_slider()
      ),
      yaxis = list(
        title = list(
          text = paste(quote_currency, "за 1 BTC"),
          standoff = 10
        ),
        range = one_btc_axis_range,
        separatethousands = TRUE
      ),
      yaxis2 = list(
        title = list(
          text = paste(
            quote_currency,
            "за",
            amount_label,
            "BTC"
          ),
          standoff = 10
        ),
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        range = displayed_amount_axis_range,
        separatethousands = TRUE
      ),
      hovermode = "x unified",
      dragmode = "zoom",
      updatemenus = list(
        trace_toggle_menu(
          label = "1 BTC",
          trace_index = 0L,
          position = 1L
        ),
        trace_toggle_menu(
          label = paste(amount_label, "BTC"),
          trace_index = 1L,
          position = 2L
        )
      )
    )

  render_book_widget(
    chart,
    hovermode = "x unified",
    connect_gaps = FALSE,
    margin = list(l = 115, r = 115, t = 155, b = 80),
    showlegend = FALSE,
    height = 660
  )
}

make_returns_widget <- function(
  data,
  market_label = "BTC/USDT"
) {
  chart_data <- complete_hourly_price_return_grid(data)

  chart <- plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~simple_return_percent,
    type = "scattergl",
    mode = "lines",
    name = "Звичайна дохідність",
    visible = TRUE,
    connectgaps = FALSE,
    line = list(
      color = "#2E86DE",
      width = 1
    ),
    hovertemplate = paste0(
      "<b>%{x|%Y-%m-%d %H:%M} UTC</b>",
      "<br>Звичайна: %{y:.4f} %",
      "<extra></extra>"
    )
  ) |>
    plotly::add_trace(
      y = ~log_return_percent,
      type = "scattergl",
      mode = "lines",
      name = "Логарифмічна дохідність",
      visible = TRUE,
      connectgaps = FALSE,
      line = list(
        color = "#C44E52",
        width = 1,
        dash = "dot"
      ),
      hovertemplate = paste0(
        "<b>%{x|%Y-%m-%d %H:%M} UTC</b>",
        "<br>Логарифмічна: %{y:.4f} %",
        "<extra></extra>"
      )
    ) |>
    plotly::layout(
      title = list(
        text = paste("Годинні дохідності", market_label),
        x = 0,
        xanchor = "left"
      ),
      xaxis = list(
        title = list(text = "Час, UTC"),
        rangeslider = time_range_slider()
      ),
      yaxis = list(
        title = list(
          text = "Дохідність, %",
          standoff = 10
        ),
        ticksuffix = " %",
        zeroline = TRUE,
        zerolinewidth = 1.2
      ),
      hovermode = "x unified",
      dragmode = "zoom",
      updatemenus = list(
        trace_toggle_menu(
          label = "Дохідність",
          trace_index = 0L,
          position = 1L
        ),
        trace_toggle_menu(
          label = "Лог-доходність",
          trace_index = 1L,
          position = 2L
        )
      )
    )

  render_book_widget(
    chart,
    hovermode = "x unified",
    connect_gaps = FALSE,
    margin = list(l = 115, r = 30, t = 155, b = 80),
    showlegend = FALSE,
    height = 660
  )
}
