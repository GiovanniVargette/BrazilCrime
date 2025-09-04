#' br_crime_predict Function
#'
#' Performs automatic ARIMA modeling and forecasting on crime time series data.
#'
#' @param dados Data.frame contendo as colunas da serie: evento, uf, municipio, data, e uma coluna com os valores numericos.
#' @param ts_col Nome da coluna (string) com os dados da serie temporal (ex: "total_vitima").
#' @param freq Frequencia temporal: "monthly" ou "yearly". Padrao e "monthly".
#' @param h Horizonte da previsao. Padrao: 12.
#' @param log Logico. Se TRUE, aplica transformacao logaritmica. Padrao: FALSE.
#' @param level Nivel de confianca do intervalo de previsao. Padrão: 95.
#'
#' @return Lista com modelo ajustado, previsao e grafico ggplot2.
#' @importFrom stats ts setNames
#' @export

br_crime_predict <- function(dados, ts_col, freq = "monthly", h = 12, log = FALSE, level = 95) {
  if (missing(dados) || missing(ts_col)) {
    stop("Voce deve fornecer o data.frame 'dados' e o nome da coluna da serie temporal ('ts_col')")
  }

  # Verificar colunas obrigatórias
  if (!all(c("data", "evento", "municipio", "uf") %in% names(dados))) {
    stop("O data.frame 'dados' deve conter as colunas: data, evento, municipio e uf")
  }

  # Converter data
  if (is.character(dados$data)) {
    dados$data <- lubridate::ym(dados$data)
  } else if (inherits(dados$data, "Date")) {
    dados$data <- lubridate::ym(format(dados$data, "%Y-%m"))
  } else {
    stop("A coluna 'data' deve estar no formato 'YYYY-MM' ou ser do tipo Date")
  }

  # Série numérica
  ts_data <- dados[[ts_col]]

  # Conversão para objeto ts
  message("Convertendo dados para objeto 'ts'")
  start_year <- lubridate::year(min(dados$data))
  start_period <- if (freq == "monthly") lubridate::month(min(dados$data)) else 1
  frequency <- if (freq == "monthly") 12 else 1

  ts_obj <- ts(ts_data, start = c(start_year, start_period), frequency = frequency)

  # Ajuste do modelo ARIMA com BIC fixado
  lambda <- if (log) 0 else NULL
  model <- forecast::auto.arima(
    ts_obj,
    lambda = lambda,
    stepwise = TRUE,
    trace = FALSE,
    approximation = FALSE,
    allowdrift = TRUE,
    allowmean = TRUE,
    ic = "bic"
  )

  # Previsão
  fcast <- forecast::forecast(model, h = h, level = level)

  # Preparar datas futuras
  future_dates <- seq.Date(from = max(dados$data), by = "month", length.out = h)
  full_dates <- c(dados$data, future_dates)

  # Preparar data.frame do gráfico
  df_plot <- data.frame(
    date = full_dates,
    value = c(ts_data, rep(NA, h)),
    fitted = c(as.numeric(model$fitted), rep(NA, h)),
    forecast = c(rep(NA, length(ts_data)), as.numeric(fcast$mean)),
    lower = c(rep(NA, length(ts_data)), fcast$lower[, 1]),
    upper = c(rep(NA, length(ts_data)), fcast$upper[, 1])
  )


  # Metadados para o título
  tipo_crime <- unique(dados$evento)
  localidade <- paste0(unique(dados$municipio), " - ", unique(dados$uf))
  periodo <- paste0(format(min(dados$data), "%b/%Y"), " a ", format(max(dados$data), "%b/%Y"))
  conf_label <- paste0("Intervalo de Confianca (", level, "%)")

  # Gráfico
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = value, color = "Observado"), linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = forecast, color = "Previsao"), linetype = "dashed", linewidth = 1.2, na.rm = TRUE) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = conf_label), alpha = 0.25, na.rm = TRUE) +
    ggplot2::labs(
      title = paste0("Previsao para ", tipo_crime),
      subtitle = paste0(localidade, " | ", periodo, " | H=", h, " | ", ifelse(log, "log-transformado", "sem log")),
      x = "Data",
      y = "Numero de Ocorrencias",
      color = "Legenda",
      fill = ""
    ) +
    ggplot2::scale_color_manual(values = c("Observado" = "#1B9E77", "Previsao" = "#D95F02")) +
    ggplot2::scale_fill_manual(values = setNames("#7570B3", conf_label)) +
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  print(p)

  return(list(model = model, forecast = fcast, plot = p, data = df_plot))

}

# Evitar notas no R CMD check sobre variáveis do ggplot2
if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "value", "forecast", "lower", "upper"
))
