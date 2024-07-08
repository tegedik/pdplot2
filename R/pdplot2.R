pdplot2 <- function(x = seq(...),
                    mean = NULL,
                    sd = NULL,
                    size = NULL,
                    prob = NULL,
                    rate = NULL,
                    lambda = NULL,
                    location = NULL,
                    scale = NULL,
                    shape = NULL,
                    shape1 = NULL,
                    shape2 = NULL,
                    df = NULL,
                    df1 = NULL,
                    df2 = NULL,
                    meanlog = NULL,
                    sdlog = NULL,
                    min = NULL,
                    max = NULL,
                    m = NULL,
                    n = NULL,
                    k = NULL,
                    dist = c("normal", "binomial", "exponential", "poisson", "chi-square",
                             "logistic", "cauchy", "beta", "gamma", "geometric", "Student's t",
                             "F", "weibull", "negative binomial", "log-normal", "uniform",
                             "hypergeometric"),
                    type = c("PDF", "CDF"),
                    show_color = FALSE,
                    linetype = c(
                      "solid", "dashed", "dotted", "dotdash", "longdash",
                      "twodash", "1F", "F1", "4C88C488", "12345678"),
                    color = c(
                      "#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                      "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")) {

  library(ggplot2)

  dist_funcs <- list(
    "normal" = list("PDF" = "dnorm", "CDF" = "pnorm", "params" = c("mean", "sd"), "title" = "Normal Distribution", "legend" = expression(paste(mu, ", ", sigma))),
    "binomial" = list("PDF" = "dbinom", "CDF" = "pbinom", "params" = c("size", "prob"), "title" = "Binomial Distribution", "legend" = expression(paste(n, ", ", p))),
    "exponential" = list("PDF" = "dexp", "CDF" = "pexp", "params" = "rate", "title" = "Exponential Distribution", "legend" = expression(paste(lambda))),
    "poisson" = list("PDF" = "dpois", "CDF" = "ppois", "params" = "lambda", "title" = "Poisson Distribution", "legend" = expression(paste(lambda))),
    "chi-square" = list("PDF" = "dchisq", "CDF" = "pchisq", "params" = "df", "title" = "Chi-Square Distribution", "legend" = expression(paste(italic(k)))),
    "logistic" = list("PDF" = "dlogis", "CDF" = "plogis", "params" = c("location", "scale"), "title" = "Logistic Distribution", "legend" = expression(paste(mu, ", ",italic(s)))),
    "cauchy" = list("PDF" = "dcauchy", "CDF" = "pcauchy", "params" = c("location", "scale"), "title" = "Cauchy Distribution", "legend" = expression(paste(italic(x)[0], ", ",gamma))),
    "beta" = list("PDF" = "dbeta", "CDF" = "pbeta", "params" = c("shape1", "shape2"), "title" = "Beta Distribution", "legend" = expression(paste(alpha, ", ",beta))),
    "gamma" = list("PDF" = "dgamma", "CDF" = "pgamma", "params" = c("scale", "shape"), "title" = "Gamma Distribution", "legend" = expression(paste(theta, ", ", k)), "custom_params" = TRUE),
    "geometric" = list("PDF" = "dgeom", "CDF" = "pgeom", "params" = "prob", "title" = "Geometric Distribution", "legend" = expression(paste(p))),
    "Student's t" = list("PDF" = "dt", "CDF" = "pt", "params" = "df", "title" = "Student's t-Distribution", "legend" = expression(paste(nu))),
    "F" = list("PDF" = "df", "CDF" = "pf", "params" = c("df1", "df2"), "title" = "F-Distribution", "legend" = expression(paste(d[1], ", ",d[2]))),
    "weibull" = list("PDF" = "dweibull", "CDF" = "pweibull", "params" = c("shape", "scale"), "title" = "Weibull Distribution", "legend" = expression(paste(k, ", ", lambda))),
    "negative binomial" = list("PDF" = "dnbinom", "CDF" = "pnbinom", "params" = c("size", "prob"), "title" = "Negative Binomial Distribution", "legend" = expression(paste(r, ", ", p))),
    "log-normal" = list("PDF" = "dlnorm", "CDF" = "plnorm", "params" = c("meanlog", "sdlog"), "title" = "Log-normal Distribution", "legend" = expression(paste(mu, ", ", sigma))),
    "uniform" = list("PDF" = "dunif", "CDF" = "punif", "params" = c("min", "max"), "title" = "Uniform Distribution", "legend" = expression(paste(a, ", ", b))),
    "hypergeometric" = list("PDF" = "dhyper", "CDF" = "phyper", "params" = c("m", "n", "k"), "title" = "Hypergeometric Distribution", "legend" = expression(paste(n, ", ", m, ", ", k)))
  )

  dist_info <- dist_funcs[[dist]]
  func <- dist_info[[type]]
  title <- paste(dist_info$title, " (",type,")", sep = "")
  legend_title <- dist_info$legend
  params <- dist_info$params
  param_values <- lapply(params, function(p) get(p))

  if (dist == "gamma" && dist_info$custom_params) {
    param_values <- list(
      shape = shape,
      rate = 1/scale
    )
    labels <- sapply(1:length(param_values[[1]]), function(i)
      paste(1/param_values$rate[i], ", ", param_values$shape[i]))
  } else {
    labels <- sapply(1:length(param_values[[1]]), function(i) paste(mapply("[", param_values, i), collapse = ", "))
  }

  gg <- do.call(rbind, lapply(1:length(param_values[[1]]), function(i) {
    if (dist == "gamma" && dist_info$custom_params) {
      param_list <- list(shape = param_values$shape[i], rate = param_values$rate[i])
    } else {
      param_list <- lapply(param_values, function(v) v[i])
    }
    cbind(i, x, y = do.call(func, c(list(x), param_list)))
  }))

  gg <- data.frame(gg)

  aes_mapping <- aes(x, y, linetype = factor(i))
  if (show_color) {
    aes_mapping <- aes(x, y, color = factor(i))
  }

  p1 <- ggplot(gg, aes_mapping) +
    geom_line(linewidth = 1) +
    scale_linetype_manual(values = linetype, labels = labels) +
    labs(
      title = title,
      y = if (type == "PDF") "Probability Density" else expression(paste("Probability ") (X <= x)),
      x = " ",
      linetype = legend_title
    ) +
    theme_minimal() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.99, if (type == "PDF") 0.99 else 0.01),
      legend.justification = c(1, if (type == "PDF") 1 else 0),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.title = element_text(size = 14, hjust = 0.5),
      legend.box.background = element_rect(colour = "black"),
      legend.text = element_text(size = 14)
    )

  if (show_color) {
    p1 <- p1 + scale_color_manual(values = color, labels = labels) +
      labs(color = legend_title)
  }

  return(p1)
}
