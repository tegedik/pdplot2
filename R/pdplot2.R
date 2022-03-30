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
  if (dist == "normal") {
    normal_dist <- function(x = seq(...), mean = NULL, sd = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dnorm"
        title1 <- "Normal Distribution (PDF)"
        label1 <- "Probability Density"
        pos <- c(.90, .90)
      } else if (type == "CDF") {
        func1 <- "pnorm"
        title1 <- "Normal Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(.90, .15)
      } else {
        print("what?")
      }

      params <- data.frame(mean, sd)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$mean, params[i, ]$sd))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$mean, ", ", params$sd)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(mu, ", ", sigma))
          )
      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$mean, ", ", params$sd)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(mu, ", ", sigma))
          )
      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    normal_dist(x = x, mean = mean, sd = sd, type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "binomial") {

    binomial_dist <- function(x = seq(...), size = NULL, prob = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dbinom"
        title1 <- "Binomial Distribution (PMF)"
        label1 <- expression(paste("Probability ") (X == x))
        pos <- c(.90, .90)
      } else if (type == "CDF") {
        func1 <- "pbinom"
        title1 <- "Binomial Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(.90, .15)
      } else {
        print("what?")
      }

      params <- data.frame(size, prob)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$size, params[i, ]$prob))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$size, ", ", params$prob)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(n, ", ", p))
          )
      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$size, ", ", params$prob)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(n, ", ", p))
          )
      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    binomial_dist(x = x, size = size, prob = prob, type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "exponential") {

    exponential_dist <- function(x = seq(...), rate = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dexp"
        title1 <- "Exponential Distribution (PDF)"
        label1 <- "Probability Density"
        pos <- c(.90, .90)
      } else if (type == "CDF") {
        func1 <- "pexp"
        title1 <- "Exponential Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(.90, .15)
      } else {
        print("what?")
      }

      params <- data.frame(rate, rate)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$rate))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$rate)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(lambda))
          )
      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$rate)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(lambda))
          )
      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    exponential_dist(x = x, rate = rate,  type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "poisson") {

    poisson_dist <- function(x = seq(...), lambda = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dpois"
        title1 <- "Poisson Distribution (PDF)"
        label1 <- expression(paste("Probability ") (X == x))
        pos <- c(.90, .90)
      } else if (type == "CDF") {
        func1 <- "ppois"
        title1 <- "Poisson Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(.90, .15)
      } else {
        print("what?")
      }

      params <- data.frame(lambda, lambda)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$lambda))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$lambda)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(lambda))
          )
      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$lambda)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(lambda))
          )
      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    poisson_dist(x = x, lambda = lambda,  type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "chi-square") {
    chi_square_dist <- function(x = seq(...), df = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dchisq"
        title1 <- "Chi-Square Distribution (PDF)"
        label1 <- "Probability Density"
        pos = c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "pchisq"
        title1 <- "Chi-Square Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos = c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(df, ncp=0)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$df, params[i, ]$ncp))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$df)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(chi^2))
          )
      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$df)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(chi^2))
          )
      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    chi_square_dist(x = x, df = df, type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "logistic") {
    logistic_dist <- function(x = seq(...), location = NULL, scale = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dlogis"
        title1 <- "Logistic Distribution (PDF)"
        label1 <- "Probability Density"
        pos <- c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "plogis"
        title1 <- "Logistic Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(location, scale)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$location, params[i, ]$scale))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$location, ", ", params$scale)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(mu, ", ",italic(s))))

      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$location, ", ", params$scale)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(mu, ", ",italic(s))))

      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    logistic_dist(x = x, location = location, scale = scale, type = type, linetype=linetype, show_color = show_color, color = color)

  }
  else if (dist == "cauchy") {
    cauchy_dist <- function(x = seq(...), location = NULL, scale = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dcauchy"
        title1 <- "Cauchy Distribution (PDF)"
        label1 <- "Probability Density"
        pos <- c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "pcauchy"
        title1 <- "Cauchy Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(location, scale)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$location, params[i, ]$scale))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$location, ", ", params$scale)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype =expression(paste(italic(x)[0], ", ",gamma))
          )

      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$location, ", ", params$scale)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(italic(x)[0], ", ",gamma))
          )

      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    cauchy_dist(x = x, location = location, scale = scale, type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "beta") {
    beta_dist <- function(x = seq(...), shape1 = NULL, shape2 = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dbeta"
        title1 <- "Beta Distribution (PDF)"
        label1 <- "Probability Density"
        pos <- c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "pbeta"
        title1 <- "Beta Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(shape1, shape2)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$shape1, params[i, ]$shape2))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$shape1, ", ", params$shape2)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(alpha, ", ",beta))
          )

      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$shape1, ", ", params$shape2)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(alpha, ", ",beta))
          )

      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    beta_dist(x = x, shape1 = shape1, shape2 = shape2, type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "gamma") {

    gamma_dist <- function(x = seq(...), shape = NULL, scale = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dgamma"
        title1 <- "Gamma Distribution (PDF)"
        label1 <- "Probability Density"
        pos <- c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "pgamma"
        title1 <- "Gamma Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(shape, scale)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$shape, (1/params[i, ]$scale)))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$shape, ", ", params$scale)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(k, ", ",theta))
          )

      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$shape, ", ", params$scale)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(k, ", ",theta))
          )

      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    gamma_dist(x = x, shape = shape, scale = scale, type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "geometric") {

    geometric_dist <- function(x = seq(...), prob = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dgeom"
        title1 <- "Geometric Distribution (PDF)"
        label1 <- expression(paste("Probability ") (X == x))
        pos <- c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "pgeom"
        title1 <- "Geometric Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(prob, prob)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$prob))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$prob)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(p))
          )
      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$prob)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(p))
          )
      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    geometric_dist(x = x, prob = prob,  type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "Student's t") {
    students_t_dist <- function(x = seq(...), df = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dt"
        title1 <- "Student's t-Distribution (PDF)"
        label1 <- "Probability Density"
        pos <- c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "pt"
        title1 <- "Student's t-Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(df, df)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$df))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$df)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(nu))
          )
      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$df)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(nu))
          )
      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    students_t_dist(x = x, df = df,  type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "F") {
    f_dist <- function(x = seq(...), df1 = NULL, df2 = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "df"
        title1 <- "F-Distribution (PDF)"
        label1 <- "Probability Density"
        pos <- c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "pf"
        title1 <- "F-Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(df1, df2)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$df1, params[i, ]$df2))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$df1, ", ", params$df2)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(d[1], ", ",d[2]))
          )
      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$df1, ", ", params$df2)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(d[1], ", ", d[2]))
          )
      }

      p1 + theme_minimal() +
        theme(
          legend.position =pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    f_dist(x = x, df1 = df1, df2 = df2, type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "weibull") {

    weibull_dist <- function(x = seq(...), shape = NULL, scale = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dweibull"
        title1 <- "Weibull Distribution (PDF)"
        label1 <- "Probability Density"
        pos <- c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "pweibull"
        title1 <- "Weibull Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(shape, scale)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$shape, params[i, ]$scale))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$scale, ", ", params$shape)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype =expression(paste(lambda, ", ", k))
          )

      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$scale, ", ", params$shape)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(lambda, ", ",k))
          )

      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    weibull_dist(x = x, shape = shape, scale = scale, type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "negative binomial") {
    negbinom_dist <- function(x = seq(...), size = NULL, prob = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dnbinom"
        title1 <- "Negative Binomial Distribution (PDF)"
        label1 <- expression(paste("Probability ") (X == x))
        pos <- c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "pnbinom"
        title1 <- "Negative Binomial Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(size, prob)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$size, params[i, ]$prob))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$size, ", ", params$prob)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(r, ", ", p))
          )
      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$size, ", ", params$prob)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(r, ", ", p))
          )
      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    negbinom_dist(x = x, size = size, prob = prob, type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "log-normal") {
    lognormal_dist <- function(x = seq(...), meanlog = NULL, sdlog = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dlnorm"
        title1 <- "Log-normal Distribution (PDF)"
        label1 <- "Probability Density"
        pos <- c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "plnorm"
        title1 <- "Log-normal Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(meanlog, sdlog)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$meanlog, params[i, ]$sdlog))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$meanlog, ", ", params$sdlog)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(mu, ", ", sigma))
          )
      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$meanlog, ", ", params$sdlog)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(mu, ", ", sigma))
          )
      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    lognormal_dist(x = x, meanlog = meanlog, sdlog = sdlog, type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else if (dist == "uniform") {
    uniform_dist <- function(x = seq(...), min = NULL, max = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dunif"
        title1 <- "Uniform Distribution (PDF)"
        label1 <- "Probability Density"
        pos <- c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "punif"
        title1 <- "Uniform Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(min, max)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$min, params[i, ]$max))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$min, ", ", params$max)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(a, ", ", b)))

      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$min, ", ", params$max)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(a, ", ", b)))

      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    uniform_dist(x = x, min = min, max = max, type = type, linetype = linetype, show_color = show_color, color = color)
  }
  else if (dist == "hypergeometric") {
    hypergeometric_dist <- function(x = seq(...), m = NULL, n = NULL, k = NULL, type = NULL, linetype=NULL, show_color = FALSE, color = NULL) {

      if (type == "PDF") {
        func1 <- "dhyper"
        title1 <- "Hypergeometric Distribution (PDF)"
        label1 <- expression(paste("Probability ") (X == x))
        pos <- c(0.90, 0.90)
      } else if (type == "CDF") {
        func1 <- "phyper"
        title1 <- "Hypergeometric Distribution (CDF)"
        label1 <- expression(paste("Probability ") (X <= x))
        pos <- c(0.90, 0.15)
      } else {
        print("what?")
      }

      params <- data.frame(m, n, k)
      gg <- do.call(rbind, lapply(1:nrow(params), function(i) {
        cbind(i, x,
              y = do.call(func1, list(x, params[i, ]$m,  params[i, ]$n,  params[i, ]$k))
        )
      }))
      gg <- data.frame(gg)

      if (show_color == FALSE) {
        p1 <- ggplot(gg, aes(x, y, linetype = factor(i))) +
          geom_line(size = 1) +
          scale_linetype_manual( # "mean, sd",
            values = linetype,
            labels = paste0(params$n, ", ", params$m, ", ", params$k)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            linetype = expression(paste(n, ", ", m, ", ", k))
          )
      } else {
        p1 <- ggplot(gg, aes(x, y, color = factor(i))) +
          geom_line(size = 1) +
          scale_color_manual( # "mean, sd",
            values = color,
            labels = paste0(params$n, ", ", params$m, ", ", params$k)
          ) +
          labs(
            title = title1,
            y = label1,
            x = " ",
            color = expression(paste(n, ", ", m, ", ", k))
          )
      }

      p1 + theme_minimal() +
        theme(
          legend.position = pos,
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 14)
        )
    }

    hypergeometric_dist(x = x, m = m, n = n, k = k,  type = type, linetype=linetype, show_color = show_color, color = color)
  }
  else {
    print("Not available.")
  }
}
