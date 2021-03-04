pdplot2 <- function(x=seq(...), mean=NULL, sd=NULL, size=NULL, prob=NULL, rate=NULL, lambda=NULL, location=NULL, scale=NULL, df=NULL, df1=NULL, df2=NULL, shape=NULL, shape1=NULL, shape2=NULL,  show.color=FALSE, line.selection=FALSE, linetype=NULL, color.selection=FALSE, color=NULL, type=c("normal", "binomial", "exponential", "poisson", "logistic", "cauchy", "chi-square", "beta", "gamma", "geometric", "Student's t", "F")){
if (type == "normal"){
  if (show.color==FALSE) {
    if (line.selection==FALSE) {
    linetype <- c( "solid", "dashed", "dotted", "dotdash", "longdash",
                   "twodash", "1F", "F1","4C88C488", "12345678")
    params = data.frame(mean,sd)
    gg <- do.call(rbind,lapply(1:nrow(params),function(i)
      cbind(i,x,y=dnorm(x,params[i,]$mean,params[i,]$sd))))
    gg <- data.frame(gg)
    ggplot(gg,aes(x,y,linetype=factor(i))) +
      geom_line(size=1)+
      scale_linetype_manual(#"mean, sd",
        values=linetype,
        labels=paste0(params$mean,", ",params$sd)) +
      labs(title="Normal Distribution",
           y="Probability Density",
           x=" ",
           linetype = expression(paste(mu, ", ", sigma))) +
      #theme(legend.position="bottom")+
      theme_minimal() +
      theme(legend.position = c(.90,.90),
            legend.title = element_text(size = 14),
            legend.box.background = element_rect(colour = "black"),
            legend.title.align=0.5,
            legend.text = element_text(size = 14))
  } else {
    params = data.frame(mean,sd)
    gg <- do.call(rbind,lapply(1:nrow(params),function(i)
      cbind(i,x,y=dnorm(x,params[i,]$mean,params[i,]$sd))))
    gg <- data.frame(gg)
    ggplot(gg,aes(x,y,linetype=factor(i))) +
      geom_line(size=1)+
      scale_linetype_manual(#"mean, sd",
        values=linetype,
        labels=paste0(params$mean,", ",params$sd)) +
      labs(title="Normal Distribution",
           y="Probability Density",
           x=" ",
           linetype = expression(paste(mu, ", ", sigma))) +
      #theme(legend.position="bottom")+
      theme_minimal() +
      theme(legend.position = c(.90,.90),
            legend.title = element_text(size = 14),
            legend.box.background = element_rect(colour = "black"),
            legend.title.align=0.5,
            legend.text = element_text(size = 14))
  }
    } else {
  if (color.selection==FALSE) {
color <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
           "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
params = data.frame(mean,sd)
gg <- do.call(rbind,lapply(1:nrow(params),function(i)
  cbind(i,x,y=dnorm(x,params[i,]$mean,params[i,]$sd))))
gg <- data.frame(gg)
ggplot(gg,aes(x,y,color=factor(i))) +
  geom_line(size=1)+
  scale_color_manual(#"mean, sd",
                     values=color,
                     labels=paste0(params$mean,", ",params$sd)) +
  labs(title="Normal Distribution",
       y="Probability Density",
       x=" ",
       color = expression(paste(mu, ", ", sigma))) +
  #theme(legend.position="bottom")+
  theme_minimal() +
  theme(legend.position = c(.90,.90),
        legend.title = element_text(size = 14),
        legend.box.background = element_rect(colour = "black"),
        legend.title.align=0.5,
        legend.text = element_text(size = 14))}

 else {
  params = data.frame(mean,sd)
  gg <- do.call(rbind,lapply(1:nrow(params),function(i)
    cbind(i,x,y=dnorm(x,params[i,]$mean,params[i,]$sd))))
  gg <- data.frame(gg)
  ggplot(gg,aes(x,y,color=factor(i))) +
    geom_line(size=1)+
    scale_color_manual(#"mean, sd",
      values=color,
      labels=paste0(params$mean,", ",params$sd)) +
    labs(title="Normal Distribution",
         y="Probability Density",
         x=" ",
         color = expression(paste(mu, ", ", sigma))) +
    #theme(legend.position="bottom")+
    theme_minimal() +
    theme(legend.position = c(.90,.90),
          legend.title = element_text(size = 14),
          legend.box.background = element_rect(colour = "black"),
          legend.title.align=0.5,
          legend.text = element_text(size = 14))}
    }
}
else if (type == "binomial"){
  if (show.color==FALSE) {
    if (line.selection==FALSE) {
    linetype <- c( "solid", "dashed", "dotted", "dotdash", "longdash",
                   "twodash", "1F", "F1","4C88C488", "12345678")
    params = data.frame(size,prob)
    gg <- do.call(rbind,lapply(1:nrow(params),function(i)
      cbind(i,x,y=dbinom(x,params[i,]$size,params[i,]$prob))))
    gg <- data.frame(gg)
    ggplot(gg,aes(x,y,linetype=factor(i))) +
      geom_line(size=1)+
      scale_linetype_manual(#"mean, sd",
        values=linetype,
        labels=paste0(params$size,", ",params$prob)) +
      labs(title="Binomial Distribution",
           y="Probability Mass",
           x=" ",
           linetype = expression(paste(n, ", ", p))) +
      #theme(legend.position="bottom")+
      theme_minimal() +
      theme(legend.position = c(.90,.90),
            legend.title = element_text(size = 14),
            legend.box.background = element_rect(colour = "black"),
            legend.title.align=0.5,
            legend.text = element_text(size = 14))
    } else {
      params = data.frame(size,prob)
      gg <- do.call(rbind,lapply(1:nrow(params),function(i)
        cbind(i,x,y=dbinom(x,params[i,]$size,params[i,]$prob))))
      gg <- data.frame(gg)
      ggplot(gg,aes(x,y,linetype=factor(i))) +
        geom_line(size=1)+
        scale_linetype_manual(#"mean, sd",
          values=linetype,
          labels=paste0(params$size,", ",params$prob)) +
        labs(title="Binomial Distribution",
             y="Probability Mass",
             x=" ",
             linetype = expression(paste(n, ", ", p))) +
        #theme(legend.position="bottom")+
        theme_minimal() +
        theme(legend.position = c(.90,.90),
              legend.title = element_text(size = 14),
              legend.box.background = element_rect(colour = "black"),
              legend.title.align=0.5,
              legend.text = element_text(size = 14))
    }
  } else {
    if (color.selection==FALSE) {
      color <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                 "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
      params = data.frame(size,prob)
      gg <- do.call(rbind,lapply(1:nrow(params),function(i)
        cbind(i,x,y=dbinom(x,params[i,]$size,params[i,]$prob))))
      gg <- data.frame(gg)
      ggplot(gg,aes(x,y,color=factor(i))) +
        geom_line(size=1)+
        scale_color_manual(#"mean, sd",
          values=color,
          labels=paste0(params$size,", ",params$prob)) +
        labs(title="Binomial Distribution",
             y="Probability Mass",
             x=" ",
             color = expression(paste(n, ", ", p))) +
        #theme(legend.position="bottom")+
        theme_minimal() +
        theme(legend.position = c(.90,.90),
              legend.title = element_text(size = 14),
              legend.box.background = element_rect(colour = "black"),
              legend.title.align=0.5,
              legend.text = element_text(size = 14))}

    else {
      params = data.frame(size,prob)
      gg <- do.call(rbind,lapply(1:nrow(params),function(i)
        cbind(i,x,y=dbinom(x,params[i,]$size,params[i,]$prob))))
      gg <- data.frame(gg)
      ggplot(gg,aes(x,y,color=factor(i))) +
        geom_line(size=1)+
        scale_color_manual(#"mean, sd",
          values=color,
          labels=paste0(params$size,", ",params$prob)) +
        labs(title="Binomial Distribution",
             y="Probability Mass",
             x=" ",
             color = expression(paste(n, ", ", p))) +
        #theme(legend.position="bottom")+
        theme_minimal() +
        theme(legend.position = c(.90,.90),
              legend.title = element_text(size = 14),
              legend.box.background = element_rect(colour = "black"),
              legend.title.align=0.5,
              legend.text = element_text(size = 14))}
  }
}
else if (type == "exponential"){
    if (show.color==FALSE) {
      if (line.selection==FALSE) {
        linetype <- c( "solid", "dashed", "dotted", "dotdash", "longdash",
                       "twodash", "1F", "F1","4C88C488", "12345678")
        params = data.frame(rate,rate)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dexp(x,params[i,]$rate))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$rate)) +
          labs(title="Exponential Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(lambda))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      } else {
        params = data.frame(rate,rate)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dexp(x,params[i,]$rate))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$rate)) +
          labs(title="Exponential Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(lambda))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      }
    } else {
      if (color.selection==FALSE) {
        color <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                   "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
        params = data.frame(rate,rate)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dexp(x,params[i,]$rate))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$rate)) +
          labs(title="Exponential Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(lambda))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}

      else {
        params = data.frame(rate,rate)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dexp(x,params[i,]$rate))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$rate)) +
          labs(title="Exponential Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(lambda))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}
    }
  }
else if (type == "poisson"){
    if (show.color==FALSE) {
      if (line.selection==FALSE) {
        linetype <- c( "solid", "dashed", "dotted", "dotdash", "longdash",
                       "twodash", "1F", "F1","4C88C488", "12345678")
        params = data.frame(lambda,lambda)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dpois(x,params[i,]$lambda))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$lambda)) +
          labs(title="Poisson Distribution",
               y="Probability Mass",
               x=" ",
               linetype = expression(paste(lambda))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      } else {
        params = data.frame(lambda,lambda)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dpois(x,params[i,]$lambda))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$lambda)) +
          labs(title="Poisson Distribution",
               y="Probability Mass",
               x=" ",
               linetype = expression(paste(lambda))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      }
    } else {
      if (color.selection==FALSE) {
        color <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                   "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
        params = data.frame(lambda,lambda)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dpois(x,params[i,]$lambda))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$lambda)) +
          labs(title="Poisson Distribution",
               y="Probability Mass",
               x=" ",
               color = expression(paste(lambda))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}

      else {
        params = data.frame(lambda,lambda)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dpois(x,params[i,]$lambda))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$lambda)) +
          labs(title="Poisson Distribution",
               y="Probability Mass",
               x=" ",
               color = expression(paste(lambda))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}
    }
  }
else if (type == "logistic"){
    if (show.color==FALSE) {
      if (line.selection==FALSE) {
        linetype <- c( "solid", "dashed", "dotted", "dotdash", "longdash",
                       "twodash", "1F", "F1","4C88C488", "12345678")
        params = data.frame(location, scale)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dlogis(x,params[i,]$location,params[i,]$scale))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$location,", ",params$scale)) +
          labs(title="Logistic Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(mu, ", ",italic(s)))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      } else {
        params = data.frame(location, scale)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dlogis(x,params[i,]$location,params[i,]$scale))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$location,", ",params$scale)) +
          labs(title="Logistic Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(mu, ", ", italic(s)))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      }
    } else {
      if (color.selection==FALSE) {
        color <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                   "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
        params = data.frame(location, scale)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dlogis(x,params[i,]$location,params[i,]$scale))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$location,", ",params$scale)) +
          labs(title="Logistic Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(mu, ", ", italic(s)))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}

      else {
        params = data.frame(location, scale)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dlogis(x,params[i,]$location,params[i,]$scale))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$location,", ",params$scale)) +
          labs(title="Logistic Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(mu, ", ", italic(s)))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}
    }
  }
else if (type == "cauchy"){
    if (show.color==FALSE) {
      if (line.selection==FALSE) {
        linetype <- c( "solid", "dashed", "dotted", "dotdash", "longdash",
                       "twodash", "1F", "F1","4C88C488", "12345678")
        params = data.frame(location, scale)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dcauchy(x,params[i,]$location,params[i,]$scale))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$location,", ",params$scale)) +
          labs(title="Cauchy Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(italic(x)[0], ", ",gamma))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      } else {
        params = data.frame(location, scale)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dcauchy(x,params[i,]$location,params[i,]$scale))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$location,", ",params$scale)) +
          labs(title="Cauchy Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(italic(x)[0], ", ", gamma))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      }
    } else {
      if (color.selection==FALSE) {
        color <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                   "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
        params = data.frame(location, scale)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dcauchy(x,params[i,]$location,params[i,]$scale))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$location,", ",params$scale)) +
          labs(title="Cauchy Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(italic(x)[0], ", ", gamma))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}

      else {
        params = data.frame(location, scale)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dcauchy(x,params[i,]$location,params[i,]$scale))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$location,", ",params$scale)) +
          labs(title="Cauchy Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(italic(x)[0], ", ", gamma))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}
    }
}
else if (type == "chi-square"){
    if (show.color==FALSE) {
      if (line.selection==FALSE) {
        linetype <- c( "solid", "dashed", "dotted", "dotdash", "longdash",
                       "twodash", "1F", "F1","4C88C488", "12345678")
        params = data.frame(df, ncp=0)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dchisq(x,params[i,]$df,params[i,]$ncp))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$df)) +
          labs(title="Chi-square Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(chi^2))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      } else {
        params = data.frame(df, ncp=0)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dchisq(x,params[i,]$df,params[i,]$ncp))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$df)) +
          labs(title="Chi-square Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(chi^2))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      }
    } else {
      if (color.selection==FALSE) {
        color <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                   "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
        params = data.frame(df, ncp=0)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dchisq(x,params[i,]$df,params[i,]$ncp))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$df)) +
          labs(title="Chi-square Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(chi^2))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}

      else {
        params = data.frame(df, ncp=0)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dchisq(x,params[i,]$df,params[i,]$ncp))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$df)) +
          labs(title="Chi-square Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(chi^2))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}
    }
  }
else if (type == "beta"){
    if (show.color==FALSE) {
      if (line.selection==FALSE) {
        linetype <- c( "solid", "dashed", "dotted", "dotdash", "longdash",
                       "twodash", "1F", "F1","4C88C488", "12345678")
        params = data.frame(shape1, shape2)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dbeta(x,params[i,]$shape1,params[i,]$shape2))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$shape1,", ",params$shape2)) +
          labs(title="Beta Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(alpha, ", ",beta))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      } else {
        params = data.frame(shape1, shape2)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dbeta(x,params[i,]$shape1,params[i,]$shape2))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$shape1,", ",params$shape2)) +
          labs(title="Beta Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(alpha, ", ", beta))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      }
    } else {
      if (color.selection==FALSE) {
        color <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                   "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
        params = data.frame(shape1, shape2)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dbeta(x,params[i,]$shape1,params[i,]$shape2))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$shape1,", ",params$shape2)) +
          labs(title="Beta Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(alpha, ", ", beta))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}

      else {
        params = data.frame(shape1, shape2)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dbeta(x,params[i,]$shape1,params[i,]$shape2))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$shape1,", ",params$shape2)) +
          labs(title="Beta Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(alpha, ", ", beta))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}
    }
}
else if (type == "gamma"){
    if (show.color==FALSE) {
      if (line.selection==FALSE) {
        linetype <- c( "solid", "dashed", "dotted", "dotdash", "longdash",
                       "twodash", "1F", "F1","4C88C488", "12345678")
        params = data.frame(shape, scale)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dgamma(x,params[i,]$shape,params[i,]$scale))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$shape,", ",params$scale)) +
          labs(title="Gamma Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(k, ", ",theta))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      } else {
        params = data.frame(shape, scale)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dgamma(x,params[i,]$shape,params[i,]$scale))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$shape,", ",params$scale)) +
          labs(title="Gamma Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(k, ", ", theta))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      }
    } else {
      if (color.selection==FALSE) {
        color <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                   "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
        params = data.frame(shape, scale)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dgamma(x,params[i,]$shape,params[i,]$scale))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$shape,", ",params$scale)) +
          labs(title="Gamma Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(k, ", ", theta))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}

      else {
        params = data.frame(shape, scale)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dgamma(x,params[i,]$shape,params[i,]$scale))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$shape,", ",params$scale)) +
          labs(title="Gamma Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(k, ", ", gamma))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}
    }
}
else if (type == "geometric"){
    if (show.color==FALSE) {
      if (line.selection==FALSE) {
        linetype <- c( "solid", "dashed", "dotted", "dotdash", "longdash",
                       "twodash", "1F", "F1","4C88C488", "12345678")
        params = data.frame(prob, prob)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dgeom(x,params[i,]$prob))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$prob)) +
          labs(title="Geometric Distribution",
               y="Probability Mass",
               x=" ",
               linetype = expression(paste(p))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      } else {
        params = data.frame(prob, prob)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dgeom(x,params[i,]$prob))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$prob)) +
          labs(title="Geometric Distribution",
               y="Probability Mass",
               x=" ",
               linetype = expression(paste(p))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      }
    } else {
      if (color.selection==FALSE) {
        color <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                   "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
        params = data.frame(prob, prob)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dgeom(x,params[i,]$prob))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$prob)) +
          labs(title="Geometric Distribution",
               y="Probability Mass",
               x=" ",
               color = expression(paste(p))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}

      else {
        params = data.frame(prob, prob)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dgeom(x,params[i,]$prob))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$prob)) +
          labs(title="Geometric Distribution",
               y="Probability Mass",
               x=" ",
               color = expression(paste(p))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}
    }
}
else if (type == "Student's t"){
    if (show.color==FALSE) {
      if (line.selection==FALSE) {
        linetype <- c( "solid", "dashed", "dotted", "dotdash", "longdash",
                       "twodash", "1F", "F1","4C88C488", "12345678")
        params = data.frame(df, df)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dt(x,params[i,]$df))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$df)) +
          labs(title="Student's t Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(nu))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      } else {
        params = data.frame(df, df)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dt(x,params[i,]$df))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$df)) +
          labs(title="Student's t Distribution",
               y="Probability Density",
               x=" ",
               linetype = expression(paste(nu))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      }
    } else {
      if (color.selection==FALSE) {
        color <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                   "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
        params = data.frame(df, df)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dt(x,params[i,]$df))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$df)) +
          labs(title="Student's t Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(nu))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}

      else {
        params = data.frame(df, df)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=dt(x,params[i,]$df))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$df)) +
          labs(title="Student's t Distribution",
               y="Probability Density",
               x=" ",
               color = expression(paste(nu))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}
    }
  }
else if (type == "F"){
    if (show.color==FALSE) {
      if (line.selection==FALSE) {
        linetype <- c( "solid", "dashed", "dotted", "dotdash", "longdash",
                       "twodash", "1F", "F1","4C88C488", "12345678")
        params = data.frame(df1, df2)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=df(x,params[i,]$df1,params[i,]$df2))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$df1,", ",params$df2)) +
          labs(title=expression(paste(italic("F"), "-Distribution")),
               y="Probability Density",
               x=" ",
               linetype = expression(paste(d[1], ", ",d[2]))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      } else {
        params = data.frame(df1, df2)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=df(x,params[i,]$df1,params[i,]$df2))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,linetype=factor(i))) +
          geom_line(size=1)+
          scale_linetype_manual(#"mean, sd",
            values=linetype,
            labels=paste0(params$df1,", ",params$df2)) +
          labs(title=expression(paste(italic("F"), "-Distribution")),
               y="Probability Density",
               x=" ",
               linetype = expression(paste(d[1], ", ",d[2]))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))
      }
    } else {
      if (color.selection==FALSE) {
        color <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                   "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
        params = data.frame(df1, df2)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=df(x,params[i,]$df1,params[i,]$df2))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$df1,", ",params$df2)) +
          labs(title=expression(paste(italic("F"), "-Distribution")),
               y="Probability Density",
               x=" ",
               color = expression(paste(d[1], ", ",d[2]))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}

      else {
        params = data.frame(df1, df2)
        gg <- do.call(rbind,lapply(1:nrow(params),function(i)
          cbind(i,x,y=df(x,params[i,]$df1,params[i,]$df2))))
        gg <- data.frame(gg)
        ggplot(gg,aes(x,y,color=factor(i))) +
          geom_line(size=1)+
          scale_color_manual(#"mean, sd",
            values=color,
            labels=paste0(params$df1,", ",params$df2)) +
          labs(title=expression(paste(italic("F"), "-Distribution")),
               y="Probability Density",
               x=" ",
               color = expression(paste(d[1], ", ",d[2]))) +
          #theme(legend.position="bottom")+
          theme_minimal() +
          theme(legend.position = c(.90,.90),
                legend.title = element_text(size = 14),
                legend.box.background = element_rect(colour = "black"),
                legend.title.align=0.5,
                legend.text = element_text(size = 14))}
    }
  }

} #end
