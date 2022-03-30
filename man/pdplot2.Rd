\name{pdplot2}
\alias{pdplot2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting Probability Density/Mass and Cumulative Distribution Functions
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
`pdplot2` visualizes probability density (mass) and cumulative distribution functions in R. It requires parameters correctly specified for each distribution.
}
\usage{
pdplot2(x, mean, sd, size, prob, rate, lambda, location, scale, shape,
  shape1, shape2, df, df1, df2, meanlog, sdlog, min, max, m, n, k,
  dist = c("normal", "binomial", "exponential", "poisson", "chi-square",
  "logistic", "cauchy", "beta", "gamma", "geometric", "Student's t","F",
  "weibull", "negative binomial", "log-normal", "uniform", "hypergeometric"),
  type = c("PDF", "CDF"),
  show_color = FALSE,
  linetype = ...,
  color = ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector of quantiles
%%     ~~Describe \code{x} here~~
}
\item{mean}{Vector of means.}
\item{sd}{Vector of standard deviations.}
\item{location}{Location parameter.}
\item{scale}{Scale parameter.}
\item{size}{Number of trials (zero or more).}
\item{prob}{Probability of success on each trial.}
\item{df}{Degrees of freedom (non-negative, but can be non-integer).}
\item{df1, df2}{Degrees of freedom. \code{Inf} allowed.}
\item{lambda}{Vector of (non-negative) means.}
\item{rate}{Vector of rates.}
\item{shape}{Shape parameter.}
\item{shape1, shape2}{Non-negative parameters of the Beta distribution.}
\item{meanlog, sdlog}{Mean and standard deviation of the distribution on the log scale }
\item{min, max}{Lower and upper limits of the distribution}
\item{m, n, k}{The number of white balls in the urn, number of black balls in the urn,
               the number of balls drawn from the urn}
\item{dist}{Probability distribution. The supported distributions are "normal", "binomial", "exponential", "poisson", "chi-square", "logistic", "cauchy", "beta", "gamma", "geometric", "Student's t","F", "weibull", "negative binomial", "log-normal", "uniform", "hypergeometric".}
\item{type}{Probability density or cumulative distribution functions.}
\item{show_color}{If TRUE, pdplot2 uses colors for different distributions. Default is FALSE.}
\item{linetype}{A character vector of linetypes. Default line types are c("solid", "dashed", "dotted", "dotdash", "longdash", twodash", "1F", "F1", "4C88C488", "12345678").}
\item{color}{A character vector of colors. Default colors are c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6", "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000").}

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{c
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\references{
H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York,
  2016.

Karthik Ram and Hadley Wickham (2018). wesanderson: A Wes Anderson Palette Generator. R
  package version 0.3.6. https://CRAN.R-project.org/package=wesanderson

R Core Team (2020). R: A language and environment for statistical computing. R Foundation
  for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.}

\author{Tahir Enes Gedik
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
This function uses only the probability density/mass and cumulative distribution functions in base R (stats package). It also requires {ggplot2} package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{g
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

# Plotting normal density function
pdplot2(seq(-5,5,0.01), mean=1, sd=1, dist="normal", type="PDF")

# Plotting several normal density functions
pdplot2(seq(-6,6,0.01), mean=c(0,0), sd=c(1,2), dist="normal", type="PDF")
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%## The function is currently defined as
%function (x)
%{
%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
