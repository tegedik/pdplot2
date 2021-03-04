\name{pd_plot}
\alias{pd_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting Probability Density/Mass Functions
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
`pdplot2` visualizes probability density (mass) functions in R. It requires parameters correctly specified for each distribution.
}
\usage{
pdplot2(x mean, sd, size, prob, rate, lambda, location, scale, df, df1, df2,
shape, shape1, shape2, show.color=FALSE, line.selection=FALSE, linetype=NULL,
color.selection=FALSE, color=NULL, type=c("normal", "binomial", "exponential",
"poisson", "logistic", "cauchy", "chi-square", "beta", "gamma", "geometric",
"Student's t", "F")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ vector of quantiles
%%     ~~Describe \code{x} here~~
}
\item{mean}{vector of means.}
\item{sd}{vector of standard deviations.}
\item{location}{location parameter.}
\item{scale}{scale parameter.}
\item{size}{number of trials (zero or more).}
\item{prob}{probability of success on each trial.}
\item{df}{degrees of freedom (non-negative, but can be non-integer).}
\item{df1, df2}{degrees of freedom. \code{Inf} allowed.}
\item{lambda}{vector of (non-negative) means.}
\item{rate}{vector of rates.}
\item{shape}{shape parameter.}
\item{shape1, shape2}{non-negative parameters of the Beta distribution.}
\item{show.color}{If TRUE, pdplot2 uses colors for different distributions. Default is FALSE.}
\item{color.selection}{If show.color=TRUE and color.selection=TRUE, users can supply a character vector of colors. Default is FALSE.}
\item{line.selection}{If show.color=FALSE (default) and line.selection=TRUE, users can supply a character vector of linetypes. Default is FALSE.}
\item{color}{A character vector of colors}
\teim{line.type}{A character vector of linetypes}
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

R Core Team (2020). R: A language and environment for statistical computing. R Foundation
  for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.}

\author{Tahir Enes Gedik
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
This function uses only the probability density/mass functions in base R (stats package). It also requires {ggplot2} package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{g
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

# Plotting normal density function
pdplot2(seq(-5,5,0.01), mean=1, sd=1, type="normal")

# Plotting several normal density functions
pdplot2(seq(-6,6,0.01), mean=c(0,0), sd=c(1,2), type="normal")



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
