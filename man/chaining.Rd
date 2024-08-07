\name{chaining}

\alias{unchain}
\alias{chain}
\alias{rebase}
\alias{convert}

\title{Chain-linking, rebasing and frequency conversion}

\author{Sebastian Weinand}

\description{
Function \code{unchain()} decouples a chained index series with monthly frequency. These unchained index series can be aggregated into higher-level indices using \code{\link{aggregate}()}. To obtain a longterm index series, the higher-level indices must be chained using function \code{chain()}. Finally, \code{rebase()} sets the index reference period. Monthly indices can be converted into annual or quarterly indices using function \code{convert()}.}

\usage{
unchain(x, t, by=12)

chain(x, t, by=12)

rebase(x, t, t.ref, verbose=FALSE)

convert(x, t, freq="annual")
}

\arguments{
  \item{x}{numeric vector of index values}
  \item{t}{date vector}
  \item{by}{for annual overlap \code{NULL}; for one-month overlap a single integer between 1 and 12 specifying the price reference month}
  \item{t.ref}{character specifying the index reference period. Could be a whole year (\code{YYYY}) or a single year-month (\code{YYYY-MM}).}
  \item{verbose}{logical indicating if messages regarding the index reference period should be printed to the console or not.}
  \item{freq}{frequency of converted index. Either \code{annual} or \code{quarterly}.}
}

\value{
Functions \code{unchain()}, \code{chain()} and \code{rebase()} return numeric values of the same length as \code{x}.

Function \code{convert()} returns a named vector of the length of quarter or years available in \code{t}, where the names correspond to the years or quarters.
}

\details{
Function \code{unchain()} sets the value of the first price reference period to \code{NA} although the value could be set to 100 (if \code{by} is not \code{NULL}) or 100 divided by the average of the year (if \code{by=NULL}). This is wanted to avoid aggregation of these values. Function \code{chain()} finally sets the values back to 100.

}

\seealso{
\code{\link{aggregate}}
}

\references{
European Commission, Eurostat, \emph{Harmonised Index of Consumer Prices (HICP) - Methodological Manual - 2024 edition}, Publications Office of the European Union, 2024, \url{https://data.europa.eu/doi/10.2785/055028}.
}

\examples{
### EXAMPLE 1

t <- seq.Date(from=as.Date("2021-12-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

100*p/p[1]
chain(unchain(p, t, by=12), t, by=12)

convert(x=p, t=t, freq="q") # quarterly index

t <- seq.Date(from=as.Date("2021-01-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

100*p/mean(p[1:12])
(res <- chain(unchain(p, t, by=NULL), t, by=NULL))
# note that for backwards compability, each month in the first
# year receives an index value of 100. this allows the same
# computation again:
chain(unchain(res, t, by=NULL), t, by=NULL)

### EXAMPLE 2
\donttest{
# set cores for testing on CRAN:
library(restatapi)
options(restatapi_cores=1)
library(data.table)

# get hicp index values for euro area with base 2015:
dt <- hicp.dataimport(id="prc_hicp_midx", filter=list(unit="I15", geo="EA"))
dt[, "time":=as.Date(paste0(time, "-01"))]
setkeyv(x=dt, cols=c("unit","coicop","time"))

# check chain-linked indices against published data:
dt[, "dec_ratio" := unchain(x=values, t=time), by="coicop"]
dt[, "chained_index" := chain(x=dec_ratio, t=time), by="coicop"]
dt[, "index_own" := rebase(x=chained_index, t=time, t.ref="2015"), by="coicop"]
dt[abs(values-index_own)>0.01,] # should be empty

# check converted indices against published data:
dta <- dt[, as.data.table(convert(x=values, t=time), keep.rownames=TRUE), by="coicop"]
setnames(x=dta, c("coicop","time","index"))
aind <- hicp.dataimport(id="prc_hicp_aind", filter=list(unit="INX_A_AVG", geo="EA"))
aind[, c("geo","unit") := NULL]
dtcomp <- merge(x=aind, y=dta, by=c("coicop","time"), all=TRUE)
dtcomp[abs(values-index)>0.01,] # should be empty
}}
