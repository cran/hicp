## ----include = FALSE----------------------------------------------------------
Sys.setenv(LANGUAGE="en")

# set cores for testing on CRAN via devtools::check_rhub()
library(restatapi)
options(restatapi_cores=1)

# load additional packages:
library(data.table)
options(datatable.print.nrows=10)
options(datatable.print.topn=5)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
# load package:
library(hicp)

# set global options:
options(hicp.coicop.version="ecoicop.hicp")         # the coicop version to be used
options(hicp.coicop.bundles=hicp:::coicop.bundles)  # coicop bundle code dictionary (e.g., 08X)
options(hicp.all.items.code="00")                   # internal code for the all-items index
options(hicp.chatty=TRUE)                           # print package-specific messages and warnings

## ----echo=FALSE---------------------------------------------------------------
load(file.path("data", "hicp_datasets.RData"))

## ----eval=FALSE---------------------------------------------------------------
# # download table of available HICP data sets:
# dtd <- datasets()

## ----warning=FALSE------------------------------------------------------------
dtd[1:5, list(title, code, lastUpdate, values)]

## ----echo=FALSE---------------------------------------------------------------
load(file.path("data", "hicp_datafilters.RData"))

## ----eval=FALSE---------------------------------------------------------------
# # download allowed filters for data set 'prc_hicp_inw':
# dtf <- datafilters(id="prc_hicp_inw")

## ----warning=FALSE------------------------------------------------------------
# allowed filters:
unique(dtf$concept)

# allowed filter values:
dtf[1:5,]

## ----echo=FALSE---------------------------------------------------------------
load(file.path("data", "hicp_itemweights.RData"))

## ----eval=FALSE---------------------------------------------------------------
# # download item weights with filters:
# item.weights <- hicp::data(id="prc_hicp_inw",
#                            filters=list("geo"=c("EA","DE","FR")),
#                            date.range=c("2015","2024"),
#                            flags=TRUE)

## ----warning=FALSE------------------------------------------------------------
# inspect data:
item.weights[1:5, ]
nrow(item.weights) # number of observations
unique(item.weights$geo) # only EA, DE, and FR
range(item.weights$time) # from 2015 to 2023

## ----warning=FALSE------------------------------------------------------------
# example codes:
ids <- c("00","CP00","01","08X")

# check if bundle codes:
is.bundle(id=ids)

# unbundle any bundle codes into their components:
unbundle(id=ids)

# bundle codes are no valid ECOICOP codes:
is.coicop(id=ids)

# games of chance have a valid ECOICOP code:
is.coicop("0943", settings=list(coicop.version="ecoicop"))

# but not in the ECOICOP-HICP version 1:
is.coicop("0943", settings=list(coicop.version="ecoicop.hicp"))

## ----warning=FALSE------------------------------------------------------------
# example codes:
ids <- c("00","01","011","01111","01112")

# no direct parent for 01111 and 01112 available:
parent(id=ids, usedict=FALSE, closest=FALSE, k=1)

# but 011 is one indirect (or closest) parent:
parent(id=ids, usedict=FALSE, closest=TRUE)

# while 011 has two (indirect) children:
child(id=ids, usedict=FALSE, closest=TRUE)

## ----warning=FALSE------------------------------------------------------------
# adjust COICOP codes:
item.weights[, "coicop":=gsub(pattern="^CP", replacement="", x=coicop)]

# derive separate trees for each time period and country:
item.weights[, "t1" := tree(id=coicop, w=values, flag=TRUE, settings=list(w.tol=0.1)), by=c("geo","time")]
item.weights[t1==TRUE,
        list("n"=uniqueN(coicop),           # varying coicops over time and space
             "w"=sum(values, na.rm=TRUE)),  # weight sums should equal 1000
        by=c("geo","time")][order(geo,time),]

# derive merged trees over time, but not across countries:
item.weights[, "t2" := tree(id=coicop, by=time, w=values, flag=TRUE, settings=list(w.tol=0.1)), by="geo"]
item.weights[t2==TRUE,
        list("n"=uniqueN(coicop),           # same selection over time in a country
             "w"=sum(values, na.rm=TRUE)),  # weight sums should equal 1000
        by=c("geo","time")][order(geo,time),]

# derive merged trees over countries and time:
item.weights[, "t3" := tree(id=coicop, by=paste(geo,time), w=values, flag=TRUE, settings=list(w.tol=0.1))]
item.weights[t3==TRUE,
        list("n"=uniqueN(coicop),           # same selection over time and across countries
             "w"=sum(values, na.rm=TRUE)),  # weight sums should equal 1000
        by=c("geo","time")][order(geo,time),]

## ----echo=FALSE---------------------------------------------------------------
load(file.path("data", "hicp_prices.RData"))

## ----eval=FALSE---------------------------------------------------------------
# # download monthly price indices:
# prc <- hicp::data(id="prc_hicp_midx",
#                   filter=list(unit="I15", geo="EA"),
#                   date.range=c("2014-12", "2024-12"))

## ----warning=FALSE------------------------------------------------------------
# manipulate data:
prc[, "time":=as.Date(paste0(time, "-01"))]
prc[, "year":=as.integer(format(time, "%Y"))]
prc[, "coicop" := gsub(pattern="^CP", replacement="", x=coicop)]
setnames(x=prc, old="values", new="index")

## ----warning=FALSE------------------------------------------------------------
# unchain price indices:
prc[, "dec_ratio" := unchain(x=index, t=time), by="coicop"]

## ----warning=FALSE------------------------------------------------------------
# manipulate item weights:
inw <- item.weights[geo=="EA", list(coicop,geo,time,values,t1)]
inw[, "time":=as.integer(time)]
setnames(x=inw, old=c("time","values","t1"), new=c("year","weight","tree"))

# merge price indices and item weights:
hicp.data <- merge(x=prc, y=inw, by=c("geo","coicop","year"), all.x=TRUE)

## ----warning=FALSE, fig.width=7, fig.align="center"---------------------------
# compute all-items HICP in one aggregation step:
hicp.own <- hicp.data[tree==TRUE, 
                      list("laspey"=laspeyres(x=dec_ratio, w0=weight)), 
                      by="time"]
setorderv(x=hicp.own, cols="time")

# chain the resulting index:
hicp.own[, "chain_laspey" := chain(x=laspey, t=time, by=12)]

# rebase the index to 2015:
hicp.own[, "chain_laspey_15" := rebase(x=chain_laspey, t=time, t.ref="2015")]

# plot all-items index:
plot(chain_laspey_15~time, data=hicp.own, type="l", xlab="Time", ylab="Index")
title("Euro area HICP")
abline(h=0, lty="dashed")

## ----warning=FALSE------------------------------------------------------------
# compute all-items HICP gradually from bottom to top:
hicp.own.all <- hicp.data[ , aggregate.tree(x=dec_ratio, w0=weight, id=coicop, formula=laspeyres), 
                           by="time"]
setorderv(x=hicp.own.all, cols="time")
hicp.own.all[, "chain_laspey" := chain(x=laspeyres, t=time, by=12), by="id"]
hicp.own.all[, "chain_laspey_15" := rebase(x=chain_laspey, t=time, t.ref="2015"), by="id"]

## ----warning=FALSE------------------------------------------------------------
# compare all-items HICP from direct and step-wise aggregation:
agg.comp <- merge(x=hicp.own.all[id=="00", list(time, "index_stpwse"=chain_laspey_15)],
                  y=hicp.own[, list(time, "index_direct"=chain_laspey_15)],
                  by="time")

# no differences -> consistent in aggregation:
nrow(agg.comp[abs(index_stpwse-index_direct)>1e-4,])

## ----warning=FALSE------------------------------------------------------------
# compute food and energy by aggregation:
spa <- spec.aggs[code%in%c("FOOD","NRG"), ]
hicp.data[time>="2019-12-01",
          aggregate(x=dec_ratio, w0=weight, id=coicop,
                    agg=spa$composition,
                    settings=list(names=spa$code)), 
          by="time"]

# compute overall index excluding food and energy by disaggregation
hicp.data[time>="2019-12-01",
          disaggregate(x=dec_ratio, w0=weight, id=coicop,
                       agg=list("00"=c("FOOD","NRG")),
                       settings=list(names="TOT_X_FOOD_NRG")), 
          by="time"]

## ----warning=FALSE, fig.width=7, fig.align="center"---------------------------
# compute annual rates of change for the all-items HICP:
hicp.data[, "ar" := rates(x=index, t=time, type="year"), by=c("geo","coicop")]

# add all-items hicp:
hicp.data <- merge(x=hicp.data,
                   y=hicp.data[coicop=="00", list(geo,time,index,weight)],
                   by=c("geo","time"), all.x=TRUE, suffixes=c("","_all"))

# ribe decomposition:
hicp.data[, "ribe" := contrib(x=index, w=weight, t=time, 
                              x.all=index_all, w.all=weight_all, type="year"),
          by="coicop"]

# annual change rates and contribtuions over time:
plot(ar~time, data=hicp.data[coicop=="00",],
     type="l", xlab="Time", ylab="", ylim=c(-2,12))
lines(ribe~time, data=hicp.data[coicop=="011"], col="red")
title("Contributions of food to overall inflation")
legend("topleft", col=c("black","red"), lty=1, bty="n", 
       legend=c("Overall inflation (in %)", "Contributions of food (in pp-points)"))

## ----echo=FALSE---------------------------------------------------------------
load(file.path("data", "ooh_prices.RData"))
load(file.path("data", "ooh_itemweights.RData"))

## ----eval=FALSE---------------------------------------------------------------
# # download quarterly OOHPI for euro area:
# dtp <- hicp::data(id="prc_hpi_ooq",
#                   filter=list(unit="I15_Q", geo="EA"),
#                   date.range=c("2014-10","2024-12"))
# 
# # download annual OOH weights for euro area:
# dtw <- hicp::data(id="prc_hpi_ooinw",
#                   filter=list(geo="EA"),
#                   date.range=c("2014","2024"))

## ----warning=FALSE------------------------------------------------------------
# manipulate indices:
dtp[, c("year","quarter") := tstrsplit(x=time, split="-Q", fixed=TRUE)]
dtp[, "year":=as.integer(year)]
dtp[, "quarter":=as.integer(quarter)]
dtp[, "time":=as.Date(paste(year, quarter*3, "01", sep="-"), format="%Y-%m-%d")]
dtp[, c("unit","quarter"):=NULL]
setnames(x=dtp, old="values", new="index")

# manipulate item weights:
dtw[, "year":=as.integer(time)]
dtw[, c("unit","time"):=NULL]
setnames(x=dtw, old="values", new="weight")

# merge indices and item weights:
dtooh <- merge(x=dtp, y=dtw, by=c("geo","expend","year"), all.x=TRUE)
setcolorder(x=dtooh, neworder=c("geo","expend","year","time"))
setkeyv(x=dtooh, cols=c("geo","expend","time"))

## -----------------------------------------------------------------------------
# unchain indices:
dtooh[, "ratio" := unchain(x=index, t=time, by=12L), by="expend"]

## -----------------------------------------------------------------------------
# aggregate, chain and rebase:
dtagg <- dtooh[expend%in%c("DW_ACQ","DW_OWN"), list("oohpi"=laspeyres(x=ratio, w0=weight)), by="time"]
dtagg[, "oohpi" := chain(x=oohpi, t=time)]
dtagg[, "oohpi" := rebase(x=oohpi, t=time, t.ref="2015")]

## -----------------------------------------------------------------------------
# derive annual change rates:
dtagg[, "ar" := rates(x=oohpi, t=time, type="year", settings=list(freq="quarter"))]

