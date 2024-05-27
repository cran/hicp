## ----include = FALSE----------------------------------------------------------
Sys.setenv(LANGUAGE="en")

# set cores for testing on CRAN via devtools::check_rhub()
library(restatapi)
options(restatapi_cores=1)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(hicp) # load package

## ----warning=FALSE------------------------------------------------------------
dtd <- hicp.datasets()
dtd[1:5, list(title, code, lastUpdate, values)]

## ----warning=FALSE------------------------------------------------------------
# dataset 'prc_hicp_inw':
dtf <- hicp.datafilters(id="prc_hicp_inw")

# allowed filters:
unique(dtf$concept)

# allowed filter values:
dtf[1:5,]

## ----warning=FALSE------------------------------------------------------------
# download item weights for euro area from 2015 on:
item.weights <- hicp.dataimport(id="prc_hicp_inw", filters=list("geo"=c("EA","DE","FR")), date.range=c("2015", NA), flags=TRUE)

# inspect data:
item.weights[1:5, ]
nrow(item.weights) # number of observations
unique(item.weights$geo) # only EA, DE, and FR
range(item.weights$time) # since 2015

## ----warning=FALSE------------------------------------------------------------
# example codes:
ids <- c("00","CP00","13","08X")

# check for bundle codes:
is.bundle(id=ids)

# unbundle any bundle codes into their components:
unbundle(id=ids)

# check if valid COICOP code including bundle codes:
is.coicop(id=ids, unbundle=TRUE)

# check if valid COICOP code excluding bundle codes:
is.coicop(id=ids, unbundle=FALSE)

## ----warning=FALSE------------------------------------------------------------
# example codes:
ids <- c("00","01","011","01111","01112")

# no direct parent for 01111 and 01112:
parent(id=ids, flag=FALSE, direct=TRUE)

# indirect parent available:
parent(id=ids, flag=FALSE, direct=FALSE)

# 011 has two (indirect) childs:
child(id=ids, flag=FALSE, direct=FALSE)

## ----warning=FALSE------------------------------------------------------------
# load data.table-package:
require(data.table)

# subset and adjust item weights table:
item.weights <- item.weights[grepl("^CP", coicop),]
item.weights[, "coicop":=gsub(pattern="^CP", replacement="", x=coicop)]

# derive separate trees for each time period and country:
item.weights[, "tree1" := tree(id=coicop, w=values, w.tol=0.1), by=c("geo","time")]
item.weights[tree1==TRUE,
        list("n"=uniqueN(coicop),           # varying coicops over time and space
             "w"=sum(values, na.rm=TRUE)),  # weight sums should equal 1000
        by=c("geo","time")]

# derive merged trees over time, but not across countries:
item.weights[, "tree2" := tree(id=coicop, by=time, w=values, w.tol=0.1), by="geo"]
item.weights[tree2==TRUE,
        list("n"=uniqueN(coicop),           # same selection over time in a country
             "w"=sum(values, na.rm=TRUE)),  # weight sums should equal 1000
        by=c("geo","time")]

# derive merged trees over countries and time:
item.weights[, "tree3" := tree(id=coicop, by=paste(geo,time), w=values, w.tol=0.1)]
item.weights[tree3==TRUE,
        list("n"=uniqueN(coicop),           # same selection over time and across countries
             "w"=sum(values, na.rm=TRUE)),  # weight sums should equal 1000
        by=c("geo","time")]

## ----warning=FALSE, fig.width=7, fig.align="center"---------------------------
# load data.table-package:
require(data.table)

# import monthly price indices:
prc <- hicp.dataimport(id="prc_hicp_midx", 
                       filter=list(unit="I15", geo="EA"),
                       date.range=c("2014-12", NA))
prc[, "time":=as.Date(paste0(time, "-01"))]
prc[, "year":=as.integer(format(time, "%Y"))]
prc[, "coicop" := gsub(pattern="^CP", replacement="", x=coicop)]
setnames(x=prc, old="values", new="index")

# unchain price indices:
prc[, "dec_ratio" := unchain(x=index, t=time), by="coicop"]

# import item weights:
inw <- item.weights[geo=="EA", list(coicop,geo,time,values)]
inw[, "time":=as.integer(time)]
setnames(x=inw, old=c("time","values"), new=c("year","weight"))

# derive coicop tree:
inw[ , "tree":=tree(id=coicop, w=weight, w.tol=0.1), by=c("geo","year")]

# merge price indices and item weights:
hicp.data <- merge(x=prc, y=inw, by=c("geo","coicop","year"), all.x=TRUE)
hicp.data <- hicp.data[year <= year(Sys.Date())-1,]

# compute all-items HICP in one aggregation step:
hicp.own <- hicp.data[tree==TRUE, 
                      list("laspey"=laspeyres(x=dec_ratio, w0=weight)), 
                      by="time"]
setorderv(x=hicp.own, cols="time")
hicp.own[, "chain_laspey" := chain(x=laspey, t=time, by=12)]
hicp.own[, "chain_laspey_15" := rebase(x=chain_laspey, t=time, t.ref="2015")]

# add published all-items HICP for comparison:
hicp.own <- merge(x=hicp.own,
                  y=hicp.data[coicop=="00", list(time, index)],
                  by="time",
                  all.x=TRUE)
plot(index-chain_laspey_15~time, 
     data=hicp.own, type="l", 
     xlab="Time", ylab="Difference (in index points)")
title("Difference between published index and own calculations")
abline(h=0, lty="dashed")

## ----warning=FALSE, fig.width=7, fig.align="center"---------------------------
# compute all-items HICP stepwise through all higher-levels:
hicp.own.all <- hicp.data[is.coicop(coicop), 
                          aggregate(x=dec_ratio, w0=weight, grp=coicop, index=laspeyres),
                          by="time"]
setorderv(x=hicp.own.all, cols="time")
hicp.own.all[, "chain_laspey" := chain(x=laspeyres, t=time, by=12), by="grp"]
hicp.own.all[, "chain_laspey_15" := rebase(x=chain_laspey, t=time, t.ref="2015"), by="grp"]

# compare all-items HICP from direct and step-wise aggregation:
agg.comp <- merge(x=hicp.own.all[grp=="00", list(time, "index_stpwse"=chain_laspey_15)],
                  y=hicp.own[, list(time, "index_direct"=chain_laspey_15)],
                  by="time")

# no differences -> consistent in aggregation:
nrow(agg.comp[abs(index_stpwse-index_direct)>1e-4,])

## ----warning=FALSE, fig.width=7, fig.align="center"---------------------------
# compute annual rates of change for the all-items HICP:
hicp.data[, "ar" := rates(x=index, t=time, type="annual"), by=c("geo","coicop")]

# add all-items hicp:
hicp.data <- merge(x=hicp.data,
                   y=hicp.data[coicop=="00", list(geo,time,index,weight)],
                   by=c("geo","time"), all.x=TRUE, suffixes=c("","_all"))

# ribe decomposition:
hicp.data[, "ribe" := contrib(x=index, w=weight, t=time, x.all=index_all, w.all=weight_all), by="coicop"]

# annual change rates over time:
plot(ar~time, data=hicp.data[coicop=="00",],
     type="l", xlab="Time", ylab="", ylim=c(-2,12))
lines(ribe~time, data=hicp.data[coicop=="01"], col="red")
title("Contributions of food to overall inflation")
legend("topleft", col=c("black","red"), lty=1, bty="n", 
       legend=c("Overall inflation (in %)", "Contributions of food (in pp-points)"))

