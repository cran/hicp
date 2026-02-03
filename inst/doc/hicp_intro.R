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
options(hicp.coicop.version="ecoicop2.hicp")    # COICOP version to be used
options(hicp.coicop.prefix="CP")                # prefix of COICOP codes
options(hicp.all.items.code="TOTAL")            # internal code for the all-items index
options(hicp.chatty=TRUE)                       # print package-specific messages and warnings

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
# dtf <- datafilters(id="prc_hicp_iw")

## ----warning=FALSE------------------------------------------------------------
# allowed filters:
unique(dtf$concept)

# allowed filter values:
dtf[1:5,]

## ----eval=FALSE---------------------------------------------------------------
# # download all available item weights:
# hicp::data(id="prc_hicp_iw", flags=TRUE)

## ----echo=FALSE---------------------------------------------------------------
load(file.path("data", "hicp_itemweights.RData"))

## ----eval=FALSE---------------------------------------------------------------
# # download item weights with filters:
# item.weights <- hicp::data(id="prc_hicp_iw",
#                            filters=list("geo"=c("EA","DE","FR")),
#                            date.range=c("2019","2025"),
#                            flags=TRUE)

## ----warning=FALSE------------------------------------------------------------
item.weights[1:5, ]

## ----warning=FALSE------------------------------------------------------------
# all-items code and codes without prefix "CP" are no valid ECOICOP codes:
is.coicop(id=c("TOTAL","CP01","CP011","CP012","012"))

# games of chance are not valid in ECOICOP-HICP ver. 1:
is.coicop("CP0943", settings=list(coicop.version="ecoicop1.hicp"))

# but in ECOICOP-HICP ver. 2:
is.coicop("CP0943", settings=list(coicop.version="ecoicop2.hicp"))

## ----warning=FALSE------------------------------------------------------------
# get parents:
parent(id=c("CP01","CP011","CP01111","CP01112"), usedict=TRUE)

# get children:
child(id=c("CP01","CP011","CP01111","CP01112"), usedict=TRUE)

## ----warning=FALSE------------------------------------------------------------
# example codes:
ids <- c("CP01","CP011","CP012","CP0111","CP0112")

# derive COICOP tree from top to bottom:
tree(ids)

# still same tree because weights add up:
tree(id=ids, w=c(0.2,0.08,0.12,0.05,0.03)) 

# now (CP011,CP012) because weights do not correctly add up at lower levels:
tree(id=ids, w=c(0.2,0.08,0.12,0.05,0.01))

## ----warning=FALSE------------------------------------------------------------
# validate codes:
is.spec.agg(id=c("TOTAL","CP01","FOOD","NRG"))

# get compositions of non-processed food and energy:
spec.agg(id=c("FOOD_NP","NRG"))

## ----echo=FALSE---------------------------------------------------------------
load(file.path("data", "hicp_prices.RData"))

## ----eval=FALSE---------------------------------------------------------------
# # download monthly price indices:
# dtp <- hicp::data(id="prc_hicp_minr",
#                   filters=list(unit="I25", geo="EA"),
#                   date.range=c("2019-12", "2025-12"))

## ----warning=FALSE------------------------------------------------------------
# convert into proper dates:
dtp[, "time":=as.Date(paste0(time, "-01"))]
dtp[, "year":=as.integer(format(time, "%Y"))]
setnames(x=dtp, old="values", new="index")

## ----warning=FALSE------------------------------------------------------------
# unchain price indices:
dtp[, "dec_ratio" := unchain(x=index, t=time), by="coicop18"]

## ----warning=FALSE------------------------------------------------------------
# manipulate item weights:
dtw <- item.weights[geo=="EA", list(coicop18,geo,time,values)]
dtw[, "time":=as.integer(time)]
setnames(x=dtw, old=c("time","values"), new=c("year","weight"))

# merge price indices and item weights:
dtall <- merge(x=dtp, y=dtw, by=c("geo","coicop18","year"), all.x=TRUE)

## ----warning=FALSE, fig.width=7, fig.align="center"---------------------------
# derive COICOP tree for index aggregation:
dtall[weight>0 & !is.na(dec_ratio), 
      "tree" := tree(id=coicop18, w=weight, flag=TRUE, settings=list(w.tol=0.1)),
      by="time"]

# compute all-items HICP in one aggregation step:
hicp.own <- dtall[tree==TRUE, 
                  list("laspey"=laspeyres(x=dec_ratio, w0=weight)), 
                  by="time"]
setorderv(x=hicp.own, cols="time")

# chain the resulting index:
hicp.own[, "chain_laspey" := chain(x=laspey, t=time, by=12)]

# rebase the index to 2025:
hicp.own[, "chain_laspey_25" := rebase(x=chain_laspey, t=time, t.ref="2025")]

# plot all-items index:
plot(chain_laspey_25~time, data=hicp.own, type="l", xlab="Time", ylab="Index")
title("Euro area HICP")
abline(h=0, lty="dashed")

## ----warning=FALSE------------------------------------------------------------
# compute all-items HICP gradually from bottom to top:
hicp.own.all <- dtall[weight>0 & !is.na(dec_ratio),
                      aggregate.tree(x=dec_ratio, w0=weight, id=coicop18, formula=laspeyres), 
                      by="time"]
setorderv(x=hicp.own.all, cols="time")
hicp.own.all[, "chain_laspey" := chain(x=laspeyres, t=time, by=12), by="id"]
hicp.own.all[, "chain_laspey_25" := rebase(x=chain_laspey, t=time, t.ref="2025"), by="id"]

## ----warning=FALSE------------------------------------------------------------
# all-items HICP from direct and gradual aggregation identical:
all(abs(hicp.own.all[id=="TOTAL", chain_laspey_25]-hicp.own$chain_laspey_25)<0.1)

## ----warning=FALSE------------------------------------------------------------
# compute food and energy by aggregation:
dtall[time>="2019-12-01",
      aggregate(x=dec_ratio, w0=weight, id=coicop18,
                agg=spec.agg(id=c("FOOD","NRG")),
                settings=list(exact=FALSE, names=c("FOOD","NRG"))), 
      by="time"]

# compute overall index excluding food and energy by disaggregation:
dtall[time>="2019-12-01",
      disaggregate(x=dec_ratio, w0=weight, id=coicop18,
                   agg=list("TOTAL"=c("FOOD","NRG")),
                   settings=list(names="TOT_X_FOOD_NRG")), 
      by="time"]

## ----warning=FALSE, fig.width=7, fig.height=4, fig.align="center"-------------
# compute annual rates of change for the all-items HICP:
dtall[, "ar" := rates(x=index, t=time, type="year"), by=c("geo","coicop18")]

# add all-items HICP:
dtall <- merge(x=dtall,
               y=dtall[coicop18=="TOTAL", list(geo,time,index,weight)],
               by=c("geo","time"), all.x=TRUE, suffixes=c("","_all"))

# Ribe decomposition:
dtall[, "ribe" := contrib(x=index, w=weight, t=time, 
                          x.all=index_all, w.all=weight_all, type="year"),
      by="coicop18"]

# annual change rates and contribtuions over time:
plot(ar~time, data=dtall[coicop18=="TOTAL",], type="l", xlab="Time", ylab="", ylim=c(-1,13))
lines(ribe~time, data=dtall[coicop18=="CP011"], col="red")
title("Contributions of food to overall inflation")
legend("topleft", col=c("red","black"), lty=1, bty="n", 
       legend=c("Contributions of food (in pp-points)", "Overall inflation (in %)"))

## ----echo=FALSE---------------------------------------------------------------
load(file.path("data", "ooh_prices.RData"))
load(file.path("data", "ooh_itemweights.RData"))

## ----eval=FALSE---------------------------------------------------------------
# # download quarterly OOHPI for euro area:
# dtp <- hicp::data(id="prc_hpi_ooq",
#                   filters=list(unit="I15_Q", geo="EA"),
#                   date.range=c("2014-10","2024-12"))
# 
# # download annual OOH weights for euro area:
# dtw <- hicp::data(id="prc_hpi_ooinw",
#                   filters=list(geo="EA"),
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

