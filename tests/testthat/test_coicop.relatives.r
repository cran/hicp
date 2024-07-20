# START


# Function is.coicop() ----------------------------------------------------


expect_true(all(is.coicop(c("01","011","0111","01111"))))
expect_false(all(is.coicop(c("01","011","0111","01111","0943"))))
expect_true(all(is.coicop(c("01","011","0111","01111","0943"), settings=list(coicop.version="ecoicop"))))
expect_true(is.coicop("08X", settings=list(unbundle=TRUE)))
expect_false(is.coicop("08X", settings=list(unbundle=FALSE)))


# Function level() --------------------------------------------------------


expect_equal(level(id=c("00","01","011","0111","01111","0943")), c(1:5,NA))
expect_equal(level(id=c("00","01","011","0111","01111","0943"), settings=list(coicop.version="ecoicop")), c(1:5,4))
expect_equal(level(id="08X", settings=list(unbundle=TRUE)), 3L)
expect_equal(level(id="08X", settings=list(unbundle=FALSE)), NA_integer_)


# Function parent() ------------------------------------------------------


# derive available parent:
expect_equal(parent(id=NA_character_, direct=T, flag=F), NA_character_)
expect_equal(parent(id=c("00"), direct=T, flag=F), as.character(c(NA)))
expect_equal(parent(id=c("00","021"), direct=T, flag=F), as.character(c(NA,NA)))
expect_equal(parent(id=c("00","021"), direct=F, flag=F), c(NA,"00"))
expect_equal(parent(id=c("00","01","0111"), direct=F, flag=F), c(NA,"00","01"))
expect_equal(parent(id=c("00","01","0111"), direct=T, flag=F), c(NA,"00",NA))
expect_equal(parent(id=c("00","0111"), direct=F, flag=F), c(NA,"00"))
expect_equal(parent(id=c("00","0111"), direct=T, flag=F), as.character(c(NA,NA)))
expect_equal(parent(id=c("00","99"), direct=T, flag=F), as.character(c(NA,NA)))

# flag if parent is available:
expect_equal(parent(id=NA_character_, direct=T, flag=T), as.logical(c(NA)))
expect_equal(parent(id=c("00"), flag=T, direct=T), c(F))
expect_equal(parent(id=c("00","021"), flag=T, direct=T), c(F,F))
expect_equal(parent(id=c("00","021"), flag=T, direct=F), c(F,T))
expect_equal(parent(id=c("00","01","0111"), flag=T, direct=F), c(F,T,T))
expect_equal(parent(id=c("00","01","0111"), flag=T, direct=T), c(F,T,F))
expect_equal(parent(id=c("00","0111"), flag=T, direct=F), c(F,T))
expect_equal(parent(id=c("00","0111"), flag=T, direct=T), c(F,F))
expect_equal(parent(id=c("00","99"), direct=T, flag=T), c(F,NA))

# flag according to different coicop versions:
expect_equal(parent(c("094","0943","09430"), settings=list(coicop.version="ecoicop")), c(F,T,T))
expect_equal(parent(c("094","0943","09430"), settings=list(coicop.version="ecoicop-hicp")), c(F,NA,NA))

# input including bundle codes:
id <- c("08","081","08X","08201","05","053","0531_2","0531","05311")

# if unbundle=FALSE, set all bundle codes to NA before
# otherwise resolve bundles. then, if bundle code is the
# parent, adjustment is needed. parent of bundle code is
# well defined
expect_equal(
  parent(id, direct=F, flag=F, settings=list(unbundle=T)),
  c(NA,"08","08","08X",NA,"05","053","053","0531"))

expect_equal(
  parent(id, direct=F, flag=F, settings=list(unbundle=F)),
  c(NA,"08",NA,"08",NA,"05",NA,"053","0531"))

expect_equal(
  parent(id, direct=T, flag=F, settings=list(unbundle=T)),
  c(NA,"08","08",NA,NA,"05","053","053","0531"))

expect_equal(
  parent(id, direct=T, flag=F, settings=list(unbundle=F)),
  c(NA,"08",NA,NA,NA,"05",NA,"053","0531"))

# check function output for flag=TRUE:
expect_equal(
  parent(id=id, flag=T, direct=T, settings=list(unbundle=T)),
  c(F,T,T,F,F,T,T,T,T))

expect_equal(
  parent(id=id, flag=T, direct=T, settings=list(unbundle=F)),
  c(F,T,NA,F,F,T,NA,T,T))

expect_equal(
  parent(id=id, flag=T, direct=F, settings=list(unbundle=T)),
  c(F,T,T,T,F,T,T,T,T))

expect_equal(
  parent(id=id, flag=T, direct=F, settings=list(unbundle=F)),
  c(F,T,NA,T,F,T,NA,T,T))


# Function child() -------------------------------------------------


# derive available children:
expect_equal(child(id=NA_character_, direct=T, flag=F), list(NA_character_))
expect_equal(child(id=c("00"), flag=F, direct=T), list(character()))
expect_equal(child(id=c("00","021"), flag=F, direct=T), list(character(), character()))
expect_equal(child(id=c("00","021"), flag=F, direct=F), list("021", character()))
expect_equal(child(id=c("00","01","0111"), flag=F, direct=F), list(c("01","0111"), "0111", character()))
expect_equal(child(id=c("00","01","0111"), flag=F, direct=T), list(c("01"), character(), character()))
expect_equal(child(id=c("00","0111"), flag=F, direct=F), list("0111", character()))
expect_equal(child(id=c("00","0111"), flag=F, direct=T), list(character(), character()))
expect_equal(child(id=c("00","99"), direct=T, flag=F), list(character(), NA_character_))

# flag if children is available:
expect_equal(child(id=NA_character_, direct=T, flag=T), NA)
expect_equal(child(id=c("00"), flag=T, direct=T), F)
expect_equal(child(id=c("00","021"), flag=T, direct=T), c(F,F))
expect_equal(child(id=c("00","021"), flag=T, direct=F), c(T,F))
expect_equal(child(id=c("00","01","0111"), flag=T, direct=F), c(T,T,F))
expect_equal(child(id=c("00","01","0111"), flag=T, direct=T), c(T,F,F))
expect_equal(child(id=c("00","0111"), flag=T, direct=F), c(T,F))
expect_equal(child(id=c("00","0111"), flag=T, direct=T), c(F,F))
expect_equal(child(id=c("00","99"), direct=T, flag=T), c(F,NA))

# flag according to different coicop versions:
expect_equal(child(c("094","0943","09430"), settings=list(coicop.version="ecoicop")), c(T,T,F))
expect_equal(child(c("094","0943","09430"), settings=list(coicop.version="ecoicop-hicp")), c(F,NA,NA))

# input including bundle codes:
id <- c("08","081","08X","08201","05","053","0531_2","0531","05311")

# check function output for flag=F:
expect_equal(
  child(id=id, flag=F, direct=T, settings=list(unbundle=T)),
  list(c("081","08X"), character(), character(), character(), "053", c("0531_2", "0531"), "05311", "05311", character()))

expect_equal(
  child(id=id, flag=F, direct=T, settings=list(unbundle=F)),
  list("081", character(), NA_character_, character(), "053", "0531", NA_character_, "05311", character()))

expect_equal(
  child(id=id, flag=F, direct=F, settings=list(unbundle=T)),
  list(c("081","08X","08201"), character(), "08201", character(), c("053","0531_2","0531","05311"), c("0531_2","0531","05311"), "05311", "05311", character()))

expect_equal(
  child(id=id, flag=F, direct=F, settings=list(unbundle=F)),
  list(c("081","08201"), character(), NA_character_, character(), c("053","0531","05311"), c("0531","05311"), NA_character_, "05311", character()))

# check function output for flag=T:
expect_equal(
  child(id=id, flag=T, direct=T, settings=list(unbundle=T)),
  c(T,F,F,F,T,T,T,T,F))

expect_equal(
  child(id=id, flag=T, direct=T,settings=list( unbundle=F)),
  c(T,F,NA,F,T,T,NA,T,F))

expect_equal(
  child(id=id, flag=T, direct=F, settings=list(unbundle=T)),
  c(T,F,T,F,T,T,T,T,F))

expect_equal(
  child(id=id, flag=T, direct=F, settings=list(unbundle=F)),
  c(T,F,NA,F,T,T,NA,T,F))

# END
