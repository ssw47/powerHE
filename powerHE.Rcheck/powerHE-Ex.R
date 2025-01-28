pkgname <- "powerHE"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "powerHE-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('powerHE')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("formatHE")
### * formatHE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: formatHE
### Title: Format powerHE Results
### Aliases: formatHE
### Keywords: formatHE

### ** Examples

# Example TTE endpoint with formatting:

endpoints_input <- list(
  list(type = "TTE",
       hr = 0.8,
       er.b = 0.25,
       s = 12,
       tte.winning.direction = "GT")
)
results <- powerHE(endpoints_input,
               sample.size = 100,
               alpha = 0.05,
               rratio = 0.5,
               output = "ALL")
formatHE(results)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("formatHE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("powerHE")
### * powerHE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: powerHE
### Title: Hierarchical Endpoints
### Aliases: powerHE
### Keywords: endpoints

### ** Examples

# For all examples, A is the default for the active group and B is the
# default for the control group.

### Two continuous (type = "Continuous"):
# For the first endpoint, the marginal distribution for the active group (A)
# follows a normal distribution with a mean of 15 (mu.a = 15) and a standard
# deviation of 60 (sd.a = 60), while the control group (B) also follows a
# normal distribution with a mean of 4 (mu.b = 4) and a standard deviation of
# 60 (sd.b = 60). The threshold to win is 5 (delta = 5) and a longer time to
# event is better (continuous.winning.direction = “GT”).

# For the second endpoint, the marginal distribution for the active group (A)
# follows a normal distribution with a mean of 40 (mu.a = 40) and a standard
# deviation of 24 (sd.a = 24), while the control group (B) also follows a
# normal distribution with a mean of 30 (mu.b = 30) and a standard deviation
# of 24 (sd.b = 24). The threshold to win is 5 (delta = 5) and a longer time
# to event is better (continuous.winning.direction = “GT”).

# We seek to find the required sample size to achieve a power of 0.85
# (power = 0.85) for detecting an overall win ratio calculated based on the
# inputted parameters of the marginal distributions with an alpha level of
# 0.05 (alpha = 0.05) and a 1:1 randomization ratio (rratio = 0.5).

endpoints_input <- list(
  list(type = "Continuous",
       mu.a = 15,
       mu.b = 4,
       sd.a = 60,
       sd.b = 60,
       delta = 5,
       continuous.winning.direction = "GT"),
  list(type = "Continuous",
       mu.a = 40,
       mu.b = 30,
       sd.a = 24,
       sd.b = 24,
       delta = 5,
       continuous.winning.direction = "GT")
)
powerHE(endpoints_input,
    power = 0.85,
    alpha = 0.05,
    rratio = 0.5,
    output = "ALL")

### Two binary (type = "Binary"):
# For the first endpoint, the marginal distribution for the active group (A)
# follows a binomial distribution with a success probability of 0.90
# (pi.a = 0.9) for one trial, while the control group (B) also follows a
# binomial distribution with a success probability of 0.85 (pi.b = 0.85) for
# one trial. A 1 represents a win (binary.winning.direction = "GT").

# For the second endpoint, the marginal distribution for the active group (A)
# follows a binomial distribution with a success probability of 0.80
# (pi.a = 0.8) for one trial, while the control group (B) also follows a
# binomial distribution with a success probability of 0.75 (pi.b = 0.75) for
# one trial. A 1 represents a win (binary.winning.direction = "GT").

# We seek to find the achieved power for detecting an overall win ratio
# calculated based on the inputted parameters of the marginal distributions
# with a sample size of 1098 (sample.size = 1098) with an alpha level
# of 0.05 (alpha = 0.05) and a 1:1 randomization ratio (rratio = 0.5).

endpoints_input <- list(
  list(type = "Binary",
      pi.a = 0.9,
      pi.b = 0.85,
      binary.winning.direction = "GT"),
  list(type = "Binary",
      pi.a = 0.8,
      pi.b = 0.75,
      binary.winning.direction = "GT")
)
powerHE(endpoints_input,
    sample.size = 1098,
    alpha = 0.05,
    rratio = 0.5,
    output = "ALL")

### One binary (type = "Binary") and one continuous (type = "Continuous"):
# For the first endpoint, the marginal distribution for the active group (A)
# follows a binomial distribution with a success probability of 0.96
# (pi.a = 0.96) for one trial, while the control group (B) also follows a
# binomial distribution with a success probability of 0.95 (pi.b = 0.95). A 1
# represents a win (binary.winning.direction = "GT").

# For the second endpoint, the marginal distribution for the active group (A)
# follows a normal distribution with a mean of 36 (mu.a = 36) and a standard
# deviation of 24 (sd.a = 24), while the control group (B) also follows a
# normal distribution with a mean of 31 (mu.b = 31) and a standard
# deviation of 24 (sd.b = 24). The threshold to win is 5 (delta = 5) and a
# longer time to event is better (continuous.winning.direction = “GT”).

# We seek to find the required sample size to achieve a power of 0.85
# (power = 0.85) for detecting an overall win ratio calculated based on the
# inputted parameters of the marginal distributions with an alpha level of
# 0.05 (alpha = 0.05) and a 1:1 randomization ratio (rratio = 0.5).

endpoints_input <- list(
  list(type = "Binary",
       pi.a = 0.96,
       pi.b = 0.95,
       binary.winning.direction = "GT"),
  list(type = "Continuous",
       mu.a = 36,
       mu.b = 31,
       sd.a = 24,
       sd.b = 24,
       delta = 5,
       continuous.winning.direction = "GT")
)
powerHE(endpoints_input,
    power = 0.85,
    alpha = 0.05,
    rratio = 0.5,
    output = "ALL")

### One TTE (type = "TTE") and one count (type = "Count"):
# For the first endpoint, the marginal distribution for the active group (A)
# follows an exponential distribution with a rate parameter of 0.16, while
# the control group (B) also follows an exponential distribution with a rate
# parameter of 0.20 (hr.a = 0.16 / 0.20 = 0.8). The follow-up time is 5 years
# (s = 5, er.b = 1 - exp(-0.20 * 5) = 0.63212), and a longer time to event is
# a win (tte.winning.direction = "GT").

# For the second endpoint, the number of hospitalizations for the active
# (A) follows a Poisson distribution with a mean of 0.75 (lam.a = 0.75),
# while the number of hospitalization in the control group (B) also follows a
# Poisson distribution with a mean of 1.1 (lam.b = 1.1). A smaller count is a
# win (count.winning.direction = "GT").

# We seek to find the achieved power for detecting an overall win ratio
# calculated based on the inputted parameters of the marginal distributions
# with a sample size of 770 (sample.size = 770) with an alpha level
# of 0.05 (alpha = 0.05) and a 1:1 randomization ratio (rratio = 0.5).

endpoints_input <- list(
  list(type = "TTE",
       tte.winning.direction = "GT",
       hr.a = 0.8,
       er.b = 0.63212,
       s = 5),
  list(type = "Count",
       count.winning.direction = "LT",
       lam.a = 0.75,
       lam.b = 1.1)
)
powerHE(endpoints_input,
    sample.size = 770,
    alpha = 0.05,
    rratio = 0.5,
    output = "ALL")

### Two ordinal (each with ordinal categories 1, 2, and 3) (type = "Ordinal"):
# For the first endpoint, the marginal distribution for the active group (A)
# follows a multinomial distribution with probabilities for the three
# categories (1, 2, 3) given by (0.45, 0.30, 0.25) (pi.ordinal.a = c(0.45,
# 0.3, 0.25)), where each of the probabilities represent the likelihood of a
# subject being in categories 1, 2, or 3. The control group (B) also follows
# a multinomial distribution with probabilities for the same three categories
# given by (0.50, 0.30, 0.20) (pi.ordinal.b = c(0.5, 0.3, 0.2)). A subject in
# a higher ordinal category wins over a subject in a lower ordinal category
# (ordinal.winning.direction = “GT").

# For the second endpoint, the marginal distribution for the active group (A)
# follows a multinomial distribution with probabilities for the three
# categories (1, 2, 3) given by (0.30, 0.30, 0.40) (pi.ordinal.a = c(0.3,
# 0.3, 0.4)), where each of the probabilities represent the likelihood of a
# subject being in categories 1, 2, or 3. The control group (B) also follows
# a multinomial distribution with probabilities for the same three categories
# given by (0.40, 0.30, 0.30) (pi.ordinal.b = c(0.4, 0.3, 0.3)). A subject in
# a higher ordinal category wins over a subject in a lower ordinal category
# (ordinal.winning.direction = “GT").

# We seek to find the required sample size to achieve a power of 0.85
# (power = 0.85) for detecting an overall win ratio calculated based on the
# inputted parameters of the marginal distributions with an alpha level of
# 0.05 (alpha = 0.05) and a 1:1 randomization ratio (rratio = 0.5).

endpoints_input <- list(
  list(type = "Ordinal",
       pi.ordinal.a = c(0.45, 0.3, 0.25),
       pi.ordinal.b = c(0.5, 0.3, 0.2),
       ordinal.winning.direction = "GT"),
  list(type = "Ordinal",
       pi.ordinal.a = c(0.3, 0.3, 0.4),
       pi.ordinal.b = c(0.4, 0.3, 0.3),
       ordinal.winning.direction = "GT")
)
powerHE(endpoints_input,
    power = 0.85,
    alpha = 0.05,
    rratio = 0.5,
    output = "ALL")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("powerHE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
