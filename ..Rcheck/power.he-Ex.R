pkgname <- "power.he"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "power.he-Ex.timings", pos = 'CheckExEnv')
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
library('power.he')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("format")
### * format

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: format
### Title: Format HIE Results
### Aliases: format
### Keywords: format

### ** Examples

# Example TTE endpoint with formatting

endpoints_input <- list(
  list(type = "TTE",
       hr = 0.8,
       er.b = 0.25,
       s = 12,
       tte.winning.direction = "GT")
)
results <- hie(endpoints_input,
               sample.size = 100,
               alpha = 0.05,
               rratio = 0.5,
               output = "ALL")
format(results)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("format", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("hie")
### * hie

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hie
### Title: Hierarchical Endpoints
### Aliases: hie
### Keywords: endpoints

### ** Examples

# Two continuous hierarchical endpoints:
# The marginal distributions for Y1A and Y1B are normal distributions with
# means 15 and 4, respectively, and standard deviations of 60. For Y2A and
# Y2B, the marginal distributions are normal distributions with means 40 and
# 30, respectively, and standard deviations of 24. For both endpoints, the
# threshold to win is chosen to be the same, with both delta1 and delta2
# equal to 5.

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
hie(endpoints_input,
    power = 0.85,
    alpha = 0.05,
    rratio = 0.5,
    output = "ALL")

# Two binary hierarchical endpoints:
# The marginal probabilities for Y1A and Y1B are binomial distributions with
# a success probability of 0.90 and 0.85, respectively, for one trial. For
# Y2A and Y2B, the marginal probabilities are binomial distributions with
# success probabilities of 0.80 and 0.75, respectively, for one trial.

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
hie(endpoints_input,
    power = 0.85,
    alpha = 0.05,
    rratio = 0.5,
    output = "ALL")

# Binary and continuous hierarchical endpoints:
# The marginal probabilities for Y1A and Y1B are binomial distributions with
# success probabilities of 0.96 and 0.95, respectively, for one trial. For
# Y2A and Y2B, the marginal distributions are normal distributions with means
# 36 and 31, respectively, and standard deviations of 24.

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
hie(endpoints_input,
    power = 0.85,
    alpha = 0.05,
    rratio = 0.5,
    output = "ALL")

# Time to death and number of hospitalizations as hierarchical endpoints:
# The marginal distributions for Y1A and Y1B are exponential distributions
# with rate parameters of 0.16 and 0.20, respectively. For Y2A, the marginal
# distribution is a Poisson distribution with a mean of 0.75, and for Y2B, it
# is a normal distribution with a mean of 1.1. The follow-up time for all
# measurements is 5 years.

endpoints_input <- list(
  list(type = "TTE",
       tte.winning.direction = "GT",
       s = 5,
       hr.a = 0.8,
       er.b = 0.63212),
  list(type = "Count",
       count.winning.direction = "LT",
       lam.a = 0.75,
       lam.b = 1.1)
)
hie(endpoints_input,
    power = 0.85,
    alpha = 0.05,
    rratio = 0.5,
    output = "ALL")

# Two ordinal hierarchical endpoints, each with 3 ordinal categories:
# The marginal distributions for Y1A and Y1B are multinomial distributions
# with probabilities for the three categories (1, 2, 3) given by
# (0.45, 0.30, 0.25) for Y1A and (0.50, 0.30, 0.20) for Y1B. For Y2A and Y2B,
# the marginal distributions are multinomial distributions with probabilities
# (0.30, 0.30, 0.40) for Y2A and (0.40, 0.30, 0.30) for Y2B. The probabilities
# represent the likelihood of a subject being in categories 1, 2, or 3. We
# assume that a subject in a higher ordinal category wins over a subject in a
# lower ordinal category.

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
hie(endpoints_input,
    power = 0.85,
    alpha = 0.05,
    rratio = 0.5,
    output = "ALL")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("hie", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
