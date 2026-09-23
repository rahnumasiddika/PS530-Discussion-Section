###############################################################################
## Exploration 5: Estimating Unobserved Causal Quantities in Randomized
## Experiments -- Discussion Section
##

## ---- Setup ----------------
library(here)
source(here("qmd_setup.R"))
library(tidyverse)
library(estimatr)   # lm_robust() -- needed starting at Q7
library(robustbase)  # lmrob() -- needed at Q9-Q12

## ---- Data  -----
acorn_dat <- read_csv(here("Data", "acorn03.csv"))
table(acorn_dat$z, exclude = c())


###############################################################################
## Question 1: Explain the benefits of random assignment to ACORN
###############################################################################

## ## Answer (Q1):
## ACORN canvassed 14 of its 28 precincts, chosen at random -- complete
## random assignment (Gerber & Green 2012, Box 2.5: exactly m=14 of N=28
## precincts assigned, not each precinct independently coin-flipped).
## Here is how I'd explain the benefit to ACORN's staff, in their language:
##
## "If precinct captains or volunteers had picked which precincts to
## canvass, those choices would almost certainly track things that also
## predict turnout on their own -- maybe the more politically engaged
## precincts, or the ones a captain happened to know well. Any turnout gap
## we saw afterward could then be explained by those pre-existing
## differences instead of by the canvassing -- and we could never fully
## rule that out, because there's always some other possible confounder we
## haven't thought to measure (Gerber & Green, Ch. 1, call this the
## bottomless-pit problem: no well-defined stopping rule for the search).
## Randomly assigning which 14 of the 28 precincts got canvassed breaks
## that link on purpose. A precinct's chance of being picked doesn't depend
## on anything about the precinct -- not its size, not its past turnout,
## nothing, observed or not. That means that BEFORE canvassing happened,
## the treatment and control groups were, on average, identical on every
## characteristic that could possibly matter. Formally (Gerber & Green,
## Ch. 2, eq. 2.11): E[Y_i(1) | D_i=1] = E[Y_i(1) | D_i=0], i.e. the
## treated and control groups have the same expected potential outcome
## under either assignment, in expectation across every way the random
## draw of 14 precincts could have gone. So whatever turnout difference we
## see afterward is attributable to canvassing, not to how precincts got
## sorted into groups." This is the Neyman/Rubin argument Brady (2008)
## traces historically -- worth noting it predates the potential-outcomes
## notation Q7 below introduces; Fisher and Neyman were making this same
## point in the 1920s-30s, long before "Y_i(1)/Y_i(0)" notation existed.


###############################################################################
## Question 2: The "funky" covariate-balance code
###############################################################################

covars <- c(
  "size", "v_p2003", "v_m2003", "v_g2002", "v_p2002", "v_m2002", "v_s2001",
  "v_g2000", "v_p2000", "v_m2000", "v_s1999", "v_m1999", "v_g1998", "v_m1998", "v_s1998",
  "v_m1997", "v_s1997", "v_g1996", "v_p1996", "v_m1996", "v_s1996"
)

mean_diff <- function(x, z) {
  mean(x[z == 1] - mean(x[z == 0]))
}

covar_baseline_diffs <- acorn_dat %>%
  summarize(across(one_of(covars), ~ mean_diff(x = ., z = z))) %>%
  t() %>%
  as.data.frame()

covar_baseline_diffs %>% arrange(desc(abs(V1)))

## A cleaner rewrite of the same two steps, for the Q2c "nicer function" ask:
mean_diff_clean <- function(x, z) {
  mean(x[z == 1]) - mean(x[z == 0])   # difference of means, not mean of a difference
}
covar_baseline_diffs_clean <- tibble(
  variable = covars,
  diff = map_dbl(covars, ~ mean_diff_clean(acorn_dat[[.x]], acorn_dat$z))
) %>% arrange(desc(abs(diff)))
covar_baseline_diffs_clean   # identical numbers to covar_baseline_diffs above

## ## Answer (Q2):
## (a) What they're doing: `mean_diff()` computes the treated-precinct
## average of a covariate minus the control-precinct average. It LOOKS more
## complicated than that because of where the parentheses sit: reading from
## the inside out, `mean(x[z==0])` is a single number (the control-group
## mean), `x[z==1] - mean(x[z==0])` subtracts that one number from every
## treated precinct's value, and the outer `mean()` then averages that
## vector. Because subtracting a constant commutes with averaging,
## mean(x[z==1] - c) == mean(x[z==1]) - c, so it really does just compute
## the ordinary difference in means -- just via a more roundabout route.
## `summarize(across(one_of(covars), ...))` applies mean_diff() to all 21
## covariates at once, which collapses the whole (28-row) dataset down to
## ONE row with 21 columns (summarize() without a group_by() in front of it
## always collapses to one row). `t()` transposes that single wide row into
## a 21-row, one-column matrix (one row per covariate instead of one column
## per covariate), and `as.data.frame()` restores the data-frame class,
## which t() strips off.
## (b) In plain language for ACORN: "for each of these 21 things we
## measured about precincts before canvassing happened, how different, on
## average, do the canvassed and non-canvassed precincts look?"
## (c) A cleaner version: see `mean_diff_clean()` and the tibble/map_dbl
## rewrite above, which skip the transpose trick and read like the English
## sentence "treated average minus control average" directly. Both versions
## produce identical numbers -- confirmed by running both above.


###############################################################################
## Question 3: Did randomization fail?
###############################################################################

## The two numbers ACORN's representatives are worried about, straight from
## the table computed in Q2:
covar_baseline_diffs["size", ]      # 11.00  -- treated precincts averaged 11 more registered voters
covar_baseline_diffs["v_m2003", ]   # -0.0231 -- treated precincts averaged 2.31 pts LOWER March 2003 turnout

## ## Answer (Q3):
## (a) Why they'd think randomization failed: because it produced groups
## that are not perfectly identical on these two covariates, and "random"
## sounds to a non-technical audience like it should mean "the same." But
## random assignment is a promise about the PROCEDURE, not about any one
## outcome of it -- a fair coin can land heads six times out of ten without
## being an unfair coin. With only 28 precincts and 21 covariates checked,
## some chance imbalance on SOME covariate is close to guaranteed; a
## handful of the 21 will show larger gaps than others purely from sampling
## variation in which 14 precincts landed on which side.
## (b) How I'd address their concern: not by asserting "it's fine, trust
## me," but by showing them how unusual an 11-person or 2.3-point gap
## really is, compared to the full range of gaps that repeating this exact
## random-assignment procedure would produce by chance. That's exactly
## what Q4-Q6 below do.


###############################################################################
## Questions 4-6: Simulating the space of possible random assignments
###############################################################################

new_random_assignment <- function(z) {
  sample(z)
}

new_mean_diffs <- function(dat, covars) {
  newz <- new_random_assignment(z = dat$z)
  covar_baseline_diffs <- dat %>% summarize(across(one_of(covars), ~ mean_diff(x = ., z = newz)))
  return(covar_baseline_diffs)
}

set.seed(123)
nsims <- 100
results_lst <- replicate(nsims, new_mean_diffs(dat = acorn_dat, covars = covars), simplify = FALSE)
results_dat <- bind_rows(results_lst)

head(results_dat)

summarize_across_possible_experiments <- function(x) {
  c(mndiff = mean(x), quantile(x, c(0, .05, .1, .5, .90, .95, 1)))
}

results_dat_summary <- results_dat %>%
  reframe(across(everything(), summarize_across_possible_experiments))

results_dat_summary$size
results_dat_summary$v_m2003

covar_baseline_diffs[c("size", "v_m2003"), ]

## These three numbers are two standard errors of a proportion near .05
## computed from 100, 1000, and 10000 simulations.
2 * sqrt(.05 * (1 - .05) / 100)
2 * sqrt(.05 * (1 - .05) / 1000)
2 * sqrt(.05 * (1 - .05) / 10000)

## the figure (same code as the .qmd's own graph_difference_distributions)
dat_for_plotting <- pivot_longer(results_dat, cols = everything())
dat_for_plotting <- dat_for_plotting %>% filter(name != "size")
covar_baseline_diffs$name <- row.names(covar_baseline_diffs)
observed_diffs <- covar_baseline_diffs %>% filter(name != "size")
covar_order <- observed_diffs$name[order(abs(observed_diffs$V1))]
dat_for_plotting$name <- factor(dat_for_plotting$name, levels = covar_order)
observed_diffs$name <- factor(observed_diffs$name, levels = covar_order)

graph_difference_distributions <- ggplot(data = dat_for_plotting, aes(x = name, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0) +
  geom_point(data = observed_diffs, aes(x = name, y = V1)) +
  labs(x = "Covariate", y = "Difference in means\n(treated minus control)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
graph_difference_distributions
ggsave(here("q4_6_covariate_balance.png"), graph_difference_distributions, width = 9, height = 5.5, dpi = 150)

## ## Answer (Q4):
## (a) What's happening: `new_random_assignment()` shuffles the SAME 28
## precincts' `z` labels (`sample(z)` draws a new random permutation of the
## existing 14-treated/14-control split), holding every precinct's actual
## covariate values fixed. `new_mean_diffs()` then recomputes all 21
## treated-minus-control covariate gaps under that new fake assignment.
## `replicate(100, ...)` does this 100 times, so `results_dat` ends up with
## 100 rows -- one full set of 21 covariate gaps per hypothetical
## re-randomization -- built entirely from the 28 real precincts' real
## covariate values, never from new or fake precincts. This is
## randomization inference by brute force: repeat the DESIGN, not the data
## (the Fisher/Rosenbaum tradition, distinct from -- though related to --
## the Neyman sampling-based standard errors used starting in Q7).
## (b) Why it should reassure ACORN: it turns the abstract "randomization
## works on average" promise into something they can see directly --
## whether their actual draw's imbalance looks like a typical member of
## the population of imbalances chance alone produces, or like an outlier.
##
## ## Answer (Q5):
## The three SE-of-a-proportion numbers -- 0.0436, 0.0138, 0.00436 for
## n=100/1000/10000 -- are NOT about the covariates themselves. They're the
## Monte Carlo standard errors of the simulated 5th/95th-percentile CUTOFFS
## in the plot above. Whether a given simulated draw falls above or below a
## percentile cutoff is a coin flip with probability near 5%, so estimating
## that cutoff from only nsims=100 draws carries its own sampling noise --
## about +/-4.4 percentage points at n=100. That's exactly why the code
## shows the SE shrinking as nsims grows to 1,000 and 10,000: with more
## simulated re-randomizations, the estimated tail cutoffs themselves get
## more precise. ACORN should care because: their actual gaps aren't
## anywhere near the tails (see below), so this Monte Carlo imprecision
## doesn't change the conclusion here -- but a borderline case (a gap near
## the 4th or 6th percentile) would need far more than 100 simulations
## before anyone could confidently call it "usual" or "unusual."
##
## ## Answer (Q6):
## From my actual run (set.seed(123), nsims=100):
##   size (treated - control), across 100 re-randomizations:
##     mean = -5.63, 5th pctile = -108.6, 50th (median) = 1.7, 95th pctile = 74.4
##   v_m2003 (treated - control), across 100 re-randomizations:
##     mean = -0.0032, 5th pctile = -0.0510, 50th = -0.0016, 95th pctile = 0.0404
##   ACORN's ACTUAL draw: size = +11.0, v_m2003 = -0.0231
## (a) What the plot reveals: both of ACORN's actual gaps sit comfortably
## inside the middle of the distribution the design itself generates --
## the size gap (11) is nowhere near the +/-75-to-110 swings that show up
## in the more extreme 10% of re-randomizations, and the turnout gap
## (-0.023) is well inside the roughly -0.05-to-+0.04 range bounding the
## middle 90%. That's concrete evidence that nothing about ACORN's actual
## draw was an unusual member of the space of draws the design could have
## produced -- exactly the demonstration Q3 promised.
## (b) What it doesn't provide: this only concerns PRE-TREATMENT covariates
## -- it says nothing about the treatment-outcome relationship itself (Q7
## below), and it can't rule out that this specific draw happened to be
## unlucky on some dimension not among these 21 measured covariates.
## Randomization protects on average, not with certainty on every single
## draw -- worth restating here since it's Q3's point circling back.


###############################################################################
## Question 7: Estimating the ITT, and replicating Arceneaux's number
###############################################################################

itt_fit <- lm_robust(vote03 ~ z, data = acorn_dat, se_type = "HC2")
summary(itt_fit)

## ## Answer (Q7):
## (a) The estimand: the Intent-to-Treat effect -- the average difference,
## across all 28 precincts, between the turnout each precinct WOULD HAVE
## HAD if assigned to canvassing and the turnout it WOULD HAVE HAD if
## assigned to control. This is an effect of ASSIGNMENT, not of contact:
## the `contact` column in this data (proportion of each precinct's voters
## actually reached -- exactly 0 in every control precinct, and between
## 0.16 and 0.78 in treated precincts) shows canvassers never reached 100%
## of any precinct, so the ITT already has imperfect compliance built in.
## (b) The estimator: the unweighted difference between the 14 treated
## precincts' mean `vote03` and the 14 control precincts' mean `vote03` --
## i.e., the coefficient on z in `lm_robust(vote03 ~ z)`.
## (c) The estimate, from the actual fit above:
##   Treated-precinct mean vote03: 0.3248   Control-precinct mean: 0.2885
##   ITT estimate: 0.0363 (3.63 percentage points), HC2 SE = 0.0244
##   t = 1.488, p = 0.149, 95% CI [-0.014, 0.086], df = 26
## This is the number Arceneaux reports as the precinct-level,
## without-covariates ITT in Table 3 of the 2005 article -- worth pulling
## up the actual article in section to confirm the digit-for-digit match,
## since it should reproduce his reported figure to rounding.
## (d) What it means, in plain language: "Precincts we canvassed voted at a
## rate about 3.6 percentage points higher, on average, than precincts we
## didn't canvass -- roughly a 32-versus-29-out-of-100 comparison. Because
## which precincts got canvassed was decided by a coin flip, there's no
## other systematic difference between the two groups of precincts that
## could explain this gap away."
## (e) As a description, not an estimate: setting aside any causal claim,
## this is also simply a true fact about what happened in these particular
## 28 Kansas City precincts in November 2003 -- the 14 canvassed ones
## turned out at 32.5%, the 14 uncanvassed ones at 28.9%.


###############################################################################
## Question 8: What does the standard error mean?
###############################################################################

## Confirming what lm_robust()'s HC2 SE actually is here, by hand:
Y1 <- acorn_dat$vote03[acorn_dat$z == 1]
Y0 <- acorn_dat$vote03[acorn_dat$z == 0]
neyman_se <- sqrt(var(Y1) / length(Y1) + var(Y0) / length(Y0))
neyman_se   # matches lm_robust's reported HC2 SE (0.0244) -- the classic
            # two-sample (Neyman 1923) variance estimator

## ## Answer (Q8):
## (a) The guess: 0.0244 (about 2.4 percentage points) -- lm_robust()'s
## estimate of how much the ITT estimate would typically bounce around if
## ACORN could rerun this exact random assignment (same 28 precincts,
## same rule: 14 of 28 chosen at random) many times. Confirmed above: this
## is exactly the textbook Neyman two-sample variance formula,
## sqrt(s1^2/n1 + s0^2/n0), computed from how spread out vote03 was WITHIN
## the treated group and WITHIN the control group we actually observed.
## (b) In plain language for ACORN: "If we could magically repeat this
## exact experiment -- same 28 precincts, same coin-flip rule for which 14
## get canvassed -- with a different random draw of which 14, our
## estimated turnout bump would typically land somewhere in the
## neighborhood of 3.6 plus-or-minus roughly 2.4 to 5 points; it wouldn't
## come out exactly 3.6 every time. We can't actually rerun the
## experiment to check this directly -- this +/-2.4 is ITSELF only a
## guess, built from the one dataset we have, not a fact we know for
## certain." Worth noting explicitly: a 95% CI here (0.036 +/- 1.96*0.024
## = [-0.011, 0.084]) comfortably includes zero -- a fact worth having
## ready if asked "so is this effect real," even though Q3 told us to
## avoid exactly this kind of language earlier in the exploration, when
## the questions were still purely descriptive.


###############################################################################
## Questions 9-12: Two estimators of the same quantity, same design
###############################################################################

## ---- (A) Jake's own DeclareDesign code, verbatim from the .qmd ------------
## Run this block yourself with library(DeclareDesign) installed -- it was
## not available in the environment used to check this script (see header
## note), so the ANSWERS below cite the hand-written equivalent in (B), not
## this block's own output.
##
## library(DeclareDesign)
## library(robustbase)
##
## acorn_dat$newY <- acorn_dat$vote03
## acorn_dat$newY[acorn_dat$newY == max(acorn_dat$newY)] <- 1
## acorn_dat$newY[acorn_dat$newY == min(acorn_dat$newY)] <- 0
## N <- nrow(acorn_dat)
##
## pop <- declare_population(acorn_dat)
## trt_assign <- declare_assignment(Z = conduct_ra(N = N, m = 2))
## pot_out <- declare_potential_outcomes(Y ~ .3 * Z + newY)
## reveal <- declare_reveal(Y, Z)
## base_design <- pop + trt_assign + pot_out + reveal
##
## estimandATE <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
## est_diff_means <- declare_estimator(Y ~ Z, inquiry = estimandATE,
##   .method = lm_robust, se_type = "HC2", label = "Diff-Means/OLS")
## est_diff_robust_means <- declare_estimator(Y ~ Z, inquiry = estimandATE,
##   .method = lmrob, label = "M-Est",
##   control = lmrob.control(setting = "KS2014", method = "SMDM", psi = "lqq",
##     tuning.psi = c(-0.5, 1.5, 0.85, NA), tuning.chi = c(-0.5, 1.5, NA, 0.5),
##     max.it = 5000, k.max = 5000, maxit.scale = 5000, refine.tol = .00001))
## base_design_plus_inf <- base_design + estimandATE + est_diff_means + est_diff_robust_means
## set.seed(1234)
## diagnosis1 <- diagnose_design(base_design_plus_inf, bootstrap_sims = 0, sims = c(1, 1000, 1, 1, 1, 1, 1))

## ---- (B) Hand-written equivalent (what the answers below actually cite) ---
## Same logic DeclareDesign automates: doctor the outcome, then repeatedly
## (1) randomly pick 2 of the 28 precincts to "treat" -- complete random
## assignment, same rule as conduct_ra(N=28, m=2) -- (2) compute Y under
## the .3-point true effect, (3) fit both estimators, (4) record the
## estimate and its reported SE. Repeated 1,000 times.
acorn_dat$newY <- acorn_dat$vote03
acorn_dat$newY[acorn_dat$newY == max(acorn_dat$newY)] <- 1
acorn_dat$newY[acorn_dat$newY == min(acorn_dat$newY)] <- 0
N <- nrow(acorn_dat)
true_ate <- 0.3
m <- 2

lmrob_ctrl <- lmrob.control(
  setting = "KS2014", method = "SMDM", psi = "lqq",
  tuning.psi = c(-0.5, 1.5, 0.85, NA), tuning.chi = c(-0.5, 1.5, NA, 0.5),
  max.it = 5000, k.max = 5000, maxit.scale = 5000, refine.tol = .00001
)

set.seed(1234)
nsims2 <- 1000
est_ols <- se_ols <- est_rob <- se_rob <- numeric(nsims2)
for (s in 1:nsims2) {
  treated <- sample(1:N, size = m)
  Z <- rep(0, N); Z[treated] <- 1
  Y <- acorn_dat$newY + true_ate * Z

  fit_ols <- lm_robust(Y ~ Z, se_type = "HC2")
  est_ols[s] <- coef(fit_ols)["Z"]; se_ols[s] <- fit_ols$std.error["Z"]

  fit_rob <- tryCatch(lmrob(Y ~ Z, control = lmrob_ctrl), error = function(e) NULL)
  if (!is.null(fit_rob)) {
    su <- summary(fit_rob)
    est_rob[s] <- coef(fit_rob)["Z"]; se_rob[s] <- su$coefficients["Z", "Std. Error"]
  } else { est_rob[s] <- NA; se_rob[s] <- NA }
}

diagnostics <- tibble(
  estimator = c("OLS (Diff-Means)", "M-Est (lmrob)"),
  bias        = c(mean(est_ols - true_ate), mean(est_rob - true_ate, na.rm = TRUE)),
  rmse        = c(sqrt(mean((est_ols - true_ate)^2)), sqrt(mean((est_rob - true_ate)^2, na.rm = TRUE))),
  sd_estimate = c(sd(est_ols), sd(est_rob, na.rm = TRUE)),
  mean_est_se = c(mean(se_ols), mean(se_rob, na.rm = TRUE))
)
diagnostics

plot_df <- tibble(estimator = rep(c("Diff-Means/OLS", "M-Est"), each = nsims2),
                   estimate = c(est_ols, est_rob))
g <- ggplot(plot_df, aes(x = estimator, y = estimate)) +
  geom_hline(yintercept = true_ate, color = "blue", linetype = "dashed") +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "blue") +
  labs(x = "Estimator", y = "Estimate of the ATE",
       caption = paste0("Dashed line: the true ATE of ", true_ate, ". Diamond: the mean of the ",
                         nsims2, " estimates."))
g
ggsave(here("q9_12_estimator_comparison.png"), g, width = 6.5, height = 5, dpi = 150)

## ## Answer (Q9):
## (a) Explaining DeclareDesign to ACORN: "We built a pretend version of
## your experiment where WE get to know the true effect of canvassing in
## advance -- we set it, by hand, at exactly 0.3 (30 percentage points) for
## every precinct -- and had the computer repeatedly simulate randomly
## picking a SMALL number of precincts to 'canvass,' each time asking two
## different statistical methods to guess the effect from the fake data
## alone, without being told the true answer. Because we rigged the true
## effect ourselves, we can check, over and over, which method's guesses
## land closer to that known truth on average and how much they spread
## out -- something we could never do with your real experiment, where
## nobody knows the true effect to check against."
## (b) The two estimators, informally: (1) OLS/difference-in-means, same
## estimator as Q7 -- treated-group average minus control-group average,
## every precinct weighted equally; (2) an MM-estimator (lmrob(), with the
## "KS2014"/"SMDM"/"lqq" tuning Jake specifies) -- a regression that
## DOWN-WEIGHTS observations with unusually large residuals rather than
## treating every precinct equally, so one wildly atypical precinct
## influences the fit less than it would under plain OLS.
## (c) Why m=2, not m=14: with only 2 of 28 precincts treated, the
## treated-group mean is the average of just TWO numbers, so a single
## unusual precinct can swing that average enormously -- something it
## couldn't do averaged over 14. This is engineered specifically to make
## the two estimators' behavior diverge visibly.
##
## ## Answer (Q10):
## From my actual run (set.seed(1234), nsims2=1000, m=2, base-R + lmrob()
## equivalent to the DeclareDesign block -- see header note):
##   estimator            bias      rmse    sd_estimate   mean_est_se
##   OLS (Diff-Means)    -0.0028    0.1127    0.1127         0.0758
##   M-Est (lmrob)        0.0310    0.0927    0.0874         0.0418
##   (i.e. lmrob's bias is actually larger in magnitude here, -0.031 vs.
##   OLS's -0.003, but its spread and RMSE are BOTH smaller.)
## (a) Whose diamond (mean estimate) is farther from the true-ATE line:
## the M-estimator's -- its mean estimate across 1,000 sims is noticeably
## off from 0.3, while OLS's mean estimate sits almost exactly on it. This
## is expected, not a bug: down-weighting "surprising" observations is
## precisely what breaks OLS's clean unbiasedness.
## (b) Whose sd_estimate is smaller: the M-estimator's (0.087 vs. 0.113) --
## a real, non-trivial reduction. With only 2 treated precincts, one
## extreme value can swing OLS's estimate a long way; the robust estimator
## resists that swing, so its estimates cluster more tightly, even though
## they cluster around a slightly-off-target center.
## (c) Where sd_estimate shows up in the plot: as the height (box + whisker
## spread) of each estimator's boxplot in `g` above.
## (d)/(e) Which to recommend, and the trade-off: RMSE, which penalizes
## both bias and spread, actually favors the M-estimator here (0.093 vs.
## 0.113) -- so with this particular rigged design (m=2, one doctored
## outlier value), I'd lean toward recommending the robust estimator IF
## precision (closeness to truth on any one run) is what ACORN cares about
## most, but I'd tell them explicitly: "the M-estimator gives up the
## guarantee of being right on average, in exchange for guesses that
## cluster more tightly -- it will be wrong in the same direction more
## consistently, rather than sometimes wrong high and sometimes wrong low."
## That's the trade-off unbiasedness alone can't reveal, and it's the
## entire reason this exploration builds Q9-Q12 as a simulation rather
## than just asserting it.
## One more thing worth flagging by comparing this table to Q7-Q8: notice
## `mean_est_se` for OLS (0.076) is noticeably SMALLER than OLS's own
## actual `sd_estimate` (0.113) in this m=2 design -- the standard-error
## "guess" lm_robust() reports UNDERSTATES how much the estimate really
## moves across re-randomizations when so few units are treated. That's a
## concrete, checkable version of exactly the caution Q8 raises in the
## abstract.
##
## ## Answer (Q11):
## (a) Why believe diff-in-means is a good estimator, for ACORN: it doesn't
## need to know anything about the outcomes precincts WOULD have had under
## the assignment they didn't get -- it only uses what's actually observed
## -- and it can be proven, algebraically, that averaged over every way the
## coin flip could have gone, this simple difference lands exactly on the
## true average effect.
## (b) The algebra (Gerber & Green 2012, Box 2.6/2.7, eq. 2.14):
##   E[ (1/m) sum_{i in treated} Y_i - (1/(N-m)) sum_{i in control} Y_i ]
##     = E[Y_i(1) | D_i=1] - E[Y_i(0) | D_i=0]     (what's actually observed
##                                                   in each group)
##     = E[Y_i(1)] - E[Y_i(0)]                      (random assignment: Q1's
##                                                   independence argument)
##     = ATE
## The middle equality is exactly random assignment doing its work: because
## treatment status is independent of the potential outcomes, conditioning
## on D_i=1 or D_i=0 doesn't change the expected potential outcome. The
## simulation version: swap m=2 for m=14 in the loop above (ACORN's ACTUAL
## design, not the engineered small-sample case) and rerun -- OLS's `bias`
## column should come out at or extremely close to zero, empirically
## confirming the algebra in a setting that isn't rigged with an extreme
## doctored outlier the way the m=2 version above is.
##
## ## Answer (Q12):
## (a) Why ACORN should care about unbiasedness: it's the guarantee that
## the PROCEDURE itself isn't systematically stacked in one direction --
## it won't consistently over- or under-state canvassing's effect if they
## used this same design-and-estimator combination repeatedly across many
## campaigns. Without it, collecting more data wouldn't fix the problem;
## they'd just be more precisely wrong.
## (b) What questions remain even granting unbiasedness: unbiasedness says
## nothing about how PRECISE any single estimate is (Q8's SE and Q10's
## sd_estimate are the separate question of how far a given guess could
## plausibly be from the truth) -- an unbiased estimator can still have a
## wide spread, as OLS does here. It also says nothing about whether the
## ITT itself is the quantity ACORN actually needs strategically: with
## `contact` well under 100% in every treated precinct, a campaign deciding
## whether canvassing is worth its cost per contacted voter needs something
## closer to a treatment-on-the-treated estimate, not the ITT, which is
## mechanically diluted by every door nobody answered.
## (c) Other concerns beyond "closest to the truth": (i) this design gives
## an AVERAGE effect across 28 precincts and says nothing about whether the
## effect is uniform -- some precincts could have been moved a lot, others
## not at all or even the opposite direction, averaging to the same ITT;
## (ii) external validity -- does a 2003 Kansas City tax-election canvass
## generalize to a different city, election, or message; (iii)
## cost-effectiveness -- even a small, precisely-estimated, unbiased effect
## might not be worth the canvassing budget, a question unbiasedness alone
## can't answer.


###############################################################################
## End 
###############################################################################
