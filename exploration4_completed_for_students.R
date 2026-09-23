###############################################################################
## Exploration 4: Description of Relationships III: Categorical Variables
## and Tables -- WORKED EXAMPLE, EXTENDED VERSION FOR DISCUSSION SECTION

###############################################################################
 
## ---- Setup  -----------
library(here)        
source(here("qmd_setup.R"))   
library(tidyverse)      

## ---- Load the data 
dta_path <- here("Data", "norris_democracy_v4.1.dta")
if (!file.exists(dta_path)) {              # download once, then reuse the local copy
  download.file(
    "https://www.dropbox.com/s/huhqag5rudwtbno/Democracy%20Cross-National%20Data%20V4.1%2009092015.dta?dl=1",
    destfile = dta_path, mode = "wb"
  )
}
library(readstata13)    
dat <- read.dta13(dta_path, convert.factors = FALSE)
## convert.factors = FALSE: keep the original numeric codes (e.g. Britcol as
## 0/1) instead of Stata's value labels, so *we* control how each variable
## gets turned into a factor below, rather than inheriting Stata's choices.

stopifnot(length(unique(dat$Nation)) == nrow(dat))
stopifnot(all(table(dat$Nation) == 1))


###############################################################################
## Questions 1-2: Your outcome and your explanatory variable
###############################################################################

## Build the two variables. transmute() (unlike mutate()) keeps ONLY the
## columns you explicitly name below -- see the NOTE TO SELF further down,
## where that bites in Q4/Q8.
wrkdat <- dat %>%
  transmute(
    Nation = Nation,               
    y = DD_democracy,                 
    z = factor(Britcol, levels = c(0, 1),
               labels = c("Not former British colony", "Former British colony"))
    ## factor(..., levels=, labels=) turns the raw 0/1 numbers into a labeled
    ## categorical variable, so R (and any table/plot) shows readable text
    ## instead of bare 0s and 1s, and so lm() treats it as a category rather
    ## than a number to average.
  )

## ---- Q1: describe y alone --------------------------------------------------
table(wrkdat$y)                      
prop.table(table(wrkdat$y))       
sum(is.na(wrkdat$y))                
table(wrkdat$y, useNA = "ifany")     # the same table, but with NA shown as its own column
nrow(wrkdat) - sum(table(wrkdat$y))  # equivalent: total rows minus the non-missing total

## ## Answer (Q1):
## My outcome, y_i, is `DD_democracy`, a 0/1 classification of whether a
## country was coded a democracy (Cheibub-Gandhi-Vreeland-style dichotomous
## regime measure), already binary in the raw file so I did not need to
## dichotomize anything myself. It is missing for 4 of the 195 countries
## (confirmed with `sum(is.na(wrkdat$y))`, since a plain `table()` would have
## silently dropped those 4 rows without telling me they existed).
## Among the 191 countries with non-missing data, 117 (61.3%) are classified
## as democracies and 74 (38.7%) are not. Substantively: as of this measure's
## construction, a majority of the world's states meet the coding rule for
## "democracy" -- but a large minority, nearly 2 in 5 countries, do not, which
## is easy to lose track of if you only remember the majority figure.

## ---- Q2: describe z alone ---------------------------------------------------
table(wrkdat$z)
prop.table(table(wrkdat$z))
sum(is.na(wrkdat$z))                
## ## Answer (Q2):
## My explanatory variable, z_i, is `Britcol`: whether the country was a
## former British colony (1) or not (0). It is missing for 3 countries
## (again via `sum(is.na())`, not the plain table, which drops them silently).
## Among the 192 with non-missing data, 129 (67.2%) were not former British
## colonies and 63 (32.8%) were.
## Theory: a long line of comparative-politics scholarship (e.g., Weiner 1987;
## La Porta et al. 1999) argues that British colonial administrations left
## behind common-law legal systems and earlier practice with competitive local
## elections, institutional habits that should make democratic consolidation
## more likely after independence. So the theory predicts z=1 should go with
## y=1. (I don't have to personally believe this theory to use it here --
## the instructions just ask that *someone* would find it plausible.)


###############################################################################
## Question 3: Describing the relationship with tables and graphs
###############################################################################

## For a RELATIONSHIP (unlike Q1/Q2, which each described one variable alone)
## both variables have to be present at once, so this filter drops any
## country missing either y or z: 195 - 190 = 5 countries dropped here.
wrkdat3 <- wrkdat %>% filter(!is.na(y), !is.na(z))
nrow(wrkdat3)                      

## Method 1: raw counts and margins
tab_yz <- table(z = wrkdat3$z, y = wrkdat3$y)   # 2x2 cross-tab: rows = z, columns = y
addmargins(tab_yz)                              # same table with row/column/grand totals added

## Method 2: row-conditional proportions -- share democratic, by colonial history
## margin=1 means "divide each ROW by its own row total" -- i.e. within each
## z group, what fraction are democracies. (margin=2 would instead divide
## each column by its column total -- a different, wrong-for-us question.)
prop.table(tab_yz, margin = 1)

## Method 3: a difference in proportions and an odds ratio (two more numerical summaries)
p_by_z <- tapply(wrkdat3$y, wrkdat3$z, mean)    # mean of a 0/1 variable = proportion coded 1
p_by_z
diff(p_by_z)                                    # former colony minus not
odds <- p_by_z / (1 - p_by_z)                   # odds = p / (1-p) for each group
odds[2] / odds[1]                               # odds ratio: former-colony odds relative to not

## Graph: bar chart of the row-conditional proportions
fig_q3 <- wrkdat3 %>%
  group_by(z) %>%                               # one group per colonial-history category
  summarize(p_dem = mean(y), n = n()) %>%        # p_dem = share democratic; n = group size
  ggplot(aes(x = z, y = p_dem, fill = z)) +
  geom_col(width = 0.6, show.legend = FALSE) +   # one bar per z category
  geom_text(aes(label = paste0(scales::percent(p_dem, accuracy = 1), " of ", n)),
            vjust = -0.4, size = 4) +            # label each bar with its % and group size
  labs(title = "Share classified as democracy, by colonial history",
       x = NULL, y = "Proportion classified as democracy") +
  ylim(0, 1) +                                   # proportions always live in [0,1]
  theme_minimal(base_size = 13)
ggsave(here("q3_colony_democracy.png"), fig_q3, width = 7, height = 4.5, dpi = 150)

## ## Answer (Q3):
## I used three tabular/numerical methods plus one graph, and deliberately
## did not just print the same table three times:
##   (1) a raw counts table with margins (addmargins) -- lets a reader see
##       the group sizes are unequal (128 vs. 62), which is exactly why I
##       don't stop here;
##   (2) row-conditional proportions (prop.table(margin=1)) -- this is the
##       one that actually answers my theory's question, "within each
##       colonial-history group, what share are democracies?"; I did NOT use
##       column-conditional proportions as my primary summary because that
##       answers a different question (the colonial-history *composition* of
##       democracies vs. non-democracies), not the one my theory makes a
##       claim about;
##   (3) a difference in proportions and an odds ratio, as a numerical
##       summary that doesn't require a plot to communicate the size of the
##       gap, and that (for the odds ratio) won't compress toward zero the
##       way a raw difference can when proportions are close to 0 or 1.

## Row-conditional proportions: 64.8% of non-former-British-colonies are
## democracies, vs. 54.8% of former British colonies -- a 10.0-percentage-
## point gap in the OPPOSITE direction from my theory. The odds ratio (0.658)
## tells the same story: the odds of being a democracy are about two-thirds
## as large for former British colonies as for other countries.
## Interpretation: the "colonial-legacy" story proposed in Q2 does not show
## up as a positive relationship here -- if anything the raw numbers point
## the other way. That could mean the theory is wrong, that colonial
## history's effect runs through a variable I haven't accounted for yet
## (like region or income), or that some other historical process confounds
## the comparison -- I can't tell which from a table alone, which is exactly
## why the exploration says not to reach for "significance" language yet.
## I also want to flag the population-vs-sample point explicitly: this is
## all 195 of the world's countries, not a sample of them, and 5 countries
## are missing either y or z. 


###############################################################################
## Question 4: A categorical explanatory variable
###############################################################################

## NOTE TO SELF (a bug I actually hit while writing this, worth not repeating):
## `transmute()` keeps ONLY the columns you name and silently drops every
## other column from `dat`, including ones you haven't used yet but will
## need later. The first time I wrote this block I left `GDPPC2007` out
## because Q4-Q7 don't need it -- then Question 8's `log(GDPPC2007)` failed
## with "object 'GDPPC2007' not found", several dozen lines later, for no
## reason visible at the point of the error. If you build your own working
## dataset once near the top of your script (sensible -- you shouldn't
## repeat this five times), name EVERY variable you'll use anywhere in the
## exploration in that one `transmute()`/`select()` call, including your
## Q8 continuous variable -- or just use `mutate()` instead of `transmute()`,
## which keeps every original column and adds your new ones on top, so
## nothing you haven't touched yet can vanish on you.
wrkdat <- dat %>%
  transmute(
    Nation = Nation,
    y = DD_democracy,
    z = factor(Britcol, levels = c(0, 1),
               labels = c("Not former British colony", "Former British colony")),
    GDPPC2007 = GDPPC2007,   # <- needed all the way down in Q8; easy to forget
    region4 = case_when(
      ## case_when() checks each condition top to bottom and assigns the
      ## first matching label -- this is how the raw 8-category Region8a
      ## code gets collapsed down to the 4 buckets Q4 asks for.
      Region8a == 1          ~ "Africa",
      Region8a %in% c(2, 4)  ~ "Asia & Middle East",
      Region8a %in% c(5, 6)  ~ "Americas",
      Region8a %in% c(3, 7, 8) ~ "Europe",
      TRUE ~ NA_character_          # anything not matched above (including
                                     # Region8a itself being NA) becomes NA,
                                     # not an unlabeled leftover category
    )
  ) %>%
  mutate(
    x = factor(region4, levels = c("Europe", "Americas", "Asia & Middle East", "Africa")),
    ## region4 above is just a character string; factor() is what actually
    ## makes it a categorical variable R will treat correctly in lm().
    x = relevel(x, ref = "Europe")     # Europe as the reference level throughout
    ## relevel() only changes which category is the OMITTED baseline in a
    ## regression (see Q5) -- it doesn't change the data or the table below.
  )

stopifnot(is.factor(wrkdat$x))         # confirm R sees x as a factor, not a number
                                        # (this is the single most common bug
                                        # students hit: a numeric region code
                                        # silently averaged by lm() instead)

table(wrkdat$x)
prop.table(table(wrkdat$x))
sum(is.na(wrkdat$x))                   # missingness check, same pattern as Q1/Q2

## ## Answer (Q4):
## My second explanatory variable, x_i, is world region. The raw file has an
## 8-category `Region8a` (Africa, Asia-Pacific, C&EEurope, Middle East, North
## America, South America, Scandinavia, Western Europe); I collapsed it to 4
## categories -- Europe, Americas, Asia & Middle East, Africa -- to satisfy
## the "2 to 4 categories" limit, and confirmed with `is.factor()` that R
## treats it as a factor rather than a number it might average by mistake.
## It is missing for 2 countries. Among the 193 with non-missing data, the
## four regions are fairly evenly sized: Asia & Middle East is largest (57
## countries, 29.5%), Americas smallest (35, 18.1%), with Europe (51, 26.4%)
## and Africa (50, 25.9%) in between. No single region dominates the world's
## country count, which matters later: none of the region-specific group
## means I compute below rest on a tiny handful of cases.
## (One judgment call worth naming: `Region8a`'s own coding lumps former
## Soviet Central Asian states, like Uzbekistan and Tajikistan, into its
## "C&EEurope" category, which lands them in my "Europe" bucket here. That's
## the original data source's classification choice, not something I did,
## but it's a real limitation of this particular region grouping.)
## Region should plausibly relate to both y (world regions differ enormously
## and systematically in average regime type) and to z (colonial history is
## itself geographically clustered -- almost all former British colonies are
## outside Europe), which is exactly the kind of variable Q4 is asking for.

## EXTRA: not all categorical variables are alike (discussion-section slide).
## `region4` here is NOMINAL -- no order at all. "Asia & Middle East" isn't
## "more" or "less" than "Africa"; you could relabel the four categories in
## any order and nothing about the data would change. That's different from
## an ORDINAL variable (ordered, but the gaps between categories aren't
## guaranteed equal -- e.g. an education-level variable with categories
## "less than HS" / "HS" / "some college" / "college+"), which is different
## again from a Likert-type / designed scale (ordered AND built so the gaps
## are roughly equal on purpose -- a 5-point agreement scale, a 7-point
## ideology scale). That third case is the one exception to "you can't loess
## a category": because the spacing is roughly even by construction, treating
## the numeric codes as genuinely numeric (means, correlations, even a loess
## curve) becomes a defensible judgment call, in a way it never is for
## `region4`. R's factor() does not know which of these three you have unless
## you tell it -- compare a plain factor() to one built with ordered = TRUE:

region4_plain   <- factor(wrkdat$region4)                                    # nominal treatment (default)
region4_ordinal <- factor(wrkdat$region4,
                           levels = c("Africa", "Asia & Middle East", "Americas", "Europe"),
                           ordered = TRUE)                                    # a genuinely ordered factor
is.ordered(region4_plain)     # FALSE -- no ordering assumed
is.ordered(region4_ordinal)   # TRUE  -- R will use polynomial (.L/.Q/.C) contrasts in a model
## (This particular ordering of region4 is arbitrary and just for illustration
## -- region really is nominal, so `region4_plain`, not `region4_ordinal`, is
## the right choice for this exploration. The point is that the CHOICE
## between them is yours to make and defend, not something R infers.)


###############################################################################
## Question 5: The same relationship from least squares
###############################################################################

wrkdat5 <- wrkdat %>% filter(!is.na(y), !is.na(x))   # complete cases for THIS question's two variables

lm_x <- lm(y ~ x, data = wrkdat5)      #leaving Europe (the
                                        # releveled reference) as the
                                        # omitted baseline
summary(lm_x)$coefficients            

## the "look at something like this" check the prompt suggests -- get the
## same information two different, independent ways and confirm they agree:
conditional_means  <- wrkdat5 %>% group_by(x) %>% summarize(mean_y = mean(y, na.rm = TRUE))
conditional_means2 <- with(wrkdat5, tapply(y, x, mean, na.rm = TRUE))  # tapply: same thing, base-R style
conditional_means
conditional_means2

## ## Answer (Q5):
## `lm(y ~ x)` turns my 4-category region factor into three indicator
## variables (Americas, Asia & Middle East, Africa), leaving Europe as the
## excluded/reference category. The coefficients:
##   Intercept = 0.840, xAmericas = 0.074, xAsia&ME = -0.376, xAfrica = -0.493
## and the group means from tapply():
##   Europe = 0.840, Americas = 0.914, Asia&ME = 0.464, Africa = 0.347
## These match exactly, the way the prompt says they will: the intercept
## (0.840) IS Europe's group mean; the xAmericas coefficient (0.074) is
## 0.914 - 0.840; the xAfrica coefficient (-0.493) is 0.347 - 0.840. I did
## not need `lm()` at all to get these numbers -- they're just differences
## between group means I could have computed with tapply() alone.
## Correct interpretation of the Africa coefficient: "the share of African
## countries classified as democracies is 0.493 lower than the share of
## European countries classified as democracies." That is a DIFFERENCE IN
## MEANS. It is not "living in Africa causes democracy to fall by 0.49," and
## it is not "each unit of Africa decreases democracy by 0.49" -- Africa is
## a category, not a quantity, so there is no "unit" to speak of.


###############################################################################
## Question 6: Both explanatory variables at once
###############################################################################

wrkdat6 <- wrkdat %>% filter(!is.na(y), !is.na(z), !is.na(x))   # need all three variables present now

lm_inter <- lm(y ~ z * x, data = wrkdat6)   # fully interacted: z*x = z + x + z:x,
                                             # i.e. lets the z-y relationship
                                             # differ separately in every region
lm_add   <- lm(y ~ z + x, data = wrkdat6)   # additive (main effects only):
                                             # forces ONE fixed z "bump" that
                                             # applies identically in every region

coef(lm_inter)
coef(lm_add)

## the table, three ways -- start with the plain cell means (no model at all)
cellmeans <- tapply(wrkdat6$y, list(wrkdat6$z, wrkdat6$x), mean)  # one mean per z-by-x cell
cellmeans

## fitted() = each model's predicted value for every country; averaging those
## predictions within each z-by-x cell tells us what each model "thinks" the
## cell mean is, so we can compare it directly to the real cell means above
fitted_inter_by_cell <- tapply(fitted(lm_inter), list(wrkdat6$z, wrkdat6$x), mean)
fitted_add_by_cell   <- tapply(fitted(lm_add),   list(wrkdat6$z, wrkdat6$x), mean)

fitted_inter_by_cell                        # should equal cellmeans exactly
max(abs(fitted_inter_by_cell - cellmeans))  # ~ 0 (floating point)

fitted_add_by_cell                          # will NOT equal cellmeans
max(abs(fitted_add_by_cell - cellmeans))    # meaningfully bigger than 0

## a direct look at the three-way table itself: margin=c(1,2) means "divide
## each y=0/y=1 pair by its own z-by-x cell total" -- i.e. proportions WITHIN
## each z-by-x cell, which is exactly what cellmeans (the y=1 slice) shows
prop.table(table(z = wrkdat6$z, x = wrkdat6$x, y = wrkdat6$y), margin = c(1, 2))

## ## Answer (Q6):
## I fit `y ~ z*x` (fully interacted) and `y ~ z+x` (additive), and compared
## both models' FITTED VALUES (not their coefficients) to the 8 cell means
## from `tapply(y, list(z,x), mean)`.
## The interacted model's fitted values match the cell-mean table EXACTLY --
## the largest difference anywhere is about 5.6e-16, which is floating-point
## zero. The additive model's fitted values do NOT match: the largest gap is
## about 0.169. The clearest example is Europe: the true cell means are 0.826
## for non-former-colonies and a striking 1.000 for former colonies (every
## former-British-colony country in Europe in this data is coded a
## democracy). The interacted model reproduces both numbers exactly. The
## additive model cannot -- it reports about 0.841 for the non-colony cell
## and 0.831 for the colony cell, both far from the true 0.826/1.000 split,
## because it is forced to apply the same colonial-history "bump" to every
## region, and that bump (estimated at essentially zero, -0.010, once
## averaged across all four regions) gets dragged down by regions where
## colonial history matters much less or points the other way.
## What I learned: the relationship between colonial history and democracy
## is not one number -- it depends heavily on region. An additive model, or
## worse, ignoring region altogether (as in Q3), erases a real and fairly
## large region-specific pattern rather than describing it. "Controlling for
## region" (additive) and "letting the colonial-history relationship differ
## by region" (interacted) are two different modeling choices that produce
## two different sets of fitted values here, and only the interacted one
## matches what the table itself says cell by cell.


###############################################################################
## Question 7: Influential countries, part 1
###############################################################################

hv       <- hatvalues(lm_x)          # leverage of each country: how much its
                                      # own y-value can move the fitted line
cook_x   <- cooks.distance(lm_x)     # Cook's distance: leverage combined with
                                      # how far off the fit that country's
                                      # residual actually is -- an overall
                                      # "how much does dropping me change things" score
resid_x  <- residuals(lm_x)          # observed y minus fitted y, for every country

## check: hat value == 1 / (cell size), for every country
inv_n <- 1 / table(wrkdat5$x)[as.character(wrkdat5$x)]
## table(wrkdat5$x) gives each region's size; indexing it by
## as.character(wrkdat5$x) expands that back out to one value per COUNTRY
## (its own region's size), so it lines up one-to-one against hv for the subtraction below
max(abs(hv - inv_n))                                    # ~ 0

## unique hat values, one per region
tapply(hv, wrkdat5$x, unique)

## residuals within each cell take (about) two values: 1-p and -p
tapply(round(resid_x, 4), wrkdat5$x, unique)

## which countries are most influential?
wrkdat5 %>%
  mutate(hat_value = hv, cooks_d = cook_x) %>%
  arrange(desc(cooks_d)) %>%          # sort highest-influence country first
  select(Nation, x, hat_value, cooks_d) %>%
  head(8)                             # just look at the top handful

## ## Answer (Q7):
## I used the saturated model from Q5, `lm(y ~ x)`. Every country's hat value
## equals exactly 1 divided by the size of its region: 0.0200 in Europe
## (n=50), 0.0286 in the Americas (n=35, the smallest region, so the highest
## per-country leverage), 0.0179 in Asia & the Middle East (n=56, the
## largest region, so the lowest leverage), and 0.0204 in Africa (n=49).
## `hatvalues()` and `1/table(x)` agree to machine precision.
## Residuals take (essentially) two values per region, as the prompt says
## they should: in the Americas, where the group mean is 0.914, every
## democracy gets residual 1-0.914=0.086 and every non-democracy gets
## -0.914. Cook's distance follows the same pattern.

## What I learned: there is no such thing as an "extreme" country within a
## purely categorical predictor -- every country in the same region has
## identical leverage. What makes a country influential is (a) how small its
## region is, and (b) whether it's the minority outcome within that region.
## Concretely: because 91% of Americas countries are coded democracies, the
## handful that are not -- Cuba, Guyana, and Haiti in this data -- have by
## far the largest Cook's distance of any countries in the dataset (0.034,
## vs. 0.020 for the next-highest group, Europe's non-democracies -- about
## 1.7 times as large). Dropping
## any one of those three would move the Americas' democracy share by
## 1/35, about 2.9 percentage points -- exactly the "dropping one country
## from a small cell moves its proportion a lot" point the prompt makes.

## EXTRA: Jake's own hint for Q7 (`1/table(z, x)`) actually points at the
## FULLY CROSSED z-by-x model from Q6 (8 cells), not the x-only model (4
## cells) used above. Cells only get smaller when you cross two categorical
## variables, so it's worth checking whether that changes who looks
## influential -- a country that was unremarkable in a big x-only region
## could sit in a small z-by-x cell once colonial history splits that region
## further.
lm_zx   <- lm(y ~ z * x, data = wrkdat6)     # the saturated model from Q6
hv_zx   <- hatvalues(lm_zx)
ck_zx   <- cooks.distance(lm_zx)

table(wrkdat6$z, wrkdat6$x)     # the 8 z-by-x cell sizes -- notice how uneven
                                 # these are compared to the 4 x-only totals:
                                 # "Former colony x Europe" has only 4 countries

## order() gives the row numbers that would sort cooks_d from largest to
## smallest; indexing every column by that same order lines them all up
## together without needing a join or a separate "arrange" step.
ord <- order(-ck_zx)
round_cols <- data.frame(Nation = wrkdat6$Nation[ord], z = wrkdat6$z[ord],
                          x = wrkdat6$x[ord], hat_value = hv_zx[ord], cooks_d = ck_zx[ord])
round_cols$hat_value <- round(round_cols$hat_value, 4)
round_cols$cooks_d   <- round(round_cols$cooks_d, 4)
head(round_cols, 10)

## Oman
i_oman <- which(wrkdat6$Nation == "Oman")
c(hat_value = unname(hv_zx[i_oman]), cooks_d = unname(ck_zx[i_oman]),
  rank_by_cooks_D = unname(rank(-ck_zx)[i_oman]))

## ## Answer (Q7 EXTRA):
## Crossing z and x makes some cells much smaller than any x-only region --
## "Former British colony x Europe" has only 4 countries, vs. 51 for Europe
## overall. The most influential country under this fuller model is Guyana
## (hat = 0.0714 = 1/14, Cook's D = 0.048, in the small "Former colony x
## Americas" cell of 14), followed by Cuba and Haiti again. A country I
## specifically checked because it kept coming up in section -- Oman -- is
## NOT unusually influential under either version of the categorical model
## (rank 48 of 190 by Cook's D here); it only becomes influential once you
## swap in a continuous variable (see the Q8 EXTRA section below). That's a
## useful check in itself: "influential" is a property of a country PLUS a
## model, not a property of the country alone -- the same point Q8 makes by
## swapping z for a continuous variable, just within the categorical side.


###############################################################################
## Question 8: Influential countries, part 2
###############################################################################

wrkdat8 <- wrkdat %>%
  mutate(logGDPPC = log(GDPPC2007)) %>%   # log scale: GDP per capita is heavily
                                           # right-skewed (a few very rich
                                           # countries), so raw dollars would
                                           # let those few countries dominate
  filter(!is.na(y), !is.na(logGDPPC), !is.na(x))

lm_cont <- lm(y ~ logGDPPC + x, data = wrkdat8)   # z (colony, binary) is gone;
                                                    # logGDPPC (continuous) takes its place
summary(lm_cont)$coefficients

cook_cont <- cooks.distance(lm_cont)
dfb_cont  <- dfbeta(lm_cont)[, "logGDPPC"]   # dfbeta: how much the logGDPPC
                                              # coefficient itself would shift
                                              # if this one country were dropped;
                                              # [, "logGDPPC"] pulls out just
                                              # that one column of the matrix
dist_from_mean <- abs(wrkdat8$logGDPPC - mean(wrkdat8$logGDPPC))
## how far (in either direction) each country's income sits from the sample
## average -- the continuous stand-in for "how unusual is this country," the
## role region-cell membership played back in Q7

cor(cook_cont, dist_from_mean)              # much weaker than Q7's exact relationship
cor(abs(dfb_cont), dist_from_mean)

wrkdat8 %>%
  mutate(cooks_d = cook_cont, dist = dist_from_mean) %>%
  arrange(desc(cooks_d)) %>%
  select(Nation, x, GDPPC2007, cooks_d, dist) %>%
  head(8)

## a simple diagnostic plot
fig_q8 <- wrkdat8 %>%
  mutate(cooks_d = cook_cont) %>%
  ggplot(aes(x = logGDPPC, y = cooks_d, color = x)) +
  geom_point(alpha = 0.75, size = 2) +      # one point per country
  labs(title = "Cook's distance once colonial history is swapped for log(GDP per capita)",
       x = "log(GDP per capita, 2007)", y = "Cook's distance", color = NULL) +
  theme_minimal(base_size = 13) + theme(legend.position = "top")
ggsave(here("q8_continuous_leverage.png"), fig_q8, width = 7.5, height = 5, dpi = 150)

## ## Answer (Q8):
## I replaced z (colonial history, binary) with log(GDP per capita, 2007), a
## continuous variable, and refit y ~ logGDPPC + x.
## Correlation between Cook's distance and |logGDPPC - mean(logGDPPC)| is
## only 0.163 -- weak. Correlation between |dfbeta for logGDPPC| and that
## same distance is 0.386 -- moderate, but nowhere near the perfect,
## mechanical relationship leverage had with region size in Q7.
## The most influential countries by Cook's distance here are Haiti (0.044),
## Uzbekistan (0.037), Guyana (0.036), Tajikistan (0.032), and Cuba (0.027).
## Cuba, Guyana, and Haiti carry over from Q7 -- they're poor-ish,
## non-democratic countries in a region that otherwise leans heavily
## democratic, so they stay influential under both specifications. But
## Uzbekistan, Tajikistan, Mauritius, and Liberia are newly prominent here
## and were nowhere near the top under the categorical version, because
## Q7's diagnostic had no way to notice "this country's income is unusually
## high or low," only "this country disagrees with its region's typical
## outcome."

## What I learned: with an all-categorical model, influence was a clean step
## function of which (small) group you belonged to. With one continuous
## predictor, influence becomes a genuinely continuous function that blends
## how far you sit from the mean with how surprising your outcome is, and
## a different (though partially overlapping) set of countries ends up
## mattering most for the fitted description.

###############################################################################
## EXTRA, beyond Q8: a few more checks from discussion section
###############################################################################

## (1) Income ALONE, no region in the model -- who looks influential with
## nothing categorical to hide behind? Close in spirit to Jake's own 9/15
## note ("Fit, leverage, and influence across the datasaurus dozen"), just
## with our real y/x instead of simulated data.
lm_income_only <- lm(y ~ logGDPPC, data = wrkdat8)
h_income <- hatvalues(lm_income_only)
e_income <- residuals(lm_income_only)
D_income <- cooks.distance(lm_income_only)

ord <- order(-D_income)
top6 <- data.frame(Nation = wrkdat8$Nation[ord], GDPPC2007 = wrkdat8$GDPPC2007[ord],
                    y = wrkdat8$y[ord], h = round(h_income[ord], 4),
                    residual = round(e_income[ord], 4), cooks_D = round(D_income[ord], 4))
head(top6, 6)

## (2) A genuine data-coding surprise, worth flagging to students directly.
## Singapore was, historically, a British colony (Straits Settlements from
## 1826, a separate Crown colony from 1946, independent in 1965). Check what
## the actual dataset says:
subset(dat, Nation == "Singapore", select = c(Nation, Britcol, DD_democracy, GDPPC2007))

## (3) "Influential" is not one thing. Jake's 9/15 note makes this point with
## a simulated dataset: the point with the largest leverage, the point with
## the largest residual, and the point with the largest Cook's distance are
## usually different points -- and the point that most changes one specific
## COEFFICIENT (dfbeta) can differ again. `one_income()` below mirrors the
## `one()` function from Jake's note: give it a row number, get back what
## that one country looks like on every measure at once.

dfb_income <- dfbeta(lm_income_only)[, "logGDPPC"]

one_income <- function(i) {
  ## unname() strips each vector's own row-name before it goes into c(), the
  ## same fix Jake's note uses -- otherwise the row name rides along and
  ## turns "h" into something like "h.94" in the printed table.
  c(Nation = wrkdat8$Nation[i], GDPPC2007 = wrkdat8$GDPPC2007[i],
    h = unname(round(h_income[i], 4)), residual = unname(round(e_income[i], 4)),
    cooks_D = unname(round(D_income[i], 4)), dfbeta = unname(round(dfb_income[i], 4)))
}
i_lev <- which.max(h_income)          # farthest from mean income
i_res <- which.max(abs(e_income))     # farthest from the fitted line
i_D   <- which.max(D_income)          # moves ALL fitted values the most
i_dfb <- which.max(abs(dfb_income))   # moves the logGDPPC COEFFICIENT the most
rbind(largest_leverage = one_income(i_lev),
      largest_residual  = one_income(i_res),
      largest_cooks_D   = one_income(i_D),
      largest_dfbeta    = one_income(i_dfb))

## (4) Cook's distance rule of thumb, applied to our own data instead of a
## textbook default. Two common rules: D > 1 (Cook's own original, quite
## conservative) and D > 4/n (the more commonly cited "look again" cutoff).
n_income <- nrow(wrkdat8)
c(n_over_1 = sum(D_income > 1), n_over_4_over_n = sum(D_income > 4 / n_income))

flagged <- which(D_income > 4 / n_income)
flagged <- flagged[order(-D_income[flagged])]
data.frame(Nation = wrkdat8$Nation[flagged], GDPPC2007 = wrkdat8$GDPPC2007[flagged],
           y = wrkdat8$y[flagged], cooks_D = round(D_income[flagged], 4))

## (5) If you drop the most influential countries and refit, does everyone
## ELSE's Cook's distance go down? Intuition says yes; it does not have to.
drop_these     <- c("Qatar", "United Arab Emirates", "Brunei Darussalam", "Kuwait")
wrkdat8_drop   <- subset(wrkdat8, !(Nation %in% drop_these))
lm_income_drop <- lm(y ~ logGDPPC, data = wrkdat8_drop)
D_income_drop  <- cooks.distance(lm_income_drop)

c(residual_SE_before = summary(lm_income_only)$sigma,
  residual_SE_after   = summary(lm_income_drop)$sigma)

## match() looks up, for each surviving country, where it sits in the
## original (undropped) vector -- the base-R equivalent of a join here.
before_after <- data.frame(
  Nation         = wrkdat8_drop$Nation,
  cooks_D_before = round(D_income[match(wrkdat8_drop$Nation, wrkdat8$Nation)], 4),
  cooks_D_after  = round(D_income_drop, 4)
)
head(before_after[order(-before_after$cooks_D_after), ], 5)

## ## Answer (EXTRA, beyond Q8):
## (1) Income alone, no region: Qatar is the single most influential country
## (Cook's D = 0.051, coded non-democratic despite the highest income in the
## data) -- but the rest of the top 6 is NOT just oil monarchies. Liberia
## (0.039) and Costa Rica (0.038) rank 2nd and 3rd -- both very poor
## countries that ARE coded democracies, i.e. unusual in the opposite
## direction from Qatar. UAE (0.030), Brunei (0.030), and Singapore (0.026)
## round out the top 6. So the real pattern isn't "rich autocracies are
## influential," it's "any country whose income and democracy status
## combine in a way the line doesn't expect is influential" -- rich-and-
## authoritarian and poor-yet-democratic both count. Region alone could
## never surface any of this: leverage there was capped at 1/(cell size),
## identical for every country in the same region, so no single country
## could ever look this unusual on its own -- that requires a continuous
## predictor.
## (2) Singapore is coded Britcol = 0 in this dataset, despite being a
## former British colony historically. 
## (3) The "most unusual" country is NOT the same country under every
## criterion -- leverage picks out Liberia, not Qatar. That's genuinely
## surprising until you remember leverage only measures distance from the
## MEAN of logGDPPC: Liberia's income is so far below average that, on the
## log scale, it sits slightly FARTHER from the mean than Qatar sits above
## it. Residual, Cook's distance, and dfbeta all point to Qatar instead,
## because Qatar combines that leverage with the single largest prediction
## error. So here it's leverage vs. the other three, rather than four
## totally separate countries -- but the lesson is the same as Jake's 9/15
## note: "influential" is genuinely more than one question, and which
## country "wins" depends on which question you actually ask.
## (4) The D > 1 rule flags zero countries here -- our largest Cook's D
## (0.051) is nowhere close. The D > 4/n rule (4/177 = 0.023) flags NINE:
## Qatar, Liberia, Costa Rica, UAE, Brunei, Singapore, Kuwait, Guinea-
## Bissau, and Burundi. Same data, two "standard" rules, zero flagged vs.
## nine flagged -- the threshold you pick is a real methodological choice,
## not a fact about the data.
## (5) Dropping the 4 oil monarchies (Qatar, UAE, Brunei, Kuwait) and
## refitting does NOT shrink everyone else's Cook's distance -- it grows.
## Residual SE drops from 0.4768 to 0.4649 once the biggest residuals are
## gone, and because Cook's distance divides by that residual variance, the
## same-sized residual for everyone else now counts as proportionally MORE
## unusual. Liberia's Cook's distance goes from 0.039 to 0.051, Costa
## Rica's from 0.038 to 0.050, Singapore's from 0.026 to 0.036 -- all up,
## not down, after the "cleanup." The lesson: dropping influential points is
## not a reliable way to make a diagnostic look calmer -- it can just
## relocate the problem.

##Resources consulted: Jake Bowers's course materials (Three Cheers for Description, 
##and the 9/15 datasaurus-dozen note), Kosuke Imai's Quantitative Social Science (Ch. 2), 
##and Claude (Anthropic).

###############################################################################
## End of script

