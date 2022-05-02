vignette("SEMinR")
install.packages("seminr")
library(seminr)

library(lavaan)
library(semPlot)
library(MVN)
library(semTools)
library(GPArotation)
library(psych)

surveydata <- Final.Cleaned.Data
nomissing=na.omit(surveydata)

subsetn=data.frame(subset(surveydata, select = c(PE1, PE2, PE3,
                                                 EE1, EE2, EE3, EE4,
                                                 SI1, SI2, SI3, SI4,
                                                 FC1, FC2, FC3,
                                                 BI1, BI2, BI3, BI4,
                                                 PR1, PR2, PR3,
                                                 FL1, FL2)))

mnormal <- mvn(subsetn)
mnormal

# Estimate the model
simple_mm <- constructs(
  composite("performanceexpectancy", multi_items("PE", 1:3)),
  composite("effortexpectancy", multi_items("EE", 1:4)),
  composite("socialinfluence", multi_items("SI" , 1:3)),
  composite("facilitatingconditions", multi_items("FC", 1:3)),
  composite("behaviouralintention", multi_items("BI", 1:4)),
  composite("perceivedrisk", multi_items("PR", 1:3)),
  composite("financialliteracy", multi_items("FL", 1:2)),
  composite("usedbitcoin", single_item("USEDBTC")),
  interaction_term(iv = "effortexpectancy", moderator = "usedbitcoin", method = two_stage),
  interaction_term(iv = "facilitatingconditions", moderator = "usedbitcoin", method = two_stage),
  interaction_term(iv = "socialinfluence", moderator = "usedbitcoin", method = two_stage),
  interaction_term(iv = "perceivedrisk", moderator = "usedbitcoin", method = two_stage),
  interaction_term(iv = "financialliteracy", moderator = "usedbitcoin", method = two_stage)
)

# Structural model

simple_sm <- relationships(
  paths(from = c("performanceexpectancy", "effortexpectancy", "socialinfluence", "facilitatingconditions", "perceivedrisk", "financialliteracy"), to = c("behaviouralintention")),
  paths(from = c("usedbitcoin", "effortexpectancy*usedbitcoin", "facilitatingconditions*usedbitcoin", "socialinfluence*usedbitcoin", "perceivedrisk*usedbitcoin", "financialliteracy*usedbitcoin"), to = c("behaviouralintention"))
  
)

# Estimate Model
Bitcoin.model <- estimate_pls(data = nomissing,
                              measurement_model = simple_mm,
                              structural_model = simple_sm)

summary_Bitcoin.model <- summary(Bitcoin.model)

summary_Bitcoin.model$paths

summary_Bitcoin.model$reliability

boot_Bitcoin.model_rep <- bootstrap_model(seminr_model = Bitcoin.model,
                                          nboot = 1000,
                                          cores = NULL,
                                          seed = 123)

sum_boot_Bitcoin.model_rep <- summary(boot_Bitcoin.model_rep)

sum_boot_Bitcoin.model_rep$bootstrapped_paths

sum_boot_Bitcoin.model_rep$bootstrapped_loadings

summary_Bitcoin.model$validity$fl_criteria

summary_Bitcoin.model$validity$htmt

plot(Bitcoin.model)

# gather paths and t-values
paths <- sum_boot_Bitcoin.model_rep$bootstrapped_paths[, "Original Est."]
tvalues <- sum_boot_Bitcoin.model_rep$bootstrapped_paths[, "T Stat."]

# degrees of freedom will be the number of rows in the data sample
df = nrow(mobi)

# calculate pvalues from tvalues and df; round to 3 decimal places
pvalues <- round( pt(tvalues, df, lower.tail = FALSE), 3)

# make a table of paths, tvalues, pvalues
data.frame(paths, tvalues, pvalues)

table(nomissing$AGE)
table(nomissing$Gender)
table(nomissing$EDUC)
table(nomissing$USEDBTC)

# CFA

CFAModel <- 'performanceexpectancy =~ PE1 + PE2 + PE3
Effortexpectancy =~ EE1 + EE2 + EE3 + EE4
Socialinfluence =~ SI1 + SI2 + SI3
Facilitatingconditions =~ FC1 + FC2 + FC3
Behaviouralintention =~ BI1 + BI2 + BI3 + BI4
Perceivedrisk =~ PR1 + PR2 + PR3
Financialliteracy =~  FL1 + FL2'

CFA.fit <- cfa(model = CFAModel, data = nomissing)
summary(CFA.fit, standardized = TRUE, fit.measures = TRUE)
