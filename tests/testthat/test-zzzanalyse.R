context("analyse")

test_that("analyse", {

  tmb_template <- "#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {

DATA_VECTOR(Pairs);
DATA_VECTOR(Year);
DATA_FACTOR(Annual);
DATA_INTEGER(nAnnual);

PARAMETER(alpha);
PARAMETER(beta1);
PARAMETER(beta2);
PARAMETER(beta3);
PARAMETER_VECTOR(bAnnual);
PARAMETER(log_sAnnual);

Type sAnnual = exp(log_sAnnual);

vector<Type> ePairs = Pairs;

Type nll = 0.0;

for(int i = 0; i < nAnnual; i++){
  nll -= dnorm(bAnnual(i), Type(0), sAnnual, true);
}

for(int i = 0; i < Pairs.size(); i++){
  ePairs(i) = exp(alpha + beta1 * Year(i) + beta2 * pow(Year(i), 2) + beta3 * pow(Year(i), 3) + bAnnual(Annual(i)));
  nll -= dpois(Pairs(i), ePairs(i), true);
}
ADREPORT(sAnnual);
ADREPORT(ePairs);
return nll;
}"

new_expr <- "
for (i in 1:length(Pairs)) {
  log(prediction[i]) <- alpha + beta1 * Year[i] + beta2 * Year[i]^2 + beta3 * Year[i]^3 + bAnnual[Annual[i]]
}"

gen_inits <- function(data) list(alpha = 4, beta1 = 1, beta2 = 0, beta3 = 0, log_sAnnual = 0, bAnnual = rep(0, data$nAnnual))

data <- bauw::peregrine
data$Annual <- factor(data$Year)


model <- model(tmb_template, gen_inits = gen_inits,
               select_data = list("Pairs" = integer(), "Year*" = integer(), Annual = factor()),
               random_effects = list(bAnnual = "Annual"),
               new_expr = new_expr)

expect_identical(parameters(model$derived), c("ePairs","sAnnual"))

analysis <- analyse(model, data = data, glance = FALSE, beep = FALSE)

expect_identical(terms(analysis, "fixed", include_constant = FALSE), as.term(sort(c("alpha", "beta1", "beta2", "beta3", "log_sAnnual"))))

expect_identical(parameters(analysis, "fixed"), sort(c("alpha", "beta1", "beta2", "beta3", "log_sAnnual")))
expect_identical(parameters(analysis, "random"), "bAnnual")

# not including derived parameters....
expect_identical(parameters(analysis), sort(c("alpha", "bAnnual", "beta1", "beta2", "beta3", "ePairs", "log_sAnnual", "sAnnual")))
expect_identical(parameters(analysis, "primary"), sort(c("alpha", "bAnnual", "beta1", "beta2", "beta3", "log_sAnnual")))
expect_error(parameters(analysis, "some"))

expect_identical(nterms(analysis, "all"), 86L)

expect_is(as.mcmcr(analysis), "mcmcr")
expect_identical(nchains(analysis), 1L)
expect_identical(niters(analysis), 1L)

glance <- glance(analysis)
expect_is(glance, "tbl")
expect_identical(colnames(glance), c("n", "K", "logLik", "IC", "converged"))
expect_equal(glance$logLik, -154.4664, tolerance = 0.0001)
expect_equal(glance$IC, 320.6974, tolerance = 0.001)
expect_identical(glance$n, 40L)
expect_identical(glance$K, 5L)
expect_true(glance$converged)

coef <- coef(analysis)

expect_is(coef, "tbl")
expect_is(coef, "mb_analysis_coef")
expect_identical(colnames(coef), c("term", "estimate", "sd", "zscore", "lower", "upper", "pvalue"))

expect_identical(coef$term, sort(as.term(c("alpha", "beta1", "beta2", "beta3", "log_sAnnual"))))

profile <- coef_profile(analysis, beep = FALSE)

expect_identical(colnames(profile), c("term", "estimate", "sd", "zscore", "lower", "upper", "pvalue"))

expect_identical(profile[c("term", "estimate", "zscore")],
                 coef[c("term", "estimate", "zscore")])

expect_equal(coef$lower[coef$term == "log_sAnnual"], -2.838932, tolerance = 0.0000001)
expect_equal(profile$lower[profile$term == "log_sAnnual"], -3.01573026)

tidy <- tidy(analysis)
expect_identical(colnames(tidy), c("term", "estimate", "lower", "upper"))

year <- predict(analysis, new_data = "Year")

expect_is(year, "tbl")
expect_identical(colnames(year), c("Year", "Pairs", "R.Pairs", "Eyasses", "Annual",
                                   "estimate", "sd", "zscore", "lower", "upper", "pvalue"))
expect_true(all(is.na(year$lower)))

expect_equal(unlist(estimates(analysis)), coef$estimate, check.names = FALSE)
})
