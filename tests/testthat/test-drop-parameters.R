context("analyse")

test_that("analyse", {

  template <- "
#include <TMB.hpp>

                template<class Type>
                Type objective_function<Type>::operator() () {

                DATA_VECTOR(Males);
                DATA_VECTOR(Disturbance);
                DATA_VECTOR(PDO);
                DATA_VECTOR(HunterDays);
                DATA_FACTOR(Year);
                DATA_INTEGER(nYear);

                PARAMETER(bSurvival);
                PARAMETER(bDisturbance);
                PARAMETER(bPDO);
                PARAMETER(bHunterDays);

                PARAMETER(bAdultsInitial);
                PARAMETER(bYearlingsInitial);

                PARAMETER(log_sMales);

                Type sMales = exp(log_sMales);

                ADREPORT(sMales);

                vector<Type> bAdults = Males;
                vector<Type> bYearlings = Males;
                vector<Type> bChicks = Males;
                vector<Type> eSurvival = Males;
                vector<Type> eMales = Males;

                Type nll = 0.0;

                for(int i = 0; i < nYear; i++){
                eSurvival(i) = invlogit(bSurvival + bDisturbance * Disturbance(i) + bPDO * PDO(i) + bHunterDays * HunterDays(i));
                }

                bAdults(0) = bAdultsInitial;
                bYearlings(0) = bYearlingsInitial;
                bChicks(0) = bAdultsInitial * 4;

                for(int i = 1; i < nYear; i++){
                bAdults(i) = (bYearlings(i-1) + bAdults(i-1)) * eSurvival(i-1);
                bYearlings(i) = bChicks(i-1) * eSurvival(i-1);
                bChicks(i) = bAdults(i) * 4;
                }

                for(int i = 0; i < nYear; i++){
                eMales(i) = bAdults(i) / 2;
                nll -= dnorm(log(Males(i)), log(eMales(i)) - pow(sMales, 2) / 2, sMales, true);
                }

                return nll;
                }"

  gen_inits <- function(data) {
    inits <- list()
    inits$bSurvival = 0
    inits$bDisturbance = 0
    inits$bPDO = 0
    inits$bHunterDays = 0
    inits$bAdultsInitial = mean(data$Males)
    inits$bYearlingsInitial = mean(data$Males) * 2
    inits$log_sMales = 0
    inits
  }

  modify_data <- function(data) {
    stopifnot(identical(as.integer(data$Year), seq_along(data$Year)))
    data
  }

  new_expr <-
    " prediction <- plogis(bSurvival + bDisturbance * Disturbance + bPDO * PDO + bHunterDays * HunterDays)
eSurvival <- prediction

Adults <- bAdultsInitial
Yearlings <- bYearlingsInitial
Chicks <- bAdultsInitial * 4

for (i in 2:length(Year)) {
Adults[i] <- (Yearlings[i-1] + Adults[i-1]) * eSurvival[i-1];
Yearlings[i] <- Chicks[i-1] * eSurvival[i-1]
Chicks[i] <- Adults[i] * 4
}
eMales <- exp(log(Adults / 2) - sMales^2 / 2)
fit <- eMales
residual <- (log(Males) - log(eMales)) / sMales
r2 <- 1 - var(Males - eMales) / var(Males)
"

  model <- model(
    template,  gen_inits,
    select_data = list(Males = 1, Disturbance = 1, Year = factor(1), PDO = 1, HunterDays = 1),
    scale = c("Disturbance", "HunterDays"), modify_data = modify_data,
    new_expr = new_expr,  drops = list("bDisturbance", "bPDO", "bHunterDays"))

  models <- make_all_models(model)

  expect_identical(models[[1]], model)

  expect_identical(names(models), c("full", "base+bHunterDays+bPDO", "base+bDisturbance+bPDO", "base+bDisturbance+bHunterDays", "base+bPDO", "base+bHunterDays", "base+bDisturbance", "base"))

})
