library(tidyverse)
library(rstan)
library(here)

here::i_am("R/007_model_both_waves.R")

dat <- readRDS(here::here("working",
                          "combined_data.rds"))


### Get in wave 1 aux data
w1_aux <- read.csv(here::here("data",
                              "previous_exercise_with_wikidata_idents.csv"))
### and now wave 2 aux data
w2_aux <- read.csv(here::here("data", "canonical_representation.csv")) |>
    mutate(DisplayName = paste0(Name, " (", Constituency, ")"))

aux <- full_join(w1_aux, w2_aux,
                 by = join_by(Name, Constituency, Party)) |>
    mutate(Party = dplyr::recode(Party,
                                 "Conservative" = "Con",
                                 "Labour" = "Lab",
                                 "Liberal Democrat" = "LD",
                                 "Plaid Cymru" = "PC",
                                 "Scottish National Party" = "SNP")) |>
    mutate(wikidata = ifelse(!is.na(wikidata),
                             wikidata,
                             wikidata_id)) |>
    dplyr::select(wikidata, Party) |>
    distinct()

mp <- match(levels(dat$wikidata.A), aux$wikidata)

if (any(mp == 0)) {
    stop("Match problem")
}
if (any(is.na(mp))) {
    stop("Match problem")
}

mp_party <- aux$Party[mp]

data_list <- list(N = nrow(dat),
                  Y = dat$dv,
                  K = 5,
                  is_wave_1 = as.numeric(dat$wave == 1),
                  idx_dim = dat$dim.num,
                  N_MPs = length(levels(dat$wikidata.A)),
                  idx_a = dat$MP_A.num,
                  idx_b = dat$MP_B.num,
                  N_parties = n_distinct(mp_party),
                  mp_party = as.numeric(factor(mp_party)),
                  mp_is_lab = as.numeric(mp_party == "Lab"),
                  mp_is_libdem = as.numeric(mp_party == "LD"),
                  mp_is_plaid = as.numeric(mp_party == "PC"),
                  mp_is_snp = as.numeric(mp_party == "SNP"),
                  mp_is_green = as.numeric(mp_party == "Green"),
                  mp_is_ruk = as.numeric(mp_party == "RUK"),
                  prior_only = 0)


### Estimate the model
stan_code <- "
data {
  int<lower=1> N;             // Number of observations
  int<lower=1> K;             // Number of ordinal categories
  array[N] int<lower=1,upper=K> Y;  // response variable
  int<lower=1> N_MPs;
  int<lower=1> N_parties;
  vector[N] is_wave_1;
  array[N] int<lower=1, upper=2> idx_dim;
  array[N] int<lower=1, upper=N_MPs> idx_a; // link responses to MPs
  array[N] int<lower=1, upper=N_MPs> idx_b; // link responses to MPs
  array[N_MPs] int<lower=1, upper=N_parties> mp_party;
  // series of dummy variables (Con = ref. cat.)
  array[N_MPs] int<lower=0, upper=1> mp_is_lab;
  array[N_MPs] int<lower=0, upper=1> mp_is_libdem;
  array[N_MPs] int<lower=0, upper=1> mp_is_snp;
  array[N_MPs] int<lower=0, upper=1> mp_is_plaid;
  array[N_MPs] int<lower=0, upper=1> mp_is_green;
  array[N_MPs] int<lower=0, upper=1> mp_is_ruk;
}

parameters {
  array[N_MPs] vector[2] alpha;  // (residual) latent trait, or intercept
  matrix[2, 6] party_fx;
  cholesky_factor_corr[2] L_Omega;
  vector<lower=0>[2] L_sigma;
  real c0; // initial threshold
  real<lower=0>skip1; 
  real<lower=0>skip2;
  vector[N_MPs] increment_w2_to_w1;
}
transformed parameters {
  matrix[2, N_MPs] theta;
  ordered[K - 1] c;

  c[1] = c0;
  c[2] = c0 + skip1;
  c[3] = c[2] + skip2;
  c[4] = c[3] + skip1;

  for (l in 1:N_MPs) {
     for (d in 1:2) { 
     theta[d, l] = alpha[l][d] +
       party_fx[d, 1] * mp_is_lab[l] +
       party_fx[d, 2] * mp_is_snp[l] +
       party_fx[d, 3] * mp_is_libdem[l] +
       party_fx[d, 4] * mp_is_plaid[l] +
       party_fx[d, 5] * mp_is_green[l] + 
       party_fx[d, 6] * mp_is_ruk[l];
      }
  }
}

model {
  vector[N] mu = rep_vector(0.0, N);   // set up the linear predictor - has to be at the top of the block
  matrix[2, 2] L_Sigma;
  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ normal(0, 2);
  alpha ~ multi_normal_cholesky(rep_vector(0.0, 2), L_Sigma);
  c0 ~ normal(-2.0, 0.5);
  skip1 ~ normal(0, 1.73);
  skip2 ~ normal(0, 1.73);
  to_vector(party_fx) ~ normal(0, 4.0);
  to_vector(increment_w2_to_w1) ~ normal(0, 0.1);

 
  for (n in 1:N) {
    if (idx_dim[n] == 1) {
    mu[n] = theta[idx_dim[n], idx_a[n]] + increment_w2_to_w1[idx_a[n]] * is_wave_1[n] -
            theta[idx_dim[n], idx_b[n]] - increment_w2_to_w1[idx_b[n]] * is_wave_1[n];
    } else {
    mu[n] = theta[idx_dim[n], idx_a[n]] -
            theta[idx_dim[n], idx_b[n]];
    }
  }

  Y ~ ordered_logistic(mu, c);

 // make thetas sum to zero
 // sum(theta[1, ]) ~ normal(0, 0.001 * N_MPs);
 // sum(theta[2, ]) ~ normal(0, 0.001 * N_MPs);
}

generated quantities {
  array[N] int<lower=1, upper=K> Y_ppc;

  // create a counter for the number of correct predictions
  int correct_pred = 0;
  real pcp;
  
  for (n in 1:N) {
    real mu = theta[idx_dim[n], idx_a[n]] - theta[idx_dim[n], idx_b[n]];
    Y_ppc[n] = ordered_logistic_rng(mu, c);
    // increment the counter holding correct preds
    correct_pred += (Y_ppc[n] == Y[n]);
  }

  // calculate percentage correctly predicted
  // multiply by 1.0 to promote from int to real
  pcp = (correct_pred * 1.0) / (N * 1.0);

}
"

writeLines(stan_code, con = here::here("working", "bothwaves.stan"))

fit <- stan(file = here::here("working", "bothwaves.stan"),
            data = data_list,
            seed = 123,
            chains = 4,
            cores = 4,
            control = list(adapt_delta = 0.95,
                           max_treedepth = 14),
            refresh = 250 # print update every 250 iters
            )

saveRDS(fit, file = here::here("working", "bothwaves_fit.rds"))

s <- summary(fit)

### Tidy the outputs
theta <- rstan::extract(fit, "theta")[[1]] |>
    as.data.frame() |>
    rownames_to_column("iter") |>
    pivot_longer(cols = -c(iter)) |>
    mutate(dimension = as.numeric(sub("\\..*", "", name)),
           mp_idx = as.numeric(sub(".*\\.", "", name)))

### Add on wikidata
mp_levels <- levels(dat$wikidata.A)
theta$wikidata <- mp_levels[theta$mp_idx]
theta$party <- mp_party[theta$mp_idx]

theta <- theta |>
    mutate_if(is.character, factor)

saveRDS(theta, file = here::here("working", "bothwaves_thetas.rds"))

increments <- rstan::extract(fit, "increment_w2_to_w1")[[1]] |>
    as.data.frame() |>
    rownames_to_column("iter") |>
    pivot_longer(cols = -c(iter)) |>
    mutate(mp_idx = as.numeric(sub("V", "", name)))

### Add on wikidata
increments$wikidata <- mp_levels[increments$mp_idx]
increments$party <- mp_party[increments$mp_idx]

increments <- increments |>
    mutate_if(is.character, factor)

saveRDS(increments, file = here::here("working", "increments.rds"))

### Cut-offs
csmry <- summary(fit, variables = "c")
cutoffs <- rstan::extract(fit, "c")[[1]] |>
    as.data.frame() |>
    rownames_to_column("iter") |>
    summarize(across(V1:V4, mean)) |>
    unlist()

saveRDS(cutoffs, file = here::here("shiny_pairwise_mps", "mean_cutoffs.rds"))
