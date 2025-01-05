library(tidyverse)
library(rstan)
library(here)

here::i_am("R/007_model_uncorrel.R")

### Read in the data
p1 <- readRDS(here::here("working", "p1_tidy_data.rds"))
p2 <- readRDS(here::here("working", "p2_tidy_data.rds"))
p1$dim <- "econ"
p2$dim <- "cult"

dat <- bind_rows(p1, p2)

### Map the MPs to person ID/ wikidata IDs
### 
hoc_url <- "https://researchbriefings.files.parliament.uk/documents/CBP-10009/HoC-GE2024-results-by-constituency.csv"
results_locn <- "data/HoC-GE2024-results-by-constituency.csv"
if (!file.exists(results_locn)) {
    download.file(hoc_url, results_locn)
}
mps <- read.csv(results_locn)

mps$DisplayName <- paste(mps$Member.first.name,
                         " ",
                         toupper(mps$Member.surname),
                         " (",
                         mps$Constituency.name,
                         ")",
                         sep = "")

### Create factor levels from this variable
mp_levels <- sort(unique(as.character(mps$DisplayName)))

mps <- mps |>
    mutate(DisplayName = factor(DisplayName,
                                levels = mp_levels,
                                ordered = TRUE),
           DisplayName.num = as.numeric(DisplayName)) |>
    arrange(DisplayName.num)

dat <- dat |>
    mutate(MP_A = factor(MP_A,
                         levels = mp_levels,
                         ordered = TRUE),
           MP_B = factor(MP_B,
                         levels = mp_levels,
                         ordered = TRUE),
           MP_A.num = as.numeric(MP_A),
           MP_B.num = as.numeric(MP_B),
           dim.num = ifelse(dim == "econ", 1, 2))


### Create a data frame of MPs ordered in this way
### With some auxiliary variables
data_list <- list(N = nrow(dat),
                  Y = dat$dv,
                  K = 5,
                  last_econ = max(which(dat$dim == "econ")),
                  first_cult = min(which(dat$dim == "cult")),
                  idx_dim = dat$dim.num,
                  N_MPs = nrow(mps),
                  N_resps = length(unique(dat$sessionid)),
                  idx_resp = as.numeric(factor(dat$sessionid)),
                  idx_a = dat$MP_A.num,
                  idx_b = dat$MP_B.num,
                  N_parties = n_distinct(mps$First.party),
                  N_regions = n_distinct(mps$Region.name),
                  mp_party = as.numeric(factor(mps$First.party)),
                  mp_is_lab = as.numeric(mps$First.party == "Lab"),
                  mp_is_libdem = as.numeric(mps$First.party == "LD"),
                  mp_is_plaid = as.numeric(mps$First.party == "PC"),
                  mp_is_snp = as.numeric(mps$First.party == "SNP"),
                  mp_is_green = as.numeric(mps$First.party == "Green"),
                  mp_region = as.numeric(factor(mps$Region.name)),
                  con19 = as.vector(scale(mps$Con / mps$Valid.votes, scale = FALSE)),
                  lab19 = as.vector(scale(mps$Lab / mps$Valid.votes, scale = FALSE)),
                  prior_only = 0)



stan_code <- "
data {
  int<lower=1> N;             // Number of observations
  int<lower=1> K;             // Number of ordinal categories
  array[N] int<lower=1,upper=K> Y;  // response variable
  int<lower=1> N_MPs;
  int<lower=1> N_parties;
  int<lower=1> last_econ;
  int<lower=last_econ, upper=N> first_cult;
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
}

parameters {
  // explanatory factors
  vector[N_MPs] alpha_econ;  // (residual) latent trait, or intercept
  vector[N_MPs] alpha_cult;  // (residual) latent trait, or intercept
  real<lower=0> sigma_econ;
  real<lower=0> sigma_cult;
  vector[5] party_fx_econ;
  vector[5] party_fx_cult;

  // ordinal response parameters
  real c0; // initial threshold
  real<lower=0>skip1; 
  real<lower=0>skip2;
}
transformed parameters {
  vector[N_MPs] theta_econ;
  vector[N_MPs] theta_cult;
  ordered[K - 1] c;

  // ordinal response transforms
  c[1] = c0;
  c[2] = c0 + skip1;
  c[3] = c[2] + skip2;
  c[4] = c[3] + skip1;

  // explanatory factor transforms
  for (l in 1:N_MPs) {
     theta_econ[l] = alpha_econ[l] +
       party_fx_econ[1] * mp_is_lab[l] +
       party_fx_econ[2] * mp_is_snp[l] +
       party_fx_econ[3] * mp_is_libdem[l] +
       party_fx_econ[4] * mp_is_plaid[l] +
       party_fx_econ[5] * mp_is_green[l];
     theta_cult[l] = alpha_cult[l] +
       party_fx_econ[1] * mp_is_lab[l] +
       party_fx_econ[2] * mp_is_snp[l] +
       party_fx_econ[3] * mp_is_libdem[l] +
       party_fx_econ[4] * mp_is_plaid[l] +
       party_fx_econ[5] * mp_is_green[l];
  }
}

model {
  // set up the linear predictor
  // has to be at the top of the block
  vector[N] mu = rep_vector(0.0, N);

  c0 ~ normal(-1, 1);
  skip1 ~ std_normal();
  skip2 ~ std_normal();
  to_vector(party_fx_econ) ~ normal(0, 2.5);
  to_vector(party_fx_cult) ~ normal(0, 2.5);
  alpha_econ ~ normal(0, sigma_econ);
  alpha_cult ~ normal(0, sigma_cult);
  sigma_econ ~ std_normal(); // half-normal b/c of constraints
  sigma_cult ~ std_normal(); // half-normal b/c of constraints

  // Observational model
  for (n in 1:last_econ) {
    mu[n] = theta_econ[idx_a[n]] - theta_econ[idx_b[n]];
  }
  for (n in first_cult:N) {
    mu[n] = theta_cult[idx_a[n]] - theta_cult[idx_b[n]];
  }
  // outcome
  Y ~ ordered_logistic(mu, c);
}

generated quantities {
  array[N] int<lower=1, upper=K> Y_ppc;
  // create a counter for the number of correct predictions
  int correct_pred = 0;
  real pcp;
  
  for (n in 1:last_econ) {
    real mu = theta_econ[idx_a[n]] - theta_econ[idx_b[n]];
    Y_ppc[n] = ordered_logistic_rng(mu, c);
    // increment the counter holding correct preds
    correct_pred += (Y_ppc[n] == Y[n]);
  }
  for (n in first_cult:N) {
    real mu = theta_cult[idx_a[n]] - theta_cult[idx_b[n]];
    Y_ppc[n] = ordered_logistic_rng(mu, c);
    // increment the counter holding correct preds
    correct_pred += (Y_ppc[n] == Y[n]);
  }
  // calculate percentage correctly predicted
  // multiply by 1.0 to promote from int to real
  pcp = (correct_pred * 1.0) / (N * 1.0);
}
"

writeLines(stan_code, con = here::here("working", "partyonly_uncorrel.stan"))

fit <- stan(file = here::here("working", "partyonly_uncorrel.stan"),
            data = data_list,
            seed = 123,
            chains = 4,
            cores = 4,
            control = list(adapt_delta = 0.95,
                           max_treedepth = 14),
            refresh = 250 # print update every 250 iters
            )

saveRDS(fit, file = here::here("working", "partyonly_fit_uncorrel.rds"))

theta_econ <- rstan::extract(fit, "theta_econ")[[1]] |>
    as.data.frame() |>
    rownames_to_column("iter") |>
    pivot_longer(cols = -c(iter)) |>
    mutate(mp_idx = as.numeric(sub("V", "", name)))


theta_cult <- rstan::extract(fit, "theta_cult")[[1]] |>
    as.data.frame() |>
    rownames_to_column("iter") |>
    pivot_longer(cols = -c(iter)) |>
    mutate(mp_idx = as.numeric(sub("V", "", name)))

theta <- left_join(theta_econ,
                   theta_cult,
                   by = join_by(iter, name, mp_idx),
                   suffix = c(".econ", ".cult"))

### Add on DisplayName and Party
theta <- cbind(theta,
               mps[theta$mp_idx, c("DisplayName", "First.party")])

### Remove the iter, name and index variables
theta <- theta |>
    dplyr::select(value.econ, value.cult, DisplayName, First.party)

### Separate DisplayName into Name and Constituency
theta <- theta |>
    separate_wider_delim(DisplayName,
                         names = c("Name", "Constituency"),
                         delim = " (",
                         too_many = "merge") |>
    mutate(Constituency = sub("\\)$", "", Constituency))

### Convert character variables to factor
theta <- theta |>
    mutate_if(is.character, factor)

gb_parties <- c("Con", "Green", "Ind", "Lab", "LD", "PC", "RUK", "SNP")

ggplot(theta |>
       filter(First.party %in% gb_parties),
       aes(x = First.party, y = value.econ)) +
    geom_boxplot()


ggplot(theta |>
       filter(First.party %in% gb_parties),
       aes(x = First.party, y = value.cult)) +
    geom_boxplot()


saveRDS(theta, file = here::here("working", "partyonly_theta_iters_uncorrel.rds"))

### What's the correlation between the two?
cor(theta$value.cult, theta$value.econ)

### Get averages
plot_df <- theta |>
    group_by(Name, Constituency, First.party) |>
    summarize(econ_mean = mean(value.econ),
              econ_lo = quantile(value.econ, 0.1),
              econ_hi = quantile(value.econ, 0.9),
              cult_mean = mean(value.cult),
              cult_lo = quantile(value.cult, 0.1),
              cult_hi = quantile(value.cult, 0.9))

ggplot(plot_df |>
       filter(First.party %in% gb_parties),
       aes(x = econ_mean,
           y = cult_mean,
           colour = First.party)) +
    geom_linerange(aes(xmin = econ_lo, xmax = econ_hi), alpha = 0.5) +
    geom_linerange(aes(ymin = cult_lo, ymax = cult_hi), alpha = 0.5) +
    # facet_wrap(~First.party) +
    theme_bw()
           
### What's the PCP like?
pcp <- rstan::extract(fit, "pcp")
