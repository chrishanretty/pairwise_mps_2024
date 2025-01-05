library(tidyverse)
library(rstan)
library(here)

here::i_am("R/006_model.R")

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
  array[N_MPs] vector[2] alpha;  // (residual) latent trait, or intercept
  matrix[2, 5] party_fx;
  cholesky_factor_corr[2] L_Omega;
  vector<lower=0>[2] L_sigma;


  // ordinal response parameters
  real c0; // initial threshold
  real<lower=0>skip1; 
  real<lower=0>skip2;
}
transformed parameters {
  matrix[2, N_MPs] theta;
  ordered[K - 1] c;

  // ordinal response transforms
  c[1] = c0;
  c[2] = c0 + skip1;
  c[3] = c[2] + skip2;
  c[4] = c[3] + skip1;

  // explanatory factor transforms
  for (l in 1:N_MPs) {
     for (d in 1:2) { 
     theta[d, l] = alpha[l][d] +
       party_fx[d, 1] * mp_is_lab[l] +
       party_fx[d, 2] * mp_is_snp[l] +
       party_fx[d, 3] * mp_is_libdem[l] +
       party_fx[d, 4] * mp_is_plaid[l] +
       party_fx[d, 5] * mp_is_green[l];
      }
  }
}

model {
  // set up the linear predictor
  // has to be at the top of the block
  vector[N] mu = rep_vector(0.0, N);
  // sigma construction
  matrix[2, 2] L_Sigma;
  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);

  // Prior model

  // prior on correlations and latent sizes
  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ cauchy(0, 2.5);

  alpha ~ multi_normal_cholesky(rep_vector(0.0, 2), L_Sigma);
  c0 ~ normal(-1, 1);
  skip1 ~ std_normal();
  skip2 ~ std_normal();
  to_vector(party_fx) ~ normal(0, 2.5);

  // Observational model
  for (n in 1:N) {
    mu[n] = theta[idx_dim[n], idx_a[n]] - theta[idx_dim[n], idx_b[n]];
  }

  // outcome
  Y ~ ordered_logistic(mu, c);
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

writeLines(stan_code, con = here::here("working", "partyonly.stan"))

fit <- stan(file = here::here("working", "partyonly.stan"),
            data = data_list,
            seed = 123,
            chains = 4,
            cores = 4,
            control = list(adapt_delta = 0.95,
                           max_treedepth = 14),
            refresh = 250 # print update every 250 iters
            )

saveRDS(fit, file = here::here("working", "partyonly_fit.rds"))

theta <- rstan::extract(fit, "theta")[[1]] |>
    as.data.frame() |>
    rownames_to_column("iter") |>
    pivot_longer(cols = -c(iter)) |>
    mutate(dimension = as.numeric(sub("\\..*", "", name)),
           mp_idx = as.numeric(sub(".*\\.", "", name)))

### Add on DisplayName and Party
theta <- cbind(theta,
               mps[theta$mp_idx, c("DisplayName", "First.party")])

### Remove the iter, name and index variables
theta <- theta |>
    dplyr::select(value, DisplayName, dimension, First.party)

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

ggplot(theta |> filter(dimension == 1),
       aes(x = First.party, y = value)) +
    geom_boxplot()

ggplot(theta |> filter(dimension == 2),
       aes(x = First.party, y = value)) +
    geom_boxplot()

saveRDS(theta, file = here::here("working", "partyonly_theta_iters.rds"))

### What's the correlation between the two?
plot_df <- theta |>
    pivot_wider(id_cols = c(Name, Constituency, First.party),
                names_from = dimension,
                values_from = value,
                values_fn = mean) |>
    group_by(Name, Constituency, First.party) |>
    summarize(dim1 = mean(`1`),
              dim2 = mean(`2`))

cor(plot_df$dim1, plot_df$dim2)

### What about within party?
plot_df |>
    group_by(First.party) |>
    summarize(the_cor = cor(dim1, dim2))
### What's the PCP like?
pcp <- rstan::extract(fit, "pcp")
mean(pcp[[1]])
l_omega <- rstan::extract(fit, "L_Omega")[[1]]
