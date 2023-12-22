library(ggplot2)
library(scales)
library(dplyr)

LAMBDA = 1
switzerland_time_series$e = log(switzerland_time_series$e)

switzerland_time_series$log_us_m = log(switzerland_time_series$us_money_stock)
switzerland_time_series$log_chf_m = log(switzerland_time_series$chf_money_stock)

switzerland_time_series$log_us_y = log(switzerland_time_series$us_real_income)
switzerland_time_series$log_chf_y = log(switzerland_time_series$chf_real_income)

#Seasonality Reduction:
switzerland_time_series = switzerland_time_series %>%
  mutate(
    log_us_m_lag1 = lag(log_us_m, 1),
    log_us_m_lag2 = lag(log_us_m, 2),
    log_us_m_lag3 = lag(log_us_m, 3),
    log_chf_m_lag1 = lag(log_chf_m, 1),
    log_chf_m_lag2 = lag(log_chf_m, 2),
    log_chf_m_lag3 = lag(log_chf_m, 3),
  ) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

switzerland_time_series = switzerland_time_series %>%
  mutate(log_us_m_lag = log_us_m + log_us_m_lag1 + log_us_m_lag2 + log_us_m_lag3)

switzerland_time_series = switzerland_time_series %>%
  mutate(log_chf_m_lag = log_chf_m + log_chf_m_lag1 + log_chf_m_lag2 + log_chf_m_lag3)

switzerland_time_series = switzerland_time_series %>%
  select(-log_us_m_lag1, -log_us_m_lag2, -log_us_m_lag3)
switzerland_time_series = switzerland_time_series %>%
  select(-log_chf_m_lag1, -log_chf_m_lag2, -log_chf_m_lag3)


m_dif = switzerland_time_series$log_us_m_lag - switzerland_time_series$log_chf_m_lag
y_dif = switzerland_time_series$log_us_y - switzerland_time_series$log_chf_y

f = m_dif - LAMBDA*y_dif
z = f - switzerland_time_series$e
one_period_lag = diff(switzerland_time_series$e, lag = 1)

#drop last values of f and z
f = head(f, -1)
z = head(z, -1)

olsmodel = lm(formula = one_period_lag ~ z)
summary(olsmodel)

biased_beta = coef(olsmodel)[2]
#Nonparametric residual bootstrap
B = 10000
N = length(one_period_lag)
u_hat = resid(olsmodel)
XB_ols = one_period_lag - u_hat

mat_b_rb = matrix(NA, B, 1)
for (b in 1:B) {
  index_b = sample.int(N, size=N, replace =T)
  u_b = u_hat[index_b, drop=F]
  
  one_period_lag_b = XB_ols + u_b
  bootstrap_model = lm(formula = one_period_lag_b ~ z)
  beta_b = coef(bootstrap_model)[2]
  
  mat_b_rb[b, ] = beta_b
}

debiased_beta = 2*biased_beta - mean(mat_b_rb)
se_b_rb = apply(mat_b_rb, 2, sd)

#in_sample_ols
in_f = head(f, 34)
in_z = head(z, 34)
in_one_period_lag = head(one_period_lag, 34)


in_sample_ols = lm(formula = in_one_period_lag ~ in_z)
summary(in_sample_ols)

in_biased_beta = coef(in_sample_ols)[2]
#Nonparametric residual bootstrap
B = 10000
in_N = length(in_one_period_lag)
in_u_hat = resid(in_sample_ols)
in_XB_ols = in_one_period_lag - in_u_hat

in_mat_b_rb = matrix(NA, B, 1)
for (b in 1:B) {
  index_b = sample.int(in_N, size=in_N, replace =T)
  u_b = in_u_hat[index_b, drop=F]
  
  one_period_lag_b = in_XB_ols + u_b
  bootstrap_model = lm(formula = one_period_lag_b ~ in_z)
  beta_b = coef(bootstrap_model)[2]
  
  in_mat_b_rb[b, ] = beta_b
}

in_debiased_beta = 2*in_biased_beta - mean(in_mat_b_rb)
in_se_b_rb = apply(in_mat_b_rb, 2, sd)

#out of sample predictions
alpha = coef(in_sample_ols)[1]
out_z = z[34:74]

e_t_predictions = numeric(41)
e_t_predictions[1] = switzerland_time_series$e[34]
for (i in 2:40) {
  print(i)
  print(out_z[i-1])
  print(e_t_predictions[i-1])
  print(as.numeric(alpha))
  print(as.numeric(in_debiased_beta))
  
  e_t_predictions[i] = as.numeric(alpha) + as.numeric(in_debiased_beta) * out_z[i - 1] + e_t_predictions[i - 1]
}

#plotting predictions against actual
e_t_out_of_sample_actual = switzerland_time_series$e[34:74]
time = switzerland_time_series$time[34:74]

difference = (e_t_predictions - e_t_out_of_sample_actual)/(e_t_out_of_sample_actual)
split_times = c(time[seq(1, length(time), by = 5)])
split_times = head(split_times, -1)
time <- factor(switzerland_time_series$time[34:74], levels = split_times)

ggplot() +
  geom_line(aes(x = time, y = difference, group = 1)) +
  labs(title = "Time Series Plot of Differences - CHF/USD Exchange", x = "Time", y = "Percentage Difference") +
  theme_minimal() +
  theme(text = element_text(size = 14), legend.position = "bottom") +
  coord_cartesian(ylim = c(-2.0, 2.0))
