
#-----------------------------------------------------------------------------
# Chris Oosthuizen
# September 2024
# Penguin wiggles
#-----------------------------------------------------------------------------

# Modelling of wiggle metrics 

#-----------------------------------
# Setup
#-----------------------------------
# Set system time zone
Sys.setenv(TZ = "GMT")

# Load libraries
library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)


source('./scripts/0.1 ggplot theme.R') 

#----------------------------------
# Read the wiggle analysis output
#----------------------------------
result_df = readRDS('./output/result_df_quant.rds')

# Get unique IDs
unique_ids <- unique(result_df$id)
length(unique_ids)

result_df = result_df %>%
  arrange(id, dive.nr) %>%
  mutate(Name = cumsum(id != lag(id, default = first(id))))

result_df$Name = paste0('id_' , result_df$Name+1)

result_df$wiggleN_Simeone = result_df$wiggle_SW_bottom

#-------------------------------------------------------
# We want to predict (future) PCE from (future) wiggles:
#-------------------------------------------------------
#------------
# brm models
#------------
# use the future package for parallelization
cores = parallel::detectCores()
# - we want to have a zero-inflated model, or a hurdle model to account for all the 0's
# - the response is counts - so Poisson models are appropriate
# - but the log link is not - we want to use an identity link, because we expect a linear relationship (not exponential)
# - negative binomial should be better than Poisson, to account for overdispersion. 

#--------------------------------------------
# fit a NB hurdle model 
#--------------------------------------------
brms::get_prior(bf(
  video_pce_IN ~ wiggleN_Simeone, hu ~ wiggleN_Simeone), 
  data = result_df,
  family = hurdle_negbinomial(link = "identity", link_shape = "log", link_hu = "logit"))

# Define weakly informative priors 
prior <- c(
  set_prior("normal(0, 2)", class = "Intercept"),              # Prior for mu_Intercept
  set_prior("normal(0, 2)", class = "Intercept", dpar = "hu"), # Prior for hu_Intercept
  set_prior("normal(1, 5)", class = "b"),                      # Prior for regression coefficients (mu_wiggleN_Simeone)
  set_prior("normal(0, 2)", class = "b", dpar = "hu"),         # Prior for regression coefficients (hu_wiggleN_Simeone)
  set_prior("lognormal(1, 0.5)", class = "shape"))

#-----------------------------------------------------
# Fit a prior (only) model and a 'posterior' hurdle model  
#-----------------------------------------------------
# https://arelbundock.com/posts/marginaleffects_priors/
# but instead of avg_comparisons I used the draws directly 

m_Simeone_prior <- brm(bf(
  video_pce_IN ~ wiggleN_Simeone, hu ~ wiggleN_Simeone), 
  data = result_df,
  family = hurdle_negbinomial(link = "identity", link_shape = "log", link_hu = "logit"),
  iter = 2000, warmup = 1000, chains = 4, cores = cores,
  prior = prior,
  sample_prior = "only", 
  file = "./brms/Simeone/m_Simeone_prior")

m_Simeone_post <- brm(bf(
  video_pce_IN ~ wiggleN_Simeone, hu ~ wiggleN_Simeone), 
  data = result_df,
  family = hurdle_negbinomial(link = "identity", link_shape = "log", link_hu = "logit"),
  iter = 2000, warmup = 1000, chains = 4, cores = cores,
  prior = prior,
  sample_prior = T, 
  file = "./brms/Simeone/m_Simeone_post")

# Compare the empirical distribution of the data y to the distributions of simulated/replicated data yrep 
# from the posterior predictive distribution. 

# bayesplot::ppc_dens_overlay(y = result_df$video_pce_IN, yrep = m_Simeone_pp[1:100, ]) +
#   labs(title = "Prior Predictive Distribution",
#        x = "video_pce_IN",
#        y = "Density")
performance::check_predictions(m_Simeone_post) +  
labs(subtitle = "Posterior Predictive Distribution", x = "video_pce_IN", y = "Density")

# Logged plot of the posterior predictive distribution
m_Simeone_pp <- posterior_predict(m_Simeone_post)

PPD = bayesplot::ppc_dens_overlay(y = log1p(result_df$video_pce_IN), yrep = log1p(m_Simeone_pp[1:100,])) + 
  labs(x = "log(Video prey captures)", y = "log(Density)") +
  gg_theme() +
  theme(legend.position = "inside", legend.position.inside = c(0.9, 0.75)) + 
  annotate("text", x = Inf, y = Inf, 
           label = "Relative vertical velocity wiggles (0.35m)\n(Simeone and Wilson 2003)", 
           size = 4, 
           hjust = 1.1, vjust = 1.6)

PPD 

# Save as PNG
ggsave(paste0("./plots/brms/Simeone_ppd.png"), PPD, width = 5, height = 5, dpi = 300)

loo_Simeone_post = loo(m_Simeone_post)
loo_Simeone_post

#-----------------------------------------------------
# Extract parameter draws for the prior-only model
#-----------------------------------------------------
prior_draws <- as_draws_df(m_Simeone_prior)

# Extract parameter draws for the posterior model
posterior_draws <- as_draws_df(m_Simeone_post)

# Combine draws into a single data frame for plotting
prior_draws$Label <- "Prior"
posterior_draws$Label <- "Posterior"
draws <- bind_rows(prior_draws, posterior_draws)

# Select only the relevant parameters
draws <- as_tibble(draws) %>%
  select(Label, starts_with("b_"), starts_with("Intercept"), starts_with("shape")) %>%
  pivot_longer(cols = -Label, names_to = "Parameter", values_to = "Value")

# Convert back to 'draws_df' object explicitly
draws <- as_draws_df(draws)

# Update parameter names for clarity
draws$Parameter <- recode(draws$Parameter,
                          "b_Intercept" = "mu_Intercept",
                          "b_wiggleN_Simeone" = "mu_wiggleN_Simeone",
                          "b_hu_Intercept" = "hu_Intercept",
                          "b_hu_wiggleN_Simeone" = "hu_wiggleN_Simeone",
                          "shape" = "Shape")

# Extract specific colors from the "magma" color map
colors <- viridis::viridis(3, option = "viridis")
# Select the colors you want
selected_colors <- colors[c(1, 2)]
selected_colors

# Plot the prior and posterior distributions
PPD2 = ggplot(draws, aes(x = Value, color = Label, fill = Label)) +
  geom_density(alpha = 0.8) +
  facet_wrap(~ Parameter, scales = "free", ncol = 2) +
  labs(#title = "Prior and Posterior Distributions",
       x = "Value",
       y = "Density") +
  gg_theme() + 
  scale_fill_manual(values = selected_colors) +
  scale_color_manual(values = selected_colors) + 
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.2),
        legend.text=element_text(size = 8),
        legend.title=element_text(size = 8),
        legend.key.size=unit(0.8, "lines")) 

PPD2 

# Save as PNG
ggsave(paste0("./plots/brms/Simeone_prior_post.png"), PPD2, width = 5, height = 5, dpi = 300)

result_df %>%
  ungroup() %>% 
  modelr::data_grid(wiggleN_Simeone = modelr::seq_range(wiggleN_Simeone, n = 100)) %>%
  add_predicted_draws(m_Simeone_post) %>%
  ggplot(aes(x = wiggleN_Simeone, y = video_pce_IN)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.90, .80, .50), alpha = 0.9) +
  geom_point(data = result_df) +
  scale_fill_brewer(palette = "Blues") +
  scale_color_brewer(palette = "Blues") + 
  labs(y = "Video PCE", x = 'Wiggle_Simeone and Wilson 2003 (0.3m)') +
  scale_x_continuous(breaks=seq(0,15,by=5))

# https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/

# https://discourse.mc-stan.org/t/confusion-on-difference-between-posterior-epred-and-posterior-predict-in-a-mixed-effects-modelling-context/28813
# The distinction between epred and predict is always whether you are talking about
# the distribution (uncertainty) of individual cases (predict) or 
# the average/expectation (epred). 
# Eg, in the context of a single-level normal regression model, 
# does the distribution include just the uncertainty in the mean/average/expectation (epred) or
# also the individual-level variation (sigma) and its uncertainty (predict)?

#posterior_epred.brmsfit {brms}
# Compute posterior draws of the expected value of the posterior predictive distribution. 
# Can be performed for the data used to fit the model (posterior predictive checks) 
# or for new data. 
# By definition, these predictions have smaller variance than the posterior predictions performed
# by the posterior_predict.brmsfit method. 
# This is because only the uncertainty in the expected value of the posterior predictive distribution 
# is incorporated in the draws computed by posterior_epred while the residual error is ignored there.
# However, the estimated means of both methods averaged across draws should be very similar.


#-------------------------------------------
# Plot video and wiggles against each other 
#-------------------------------------------
#https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/

get_Density <- function(x, y, ...) {
  Density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, Density_out$x)
  int_y <- findInterval(y, Density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(Density_out$z[comb_int])
}

# CE_mu_m_Simeone = conditional_effects(m_Simeone_post, method = "posterior_epred", prob = 0.9)
# CE_mu_m_Simeone
# CE_mu_h_m_Simeone = conditional_effects(m_Simeone_post, method = "posterior_epred", prob = 0.9, dpar = "hu")
# CE_mu_h_m_Simeone

# posterior_predict.brmsfit {brms}
# Compute posterior draws of the posterior predictive distribution. 
# Can be performed for the data used to fit the model (posterior predictive checks) or
# for new data. 
# By definition, these draws have higher variance than draws of the expected value
# of the posterior predictive distribution computed by posterior_epred.brmsfit.
# This is because the residual error is incorporated in posterior_predict. 
# However, the estimated means of both methods averaged across draws should be very similar.

# CE_pi_m_Simeone = conditional_effects(m_Simeone_post, method = "posterior_predict", prob = 0.9)
# CE_pi_m_Simeone


# --------------------------
# Extract components:
# --------------------------
CE50_mu = conditional_effects(m_Simeone_post, method = "posterior_epred", prob = 0.5)
CE50_mu_h = conditional_effects(m_Simeone_post, method = "posterior_epred", prob = 0.5, dpar = "hu")
CE50_pp = conditional_effects(m_Simeone_post, method = "posterior_predict", prob = 0.5)

CE80_mu = conditional_effects(m_Simeone_post, method = "posterior_epred", prob = 0.8)
CE80_mu_h = conditional_effects(m_Simeone_post, method = "posterior_epred", prob = 0.8, dpar = "hu")
CE80_pp = conditional_effects(m_Simeone_post, method = "posterior_predict", prob = 0.8)

CE90_mu = conditional_effects(m_Simeone_post, method = "posterior_epred", prob = 0.9)
CE90_mu_h = conditional_effects(m_Simeone_post, method = "posterior_epred", prob = 0.9, dpar = "hu")
CE90_pp = conditional_effects(m_Simeone_post, method = "posterior_predict", prob = 0.9)

wig = (CE50_mu)[[1]]$wiggleN_Simeone
vid = (CE50_mu)[[1]]$video_pce_IN
mu = (CE50_mu)[[1]]$estimate

ppwig = (CE50_pp)[[1]]$wiggleN_Simeone
ppvid = (CE50_pp)[[1]]$video_pce_IN
ppmu = (CE50_pp)[[1]]$estimate

lwl50 = (CE50_mu)[[1]]$lower
upl50 = (CE50_mu)[[1]]$upper
lwl80 = (CE80_mu)[[1]]$lower
upl80 = (CE80_mu)[[1]]$upper
lwl90 = (CE90_mu)[[1]]$lower
upl90 = (CE90_mu)[[1]]$upper

pp50lwl = (CE50_pp)[[1]]$lower
pp50upl = (CE50_pp)[[1]]$upper
pp80lwl = (CE80_pp)[[1]]$lower
pp80upl = (CE80_pp)[[1]]$upper
pp90lwl = (CE90_pp)[[1]]$lower
pp90upl = (CE90_pp)[[1]]$upper

plotdat_Simeone = data.frame(wig = wig, vid = vid, 
                     mu = mu, 
                    # lwl = lwl, upl = upl,
                     ppwig = ppwig, ppvid = ppvid, 
                    #pp50mu = pp50mu,
                     pp50lwl = pp50lwl, pp50upl = pp50upl,
                     pp80lwl = pp80lwl, pp80upl = pp80upl,
                     pp90lwl = pp90lwl, pp90upl = pp90upl)

Density_est <- result_df %>% 
  dplyr::select(id, video_pce_IN, wiggleN_Simeone) %>% 
  mutate(Density = get_Density(video_pce_IN, wiggleN_Simeone, n = 100))

prediction_plot = ggplot() + 
  # posterior prediction intervals  
  geom_ribbon(data = plotdat_Simeone, aes(x = ppwig, ymin = pp50lwl, ymax = pp50upl), alpha = 0.2, fill = "grey51") +
  geom_ribbon(data = plotdat_Simeone, aes(x = ppwig, ymin = pp80lwl, ymax = pp80upl), alpha = 0.2, fill = "grey51") +
  geom_ribbon(data = plotdat_Simeone, aes(x = ppwig, ymin = pp90lwl, ymax = pp90upl), alpha = 0.2, fill = "grey51", col = "black")+ 
# epred (mu)
#  geom_ribbon(data = plotdat, aes(x = wig, ymin = lwl50, ymax = upl50), alpha = 0.4, fill = "#440154FF")+
#  geom_ribbon(data = plotdat, aes(x = wig, ymin = lwl80, ymax = upl80), alpha = 0.3, fill = "#440154FF") +
geom_ribbon(data = plotdat_Simeone, aes(x = wig, ymin = lwl90, ymax = upl90), alpha = 0.3, fill = "#440154FF") +
  geom_line(data = plotdat_Simeone, aes(x = wig, y = mu), linewidth = 1, color = "black") +
 # gg_theme() +
  gg_theme()+
  ggnewscale::new_scale_color() +
  geom_point(data = Density_est , 
             aes(x = wiggleN_Simeone, y = video_pce_IN, color = Density), alpha = 1) + 
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1) +
  xlab("Relative vertical velocity wiggle count (0.35m)") + 
  ylab("Video observed prey captures") 

# Save as PNG
ggsave(paste0("./plots/brms/Simeone_prediction_plot.png"), prediction_plot, width = 6, height = 4, dpi = 300)


#----------------------------------------------------------------------------------------
# brms LOGO-CV
#----------------------------------------------------------------------------------------

# Fit multiple models and store results in a list
models_Simeone_train <- list()
predict_on_test_Simeone <- list()
ggplots_Simeone <- list()
ggplots2_Simeone <- list()

# Get individual IDs 
unique_ids <- unique(result_df$id)
length(unique_ids )

for (ids in unique_ids) {

# ids = '2023_01_14_DI10'   # testing

# Train set: Include all individuals except the one excluded in the test set
train_data <- result_df %>% 
               filter(id != ids)

n_distinct(train_data$id)

# Test set: Exclude one individual
test_data <- result_df %>%  
             filter(id == ids)

n_distinct(test_data$id)

# fit model on training data
models_Simeone_train[[ids]] <- 
    brm(bf(video_pce_IN ~ wiggleN_Simeone, hu ~ wiggleN_Simeone), 
        data = train_data,
        family = hurdle_negbinomial(link = "identity", link_shape = "log", link_hu = "logit"),
        iter = 2000, warmup = 1000, chains = 4, cores = cores,
        prior = prior,
        sample_prior = T, 
        file = paste("./brms/Simeone/m_Simeone_train_", ids))

# predict on excluded individual
predict_on_test = as.data.frame(predict(models_Simeone_train[[ids]], 
                                newdata = test_data))

# Are there any PCE in the test data? 
test_data$video_pce_IN

# Are there any PCE predicted? 
predict_on_test$Estimate
predict_on_test$Q2.5
predict_on_test$Q97.5

# add model predictions to the test data 
predict_on_test_Simeone[[ids]] = cbind(test_data, predict_on_test)

#n_distinct(predict_on_test_Simeone[[ids]]$id)
#unique(predict_on_test_Simeone[[ids]]$id)

ggplots_Simeone[[ids]] = ggplot(data = 
         predict_on_test_Simeone[[ids]],
         aes(x = video_pce_IN, y = Estimate)) + 
         geom_point() + 
         theme_bw() + 
         xlim(0,126) +
         labs(subtitle = paste0("Simeone and Wilson 2003 (0.3m). ",
                        "Training data (N) = ", n_distinct(train_data$id),
                        ". Test ID = ", unique(predict_on_test_Simeone[[ids]]$id)))

ggplots_Simeone[[ids]]

# Save as PNG

ggsave(paste0("./plots/brms/Simeone/Simeone_prediction_observed_", ids, ".png"), ggplots_Simeone[[ids]], width = 7, height = 5, dpi = 300)

# Cumulative PCE 
# Video observed cumulative PCE
predict_on_test_Simeone[[ids]]$video_cumulativePCE

# Cumulative wiggles
predict_on_test_Simeone[[ids]]$cumulativeEstimate = cumsum(predict_on_test_Simeone[[ids]]$Estimate)
predict_on_test_Simeone[[ids]]$cumulativeQ2.5 = cumsum(predict_on_test_Simeone[[ids]]$Q2.5)
predict_on_test_Simeone[[ids]]$cumulativeQ97.5 = cumsum(predict_on_test_Simeone[[ids]]$Q97.5)

ggplots2_Simeone[[ids]] = predict_on_test_Simeone[[ids]] %>%
         ggplot() + 
         geom_line(aes(x = dive.nr, y = video_cumulativePCE), color = "black") +
         geom_point(aes(x = dive.nr, y = video_cumulativePCE), color = "black") +
         geom_line(aes(x = dive.nr, y = cumulativeEstimate), color = "blue") + 
  geom_line(aes(x = dive.nr, y = cumulativeQ2.5), color = "blue", linetype = 2) + 
  geom_line(aes(x = dive.nr, y = cumulativeQ97.5), color = "blue", linetype = 2) + 
  theme_bw() +
  xlab("Dive number") + 
  ylab("Cumulative captures") + 
  labs(subtitle = paste0("Simeone and Wilson 2003 (0.3m). ",
                 "Test ID = ", unique(predict_on_test_Simeone[[ids]]$id),
                 ". Black: video PC; Blue: predicted PC"))

# Save as PNG
ggsave(paste0("./plots/brms/Simeone/Simeone_cumulative_pred_obs_", ids, ".png"), ggplots2_Simeone[[ids]], width = 7, height = 5, dpi = 300)

}

######

predict_test_Simeone = bind_rows(predict_on_test_Simeone)
dim(predict_test_Simeone)
names(predict_test_Simeone)


Density_est <- predict_test_Simeone %>% 
  dplyr::select(id, video_pce_IN, Estimate, Q2.5, Q97.5) %>% 
  mutate(Density = get_Density(video_pce_IN, Estimate, n = 100))


#-------------------------------------------
# plot LOGO-CV predictions of all penguins
#-------------------------------------------

LOGOCV_preds = Density_est %>%
#  slice(1:40) %>%
  ggplot(aes(x = Estimate, y =video_pce_IN , color = Density)) +
  geom_point(alpha = 0.3) + 
#  geom_pointrange(aes(xmin = Q2.5, xmax = Q97.5)) + 
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1) +
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.99, 0.01),  # Adjust legend position
    legend.justification = c(1, 0),  # Justify legend to bottom-right
    legend.box.just = "right") +       # Align legend to the right of the box
  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  #  geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
  xlab("Model estimated prey captures") +
  ylab("Video prey captures") +
  scale_y_continuous(breaks = seq(0,125, by = 25)) +
  scale_x_continuous(limits = c(0, 60)) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = "Relative vertical velocity wiggles (0.35m)\n(Simeone and Wilson 2003)", size = 4, 
           hjust = 1.1, vjust = 1.6) + 
  theme(#legend.text=element_text(size = 8),
    #           legend.title=element_text(size = 10),
    legend.key.size=unit(0.8, "lines")) 

LOGOCV_preds 

# Save as PNG
ggsave(paste0("./plots/brms/Simeone_LOGOCV_prediction_plot.png"), LOGOCV_preds, width = 6, height = 4, dpi = 300)

#------------------------------------------------------
# plot LOGO-CV cumulative predictions of all penguins
#------------------------------------------------------

LOGOCV_cum_preds =  
predict_test_Simeone  %>%
  arrange(dive.nr) %>%
  ggplot() + 
  geom_point(aes(x =cumulativeEstimate, y = video_cumulativePCE), color = "navy", size = 1) +
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black")

LOGOCV_cum_preds

# Save as PNG
ggsave(paste0("./plots/brms/Simeone_LOGOCV_cum_prediction_plot.png"), LOGOCV_cum_preds, width = 6, height = 4, dpi = 300)


