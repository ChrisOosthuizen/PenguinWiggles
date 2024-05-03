#---------------------------
# Logistic regression
#---------------------------

table(result_df$foraging_dive01)

logit.reg <- glm(foraging_dive01 ~ wiggleN_Halsey_05m + ascspeed + maxdep ,#+ as.numeric(dive.gap),
                 data = result_df,
                 family = binomial(link = "logit"))

summary(logit.reg)

range(result_df$wiggleN_Halsey_05m)
xweight <- seq(min(result_df$wiggleN_Halsey_05m), max(result_df$wiggleN_Halsey_05m), 0.01)
xweight2 <- seq(min(result_df$ascspeed), max(result_df$ascspeed), length.out = length(xweight))
xweight3 <- seq(min(result_df$maxdep), max(result_df$maxdep), length.out = length(xweight))
#xweight4 <- seq(min(result_df$dive.gap), max(result_df$dive.gap), length.out = length(xweight))

#Now we use the predict() function to create the model for all of the values of xweight.
yweight <- predict(logit.reg, list(wiggleN_Halsey_05m = xweight, 
                                   ascspeed = xweight2,
                                   maxdep = xweight3#,
                                   # dive.gap = xweight4
                                   
),
type="response")

xy = as.data.frame(cbind(xweight, yweight))

#Now we plot.
ggplot(data = result_df,
       aes(x = wiggleN_Halsey_05m, 
           y = foraging_dive01)) +  
  geom_jitter(height = 0.1, 
              width = 0.45, alpha = 0.3) + 
  geom_line(data = xy, aes(x = xweight, y = yweight), linewidth = 2)


library(jtools)
effect_plot(logit.reg, 
            pred = wiggleN_Halsey_05m, 
            plot.points = T,jitter = c(0.2, 0.1),
            interval = TRUE, y.label = "% testing positive")

library(lme4)
m <- glmer(foraging_dive01 ~ wiggleN_Halsey_2m + (1|id), 
           data = result_df, family = binomial)
summary(m, corr = FALSE)



#---------------------------
# Logistic regression
#---------------------------

logit.reg <- glm(foraging_dive01 ~ wiggleN_Takahashi + ascspeed + maxdep ,#+ as.numeric(dive.gap),
                 data = result_df,
                 family = binomial(link = "logit"))

summary(logit.reg)

xweight <- seq(min(result_df$wiggleN_Takahashi), max(result_df$wiggleN_Takahashi), 0.01)
xweight2 <- seq(min(result_df$ascspeed), max(result_df$ascspeed), length.out = length(xweight))
xweight3 <- seq(min(result_df$maxdep), max(result_df$maxdep), length.out = length(xweight))
#xweight4 <- seq(min(result_df$dive.gap), max(result_df$dive.gap), length.out = length(xweight))

#Now we use the predict() function to create the model for all of the values of xweight.
yweight <- predict(logit.reg, list(wiggleN_Takahashi = xweight, 
                                   ascspeed = xweight2,
                                   maxdep = xweight3#,
                                   # dive.gap = xweight4
                                   
),
type="response")

xy = as.data.frame(cbind(xweight, yweight))

#Now we plot.
ggplot(data = result_df,
       aes(x = wiggleN_Takahashi, 
           y = foraging_dive01)) +  
  geom_jitter(height = 0.1, 
              width = 0.45, alpha = 0.3) + 
  geom_line(data = xy, aes(x = xweight, y = yweight), linewidth = 2)
