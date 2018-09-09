# Author:
#       Brandon Dey
#
# Date: 
#       8.28.18
#
# Purpose: 
# This script compares smoothers for article 2 for ODSC.
# 



#################
## ENVIRONMENT ##
#################
# load libraries

library(tidyverse)
library(mgcv)
library(tidyquant)
library(cowplot)

# Friedman's function = 3sin(2pi(1-x)^2)
friedmanize <- function(x){3*sin(2*pi*(1-x)^2)}

# sample and make df
n <- 500
ra <- sample(0:n, n)/n
df <-  data.frame(x = ra, y = friedmanize(ra))

# plot base plot
ggplot(data = df, 
       aes(x = x, y = y)) + 
  
  # plot the friedman line
  stat_function(
    col = "red",
    alpha = 1,
    size = 2,
    fun = function(x){3*sin(2*pi*(1-x)^2)}) +
  
  # plot jittered points around the friedman function
  geom_jitter(aes(x = x), 
              width = .08, 
              alpha = .75, 
              col = "gray") +
  
  theme_bw(base_size = 16) +
  
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1)) + 
  
  guides(col = F, 
         size = F) -> gg_base


###########################
## Simple Moving Average ## 
###########################

gg_base +
  # plot the smoothed data points
  geom_ma(n = 2, 
          ma_fun = SMA,
          col = "green", 
          linetype = 1)+
  
  labs(x = "x", 
       y = "y", 
       title = "How Does a Simple Moving Average Fit the Friedman Formula?",
       subtitle = "Poorly!", 
       caption = paste0("Graph shows how a Simple Moving Average, the green line, \n approximates the underlying Friedman formula, the red line, using ", n, " data points \n jittered about the Friedman formula (by 8% in positive and negative directions (width = .08)). \n There are only 2 observations in each bin.")) -> gg_sma


ggsave(gg_sma, 
       height = 10,
       width = 10, 
       dpi = 250,
       device = "jpeg", 
       file = "./Plots/sma.jpeg")




# Expoential moving average. 
gg_base +
  # plot the smoothed data points
  geom_ma(n = 2, 
          ma_fun = EMA, 
          col = "green", 
          linetype = 1) +
  
  labs(x = "x", 
       y = "y", 
       title = "How Does an Exponential Moving Average Fit the Friedman Formula?",
       subtitle = "Better! But still poorly.", 
       caption = paste0("Graph shows how an Exponential Moving Average, the green line, \n approximates the underlying Friedman formula, the red line, using ", n, " data points \n jittered about the Friedman formula (by 8% in positive and negative directions (width = .08)). \n There are only 2 observations in each bin.")) -> gg_ema


ggsave(gg_ema, 
       height = 10,
       width = 10, 
       dpi = 275,
       device = "jpeg", 
       file = "./Plots/ema.jpeg")


plot_grid(gg_sma, 
          gg_ema, 
          labels = "AUTO") -> gg_ema_sma

ggplot2::ggsave(gg_ema_sma, 
                height = 15,
                width = 20, 
                dpi = 165,
                device = "jpeg", 
                file = "./Plots/sma_ema.jpeg")



###########
## LOESS ##
###########

# plot loess, defaults
gg_base +
  # plot the smoothed data points
  geom_smooth(method = "loess", col = "green") +
  
  labs(x = "x", 
       y = "y", 
       title = "How Does a Default Loess Smoother Fit to the Friedman Formula?",
       subtitle = "Poorly!", 
       caption = paste0("Graph shows how Loess with default span, the green line, \n approximates the underlying Friedman formula, the red line, using ", n, " data points \n jittered about the Friedman formula (by 8% in positive and negative directions (width = .08)).")) -> gg_loess_d


ggsave(gg_loess_d, 
       height = 10,
       width = 10, 
       device = "jpeg", 
       file = "./Plots/loess - default.jpeg")



# plot loess, small bin width -- span.
gg_base +
  # plot the smoothed data points
  geom_smooth(method = "loess", 
              col = "green", 
              span = .5) +
  
  labs(x = "x", 
       y = "y", 
       title = "How Does a Wigglier Loess Smoother Fit to the Friedman Formula?",
       subtitle = "Better!", 
       caption = paste0("Graph shows how Loess with 50% span, the green line, \n approximates the underlying Friedman formula, the red line, using ", n, " data points \n jittered about the Friedman formula (by 8% in positive and negative directions (width = .08)).")) -> gg_loess_wig


ggsave(gg_loess_wig, 
       height = 10,
       width = 10, 
       device = "jpeg", 
       file = "./Plots/loess - wiggly.jpeg")


plot_grid(gg_loess_d, 
          gg_loess_wig, 
          labels = "AUTO") -> gg_loesses

ggplot2::ggsave(gg_loesses, 
                height = 15,
                width = 20, 
                dpi = 220,
                device = "jpeg", 
                file = "./Plots/loesses.jpeg")

###############################################
## mgcv::gam() 
## is used with formula = y ~ s(x, bs = "cs")
###############################################

gg_base +
  # plot the smoothed data points
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs"),
              col = "green") +
  
  labs(x = "x", 
       y = "y", 
       title = "How Does a Generalized Additive Model With Integrated Smoothness Estimation Fit to the Friedman Formula?",
       subtitle = "Well!", 
       caption = paste0("Graph shows how a Generalized Additive Model With Integrated Smoothness Estimation, the green line, \n approximates the underlying Friedman formula, the red line, using ", n, " data points \n jittered about the Friedman formula (by 8% in positive and negative directions (width = .08)).")) -> gg_gam


ggsave(gg_gam, 
       height = 10,
       width = 15, 
       dpi = 330,
       device = "jpeg", 
       file = "./Plots/gam.jpeg")


####################
## Spline ##
####################
gg_base +
  # plot the spline
  geom_line(data = data.frame(spline(df, n = 250)), 
            col = "green") +
  
  labs(x = "x", 
       y = "y", 
       title = "How Does a Cubic Spline Fit the Friedman Formula?",
       subtitle = "Well!", 
       caption = paste0("Graph shows how a Spline, the green line, \n approximates the underlying Friedman formula, the red line, using ", n, " data points \n jittered about the Friedman formula (by 8% in positive and negative directions (width = .08)). \n The spline is interpolated at 500 equally spaced points spanning the interval of x, using the default method of stats::spline(): \n fnn or 'Forsythe, Malcolm and Moler (an exact cubic is fitted through the four points at each end of the data, \n and this is used to determine the end conditions'")) -> gg_spline


ggsave(gg_spline, 
       height = 10,
       width = 15, 
       dpi = 330,
       device = "jpeg", 
       file = "./Plots/spline n.jpeg")




