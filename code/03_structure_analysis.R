library(tidyverse)
select <- dplyr::select
library(broom)
library(multcomp)

# Load datasets created earlier in '01_plot_data_prep.R'
prefire_df <- readRDS('dataframes/prefire_df.R')
postfire_df <- readRDS('dataframes/postfire_df.R')
info_df <- readRDS('dataframes/info_df.R')

# Qualitative exploration of the distribution of postfire tree density ---------
# Summarize both by species and combine 
prefire_plot <- prefire_df %>% 
  group_by(plot_num, season) %>% 
  summarise(pre_dens = sum(dens_ha)) 

postfire_plot <- postfire_df %>% 
  #mutate(dens_ha = ifelse(dens_ha > 500000, 500000, dens_ha)) %>% 
  group_by(plot_num, season) %>% 
  summarise(post_dens = sum(dens_ha)) 

combined_plot <- full_join(prefire_plot, postfire_plot) %>% 
  mutate(season = factor(season, levels = c('2019','2020'), 
                         labels = c('Mature (200+ yrs)', 'Young (15 yrs)')))

# Medians
std_e <- function(x) sd(x)/sqrt(length(x))

combined_plot %>% 
  group_by(season) %>% 
  summarize(across(c(pre_dens, post_dens), list(med = median, se = std_e)))

# Tests
young_test <- combined_plot %>%
  pivot_longer(cols = c(pre_dens, post_dens), names_to = 'time', values_to = 'dens') %>% 
  mutate(time = as.factor(time)) %>% 
  ungroup() %>% 
  filter(season == 'Young (15 yrs)') %>% 
  as.data.frame()

coin::median_test(dens~time, data = young_test)

old_test <- combined_plot %>%
  pivot_longer(cols = c(pre_dens, post_dens), names_to = 'time', values_to = 'dens') %>% 
  mutate(time = as.factor(time)) %>% 
  ungroup() %>% 
  filter(season == 'Mature (200+ yrs)') %>% 
  as.data.frame()

coin::median_test(dens~time, data = old_test)

old_test <- combined_plot %>%
  ungroup() %>% 
  filter(season == 'Mature (200+ yrs)') %>% 
  select(pre_dens, post_dens) %>% 
  as.data.frame()

ks.test(old_test$pre_dens, old_test$post_dens, alternative = 'two.sided')

# Plotting
ggplot(combined_plot) +
  geom_histogram(aes(x = pre_dens, fill = 'Prefire'), binwidth = 0.2, 
                 color = 'black') +
  geom_histogram(aes(x = post_dens, fill = 'Postfire', ), binwidth = 0.2, 
                 color = 'black', alpha = 0.7) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  facet_wrap(~season, ncol = 2, scales = 'free_y') +
  scale_fill_manual(values = c('#d95f02','#137054')) +
  theme_bw(base_size = 10) +
  labs(x = 'Tree density (#/ha)', y = 'Number of plots') + 
  scale_y_continuous(breaks = seq(0,12,2)) +
  theme(axis.text = element_text(color = 'black', size = 10),
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 12),
        legend.position = 'bottom',
        legend.title = element_blank())

ggsave('density_histograms.png', height = 4, width = 7, units = 'in', dpi = 600)


# Relationship between pre and post
combined_plot %>% 
  filter(post_dens > 0) %>% 
  group_by(season) %>% 
  do(tidy(lm(log10(post_dens)~log10(pre_dens), data = .)))

combined_plot %>% 
  mutate(across(c(pre_dens, post_dens), ~ ifelse(.x == 0, 1, .x))) %>% 
  group_by(season) %>% 
  summarize(R2 = cor.test(log(pre_dens),log(post_dens))$estimate,
            p = cor.test(pre_dens,post_dens)$p.value)
  
ggplot(combined_plot, aes(x = pre_dens, y = post_dens)) +
  geom_point(aes(fill = season), shape = 21, size = 2) +
  geom_smooth(aes(color = season), method = 'lm', size = 1, se = T) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = 'lb') +
  scale_color_manual('', values = c('#7570b3','#e7298a')) +
  scale_fill_manual('', values = c('#7570b3','#e7298a')) +
  theme(panel.grid.minor = element_blank()) +
  theme_bw(base_size = 10) +
  labs(x = 'Prefire density (trees/ha)', y = 'Postfire density (trees/ha)') +
  theme(legend.position = 'bottom')

ggsave('density_scatter_linear.png', height = 5, width = 5, units = 'in', dpi = 600)

# Compare understory cover short and long
plot_df <- readRDS('dataframes/glac_plot_df.R') %>% 
  full_join(info_df)

understory <- plot_df %>% 
  select(utm_x, utm_y, forb, gram, shrub, tree, total)

understory %>% 
  group_by(season) %>% 
  summarise(across(c(forb, gram, shrub, tree, total), list(mean = mean, se = std_e)))

understory_long <- understory %>% 
  pivot_longer(cols = forb:total, names_to = 'type', values_to = 'pct') %>% 
  filter(type != 'tree') 

ggplot(understory_long) +
  geom_boxplot(aes(x = season, y = pct, fill = season), alpha = 0.8) +
  facet_wrap(~type, scales = 'free', nrow = 1)

# ANOVAs
# ANOVA test for differences in means among aspects
understory_long %>%
  group_by(type) %>% 
  do(glance(aov(pct ~ season, data = .))) %>% 
  mutate_if(is.numeric, round, 4) 

understory_long %>%
  group_by(type) %>% 
  do(tidy(aov(pct ~ season, data = .))) %>% 
  filter(term != 'Residuals') %>% 
  mutate_if(is.numeric, round, 3) 

# Tukey multiple pairwise comparisons of significant differences
understory_long %>%
  group_by(type) %>% 
  do(tidy(TukeyHSD(aov(pct ~ season, data = .), conf.level = 0.95))) 

tukey_fri_cld <- understory_long %>%
  group_by(type) %>% 
  do(tidy(cld(glht(aov(pct ~ season, data = .),
                   linfct = mcp(season = 'Tukey')),
              level = 0.05))) 

understory_long %>% 
  group_by(season, type) %>% 
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$pct, na.rm=TRUE)))) %>% 
  ungroup() %>% 
  left_join(., tukey_fri_cld) %>% 
  mutate(season = factor(season, levels = c('2019','2020'), 
                         labels = c('Mature (200+ yrs)', 'Young (15 yrs)'))) %>% 
  # Begin plotting
  ggplot(., aes(x = type, y = Mean, fill = season)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                size = 0.75, width = 0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual('Stand age', values = c('#7570b3','#e7298a')) +
  scale_x_discrete(labels = c('Forb', 'Graminoid', 'Shrub', 'Total')) +
  geom_text(aes(y = Upper + 3, label = letters), position = position_dodge(width = 0.9),
            color = 'black', size = 4) +
  # geom_text(aes(y = -0.03, label = substring(aspect, 1, 1)), 
  #           position = position_dodge(width = 0.9), color = 'grey30') +
  labs(y = 'Mean Percent Cover', x = 'Functional Type') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(color = 'black', size = 10),
        panel.grid.minor.y = element_blank(), 
        legend.position = 'bottom') 

ggsave('cover_comparison.png', height = 5, width = 6, units = 'in', dpi = 600)

