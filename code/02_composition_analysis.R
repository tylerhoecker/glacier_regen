library(tidyverse)
select <- dplyr::select
library(vegan)
library(ggalluvial)

# Load datasets created earlier in 'regen_plot_explore.R'
prefire_df <- readRDS('dataframes/prefire_df.R')
postfire_df <- readRDS('dataframes/postfire_df.R')
info_df <- readRDS('dataframes/info_df.R')

# Plot composition by elevation ------------------------------------------------
prefire_stats <- full_join(prefire_df, select(info_df, plot_num, phys_elev)) %>% 
  mutate(plot_num = fct_reorder(plot_num, phys_elev),
         elev_class = round(phys_elev/100)) %>% 
  group_by(plot_num) %>%
  top_n(1, impval) 

sp_cols <- c('LAOC' = '#e6ab02', 'TSHE' = '#d95f02', 'THPL' = '#377eb8', 
             'PSME' = '#e7298a', 'PIMO' = '#66a61e', 'PIEN' = '#1b9e77', 'ABLA' = '#984ea3',
             'PICO' = '#a65628','POTR' = '#e41a1c',  'None' = 'grey')

ggplot(prefire_stats) +
  geom_bar(aes(x = elev_class, fill = sp, alpha = season, color = season), width = 0.9) +
  scale_x_continuous(breaks = seq(10,16,1),labels = seq(10,16,1)*100) +
  #scale_x_discrete(labels = sort(unique(prefire_df$phys.phys_elev))) +
  scale_fill_manual('Prefire dominance', values = sp_cols) +
  scale_alpha_manual('', values = c(0.7,1), labels = c('Young (15 yrs)','Mature (200+ yrs)')) +
  scale_color_manual('', values = c('black','transparent'), labels = c('Young (15 yrs)','Mature (200+ yrs)')) +
  #scale_y_continuous(labels = c(0,1,2), breaks = c(0,1,2)) +
  theme_bw(base_size = 10) +
  labs(x = 'Plot elevation (m)', y = 'Number of plots') +
  theme(legend.position = 'bottom',
        legend.box = 'vertical',
        axis.text = element_text(color = 'black', size = 10))

ggsave('prefire_sp_elev.png', height = 7.5, width = 5, units = 'in', dpi = 600)

# Alluvial plots
# -------------------------------------------------------------------------------
pre_dom <- prefire_df %>% 
  group_by(plot_num) %>% 
  top_n(1, impval) %>% 
  rename(pre_sp = sp, pre_dens = dens_ha) %>% 
  select(plot_num, season, pre_sp, pre_dens)

post_dom <- postfire_df %>% 
  rowwise() %>% 
  mutate(sp = ifelse(sum(dens_ha) == 0, 'None', sp),
         dens_ha = ifelse(sp == 'None', 1, dens_ha)) %>%
  group_by(plot_num) %>% 
  top_n(1, impval) %>%
  select(plot_num, season, post_sp = sp, post_dens = dens_ha) %>% 
  distinct(.keep_all = T) 

alluv_df <- full_join(pre_dom, post_dom) %>% 
  pivot_longer(cols = c(pre_sp,post_sp), names_to = 'time', values_to = 'dom_sp') %>% 
  group_by(season, time, dom_sp) %>% 
  mutate(count = n()) %>% 
  mutate(dom_sp = factor(dom_sp, levels = c('PIEN','ABLA','PICO','LAOC','PSME','TSHE','THPL','POTR','None')),
         time = factor(time, levels = c('pre_sp', 'post_sp'))) %>% 
  #filter(season == '2019') %>% 
  ungroup() %>% 
  select(season, time, count, plot_num, dom_sp) %>% 
  mutate(season = factor(season, levels = c('2019','2020'), 
                         labels = c('Mature (200+ yrs)', 'Young (15 yrs)')))

# Just for 20202
fill_in <- expand.grid('time' = as.factor(c('pre_sp', 'post_sp')),
                       'plot_num' = unique(alluv_df$plot_num))
alluv_df <- full_join(alluv_df, fill_in) %>% 
  distinct(time, plot_num, .keep_all = T)

alluv_df %>% 
  group_by(season, time, dom_sp) %>% 
  tally() %>% 
  arrange(season, dom_sp) %>% 
  write_csv('dominance_alluv_table.csv')

ggplot(alluv_df,
       aes(x = time, stratum = dom_sp, alluvium = plot_num,
           weight = count,
           fill = dom_sp, label = dom_sp)) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum", size = 3) + 
  scale_fill_manual('Dominant \nspecies', values = sp_cols) +
  scale_x_discrete(labels = c('Prefire','Postfire')) +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.text = element_text(color = 'black', size = 12),
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 12)) +
  labs(y = 'Number of plots') +
  facet_wrap(~season, scales = 'free_y') +
  theme(legend.position = 'none')

ggsave('alluvial_plot.png', height = 5, width = 7, units = 'in', dpi = 600)
# -------------------------------------------------------------------------------

# Quantitative community analyses ----------------------------------------------
# Create community composition dataframe ---------------------------------------
# Curtis-Bray Dissimilarity between same plot pre- and post-fire
diss_df <- full_join(prefire_df, postfire_df) %>%
  group_by(season, plot_num, time) %>%
  pivot_wider(names_from = sp, values_from = dens_ha, values_fill = 0) %>%
  # Collapse multiple rows from each plot
  summarise(across(c(PICO,PSME,LAOC,TSHE,THPL,PIEN,ABLA,POTR), sum)) %>%
  # Round
  summarise(across(c(PICO,PSME,LAOC,TSHE,THPL,PIEN,ABLA,POTR), round)) %>%
  # Remove empty rows
  rowwise() %>% 
  mutate(empty = sum(c(PICO,PSME,LAOC,TSHE,THPL,PIEN,ABLA,POTR))) %>% 
  filter(empty > 0)  %>% 
  ungroup() %>% 
  group_by(season, plot_num) %>% 
  # Convert to binary presence/absence
  #summarise(across(c(PICO,PSME,LAOC,TSHE,THPL,PIEN,ABLA), ~ ifelse(.x > 0, 1, 0))) %>%
  # mutate(empty = sum(PICO,PSME,LAOC,TSHE,THPL,PIEN,ABLA))
  mutate(season = factor(season, levels = c('2019','2020'),
                         labels = c('Mature (200+ yrs)', 'Young (15 yrs)'))) %>%
  group_modify(~ as.data.frame(vegdist(.x, 'bray')[1])) %>% 
  rename(bray = `vegdist(.x, "bray")[1]`) 

# ANOVA
# Tukey multiple pairwise comparisons of significant differences
std_e <- function(x) sd(x)/sqrt(length(x))

diss_df %>% 
  group_by(season) %>% 
  summarize(across(bray, list(med = ~ median(.x, na.rm = T), se = ~ std_e(!is.na(.x)))))

diss_df %>%
  aov(bray ~ season, data = .) %>% 
  summary()

# Plot
ggplot(diss_df) +
  # geom_histogram(data = filter(diss_df, season == 'Long-interval (>200 yrs)'),
  #                 aes(x = bray, fill = season, color = season), 
  #                position = 'dodge', bins = 14) +
  # geom_histogram(data = filter(diss_df, season == 'Short-interval (15 yrs)'),
  #                aes(x = bray, fill = season, color = season), 
  #                position = 'dodge', bins = 14, alpha = 0.8) +
  geom_boxplot(aes(x = season, y = bray, fill = season)) +
  scale_color_manual('FRI', values = c('#524f80','#961b5b')) +
  scale_fill_manual('FRI', values = c('#7570b3','#e7298a')) +
  theme_bw(base_size = 10) +
  #scale_y_continuous(breaks = seq(0,10,5)) +
  labs(x = '', y = 'Bray-Curtis Dissimilarity Index') +
  theme(legend.position = 'none',
        axis.text = element_text(color = 'black', size = 10))

ggsave('braycurtis.png', height = 7, width = 3, units = 'in', dpi = 300)

# Non-metric multidimensional scaling
nmds_df <- full_join(prefire_df, postfire_df) %>%
  group_by(season, plot_num, time) %>%
  pivot_wider(names_from = sp, values_from = dens_ha, values_fill = 0) %>%
  # Collapse multiple rows from each plot
  summarise(across(c(PICO,PSME,LAOC,TSHE,THPL,PIEN,ABLA,POTR), sum)) %>%
  # Convert to binary presence/absence
  group_by(season, plot_num, time) %>%
  summarise(across(c(PICO,PSME,LAOC,TSHE,THPL,PIEN,ABLA,POTR), ~ ifelse(.x > 0, 1, 0))) %>%
  mutate(rowID = paste0(season,'_',time,'_',plot_num)) %>%
  column_to_rownames(var = 'rowID') %>%
  filter(rowSums(select(., PICO,PSME,LAOC,TSHE,THPL,PIEN,ABLA,POTR)) > 0) 

nmds_results <- metaMDS(select(nmds_df, -time, -plot_num, -season),
                        distance = 'bray',
                        autotransform = FALSE,
                        k = 2,
                        trymax = 50,
                        pc= TRUE)

# treat <- paste(nmds_df$time,nmds_df$season,sep = '_')
# unique(treat)
# 
# # 'Default' vegan plots
# ordiplot(nmds_results,type="n")
# ordispider(nmds_results,groups=treat, col=c('#7570b3','#e7298a','#524f80','#961b5b'),label=T)
# orditorp(nmds_results,display="species",air=0.01)
# orditorp(nmds_results,display="sites", air=0.01,cex=.25)

# Spider plots in ggplot: https://stackoverflow.com/questions/47516448/how-to-get-ordispider-like-clusters-in-ggplot-with-nmds   
# Extract scores and  summarize i
std_e <- function(x) sd(x)/sqrt(length(x))

nmds_scores <- scores(nmds_results, display = 'sites') %>% 
  as.data.frame() %>% 
  rownames_to_column(var = 'id') %>% 
  separate(id, into = c('season','time','plot')) %>% 
  unite('group', c(season, time), sep = '_')

nmds_means <- nmds_scores %>% 
  group_by(group) %>% 
  summarise(across(c(NMDS1,NMDS2), list(mean = mean, se = std_e)))

nmds_spp <- scores(nmds_results, display = 'species') %>% 
  as.data.frame() %>% 
  rownames_to_column(var = 'species')
  
nmds_cols <- c("2020_postfire" = '#e7298a',
               "2020_prefire"  = '#961b5b',
               "2019_postfire" = '#7570b3',
               "2019_prefire" = '#524f80')
  
ggplot(nmds_means, aes(x = NMDS1_mean, y = NMDS2_mean)) +
  geom_jitter(data = nmds_scores, aes(x = NMDS1, y = NMDS2, color = group), 
              width = 0.05, height = 0.05,
              alpha = 0.7) +
  geom_errorbar(aes(ymin = NMDS2_mean - 1.96*NMDS1_se, 
                    ymax = NMDS2_mean + 1.96*NMDS1_se,
                    color = group)) +
  geom_errorbar(aes(xmin = NMDS1_mean - 1.96*NMDS1_se, 
                    xmax = NMDS1_mean + 1.96*NMDS1_se,
                    color = group)) +
  geom_point(aes(fill = group), shape = 21, size = 5) +
  geom_label(data = nmds_spp, aes(x = NMDS1, y = NMDS2, label = species),
             size = 2, alpha = 0.7, fill = 'grey80') +
  scale_fill_manual('', values = nmds_cols, 
                    labels = c('Postfire-Mature','Prefire-Mature','Postfire-Young','Prefire-Young')) +
  scale_color_manual('', values = nmds_cols,
                     labels = c('Postfire-Mature','Prefire-Mature','Postfire-Young','Prefire-Young')) +
  labs(x = 'NMDS 1', y = 'NMDS 2') +
  scale_x_continuous(breaks = seq(-1,1), limits = c(-1,1)) +
  scale_y_continuous(breaks = seq(-1,1), limits = c(-1,1)) +
  coord_fixed() +
  theme_bw(base_size = 10) +
  theme(legend.position = 'bottom',
        axis.text = element_text(color = 'black', size = 10))
  
ggsave('nmds.png', height = 5.5, width = 5, units = 'in', dpi = 600)



# OLD
# Significance testing
# All groups
ano <- anosim(select(nmds_df, -time, -plot_num, -season), treat, distance = "bray", permutations = 999)
summary(ano)
# R = 0.2574 p = 0.0001

# Just short
short_c <- filter(nmds_df, season == 'Short-interval (15 yrs)')
short_t <- paste(short_c$time,short_c$season,sep = '_')
short_ano <- anosim(select(short_c, -time, -plot_num, -season), short_t, distance = "bray", permutations = 999)
summary(short_ano)
# R = 0.1831 p = 0.0002

# Just long
long_c <- filter(nmds_df, season == 'Long-interval (>200 yrs)')
long_t <- paste(long_c$time,long_c$season,sep = '_')
long_ano <- anosim(select(long_c, -time, -plot_num, -season), long_t, distance = "bray", permutations = 999)
summary(long_ano)
# R = 0.2519 p = 0.0001

# - - 
# Bind to group names 
scrs <- cbind(as.data.frame(scrs), Group = treat)
# Calculate the centroids for each group
cent <- aggregate(cbind(NMDS1, NMDS2) ~ Group, data = scrs, FUN = mean)
# Calculate endpoints for segments
segs <- merge(scrs, setNames(cent, c('Group','oNMDS1','oNMDS2')),
              by = 'Group', sort = FALSE)
# Add a column with species labels
sps <- scores(nmds_results, display = 'species') %>% 
  as.data.frame() %>% 
  rownames_to_column(var = 'species')

# Plot! 
ggplot(scrs, aes(x = NMDS1, y = NMDS2)) +
  geom_segment(data = segs, #filter(segs,Group == 'prefire_Long-interval (>200 yrs)')
               aes(xend = oNMDS1, yend = oNMDS2, color = Group), alpha = 0.8) +
  geom_point(data = cent, #filter(cent,Group == 'prefire_Long-interval (>200 yrs)')
             size = 6, aes(color = Group)) +
  geom_point(data = scrs, #filter(scrs,Group == 'prefire_Long-interval (>200 yrs)')
             aes(color = Group), size = 1) +
  geom_label(data = sps, aes(x = NMDS1, y = NMDS2, label = species)) +
  # scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1)) +
  # scale_y_continuous(limits = c(-1.,1), breaks = seq(-1,1)) +
  coord_fixed() +
  theme_bw(base_size = 12) +
  scale_color_manual('FRI', values = c('#7570b3','#e7298a','#524f80','#961b5b')) +
  theme()




