# Load packages
library(tidyverse)
#library(MuMIn)
library(glmmTMB)
#library(DHARMa)
select <- dplyr::select

# Dataframe of all responses and predictors created by scripts 1 and 4
model_df <- readRDS('dataframes/model_df.R')

# Create summary tables of predictors
std_e <- function(x) sd(x)/sqrt(length(x))

model_df %>%
  ungroup() %>% 
  # Remove the sites with missing soil data... not be necessary later if soil predictors are unimportant
  filter(!is.na(pH)) %>% 
  select(contains('pre_basal'), contains('post_dens'),
         dist_unburned,serot,canopy,dev_ne,vpd,OM) %>% 
  pivot_longer(cols = everything(), names_to = 'variable', values_to = 'value') %>% 
  filter(!is.na(value)) %>% 
  group_by(variable) %>% 
  summarise(across(everything(), list(mean = mean, se = std_e, min = min, max = max, n = length))) %>% 
  write_csv('predictor_summary.csv')

# Rescaled data for modeling: 
rescaled_df <- model_df %>% 
  ungroup() %>% 
  # # Group like species 
  # mutate(post_dens_ha_PIAB = post_dens_ha_PIEN + post_dens_ha_ABLA,
  #        post_dens_ha_TSTH = post_dens_ha_TSHE + post_dens_ha_THPL,
  #        pre_basal_PIAB = pre_basal_PIEN + pre_basal_ABLA,
  #        pre_basal_TSTH = pre_basal_TSHE + pre_basal_THPL) %>%
  mutate(season = as.numeric(season)) %>% 
  # Remove the sites with missing soil data... not be necessary later if soil predictors are unimportant
  filter(!is.na(pH)) %>% 
  # Remove info variables and exclude some predictors w/o good bio rationale
  select(season, contains('post_dens'),contains('pre_basal'),dist_unburned,serot,canopy,dev_ne,tmax,vpd,OM,clay) %>% 
  # Truncate the very large post-fire density value (> 1 million) from one site
  # Because of high density this number was based on a relatively small area so could be overestimated, still v. large
  #mutate(across(contains('post_dens'), ~ ifelse(.x > 1000000, 500000, .x))) %>% 
  # Rescale values: centered on zero and rescaled by SD
  mutate(across(c(everything() & !contains('post')), scale)) %>%
  # Round the densities to whole numbers so they can be modeled as negative binomial
  mutate(across(c(contains('post_dens')),round)) %>%  
  as.data.frame()

# Different look at collinearity
z <- cor(select(rescaled_df, -contains('post_dens')), method = 'spearman')
z[lower.tri(z,diag=TRUE)] <- NA  #Prepare to drop duplicates and meaningless information
z <- as.data.frame(as.table(z))  #Turn into a 3-column table
z <- na.omit(z)  #Get rid of the junk we flagged above
z <- z[order(-abs(z$Freq)),] 
z[abs(z$Freq) >= 0.5,] 

# Reduced predictor list, based on ecological rationale to select between correlated vars:
# pre_basal_sp (except PICO), serot (only PICO), dist_unburned, canopy, dev_ne, vpd, OM

species_list <- list('LAOC','PICO','PSME','PIEN','ABLA','TSHE') # 'PIAB','TSTH'

#sp = species_list[[1]]

# This is a simple a-priori model selection procedure
# A set of ecologically meaningful variables was selected and checked for collinearity. 
species_models <- function(sp){
  
  print(paste("Running",sp))
  response <- paste0('post_dens_ha_',sp)
  basal <- paste0('pre_basal_',sp)

  if(sp == 'PICO'){
    model_form <- as.formula(
      paste(response, '~', paste0(c('serot','dist_unburned','vpd','dev_ne','OM'),
                                  collapse = '+')))
  } else {
    model_form <- as.formula(
      paste(response, '~', paste0(c(basal,'dist_unburned','vpd','dev_ne','OM'),
                                  collapse = '+')))
  }
 
  negbimodel <- glmmTMB(model_form, family = nbinom2, data = rescaled_df) 
  nullmodel <- glmmTMB(as.formula(paste0(response,'~1')), family = nbinom2, data = rescaled_df) 
  
  # Likelihood-ratio based R2
  # Test of explanatory power of full model compared to a null model
  r2stat <- attributes(MuMIn::r.squaredLR(negbimodel, nullmodel))[1]
  

  # Diagnostics from DHARMa - from vignette('DHARMa')
  testDispersion(negbimodel)
  testZeroInflation(negbimodel)
  simulationOutput <- simulateResiduals(fittedModel = negbimodel, plot = F)
  plot(simulationOutput)
  
  model_summ <- summary(negbimodel)
  results <- 
    list('cond' = model_summ$coefficients$cond) %>% #, 'zi' = model_summ$coefficients$zi
    map_df(~ as.data.frame(.x) %>% 
             rownames_to_column(var = 'term') %>%
             rename(estimate = Estimate, 
                    se = `Std. Error`, 
                    zval = `z value`, 
                    pval = `Pr(>|z|)`) %>% 
             mutate(across(c(estimate, se, zval, pval), round, 4)),
      .id = 'model') %>% 
    mutate(species = sp,
           r2_nag = r2stat,
           term = ifelse(term == !!basal, 'prefire_BA_sp',term))
    
  return(results)
}

density_sp_glm <- map_df(species_list, species_models)

terms_to_plot <- density_sp_glm %>% 
  filter(term != '(Intercept)') %>% 
  mutate(signif = ifelse(pval < 0.05, 1, 0)) %>% 
  mutate(term = ifelse(term %in% c('serot','prefire_BA_sp'), 'serot_BA', term),
         type = case_when(term %in% c('serot_BA','dist_unburned') ~ 'Seed sources', #'pre_dens_ha','pre_dbh_mean'
                          term %in% c('canopy','vpd','dev_ne') ~ 'Microclimate',
                          term == 'OM' ~ 'Soil'),
         term = factor(term, levels = c('OM','serot_BA','dist_unburned','vpd','dev_ne','canopy'),
                       labels = c('Soil OM','Serotiny or BA','Dist. low sev.','VPD','Aspect','Dead canopy')),
         species = factor(species, levels = c('PIEN','ABLA','PICO','LAOC','PSME','TSHE')), # 'PIAB','TSTH'
         estimate = ifelse(estimate < -10, -10, estimate),
         estimate = ifelse(estimate >  10, 10, estimate),
         ci_min = ifelse(estimate-(1.96*se) < -10, -11, estimate-(1.96*se)),
         ci_max = ifelse(estimate+(1.96*se) > 10, 11, estimate+(1.96*se))) 
# %>%
#   filter(species != 'THPL')

ggplot(terms_to_plot) +
  geom_hline(aes(yintercept = 0), color = 'grey50') +
  geom_pointrange(aes(x = term, 
                      y = estimate, 
                      ymin = ci_min, 
                      ymax = ci_max, 
                      fill = type,
                      alpha = signif),
                  position = position_dodge(width = .2), shape = 21, size = 0.5) +
  facet_wrap(~species, nrow = 1, scales = 'free_x') +
  scale_alpha_continuous('P-value', breaks = c(0,1), range = c(0.6,1), labels = c('>= 0.05','< 0.05')) +
  scale_fill_manual('Predictor type', values = c('#7570b3','#1b9e77','#d95f02')) +
  labs(y = 'Effect size (log-count)') +
  coord_flip() + #
  theme_bw(base_size = 10) +
  theme(axis.text = element_text(color = 'black', size = 8),
        axis.text.x = element_text(color = 'black', size = 6),
        axis.title.y = element_blank(),
        strip.background =  element_blank(),
        legend.position = 'bottom')

ggsave('effect_sizes.png', height = 2.5, width = 8, units = 'in', dpi = 600)







# Older averaging stuff that's not working for ZI models
# Create subset matrix that will exclude collinear terms, where abs. val. r > 0.5
cormat <- abs(cor(select(rescaled_df, -contains('post')), method = 'spearman')) <= 0.5
cormat[!lower.tri(cormat)] <- NA
# Need to change these names to work with glmmTMB term naming system
dimnames(cormat)[[1]] <- paste0('cond(',dimnames(cormat)[[1]],')')
dimnames(cormat)[[2]] <- paste0('cond(',dimnames(cormat)[[2]],')')


# This function is used in the calculation of QAIC: https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf
dfun <- function(object){with(object,sum((weights * residuals^2)[weights > 0])/df.residual)}
model_avg_fn <- function(sp){
  
  # String together various predictors
  response <- paste0('post_dens_ha_',sp)
  basal <- 'pre_basal_all'  # paste0(paste0(c('pre_basal_','pre_dens_ha_'),sp, collapse = '+'),'+pre_basal_all')
  veg <- paste0(c('dist_unburned'), collapse = '+') #,'shrub','canopy',
  climate <-  paste0(c('tmax','vpd'), # ,'precip_anom','tmax_anom','tmin_anom','vpd_anom'
                     collapse = '+') 
  topo <- paste0(c('dev_ne','tpi','OM'), collapse = '+') #'slope','tri','TN'
  
  # Global model formula, with all predictors
  model_form <- as.formula(paste(response, '~', 
                                 paste0(c(basal,veg,climate,topo), 
                                        collapse = '+')))
  
  glob_mod <- glmmTMB(model_form, family = nbinom2, data = rescaled_df)
  
    # glm(model_form,
    #               family = MASS::negative.binomial(link = "log", theta = 1),
    #               data = rescaled_df,
    #               na.action = 'na.fail')
    # 
  # Now perform dredge. Max # of predictors = 7. Rank by QIAC Exlcude based on cormat.
  # Only noticeable difference with QAIC is that is considers two models equally supported instead of 1.
  dredge_obj <- dredge(
    glob_mod, 
    m.lim = c(1,5),
    subset = cormat,
    rank = 'AICc')
    # ,
    # rank = 'QAIC', chat = dfun(glob_mod))
    # 
  
  if( length(which(dredge_obj[,'delta'] <= 2)) > 1 ){
    
    avg_obj <- model.avg(dredge_obj, subset = delta <= 2)
    
    conf_ints <- confint(avg_obj) %>%
      as.data.frame() %>%
      rownames_to_column(var = 'term') %>%
      rename(lower = `2.5 %`, upper = `97.5 %`)
    
    result <- avg_obj[['coefficients']][2,] %>%
      as.data.frame() %>%
      rownames_to_column(var = 'term') %>%
      rename(estimate = '.') %>%
      full_join(conf_ints) %>%
      mutate(signif = ifelse(lower*upper < 0, 0, 1),
             species = sp)
    result$term <- gsub(paste0('_',sp),"",result$term)
    result$term <- gsub('cond(.*)',"",result$term)
    
  }else{
    
    avg_obj <- get.models(dredge_obj, subset = delta <= 2)[[1]]
    
    conf_ints <- confint(avg_obj) %>%
      as.data.frame() %>%
      rownames_to_column(var = 'term') %>%
      rename(lower = `2.5 %`, upper = `97.5 %`)
    
    sum_obj <- summary(avg_obj)
    sum_obj$coefficients$cond
    
    result <- sum_obj$coefficients$cond %>%
      as.data.frame() %>%
      rownames_to_column(var = 'term') %>%
      rename(estimate = Estimate) %>%
      full_join(conf_ints) %>%
      mutate(signif = ifelse(lower*upper < 0, 0, 1),
             species = sp) %>% 
      select(term, estimate, lower, upper, signif, species)
    result$term <- gsub(paste0('_',sp),"",result$term)
  }
  
  return(result)
  return(dredge_obj)
}

species_avg_terms <- map_df(species_list, model_avg_fn) 

terms_to_plot <- species_avg_terms %>% 
  mutate(signif = as.integer(signif)) %>% 
  filter(term != '(Intercept)')

ggplot(terms_to_plot) +
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper, alpha = signif)) +
  facet_wrap(~species, nrow = 1) +
  scale_alpha_continuous(breaks = c(0,1), range = c(0.5,1)) +
  coord_flip(ylim = c(-10,5)) +
  theme_bw(base_size = 12)



# Partial R2 stuff for each term

basal_form <- as.formula(paste(response, '~', 
                               paste0(c('dist_unburned','serot','vpd','dev_ne'), 
                                      collapse = '+')))
dist_form <- as.formula(paste(response, '~', 
                              paste0(c('pre_basal_all','serot','vpd','dev_ne'), 
                                     collapse = '+')))
serot_form <- as.formula(paste(response, '~', 
                               paste0(c('pre_basal_all','dist_unburned','vpd','dev_ne'), 
                                      collapse = '+')))
vpd_form <- as.formula(paste(response, '~', 
                             paste0(c('pre_basal_all','dist_unburned','serot','dev_ne'), 
                                    collapse = '+')))
ne_form <- as.formula(paste(response, '~', 
                            paste0(c('pre_basal_all','dist_unburned','serot','vpd','dev_ne'), 
                                   collapse = '+')))

