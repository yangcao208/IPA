library(tidyverse)

hfpef_metab = readxl::read_excel('C:/Users/yangc/OneDrive/Desktop/metabolomics-HFpEF.xlsx')

pca = prcomp(hfpef_metab[, -1] %>% as.matrix() %>% t, scale. = T)

pca2 = pca$x %>% as.data.frame() %>% rownames_to_column('sample') %>%
  mutate(diet = rep(c('Chow', 'HFpEF'), each=8))

var_expl = pca$sdev^2 / sum(pca$sdev^2)

ggplot(pca2, aes(x=PC1, y=PC2, color=diet)) +
  geom_point(size=3) +
   xlab(sprintf('PC1: (%2.1f %%)', var_expl[1] * 100)) +
  ylab(sprintf('PC2: (%2.1f %%)', var_expl[2] * 100)) +
  stat_ellipse(geom='polygon', alpha=0.3, aes(fill=diet)) +
  scale_color_manual(values=c('Chow'='#619CFF', 'HFpEF'='#F8766D')) +
  scale_fill_manual(values=c('Chow'='#619CFF', 'HFpEF'='#F8766D')) +
  theme_bw() +
  theme(axis.title = element_text(size=24),
        axis.text = element_text(size=18),
        legend.text = element_text(size=18),
        legend.title=element_blank())

ggsave('C:/Users/yangc/OneDrive/Desktop/metabolomics-HFpEF/hfpef_metabolomics_pca_20210609.pdf', width=8, height=7)


# exclude Y7 --------------------------------------------------------------

pca_no7 = prcomp(hfpef_metab[, -c(1, 8)] %>% as.matrix() %>% t, scale. = T)

pca2 = pca_no7$x %>% as.data.frame() %>% rownames_to_column('sample') %>%
  mutate(diet = rep(c('Chow', 'HFpEF'), c(7,8)))

var_expl = pca_no7$sdev^2 / sum(pca_no7$sdev^2)

ggplot(pca2, aes(x=PC1, y=PC2, color=diet)) +
  geom_point(size=3) +
    xlab(sprintf('PC1: (%2.1f %%)', var_expl[1] * 100)) +
  ylab(sprintf('PC2: (%2.1f %%)', var_expl[2] * 100)) +
  stat_ellipse(geom='polygon', alpha=0.3, aes(fill=diet)) +
  scale_color_manual(values=c('Chow'='#619CFF', 'HFpEF'='#F8766D')) +
  scale_fill_manual(values=c('Chow'='#619CFF', 'HFpEF'='#F8766D')) +
  theme_bw() +
  theme(axis.title = element_text(size=24),
        axis.text = element_text(size=18),
        legend.text = element_text(size=18),
        legend.title=element_blank())

ggsave('C:/Users/yangc/OneDrive/Desktop/hfpef_metabolomics_pca_20210609_no_Y7.pdf', width=8, height=7)
