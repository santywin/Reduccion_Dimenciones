library(rrcov)
library(dplyr)
library(tidyr)

?PcaCov

mt2 <- mtcars %>%
  select(mpg, disp, hp, drat, wt, qsec)

pca_clas<- PcaClassic(mt2, scale = TRUE)

pca_rob <- PcaCov(mt2, scale=TRUE)

dist_rob <- pca.distances(pca_rob, mt2, rankMM(mt2))

tibble(sco=dist_rob@sd, ort=dist_rob@od) %>%
  ggplot(aes(sco, ort)) + geom_point() +
  geom_hline(yintercept = dist_rob@cutoff.od) +
  geom_vline(xintercept = dist_rob@cutoff.sd)

