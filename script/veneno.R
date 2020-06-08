library(factoextra)
library(FactoMineR)
library(dplyr)
library(tidyr)
library(patchwork)

data(poison)

vene <- poison

summary(vene)


as_tibble(vene) %>% 
  select_if(is.factor) %>% 
  gather(preg, resp) %>% 
  count(preg, resp) %>% 
  ggplot(aes(resp, n)) + geom_col() + 
  facet_grid(~preg, scales = "free_x")

vene_mca <- MCA(vene[, 5:15], graph = FALSE)
vene_mca
get_eig(vene_mca)

fviz_screeplot(vene_mca)

fviz_mca_biplot(vene_mca)

get_mca_var(vene_mca)

fviz_mca_var(vene_mca, choice = "mca.cor")

fviz_mca_var(vene_mca, choice = "mca.cor")

fviz_mca_var(vene_mca, repel = TRUE)

fviz_mca_var(vene_mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

fviz_cos2(vene_mca, choice = "var", axes = 1:2)


p1 <- fviz_contrib(vene_mca, choice = "var", axes = 1, top = 15)
p2 <- fviz_contrib(vene_mca, choice = "var", axes = 2, top = 15)

p1 + p2


fviz_mca_var(vene_mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)


fviz_mca_ind(vene_mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_mca_ind(vene_mca, 
             label = "none",
             habillage = "Vomiting",  
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence"
             ) 

fviz_ellipses(vene_mca, c("Vomiting", "Fever"),
              geom = "point")


vene_mca <- MCA(vene, ind.sup = 53:55, 
               quanti.sup = 1:2, quali.sup = 3:4,  graph=FALSE)


fviz_mca_biplot(vene_mca, repel = TRUE)
fviz_mca_var(vene_mca, choice = "mca.cor",
             repel = TRUE)

