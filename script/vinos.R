library(FactoMineR)
library(patchwork)
data(wine)

vino <- wine

# 1er grupo: especifica el origen de los vinos, incluida las
# variables del label y el soil. 2 columnas

# 2do grupo: variables continuas que describen el olor antes
# de agitar. 5 columnas

# 3er grupo: variables continuas que describen la inspeccion
# visual de los vinos. 3 columnas

# 4to grupo: variables continuas sobre el olor despues de 
# agitar. 10 columnas

# 5to grupo: sabor de los vinos. 9 columnas

# 6to grupo: juicio general. 2 columnas

names(vino)

vino_mfa <- MFA(vino, 
               group = c(2, 5, 3, 10, 9, 2), 
               type = c("n", "s", "s", "s", "s", "s"),
               name.group = c("origen","olor.ants","visual",
                              "olor.desps", "sabor","juicio"),
               num.group.sup = c(1, 6),
               graph = FALSE)



fviz_screeplot(vino_mfa)

get_eig(vino_mfa)

fviz_mfa_var(vino_mfa, "group")

p1 <- fviz_contrib(vino_mfa, "group", axes = 1)

p2 <- fviz_contrib(vino_mfa, "group", axes = 2)

p1 + p2


fviz_mfa_var(vino_mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE)

#1er component: harmoniosos, 2do olor spicy
fviz_mfa_var(vino_mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")

fviz_contrib(vino_mfa, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco")

fviz_contrib(vino_mfa, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")


fviz_mfa_var(vino_mfa, "quanti.var", col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))


fviz_mfa_ind(vino_mfa, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, invisible = "quali.var")


fviz_mfa_ind(vino_mfa, partial = c("1DAM", "1VAU", "2ING")) 

fviz_mfa_axes(vino_mfa)

