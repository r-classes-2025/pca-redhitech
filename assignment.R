
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra)


top_speakers <- friends |> 
  count(speaker, sort = TRUE) |>
  slice_head(n = 6) |> 
  pull(speaker) |>
  as.character()


friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |>
  mutate(text = str_remove_all(text, "\\d+")) |>
  unnest_tokens(word, text) |>
  select(speaker, word)


friends_tf <- friends_tokens |>
  count(speaker, word) |>
  group_by(speaker) |>
  arrange(desc(n), word) |>  
  slice_head(n = 500) |>    
  mutate(tf = n / sum(n)) |>  
  ungroup() |>
  select(speaker, word, tf)    


friends_tf_wide <- friends_tf |> 
  arrange(speaker) |>       
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |>
  column_to_rownames("speaker")


set.seed(123)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)


pca_fit <- prcomp(friends_tf_wide, center = TRUE, scale. = TRUE)


q <- fviz_pca_biplot(pca_fit,  
                     geom = c("text"),
                     select.var = list(cos2 = 20),
                     habillage = as.factor(km.out$cluster),
                     col.var = "steelblue",
                     alpha.var = 0.3,
                     repel = FALSE,
                     ggtheme = theme_minimal()) +
  theme(legend.position = "none")


q
