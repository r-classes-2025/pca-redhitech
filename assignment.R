# --- БИБЛИОТЕКИ ---
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra)

# 1. Топ спикеров
top_speakers <- friends |> 
  count(speaker, sort = TRUE) |>
  slice_head(n = 6) |> 
  pull(speaker) |>
  as.character()

# 2. Токенизация и очистка
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |>
  mutate(text = str_remove_all(text, "\\d+")) |> # Удаляем цифры
  unnest_tokens(word, text) |>
  filter(word != "") |>                           # ВАЖНО: Удаляем пустые токены (как просили в подсказке)
  select(speaker, word)

# 3. Частотность (САМОЕ ВАЖНОЕ ИСПРАВЛЕНИЕ)
friends_tf <- friends_tokens |>
  count(speaker, word) |>
  group_by(speaker) |>
  # Сначала считаем TF (пока есть ВСЕ слова), а не после фильтрации
  mutate(tf = n / sum(n)) |>     
  # Теперь сортируем для slice_head (число + алфавит для стабильности)
  arrange(desc(n), word) |>      
  # И только теперь отрезаем верхушку
  slice_head(n = 500) |>         
  ungroup() |>
  select(speaker, word, tf)

# 4. Широкий формат
friends_tf_wide <- friends_tf |> 
  arrange(speaker) |>            # Сортируем спикеров по алфавиту
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |>
  column_to_rownames("speaker")

# 5. K-means
set.seed(123)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)

# 6. PCA
pca_fit <- prcomp(friends_tf_wide, center = TRUE, scale. = TRUE)

# 7. Биплот
q <- fviz_pca_biplot(pca_fit,  
                geom = c("text"),               
                select.var = list(cos2 = 20),
                habillage = as.factor(km.out$cluster),
                col.var = "steelblue",
                alpha.var = 0.3,
                repel = FALSE,                  # FALSE для прохождения теста
                ggtheme = theme_minimal()) +
  theme(legend.position = "none")

# Вывод
q
