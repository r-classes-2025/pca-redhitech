# 1. отберите 6 главных персонажей (по количеству реплик)
# сохраните как символьный вектор
top_speakers <- friends |> 
  # ваш код здесь
  count(speaker, sort = TRUE) |>
  slice_max(n, n = 6) |>
  pull(speaker) |>
  as.character()

# 2. отфильтруйте топ-спикеров, 
# токенизируйте их реплики, удалите из них цифры
# столбец с токенами должен называться word
# оставьте только столбцы speaker, word
friends_tokens <- friends |> 
  # ваш код здесь
  filter(speaker %in% top_speakers) |>
  mutate(text = str_remove_all(text, "\\d+")) |>
  unnest_tokens(word, text) |>
  select(speaker, word)

# 3. отберите по 500 самых частотных слов для каждого персонажа
# посчитайте относительные частотности для слов
friends_tf <- friends_tokens |>
  # ваш код здесь
  count(speaker, word) |>
  group_by(speaker) |>
  slice_max(n, n = 500, with_ties = FALSE) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

# 4. преобразуйте в широкий формат; 
# столбец c именем спикера превратите в имя ряда, используя подходящую функцию 
friends_tf_wide <- friends_tf |> 
  # ваш код здесь
  select(speaker, word, prop) |>
  pivot_wider(names_from = word, values_from = prop, values_fill = 0) |>
  column_to_rownames("speaker")

# 5. установите зерно 123
# проведите кластеризацию k-means (k = 3) на относительных значениях частотности (nstart = 20)
# используйте scale()

set.seed(123)
# ваш код здесь
km.out <- # ваш код здесь
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)


# 6. примените к матрице метод главных компонент (prcomp)
# центрируйте и стандартизируйте, использовав аргументы функции
pca_fit <- # ваш код здесь
pca_fit <- prcomp(friends_tf_wide, center = TRUE, scale. = TRUE)

# 7. Покажите наблюдения и переменные вместе (биплот)
# в качестве геома используйте текст (=имя персонажа)
# цветом закодируйте кластер, выделенный при помощи k-means
# отберите 20 наиболее значимых переменных (по косинусу, см. документацию к функции)
# сохраните график как переменную q

q <- # ваш код здесь


q <- fviz_pca_biplot(pca_fit,  geom = c("text"),
                select.var = list(cos2 = 20),
                habillage = as.factor(km.out$cluster),
                col.var = "steelblue",
                alpha.var = 0.3,
                repel = TRUE,
                ggtheme = theme_minimal()) +
  theme(legend.position = "none")
