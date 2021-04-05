
#load libraries

library(ggplot2)
library(reshape2)
library(forcats)
library(stringr)
library(dplyr)
library(tidymodels)
library(textrecipes)
library(textfeatures)
library(glmnet)
library(themis)
library(stopwords)

#Cargar data
booksReviews<- read.csv2("Tutorials/AmzReviews.csv")

Clean_String <- function(string){
  # minúscula
  temp <- tolower(string)
  # Remover todo lo que no sea número o letra 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # remover espacios extra
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  
  return(temp)
  
}

# Aplicar la función a los comentarios
booksReviews$comments <- Clean_String(booksReviews$comments)

set.seed(1234) # Asegurar siempre una misma semilla aleatoria.

#Reclasificar las 5 categorías de stars en 2 categorías
#booksReviews ya tiene el proceso de limpieza de datos

reviewClass <- booksReviews %>%
  mutate(class = factor(if_else(stars <= 3,"Not Great", "Great")),
         len= str_length(comments)
         )

#Realizar la partición de las muestras

reviews_split <- initial_split(reviewClass,prop=.7)

reviews_train <- training(reviews_split)
reviews_test <- testing(reviews_split)

# Setear la receta del modelo a utilizar

reviews_recipe <- recipe(class ~ comments+len, 
                         data = reviews_train)

#Aplicar los pasos de procesamiento de datos

reviews_recipeProcessed <- reviews_recipe %>%
 # step_mutate(comments_raw = comments) %>%
 # step_textfeature(comments_raw) %>%
 # update_role(all_numeric(),new_role = "predictor") %>% 
  step_text_normalization(comments) %>% 
  step_tokenize(comments) %>%
  step_stopwords(comments, keep = FALSE) %>%
  step_untokenize(comments) %>%
  step_tokenize(comments, token = "ngrams", 
                options = list(n = 2, n_min = 1)) %>%
  step_tokenfilter(comments, max_tokens = 200) %>%
  step_tfidf(comments)  %>%
  step_upsample(class) 

#Setear el workflow para trabajar el modelo de Machine Learning

reviews_wf <- workflow() %>%
  add_recipe(reviews_recipeProcessed)

# Especificación del modelo

rlasso_spec <-  logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")


#Ajustar el modelo con los datos

lasso_wf <- reviews_wf %>%
  add_model(rlasso_spec)

# Creamos un grid para entrenar los parametros adicionales del modelo
lambda_grid <- grid_random(penalty(), size = 20)

#Seteamos semilla aleatoria y creamos los subsets de la validación cruzada

set.seed(123)
reviews_folds <- vfold_cv(reviews_train,v=5)

set.seed(2020)
lasso_grid <- tune_grid(lasso_wf,
                        resamples = reviews_folds,
                        grid = lambda_grid,
                        control = control_resamples(save_pred = TRUE),
                        metrics = metric_set(f_meas, recall, precision)
)

best_f <- lasso_grid %>%
  select_best("f_meas")

#Entrenamos el modelo final con los valores del mejor modelo de entrenamiento
final_lasso <- finalize_workflow(lasso_wf, best_f) %>%
  fit(reviews_train)


review_final <- last_fit(final_lasso, 
                         split=reviews_split,
                         metrics = metric_set(f_meas, recall, precision)
)

# save environment

save.image(file="Tutorials/T5Data.RData")


# Observamos métricas del modelo evaluado en datos de prueba

lasso_grid %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  facet_wrap(~.metric) +
  scale_x_log10() +
  theme_minimal()


lasso_grid %>%
  collect_metrics()

review_final %>%
  collect_metrics()

preds<- review_final %>%
  collect_predictions()


reviewClass$comments[10]
comment<- "the story was good, however the book is bad."
len<- str_length(comment)

new_comment <- tribble(~comments,~len,comment,len)
new_comment

prediction<-predict(final_lasso, new_data = new_comment)


paste0("el resultado para el comentario ","'",new_comment$comments,"'","es: ",
       prediction$.pred_class)


reviewClass %>%
  group_by(class) %>%
  summarise(m=median(len))
