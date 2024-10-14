
# Libraries ---------------------------------------------------------------
library(tidyverse)

# Load data ---------------------------------------------------------------

ingredient_budget_bytes_prices <- 
  read_csv("data/budget_bytes_ingredients_prices.csv")

ingredients_to_query_and_grams <- 
  read_csv("data/full_processed_ingredients.csv")

distinct_ingredients <- 
  read_csv("data/budget_bytes_ingredients.csv")

ingredients_to_kroger <- 
  read_csv("data/ingredients_kroger_upc.csv")

recipe_descriptions <- 
  read_csv("data/all_budget_bytes_recipes.csv")

# Full ingredients clean data ---------------------------------------------------

budget_bytes_ingredients <- 
  ingredient_budget_bytes_prices %>% 
  rename(recipe_ingredient_description = ingredient,
         recipe_ingredient_price = cost) %>% 
  select(recipe_name, recipe_ingredient_description, recipe_ingredient_price) %>% 
  left_join(distinct_ingredients, by = join_by(recipe_ingredient_description == ingredient)) %>% 
  rename(ingredient_description_id = rowid)

bb_ingredients_query_grams <- 
  budget_bytes_ingredients %>% 
  left_join(ingredients_to_query_and_grams, by = join_by(ingredient_description_id == rowid)) %>% 
  rename(claude_query_for_kroger = query,
         claude_ingredient_gram_estimate = grams) %>% 
  select(!ingredient_description_id)

bb_ingredients_kroger_products <- 
  bb_ingredients_query_grams %>% 
  left_join(ingredients_to_kroger, by = join_by(claude_query_for_kroger == food_name)) %>% 
  rename(kroger_query = query,
         claude_product_grams_estimate = product_grams)

ingredients_clean <- 
  bb_ingredients_kroger_products %>% 
  mutate(product_name = str_extract(product_uri, "/p/([^/]+)") %>% str_remove("/p/"),
         kroger_upc = str_extract(product_uri, "\\d+(?=\\?)"),
         recipe_ingredient_price = gsub("\\$", "", recipe_ingredient_price),
         recipe_ingredient_price = as.double(recipe_ingredient_price),
         price = case_when(
           recipe_ingredient_price == 0 ~ 0,
           TRUE ~ product_local_price
         ))

ingredients_final <- 
  ingredients_clean %>% 
  mutate(product_price_per_g = price / claude_product_grams_estimate,
         ingredient_price = product_price_per_g * claude_ingredient_gram_estimate,
         bb_underspend = ingredient_price - recipe_ingredient_price)

# Detect anomalies --------------------------------------------------------

ingredients_to_kroger %>% 
  count(query) %>% 
  arrange(desc(n))


# Recipe analysis ---------------------------------------------------------

recipe_costs <- 
  ingredients_final %>% 
  group_by(recipe_name) %>% 
  summarise(bb_price = sum(recipe_ingredient_price),
            total_grams = sum(claude_ingredient_gram_estimate),
            kroger_price = sum(ingredient_price))

clean_recipes <- 
  recipe_descriptions %>% 
  mutate(total_calories = serving_count * calories,
         total_fat = serving_count * fat,
         total_carbs = serving_count * carbohydrates,
         total_protein = serving_count * protein,
         total_fiber = serving_count * fiber,
         total_sodium = serving_count * sodium) %>% 
  select(recipe_name, rating, number_of_ratings, prep_time, cook_time, total_calories, total_fat, total_carbs, total_protein, total_fiber, total_sodium)

weight_lbs <- 175
recommended_g_protein <- 0.36 * weight_lbs
high_g_protein <- 0.75 * weight_lbs
calories_consumed <- 2500
min_protein_g_per_cal <- recommended_g_protein / calories_consumed
high_protein_g_per_cal <- high_g_protein / calories_consumed

recipes_with_cost <- 
  clean_recipes %>% 
  left_join(recipe_costs, by = join_by(recipe_name)) %>% 
  mutate(cost_per_calorie = bb_price / total_calories,
         cost_per_2500_cal = 2500 * cost_per_calorie,
         protein_g_per_cal = total_protein / total_calories,
         protein_per_2500_cal = 2500 * protein_g_per_cal,
         sufficient_protein = ifelse(protein_g_per_cal > min_protein_g_per_cal, TRUE, FALSE),
         high_protein = ifelse(protein_g_per_cal > high_protein_g_per_cal, TRUE, FALSE),
         cost_per_gram = kroger_price / total_grams)

# Visualization -----------------------------------------------------------


