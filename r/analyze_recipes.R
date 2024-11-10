
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

animal_products <- 
  read_csv("data/tagged_veggie_recipes.csv")

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
           TRUE ~ product_local_price))

# Make sure product data is correct ----------------------------------
error_detect <- 
  ingredients_clean %>% 
  filter(!is.na(kroger_upc)) %>% 
  select(product_name, claude_product_grams_estimate, product_sold_by_unit) %>% 
  distinct() %>% 
  mutate(claude_ounces = claude_product_grams_estimate * 0.035274) %>% # convert claude's estimate to grams
  mutate(fl_oz = grepl("fl oz", product_sold_by_unit), # find fl_oz measures
         gal = grepl("gal", product_sold_by_unit), # gal
         pound = grepl("pound", product_sold_by_unit),
         lb = grepl("lb", product_sold_by_unit),
         count = grepl("ct", product_sold_by_unit),
         ounces = grepl("oz", product_sold_by_unit)) %>% 
  mutate(unknown = !lb & !gal & !count & !ounces & !pound & !fl_oz, 
         only_ounces = ounces & !count & !lb & !gal & !pound & !fl_oz, 
         only_count = count & !lb & !gal & !ounces & !pound & !fl_oz,
         very_tough = only_count | unknown, #These are products with no explicit mass or volume
         tough = very_tough | gal | fl_oz) #These are products without explicit mass

error_detect %>% 
  filter(tough == TRUE) %>% 
  select(product_name, claude_product_grams_estimate, product_sold_by_unit, claude_ounces) %>% 
  arrange(desc(claude_ounces)) %>% 
  print(n=500)

#pineapple, cucumber, and corn are inconsistent

#arrange the ingredients from estimated ounces high to low and check for issues
error_detect %>% 
  filter(ounces == TRUE) %>% 
  select(product_name, claude_product_grams_estimate, product_sold_by_unit, claude_ounces) %>% 
  arrange(desc(claude_ounces)) %>% 
  view()

error_detect %>% 
  filter(ounces == FALSE) %>% 
  select(product_name, claude_product_grams_estimate, product_sold_by_unit, claude_ounces) %>% 
  arrange(desc(claude_ounces)) %>% 
  view()

# This item seems to be wrong: private-selection-heritage-boneless-pork-shoulder-roast-natural-duroc-pork

# Make sure full ingredients data is correct ------------------------------
ingredients_clean %>% 
  mutate(product_price_per_g = price / claude_product_grams_estimate,
         ingredient_price = product_price_per_g * claude_ingredient_gram_estimate,
         bb_underspend = ingredient_price - recipe_ingredient_price) %>% 
  view()

# Create final ingredients dataset ----------------------------------------

fixed_grains_mass <- 
  bb_ingredients_query_grams %>%
  mutate(ingredient_gram_estimate = case_when(
    # Match cooked (but not uncooked) rice, excluding bulgur
    str_detect(recipe_ingredient_description, "(?<!un)cooked") &
      str_detect(recipe_ingredient_description, "rice") &
      !str_detect(recipe_ingredient_description, "bulgur") ~ claude_ingredient_gram_estimate/2.5,
    
    # Match cooked (but not uncooked) bulgur
    str_detect(recipe_ingredient_description, "(?<!un)cooked") &
      str_detect(claude_query_for_kroger, "bulgur") ~ claude_ingredient_gram_estimate/4.1,
    
    # Match cooked (but not uncooked) farro
    str_detect(recipe_ingredient_description, "(?<!un)cooked") &
      str_detect(claude_query_for_kroger, "farro") ~ claude_ingredient_gram_estimate/3,
    
    # Match cooked (but not uncooked) quinoa
    str_detect(recipe_ingredient_description, "(?<!un)cooked") &
      str_detect(claude_query_for_kroger, "quinoa") ~ claude_ingredient_gram_estimate/3.1,    
    
    # Match cooked (but not uncooked) mashed potatoes
    str_detect(claude_query_for_kroger, "mashed potatoes") ~ claude_ingredient_gram_estimate/5,
    
    # Match cooked (but not uncooked) lentils
    str_detect(recipe_ingredient_description, "(?<!un)cooked") &
      str_detect(claude_query_for_kroger, "lentil") ~ claude_ingredient_gram_estimate/3.1,
    
    # All other cases remain unchanged
    TRUE ~ claude_ingredient_gram_estimate
  ))


kroger_product_data <- 
  ingredients_to_kroger %>% 
  select(kroger_upc, product_grams, product_uri, product_local_price, product_categories, product_sold_by_unit) %>% 
  group_by(kroger_upc) %>% 
  summarise(product_grams = mean(product_grams),
            product_uri = first(product_uri),
            product_local_price = first(product_local_price),
            product_categories = first(product_categories),
            product_sold_by_unit = first(product_sold_by_unit)) %>% 
  distinct()

corrected_kroger_upc <- 
  ingredients_to_kroger %>% 
  mutate(fixed_kroger_upc = case_when(
    
    # Convert old fashioned bobs red mill oats to kroger oats
    str_detect(kroger_upc, "3997805155") ~ "1111076655.0",
    
    # Convert bobs red mill oats to kroger oats
    str_detect(kroger_upc, "3997805155") ~ "1111076655.0",
    
    # Convert ottogi cooked rice pack to dry rice
    str_detect(kroger_upc, "64517589041") ~ "1111084706.0",
    
    # Convert kroger cooked rice pack to dry rice
    str_detect(kroger_upc, "1111001950") ~ "1111087415.0",
    
    # Convert boiled eggs to eggs
    str_detect(kroger_upc, "74602520246") ~ "1111085724.0",
    
    # Ham hocks are NA
    str_detect(food_name, "smoked ham hocks") ~ "21386300000.0",
    
    # Chicken pieces should not be fried and frozen
    str_detect(kroger_upc, "3100011611.0") ~ "28334750000.0",
    
    # These are overly fancy bacon pieces
    str_detect(kroger_upc, "85672600787.0") ~ "26390250000.0",

    # All other cases remain unchanged
    TRUE ~ kroger_upc
  )) %>% 
  select(food_name, query, alternate_query_inbox, fixed_kroger_upc)
  
products_remapped <- 
  corrected_kroger_upc %>% 
  left_join(kroger_product_data, by = join_by(fixed_kroger_upc == kroger_upc))

kroger_corrected <- 
  products_remapped %>% 
  mutate(product_grams = case_when(product_uri == "/p/private-selection-heritage-boneless-pork-shoulder-roast-natural-duroc-pork/0021358400000?cid=dis.api.tpi_products-api_20240521_b:all_c:p_t:thalosfoodresearch-f" ~ 453.59200,
                                                   .default = product_grams))

# Correct recipe data -----------------------------------------------------
recipe_descriptions <- 
  recipe_descriptions %>% 
  # This recipe nutrition data was incorrectly read
  mutate(protein = if_else(recipe_name == "Roasted Turkey Breast with Stuffing", 66, protein),
         fiber = if_else(recipe_name == "Roasted Turkey Breast with Stuffing", 1, fiber),
         calories = if_else(recipe_name == "Roasted Turkey Breast with Stuffing", 552, calories),
         fat = if_else(recipe_name == "Roasted Turkey Breast with Stuffing", 18, fat),
         carbohydrates = if_else(recipe_name == "Roasted Turkey Breast with Stuffing", 33, carbohydrates),
         sodium = if_else(recipe_name == "Roasted Turkey Breast with Stuffing", 1722, sodium)) %>% 
  # This recipe has bad nutrition data
  mutate(protein = if_else(recipe_name == "Slow Cooker Chicken", 64, protein),
         fiber = if_else(recipe_name == "Slow Cooker Chicken", NA, fiber),
         calories = if_else(recipe_name == "Slow Cooker Chicken", 397, calories),
         fat = if_else(recipe_name == "Slow Cooker Chicken", 15, fat),
         carbohydrates = if_else(recipe_name == "Slow Cooker Chicken", 1.5, carbohydrates),
         sodium = if_else(recipe_name == "Slow Cooker Chicken", NA, sodium)) %>% 
  # This recipe has bad nutrition data
  filter(recipe_name != "Limbers") %>% 

# Merge up ----------------------------------------------------------------
merged_ingredients <- 
  fixed_grains_mass %>% 
  left_join(kroger_corrected, by = join_by(claude_query_for_kroger == food_name)) %>% 
  rename(kroger_query = query,
         claude_product_grams_estimate = product_grams)

ingredients_final <- 
  merged_ingredients %>% 
  mutate(product_name = str_extract(product_uri, "/p/([^/]+)") %>% str_remove("/p/"),
         kroger_upc = str_extract(product_uri, "\\d+(?=\\?)"),
         recipe_ingredient_price = gsub("\\$", "", recipe_ingredient_price),
         recipe_ingredient_price = as.double(recipe_ingredient_price),
         price = case_when(
           recipe_ingredient_price == 0 ~ 0,
           TRUE ~ product_local_price)) %>% 
  mutate(product_price_per_g = price / claude_product_grams_estimate,
         ingredient_price = product_price_per_g * claude_ingredient_gram_estimate,
         bb_underspend = ingredient_price - recipe_ingredient_price)

# Detect anomalies --------------------------------------------------------

ingredients_final %>% 
  distinct(claude_query_for_kroger, product_price_per_g) %>% 
  arrange(desc(product_price_per_g)) %>% 
  view()

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
  select(recipe_name, url, rating, number_of_ratings, prep_time, cook_time, total_calories, total_fat, total_carbs, total_protein, total_fiber, total_sodium) %>% 
  left_join(animal_products, by = join_by(recipe_name))

weight_lbs <- 175
recommended_g_protein <- 0.36 * weight_lbs
high_g_protein <- 0.75 * weight_lbs
calories_consumed <- 2500
min_protein_g_per_cal <- recommended_g_protein / calories_consumed
high_protein_g_per_cal <- high_g_protein / calories_consumed

recipes_with_cost <- 
  clean_recipes %>% 
  left_join(recipe_costs, by = join_by(recipe_name)) %>% 
  mutate(dollars_per_calorie = kroger_price / total_calories,
         dollars_per_2500_cal = 2500 * dollars_per_calorie,
         protein_g_per_cal = total_protein / total_calories,
         protein_g_per_100cal = protein_g_per_cal*100,
         protein_per_2500_cal = 2500 * protein_g_per_cal,
         sufficient_protein = ifelse(protein_g_per_cal > min_protein_g_per_cal, TRUE, FALSE),
         high_protein = ifelse(protein_g_per_cal > high_protein_g_per_cal, TRUE, FALSE),
         dollars_per_100g = 100*kroger_price / total_grams,
         fiber_g_per_100g = total_fiber / total_grams,
         calories_per_100g = total_calories / total_grams)


# Output csv --------------------------------------------------------------

final_recipe_table <- 
  recipes_with_cost %>% 
  relocate(c(url, recipe_name, dollars_per_2500_cal, protein_per_2500_cal, calories_per_100g, vegetarian, sufficient_protein, fiber_g_per_100g), .before = everything())

write_csv(final_recipe_table, "final_recipe_table.csv")

# Visualization -----------------------------------------------------------

model <- 
  lm(dollars_per_calorie ~ protein_g_per_cal, data = recipes_with_cost)

residuals(model)

recipes_with_cost %>% 
  ggplot(mapping= aes(x = protein_g_per_cal, y = dollars_per_calorie))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE)

recipes_with_cost %>% 
  ggplot(mapping= aes(x = total_fat/total_calories, y = dollars_per_calorie))+
  geom_point()

recipes_with_cost %>% 
  ggplot(mapping= aes(x = total_carbs/total_calories, y = dollars_per_calorie))+
  geom_point()

recipes_with_cost %>% 
  ggplot(mapping= aes(x = total_fiber/total_calories, y = dollars_per_calorie))+
  geom_point()


recipe_costs %>% 
  mutate(bb_underest = kroger_price - bb_price) %>% 
  view()

ingredients_to_kroger %>% 
  mutate(category_list = strsplit(product_categories, ",")) %>% 
  filter(map_lgl(category_list, ~ "Produce" %in% .x)) %>% 
  view()

ingredients_to_kroger %>% 
  mutate(category_list = strsplit(product_categories, ",")) %>% 
  count(product_categories) %>% 
  arrange(desc(n)) %>% 
  print(n=500)

ingredients_to_kroger %>% 
  filter(!is.na(kroger_upc))

# Further -----------------------------------------------------------------

#Substitute beyond beef and impossible beef for ground beef and see what happens

