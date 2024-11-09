from ingredient_csv_processor import split_ingredients_csv, merge_mass_query_csv_files
from llm_calls import ingredients_to_query_and_mass_df, pick_best_kroger_result, write_alternate_query_list, test_for_animal_products
from kroger_api_handler import KrogerAPIHandler
import os
import pandas as pd
from pathlib import Path
import os

PROJECT_ROOT = Path(__file__).resolve().parent.parent
os.chdir(PROJECT_ROOT)

def main():
    input_csv = PROJECT_ROOT / 'data' / 'budget_bytes_ingredients.csv'
    chunked_ingredients_folder = PROJECT_ROOT / 'data' / 'chunked_ingredients_data'
    
    split_ingredients_csv(input_csv, chunked_ingredients_folder)

    chunked_ingredients_file_paths = [os.path.join(chunked_ingredients_folder, filename) for filename in os.listdir(chunked_ingredients_folder)]

    # for ingredient_chunk in chunked_ingredients_file_paths:
    #     print(ingredient_chunk)
    #     processed_df_name = ingredient_chunk.replace('chunked_ingredients_data/ingredients_chunk_', 'processed_ingredients_chunks/processed_ingredients_chunk_')
    #     processed_df = ingredients_to_query_and_mass_df(ingredient_chunk)
    #     processed_df.to_csv(processed_df_name, index=False)
    #     print(processed_df_name)

    merge_mass_query_csv_files(PROJECT_ROOT / 'data' / 'processed_ingredients_chunks', PROJECT_ROOT / 'data' / 'full_processed_ingredients.csv')

    # _write_blank_kroger_upc_csv()

    # _search_kroger_for_all_queries()

    _retry_searches_on_failed_queries()

    _tag_vegetarian_recipes()

def _write_blank_kroger_upc_csv():
    full_processed_ingredients = pd.read_csv(PROJECT_ROOT / 'data' / 'full_processed_ingredients.csv')
    unique_query_array = full_processed_ingredients['query'].unique()
    ingredients_kroger_upc = pd.DataFrame(unique_query_array, columns=['food_name'])
    ingredients_kroger_upc['query'] = ingredients_kroger_upc['food_name']
    ingredients_kroger_upc['alternate_query_inbox'] = pd.NA
    ingredients_kroger_upc['kroger_upc'] = pd.NA
    ingredients_kroger_upc['product_grams'] = pd.NA
    ingredients_kroger_upc['product_uri'] = pd.NA
    ingredients_kroger_upc['product_local_price'] = pd.NA
    ingredients_kroger_upc['product_categories'] = pd.NA
    ingredients_kroger_upc['product_sold_by_unit'] = pd.NA
    ingredients_kroger_upc.to_csv(PROJECT_ROOT / 'data' / 'ingredients_kroger_upc.csv', index=False)

def _search_kroger_for_all_queries():
    starting_ingredients_kroger_upc = pd.read_csv(PROJECT_ROOT / 'data' / 'ingredients_kroger_upc.csv')
    edited_ingredients_kroger_upc = starting_ingredients_kroger_upc

    # Search kroger for each query, then pick the best result with LLM
    for index, row in starting_ingredients_kroger_upc.iterrows():
        if pd.isna(row['kroger_upc']):
            query = row['query']
            try:
                print(f"Searching for {query}")
                kroger_handler = KrogerAPIHandler(os.getenv('KROGER_CLIENT_ID'), os.getenv('KROGER_API_KEY'))
                upc, grams, uri, price, categories, sold_by = pick_best_kroger_result(query, kroger_handler.query_to_df_of_results(query))

                edited_ingredients_kroger_upc.loc[index, ['kroger_upc', 'product_grams', 'product_uri', 'product_local_price', 'product_categories', 'product_sold_by_unit']] = upc, grams, uri, price, categories, sold_by

            except Exception as e:
                print(f"Error processing {query}: {str(e)}")
                edited_ingredients_kroger_upc.loc[index, ['kroger_upc', 'product_grams', 'product_uri', 'product_local_price', 'product_categories', 'product_sold_by_unit']] = pd.NA

            print("Editing the ingested csv")
            edited_ingredients_kroger_upc.to_csv(PROJECT_ROOT / 'data' / 'ingredients_kroger_upc.csv', index=False)

def _retry_searches_on_failed_queries():
    ingredients_to_kroger_products_df = pd.read_csv(PROJECT_ROOT / 'data' / 'ingredients_kroger_upc.csv')

    for index, row in ingredients_to_kroger_products_df.iterrows():
        if pd.isna(row['kroger_upc']): 
            # Iterate over all rows where we were not able to find a product match
            food_name = ingredients_to_kroger_products_df.loc[index, 'food_name']
            alternate_queries = write_alternate_query_list(food_name)
            ingredients_to_kroger_products_df.loc[index, 'alternate_query_inbox'] = str(alternate_queries)

            for alt_q in alternate_queries:
                try:
                    kroger_handler = KrogerAPIHandler(os.getenv('KROGER_CLIENT_ID'), os.getenv('KROGER_API_KEY'))

                    upc, grams, uri, price, categories, sold_by = pick_best_kroger_result(food_name, kroger_handler.query_to_df_of_results(alt_q))

                    ingredients_to_kroger_products_df.loc[index, ['query', 'kroger_upc', 'product_grams', 'product_uri', 'product_local_price', 'product_categories', 'product_sold_by_unit']] = alt_q, upc, grams, uri, price, categories, sold_by

                    break
                except Exception as e:
                    print(f"Error processing {alt_q}: {str(e)}")

            print("Editing the ingested csv")
            ingredients_to_kroger_products_df.to_csv(PROJECT_ROOT / 'data' / 'ingredients_kroger_upc.csv', index=False)

def _write_tagged_veggie_recipes():
    # Get the recipe dataset
    ingredients_df = pd.read_csv(PROJECT_ROOT / 'data' / 'budget_bytes_ingredients_prices.csv')

    # Make wider by moving ingredient entries into a list
    recipes_df = ingredients_df.groupby('recipe_name')['ingredient'].agg(list).reset_index()
    recipes_df = recipes_df.rename(columns={'ingredient': 'ingredients'})
    recipes_df['vegetarian'] = pd.NA
    recipes_df['vegan'] = pd.NA

    recipes_df.to_csv(PROJECT_ROOT / 'data' / 'tagged_veggie_recipes.csv', index=False)
    print(recipes_df)

def _tag_vegetarian_recipes():
    # Get the recipe dataset
    recipes_df = pd.read_csv(PROJECT_ROOT / 'data' / 'tagged_veggie_recipes.csv')

    # First, explicitly convert the columns to boolean type
    recipes_df['vegetarian'] = recipes_df['vegetarian'].astype('boolean')
    recipes_df['vegan'] = recipes_df['vegan'].astype('boolean')
    
    for index, row in recipes_df.iterrows():
        if pd.isna(row['vegetarian']):
            is_vegetarian, is_vegan = test_for_animal_products(row['recipe_name'], row['ingredients'])

            try:
                recipes_df.loc[index, ['vegetarian', 'vegan']] = is_vegetarian, is_vegan

            except Exception as e:
                print(f"Error classifying animal products in {row['recipe_name']}: {str(e)}")

            recipes_df.to_csv(PROJECT_ROOT / 'data' / 'tagged_veggie_recipes.csv', index=False)

if __name__ == "__main__":
    _tag_vegetarian_recipes()