from ingredient_csv_processor import split_ingredients_csv, merge_mass_query_csv_files
from llm_calls import ingredients_to_query_and_mass_df, pick_best_kroger_result, write_alternate_query_list
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

def _write_blank_kroger_upc_csv():
    full_processed_ingredients = pd.read_csv(PROJECT_ROOT / 'data' / 'full_processed_ingredients.csv')
    unique_query_array = full_processed_ingredients['query'].unique()
    ingredients_kroger_upc = pd.DataFrame(unique_query_array, columns=['food_name'])
    ingredients_kroger_upc['query'] = ingredients_kroger_upc['food_name']
    ingredients_kroger_upc['alternate_query_inbox'] = []
    ingredients_kroger_upc['kroger_upc'] = pd.NA
    ingredients_kroger_upc['product_grams'] = pd.NA
    ingredients_kroger_upc['product_uri'] = pd.NA
    ingredients_kroger_upc['product_local_price'] = pd.NA
    ingredients_kroger_upc.to_csv(PROJECT_ROOT / 'data' / 'ingredients_kroger_upc.csv', index=False)

def _fix_zero_result_queries():
    kroger_searches = pd.read_csv(PROJECT_ROOT / 'data' / 'ingredients_kroger_upc.csv')
    kroger_searches_edit = kroger_searches

    queries_to_update = [
        #old llm generated query, new human corrected query
        ('coleslaw mix', 'coleslaw kit'),
        ('ground beef 15% fat', 'ground beef 80%'),
        ('macaroni noodles', 'dry macaroni'),
        ('aquafaba', 'canned chickpeas'),
        ('5 spice powder', 'five spice powder'),
        ('canned tuna', 'canned tuna'),
        ('cooked farro', 'ready rice'),
        ('melted butter', 'butter'),
        ('fresh lime juice', 'lime juice'),
        ('grated fresh ginger', 'ginger root'),
        ('cooked black beans', 'canned black beans'),
        ('neutral oil', 'vegetable oil'),
        ('ripe bananas', 'bananas'),
        ('small onion', 'onion'),
        ('celery stalk', 'celery'),
        ('powdered ginger', 'ground ginger'),
        ('broccoli stem', 'broccoli'),
        ('cold butter', 'butter'),
        ('cod steaks', 'cod fillet'),
        ('canned Albacore tuna', 'canned albacore tuna'),
        ('quick cooking grits', 'instant grits'),
        ('smoked ham hocks', 'smoked bacon'),
        ('Mexican Chorizo', 'chorizo'),
        ('frozen black eyed peas', 'frozen peas blackeye'),
        ('frozen onion bell pepper celery mix', 'frozen mirepoix'),
        ('Mexican chorizo', 'chorizo')
    ]
    'sunflower seeds'
    'harissa'
    'ground chipotle'
    ''
    for old_query, new_query in queries_to_update:
        _replace_llm_kroger_query(kroger_searches_edit, old_query, new_query)

    kroger_searches_edit.to_csv(PROJECT_ROOT / 'data' / 'ingredients_kroger_upc.csv', index=False)

def _replace_llm_kroger_query(query_df, llm_generated_kroger_query, new_kroger_query):
    mask = query_df['llm_generated_query'] == llm_generated_kroger_query
    query_df.loc[mask, ['query', 'kroger_upc', 'product_grams', 'product_uri' , 'product_local_price']] = [new_kroger_query, pd.NA, pd.NA, pd.NA, pd.NA,]

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
                upc, grams, uri, price = pick_best_kroger_result(query, kroger_handler.query_to_df_of_results(query))

                edited_ingredients_kroger_upc.loc[index, ['kroger_upc', 'product_grams', 'product_uri', 'product_local_price']] = upc, grams, uri, price

            except Exception as e:
                print(f"Error processing {query}: {str(e)}")
                edited_ingredients_kroger_upc.loc[index, ['kroger_upc', 'product_grams', 'product_uri', 'product_local_price']] = pd.NA

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

                    upc, grams, uri, price = pick_best_kroger_result(food_name, kroger_handler.query_to_df_of_results(alt_q))

                    ingredients_to_kroger_products_df.loc[index, ['query', 'kroger_upc', 'product_grams', 'product_uri', 'product_local_price']] = alt_q, upc, grams, uri, price

                    break
                except Exception as e:
                    print(f"Error processing {alt_q}: {str(e)}")

            print("Editing the ingested csv")
            ingredients_to_kroger_products_df.to_csv(PROJECT_ROOT / 'data' / 'ingredients_kroger_upc.csv', index=False)

if __name__ == "__main__":
    main()