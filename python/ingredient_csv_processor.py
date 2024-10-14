import pandas as pd
import os

def split_ingredients_csv(input_file, output_dir, chunk_size=50):
    for i, chunk in enumerate(pd.read_csv(input_file, chunksize=chunk_size)):
        output_file = os.path.join(output_dir, f'ingredients_chunk_{i}.csv')
        chunk.to_csv(output_file, index=False)

def merge_mass_query_csv_files(files_directory, output_file_path):
    file_paths = [os.path.join(files_directory, file) for file in os.listdir(files_directory)]
    dfs = [pd.read_csv(file_path) for file_path in file_paths]
    merged_df = pd.concat(dfs, ignore_index=True)
    merged_df.to_csv(output_file_path, index=False)