import anthropic
import re
import pandas as pd
from io import StringIO
from pathlib import Path
import json

PROJECT_ROOT = Path(__file__).resolve().parent.parent

def ingredients_to_query_and_mass_df(csv_file):
    client = anthropic.Anthropic()

    with open(csv_file, 'r') as text_file:
        ingredients_csv_text = text_file.read()
    user_prompt = "<Ingredients>\n"+ingredients_csv_text+"</Ingredients>"
    print(user_prompt)

    with open(PROJECT_ROOT / 'llm_prompts' / 'convert_ingredient_to_search.txt', 'r') as text_file:
        system_prompt = text_file.read()

    response = client.beta.prompt_caching.messages.create(
        model="claude-3-5-sonnet-20240620",
        max_tokens=8192,
        temperature=0,
        system=[
        {
            "type": "text", 
            "text": system_prompt,
            "cache_control": {"type": "ephemeral"}
        }
        ],
        messages=[
            {"role": "user", "content": user_prompt},
            {"role": "assistant", "content": "<Thinking>"}
        ],
    )
    print(response)
    response_text = response.content[0].text
    csv_text = _extract_xml_tag(response_text, "Answer")
    data_frame = pd.read_csv(StringIO(csv_text))
    return data_frame

def pick_best_kroger_result(query, results_df):
    client = anthropic.Anthropic()

    filtered_results_df = results_df[['kroger_api_categories', 'kroger_api_description', 'kroger_api_pricing_unit']]
    kroger_results_text = filtered_results_df.to_csv()

    user_prompt = "<UserQuery>\n"+query+"\n</UserQuery>"+"\n<KrogerResults>\n"+"id"+kroger_results_text+"</KrogerResults>"
    print(user_prompt)

    with open(PROJECT_ROOT / 'llm_prompts' / 'pick_kroger_best_match.txt', 'r') as text_file:
        system_prompt = text_file.read()

    response = client.beta.prompt_caching.messages.create(
        model="claude-3-5-sonnet-20240620",
        max_tokens=8192,
        temperature=0,
        system=[
        {
            "type": "text", 
            "text": system_prompt,
            "cache_control": {"type": "ephemeral"}
        }
        ],
        messages=[
            {"role": "user", "content": user_prompt},
            {"role": "assistant", "content": "<Thinking>"}
        ],
    )
    print(response)
    response_text = response.content[0].text

    result_index = int(_extract_xml_tag(response_text, 'Product'))
    result_grams = _extract_xml_tag(response_text, 'Grams')

    if result_index == -1 or result_grams == 0:
        raise Exception("Kroger product match not found")

    result_kroger_api_upc = results_df.loc[result_index, 'kroger_api_upc']
    result_kroger_api_uri = results_df.loc[result_index, 'kroger_api_uri']
    result_kroger_api_local_price = results_df.loc[result_index, 'kroger_api_local_regular_price']
    result_kroger_api_categories = results_df.loc[result_index, 'kroger_api_categories']
    result_kroger_api_pricing_unit = results_df.loc[result_index, 'kroger_api_pricing_unit']

    return result_kroger_api_upc, result_grams, result_kroger_api_uri, result_kroger_api_local_price, result_kroger_api_categories, result_kroger_api_pricing_unit

def write_alternate_query_list(food_name):
    client = anthropic.Anthropic()

    user_prompt = "<IngredientName>\n"+food_name+"\n</IngredientName>"
    print(user_prompt)

    with open(PROJECT_ROOT / 'llm_prompts' / 'write_alternative_kroger_queries.txt', 'r') as text_file:
        system_prompt = text_file.read()

    response = client.beta.prompt_caching.messages.create(
        model="claude-3-5-sonnet-20240620",
        max_tokens=8192,
        temperature=0,
        system=[
        {
            "type": "text", 
            "text": system_prompt,
            "cache_control": {"type": "ephemeral"}
        }
        ],
        messages=[
            {"role": "user", "content": user_prompt},
            {"role": "assistant", "content": "<Thinking>"}
        ],
    )
    print(response)
    response_text = response.content[0].text

    alt_query_list = json.loads(_extract_xml_tag(response_text, 'Answer'))
    return alt_query_list

def _extract_xml_tag(llm_response, xml_tag):
    xml_tag_pattern = fr'<{xml_tag}>(.*?)</{xml_tag}>'
    matches = re.findall(xml_tag_pattern, llm_response, re.DOTALL)
    #Assume there is only one match
    clean_xml_content = matches[0].strip()
    return clean_xml_content