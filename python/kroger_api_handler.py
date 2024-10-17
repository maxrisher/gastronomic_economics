import requests
import pandas as pd

class KrogerAPIHandler:
    def __init__(self, client_id, api_key):
        self.api_key = api_key
        self.client_id = client_id
        self.base_url = "https://api.kroger.com/v1/"
        self.kroger_access_token = None
        self.results_json = None

    def get_kroger_access_token(self):
        url = f"{self.base_url}connect/oauth2/token"
        headers = {"Content-Type": "application/x-www-form-urlencoded"}
        data = {
            "grant_type": "client_credentials",
            "scope": "product.compact"
        }
        response = requests.post(url, headers=headers, data=data, auth=(self.client_id, self.api_key))
        self.kroger_access_token = response.json()['access_token']

    def search_products(self, query, limit=50, location_id='62000123'):
        self.get_kroger_access_token()

        url = f"{self.base_url}products"
        params = {
            "filter.term": query,
            "filter.limit": limit,
            "filter.locationId": location_id,
        }
        headers = {
            "Accept": "application/json",
            "Authorization": f"Bearer {self.kroger_access_token}"
        }
        response = requests.get(url, params=params, headers=headers)
        self.results_json = response.json()
    
    def query_to_df_of_results(self, query):
        self.search_products(query)

        data = []
        for product in self.results_json['data']:
            row = {
                'kroger_api_product_id': product.get('productId', ''),
                'kroger_api_upc': product.get('upc', ''),
                'kroger_api_uri': product.get('productPageURI', ''),
                'kroger_api_brand': product.get('brand', ''),
                'kroger_api_categories': ', '.join(product.get('categories', [])),
                'kroger_api_country_origin': product.get('countryOrigin', ''),
                'kroger_api_description': product.get('description', ''),
                'kroger_api_pricing_unit': product['items'][0].get('size', '') if product.get('items') else '',
                'kroger_api_temperature_indicator': product.get('temperature', {}).get('indicator', '')
            }

            row['kroger_api_local_regular_price'] = product['items'][0].get('price', {}).get('regular', '')
            row['kroger_api_national_regular_price'] = product['items'][0].get('nationalPrice', {}).get('regular', '')

            data.append(row)

        results_df = pd.DataFrame(data)

        # filter out products with no listed price
        results_w_price = results_df[results_df['kroger_api_local_regular_price'] != '']

        return results_w_price
    