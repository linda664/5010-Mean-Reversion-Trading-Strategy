import requests
import pandas as pd

# --- Part 1: Fetch Data from FRED API ---
api_key = 'd99c14e32b9782a291500adfade501f4'
base_url = 'https://api.stlouisfed.org/fred/'

# Define the series IDs for the indicators
series_ids = {
    'CBOE Gold ETF VIX Index': 'VIXCLS',
    'Real Gross Domestic Product': 'GDPC1',
    'Consumer Loans, All Commercial Banks': 'CLSACBW027SBOG',
    '10-Year Treasury Bond Yield': 'GS10',
    'Consumer Confidence': 'UMCSENT',
    'Federal Funds Rate': 'FEDFUNDS',
    'US Current Account': 'BOPGSTB',
    'US Dollar Index': 'DTWEXBGS',
    'S&P 500': 'SP500',
    'NASDAQ Composite Index': 'NASDAQCOM',
    'Unemployment Rate': 'UNRATE',
    'PPI (Producer Price Index)': 'PPIACO',
    'CPI (Consumer Price Index)': 'CPIAUCSL',
    'Existing Home Sales': 'EXHOSLUSM495S',
    'All Employees, Total Nonfarm': 'PAYEMS'
}

# Define the start and end dates
start_date = '2015-04-17'
end_date = pd.to_datetime('today').strftime('%Y-%m-%d')  # Use the current date as the end date

# Initialize a list to hold DataFrames for each indicator
data_frames = []

# Fetch data from the FRED API for each indicator
for indicator, series_id in series_ids.items():
    params = {
        'series_id': series_id,
        'api_key': api_key,
        'file_type': 'json',
        'observation_start': start_date,
        'observation_end': end_date
    }

    try:
        # Make the API request
        response = requests.get(base_url + 'series/observations', params=params)
        response.raise_for_status()  # Ensure we catch any issues with the request

        # Parse the response JSON data
        data = response.json()

        # Check if data is available for the indicator
        if 'observations' in data:
            df = pd.DataFrame(data['observations'])
            if 'date' in df.columns and 'value' in df.columns:
                df['date'] = pd.to_datetime(df['date'])
                df['value'] = pd.to_numeric(df['value'], errors='coerce')
                df = df.set_index('date')['value'].rename(indicator)  # Set date as index and rename the column
                data_frames.append(df)
        else:
            print(f"No observations found for {indicator}")
    except Exception as e:
        print(f"Error fetching {indicator}: {str(e)}")

# Combine all the data into a single DataFrame
df_all = pd.concat(data_frames, axis=1)

# Sort the DataFrame by date, forward-fill missing data, and drop rows with all missing values
df_all = df_all.sort_index().ffill().dropna()

# Save the historical data to a CSV file
df_all.to_csv('economic_indicators_historical.csv')

# Print success message and preview the first 5 rows of the data
print("\nSuccessfully fetched and saved historical data. First 5 rows:")
print(df_all.head())


