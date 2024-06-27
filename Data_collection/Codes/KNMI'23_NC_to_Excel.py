#This code was created with the use of the KNMI'23 modelled time series instruction file and the instructions in the pcse_notebooks repository of Allard de Wit
import pandas as pd
import xarray as xr
import numpy as np
from pathlib import Path
import logging
import os
import sys

# Initialize logging
logging.basicConfig()
logger = logging.getLogger(__name__)
logger.setLevel(os.environ.get("LOG_LEVEL", logging.INFO))

# Define variables
variables = ['rsds', 'tasmin', 'tasmax', 'hurs', 'sfcwind', 'pr', 'tas']
year = 2150
scen = 'H'
variant = 'n'
scenario = f'{scen}{variant}_{year}'
data_directory = Path("C:/Users/Suzanne van Esch/KNMI23")

# Define geographic bounds for part of the Netherlands
lat_min, lat_max = 51.5, 51.5
lon_min, lon_max = 5.9, 5.9

# Initialize an empty DataFrame to store data for all dates
all_data_df = pd.DataFrame()

# Loop through each variable and gather data
for var in variables:
    file = data_directory / f'{var}_{scenario}_interp.nc'

    # Check if the file exists
    if not file.is_file():
        logger.error(f"File {file} does not exist.")
        sys.exit(1)

    # Open the dataset and extract data for the specified region
    ds = xr.open_dataset(file)
    ds_region = ds[var].sel(lat=slice(lat_min, lat_max), lon=slice(lon_min, lon_max))

    # Convert to DataFrame, select ensemble
    df = ds_region.sel(ens=8).to_dataframe().reset_index()

    # Apply transformation to 'rsds' values
    if var == 'rsds':
        df[var] = df[var] * 86.4

    # Keep only relevant columns and rename the variable column
    df = df[['time', var]].rename(columns={'time': 'Date', var: var})

    # Merge with the existing data
    if all_data_df.empty:
        all_data_df = df
    else:
        all_data_df = pd.merge(all_data_df, df, on='Date', how='outer')

# Sort the DataFrame by Date
all_data_df = all_data_df.sort_values('Date').reset_index(drop=True)

# Ensure Date column is in datetime format
all_data_df['Date'] = pd.to_datetime(all_data_df['Date'])

# Apply hurs transformation using tas values
if 'hurs' in all_data_df.columns and 'tas' in all_data_df.columns:
    all_data_df['hurs'] = all_data_df.apply(
        lambda row: row['hurs'] * (0.61094 * np.exp((17.625 * row['tas']) / (row['tas'] + 243.04)))/100,
        axis=1
    )

# Drop the 'tas' column as it is not needed in the final output
all_data_df = all_data_df.drop(columns=['tas'])


# Save the DataFrame to an Excel file
output_file = "C:/Users/Suzanne van Esch/PycharmProjects/pythonProject/par/weather/"f'8_{scen}{variant}_{year}.xlsx'
all_data_df.to_excel(output_file, index=False)
logger.info(f"Data saved to {output_file}")

if __name__ == "__main__":
    pass
