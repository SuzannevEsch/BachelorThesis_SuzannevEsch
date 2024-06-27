import sys
from pathlib import Path
import pandas as pd
import pcse

# Print versions
print("This notebook was built with:")
print(f"python version: {sys.version}")
print(f"PCSE version: {pcse.__version__}")

# File paths and parameters
data_dir = Path.cwd() / "par"
weather_dir = Path("C:/Users/Suzanne van Esch/PycharmProjects/pythonProject/par/weather_H2150/")
output_dir = Path.cwd() / "output"
output_excel_file = output_dir / "H2150_NI3_TWSO_WP_IRRTOT_PRTOT.xlsx"

# Function to run the simulation for a given weather file
def run_simulation(weather_file):
    from pcse.input import YAMLCropDataProvider, CABOFileReader, YAMLAgroManagementReader, ExcelWeatherDataProvider
    from pcse.base import ParameterProvider
    from pcse.models import Wofost73_WLP_CWB

    # Load crop, soil, and site data
    cropd = YAMLCropDataProvider(Wofost73_WLP_CWB)
    cropd.set_active_crop('potato', 'Fontane')
    soild = CABOFileReader(data_dir / "SOIL_V.txt")
    sited = CABOFileReader(data_dir / "site/SITE_V_H2150.txt")
    parameters = ParameterProvider(cropdata=cropd, soildata=soild, sitedata=sited)

    # Load agromanagement and weather data
    agromanagement = YAMLAgroManagementReader(data_dir / "Agromanagement_2136_2165_no_irrigation3.agro")
    weatherdataprovider = ExcelWeatherDataProvider(weather_file)

    # Initialize and run the WOFOST model
    wofsim = Wofost73_WLP_CWB(parameters, weatherdataprovider, agromanagement)
    wofsim.run_till_terminate()

    # Get the simulation output
    output_data = wofsim.get_output()

    # Create a DataFrame from the output data
    df_results = pd.DataFrame(output_data)
    df_results.set_index("day", inplace=True)
    df_results.index = pd.to_datetime(df_results.index)

    # Calculate differences for RAINT and TOTIRR
    for year in range(2136, 2166):
        if f"{year}-05-14" in df_results.index.strftime('%Y-%m-%d'):
            for i in range(1, len(df_results)):
                start_date = pd.Timestamp(f"{year}-05-15")
                end_date = pd.Timestamp(f"{year}-10-15")
                if start_date <= df_results.index[i] <= end_date:
                    df_results.iloc[i, df_results.columns.get_loc('RAINT')] -= df_results.loc[f"{year}-05-14", 'RAINT']
                    df_results.iloc[i, df_results.columns.get_loc('TOTIRR')] -= df_results.loc[f"{year}-05-14", 'TOTIRR']

    # Filter data for the date 10-20 for every year
    filtered_data = df_results[df_results.index.strftime('%m-%d') == '10-15']

    # Strip any leading/trailing spaces from column names
    filtered_data.columns = filtered_data.columns.str.strip()

    # Calculate WP
    if 'RAINT' in filtered_data.columns and 'TOTIRR' in filtered_data.columns:
        filtered_data.loc[:, 'WP'] = filtered_data.apply(
            lambda row: row['TWSO'] / (row['RAINT'] + (row['TOTIRR'] / 0.7)), axis=1
        )
    else:
        filtered_data.loc[:, 'WP'] = filtered_data['RAINT']

    # Add total irrigation
    filtered_data.loc[:, 'IRRTOT'] = filtered_data.apply(
        lambda row: row['TOTIRR'], axis=1
    )
    # Add total precipitation
    filtered_data.loc[:, 'PRTOT'] = filtered_data.apply(
        lambda row: row['RAINT'], axis=1
    )

    # Select only the data for the TWSO, WP, IRRTOT and PRTOT variable
    TWSO_WP_IRRTOT_PRTOT_data = filtered_data[['TWSO', 'WP', 'IRRTOT', 'PRTOT']]

    return TWSO_WP_IRRTOT_PRTOT_data

# Find all weather files in the weather directory
weather_files = list(weather_dir.glob("*.xlsx"))

# List to store the results
all_results = []

# Run the simulation for each weather file
for weather_file in weather_files:
    scenario = "_".join(weather_file.stem.split("_")[1:])
    ensemble = weather_file.stem.split("_")[0]

    TWSO_WP_IRRTOT_PRTOT_data = run_simulation(weather_file)

    # Add columns for scenario and ensemble
    TWSO_WP_IRRTOT_PRTOT_data['scenario'] = scenario
    TWSO_WP_IRRTOT_PRTOT_data['ensemble'] = ensemble

    # Append the results to the list
    all_results.append(TWSO_WP_IRRTOT_PRTOT_data)

# Combine all the results into a single DataFrame
combined_results = pd.concat(all_results).reset_index(drop=True)

# Save the results to a single sheet in the Excel file
combined_results.to_excel(output_excel_file, sheet_name="Sheet1", index=False)

print(f"Simulation results have been written to {output_excel_file}")
