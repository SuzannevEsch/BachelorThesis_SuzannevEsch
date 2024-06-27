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
weather_file = Path("C:/Users/Suzanne van Esch/PycharmProjects/pythonProject/par/weather_reference/1991-2020_Reference.xlsx")
output_dir = Path.cwd() / "output"
output_excel_file = output_dir / "1991-1992_Reference_NI1_TWSO_WP_IRRTOT_PRTOT.xlsx"

# Ensure output directory exists
output_dir.mkdir(parents=True, exist_ok=True)

from pcse.fileinput import YAMLCropDataProvider, CABOFileReader, YAMLAgroManagementReader, ExcelWeatherDataProvider
from pcse.input import YAMLCropDataProvider, CABOFileReader, YAMLAgroManagementReader, ExcelWeatherDataProvider
from pcse.base import ParameterProvider
from pcse.models import Wofost73_WLP_CWB

# Load crop, soil, and site data
cropd = YAMLCropDataProvider(Wofost73_WLP_CWB)
cropd.set_active_crop('potato', 'Fontane')
soild = CABOFileReader(data_dir / "SOIL_V.txt")
sited = CABOFileReader(data_dir /"site/SITE_V_Reference.txt")
parameters = ParameterProvider(cropdata=cropd, soildata=soild, sitedata=sited)

# Load agromanagement and weather data
agromanagement = YAMLAgroManagementReader(data_dir / "Agromanagement_1991_2020_no_irrigation1.agro")
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
for year in range(1991, 2021):
    if f"{year}-04-14" in df_results.index.strftime('%Y-%m-%d'):
        for i in range(1, len(df_results)):
            start_date = pd.Timestamp(f"{year}-04-15")
            end_date = pd.Timestamp(f"{year}-09-15")
            if start_date <= df_results.index[i] <= end_date:
                df_results.iloc[i, df_results.columns.get_loc('RAINT')] -= df_results.loc[f"{year}-04-14", 'RAINT']
                df_results.iloc[i, df_results.columns.get_loc('TOTIRR')] -= df_results.loc[f"{year}-04-14", 'TOTIRR']

# Filter data for the date 10-20 for every year
filtered_data = df_results[df_results.index.strftime('%m-%d') == '09-15']

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

# Concatenate all the data into a single DataFrame
final_data = pd.DataFrame(TWSO_WP_IRRTOT_PRTOT_data)

# Save the DataFrame to an Excel file
final_data.to_excel(output_excel_file, sheet_name='Simulation Results')

print(f"Simulation results saved to {output_excel_file}")
