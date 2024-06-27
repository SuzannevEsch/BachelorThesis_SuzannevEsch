import os


# Define the function to generate the yield schedule script
def generate_yield_schedule(start_year, end_year, file_path):

    # Initializing the script content
    script = """
AgroManagement:
"""
    # Repeating the pattern for each year from start_year to end_year
    for year in range(start_year, end_year + 1):
        script += f"""
- {year}-01-01:
    CropCalendar:
        crop_name: potato
        variety_name: Fontane
        crop_start_date: {year}-03-15
        crop_start_type: sowing
        crop_end_date: {year}-08-15
        crop_end_type: harvest
        max_duration: 155
    TimedEvents: null
    StateEvents: null
- {year}-08-16: null
"""

    # Writing the script to a text file
    with open(file_path, "w") as file:
        file.write(script)


# Set the parameters
start_year = 2086
end_year = 2115
file_path = f"C:/Users/Suzanne van Esch/PycharmProjects/pythonProject/par/Agromanagement_{start_year}_{end_year}_no_irrigation2.agro"

# Generate the yield schedule
generate_yield_schedule(start_year, end_year, file_path)
