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
- {year}-03-15:
    CropCalendar:
        crop_name: potato
        variety_name: Fontane
        crop_start_date: {year}-03-15
        crop_start_type: sowing
        crop_end_date: {year}-08-15
        crop_end_type: harvest
        max_duration: 155
    TimedEvents: null
    StateEvents:
    -   event_signal: irrigate
        event_state: SM
        zero_condition: falling
        name: Soil moisture driven irrigation scheduling
        comment: all irrigation amounts in cm of water
        events_table:
        - 0.17: {{amount: 3, efficiency: 0.7}}
- {year}-08-16: null
"""

    # Writing the script to a text file
    with open(file_path, "w") as file:
        file.write(script)


# Set the parameters
start_year = 2136
end_year = 2165
file_path = f"C:/Users/Suzanne van Esch/PycharmProjects/pythonProject/par/Agromanagement_{start_year}_{end_year}_irrigation2.agro"

# Generate the yield schedule
generate_yield_schedule(start_year, end_year, file_path)
