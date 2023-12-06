#!/usr/bin/env python
# coding: utf-8

# [Census API Datasets]('https://api.census.gov/data.html')
# * ACS/Tiger Mapping files: https://www2.census.gov/geo/tiger/TIGER_DP/
# * 2022 Relationship Files: https://www2.census.gov/geo/tiger/TIGER2022/

# In[1]:


import pandas 
import json
import requests
import numpy
import os
import sys
import geopandas
import janitor
from path import Path


# In[2]:


# path configuration
working_directory = Path.getcwd()

inputs_path = working_directory / 'inputs'
outputs_path = working_directory / 'outputs'


# In[3]:


# export function
def to_file(callback, desired_name, extension='csv'):
    current_files = sorted(outputs_path.files(desired_name + '*.' + extension))
    if current_files:
        last_file = current_files[-1]
        os.remove(last_file)
    final_name = '{}.{}'.format(desired_name, extension)
    callback(outputs_path / final_name)


# In[4]:


def _geo_merge(frame,geo_frames_dictionary,geo):
    if isinstance(frame.columns, pandas.core.index.MultiIndex):
        frame.columns = [
            '-'.join(x)
            for x in frame.columns.values
        ]
    current_geo = geo_frames_dictionary[geo]
    with_geo = current_geo.merge(frame, left_index=True,right_index=True).drop('',errors='ignore')
    return with_geo


# In[5]:


# capture geoid codes from url
geoid_codes = 'https://transition.fcc.gov/oet/info/maps/census/fips/fips.txt'

# read in url and retain fips codes and names, set as string objects and index
state_fips = pandas.read_fwf(geoid_codes,skiprows=16,nrows=53,names=['FIPS Code','Name'],dtype={'FIPS Code': 'str'})
county_fips = pandas.read_fwf(geoid_codes,skiprows=73,names=['FIPS Code','Name'],dtype={'FIPS Code': 'str'})

# Define a function to apply str.title()
def title_col(df, column_name):
    df[column_name] = df[column_name].str.title()

# apply function and rename with appropriate geos
title_col(state_fips, 'Name')
title_col(county_fips, 'Name')

# rename column fips codes to appropriate geos

state_fips.rename(columns={'FIPS Code':'state'},inplace=True)
county_fips.rename(columns={'FIPS Code':'county'},inplace=True)


# In[6]:


# make dictionaries
state_dict = state_fips.set_index('state')['Name'].to_dict()
county_dict = county_fips.set_index('county')['Name'].to_dict()


# #### Upload Geos -------

# In[7]:


# set mna crossings
mna_crossings_path =  Path(os.path.join(r"C:\Users\nanzawi\OneDrive - HDR, Inc\Data_Science\MODOT_RR\inputs\MoDOT_MNA_Crossings.xlsx"))

# grab shape files to merge to - if you want to grab from api
mo_blocks_shapefile = geopandas.read_file("https://www2.census.gov/geo/tiger/TIGER2022/BG/tl_2022_29_bg.zip")

# or if personal folder, grab and loop through
import glob

all_features = []

for geojson_file in glob.glob(os.path.join(inputs_path, 'Census*.geojson')):
    with open(geojson_file, 'r') as file:
        geojson_data = json.load(file)
        if 'features' in geojson_data:
            all_features.extend(geojson_data['features'])
            
merged_geojson = {
    'type': 'FeatureCollection',
    'features': all_features
}

output_file = 'merged.geojson'
with open(output_file, 'w') as outfile:
    json.dump(merged_geojson, outfile)


# In[9]:


# read_file 
merged_blocks = geopandas.read_file('merged.geojson').clean_names()


# ### Example API

# In[12]:


base_url = "https://api.census.gov/data"
dataset = "acs/acs5"
year = "2021"
variables = "B01001_001E"  # Population variable
state = "29"  # MO FIPS code  
counties = ["037","013","217","097","145","011"] # county FIPS codes for Jasper, Bates, Cass, Barton, Vernon
block_group = "*"  # All tracts within the county
api_key = "84c7d937e5b6038afeb17bd5615eceef179ff9a3"


# In[13]:


data_per_county = {}

for county in counties:
    endpoint = f"{base_url}/{year}/{dataset}?get={variables}&for=block%20group:{block_group}&in=state:{state}&in=county:{county}&key={api_key}"

    response = requests.get(endpoint)
    if response.status_code == 200:
        data = response.json()
        data_per_county[county] = data
        # Process or handle data for each county here
        print(f"Data fetched for county {county}")
    else:
        print(f"Error fetching data for county {county}. Status code: {response.status_code}")

# You can process or aggregate data for each county within this loop.


# In[14]:


# Convert the fetched data into pandas DataFrames for each county, skipping the first row
dataframes = {county: pandas.DataFrame(data[1:], columns=data[0]) for county, data in data_per_county.items()}

# Concatenate DataFrames
merged_dataframe = pandas.concat(dataframes.values(), keys=dataframes.keys())

display(
     merged_dataframe.head(),
     merged_dataframe.shape
)


# In[15]:


# create block group from fips codes to geoid
slim_df = merged_dataframe.copy()

slim_df['geoid'] = slim_df['state'] + slim_df['county'] + slim_df['tract'] + slim_df['block group']


# In[16]:


# make geograpihes a string
slim_df[['state','county','tract']] = slim_df[['state','county','tract']].astype('str')


# In[17]:


# add prefix to county name
slim_df['county'] = slim_df['state'] + slim_df['county']

# append  dictionary
slim_df = slim_df.replace({'county':county_dict,
                 'state':state_dict})

# change table name
slim_df.rename(columns={'B01001_001E':'total_population'},inplace= True)


# In[18]:


# view
slim_df.head()


# In[41]:


# merge slim_df with spatial files on geoid
# geo_blockgroups = mo_blocks_shapefile.merge(slim_df, left_on='GEOID',right_on='geoid')


# In[19]:


# You now make it for those that intersect the MNA - created with one mile buffer around each point
mna_crossings = pandas.read_excel(mna_crossings_path)

# view
mna_crossings.head()


# In[20]:


# make mna points spatial
crs= '32610'

points = geopandas.GeoDataFrame(
    mna_crossings, crs=crs, geometry=geopandas.points_from_xy(mna_crossings.Longitude, mna_crossings.Latitude)
)


# In[65]:


merged_blocks = merged_blocks.to_crs(points.crs)


# In[24]:


buffer_miles = 1  # Define the buffer distance in miles

# Calculate the buffer distance in degrees (approximation: 1 degree ≈ 69 miles)
buffer_degrees = buffer_miles / 69.0

# Buffer the points in degrees
points['geometry'] = points.geometry.buffer(buffer_degrees)


# In[25]:


# view these dataframes
display(
     points.head(),
     merged_blocks.head()
)


# In[26]:


# view

base = merged_blocks.plot(color='white', edgecolor='black')

points.plot(ax=base, marker='o', color='red', markersize=5);


# In[27]:


# calculate weighted population density

merged_blocks['weighted_population'] = merged_blocks['total_population'] * merged_blocks['total_population_per_square_mile']
merged_blocks['weighted_population_density'] = merged_blocks['weighted_population'] / merged_blocks['area_square_miles_']


# In[45]:


# create areas of intersection

df_is = geopandas.overlay(merged_blocks, points, how='intersection')

# drop unnneeded columns
to_drop = ['id', 'shid', 'prioritydatasources','geometry']
df_is = pandas.DataFrame(df_is).drop(to_drop, axis=1)


# In[47]:


# merge for analysis purposes
flat_file = df_is.groupby(['Zone Name','Zone ID','label','Address']).agg({
    'total_population_per_square_mile': 'sum',
    'total_population':'sum',
    'area_square_miles_':'sum',
    'weighted_population': 'sum',
    'weighted_population_density': 'mean'  # or any other aggregation function
}).reset_index()


# In[56]:


merged_zones = df_is.groupby(['Zone Name','Zone ID','Latitude','Longitude']).agg({
    'total_population_per_square_mile': 'mean',
    'total_population':'mean',
    'area_square_miles_':'mean',
    'weighted_population': 'mean',
    'weighted_population_density': 'mean'  # or any other aggregation function
}).reset_index()


# In[58]:


merged_zones.columns = merged_zones.columns.str.replace('total','avg').str.replace('weighted','avg_weighted').str.replace('area','avg_area')


# In[59]:


merged_zones


# In[60]:


to_file(merged_zones.to_csv,'MoDOTMNA_ZoneCrossingsPopulationStats')


# In[123]:


# Create a Pandas Excel writer using the ExcelWriter method
excel_writer = pandas.ExcelWriter('MoDOTMNA_ZoneCrossingsPopulationStats.xlsx', engine='xlsxwriter')

# Write the DataFrames to the ExcelWriter
df_list = [flat_file, merged_zones]
for i, df in enumerate(df_list):
    df.to_excel(excel_writer, sheet_name=f'df_{i}', index=False)

# Close the ExcelWriter to save the Excel spreadsheet
excel_writer.close()

