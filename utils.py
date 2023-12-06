# utils.py
import io
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
from path import Path
import os

def plot_map(lat, lon, population, area, 
             lat_0=37.5, lon_0=-119, width=1E6, height=1.2E6,
             title="Map", colorbar_label=r'$\log_{10}({\rm population})$',
             legend_points=[100, 300, 500]):
    """
    Plots a map with scatter points. 
    Points are colored and sized based on population and area respectively.

    Parameters:
    lat (array-like): Latitudes of the points.
    lon (array-like): Longitudes of the points.
    population (array-like): Population data used for coloring the points.
    area (array-like): Area data used for sizing the points.
    lat_0, lon_0 (float): Latitude and longitude of the map center.
    width, height (float): Width and height of the map.
    title (str): Title of the plot.
    colorbar_label (str): Label for the colorbar.
    legend_points (list): List of area sizes for the legend.
    """

    # Draw the map background
    fig = plt.figure(figsize=(8, 8))
    m = Basemap(projection='lcc', resolution='h', 
                lat_0=lat_0, lon_0=lon_0,
                width=width, height=height)
    m.shadedrelief()
    m.drawcoastlines(color='gray')
    m.drawcountries(color='gray')
    m.drawstates(color='gray')

    # Scatter city data, with color reflecting population
    # and size reflecting area
    m.scatter(lon, lat, latlon=True,
              c=np.log10(population), s=area,
              cmap='Reds', alpha=0.5)

    # Create colorbar and legend
    plt.colorbar(label=colorbar_label)
    plt.clim(3, 7)

    # Make legend with dummy points
    for a in legend_points:
        plt.scatter([], [], c='k', alpha=0.5, s=a,
                    label=str(a) + ' km$^2$')
    plt.legend(scatterpoints=1, frameon=False,
               labelspacing=1, loc='lower left')

    plt.title(title)
    plt.show()


def plot_map_basic(lat, lon, lat_0, lon_0, title):
    """
    Plots a map with scatter points. 
    Points are colored and sized based on population and area respectively.

    Parameters:
    lat (array-like): Latitudes of the points.
    lon (array-like): Longitudes of the points.
    lat_0, lon_0 (float): Latitude and longitude of the map center.
    width, height (float): Width and height of the map.
    title (str): Title of the plot.
    colorbar_label (str): Label for the colorbar.
    legend_points (list): List of area sizes for the legend.
    """    
    fig = plt.figure(figsize=(8, 8))
    m = Basemap(projection='lcc', resolution='h', 
                lat_0=lat_0, lon_0=lon_0,
                width=1E6, height=1.2E6)
    m.shadedrelief()
    m.drawcoastlines(color='gray')
    m.drawcountries(color='gray')
    m.drawstates(color='gray')

    # Scatter city data
    m.scatter(lon, lat, latlon=True, c='blue', s=50, alpha=0.5)

    plt.title(title)
    plt.show()

    
# read in shapefile

from pathlib import Path
import fiona
import geopandas

def read_shapefile(file_path):
    """
    Read a zipped shapefile and return a GeoDataFrame.

    Parameters:
    - file_path (str): The path to the zipped shapefile.

    Returns:
    - geopandas.GeoDataFrame: A GeoDataFrame containing the shapefile data.
    """
    try:
        # Read in shapefile
        with open(file_path, 'rb') as file:
            zipshp = io.BytesIO(file.read())

        with fiona.BytesCollection(zipshp.read()) as src:
            crs = src.crs
            gdf = geopandas.GeoDataFrame.from_features(src, crs=crs)

        return gdf

    except FileNotFoundError:
        print(f"Error: File not found at {file_path}")
        return None

    except Exception as e:
        print(f"An error occurred: {e}")
        return None

# Example usage
if __name__ == "__main__":
    # Replace 'C:/file_path' with the actual path to your zipped shapefile
    file_path = "C:/file_path"
    gdf = read_shapefile(file_path)

    if gdf is not None:
        print("Shapefile loaded successfully.")
        # Perform further processing or visualization with the GeoDataFrame
    else:
        print("Failed to load shapefile.")
