import os, sys, csv
from pathlib import Path
from qgis import processing
#from qgis.core import QgsRasterLayer, QgsCoordinateReferenceSystem
import time
from osgeo import ogr

# remove file if it exists
def remove_if_exists(directory_or_filepath) -> None:
    print(f'Removing:{directory_or_filepath}')
    
    if os.path.isfile(directory_or_filepath):
        try:
            os.remove(directory_or_filepath)
        except OSError as e:
            print(f'Could not remove file: {directory_or_filepath}')
            print(e)
    elif os.path.isdir(directory_or_filepath):
        if not os.rmdir(directory_or_filepath):
            print(f'Failed to remove directory: {directory_or_filepath}')

# clear all layers
def clear_all_layers() -> None:
    #Remove all layers from project            
    QgsProject.instance().clear()
    # Refresh map canvas
    iface.mapCanvas().refresh

# create function to load GLDAS.nc4 files as raster layers
def create_gldas_raster_layers(filePath):
    surftemp_uri = f'NETCDF:"{filePath}":AvgSurfT_tavg' 
    soilmoist_uri = f'NETCDF:"{filePath}":SoilMoist_S_tavg'
    
    surftempraster = QgsRasterLayer(surftemp_uri, 'surftempraster')
    soilmoistraster = QgsRasterLayer(soilmoist_uri, 'soilmoistraster')

    if not surftempraster.isValid():
        print("Surf Temp Raster Layer Failed To Load!!!")
        
    if not soilmoistraster.isValid():
        print("Soil Moisture Raster Layer Failed To Load!!!")
   
    return surftempraster, soilmoistraster

    
# create function to load sample sites that correspond with raster bands at specific time points
def create_vector_layer(filePath):
    uri = f'file:///{filePath}?delimiter=,&xField=Lon&yField=Lat'
    vector_layer = QgsVectorLayer(uri, 'sampleSite', 'delimitedtext')
    
    if not vector_layer.isValid():
        print('Point layer could not be created!!!')
    
    return vector_layer

def add_raster_projections(file_path, gldas_data):
    # specifies the _GLDAS.nc4 files
    gldas_file_path = os.path.join(file_path, gldas_data)
    # extract surface temp and soil moisture from .nc4 files and make them rasters
    print(gldas_file_path)
    surftempraster, soilmoistraster = create_gldas_raster_layers(gldas_file_path)
    # name each raster based on band type + unique name and save as a .tif
    
    # add projections to raster layers, and re-sample them using bilinear interpolation
    surftemp_image_path = os.path.join(file_path, f'surftemp_{os.path.basename(file_path)}.tif')
    remove_if_exists(surftemp_image_path)
        
    processing.runAndLoadResults("gdal:warpreproject", 
                   {'INPUT': surftempraster,
                    'SOURCE_CRS': None,
                    'TARGET_CRS':QgsCoordinateReferenceSystem('EPSG:4326'),
                    'RESAMPLING':1, #bilinear interpolation
                    'NODATA':None,
                    'TARGET_RESOLUTION':None,
                    'OPTIONS':'',
                    'DATA_TYPE':0,
                    'TARGET_EXTENT':None,
                    'TARGET_EXTENT_CRS':None,
                    'MULTITHREADING':False,
                    'EXTRA':'',
                    'OUTPUT': surftemp_image_path})
    soilmoist_image_path = os.path.join(file_path, f'soilmoist_{os.path.basename(file_path)}.tif')
    
    remove_if_exists(soilmoist_image_path)
    processing.runAndLoadResults("gdal:warpreproject", 
                   {'INPUT': soilmoistraster,
                    'SOURCE_CRS': None,
                    'TARGET_CRS':QgsCoordinateReferenceSystem('EPSG:4326'),
                    'RESAMPLING':1, #bilinear interpolation
                    'NODATA':None,
                    'TARGET_RESOLUTION':None,
                    'OPTIONS':'',
                    'DATA_TYPE':0,
                    'TARGET_EXTENT':None,
                    'TARGET_EXTENT_CRS':None,
                    'MULTITHREADING':False,
                    'EXTRA':'',
                    'OUTPUT': soilmoist_image_path})
    
def create_vector(csv_file_path):
    # load each .csv containing sample site coordinates as vector point layers
    vector_layer = create_vector_layer(csv_file_path)
    directory_name = os.path.dirname(csv_file_path)
    location_layer = os.path.join(directory_name, f'location_{os.path.basename(directory_name)}')
    
    remove_if_exists(location_layer+'.gpkg')
    
    processing.runAndLoadResults("native:reprojectlayer", 
    {'INPUT': vector_layer,
    'TARGET_CRS':QgsCoordinateReferenceSystem('EPSG:4326'),
    'OPERATION':'+proj=noop',
    'OUTPUT': location_layer})

def extract_raster_data(dated_sample_folder: str) -> [dict, dict]:
    # sample rasters using corresponding sample sites
    surftemp_output = os.path.join(output_folder_path, f'surftemp_{os.path.basename(dated_sample_folder)}')
    location_layer = os.path.join(dated_sample_folder, f'location_{os.path.basename(dated_sample_folder)}')
    surftemp_image_path = os.path.join(dated_sample_folder, f'surftemp_{os.path.basename(dated_sample_folder)}.tif')
    soilmoist_image_path = os.path.join(dated_sample_folder, f'soilmoist_{os.path.basename(dated_sample_folder)}.tif')
    
    if not os.path.isfile(surftemp_output):
        surftemp_result = processing.run("native:rastersampling", 
        {'INPUT': f'{location_layer}.gpkg',
        'RASTERCOPY': surftemp_image_path,
        'COLUMN_PREFIX':'SURFTEMP_K',
        'OUTPUT': surftemp_output})
    
    soilmoist_output = os.path.join(output_folder_path, f'soilmoist_{os.path.basename(dated_sample_folder)}')
    if not os.path.isfile(soilmoist_output):
        soilmoist_result = processing.run("native:rastersampling", {
        'INPUT': f'{location_layer}.gpkg',
        'RASTERCOPY': soilmoist_image_path,
        'COLUMN_PREFIX':'SOILMOIST_kgm-2',
        'OUTPUT': soilmoist_output})
    
    return surftemp_result, soilmoist_result

def create_raster_and_vector_layers(dated_sample_folder: str) -> None:
    if os.path.isdir(dated_sample_folder):
        file_names = os.listdir(dated_sample_folder)
        
    # give .nc4 and .csv files unique names
    for file_name in file_names:
        if file_name.endswith('GLDAS.nc4'):
            add_raster_projections(dated_sample_folder, file_name)
        elif file_name.endswith('collectiondata.csv'):
            create_vector(os.path.join(dated_sample_folder, file_name))
            
def load_all_exported_layers(export_dir: str) -> None:
    clear_all_layers()
    file_list = os.listdir(export_dir)
    
    for file in file_list:
        full_filepath = os.path.join(export_dir, file)
        
        conn = ogr.Open(full_filepath)
        for i in conn:
            iface.addVectorLayer(full_filepath + "|layername=" + i.GetName(), i.GetName(), 'ogr')
            
def merge_all_weatherdata() -> None:
    all_layers = list(QgsProject.instance().mapLayers().values())
    
    parameters = {'LAYERS': all_layers,
                      'CRS': 'EPSG:4326',
                     'OUTPUT': os.path.join(data_dir, 'weather_merged.gpkg')}

                  
    processing.runAndLoadResults("qgis:mergevectorlayers", parameters)

# Change directories to where the datasetfetcher folder is.
data_dir = os.path.join('D:/','01_GradSchool', '_DissertationWork', 'Chapter4', '03_code', 'Weather2.0')
os.chdir(data_dir)

# define paths
sample_data_folder = os.path.join(data_dir, 'sample_data')
sample_dates = os.listdir(sample_data_folder)

# get list of file names from relevant dir
output_folder_path = os.path.join(data_dir, 'outputs')
# create path to output folder for results
if not os.path.exists(output_folder_path):
    os.makedirs(output_folder_path)

for sample in sample_dates:
    clear_all_layers()
    dated_sample_folder = os.path.join(sample_data_folder, sample)
    create_raster_and_vector_layers(dated_sample_folder)
    surftemp_result, soilmoist_result = extract_raster_data(dated_sample_folder)


load_all_exported_layers(output_folder_path)
merge_all_weatherdata()

time.sleep(30)
clear_all_layers()