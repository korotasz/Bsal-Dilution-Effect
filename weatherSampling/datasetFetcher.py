#!/usr/bin/env python

__author__ = "Jacob Dubs, Alexis Korotasz"
__version__ = "1.0.0"
__maintainer__ = "Alexis Korotasz"
__email__ = "akorotas@nd.edu"
__status__ = "Production"

import csv # Used for writing/reading to CSV files.
import sys # Systems module (for things like arguments, etc).
import datetime as datetime # Used for creating dates.
import requests # Used for retrieving data from the web.
import os # Used for common operating system operations.
from dataclasses import dataclass # Used to create dataclasses.
from enum import Enum # Used for creating enumerations.

class DataType(Enum):
    GLDAS = 1
#    IMERG = 2

@dataclass
class Sample:
    '''A class representing an individual sample collection.'''
    Lat: str
    Lon: str
    timepoint: str
    year: str
    month: str
    day: str
    
    def get_f_month(self) -> str:
        return str('%02d' % int(self.month))

    f_month = property(get_f_month)

    def get_f_year(self) -> str:
        return str(self.year)

    f_year = property(get_f_year)

    def get_f_day(self) -> str:
        return str('%02d' % int(self.day))

    f_day = property(get_f_day)

def parse_collection_data(csv_path: str) -> list[Sample]: # type: ignore
    '''
        Takes in a file path which contains the data for the collection sites used.
        @return - An array of the collection site sample objects.
    '''
    try:
        with open(csv_path, mode='r') as collection_data:
            data_reader = csv.reader(collection_data, delimiter=",")
            
            sample_collection = []
            for row in data_reader:
                samp = Sample(row[0], row[1], row[2], row[3], row[4], row[5])
                sample_collection.append(samp)
                
            return sample_collection[1:]
        
    except IOError:
        print('Check to make sure that a csv file was passed in.')

def create_directory(path: str) -> None:
    '''Create a directory on the filesystem if it doesn't exist.'''
    try:
        if not os.path.exists(path):
            os.makedirs(path)
    except IOError as e:
        print(f'Could not create the folder to save to.\n{e}')

def already_downloaded(filepath: str) -> bool:
    if os.path.isfile(filepath) and filepath.endswith('GLDAS.nc4'):
        return True
#    elif os.path.isfile(filepath) and filepath.endswith('IMERG.nc4'):
#        return True
    else:
        return False

def already_saved_collectiondata(sample: Sample, filePath: str) -> bool:
    if os.path.exists(filePath):
        with open(filePath, 'r') as f:
            contents = f.read()
            exists = f'{sample.Lat},{sample.Lon},{sample.timepoint},{sample.year},{sample.month},{sample.day}' in contents
        return exists
    else:
        return False

def build_url(sample: Sample, data_type: DataType) -> str:
    '''
        Build the URL for downloading the datatype.
        @return A string representing the url.
    '''

    if data_type == DataType.GLDAS:
        # GLDAS GRACE satellites for temp and soil moisture data
        return 'https://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fdata%2FGLDAS%2FGLDAS_CLSM025_DA1_D.2.2%2F' + sample.f_year + '%2F' + sample.f_month + '%2FGLDAS_CLSM025_DA1_D.A' + sample.f_year + sample.f_month + sample.f_day + '.022.nc4&SERVICE=L34RS_LDAS&LABEL=GLDAS_CLSM025_DA1_D.A' + sample.f_year + sample.f_month + sample.f_day + '.022.nc4.SUB.nc4&VERSION=1.02&FORMAT=bmM0Lw&DATASET_VERSION=2.2&SHORTNAME=GLDAS_CLSM025_DA1_D&BBOX=35.085%2C-11.602%2C71.823%2C42.363'
#    elif data_type == DataType.IMERG:
#        return <IMERG URL>
    else:    
        raise Exception(f"Invalid data type: {data_type}!")


def download_data(data_type: DataType, sample: Sample, save_path: str) -> None:
    '''
        Builds the correct url for the datatype, downloads the data, and
        saves it.
    '''
    sample_path = os.path.join(save_path, f'{sample.f_year}-{sample.f_month}-{sample.f_day}') # Generates folder name for sample data
    filename = os.path.join(sample_path, f'{sample.f_year}-{sample.f_month}-{sample.f_day}_{data_type.name}.nc4') # Generates custom file name for each .nc4 file

    if already_downloaded(filename):
        print(f"Already Downloaded (Skipping) -> {filename}")
        return

    create_directory(sample_path)
    url = build_url(sample, data_type)

    try:
        result = requests.get(url)
        result.raise_for_status()

        try:
            f = open(filename, 'wb')
            f.write(result.content)
            f.close()
            print(f"Successfully Downloaded -> {filename}")
        except OSError as e:
            print(f'''Could Not Save {filename}
                    \t{e}''')

    except requests.HTTPError as e:
        print(f'''\nError Downloading {filename}\nURL ({url})\n(Error {e.response.status_code}) - {e.response.reason}\n''')

def download_gldas_data(sample: Sample, save_path: str) -> None:
    '''Download the GLDAS data.'''
    download_data(DataType.GLDAS, sample, save_path)

#def download_imerg_data(sample: Sample, save_path: str) -> None:
#    '''Download the IMERG data.'''
#    download_data(DataType.IMERG, sample, save_path)

def save_collection_data(sample: Sample, save_path: str) -> None:
    '''
        Save the collection data to the folder where the downloaded sample data
        has been saved.
    '''
    sample_path = os.path.join(save_path, f'{sample.f_year}-{sample.f_month}-{sample.f_day}')
    create_directory(sample_path)

    collection_data_fp = os.path.join(sample_path, 'collectiondata.csv')

    try:
        if not os.path.exists(collection_data_fp):
            f = csv.writer(open(collection_data_fp, 'w', newline=''))
            f.writerow(['Lat', 'Lon', 'timepoint', 'year','month', 'day'])
            f.writerow([sample.Lat, sample.Lon, sample.timepoint, sample.year, sample.month, sample.day])
            print(f'Successfully Saved -> {collection_data_fp}')
        elif os.path.exists(collection_data_fp) and not already_saved_collectiondata(sample, collection_data_fp):
            f = csv.writer(open(collection_data_fp, 'a', newline=''))
            f.writerow([sample.Lat, sample.Lon, sample.timepoint, sample.year, sample.month, sample.day])
            print(f'Successfully Appended -> {collection_data_fp}')
    except OSError as e:
        print(f"\nError Saving Collection Data\n{e}\n")

if __name__ == '__main__':
    if len(sys.argv) == 1:
        # No path was passed in, let's warn.
        print('''
        Please pass in the correct commandline arguments:
            [WEATHER_DATA]: The csv file with weather data.
            [OUTPUT_PATH] *Optional*: The directory to output sample data to.
        ''')
        sys.exit(-1)

    output_directory = os.getcwd()
    save_path = os.path.join(output_directory, 'sample_data')

    if len(sys.argv) == 3:
        #We have another path, it must be the output directory.
        save_path = os.path.join(sys.argv[2], 'sample_data')

    coll_site_data = parse_collection_data(sys.argv[1])
    for samp in coll_site_data:
        download_gldas_data(samp, save_path)
#        download_imerg_data(samp, save_path)
        save_collection_data(samp, save_path)

