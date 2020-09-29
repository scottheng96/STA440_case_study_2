import pandas as pd
import os
from itertools import chain
# This function basically un-nests the nested arrays within the pickle format
def flatten(listOfLists):
    "Flatten one level of nesting"
    return chain.from_iterable(listOfLists)

files = ['S10', 'S11', 'S13', 'S14', 'S15', 'S16', 'S17', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9']

path = "/hpc/group/sta440-f20/WESAD/WESAD"

#output_path = "/hpc/group/sta440-f20/sdh45/output_data"
def get_chest_pickle(file_list):
    for file in file_list:
        file_name = "".join([path, "/", file, "/", file, ".pkl"])
        data = pd.read_pickle(file_name)
        
        # All are sampled at 700 Hz, downsampled to 4Hz
        df = pd.DataFrame()
        df['ACC_chest_X'] = data['signal']['chest']['ACC'][:,0][::175]
        df['ACC_chest_Y'] = data['signal']['chest']['ACC'][:,1][::175]
        df['ACC_chest_Z'] = data['signal']['chest']['ACC'][:,2][::175]
        df['EMG'] = list(flatten(data['signal']['chest']['EMG'][::175]))
        df['ECG'] = list(flatten(data['signal']['chest']['ECG'][::175]))
        df['EDA'] = list(flatten(data['signal']['chest']['EDA'][::175]))
        df['Temp'] = list(flatten(data['signal']['chest']['Temp'][::175]))
        df['Resp'] = list(flatten(data['signal']['chest']['Resp'][::175]))
        df['Label'] = data['label'][::175]
        df['Participant'] = file
        csv_file = "".join([output_path,'/',file,'_chest','.csv'])
        df.to_csv(csv_file)
        

get_chest_pickle(files)


def get_wrist_pickle(file_list):
    for file in file_list:
        file_name = "".join([path, "/", file, "/", file, ".pkl"])
        data = pd.read_pickle(file_name)

        # BVP: 64Hz, ACC: 32Hz, EDA: 4Hz, Temp: 4Hz
        # Converts all columns to 4Hz taken periodically within each column [::x] with x being the periodic factor
        df = pd.DataFrame()
        df['ACC_wrist_X'] = data['signal']['wrist']['ACC'][:,0][::8]
        df['ACC_wrist_Y'] = data['signal']['wrist']['ACC'][:,1][::8]
        df['ACC_wrist_Z'] = data['signal']['wrist']['ACC'][:,2][::8]
        df['BVP_wrist'] = list(flatten(data['signal']['wrist']['BVP'][::16]))
        df['EDA_wrist'] = list(flatten(data['signal']['wrist']['EDA']))
        df['TEMP_wrist'] = list(flatten(data['signal']['wrist']['TEMP']))
        df['Label'] = data['label'][::175]
        df['Participant'] = file
        csv_file = "".join([output_path,'/',file,'_wrist','.csv'])
        df.to_csv(csv_file)

get_wrist_pickle(files)
