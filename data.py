import pandas as pd
import os
from itertools import chain
# This function basically un-nests the nested arrays within the pickle format
def flatten(listOfLists):
    "Flatten one level of nesting"
    return chain.from_iterable(listOfLists)

#os.listdir("/Users/ethanshen/Downloads/WESAD")
#data = pd.read_pickle("/Users/ethanshen/Documents/College/Fa20/STA 440/S2/S2.pkl")

#df = pd.DataFrame()

files = ['S10', 'S11', 'S13', 'S14', 'S15', 'S16', 'S17', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9']
path = "/Users/ethanshen/Downloads/WESAD" 
path = "/hpc/group/sta440-f20/WESAD/WESAD"
def get_chest_pickle(file_list):
    chest_data = pd.DataFrame()
    for file in file_list:
        file_name = "".join([path, "/", file, "/", file, ".pkl"])
        data = pd.read_pickle(file_name)
        
        df = pd.DataFrame()

        df['ACC_chest_X'] = data['signal']['chest']['ACC'][:,0]
        df['ACC_chest_Y'] = data['signal']['chest']['ACC'][:,1]
        df['ACC_chest_Z'] = data['signal']['chest']['ACC'][:,2]
        df['EMG'] = list(flatten(data['signal']['chest']['EMG']))
        df['ECG'] = list(flatten(data['signal']['chest']['ECG']))
        df['EDA'] = list(flatten(data['signal']['chest']['EDA']))
        df['Temp'] = list(flatten(data['signal']['chest']['Temp']))
        df['Resp'] = list(flatten(data['signal']['chest']['Resp']))
        df['Label'] = data['label']
        df['Participant'] = file
        chest_data = pd.concat([chest_data, df])
    return chest_data


chest_metrics_df = get_chest_pickle(files)

chest_metrics_df.to_csv(('Chest_metrics.csv'), mode='a', header=True)

def get_wrist_pickle(file_list):
    wrist_data = pd.DataFrame()
    for file in file_list:
        file_name = "".join([path, "/", file, "/", file, ".pkl"])
        data = pd.read_pickle(file_name)

        ACC_wrist_x = data['signal']['wrist']['ACC'][:,0]
        ACC_wrist_y = data['signal']['wrist']['ACC'][:,1]
        ACC_wrist_z = data['signal']['wrist']['ACC'][:,2]
        BVP_wrist = list(flatten(data['signal']['wrist']['BVP']))
        EDA_wrist = list(flatten(data['signal']['wrist']['EDA']))
        TEMP_wrist = list(flatten(data['signal']['wrist']['TEMP']))

    return ACC_wrist_x

get_wrist_pickle(files)