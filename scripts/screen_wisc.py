#!/usr/bin/env python3

#%%
# This script was created to screen the abcd neurocog battery data by the 
# subjects that have resting state FNC data that has been processed
# and put through group ica. Each file generated, will contain
# only the subjects that match the list of the 255 subjects in the
# the ica.
# Carlos Rodriguez, 10/19/21
# To run: open a terminal, navigate to the script directory, and then
# type python screen_wisc.py, and press enter

#%%
# Import libraries
import pandas as pd
import os

# Set the path to 
study_root = '/export/research/analysis/human/jhouck/cobre06_65007/carlos_work/ica_300/ica_300_graph_wisc'
files_root = '/export/research/analysis/human/jhouck/abcd/collection_2573'

# Change directory
os.chdir(study_root)

# Files to filter
#files = ['abcd_ps01.txt'] # used previously to filter abcd_ps01.txt
files = ['abcd_tbss01.txt', 'cct01.txt', 'lmtp201.txt']

# Load subject keys/GUIDs
subjects = pd.read_csv(study_root + '/data/' + 'ica_300_guids.csv')

# Convert subjects to list
subjects = list(subjects['GUID'])

# For loop to go through files
for i in range(len(files)):
    file_name = files[i]
    print(file_name)
    df = pd.read_csv(files_root + '/' + file_name, 
                     sep = '\t', low_memory= False)                             # reads in the .txt file
    index = df.subjectkey.isin(subjects)                                        # creates an index based off our subjects list
    index[0] = True                                                             # Modified this index to keep the double column names
    filtered_subjects = df[index]                                               # Uses the index to select rows
    out_path = study_root + '/data/filtered_' + file_name[0:-3] + 'csv'         # Sets the output path
    filtered_subjects.to_csv(out_path, index = False)                           # Saves a new file with filtered prefix in .csv format
    print(filtered_subjects['eventname'].value_counts())
