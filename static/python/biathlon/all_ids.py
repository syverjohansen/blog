import pandas as pd
import json
import os
L = pd.read_feather(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/L.feather'))
M = pd.read_feather(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/M.feather'))

def all_dict(df, file_path):
	    # Filter the DataFrame to keep only the rows with the maximum date
    df_all = df
    df_all = df_all[["Skier", "ID"]]

    all_dict = df_all.set_index("ID")["Skier"].to_dict()
    with open(file_path, 'w') as f:
        json.dump(all_dict, f, indent=4)


all_dict(L, os.path.expanduser("~/blog/daehl-e/static/python/biathlon/excel365/L_all_ids.json"))
all_dict(M, os.path.expanduser("~/blog/daehl-e/static/python/biathlon/excel365/M_all_ids.json"))


