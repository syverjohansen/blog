import pandas as pd
import json
import os
L = pd.read_feather(os.path.expanduser('~/ski/elo/python/nordic-combined/polars/excel365/L.feather'))
M = pd.read_feather(os.path.expanduser('~/ski/elo/python/nordic-combined/polars/excel365/M.feather'))

def current_dict(df, file_path):
	    # Filter the DataFrame to keep only the rows with the maximum date
    df_current = df.loc[df['Date'] == max(df['Date'])]
    df_current = df_current[["Skier", "ID"]]

    current_dict = df_current.set_index("ID")["Skier"].to_dict()
    with open(file_path, 'w') as f:
        json.dump(current_dict, f, indent=4)


current_dict(L, os.path.expanduser("~/blog/daehl-e/static/python/nordic-combined/excel365/L_current_ids.json"))
current_dict(M, os.path.expanduser("~/blog/daehl-e/static/python/nordic-combined/excel365/M_current_ids.json"))


