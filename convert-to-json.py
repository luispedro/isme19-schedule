import pandas as pd
pd.read_table('ISMB_ECCB 2023 - All sessions.tsv').to_json(open('ISMB_ECCB_2023_All_sessions.json', 'wt'), orient='records', force_ascii=False)
