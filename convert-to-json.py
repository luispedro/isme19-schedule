import pandas as pd
COLS = ['Session',
 'Room',
 'Weekday',
 'Date',
 'Timespan',
 'Format',
 'Type',
 'Speaker',
 'Title',
 'Abstract']

URL = 'https://docs.google.com/spreadsheets/d/169W9o9nGMqYQ5Ojplh1jd3WkEHNgWk4EwREkWo6fD6k/gviz/tq?tqx=out:csv'
timetable = pd.read_csv(URL, usecols=COLS)
with open('ISMB_ECCB_2023_All_sessions.json', 'wt') as out:
    timetable.to_json(
            out,
            orient='records',
            force_ascii=False)
