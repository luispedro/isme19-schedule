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

def fix_encoding1(s):
    try:
        return s.encode('macroman').decode('utf-8')
    except:
        return s
def fix_encodings(talk):
    if talk.Session == "Function":
        talk.Title = fix_encoding1(talk.Title)
        talk.Speaker = fix_encoding1(talk.Speaker)
    return talk

timetable = timetable.apply(fix_encodings, axis=1)

with open('ISMB_ECCB_2023_All_sessions.json', 'wt') as out:
    timetable.to_json(
            out,
            orient='records',
            force_ascii=False)
