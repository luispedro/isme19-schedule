import requests
import pandas as pd
from os import path

COLS = [
 'Track',
 'Room',
 'Weekday',
 'Date',
 'Timespan',
 'Format',
 'Speaker',
 'Title',
 'Abstract']

URL = 'https://www.iscb.org/images/stories/ismb2024/document.ScheduleByTrack.ISMB.2024.xlsx'
if not path.exists(path.basename(URL)):
    print('Downloading file...')
    r = requests.get(URL)
    with open(path.basename(URL), 'wb') as f:
        f.write(r.content)

timetable = pd.read_excel(path.basename(URL))
timetable = timetable.iloc[:-1]
timetable.rename(columns={'Confirmed Presenter': 'Speaker'}, inplace=True)
timetable['Room'] = timetable['Room'].fillna('-').astype(str)
timetable['Weekday'] = timetable['Date'].dt.day_name()

assert set(timetable["Start Time"].dropna().astype(str).str.split(':').str[-1]) == {'00'}
assert set(timetable["End Time"].dropna().astype(str).str.split(':').str[-1]) == {'00'}

timetable['Timespan'] = timetable.apply(lambda x: x['Start Time'].strftime('%H:%M') + '-' + x['End Time'].strftime('%H:%M'), axis=1)
timetable['Date'] = timetable['Date'].dt.strftime('%d %B')
timetable = timetable[COLS]

with open('ISMB_2024_All_sessions.json', 'wt') as out:
    timetable.to_json(
            out,
            orient='records',
            force_ascii=False)
