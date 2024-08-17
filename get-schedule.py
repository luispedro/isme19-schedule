import json
from jug import TaskGenerator
import requests
from time import sleep
from random import random
import bs4


BASE_URL = 'https://www.eventure-online.com/eventure/login.form?U7db3f155-cc7a-482b-ac36-191cd4f21cd7'
URL = 'https://www.eventure-online.com/eventure/public/ajaxGetPublicSessionList.form?cuuid=7db3f155-cc7a-482b-ac36-191cd4f21cd7'
SESSION_URL_PAT = 'https://www.eventure-online.com/eventure/public/ajaxViewPublicSession.form?cuuid=7db3f155-cc7a-482b-ac36-191cd4f21cd7&sessionUuid={uuid}'

POSTER_LIST_URL = 'https://www.eventure-online.com/eventure/public/ajaxGetPublicPosterList.form?cuuid=7db3f155-cc7a-482b-ac36-191cd4f21cd7'
POSTER_DETAILS_URL_PAT = 'https://www.eventure-online.com/eventure/public/ajaxViewPublicSession.form?cuuid=7db3f155-cc7a-482b-ac36-191cd4f21cd7&posterUuid={uuid}'


@TaskGenerator
def get_session_information():
    r = requests.get(BASE_URL)
    cookies = r.cookies

    r = requests.get(URL, cookies=cookies)
    p = bs4.BeautifulSoup(r.text, 'html.parser')

    Sessions = []

    for t in p.find_all('tr')[1:]:
        uuid = t.get('data-uuid')
        toks = t.find_all('td')
        code = toks[0].text
        title = toks[1].text
        stype = toks[2].text
        location = toks[3].text
        date, time = toks[4].strings

        s_url = SESSION_URL_PAT.format(uuid=uuid)
        r = requests.get(s_url, cookies=cookies)
        p = bs4.BeautifulSoup(r.text, 'html.parser')
        Sessions.append({
            'uuid': uuid,
            'code': code,
            'title': title,
            'type': stype,
            'location': location,
            'date': str(date), # Otherwise we get a NavigableString which is not picklable
            'time': str(time),
            'session-info': p
            })
        sleep(.2 + 2*random())
        print(f'Got session {title} (total of {len(Sessions)} sessions)')
    return Sessions


@TaskGenerator
def parse_session_info(session_info):
    for s in session_info:
        si = s['session-info']
        for i in si.find_all('img'):
            i.decompose()
        tabs = si.find_all('table')
        tab = tabs[2]
        # The first element is type of session, which we already have
        tab.find_all('tr')[0].decompose()
        s['session-info'] = tab.prettify()
        s['session-info-text'] = ' '.join(tab.strings).strip()
        if not s['session-info-text']:
            s['session-info-text'] = 'No details available'
    return session_info


@TaskGenerator
def save_session_info(session_info):
    json.dump(session_info, open('src/ISME19_all_sessions.json', 'wt'))

@TaskGenerator
def get_poster_session_information():
    r = requests.get(BASE_URL)
    cookies = r.cookies
    r = requests.get(POSTER_LIST_URL, cookies=cookies)
    p = bs4.BeautifulSoup(r.text, 'html.parser')
    poster_sessions = []
    for t in p.find('table').find_all('tr')[1:]:
        uuid = t.get('data-uuid')
        toks = t.find_all('td')
        code = toks[0].text
        title = toks[1].text
        stype = toks[2].text
        location = toks[3].text
        date, time = toks[4].strings

        s_url = POSTER_DETAILS_URL_PAT.format(uuid=uuid)
        nr = requests.get(s_url, cookies=cookies)
        np = bs4.BeautifulSoup(nr.text, 'html.parser')
        poster_sessions.append({
            'uuid': uuid,
            'code': code,
            'title': title,
            'type': stype,
            'location': location,
            'date': str(date), # Otherwise we get a NavigableString which is not picklable
            'time': str(time),
            'session-info': np
            })
        sleep(.2 + 2*random())
        print(f'Got poster session {title} (total of {len(poster_sessions)} sessions)')
    return poster_sessions

@TaskGenerator
def parse_poster_info(posters_info):
    reparsed = []
    for p in posters_info:
        p = p.copy()
        posters = []
        for tr in p['session-info'].find_all('table')[2].find_all('tr')[1:]:
            elems = list(tr.strings)
            title = ''.join(elems[1:-1])
            author = str(elems[-1])
            posters.append((title, author))
        p['posters'] = posters
        del p['session-info']
        reparsed.append(p)
    return reparsed


@TaskGenerator
def format_save_posters(parsed_posters):
    posters = []
    for s in parsed_posters:
        for title,author in s['posters']:
            posters.append({
                'title': title,
                'author': author,
                'location': s['location'],
                'date': s['date'],
                'time': s['time'],
                'session_title': s['title'],
                })
    json.dump(posters, open('src/ISME19_all_posters.json', 'wt'))
    return posters


session_info = get_session_information()
parsed_info = parse_session_info(session_info)
save_session_info(parsed_info)
posters_info = get_poster_session_information()
parsed_posters = parse_poster_info(posters_info)
format_save_posters(parsed_posters)
