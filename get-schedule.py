import json
from jug import TaskGenerator
import requests
from time import sleep
from random import random
import bs4


BASE_URL = 'https://www.eventure-online.com/eventure/login.form?U7db3f155-cc7a-482b-ac36-191cd4f21cd7'
URL = 'https://www.eventure-online.com/eventure/public/ajaxGetPublicSessionList.form?cuuid=7db3f155-cc7a-482b-ac36-191cd4f21cd7'
SESSION_URL_PAT = 'https://www.eventure-online.com/eventure/public/ajaxViewPublicSession.form?cuuid=7db3f155-cc7a-482b-ac36-191cd4f21cd7&sessionUuid={uuid}'


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
        sleep(1 + 2*random())
        print(f'Got session {title} (total of {len(Sessions)} sessions)')
    return Sessions


@TaskGenerator
def parse_session_info(session_info):
    for s in session_info:
        si = s['session-info']
        for i in si.find_all('img'):
            i.decompose()
        tab = si.find('table')
        # The first elements are title & location (which we already have)
        tab.find_all('tr')[1].decompose()
        tab.find_all('tr')[1].decompose()
        s['session-info'] = tab.prettify()
        s['session-info-text'] = ' '.join(tab.strings).strip()
    return session_info


@TaskGenerator
def save_session_info(session_info):
    json.dump(session_info, open('src/ISME19_all_sessions.json', 'wt'))


session_info = get_session_information()
parsed_info = parse_session_info(session_info)
save_session_info(parsed_info)

