import re
import unicodedata
from pyquery import PyQuery as pq
from phonology import consonants


SUPER_H = unicodedata.lookup('MODIFIER LETTER SMALL H')

def clean_html(html):
    html = html.replace('<strong>', '')
    html = html.replace('</strong>', '')
    html = html.replace('<sup>h</sup>', SUPER_H)
    html = html.replace('&nbsp;', ' ')
    html = re.sub('\s+', ' ', html)
    return html

def extract_suffixes(html):
    html = clean_html(html)
    d = pq(html)

    # Extract suffixes from tables.
    #for table in d('table'):
        #table = pq(table)
        ## Look for one definition row and 9 degree rows.
        #rows = table('tr')
        #if len(rows) != 10:
            #continue
        #print(pq(rows[0]).text())
    for tr in d('tr'):
        tr = pq(tr)
        cells = tr('td')
        if len(cells) != 3:
            continue

        td = cells[1]
        if td.attrib.get('bgcolor') == '#CCCCCC':
            text = pq(td).text()
            if text == text.upper():
                print(text)

if __name__ == '__main__':
    filename = 'source_material/07_suffixes.html'

    html = open(filename).read()
    suffix_data = extract_suffixes(html)
    
    #for line in suffix_data:
        #print(line)
