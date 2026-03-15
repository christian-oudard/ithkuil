#!/usr/bin/env python3
"""
Render Ithkuil formatives to SVG script.

Takes JSON input (from Haskell gloss tool) and produces SVG output.
Can also be used standalone with manual formative specifications.

Usage:
  echo '{"root":"m","stem":1,"version":"PRC","func":"STA","spec":"BSC",
         "ctx":"EXS","case":"POS","affixes":[{"cs":"ţř","degree":5,"type":1,"slot":5}]}' | python3 render_formative.py

  python3 render_formative.py --word malëuţřait
  python3 render_formative.py --sentence "Malëuţřait welacu"
"""
import sys, os, json, argparse
sys.path.insert(0, os.path.dirname(__file__))

from render import FormativeRenderer, render_consonant_cluster

# Case name -> (type_index, num_index)
CASE_MAP = {
    # Transrelative (type 0)
    'THM': (0,1), 'INS': (0,2), 'ABS': (0,3), 'AFF': (0,4), 'STM': (0,5),
    'EFF': (0,6), 'ERG': (0,7), 'DAT': (0,8), 'IND': (0,9),
    # Appositive (type 1)
    'POS': (1,1), 'PRP': (1,2), 'GEN': (1,3), 'ATT': (1,4), 'PDC': (1,5),
    'ITP': (1,6), 'OGN': (1,7), 'IDP': (1,8), 'PAR': (1,9),
    # Associative (type 2)
    'APL': (2,1), 'PUR': (2,2), 'TRA': (2,3), 'DFR': (2,4), 'CRS': (2,5),
    'TSP': (2,6), 'CMM': (2,7), 'CMP': (2,8), 'CSD': (2,9),
    # Adverbial (type 3)
    'FUN': (3,1), 'TFM': (3,2), 'CLM_': (3,3), 'RSL': (3,4), 'CSM': (3,5),
    'CON': (3,6), 'AVR': (3,7), 'CVS': (3,8), 'SIT': (3,9),
    # Relational (type 4)
    'PRN': (4,1), 'DES': (4,2), 'CLA': (4,3), 'EXP': (4,4), 'DPN': (4,5),
    'PVS': (4,6), 'PTL': (4,7), 'LIM': (4,8),
    # Affinitive (type 5)
    'ACC': (5,1), 'PRI': (5,2), 'ASI': (5,3), 'EXC': (5,4), 'CNR': (5,5),
    'STA_': (5,6), 'COR': (5,7), 'DEP': (5,8),
    # Spatio-Temporal I (type 6)
    'LOC': (6,1), 'ATD': (6,2), 'ALL': (6,3), 'ABL': (6,4), 'ORI': (6,5),
    'IRL': (6,6), 'INV': (6,7), 'NAV': (6,8),
    # Spatio-Temporal II (type 7)
    'CNS_': (7,1), 'ASS': (7,2), 'PER': (7,3), 'PRO': (7,4), 'PCV': (7,5),
    'PCR': (7,6), 'ELP': (7,7), 'PLM': (7,8),
}


def render_from_json(data):
    """Render a formative from JSON specification."""
    r = FormativeRenderer()

    # Primary character
    r.add_primary(
        spec=data.get('spec', 'BSC'),
        ctx=data.get('ctx', 'EXS'),
        stem=data.get('stem', 1),
        func=data.get('func', 'STA'),
    )

    # Root consonant(s)
    root = data.get('root', '')
    for c in render_consonant_cluster(root):
        r.add_secondary(c)

    # Slot V affixes
    for afx in data.get('affixes', []):
        slot = afx.get('slot', 5)
        rotated = (slot == 7)
        cs = afx.get('cs', '')
        degree = afx.get('degree')
        atype = afx.get('type', 1)
        for c in render_consonant_cluster(cs):
            r.add_secondary(c, rotated=rotated, degree=degree, affix_type=atype)

    # Quaternary (case)
    case_name = data.get('case', 'THM')
    case_type, case_num = CASE_MAP.get(case_name, (0, 1))
    r.add_quaternary(case_type=case_type, case_num=case_num)

    return r.to_svg()


def render_sentence(words_json):
    """Render multiple formatives as a sentence."""
    parts = [
        '<svg xmlns="http://www.w3.org/2000/svg" width="1200" height="150" viewBox="0 0 1200 150">',
        '<rect width="100%" height="100%" fill="white"/>',
    ]
    x_offset = 10
    for word_data in words_json:
        r = FormativeRenderer()
        r.x_cursor = 0  # reset for each word

        r.add_primary(
            spec=word_data.get('spec', 'BSC'),
            ctx=word_data.get('ctx', 'EXS'),
            stem=word_data.get('stem', 1),
            func=word_data.get('func', 'STA'),
        )
        for c in render_consonant_cluster(word_data.get('root', '')):
            r.add_secondary(c)
        for afx in word_data.get('affixes', []):
            rotated = afx.get('slot', 5) == 7
            for c in render_consonant_cluster(afx.get('cs', '')):
                r.add_secondary(c, rotated=rotated,
                                degree=afx.get('degree'), affix_type=afx.get('type', 1))
        case_name = word_data.get('case', 'THM')
        ct, cn = CASE_MAP.get(case_name, (0, 1))
        r.add_quaternary(case_type=ct, case_num=cn)

        # Embed word SVG content
        inner = '\n'.join(e for e in r.elements if e)
        parts.append(f'<g transform="translate({x_offset},10)">{inner}</g>')
        x_offset += r.x_cursor + 25  # word spacing

    parts.append('</svg>')
    return '\n'.join(parts)


def main():
    parser = argparse.ArgumentParser(description='Render Ithkuil formatives to SVG')
    parser.add_argument('--json', help='JSON formative spec (or - for stdin)')
    parser.add_argument('--sentence', help='JSON array of formatives')
    parser.add_argument('--output', '-o', default='-', help='Output file (- for stdout)')
    parser.add_argument('--demo', action='store_true', help='Run demo rendering')
    args = parser.parse_args()

    if args.demo:
        # Demo: render "Malëuţřait" and a simple sentence
        demo_data = [
            {"root": "m", "stem": 1, "func": "STA", "spec": "BSC", "ctx": "EXS",
             "case": "POS", "affixes": [{"cs": "ţř", "degree": 5, "type": 1, "slot": 5}]},
            {"root": "l", "stem": 1, "func": "DYN", "spec": "BSC", "ctx": "EXS",
             "case": "ERG"},
            {"root": "kš", "stem": 1, "func": "STA", "spec": "CTE", "ctx": "EXS",
             "case": "ABS", "affixes": [{"cs": "r", "degree": 4, "type": 2, "slot": 7}]},
        ]
        svg = render_sentence(demo_data)
        outfile = args.output if args.output != '-' else 'script/demo_sentence.svg'
        with open(outfile, 'w') as f:
            f.write(svg)
        print(f'Wrote demo to {outfile}', file=sys.stderr)
        return

    if args.json:
        data = json.loads(args.json) if args.json != '-' else json.load(sys.stdin)
        svg = render_from_json(data)
    elif args.sentence:
        data = json.loads(args.sentence)
        svg = render_sentence(data)
    else:
        # Read from stdin
        data = json.load(sys.stdin)
        if isinstance(data, list):
            svg = render_sentence(data)
        else:
            svg = render_from_json(data)

    if args.output == '-':
        print(svg)
    else:
        with open(args.output, 'w') as f:
            f.write(svg)
        print(f'Wrote {args.output}', file=sys.stderr)


if __name__ == '__main__':
    main()
