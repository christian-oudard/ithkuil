from .phonology import (
    consonants, typing_consonants,
    vowels, typing_vowels,
    tone_names, tones, typing_tones,
    convert_typed,
)
from .grammar import deconstruct_formative
from .abbreviations import abbreviation_table

def format_help(width=80):
    output = []

    output.append('Consonants')
    buf = []
    for c, t in sorted(zip(consonants, typing_consonants)):
        if c != t:
            buf.append('{:<2} - {:<3}   '.format(c, t))
    output.extend(format_grid_lines(buf, width=width))

    output.append('Vowels')
    buf = []
    for c, t in sorted(zip(vowels, typing_vowels)):
        if c != t:
            buf.append('{:<2} - {:<3}   '.format(c, t))
    output.extend(format_grid_lines(buf, width=width))

    output.append('Tones (at beginning of word)')
    buf = []
    for tone_name in tone_names:
        c = tones[tone_name]
        t = typing_tones[tone_name]
        if c:
            output.append('{:<13}  {!r} - {!r}   '.format(tone_name, c, t))

    return '\n'.join(output)

def format_grid_lines(buf, width=80):
    output = []
    column_size = len(buf[0])
    num_columns = width // column_size
    while buf:
        line = buf[:num_columns]
        del buf[:num_columns]
        output.append(''.join(line))
    return output

if __name__ == '__main__':
    print('Type text to convert, "h" for help, or "q" to quit.')
    while True:
        try:
            typed = input('> ')
        except (EOFError, KeyboardInterrupt):
            break
        if typed == 'q':
            break
        elif typed == 'h':
            print(format_help())
        else:
            text = convert_typed(typed)
            print(text)
            for word in text.split():
                word = word.strip()
                if word == '':
                    continue
                print()
                print('"{}"'.format(word))
                deconstruction = deconstruct_formative(word)
                if deconstruction is None:
                    print('???')
                else:
                    for slot, value, meaning in deconstruction:
                        if value == '':
                            print('  {} empty'.format(slot))
                        else:
                            print('  {} = {}'.format(slot, value))
                        if slot == 'Cr':
                            print('    "{}"'.format(meaning))
                        else:
                            for abbr in meaning:
                                desc = abbreviation_table[abbr]
                                print('    {} : {}'.format(abbr, desc))
        print()