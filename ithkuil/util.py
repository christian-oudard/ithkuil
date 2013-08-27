import re


def choices_pattern(options, split=False):
    options = list(options)
    options.sort(key=lambda x: (-len(x), x))
    options = [re.escape(s) for s in options]
    if split:
        template = r'({})'
    else:
        template = r'(?:{})'
    return template.format('|'.join(options))
