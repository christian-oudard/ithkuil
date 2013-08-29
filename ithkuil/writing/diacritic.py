class Diacritic:

    consonant_affix = NotImplemented


class Dot(Diacritic):

    consonant_affix = '-w'


class HBar(Diacritic):

    consonant_affix = '-y'


class VBar(Diacritic):

    consonant_affix = '-l'


class AcuteBar(Diacritic):

    consonant_affix = '-r'


class GraveBar(Diacritic):

    consonant_affix = '-r^'


class ArcLeft(Diacritic):

    consonant_affix = '-m'


class ArcRight(Diacritic):

    consonant_affix = '-n'


class HookRight(Diacritic):

    consonant_affix = 's-'


class AngleDownRight(Diacritic):

    consonant_affix = 'z-'


class HookLeft(Diacritic):

    consonant_affix = 's^-'


class AngleUpRight(Diacritic):

    consonant_affix = 'z^-'


class AngleUpLeft(Diacritic):

    consonant_affix = ('f-', 'v-')


class AngleDownLeft(Diacritic):

    consonant_affix = ('t,-', 'dh-')
