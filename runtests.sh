#! /bin/sh

nosetests-3.3 --verbosity=2 --with-doctest --with-coverage --cover-tests \
    --cover-package grammar \
    --cover-package writing \
    --cover-package ime \
    --cover-package phonology \
    $@
