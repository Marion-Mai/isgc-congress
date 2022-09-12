# Running the module as as script

First, add the folder containing this module to your $PYTHON_PATH:

`PATH_TO_PARENT_DIR=/home/user/projects/isgc-congress/src`

`export PYTHONPATH=$PYTHONPATH:${PATH_TO_PARENT_DIR}`

Then...

`python -m isgc_sashimi ${PATH_TO_DATA_DIR}`

cleans data and outputs `isgc_2015-2019_clean.csv.xz`, with new columns:

- abstract_text__cleaned
- abstract_text__is_cleaned
- abstract_text__has_authors

`python -m isgc_sashimi ${PATH_TO_DATA_DIR} check_clean`

in addition, interactively displays what was cleaned.

`python -m isgc_sashimi ${PATH_TO_DATA_DIR} check_clean ${START}`

starts displaying changes from index ${START}.

## Domain-topic modeling (SASHIMI)

Requires Python modules `abstractology` and `graph-tool`.

`python -m isgc_sashimi ${PATH_TO_DATA_DIR} sashimi`

will do the following, in sequence:

- cleans data and outputs `isgc_2015-2019_clean.csv.xz`.
- produces a domain-topic model of the data
- plots a domain-topic map
- produces chained models for time ('year') and conference topic ('topic_1')
- plots the respective domain-chained maps
