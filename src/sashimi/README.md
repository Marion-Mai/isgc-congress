# Running as as script

`python ${PATH_TO_THIS_MODULE} ${PATH_TO_DATA_DIR}`

cleans data and outputs `isgc_2015-2019_clean.csv.xz`.

`python ${PATH_TO_THIS_MODULE} ${PATH_TO_DATA_DIR} check_clean`

in addition, interactively displays what was cleaned.

`python ${PATH_TO_THIS_MODULE} ${PATH_TO_DATA_DIR} check_clean ${START}`

starts displaying changes from index ${START}.

## Requires Python modules `abstractology` and `graph-tool`:

`python ${PATH_TO_THIS_MODULE} ${PATH_TO_DATA_DIR} sashimi`

- cleans data and outputs `isgc_2015-2019_clean.csv.xz`.
- produces a domain-topic model of the data
- plots a domain-topic map
- produces chained models for time ('year') and conference topic ('topic_1')
- plots the respective domain-chained maps

