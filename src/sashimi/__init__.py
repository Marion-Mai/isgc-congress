#! /usr/bin/env python

import graph_tool  # not used, but avoids an import order bug
import sys
import abstractology
import pandas as pd
from .clean import clean_text, check_clean
from pathlib import Path

graph_tool  # just so the linter won't complain

""" NOTES

- Data columns:
    ['abstract_text',
     'abstract_title',
     'bibliography',
     'cancelled',
     'code',
     'figure_legend_1',
     'figure_legend_2',
     'figure_title_1',
     'figure_title_2',
     'final_status',
     'id',
     'index',
     'is_complete',
     'keyword_1',
     'keyword_2',
     'keyword_3',
     'keyword_4',
     'keywords',
     'legend_1',
     'legend_2',
     'not_to_remind',
     'program_day',
     'program_session',
     'publish_onsite',
     'relance_register',
     'topic_1',
     'topic_2',
     'topic_3',
     'user_id',
     'validate',
     'year']
"""


ISGC_FILES_DIR = Path(sys.argv[1] if len(sys.argv) > 1 else ".")
ISGC_2015_FILE = ISGC_FILES_DIR / "abstracts_contents_2015.tsv"
ISGC_2017_FILE = ISGC_FILES_DIR / "abstracts_contents_1719.tsv"


def get_data(clean=True):
    df15 = pd.read_csv(ISGC_2015_FILE, sep="\t")
    df15 = df15.dropna(subset=["abstract_text"])
    df17 = pd.read_csv(ISGC_2017_FILE, sep="\t")
    df17 = df17.dropna(subset=["abstract_text"])
    df = df15.append(df17)
    df = df.reset_index()

    if clean:
        df["abstract_text"] = clean_text(df)
        df.dropna(subset=["abstract_text"], inplace=True)

    return df


def _load_data(a, get_data=get_data):
    a.data = get_data()
    corpus_name = "isgc_2015-2017.df"
    if corpus_name not in a.loaded["data"]:
        a.loaded["data"].append(corpus_name)
        a.col_title = "abstract_title"
        a.col_time = "year"
        a.text_source = "abstract_text"
    a.process_corpus(ngrams=1)  # no ngrams while I don't fix gensim on guix


def bootstrap():
    a = abstractology.Graphology()
    _load_data(a)
    a.load_domain_topic_model()
    a.set_graph(extend={"prop": "year"})
    a.load_domain_chained_model()
    a.set_graph(extend={"prop": "topic_1"})
    a.load_domain_chained_model()
    a.register_config()

    return a


def load():
    a = abstractology.Graphology(
        config="auto_abstractology/reports/config.json",
        load_data=False,
    )
    _load_data(a)

    return a


def plot(a):
    a.load_domain_topic_model()
    a.plot_sashimi("ISGC 2015-2017")

    a.set_graph(extend={"prop": "year"})
    a.load_domain_chained_model()
    a.plot_sashimi("ISGC 2015-2017", chained=True)

    a.set_graph(extend={"prop": "topic_1"})
    a.load_domain_chained_model()
    a.plot_sashimi("ISGC 2015-2017", chained=True)


def main():
    action = sys.argv[2] if len(sys.argv) > 2 else "check_clean"
    if action == "check_clean":
        unclean = get_data(False)
        clean = get_data(True)
        check_clean(unclean.loc[clean.index], clean["abstract_text"])
    elif action == "sashimi":
        try:
            a = load()
        except FileNotFoundError:
            a = bootstrap()
        plot(a)


main()
