#! /usr/bin/env python

import graph_tool  # not used, but avoids an import order bug
import abstractology
import pandas as pd

graph_tool

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


def reload():
    """Live reload while developing"""
    import importlib

    importlib.reload(abstractology)


def get_data():
    df15 = pd.read_csv("abstracts_contents_2015.tsv", sep="\t")
    df15 = df15.dropna(subset=["abstract_text"])
    df17 = pd.read_csv("abstracts_contents_1719.tsv", sep="\t")
    df17 = df17.dropna(subset=["abstract_text"])
    df = df15.append(df17)
    df = df.reset_index()

    return df


def _load_data(a, get_data=get_data):
    a.data = get_data()
    a.process_corpus(ngrams=1)  # no ngrams while I don't fix gensim on guix
    corpus_name = "isgc_2015-2017.df"
    if corpus_name not in a.loaded["data"]:
        a.loaded["data"].append(corpus_name)
        a.col_title = "abstract_title"
        a.col_time = "year"
        a.text_source = "abstract_text"


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
    try:
        a = load()
    except FileNotFoundError:
        a = bootstrap()
    plot(a)
    a.data.columns
    reload()


if __name__ == "__main__":
    main()
