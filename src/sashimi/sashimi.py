#!/usr/bin/env python

import graph_tool  # not used, but avoids an import order bug
import abstractology
from . import get_data

graph_tool  # just so the linter won't complain


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
