#!/usr/bin/env python

import graph_tool  # not used, but avoids an import order bug # noqa
import abstractology
from . import get_data

CORPUS_NAME = "isgc_2015-2017"


def bootstrap():
    corpus = abstractology.Graphology()
    corpus.text_source = "abstract_text"
    corpus.col_title = "abstract_title"
    corpus.col_time = "year"
    _load_data(corpus)

    corpus.load_domain_topic_model()
    corpus.set_graph(extend={"prop": "year"})
    corpus.load_domain_chained_model()
    corpus.set_graph(extend={"prop": "topic_1"})
    corpus.load_domain_chained_model()

    corpus.register_config()
    return corpus


def load():
    corpus = abstractology.Graphology(
        config="auto_abstractology/reports/config.json",
        load_data=False,
    )
    _load_data(corpus)
    return corpus


def plot(corpus):
    corpus.load_domain_topic_model()
    corpus.domain_map("ISGC 2015-2017")

    corpus.set_graph(extend={"prop": "year"})
    corpus.load_domain_chained_model()
    corpus.domain_map("ISGC 2015-2017", chained=True)

    corpus.set_graph(extend={"prop": "topic_1"})
    corpus.load_domain_chained_model()
    corpus.domain_map("ISGC 2015-2017", chained=True)


def _load_data(corpus):
    corpus.load_data(get_data(), "dataframe", name=CORPUS_NAME)
    corpus.process_corpus(ngrams=1)  # no ngrams while I don't fix gensim on guix
