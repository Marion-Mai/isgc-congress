#!/usr/bin/env python

import graph_tool  # not used, but avoids an import order bug # noqa
import abstractology

DEFAULT_CORPUS_NAME = "isgc"


def bootstrap(data):
    corpus = abstractology.Graphology()
    corpus.text_source = "abstract_text"
    corpus.col_title = "abstract_title"
    corpus.col_time = "year"
    _load_data(corpus, data)

    corpus.load_domain_topic_model()
    corpus.set_chain("year")
    corpus.load_domain_chained_model()
    corpus.set_chain("topic_1")
    corpus.load_domain_chained_model()

    corpus.register_config()
    return corpus


def load(data):
    corpus = abstractology.Graphology(
        config="auto_abstractology/reports/config.json",
        load_data=False,
    )
    _load_data(corpus, data)
    return corpus


def plot(corpus):
    corpus.load_domain_topic_model()
    corpus.domain_map("ISGC 2015-2017")

    corpus.set_chain("year")
    corpus.load_domain_chained_model()
    corpus.domain_map("ISGC 2015-2017", chained=True)

    corpus.set_chain(extend="topic_1")
    corpus.load_domain_chained_model()
    corpus.domain_map("ISGC 2015-2017", chained=True)


def _load_data(corpus, data):
    corpus.load_data(data, "dataframe", name=getattr(data, "name", DEFAULT_CORPUS_NAME))
    corpus.process_corpus(ngrams=1)  # no ngrams while I don't fix gensim on guix
