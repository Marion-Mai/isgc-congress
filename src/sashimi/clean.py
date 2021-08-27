#!/usr/bin/env python

import re
from difflib import unified_diff
from itertools import permutations

CLEAN_REFLAGS = re.IGNORECASE | re.MULTILINE | re.DOTALL | re.VERBOSE


def clean_text(df):
    """
    Known untreated entries:
    964: "The research was co-financed"
    """

    def and_sections(re0, re1):
        return re0 + r"\s* (?: and | & ) \s*" + re1

    # Remove entries with no valid content
    unwanted_res = "^Lorem ipsum dolor sit amet"
    b_unwanted = df["abstract_text"].str.contains(unwanted_res)
    clean_df = df[~b_unwanted]

    # Section names to be removed
    section_names_res = [
        r"backgrounds?",
        r"conclusions?",
        r"discussions?",
        r"experiments?",
        r"experimental",
        r"introductions?",
        r"materials?",
        r"methods?",
        r"perspectives?",
        r"prospects?",
        r"objectives?",
        r"outlooks?",
        r"results?",
        r"significance",
        r"summary",
    ]
    section_names_re = r"|".join(
        [and_sections(x, y) for x, y in permutations(section_names_res, 2)]
        + section_names_res
    )

    # Remove invalid content from entries
    unclean_from_start_of_text_res = [
        r".* \n abstract (?: [^\n\w,]* \n)",
    ]
    unclean_res = [
        r"^ keys?\ ?words? (?: [^\n\w]* \n )? [^\n]*",
        r"(^ (?:" + section_names_re + r") (?: [^\n\w]* \n | \s* [^\n\w\s,&]+ ))",
    ]
    unclean_until_end_of_text_res = [
        r"^ acknowledge?ments? :? .*",
        r"^ r[eé]f[eé]rences? \s* :? \s* \n .*",
        r"^ [^\n\w]* [12] [^\n\w]+ \w [^\n]+ (?<!\d)(?:1[6789]|20)[0-9]{2}(?!\d) .*",
    ]
    unclean_rx = re.compile(
        pattern=r"|".join(
            unclean_from_start_of_text_res + unclean_res + unclean_until_end_of_text_res
        ),
        flags=CLEAN_REFLAGS,
    )
    clean_abstract_text = clean_df["abstract_text"].str.replace(unclean_rx, "")

    return clean_abstract_text


def check_clean(df, clean_abstract_text, start=0):
    comp = df["abstract_text"].compare(clean_abstract_text)
    for idx, diff in comp.agg(
        lambda x: unified_diff(x["self"].split("\n"), x["other"].split("\n")), axis=1
    ).items():
        if idx < start:
            continue
        for line in diff:
            print(line)
        print("\n" + 70 * "-" + str(idx) + "\n")
        if input():
            break


def search_text(df, rexp):
    sel = df.abstract_text.str.contains(
        rexp,
        flags=CLEAN_REFLAGS,
    )
    for idx, txt in df.loc[sel, "abstract_text"].items():
        print(txt)
        print("\n" + 70 * "-" + str(idx) + "\n")
        if input():
            break


def extract_text(df, rexp):
    return (
        df["abstract_text"]
        .str.extractall(
            rexp,
            flags=CLEAN_REFLAGS,
        )
        .dropna()
    )
