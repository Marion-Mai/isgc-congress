#!/usr/bin/env python

import re
import pandas as pd
from difflib import unified_diff
from itertools import permutations

CLEAN_REFLAGS = re.IGNORECASE | re.MULTILINE | re.DOTALL | re.VERBOSE


def clean_text(df):
    """
    Known untreated entries:
    - some title plus authors headers with no clear separation
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
        r"motivation?",
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
    section_numbering_re = r"[^\n\w]* (?: \d? [^\n\w]* )"
    # Remove invalid content from entries
    unclean_from_start_of_text_res = [
        r"(?: ^ | .* \n)" + section_numbering_re + r"abstract (?: [^\n\w,]* \n)",
    ]
    unclean_res = [
        r"^" + section_numbering_re + r"keys?\ ?words? (?: [^\n\w]* \n )? [^\n]*",
        r"^"
        + section_numbering_re
        + r"(?:"
        + section_names_re
        + r") (?: \ * section)? (?: [^\n\w]* \n | \s* [^\n\w\s,&]+ )",
    ]
    unclean_until_end_of_text_res = [
        r"^" + section_numbering_re + r"ac?knowled?ge?ments? :? .*",
        r"^" + section_numbering_re + r"r[eé]f[eé]rences? \s* :? .*",
        r"^ [^\n\w]* [12] [^\n\w]+ \w [^\n]+ (?<!\d)(?:1[6789]|20)[0-9]{2}(?!\d) .*",
    ]
    unclean_rx = re.compile(
        pattern=r"|".join(
            unclean_from_start_of_text_res + unclean_res + unclean_until_end_of_text_res
        ),
        flags=CLEAN_REFLAGS,
    )
    clean_abstract_text = clean_df["abstract_text"].str.replace(unclean_rx, "")

    # Remove even more funding info (max 61) excluding (10) manually identified wrong matches
    clean_extra_funding_rx = re.compile(
        r"(^ [^\n]*"
        r"(?: fund[eis] | financ | supported\ by | support\ of | support\ from | grant )"
        r"[^\n]* \s* ) \Z",
        flags=CLEAN_REFLAGS,
    )
    up_index = clean_abstract_text.index.difference(
        [23, 968, 999, 1243, 1373, 1416, 1469, 1560, 1700, 1710]
    )
    clean_abstract_text = clean_abstract_text.loc[up_index].str.replace(
        clean_extra_funding_rx, ""
    )

    return clean_abstract_text


def check_clean(df_or_series, clean_abstract_text, start=0, interactive=True):
    """Compares two textual series showing diffs for each entry.

    If passed a dataframe as first argument, picks the "abstract_text" column.
    If `start` is provided, skips abstracts indexed less than its value.
    If not `interactive`, returns the diffs as a `pandas.Series`
    If `interactive`, waits for input at each entry, stopping if sent a nonempty string.
    """
    if not isinstance(df_or_series, pd.Series):
        abstract_text = df_or_series["abstract_text"]
    else:
        abstract_text = df_or_series
    abstract_text = abstract_text.loc[clean_abstract_text.index]
    comp = abstract_text.compare(clean_abstract_text)
    if comp.empty:
        print("No differences found.")
        return
    diff = comp.agg(
        lambda x: unified_diff(x["self"].split("\n"), x["other"].split("\n")), axis=1
    )
    if not interactive:
        return diff.map("\n".join)
    print(f"Found {len(diff)} modified documents.\n")
    for idx, diff in diff.loc[start:].items():
        for line in diff:
            print(line)
        print("\n" + 70 * "-" + str(idx) + "\n")
        if input():
            print("\nInterrupted!\n")
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
