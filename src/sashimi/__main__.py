import sys
from pathlib import Path
from . import get_data
from .clean import check_clean

# User input
ISGC_FILES_DIR = Path(sys.argv[1] if len(sys.argv) > 1 else ".")
ISGC_FILES = [
    ISGC_FILES_DIR / file
    for file in ["abstracts_contents_2015.tsv", "abstracts_contents_1719.tsv"]
]
CLEAN_FILE = "abstracts_contents_clean.csv.xz"
ACTION = sys.argv[2] if len(sys.argv) > 2 else "check_clean"
START = int(sys.argv[3]) if len(sys.argv) > 3 else 0


def main():
    if ACTION == "check_clean":
        unclean = get_data(ISGC_FILES, False)
        clean = get_data(ISGC_FILES, True)
        clean.to_csv(CLEAN_FILE)
        check_clean(unclean, clean["abstract_text"], START)
    elif ACTION == "sashimi":
        import sashimi
        try:
            a = sashimi.load()
        except FileNotFoundError:
            a = sashimi.bootstrap()
        sashimi.plot(a)


main()
