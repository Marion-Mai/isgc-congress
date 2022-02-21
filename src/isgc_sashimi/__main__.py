import sys
from pathlib import Path
from . import get_data
from .clean import check_clean

# User input
ISGC_FILES_DIR = Path(sys.argv[1] if len(sys.argv) > 1 else ".")
ACTION = sys.argv[2] if len(sys.argv) > 2 else None
START = int(sys.argv[3]) if len(sys.argv) > 3 else 0

# Path definitions
ISGC_FILES = [
    ISGC_FILES_DIR / file
    for file in ["abstracts_contents_2015.tsv", "abstracts_contents_1719.tsv"]
]
CLEAN_FILE = Path("isgc_2015-2019_clean.csv.xz")


def main():
    clean_data = get_data(ISGC_FILES, True)
    clean_data.to_csv(CLEAN_FILE)
    print(f"Saved {CLEAN_FILE}")
    clean_data.name = CLEAN_FILE.name.split(".")[0]

    if ACTION == "check_clean":
        unclean_data = get_data(ISGC_FILES, False)
        check_clean(unclean_data, clean_data["abstract_text"], START)

    elif ACTION == "sashimi":
        from . import sashimi

        try:
            corpus = sashimi.load(clean_data)
        except FileNotFoundError:
            corpus = sashimi.bootstrap(clean_data)
        sashimi.plot(corpus)


if __name__ == "__main__":
    main()
