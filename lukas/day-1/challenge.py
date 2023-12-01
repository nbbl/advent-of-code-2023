import argparse
from typing import List
import re

corpus = [
    "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
]


def parse_file(filename: str) -> List[str]:
    """
    Parse file
    :param filename:
    :return:
    """
    lines = []
    with open(filename, "r") as f:
        lines = f.read().splitlines()
    if not lines:
        raise Exception(f"Failed to parse {filename}")
    return lines


def tokenize(line: str) -> List[str]:
    """
    Tokenize the string with the character and word corpus. Finds start and end indices of each token (if present)
    :param line:
    :return:
    """
    all_num_indices = []
    for key in corpus:
        occurrences = [(m.start(), m.end()) for m in re.finditer(key, line)]
        if occurrences:
            all_num_indices += occurrences
    all_num_indices = sorted(all_num_indices)
    tokens = [line[index[0]:index[1]] for index in all_num_indices]
    return tokens


def tokens_to_ints(tokens: List[str]) -> List[int]:
    """
    Extracts first and last int using corpus

    :param tokens:
    :return:
    """
    ints = []
    for token in tokens:
        try:
            integer = int(token)
            ints.append(integer)
        except ValueError:
            ints.append(corpus.index(token) - 8)  # ¯\_(ツ)_/¯
        except Exception as e:
            raise Exception(f"Unknown error: {e}")
    if len(ints) == 1:  # Special case for only single digit
        ints.append(ints[0])
    elif len(ints) > 2:
        del ints[1:-1]  # Remove everything except first and last list items
    return ints


def tokenize_all(lines: List[str]) -> List[List[int]]:
    """
    Helper

    :param lines:
    :return:
    """
    all_ints = []
    for line in lines:
        tokens = tokenize(line)
        ints = tokens_to_ints(tokens)
        all_ints.append(ints)
    return all_ints


'''
# For challenge Pt 1
def get_ints(lines: List[str]) -> List[List[int]]:
    """
    Extract ints from each line, extract first and last
    :param lines:
    :return:
    """
    ints = []
    for line in lines:
        line_ints = list(filter(
            lambda c: True if c.isdigit() else False, line
        ))  # Filter for digits
        line_ints = [int(i) for i in line_ints]  # Cast everything to int

        if len(line_ints) == 1: # Special case for only single digit
            line_ints.append(line_ints[0])
        elif len(line_ints) > 2:
            del line_ints[1:-1]  # Remove everything except first and last list items
        ints.append(line_ints)
    return ints
'''


def summation(ints: List[List[int]]) -> int:
    """
    Combine the ints and sum them
    :param ints:
    :return:
    """
    build = lambda tup: int(str(tup[0]) + str(tup[1]))
    return sum([build(int_list) for int_list in ints])



if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog='Advent of Code 2023 - Day 1',
        description='',
        epilog='')
    parser.add_argument('filename')
    args = parser.parse_args()
    lines = parse_file(args.filename)
    ints = tokenize_all(lines)
    total = summation(ints)
    print(f"Answer: {total}")
