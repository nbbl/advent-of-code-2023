import argparse
import json
from typing import List, Dict
import re

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


def load_constraints(filename: str) -> Dict[str, int]:
    with open(filename, "r") as f:
        return json.load(f)


def flatten_and_sum(games: List[str], constraints: Dict[str, int]) -> int:
    games = list(map(lambda line: line.split(":")[1].strip(), games))
    regex = "([0-9]+) (green|blue|red)"
    total = 0
    for i, game in enumerate(games):
        draws = game.split(";")
        for x, draw in enumerate(draws):
            colors = draw.strip().split(", ")
            match_groups = [re.search(regex, color) for color in colors]
            truth_table = list(map(lambda match: int(match.group(1)) > constraints[match.group(2)], match_groups))
            if any(truth_table):
                break
            if x == len(draws) - 1:
                total += i+1
    return total

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog='Advent of Code 2023 - Day 2',
        description='',
        epilog='')
    parser.add_argument('filename')
    args = parser.parse_args()
    filename = args.filename
    constraints_filename = filename.split(".")[0] + "-constraints.json"
    lines = parse_file(filename)
    constraints = load_constraints(constraints_filename)
    summation = flatten_and_sum(lines, constraints)
    print(f"Answer: {summation}")
