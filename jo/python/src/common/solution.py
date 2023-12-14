from abc import ABC, abstractmethod


class Solution(ABC):
    sample_input: str
    sample_input_2: str = ''
    input_file: str
    test_mode: bool

    def __init__(self, test_mode=False):
        self.test_mode = test_mode

    def load_input_file(self):
        with open(self.input_file) as f:
            return f.readlines()

    @staticmethod
    def clean_sample_input(sample_input):
        return filter(None, map(str.strip, sample_input.split('\n')))

    @abstractmethod
    def parse_line(self, line):
        pass

    @abstractmethod
    def run_part1(self, lines, debug=False):
        pass

    @abstractmethod
    def run_part2(self, lines, debug=False):
        pass

    def check_sample_solution_part1(self):
        lines = self.clean_sample_input(self.sample_input)
        return self.run_part1(lines, debug=True)

    def compute_real_solution_part1(self):
        lines = self.load_input_file()
        return self.run_part1(lines)

    def check_sample_solution_part2(self):
        lines = self.clean_sample_input(self.sample_input_2 or self.sample_input)
        return self.run_part2(lines, debug=True)

    def compute_real_solution_part2(self):
        lines = self.load_input_file()
        return self.run_part2(lines)
