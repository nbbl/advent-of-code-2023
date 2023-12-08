from abc import ABC, abstractmethod


class Solution(ABC):
    sample_input: str
    sample_input_2: str = ''
    input_file: str
    test_mode: bool

    def __init__(self, test_mode=True):
        self.test_mode = test_mode

    def load_input_file(self):
        with open(self.input_file) as f:
            return f.readlines()

    def clean_sample_input(self):
        return filter(None, map(str.strip, self.sample_input.split()))

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
        lines = self.clean_sample_input()
        return self.run_part1(lines, debug=True)

    def compute_real_solution_part1(self):
        lines = self.load_input_file()
        return self.run_part1(lines)

    def check_sample_solution_part2(self):
        lines = map(str.strip, (self.sample_input_2 or self.sample_input).split())
        return self.run_part2(lines, debug=True)

    def compute_real_solution_part2(self):
        lines = self.load_input_file()
        return self.run_part2(lines)


# A weird decorator for testing while coding... it's bad but it's good. Maybe.
def has_tests(test_data):
    def function_wrapper(func):
        def inner(self, *args, **kwargs):
            if self.test_mode:
                print(f'Testing {func.__name__}... ', end='')
                for params, expected_res in test_data:
                    assert func(self, params) == expected_res
                print('✅')
            else:
                print(f'Running {func.__name__}:')
                print(args[0])
                func(self, *args, **kwargs)
        return inner
    return function_wrapper
