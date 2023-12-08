import re

from common.solution import Solution


class Day1(Solution):
    sample_input = """1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet"""
    sample_input_2 = """two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen"""
    input_file = "../jupyter/input_day1.txt"

    def parse_line(self, line):
        return line

    @staticmethod
    def extract_calibration_value(txt, debug=False):
        digits = re.findall(r'(\d)', txt)
        if debug:
            print(digits)
        return int(digits[0] + digits[-1])

    @staticmethod
    def extract_calibration_value_2(txt, debug=False):
        digit_strings = ['one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine']
        digit_regex = r'(\d|' + '|'.join(digit_strings) + ')'

        digits = [digit if digit.isnumeric() else str(digit_strings.index(digit) + 1)
                  for digit in re.findall(digit_regex, txt)]
        if debug:
            print(digits)
        return int(digits[0] + digits[-1])

    def run_part1(self, lines, debug=False):
        return sum(map(self.extract_calibration_value, lines))

    def run_part2(self, lines, debug=False):
        return sum(map(self.extract_calibration_value_2, lines))


if __name__ == '__main__':
    day1 = Day1()
    assert day1.check_sample_solution_part1() == 142
    assert day1.check_sample_solution_part2() == 281
    print(day1.compute_real_solution_part1())
    print(day1.compute_real_solution_part2())
