from common.solution import Solution, has_tests


class Day4(Solution):
    test_mode = True
    sample_input = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""
    input_file = "data/input_day4.txt"

    @has_tests([
        ('Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53',
         [[41, 48, 83, 86, 17], [83, 86, 6, 31, 17, 9, 48, 53]])
    ])
    def parse_line(self, line):
        return [[int(n) for n in l.split()]
                for l in line.strip().split(': ')[1].split(' | ')]

    def run_part1(self, lines, debug=False):
        pass

    def run_part2(self, lines, debug=False):
        pass


if __name__ == '__main__':
    day4 = Day4(test_mode=False)
    day4.parse_line('')