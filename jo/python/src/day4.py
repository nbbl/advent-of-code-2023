from common.solution import Solution


class Day4(Solution):
    sample_input = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""
    input_file = "data/input_day4.txt"
    cards_won_map = {}

    def parse_line(self, line):
        return [[int(n) for n in l.split()]
                for l in line.strip().split(': ')[1].split(' | ')]

    @staticmethod
    def matching_numbers(winning_numbers, player_numbers):
        return set(num for num in winning_numbers if num in player_numbers)

    @staticmethod
    def card_value(card_matches):
        return 2**(len(card_matches)-1) if card_matches else 0

    def num_cards_won(self, cards, card_index):
        # Already computed
        if card_index in self.cards_won_map:
            return self.cards_won_map[card_index]

        self.cards_won_map[card_index] = (
            # Last card, no next cards to pick
            1 if card_index == len(cards) else
            # No matching numbers
            1 if (num_matching_numbers := len(self.matching_numbers(*cards[card_index]))) == 0 else
            # Grab copies
            1 + sum(self.num_cards_won(cards, copy_index) for copy_index in
                    range(card_index + 1, card_index + 1 + num_matching_numbers))
        )
        return self.cards_won_map[card_index]

    def run_part1(self, lines, debug=False):
        return sum(self.card_value(self.matching_numbers(*self.parse_line(line))) for line in lines)

    def run_part2(self, lines, debug=False):
        self.cards_won_map = {}  # This is kind of important.
        cards = list(map(self.parse_line, lines))
        return sum(self.num_cards_won(cards, card_index=i) for i in range(len(cards)))


if __name__ == '__main__':
    day4 = Day4()
    assert day4.check_sample_solution_part1() == 13
    print(day4.compute_real_solution_part1())
    assert day4.check_sample_solution_part2() == 30
    print(day4.compute_real_solution_part2())
