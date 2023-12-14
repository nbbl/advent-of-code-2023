import unittest
from parameterized import parameterized

from day4 import Day4


class TestDay4(unittest.TestCase):

    @parameterized.expand([
        ('Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53',
         [[41, 48, 83, 86, 17], [83, 86, 6, 31, 17, 9, 48, 53]])
    ])
    def test_parse_line(self, line, expected):
        assert Day4().parse_line(line) == expected

    @parameterized.expand([
        ([41, 48, 83, 86, 17], [83, 86, 6, 31, 17, 9, 48, 53], [48, 83, 17, 86]),
        ([13, 32, 20, 16, 61], [61, 30, 68, 82, 17, 32, 24, 19], [32, 61]),
        ([41, 92, 73, 84, 69], [59, 84, 76, 51, 58, 5, 54, 83], [84]),
        ([31, 18, 13, 56, 72], [74, 77, 10, 23, 35, 67, 36, 11], [])
    ])
    def test_matching_numbers(self, winning_numbers, player_numbers, expected):
        res = Day4().matching_numbers(winning_numbers, player_numbers)
        assert set(res) == set(expected)

    @parameterized.expand([
        ([48, 83, 17, 86], 8),
        ([], 0)
    ])
    def test_card_value(self, card_matches, expected):
        assert Day4().card_value(card_matches) == expected

    @parameterized.expand([
        ([[[31, 18, 13, 56, 72], [74, 77, 10, 23, 35, 67, 36, 11]]], 0, 1)
    ])
    def test_num_cards_won(self, cards, card_index, expected):
        assert Day4().num_cards_won(cards, card_index) == expected


if __name__ == '__main__':
    unittest.main()
