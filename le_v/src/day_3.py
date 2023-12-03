# You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.
#
# It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.
#
# "Aaah!"
#
# You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.
#
# The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.
#
# The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)
#
# Here is an example engine schematic:
#
# 467..114..
# ...*......
# ..35..633.
# ......#...
# 617*......
# .....+.58.
# ..592.....
# ......755.
# ...$.*....
# .664.598..
# In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.
#
# Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

# --- Part Two ---
# The engineer finds the missing part and installs it in the engine! As the engine springs to life, you jump in the closest gondola, finally ready to ascend to the water source.
#
# You don't seem to be going very fast, though. Maybe something is still wrong? Fortunately, the gondola has a phone labeled "help", so you pick it up and the engineer answers.
#
# Before you can explain the situation, she suggests that you look out the window. There stands the engineer, holding a phone in one hand and waving with the other. You're going so slowly that you haven't even left the station. You exit the gondola.
#
# The missing part wasn't the only issue - one of the gears in the engine is wrong. A gear is any * symbol that is adjacent to exactly two part numbers. Its gear ratio is the result of multiplying those two numbers together.
#
# This time, you need to find the gear ratio of every gear and add them all up so that the engineer can figure out which gear needs to be replaced.
#
# Consider the same engine schematic again:
#
# 467..114..
# ...*......
# ..35..633.
# ......#...
# 617*......
# .....+.58.
# ..592.....
# ......755.
# ...$.*....
# .664.598..
# In this schematic, there are two gears. The first is in the top left; it has part numbers 467 and 35, so its gear ratio is 16345. The second gear is in the lower right; its gear ratio is 451490. (The * adjacent to 617 is not a gear because it is only adjacent to one part number.) Adding up all of the gear ratios produces 467835.
#
# What is the sum of all of the gear ratios in your engine schematic?

input_data = input("Paste data") or """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""

import itertools as it
import functools
import operator as op

numbers = []
symbols = []

groups = [(k, list(a)) for k, a in it.groupby(enumerate(input_data), key=lambda x: "skip" if x[1] in [".", "\n"] else "num" if x[1].isnumeric() else "symbol")]
for group_type, elems in groups:
    match group_type:
        case "skip":
            continue
        case "num":
            idx, values = zip(*elems)
            numbers.append((set(idx), int("".join(values))))
        case "symbol":
            symbols.extend(elems)
grid_width = input_data.find("\n")

print(numbers)
print(symbols)
print(grid_width)

valid_indices = {idx: {idx + l + c for c, l in it.product((-1, 0, 1), (-grid_width - 1, 0, grid_width + 1))} for idx, _ in symbols}
print(valid_indices)
all_valid_indices = set().union(*valid_indices.values())
print(all_valid_indices)

total_part_numbers = sum(value for indices, value in numbers if indices & all_valid_indices)

total_gear_ratio = 0
for gear_indices in valid_indices.values():
    adjacent_parts = [value for part_indices, value in numbers if gear_indices & part_indices]
    if len(adjacent_parts) > 1:
        total_gear_ratio += functools.reduce(op.mul, adjacent_parts)

print(f"Sum of part numbers: {total_part_numbers}")
print(f"Sum of gear ratios: {total_gear_ratio}")

