#!/usr/bin/env python3
import sys
from itertools import product

part2 = False
# part2 = True

def parse_input():
    """Parse the input into a list of (test_value, numbers)."""
    equations = []
    for line in sys.stdin:
        test_value, numbers = line.split(":")
        test_value = int(test_value.strip())
        numbers = list(map(int, numbers.strip().split()))
        equations.append((test_value, numbers))
    return equations

def evaluate_expression(numbers, operators):
    """Evaluate the expression with numbers and operators left-to-right."""
    result = numbers[0]
    for num, op in zip(numbers[1:], operators):
        if op == "+":
            result += num
        elif op == "*":
            result *= num
        elif op == "|":
            result = int(str(result) + str(num))
    return result

def is_equation_valid(test_value, numbers):
    """Check if the equation can produce the test value."""
    num_operators = len(numbers) - 1
    for operator_combination in product("+*|", repeat=num_operators):
        if evaluate_expression(numbers, operator_combination) == test_value:
            return True
    return False

def calculate_total_calibration():
    """Calculate the total calibration result."""
    equations = parse_input()
    total_calibration = 0

    for test_value, numbers in equations:
        if is_equation_valid(test_value, numbers):
            total_calibration += test_value

    return total_calibration

# Example usage
input_data = [
    "190: 10 19",
    "3267: 81 40 27",
    "83: 17 5",
    "156: 15 6",
    "7290: 6 8 6 15",
    "161011: 16 10 13",
    "192: 17 8 14",
    "21037: 9 7 18 13",
    "292: 11 6 16 20",
]

result = calculate_total_calibration()
print("Total Calibration Result:", result)
