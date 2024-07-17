import re

def get_number_sum(s: str) -> int:
    numbers = re.findall(r'\d', s)

    if numbers:
        return int(numbers[0]) * 10 + int(numbers[-1])
    else:
        return 0

def read_input(file_name: str) -> list[str]:
    with open(file_name, encoding="utf-8") as f:
        return f.readlines()

lines = read_input("input.txt")
RES = 0
for line in lines:
    RES += get_number_sum(line)

print(RES)
