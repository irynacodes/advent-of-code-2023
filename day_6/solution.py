def process_race(race: tuple[int, int]) -> tuple[int, int]:
    time = race[0]
    winning_time = find_winning(time // 2, race)
    first_winning = get_lowest_winning(winning_time // 2, 0, winning_time, race)
    last_winning = get_highest_winning((winning_time + time) // 2, winning_time, time, race)
    return (first_winning, last_winning)

# of course it would be possible to unite these two almost identical functions into one
# however, i think keeping them separate is just a bit more neat
def get_lowest_winning(time: int, lower: int, upper: int, race: tuple[int, int]) -> int:
    if time == lower:
        return upper

    if is_time_winning(time, race):
        return get_lowest_winning((time + lower) // 2, lower, time, race)
    else:
        return get_lowest_winning((time + upper) // 2, time, upper, race)

def get_highest_winning(time: int, lower: int, upper: int, race: tuple[int, int]) -> int:
    if time == lower:
        return lower

    if is_time_winning(time, race):
        return get_highest_winning((time + upper) // 2, time, upper, race)
    else:
        return get_highest_winning((time + lower) // 2, lower, time, race)

def find_winning(holdTime: int, race: tuple[int, int]) -> int:
    raceTime = race[0]

    if is_time_winning(holdTime, race):
        return holdTime
    else:
        if (holdTime > 0):
            find_winning(holdTime - 1, race)
        if (holdTime < raceTime):
            find_winning(holdTime + 1, race)

def is_time_winning(holdTime: int, race: tuple[int, int]) -> bool:
    raceTime, dist = race
    speed = holdTime
    return (raceTime - holdTime) * speed > dist

def read_races(input: str) -> dict[int, int]:
    times = input[0].split(":")[1].strip().split()
    distances = input[1].split(":")[1].strip().split()
    races = dict(map(lambda i, j : (int(i), int(j)), times, distances))
    return races

def read_input(file_name: str) -> list[str]:
    with open(file_name, encoding="utf-8") as f:
        return f.readlines()

input = read_input("input.txt")
races = read_races(input)
res = 1
for race in races.items():
    firstWinning, lastWinning = process_race(race)
    res *= lastWinning - firstWinning + 1

print(res)