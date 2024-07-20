def process_race(race: tuple[int, int]) -> tuple[int, int]:
    time = race[0]
    winningTime = find_winning(time // 2, race)
    firstWinning = getLowestWinning(winningTime // 2, 0, winningTime, race)
    lastWinning = getHighestWinning((winningTime + time) // 2, winningTime, time, race)
    return (firstWinning, lastWinning)

# of course it would be possible to unite these two almost identical functions into one
# however, i think keeping them separate is just a bit more neat
def getLowestWinning(time: int, lower: int, upper: int, race: tuple[int, int]) -> int:
    if time == lower:
        return upper

    if is_time_winning(time, race):
        return getLowestWinning((time + lower) // 2, lower, time, race)
    else:
        return getLowestWinning((time + upper) // 2, time, upper, race)

def getHighestWinning(time: int, lower: int, upper: int, race: tuple[int, int]) -> int:
    if time == lower:
        return lower

    if is_time_winning(time, race):
        return getHighestWinning((time + upper) // 2, time, upper, race)
    else:
        return getHighestWinning((time + lower) // 2, lower, time, race)

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