#!/usr/bin/env python3

with open("day1.txt") as f:
    cals = [line.strip() for line in f]

# part 1
cur = 0
max_so_far = 0

for cal in cals:
    if cal == '':
        cur = 0
    else:
        cur += int(cal)
        max_so_far = max(max_so_far, cur)

print(max_so_far)

# part 2
cur = 0
elves = []

for cal in cals:
    if cal == '':
        elves.append(cur)
        cur = 0
    else:
        cur += int(cal)
elves.append(cur)
print(sum(sorted(elves)[::-1][:3]))
