#
# généré par chatgpt, puis modifié pour debug
# et rendre correct
#

def is_safe(report):
    n = len(report)
    if n <= 2:
        return True  # Always safe if 2 or fewer elements

    # Compute differences
    differences = [report[i+1] - report[i] for i in range(n-1)]

    # Check monotonicity
    is_increasing = all(1 <= diff <= 3 for diff in differences)
    is_decreasing = all(-3 <= diff <= -1 for diff in differences)

    if is_increasing or is_decreasing:
        return True, []
    else:
        return False, differences




def is_safe_with_dampener(report):
    # if report[0] == 36:
        # breakpoint()
    safe, differences =  is_safe(report)
    if safe:
        print("Safe:", report)
        return True

    assert len(differences) >= 3

    countup = 0
    countdown = 0
    for i in range(3):
        if differences[i] < 0:
            countdown += 1
        else:
            countup += 1

    pot_increasing = countup >= 2

    # Identify violations
    if pot_increasing:
        violations = [i for i, diff in enumerate(differences) if not (1 <= diff <= 3)]
    else:
        violations = [i for i, diff in enumerate(differences) if not (-3 <= diff <= -1)]

    if len(violations) > 2:
        return False  # More than one violation cannot be fixed with the dampener

    assert len(violations) >= 1

    # Check if removing one level fixes the issue
    v = violations[0]

    # Simulate removing `report[v]` or `report[v+1]` and check monotonicity
    for k in range(-1,2):
        if v+k < 0 or v+k >= len(report):
            continue
        new_report = report[0:v+k] + report[v+k+1:]
        safe,_ = is_safe(new_report)
        if safe:
            print("Safe:", report)
            return True

    return False

# Read reports from the input file
# with open("input-small.txt") as file:
with open("input-mine.txt") as file:
    reports = [[int(level) for level in line.split()] for line in file]

# Count the number of safe reports considering the Problem Dampener
safe_reports_with_dampener = sum(is_safe_with_dampener(report) for report in reports)

print("Nombre de rapports sûrs avec le Problem Dampener :", safe_reports_with_dampener)
