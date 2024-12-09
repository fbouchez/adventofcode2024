#
# généré par chatgpt, correct du premier coup
#


def is_safe(report):
    differences = [report[i+1] - report[i] for i in range(len(report) - 1)]
    all_increasing = all(1 <= diff <= 3 for diff in differences)
    all_decreasing = all(-3 <= diff <= -1 for diff in differences)
    return all_increasing or all_decreasing

def is_safe_with_dampener(report):
    # Check if the report is already safe
    if is_safe(report):
        print("Safe:", report)
        return True
    
    # Try removing each level and check if the report becomes safe
    for i in range(len(report)):
        modified_report = report[:i] + report[i+1:]  # Remove level i
        if is_safe(modified_report):
            print("Safe:", report)
            return True
    
    # If no single removal makes the report safe, it's unsafe
    return False

# Read reports from the input file
with open("input-mine.txt") as file:
    reports = [[int(level) for level in line.split()] for line in file]

# Count the number of safe reports considering the Problem Dampener
safe_reports_with_dampener = sum(is_safe_with_dampener(report) for report in reports)

print("Nombre de rapports sûrs avec le Problem Dampener :", safe_reports_with_dampener)
