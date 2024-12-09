from collections import defaultdict, deque

# Lire les règles
# with open("input-small.txt", "r") as file:
with open("input-mine.txt", "r") as file:
    data = file.read().strip().split("\n\n")

rules = [tuple(map(int, line.split("|"))) for line in data[0].splitlines()]

# Parse the updates
updates = [list(map(int, update.split(","))) for update in data[1].splitlines()]

# Construire le graphe
graph = defaultdict(set)
all_pages = set()

for x, y in rules:
    graph[x].add(y)
    all_pages.update([x, y])


def build_graph(pages):
    graph = defaultdict(set)

    def valid_rule(r):
        x,y = r
        return x in pages and y in pages


    print("graph for", pages)
    loc_rules = list(filter(valid_rule, rules))
    print (loc_rules)

    for x, y in loc_rules:
        graph[x].add(y)

    return graph



print("Toutes les pages:", all_pages)
# assert 36 in all_pages
# assert 9999 in all_pages

def print_cycle(node, stack):
    print("cycle: ", node, end=" ")

    while True:
        x = stack.pop()
        print(x, end=" ")
        if x == node:
            break
    print()

def cherche_cycle(graph):
    for p in all_pages:
        init = p
        print("init is", init)
        break

    # init = all_pages[0]
    visited = set()


    def visit(x, stack):
        stack.append(x)
        for n in graph[x]:
            if n in stack:
                print_cycle(n, stack)
                exit(42)

            if n in visited:
                continue

            # precedent[n] = x
            visit(n,stack)
        stack.pop()

    stack = []
    visit(init, stack)



# cherche_cycle(graph)



# Vérifier l'absence de cycles (Kahn's algorithm)
def find_total_order(graph, pages):
    order = {}
    indegree = {node: 0 for node in pages}
    for node in graph:
        for neighbor in graph[node]:
            indegree[neighbor] += 1

    queue = deque([node for node in pages if indegree[node] == 0])
    visited = 0

    while queue:
        # Si plus d'un élément est disponible dans la queue, ce n'est pas un ordre total
        if len(queue) > 1:
            print( "multiple choice")
            assert False
            return "multiple choice"

        node = queue.popleft()
        visited += 1
        order[node] = visited ## order of visit
        for neighbor in graph[node]:
            indegree[neighbor] -= 1
            if indegree[neighbor] == 0:
                queue.append(neighbor)

    # Si tous les nœuds ont été visités et qu'il n'y avait jamais plus d'une option, c'est un ordre total
    if visited != len(pages):
        print("Visité", visited, "mais il y a", len(pages))
        assert False
        return "has cycle(s)"
    else:
        return order



fix = []

# Vérifier la comparabilité de toutes les paires
def is_comparable_pages(pages):

    graph = build_graph(pages)
    order = find_total_order(graph, pages)
    # print(graph)

    def dfs(start, target):
        stack = [start]
        visited = set()
        while stack:
            node = stack.pop()
            if node == target:
                return True
            if node not in visited:
                visited.add(node)
                stack.extend(graph[node])
        return False

    for x in pages:
        for y in pages:
            if x != y and not (dfs(x, y) or dfs(y, x)):
                return False

    print("checking pages", pages)
    mypages = list(map((lambda x: (order[x],x)), pages))
    mypages.sort()
    fix.append(mypages)
    return True

# Résultat
# ret = is_total_order(graph)
# if ret == "has cycle(s)":
    # print("Les règles ne forment pas un DAG : il existe un cycle.")
# elif ret == "multiple choice":
    # print("Les règles ne forment pas un ordre total : certaines paires ne sont pas comparables.")
# else:
    # print("Les règles forment un ordre total.")


# print("order topo", order)

# Function to check if an update is valid
def is_valid_update(update, rules):
    index_map = {page: i for i, page in enumerate(update)}
    for x, y in rules:
        if x in update and y in update:
            if index_map[x] > index_map[y]:
                return False
    return True



# Find valid updates and their middle pages
valid_middle_pages = []
broken = []
for update in updates:
    if is_valid_update(update, rules):
        print("Valide:", update)
        assert len(update) % 2 == 1
        middle_index = len(update) // 2
        # valid_middle_pages.append(sorted(update)[middle_index]) ## initial chatgpt
        valid_middle_pages.append(update[middle_index])
    else:
        broken.append(update)

# Sum the middle pages of valid updates
result = sum(valid_middle_pages)
print("résultat partie 1:", result)

for update in broken:
    is_comparable_pages(update)

print("Updates fixed")
somme = 0
for f in fix:
    assert len(f) % 2 == 1
    middle_index = len(f) // 2
    _, x = f[middle_index]
    somme += x

print("résultat partie 2:", somme)
