import json
import sys


obj = json.load(open(sys.argv[1]))
initial = obj["initial"]
transitions = []
for k, v in obj["transitions"].items():
    for t in v:
        transitions.append(
            ";"
            + k
            + t["read"]
            + ("." if t["to_state"] in obj["finals"] else t["to_state"])
            + t["write"]
            + ("-" if t["action"] == "LEFT" else "+")
        )
print(f"{initial}[{''.join(transitions)}]!")
