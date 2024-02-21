import json
import string

DIRS = ["L", "R"]
UTM_SIZES = [2, 4, 10]
for n in UTM_SIZES:
    LOW = list(string.ascii_lowercase[:n])
    UPP = list(string.ascii_uppercase[:n])
    PAIRS = [X + x for X in UPP for x in LOW]
    TRIPLETS = [p + d for d in DIRS for p in PAIRS]
    json.dump(
        {
            "name": "universal_turing_machine",
            "alphabet": [" ", "[", "]", ";", ".", "!", "+", "-"] + UPP + LOW,
            "blank": " ",
            "states": [
                "start",
                *[f"state_{s}" for s in UPP],
                *[f"read_{s}" for s in UPP],
                *[f"state_{s}" for s in PAIRS],
                *[f"check_state_{s}" for s in PAIRS],
                *[f"check_char_{s}" for s in PAIRS],
                *[f"skip_semicolon_{s}" for s in PAIRS],
                "get_new_state",
                "last_write",
                *[f"last_write_{s}" for s in LOW],
                *[f"finish_{s}" for s in LOW],
                *[f"get_new_char_{s}" for s in UPP],
                *[f"get_new_dir_{s}" for s in PAIRS],
                *[f"apply_{s}" for s in TRIPLETS],
                *[f"write_{s}" for s in TRIPLETS],
                *[f"move_left_{s}" for s in UPP],
                *[f"move_left_{s}" for s in PAIRS],
                *[f"move_right_{s}" for s in PAIRS],
                "HALT",
                "UNEXPECTED_TRANSITION",
            ],
            "initial": "start",
            "finals": ["HALT", "UNEXPECTED_TRANSITION"],
            "transitions": {
                "start": [
                    {"read": s, "to_state": f"state_{s}", "write": s, "action": "RIGHT"}
                    for s in UPP
                ],
                **{
                    f"state_{s1}": [
                        *[
                            {
                                "read": s2,
                                "to_state": f"state_{s1}",
                                "write": s2,
                                "action": "RIGHT",
                            }
                            for s2 in UPP + LOW
                        ],
                        {
                            "read": ".",
                            "to_state": f"state_{s1}",
                            "write": ".",
                            "action": "RIGHT",
                        },
                        {
                            "read": "-",
                            "to_state": f"state_{s1}",
                            "write": "-",
                            "action": "RIGHT",
                        },
                        {
                            "read": "+",
                            "to_state": f"state_{s1}",
                            "write": "+",
                            "action": "RIGHT",
                        },
                        {
                            "read": ";",
                            "to_state": f"state_{s1}",
                            "write": ";",
                            "action": "RIGHT",
                        },
                        {
                            "read": "[",
                            "to_state": f"state_{s1}",
                            "write": "[",
                            "action": "RIGHT",
                        },
                        {
                            "read": "]",
                            "to_state": f"state_{s1}",
                            "write": "]",
                            "action": "RIGHT",
                        },
                        {
                            "read": "!",
                            "to_state": f"read_{s1}",
                            "write": "!",
                            "action": "RIGHT",
                        },
                    ]
                    for s1 in UPP
                },
                **{
                    f"read_{s1}": [
                        {
                            "read": " ",
                            "to_state": f"state_{s1}a",
                            "write": "a",
                            "action": "LEFT",
                        },
                        *[
                            {
                                "read": s2,
                                "to_state": f"state_{s1}{s2}",
                                "write": s2,
                                "action": "LEFT",
                            }
                            for s2 in LOW
                        ],
                    ]
                    for s1 in UPP
                },
                **{
                    f"state_{s1}": [
                        *[
                            {
                                "read": s2,
                                "to_state": f"state_{s1}",
                                "write": s2,
                                "action": "LEFT",
                            }
                            for s2 in UPP + LOW + list(".-+]!")
                        ],
                        {
                            "read": ";",
                            "to_state": f"check_state_{s1}",
                            "write": ";",
                            "action": "RIGHT",
                        },
                        {
                            "read": "[",
                            "to_state": "UNEXPECTED_TRANSITION",
                            "write": "[",
                            "action": "LEFT",
                        },
                    ]
                    for s1 in PAIRS
                },
                **{
                    f"check_state_{s1}": [
                        {
                            "read": s2,
                            "to_state": f"check_char_{s1}",
                            "write": s2,
                            "action": "RIGHT",
                        }
                        if s2 == s1[0]
                        else {
                            "read": s2,
                            "to_state": f"skip_semicolon_{s1}",
                            "write": s2,
                            "action": "LEFT",
                        }
                        for s2 in UPP
                    ]
                    for s1 in PAIRS
                },
                **{
                    f"check_char_{s1}": [
                        {
                            "read": s2,
                            "to_state": "get_new_state",
                            "write": s2,
                            "action": "RIGHT",
                        }
                        if s2 == s1[1]
                        else {
                            "read": s2,
                            "to_state": f"skip_semicolon_{s1}",
                            "write": s2,
                            "action": "LEFT",
                        }
                        for s2 in LOW
                    ]
                    for s1 in PAIRS
                },
                **{
                    f"skip_semicolon_{s1}": [
                        *[
                            {
                                "read": s2,
                                "to_state": f"skip_semicolon_{s1}",
                                "write": s2,
                                "action": "LEFT",
                            }
                            for s2 in UPP
                        ],
                        {
                            "read": ";",
                            "to_state": f"state_{s1}",
                            "write": ";",
                            "action": "LEFT",
                        },
                    ]
                    for s1 in PAIRS
                },
                "get_new_state": [
                    *[
                        {
                            "read": s,
                            "to_state": f"get_new_char_{s}",
                            "write": s,
                            "action": "RIGHT",
                        }
                        for s in UPP
                    ],
                    {
                        "read": ".",
                        "to_state": "last_write",
                        "write": ".",
                        "action": "RIGHT",
                    },
                ],
                "last_write": [
                    {
                        "read": s,
                        "to_state": f"last_write_{s}",
                        "write": s,
                        "action": "RIGHT",
                    }
                    for s in LOW
                ],
                **{
                    f"last_write_{s1}": [
                        *[
                            {
                                "read": s2,
                                "to_state": f"last_write_{s1}",
                                "write": s2,
                                "action": "RIGHT",
                            }
                            for s2 in list(";]") + UPP + LOW + list(".-+")
                        ],
                        {
                            "read": "!",
                            "to_state": f"finish_{s1}",
                            "write": "!",
                            "action": "RIGHT",
                        },
                    ]
                    for s1 in LOW
                },
                **{
                    f"finish_{s1}": [
                        {"read": s2, "to_state": "HALT", "write": s1, "action": "RIGHT"}
                        for s2 in [" "] + LOW
                    ]
                    for s1 in LOW
                },
                **{
                    f"get_new_char_{s1}": [
                        {
                            "read": s2,
                            "to_state": f"get_new_dir_{s1}{s2}",
                            "write": s2,
                            "action": "RIGHT",
                        }
                        for s2 in LOW
                    ]
                    for s1 in UPP
                },
                **{
                    f"get_new_dir_{s1}": [
                        {
                            "read": "-",
                            "to_state": f"apply_{s1}L",
                            "write": "-",
                            "action": "RIGHT",
                        },
                        {
                            "read": "+",
                            "to_state": f"apply_{s1}R",
                            "write": "+",
                            "action": "RIGHT",
                        },
                    ]
                    for s1 in PAIRS
                },
                **{
                    f"apply_{s1}": [
                        *[
                            {
                                "read": s2,
                                "to_state": f"apply_{s1}",
                                "write": s2,
                                "action": "RIGHT",
                            }
                            for s2 in list(";]") + UPP + LOW + list(".-+")
                        ],
                        {
                            "read": "!",
                            "to_state": f"write_{s1}",
                            "write": "!",
                            "action": "RIGHT",
                        },
                    ]
                    for s1 in TRIPLETS
                },
                **{
                    f"write_{s1}L": [
                        *[
                            {
                                "read": s2,
                                "to_state": f"move_left_{s1[0]}",
                                "write": s1[1],
                                "action": "LEFT",
                            }
                            for s2 in [" "] + LOW
                        ]
                    ]
                    for s1 in PAIRS
                },
                **{
                    f"move_left_{s1}": [
                        *[
                            {
                                "read": s2,
                                "to_state": f"move_left_{s1}{s2}",
                                "write": "!",
                                "action": "RIGHT",
                            }
                            for s2 in LOW
                        ],
                        {
                            "read": "!",
                            "to_state": f"move_left_{s1}",
                            "write": "!",
                            "action": "LEFT",
                        },
                        {
                            "read": "]",
                            "to_state": "HALT",
                            "write": "]",
                            "action": "RIGHT",
                        },
                    ]
                    for s1 in UPP
                },
                **{
                    f"move_left_{s}": [
                        {
                            "read": "!",
                            "to_state": f"state_{s[0]}",
                            "write": s[1],
                            "action": "LEFT",
                        }
                    ]
                    for s in PAIRS
                },
                **{
                    f"write_{s1}R": [
                        {
                            "read": s2,
                            "to_state": f"move_right_{s1}",
                            "write": "!",
                            "action": "LEFT",
                        }
                        for s2 in [" "] + LOW
                    ]
                    for s1 in PAIRS
                },
                **{
                    f"move_right_{s}": [
                        {
                            "read": "!",
                            "to_state": f"state_{s[0]}",
                            "write": s[1],
                            "action": "RIGHT",
                        }
                    ]
                    for s in PAIRS
                },
            },
        },
        open(f"tests/resources/valid/utm{n}.json", "w"),
        indent=4,
    )
