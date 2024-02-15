import json
import string

N = 4
LOW = list(string.ascii_lowercase[:N])
UPP = list(string.ascii_uppercase[:N])
PAIRS = [X + x for X in UPP for x in LOW]
DIRS = ["L", "R"]
# TODO: only TRIPLETS
TRIPLETS_SEPARATED = [p + d for d in DIRS for p in PAIRS]
TRIPLETS_TOGETHER = [p + d for p in PAIRS for d in DIRS]

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
            *[f"apply_{s}" for s in TRIPLETS_SEPARATED],
            *[f"write_{s}" for s in TRIPLETS_SEPARATED],
            *[f"move_left_{s}" for s in UPP],
            *[f"move_left_{s}" for s in PAIRS],
            *[f"move_right_{s}" for s in PAIRS],
            "HALT",
            "UNEXPECTED_TRANSITION",
            "TODO_NEG_POS",
        ],
        "initial": "start",
        "finals": ["HALT", "UNEXPECTED_TRANSITION", "TODO_NEG_POS"],
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
                for s1 in TRIPLETS_TOGETHER
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
            "move_left_A": [
                {
                    "read": "a",
                    "to_state": "move_left_Aa",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "b",
                    "to_state": "move_left_Ab",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "c",
                    "to_state": "move_left_Ac",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "d",
                    "to_state": "move_left_Ad",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "!",
                    "to_state": "move_left_A",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "]",
                    "to_state": "TODO_NEG_POS",
                    "write": "]",
                    "action": "RIGHT",
                },
            ],
            "move_left_B": [
                {
                    "read": "a",
                    "to_state": "move_left_Ba",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "b",
                    "to_state": "move_left_Bb",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "c",
                    "to_state": "move_left_Bc",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "d",
                    "to_state": "move_left_Bd",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "!",
                    "to_state": "move_left_B",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "]",
                    "to_state": "TODO_NEG_POS",
                    "write": "]",
                    "action": "RIGHT",
                },
            ],
            "move_left_C": [
                {
                    "read": "a",
                    "to_state": "move_left_Ca",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "b",
                    "to_state": "move_left_Cb",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "c",
                    "to_state": "move_left_Cc",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "d",
                    "to_state": "move_left_Cd",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "!",
                    "to_state": "move_left_C",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "]",
                    "to_state": "TODO_NEG_POS",
                    "write": "]",
                    "action": "RIGHT",
                },
            ],
            "move_left_D": [
                {
                    "read": "a",
                    "to_state": "move_left_Da",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "b",
                    "to_state": "move_left_Db",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "c",
                    "to_state": "move_left_Dc",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "d",
                    "to_state": "move_left_Dd",
                    "write": "!",
                    "action": "RIGHT",
                },
                {
                    "read": "!",
                    "to_state": "move_left_D",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "]",
                    "to_state": "TODO_NEG_POS",
                    "write": "]",
                    "action": "RIGHT",
                },
            ],
            "move_left_Aa": [
                {"read": "!", "to_state": "state_A", "write": "a", "action": "LEFT"}
            ],
            "move_left_Ab": [
                {"read": "!", "to_state": "state_A", "write": "b", "action": "LEFT"}
            ],
            "move_left_Ac": [
                {"read": "!", "to_state": "state_A", "write": "c", "action": "LEFT"}
            ],
            "move_left_Ad": [
                {"read": "!", "to_state": "state_A", "write": "d", "action": "LEFT"}
            ],
            "move_left_Ba": [
                {"read": "!", "to_state": "state_B", "write": "a", "action": "LEFT"}
            ],
            "move_left_Bb": [
                {"read": "!", "to_state": "state_B", "write": "b", "action": "LEFT"}
            ],
            "move_left_Bc": [
                {"read": "!", "to_state": "state_B", "write": "c", "action": "LEFT"}
            ],
            "move_left_Bd": [
                {"read": "!", "to_state": "state_B", "write": "d", "action": "LEFT"}
            ],
            "move_left_Ca": [
                {"read": "!", "to_state": "state_C", "write": "a", "action": "LEFT"}
            ],
            "move_left_Cb": [
                {"read": "!", "to_state": "state_C", "write": "b", "action": "LEFT"}
            ],
            "move_left_Cc": [
                {"read": "!", "to_state": "state_C", "write": "c", "action": "LEFT"}
            ],
            "move_left_Cd": [
                {"read": "!", "to_state": "state_C", "write": "d", "action": "LEFT"}
            ],
            "move_left_Da": [
                {"read": "!", "to_state": "state_D", "write": "a", "action": "LEFT"}
            ],
            "move_left_Db": [
                {"read": "!", "to_state": "state_D", "write": "b", "action": "LEFT"}
            ],
            "move_left_Dc": [
                {"read": "!", "to_state": "state_D", "write": "c", "action": "LEFT"}
            ],
            "move_left_Dd": [
                {"read": "!", "to_state": "state_D", "write": "d", "action": "LEFT"}
            ],
            "write_AaR": [
                {
                    "read": " ",
                    "to_state": "move_right_Aa",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Aa",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Aa",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Aa",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Aa",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_AbR": [
                {
                    "read": " ",
                    "to_state": "move_right_Ab",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Ab",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Ab",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Ab",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Ab",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_AcR": [
                {
                    "read": " ",
                    "to_state": "move_right_Ac",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Ac",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Ac",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Ac",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Ac",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_AdR": [
                {
                    "read": " ",
                    "to_state": "move_right_Ad",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Ad",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Ad",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Ad",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Ad",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_BaR": [
                {
                    "read": " ",
                    "to_state": "move_right_Ba",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Ba",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Ba",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Ba",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Ba",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_BbR": [
                {
                    "read": " ",
                    "to_state": "move_right_Bb",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Bb",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Bb",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Bb",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Bb",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_BcR": [
                {
                    "read": " ",
                    "to_state": "move_right_Bc",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Bc",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Bc",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Bc",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Bc",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_BdR": [
                {
                    "read": " ",
                    "to_state": "move_right_Bd",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Bd",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Bd",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Bd",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Bd",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_CaR": [
                {
                    "read": " ",
                    "to_state": "move_right_Ca",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Ca",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Ca",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Ca",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Ca",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_CbR": [
                {
                    "read": " ",
                    "to_state": "move_right_Cb",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Cb",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Cb",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Cb",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Cb",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_CcR": [
                {
                    "read": " ",
                    "to_state": "move_right_Cc",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Cc",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Cc",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Cc",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Cc",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_CdR": [
                {
                    "read": " ",
                    "to_state": "move_right_Cd",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Cd",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Cd",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Cd",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Cd",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_DaR": [
                {
                    "read": " ",
                    "to_state": "move_right_Da",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Da",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Da",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Da",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Da",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_DbR": [
                {
                    "read": " ",
                    "to_state": "move_right_Db",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Db",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Db",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Db",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Db",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_DcR": [
                {
                    "read": " ",
                    "to_state": "move_right_Dc",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Dc",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Dc",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Dc",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Dc",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "write_DdR": [
                {
                    "read": " ",
                    "to_state": "move_right_Dd",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "a",
                    "to_state": "move_right_Dd",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "b",
                    "to_state": "move_right_Dd",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "c",
                    "to_state": "move_right_Dd",
                    "write": "!",
                    "action": "LEFT",
                },
                {
                    "read": "d",
                    "to_state": "move_right_Dd",
                    "write": "!",
                    "action": "LEFT",
                },
            ],
            "move_right_Aa": [
                {"read": "!", "to_state": "state_A", "write": "a", "action": "RIGHT"}
            ],
            "move_right_Ab": [
                {"read": "!", "to_state": "state_A", "write": "b", "action": "RIGHT"}
            ],
            "move_right_Ac": [
                {"read": "!", "to_state": "state_A", "write": "c", "action": "RIGHT"}
            ],
            "move_right_Ad": [
                {"read": "!", "to_state": "state_A", "write": "d", "action": "RIGHT"}
            ],
            "move_right_Ba": [
                {"read": "!", "to_state": "state_B", "write": "a", "action": "RIGHT"}
            ],
            "move_right_Bb": [
                {"read": "!", "to_state": "state_B", "write": "b", "action": "RIGHT"}
            ],
            "move_right_Bc": [
                {"read": "!", "to_state": "state_B", "write": "c", "action": "RIGHT"}
            ],
            "move_right_Bd": [
                {"read": "!", "to_state": "state_B", "write": "d", "action": "RIGHT"}
            ],
            "move_right_Ca": [
                {"read": "!", "to_state": "state_C", "write": "a", "action": "RIGHT"}
            ],
            "move_right_Cb": [
                {"read": "!", "to_state": "state_C", "write": "b", "action": "RIGHT"}
            ],
            "move_right_Cc": [
                {"read": "!", "to_state": "state_C", "write": "c", "action": "RIGHT"}
            ],
            "move_right_Cd": [
                {"read": "!", "to_state": "state_C", "write": "d", "action": "RIGHT"}
            ],
            "move_right_Da": [
                {"read": "!", "to_state": "state_D", "write": "a", "action": "RIGHT"}
            ],
            "move_right_Db": [
                {"read": "!", "to_state": "state_D", "write": "b", "action": "RIGHT"}
            ],
            "move_right_Dc": [
                {"read": "!", "to_state": "state_D", "write": "c", "action": "RIGHT"}
            ],
            "move_right_Dd": [
                {"read": "!", "to_state": "state_D", "write": "d", "action": "RIGHT"}
            ],
        },
    },
    open("okok.json", "w"),
    indent=4,
)
