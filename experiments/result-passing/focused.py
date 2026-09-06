#!/usr/bin/env python3
"""Repeat class mapping in longer batches to check the noisy application result."""
from run import CASES, main

CASES.clear()
CASES["class_map"] = 3

if __name__ == "__main__":
    main()
