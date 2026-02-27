"""
Test script for the ClinicalTrialDataAgent.

Runs 3 example natural language queries against the AE dataset and prints results.
"""

import os
import sys

# Ensure the module is importable
sys.path.insert(0, os.path.dirname(__file__))

from clinical_data_agent import ClinicalTrialDataAgent

CSV_PATH = os.path.join(os.path.dirname(__file__), "adae.csv")


def main():
    print("=" * 70)
    print("Clinical Trial Data Agent - Test Script")
    print("=" * 70)

    # Initialize the agent (will use mock LLM if no API key is set)
    agent = ClinicalTrialDataAgent(CSV_PATH)

    # --- Example queries ------------------------------------------------------
    queries = [
        "Give me the subjects who had Adverse events of Moderate severity",
        "Which patients experienced cardiac disorders?",
        "Show me subjects with serious adverse events",
    ]

    for i, question in enumerate(queries, 1):
        print(f"\n{'─' * 70}")
        print(f"Example {i}:")
        print(f"{'─' * 70}")
        result = agent.query(question)
        print(result)
        print()


if __name__ == "__main__":
    main()
