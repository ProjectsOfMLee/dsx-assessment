"""
Question 6: GenAI Clinical Data Assistant (LLM & LangChain)

Translates natural language queries about adverse events into structured
Pandas filters using an LLM (OpenAI via LangChain) or a mock fallback.

Usage:
    agent = ClinicalTrialDataAgent("adae.csv")
    result = agent.query("Give me subjects with severe adverse events")
"""

import json
import os
import re
from dataclasses import dataclass
from typing import Optional

import pandas as pd
from pydantic import BaseModel, Field


# --- Schema definition for the LLM -------------------------------------------

AE_SCHEMA = """
The ADAE (Adverse Events) dataset contains one row per adverse event per subject.
Key columns for filtering:

- USUBJID: Unique subject identifier (e.g., "01-701-1015"). Used to identify patients.
- AETERM: Reported term for the adverse event, uppercase (e.g., "HEADACHE", "DIARRHOEA", "APPLICATION SITE PRURITUS").
- AEDECOD: Standardized/decoded adverse event term, uppercase (e.g., "HEADACHE", "DIARRHOEA").
- AESOC: Primary System Organ Class, uppercase (e.g., "CARDIAC DISORDERS", "NERVOUS SYSTEM DISORDERS", "SKIN AND SUBCUTANEOUS TISSUE DISORDERS", "GASTROINTESTINAL DISORDERS").
- AESEV: Severity/intensity of the AE. Values: "MILD", "MODERATE", "SEVERE".
- AEREL: Causality/relationship to study drug. Values: "NONE", "REMOTE", "POSSIBLE", "PROBABLE".
- AESER: Serious adverse event flag. Values: "Y" (yes), "N" (no).
- AEOUT: Outcome of the AE (e.g., "RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED").
- AESTDTC: Start date of the adverse event (ISO 8601 format, e.g., "2014-01-03").
- AEENDTC: End date of the adverse event (ISO 8601 format).
- AEBODSYS: Body system or organ class (similar to AESOC).

All text values are UPPERCASE. When filtering, always convert the filter value to uppercase.
"""

SYSTEM_PROMPT = f"""You are a clinical data query assistant. Given a natural language
question about adverse events, extract the filtering intent and return a JSON object.

Dataset schema:
{AE_SCHEMA}

Rules:
1. Identify which column to filter on (target_column).
2. Extract the value to filter for (filter_value). Always UPPERCASE.
3. Use case-insensitive partial matching when the user's term might not exactly match.
4. For severity questions ("mild", "moderate", "severe") -> target_column = "AESEV".
5. For specific conditions/terms ("headache", "nausea") -> target_column = "AETERM".
6. For body systems ("cardiac", "skin", "nervous") -> target_column = "AESOC".
7. For causality/relationship ("related", "probable", "possible") -> target_column = "AEREL".
8. For serious events ("serious", "SAE") -> target_column = "AESER", filter_value = "Y".

Return ONLY a JSON object with exactly these keys:
{{"target_column": "<column_name>", "filter_value": "<value>"}}
"""


# --- Structured output model --------------------------------------------------

class QueryIntent(BaseModel):
    """Structured output from the LLM: which column to filter and what value."""
    target_column: str = Field(description="The ADAE column to filter on")
    filter_value: str = Field(description="The value to search for (uppercase)")


# --- Query result -------------------------------------------------------------

@dataclass
class QueryResult:
    """Result of executing a query against the AE dataframe."""
    query: str
    target_column: str
    filter_value: str
    n_subjects: int
    subject_ids: list[str]

    def __str__(self) -> str:
        ids_display = ", ".join(self.subject_ids[:10])
        if len(self.subject_ids) > 10:
            ids_display += f", ... ({len(self.subject_ids) - 10} more)"
        return (
            f"Query: {self.query}\n"
            f"Filter: {self.target_column} contains '{self.filter_value}'\n"
            f"Unique subjects: {self.n_subjects}\n"
            f"Subject IDs: [{ids_display}]"
        )


# --- Mock LLM for when no API key is available --------------------------------

class MockLLM:
    """
    Rule-based fallback that mimics LLM behavior for common query patterns.
    Used when no OpenAI API key is configured.
    """

    KEYWORD_MAP = {
        # Severity keywords
        "severe": ("AESEV", "SEVERE"),
        "mild": ("AESEV", "MILD"),
        "moderate": ("AESEV", "MODERATE"),
        "severity": ("AESEV", None),
        "intensity": ("AESEV", None),
        # Seriousness
        "serious": ("AESER", "Y"),
        "sae": ("AESER", "Y"),
        # Causality
        "related": ("AEREL", "PROBABLE"),
        "probable": ("AEREL", "PROBABLE"),
        "possible": ("AEREL", "POSSIBLE"),
        "remote": ("AEREL", "REMOTE"),
        "causality": ("AEREL", None),
        "relationship": ("AEREL", None),
        # Body systems
        "cardiac": ("AESOC", "CARDIAC DISORDERS"),
        "heart": ("AESOC", "CARDIAC DISORDERS"),
        "skin": ("AESOC", "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"),
        "dermatolog": ("AESOC", "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"),
        "nervous": ("AESOC", "NERVOUS SYSTEM DISORDERS"),
        "neurolog": ("AESOC", "NERVOUS SYSTEM DISORDERS"),
        "gastrointestinal": ("AESOC", "GASTROINTESTINAL DISORDERS"),
        "gi ": ("AESOC", "GASTROINTESTINAL DISORDERS"),
        "respiratory": ("AESOC", "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS"),
        "psychiatric": ("AESOC", "PSYCHIATRIC DISORDERS"),
        "infection": ("AESOC", "INFECTIONS AND INFESTATIONS"),
        "eye": ("AESOC", "EYE DISORDERS"),
        "renal": ("AESOC", "RENAL AND URINARY DISORDERS"),
        "vascular": ("AESOC", "VASCULAR DISORDERS"),
        "hepato": ("AESOC", "HEPATOBILIARY DISORDERS"),
        "musculoskeletal": ("AESOC", "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS"),
    }

    # Common AE terms for direct matching
    COMMON_TERMS = [
        "HEADACHE", "NAUSEA", "DIARRHOEA", "DIZZINESS", "VOMITING",
        "FATIGUE", "RASH", "PRURITUS", "ERYTHEMA", "INSOMNIA",
        "APPLICATION SITE", "BRADYCARDIA", "COUGH", "PAIN",
    ]

    def parse(self, question: str) -> QueryIntent:
        q_lower = question.lower()

        # Check keyword map first (severity, seriousness, causality, body systems)
        for keyword, (col, val) in self.KEYWORD_MAP.items():
            if keyword in q_lower:
                if val is not None:
                    return QueryIntent(target_column=col, filter_value=val)
                # For generic keywords like "severity", try to extract the value
                for sev in ["SEVERE", "MODERATE", "MILD"]:
                    if sev.lower() in q_lower:
                        return QueryIntent(target_column=col, filter_value=sev)

        # Check for specific AE terms
        for term in self.COMMON_TERMS:
            if term.lower() in q_lower:
                return QueryIntent(target_column="AETERM", filter_value=term)

        # Default: treat the main noun as an AETERM search
        # Extract the most likely clinical term from the question
        words = re.sub(r"[^a-zA-Z\s]", "", question).upper().split()
        # Remove common stop words
        stop_words = {
            "GIVE", "ME", "THE", "SUBJECTS", "WHO", "HAD", "HAVE", "WITH",
            "ADVERSE", "EVENTS", "EVENT", "AE", "AES", "OF", "A", "AN",
            "ANY", "ALL", "SHOW", "LIST", "FIND", "GET", "PATIENTS",
            "WHAT", "WHICH", "HOW", "MANY", "ARE", "THERE", "THAT",
        }
        clinical_words = [w for w in words if w not in stop_words and len(w) > 2]
        if clinical_words:
            return QueryIntent(
                target_column="AETERM",
                filter_value=" ".join(clinical_words),
            )

        return QueryIntent(target_column="AETERM", filter_value=question.upper())


# --- Main agent class ---------------------------------------------------------

class ClinicalTrialDataAgent:
    """
    Translates natural language queries into Pandas filters on the AE dataset.

    Uses an LLM (OpenAI via LangChain) when an API key is available,
    otherwise falls back to a rule-based mock.
    """

    def __init__(self, csv_path: str, api_key: Optional[str] = None):
        self.df = pd.read_csv(csv_path)
        self.api_key = api_key or os.environ.get("OPENAI_API_KEY")
        self.llm = self._init_llm()

    def _init_llm(self):
        """Initialize LLM or fall back to mock."""
        if self.api_key:
            try:
                from langchain_openai import ChatOpenAI

                llm = ChatOpenAI(
                    model="gpt-4o-mini",
                    temperature=0,
                    api_key=self.api_key,
                )
                print("Using OpenAI LLM (gpt-4o-mini)")
                return llm
            except Exception as e:
                print(f"Failed to initialize OpenAI LLM: {e}")
                print("Falling back to mock LLM.")
                return MockLLM()
        else:
            print("No OpenAI API key found. Using mock LLM.")
            return MockLLM()

    def parse_question(self, question: str) -> QueryIntent:
        """
        Parse a natural language question into a structured query intent.

        Uses the LLM to extract target_column and filter_value from the question.
        """
        if isinstance(self.llm, MockLLM):
            return self.llm.parse(question)

        # Real LLM path: send the question with schema context
        from langchain_core.messages import HumanMessage, SystemMessage

        messages = [
            SystemMessage(content=SYSTEM_PROMPT),
            HumanMessage(content=question),
        ]

        response = self.llm.invoke(messages)
        content = response.content.strip()

        # Extract JSON from response (handle markdown code blocks)
        json_match = re.search(r"\{[^}]+\}", content)
        if json_match:
            parsed = json.loads(json_match.group())
            return QueryIntent(**parsed)

        raise ValueError(f"Could not parse LLM response: {content}")

    def execute_filter(self, intent: QueryIntent) -> pd.DataFrame:
        """
        Apply the parsed query intent as a Pandas filter on the AE dataframe.

        Uses case-insensitive partial matching (str.contains) to handle
        variations in how terms appear in the data.
        """
        col = intent.target_column
        val = intent.filter_value.upper()

        if col not in self.df.columns:
            raise ValueError(
                f"Column '{col}' not found in dataset. "
                f"Available: {list(self.df.columns)}"
            )

        # For categorical columns with exact values, use exact match
        exact_match_cols = {"AESEV", "AESER", "AEREL"}
        if col in exact_match_cols:
            mask = self.df[col].str.upper() == val
        else:
            # For text columns (AETERM, AESOC, etc.), use partial matching
            mask = self.df[col].str.upper().str.contains(val, na=False)

        return self.df[mask]

    def query(self, question: str) -> QueryResult:
        """
        End-to-end: parse a natural language question and return matching subjects.

        Steps:
        1. LLM parses the question into target_column + filter_value
        2. Pandas filter is applied to the AE dataframe
        3. Unique subject IDs and count are returned
        """
        # Step 1: Parse question via LLM
        intent = self.parse_question(question)

        # Step 2: Execute Pandas filter
        filtered = self.execute_filter(intent)

        # Step 3: Extract unique subjects
        subject_ids = sorted(filtered["USUBJID"].unique().tolist())

        return QueryResult(
            query=question,
            target_column=intent.target_column,
            filter_value=intent.filter_value,
            n_subjects=len(subject_ids),
            subject_ids=subject_ids,
        )
