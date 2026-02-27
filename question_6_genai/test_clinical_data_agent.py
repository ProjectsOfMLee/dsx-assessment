"""
Comprehensive test suite for Question 6: GenAI Clinical Data Assistant.

Tests MockLLM parsing, execute_filter, end-to-end query, and edge cases.

Run: pytest test_clinical_data_agent.py -v
"""

import os
from pathlib import Path

import pandas as pd
import pytest

from clinical_data_agent import (
    ClinicalTrialDataAgent,
    MockLLM,
    QueryIntent,
    QueryResult,
)

CSV_PATH = str(Path(__file__).parent / "adae.csv")

# Load data once for verification
_df = pd.read_csv(CSV_PATH, low_memory=False)


@pytest.fixture
def agent():
    """Agent using mock LLM (no API key)."""
    return ClinicalTrialDataAgent(CSV_PATH, api_key=None)


@pytest.fixture
def mock_llm():
    return MockLLM()


# ==============================================================================
# 1. MockLLM PARSING: SEVERITY
# ==============================================================================


class TestMockLLMSeverity:
    def test_parse_severe(self, mock_llm):
        intent = mock_llm.parse("Give me subjects with severe adverse events")
        assert intent.target_column == "AESEV"
        assert intent.filter_value == "SEVERE"

    def test_parse_mild(self, mock_llm):
        intent = mock_llm.parse("Show mild AEs")
        assert intent.target_column == "AESEV"
        assert intent.filter_value == "MILD"

    def test_parse_moderate(self, mock_llm):
        intent = mock_llm.parse("Adverse events of Moderate severity")
        assert intent.target_column == "AESEV"
        assert intent.filter_value == "MODERATE"

    def test_parse_severity_case_insensitive(self, mock_llm):
        intent = mock_llm.parse("SEVERE events")
        assert intent.target_column == "AESEV"
        assert intent.filter_value == "SEVERE"


# ==============================================================================
# 2. MockLLM PARSING: SERIOUSNESS
# ==============================================================================


class TestMockLLMSeriousness:
    def test_parse_serious(self, mock_llm):
        intent = mock_llm.parse("Show me subjects with serious adverse events")
        assert intent.target_column == "AESER"
        assert intent.filter_value == "Y"

    def test_parse_sae(self, mock_llm):
        intent = mock_llm.parse("List all SAE records")
        assert intent.target_column == "AESER"
        assert intent.filter_value == "Y"


# ==============================================================================
# 3. MockLLM PARSING: CAUSALITY
# ==============================================================================


class TestMockLLMCausality:
    def test_parse_probable(self, mock_llm):
        intent = mock_llm.parse("Which AEs are probably related?")
        assert intent.target_column == "AEREL"
        assert intent.filter_value == "PROBABLE"

    def test_parse_possible(self, mock_llm):
        intent = mock_llm.parse("Show possible causality events")
        assert intent.target_column == "AEREL"
        assert intent.filter_value == "POSSIBLE"

    def test_parse_remote(self, mock_llm):
        intent = mock_llm.parse("Find remote relationship AEs")
        assert intent.target_column == "AEREL"
        assert intent.filter_value == "REMOTE"

    def test_parse_related(self, mock_llm):
        intent = mock_llm.parse("Show drug-related adverse events")
        assert intent.target_column == "AEREL"
        assert intent.filter_value == "PROBABLE"


# ==============================================================================
# 4. MockLLM PARSING: BODY SYSTEMS (AESOC)
# ==============================================================================


class TestMockLLMBodySystems:
    def test_parse_cardiac(self, mock_llm):
        intent = mock_llm.parse("Which patients experienced cardiac disorders?")
        assert intent.target_column == "AESOC"
        assert intent.filter_value == "CARDIAC DISORDERS"

    def test_parse_heart(self, mock_llm):
        intent = mock_llm.parse("Show heart disorder events")
        assert intent.target_column == "AESOC"
        assert intent.filter_value == "CARDIAC DISORDERS"

    def test_parse_skin(self, mock_llm):
        intent = mock_llm.parse("Skin adverse events")
        assert intent.target_column == "AESOC"
        assert intent.filter_value == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"

    def test_parse_nervous(self, mock_llm):
        intent = mock_llm.parse("Nervous system disorders")
        assert intent.target_column == "AESOC"
        assert intent.filter_value == "NERVOUS SYSTEM DISORDERS"

    def test_parse_gastrointestinal(self, mock_llm):
        intent = mock_llm.parse("Gastrointestinal events")
        assert intent.target_column == "AESOC"
        assert intent.filter_value == "GASTROINTESTINAL DISORDERS"

    def test_parse_respiratory(self, mock_llm):
        intent = mock_llm.parse("Respiratory disorders")
        assert intent.target_column == "AESOC"
        assert intent.filter_value == "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS"

    def test_parse_psychiatric(self, mock_llm):
        intent = mock_llm.parse("Psychiatric events")
        assert intent.target_column == "AESOC"
        assert intent.filter_value == "PSYCHIATRIC DISORDERS"

    def test_parse_infection(self, mock_llm):
        intent = mock_llm.parse("Any infection events?")
        assert intent.target_column == "AESOC"
        assert intent.filter_value == "INFECTIONS AND INFESTATIONS"

    def test_parse_eye(self, mock_llm):
        intent = mock_llm.parse("Eye disorders")
        assert intent.target_column == "AESOC"
        assert intent.filter_value == "EYE DISORDERS"

    def test_parse_renal(self, mock_llm):
        intent = mock_llm.parse("Renal events")
        assert intent.target_column == "AESOC"
        assert intent.filter_value == "RENAL AND URINARY DISORDERS"

    def test_parse_vascular(self, mock_llm):
        intent = mock_llm.parse("Vascular disorders")
        assert intent.target_column == "AESOC"
        assert intent.filter_value == "VASCULAR DISORDERS"


# ==============================================================================
# 5. MockLLM PARSING: SPECIFIC AE TERMS
# ==============================================================================


class TestMockLLMAETerms:
    def test_parse_headache(self, mock_llm):
        intent = mock_llm.parse("Subjects with headache")
        assert intent.target_column == "AETERM"
        assert intent.filter_value == "HEADACHE"

    def test_parse_diarrhoea(self, mock_llm):
        intent = mock_llm.parse("Show diarrhoea events")
        assert intent.target_column == "AETERM"
        assert intent.filter_value == "DIARRHOEA"

    def test_parse_pruritus(self, mock_llm):
        intent = mock_llm.parse("Find pruritus cases")
        assert intent.target_column == "AETERM"
        assert intent.filter_value == "PRURITUS"

    def test_parse_rash(self, mock_llm):
        intent = mock_llm.parse("Patients with rash")
        assert intent.target_column == "AETERM"
        assert intent.filter_value == "RASH"

    def test_parse_application_site(self, mock_llm):
        intent = mock_llm.parse("Application site reactions")
        assert intent.target_column == "AETERM"
        assert intent.filter_value == "APPLICATION SITE"

    def test_parse_dizziness(self, mock_llm):
        intent = mock_llm.parse("Subjects who had dizziness")
        assert intent.target_column == "AETERM"
        assert intent.filter_value == "DIZZINESS"

    def test_parse_bradycardia(self, mock_llm):
        intent = mock_llm.parse("Show bradycardia events")
        assert intent.target_column == "AETERM"
        assert intent.filter_value == "BRADYCARDIA"


# ==============================================================================
# 6. MockLLM PARSING: FALLBACK / EDGE CASES
# ==============================================================================


class TestMockLLMFallback:
    def test_parse_unknown_term_falls_back_to_aeterm(self, mock_llm):
        """Unknown clinical term should default to AETERM search."""
        intent = mock_llm.parse("Show me subjects with hypotension")
        assert intent.target_column == "AETERM"
        assert "HYPOTENSION" in intent.filter_value

    def test_parse_strips_stop_words(self, mock_llm):
        """Stop words should be removed from fallback AETERM value."""
        intent = mock_llm.parse("Give me the subjects who had cough")
        assert intent.target_column == "AETERM"
        assert intent.filter_value == "COUGH"

    def test_parse_returns_query_intent_type(self, mock_llm):
        intent = mock_llm.parse("severe events")
        assert isinstance(intent, QueryIntent)


# ==============================================================================
# 7. execute_filter
# ==============================================================================


class TestExecuteFilter:
    def test_exact_match_aesev(self, agent):
        intent = QueryIntent(target_column="AESEV", filter_value="SEVERE")
        result = agent.execute_filter(intent)
        assert len(result) == 43

    def test_exact_match_aeser(self, agent):
        intent = QueryIntent(target_column="AESER", filter_value="Y")
        result = agent.execute_filter(intent)
        assert len(result) == 3

    def test_exact_match_aerel(self, agent):
        intent = QueryIntent(target_column="AEREL", filter_value="PROBABLE")
        result = agent.execute_filter(intent)
        assert len(result) == 361

    def test_partial_match_aeterm(self, agent):
        """AETERM uses str.contains for partial matching."""
        intent = QueryIntent(target_column="AETERM", filter_value="PRURITUS")
        result = agent.execute_filter(intent)
        # Should match both "PRURITUS" and "APPLICATION SITE PRURITUS"
        assert len(result) > 84  # 84 exact PRURITUS + APPLICATION SITE PRURITUS

    def test_partial_match_aesoc(self, agent):
        intent = QueryIntent(target_column="AESOC", filter_value="CARDIAC DISORDERS")
        result = agent.execute_filter(intent)
        assert len(result) == 91

    def test_invalid_column_raises_error(self, agent):
        intent = QueryIntent(target_column="NONEXISTENT", filter_value="X")
        with pytest.raises(ValueError, match="not found in dataset"):
            agent.execute_filter(intent)

    def test_no_match_returns_empty(self, agent):
        intent = QueryIntent(target_column="AESEV", filter_value="CRITICAL")
        result = agent.execute_filter(intent)
        assert len(result) == 0

    def test_filter_case_insensitive(self, agent):
        """Filter value is uppercased internally."""
        intent = QueryIntent(target_column="AESEV", filter_value="severe")
        result = agent.execute_filter(intent)
        assert len(result) == 43


# ==============================================================================
# 8. END-TO-END QUERY
# ==============================================================================


class TestEndToEndQuery:
    def test_query_moderate_severity(self, agent):
        result = agent.query(
            "Give me the subjects who had Adverse events of Moderate severity"
        )
        assert result.target_column == "AESEV"
        assert result.filter_value == "MODERATE"
        assert result.n_subjects == 136
        assert len(result.subject_ids) == 136

    def test_query_cardiac_disorders(self, agent):
        result = agent.query("Which patients experienced cardiac disorders?")
        assert result.target_column == "AESOC"
        assert result.filter_value == "CARDIAC DISORDERS"
        assert result.n_subjects == 44

    def test_query_serious_events(self, agent):
        result = agent.query("Show me subjects with serious adverse events")
        assert result.target_column == "AESER"
        assert result.filter_value == "Y"
        assert result.n_subjects == 3

    def test_query_severe_events(self, agent):
        result = agent.query("List severe adverse events")
        assert result.n_subjects == 31

    def test_query_mild_events(self, agent):
        result = agent.query("Show mild AEs")
        assert result.n_subjects == 191

    def test_query_headache(self, agent):
        result = agent.query("Subjects with headache")
        assert result.n_subjects == 16

    def test_query_probable_causality(self, agent):
        result = agent.query("Show probably related AEs")
        assert result.n_subjects == 125

    def test_query_skin_disorders(self, agent):
        result = agent.query("Skin adverse events")
        assert result.n_subjects == 105

    def test_query_nervous_system(self, agent):
        result = agent.query("Nervous system disorders")
        assert result.n_subjects == 59

    def test_query_gastrointestinal(self, agent):
        result = agent.query("Gastrointestinal events")
        assert result.n_subjects == 53

    def test_query_subject_ids_sorted(self, agent):
        result = agent.query("Show mild AEs")
        assert result.subject_ids == sorted(result.subject_ids)

    def test_query_returns_query_result_type(self, agent):
        result = agent.query("severe events")
        assert isinstance(result, QueryResult)

    def test_query_preserves_original_question(self, agent):
        q = "Show me subjects with serious adverse events"
        result = agent.query(q)
        assert result.query == q


# ==============================================================================
# 9. QueryResult FORMATTING
# ==============================================================================


class TestQueryResult:
    def test_str_truncates_at_10_ids(self):
        result = QueryResult(
            query="test",
            target_column="AESEV",
            filter_value="MILD",
            n_subjects=20,
            subject_ids=[f"SUBJ-{i:03d}" for i in range(20)],
        )
        s = str(result)
        assert "10 more" in s

    def test_str_no_truncation_under_10(self):
        result = QueryResult(
            query="test",
            target_column="AESEV",
            filter_value="MILD",
            n_subjects=3,
            subject_ids=["A", "B", "C"],
        )
        s = str(result)
        assert "more" not in s
        assert "A, B, C" in s

    def test_str_contains_filter_info(self):
        result = QueryResult(
            query="test query",
            target_column="AESEV",
            filter_value="SEVERE",
            n_subjects=5,
            subject_ids=["S1", "S2", "S3", "S4", "S5"],
        )
        s = str(result)
        assert "AESEV" in s
        assert "SEVERE" in s
        assert "5" in s
        assert "test query" in s


# ==============================================================================
# 10. AGENT INITIALIZATION
# ==============================================================================


class TestAgentInit:
    def test_agent_loads_data(self, agent):
        assert len(agent.df) == 1191

    def test_agent_uses_mock_llm_without_api_key(self, agent):
        assert isinstance(agent.llm, MockLLM)

    def test_agent_data_has_required_columns(self, agent):
        required = ["USUBJID", "AETERM", "AEDECOD", "AESOC", "AESEV",
                     "AEREL", "AESER"]
        for col in required:
            assert col in agent.df.columns, f"Missing column: {col}"


# ==============================================================================
# 11. EDGE CASES
# ==============================================================================


class TestEdgeCases:
    def test_empty_result_query(self, agent):
        """Query that matches no records returns 0 subjects."""
        intent = QueryIntent(target_column="AETERM", filter_value="ZZZZNONEXISTENT")
        filtered = agent.execute_filter(intent)
        assert len(filtered) == 0

    def test_empty_result_end_to_end(self, agent):
        """End-to-end query with no matches."""
        result = agent.query("Show me subjects with zzzznonexistent condition")
        assert result.n_subjects == 0
        assert result.subject_ids == []

    def test_partial_match_application_site(self, agent):
        """'APPLICATION SITE' matches multiple AETERM values."""
        result = agent.query("Application site reactions")
        assert result.n_subjects == 85
        assert result.target_column == "AETERM"

    def test_pruritus_includes_application_site_pruritus(self, agent):
        """AETERM partial match: 'PRURITUS' matches both PRURITUS and APPLICATION SITE PRURITUS."""
        result = agent.query("Find pruritus cases")
        # 108 unique subjects have AETERM containing "PRURITUS"
        assert result.n_subjects == 108

    def test_diarrhoea_query(self, agent):
        result = agent.query("Show diarrhoea events")
        assert result.n_subjects == 18

    def test_query_with_mixed_case(self, agent):
        """Mixed case in query should still work."""
        result = agent.query("SEVERE adverse events")
        assert result.n_subjects == 31

    def test_multiple_keywords_first_match_wins(self, mock_llm):
        """When query has multiple keywords, first keyword match wins."""
        # "severe" comes before "cardiac" in KEYWORD_MAP iteration
        intent = mock_llm.parse("severe cardiac events")
        assert intent.target_column == "AESEV"
        assert intent.filter_value == "SEVERE"

    def test_query_intent_pydantic_validation(self):
        """QueryIntent validates via Pydantic."""
        intent = QueryIntent(target_column="AESEV", filter_value="MILD")
        assert intent.target_column == "AESEV"
        assert intent.filter_value == "MILD"

    def test_query_intent_pydantic_rejects_missing_fields(self):
        """QueryIntent requires both fields."""
        with pytest.raises(Exception):
            QueryIntent(target_column="AESEV")

    def test_execute_filter_exact_vs_partial(self, agent):
        """AESEV uses exact match; AETERM uses partial match."""
        # "MILD" exact in AESEV = 770 records
        exact = agent.execute_filter(
            QueryIntent(target_column="AESEV", filter_value="MILD")
        )
        assert len(exact) == 770

        # "MILD" partial in AETERM = 0 (no AETERM contains "MILD")
        partial = agent.execute_filter(
            QueryIntent(target_column="AETERM", filter_value="MILD")
        )
        assert len(partial) == 0

    def test_all_severity_counts(self, agent):
        """Verify all severity level record counts."""
        for sev, expected in [("MILD", 770), ("MODERATE", 378), ("SEVERE", 43)]:
            result = agent.execute_filter(
                QueryIntent(target_column="AESEV", filter_value=sev)
            )
            assert len(result) == expected, f"Mismatch for {sev}"

    def test_all_severity_counts_sum_to_total(self, agent):
        """Sum of severity counts equals total records."""
        total = 0
        for sev in ["MILD", "MODERATE", "SEVERE"]:
            total += len(
                agent.execute_filter(
                    QueryIntent(target_column="AESEV", filter_value=sev)
                )
            )
        assert total == 1191
