"""Tests for the Clinical Trial Data API."""

from pathlib import Path

import pandas as pd
from fastapi.testclient import TestClient

from app import app, calculate_risk_score, classify_risk

client = TestClient(app)

# Load data once for verification
_df = pd.read_csv(Path(__file__).parent / "adae.csv", low_memory=False)
_WEIGHTS = {"MILD": 1, "MODERATE": 3, "SEVERE": 5}


# --- GET / -------------------------------------------------------------------


def test_root_returns_welcome_message():
    response = client.get("/")
    assert response.status_code == 200
    assert response.json() == {"message": "Clinical Trial Data API is running"}


# --- POST /ae-query ----------------------------------------------------------


def test_ae_query_no_filters():
    """No filters → all records returned."""
    response = client.post("/ae-query", json={})
    assert response.status_code == 200
    data = response.json()
    assert data["count"] > 0
    assert len(data["subject_ids"]) > 0


def test_ae_query_severity_filter():
    """Filter by severity only."""
    response = client.post("/ae-query", json={"severity": ["SEVERE"]})
    assert response.status_code == 200
    data = response.json()
    assert data["count"] > 0
    # Severe events should be fewer than total
    all_response = client.post("/ae-query", json={})
    assert data["count"] < all_response.json()["count"]


def test_ae_query_treatment_arm_filter():
    """Filter by treatment arm only."""
    response = client.post("/ae-query", json={"treatment_arm": "Placebo"})
    assert response.status_code == 200
    data = response.json()
    assert data["count"] > 0


def test_ae_query_combined_filters():
    """Filter by both severity and treatment arm."""
    response = client.post(
        "/ae-query",
        json={"severity": ["MILD", "MODERATE"], "treatment_arm": "Placebo"},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["count"] > 0

    # Combined filter should return fewer than severity-only
    sev_only = client.post("/ae-query", json={"severity": ["MILD", "MODERATE"]})
    assert data["count"] <= sev_only.json()["count"]


def test_ae_query_null_filters_ignored():
    """Null values in filters should be treated as absent."""
    response = client.post(
        "/ae-query", json={"severity": None, "treatment_arm": None}
    )
    assert response.status_code == 200
    data = response.json()
    # Should return all records (same as no filters)
    all_response = client.post("/ae-query", json={})
    assert data["count"] == all_response.json()["count"]


def test_ae_query_empty_severity_list():
    """Empty severity list → no records match."""
    response = client.post("/ae-query", json={"severity": []})
    assert response.status_code == 200
    data = response.json()
    assert data["count"] == 0
    assert data["subject_ids"] == []


def test_ae_query_nonexistent_arm():
    """Non-existent treatment arm → no records."""
    response = client.post(
        "/ae-query", json={"treatment_arm": "NonExistentArm"}
    )
    assert response.status_code == 200
    data = response.json()
    assert data["count"] == 0


# --- GET /subject-risk/{subject_id} ------------------------------------------


def test_subject_risk_valid_subject():
    """Known subject returns a valid risk score."""
    response = client.get("/subject-risk/01-701-1015")
    assert response.status_code == 200
    data = response.json()
    assert data["subject_id"] == "01-701-1015"
    assert isinstance(data["risk_score"], int)
    assert data["risk_score"] >= 0
    assert data["risk_category"] in ("Low", "Medium", "High")


def test_subject_risk_category_boundaries():
    """Verify the risk score for 01-701-1015 matches expected category."""
    response = client.get("/subject-risk/01-701-1015")
    data = response.json()
    score = data["risk_score"]
    category = data["risk_category"]

    if score < 5:
        assert category == "Low"
    elif score < 15:
        assert category == "Medium"
    else:
        assert category == "High"


def test_subject_risk_not_found():
    """Non-existent subject returns 404."""
    response = client.get("/subject-risk/NONEXISTENT")
    assert response.status_code == 404
    assert "not found" in response.json()["detail"].lower()


def test_subject_risk_score_calculation():
    """Verify risk score matches manual calculation for a known subject."""
    response = client.get("/subject-risk/01-701-1015")
    data = response.json()

    subject_aes = _df[_df["USUBJID"] == "01-701-1015"]
    expected_score = int(subject_aes["AESEV"].map(_WEIGHTS).sum())

    assert data["risk_score"] == expected_score


# ==============================================================================
# EDGE CASES: /ae-query
# ==============================================================================


def test_ae_query_severity_case_insensitive():
    """Lowercase severity values should match (API uppercases them)."""
    response = client.post("/ae-query", json={"severity": ["mild"]})
    assert response.status_code == 200
    upper = client.post("/ae-query", json={"severity": ["MILD"]})
    assert response.json()["count"] == upper.json()["count"]


def test_ae_query_multiple_severities_skip_moderate():
    """Filter MILD + SEVERE, skipping MODERATE."""
    response = client.post("/ae-query", json={"severity": ["MILD", "SEVERE"]})
    assert response.status_code == 200
    data = response.json()
    # Should be less than all records (MODERATE excluded)
    all_resp = client.post("/ae-query", json={})
    assert data["count"] < all_resp.json()["count"]
    assert data["count"] > 0


def test_ae_query_all_severities_equals_no_filter():
    """Filtering by all three severities should return same as no filter."""
    all_sev = client.post(
        "/ae-query", json={"severity": ["MILD", "MODERATE", "SEVERE"]}
    )
    no_filter = client.post("/ae-query", json={})
    assert all_sev.json()["count"] == no_filter.json()["count"]


def test_ae_query_treatment_arm_case_insensitive():
    """Lowercase treatment arm should match (API uppercases)."""
    lower = client.post("/ae-query", json={"treatment_arm": "placebo"})
    upper = client.post("/ae-query", json={"treatment_arm": "Placebo"})
    assert lower.json()["count"] == upper.json()["count"]
    assert lower.json()["count"] > 0


def test_ae_query_each_treatment_arm():
    """Each treatment arm returns expected record counts."""
    placebo = client.post("/ae-query", json={"treatment_arm": "Placebo"})
    assert placebo.json()["count"] == 301

    high = client.post(
        "/ae-query", json={"treatment_arm": "Xanomeline High Dose"}
    )
    assert high.json()["count"] == 436

    low = client.post(
        "/ae-query", json={"treatment_arm": "Xanomeline Low Dose"}
    )
    assert low.json()["count"] == 454


def test_ae_query_arm_counts_sum_to_total():
    """Sum of per-arm counts equals total records."""
    total = client.post("/ae-query", json={}).json()["count"]
    placebo = client.post("/ae-query", json={"treatment_arm": "Placebo"}).json()["count"]
    high = client.post("/ae-query", json={"treatment_arm": "Xanomeline High Dose"}).json()["count"]
    low = client.post("/ae-query", json={"treatment_arm": "Xanomeline Low Dose"}).json()["count"]
    assert placebo + high + low == total


def test_ae_query_severe_placebo_combined():
    """SEVERE + Placebo: 8 records, 7 subjects."""
    response = client.post(
        "/ae-query",
        json={"severity": ["SEVERE"], "treatment_arm": "Placebo"},
    )
    data = response.json()
    assert data["count"] == 8
    assert len(data["subject_ids"]) == 7


def test_ae_query_subject_ids_sorted():
    """Subject IDs in response are sorted alphabetically."""
    response = client.post("/ae-query", json={})
    ids = response.json()["subject_ids"]
    assert ids == sorted(ids)


def test_ae_query_nonexistent_severity():
    """Non-existent severity value returns 0 records."""
    response = client.post("/ae-query", json={"severity": ["CRITICAL"]})
    assert response.status_code == 200
    assert response.json()["count"] == 0
    assert response.json()["subject_ids"] == []


def test_ae_query_total_records():
    """No-filter query returns all 1191 records."""
    response = client.post("/ae-query", json={})
    assert response.json()["count"] == 1191


def test_ae_query_total_unique_subjects():
    """No-filter query returns 225 unique subjects."""
    response = client.post("/ae-query", json={})
    assert len(response.json()["subject_ids"]) == 225


# ==============================================================================
# EDGE CASES: /subject-risk
# ==============================================================================


def test_subject_risk_low_category():
    """01-701-1015: 3 MILD AEs → score=3 → Low."""
    response = client.get("/subject-risk/01-701-1015")
    data = response.json()
    assert data["risk_score"] == 3
    assert data["risk_category"] == "Low"


def test_subject_risk_medium_category():
    """01-701-1023: score=6 → Medium."""
    response = client.get("/subject-risk/01-701-1023")
    data = response.json()
    assert data["risk_score"] == 6
    assert data["risk_category"] == "Medium"


def test_subject_risk_high_category():
    """01-701-1097: score=20 → High."""
    response = client.get("/subject-risk/01-701-1097")
    data = response.json()
    assert data["risk_score"] == 20
    assert data["risk_category"] == "High"


def test_subject_risk_single_ae():
    """01-701-1181: 1 MODERATE AE → score=3 → Low."""
    response = client.get("/subject-risk/01-701-1181")
    data = response.json()
    assert data["risk_score"] == 3
    assert data["risk_category"] == "Low"


def test_subject_risk_most_aes():
    """01-701-1302: 23 AEs → score=27 → High."""
    response = client.get("/subject-risk/01-701-1302")
    data = response.json()
    assert data["risk_score"] == 27
    assert data["risk_category"] == "High"


def test_subject_risk_with_severe():
    """01-701-1211: 9 AEs including SEVERE → score=19 → High."""
    response = client.get("/subject-risk/01-701-1211")
    data = response.json()
    assert data["risk_score"] == 19
    assert data["risk_category"] == "High"


def test_subject_risk_score_matches_manual_for_all_categories():
    """Verify score calculation for one subject per risk category."""
    for subj_id in ["01-701-1015", "01-701-1023", "01-701-1097"]:
        response = client.get(f"/subject-risk/{subj_id}")
        api_score = response.json()["risk_score"]
        manual = int(_df[_df["USUBJID"] == subj_id]["AESEV"].map(_WEIGHTS).sum())
        assert api_score == manual, f"Mismatch for {subj_id}"


# ==============================================================================
# UNIT TESTS: classify_risk and calculate_risk_score
# ==============================================================================


def test_classify_risk_boundaries():
    """Verify classify_risk at exact boundary values."""
    assert classify_risk(0) == "Low"
    assert classify_risk(4) == "Low"
    assert classify_risk(5) == "Medium"
    assert classify_risk(14) == "Medium"
    assert classify_risk(15) == "High"
    assert classify_risk(100) == "High"


def test_calculate_risk_score_all_mild():
    """3 MILD AEs → score = 3."""
    series = pd.Series(["MILD", "MILD", "MILD"])
    assert calculate_risk_score(series) == 3


def test_calculate_risk_score_mixed():
    """1 MILD + 1 MODERATE + 1 SEVERE → 1 + 3 + 5 = 9."""
    series = pd.Series(["MILD", "MODERATE", "SEVERE"])
    assert calculate_risk_score(series) == 9


def test_calculate_risk_score_empty():
    """Empty series → score = 0."""
    series = pd.Series([], dtype=str)
    assert calculate_risk_score(series) == 0


def test_calculate_risk_score_unknown_severity():
    """Unknown severity values get weight 0 (via fillna)."""
    series = pd.Series(["MILD", "UNKNOWN"])
    assert calculate_risk_score(series) == 1


# ==============================================================================
# STRUCTURAL / API TESTS
# ==============================================================================


def test_response_content_type_json():
    """All endpoints return application/json."""
    for resp in [
        client.get("/"),
        client.post("/ae-query", json={}),
        client.get("/subject-risk/01-701-1015"),
    ]:
        assert resp.headers["content-type"] == "application/json"


def test_openapi_docs_accessible():
    """OpenAPI docs endpoint is accessible."""
    response = client.get("/docs")
    assert response.status_code == 200


def test_invalid_json_body_returns_422():
    """Invalid JSON body returns 422 Unprocessable Entity."""
    response = client.post(
        "/ae-query",
        content="not valid json",
        headers={"content-type": "application/json"},
    )
    assert response.status_code == 422


def test_ae_query_invalid_field_ignored():
    """Extra fields in request body are ignored (Pydantic default)."""
    response = client.post(
        "/ae-query", json={"severity": ["MILD"], "unknown_field": "value"}
    )
    assert response.status_code == 200
    assert response.json()["count"] > 0
