"""
Question 5: Clinical Data API (FastAPI)

RESTful API serving clinical trial adverse event data with:
- Dynamic cohort filtering by severity and treatment arm
- Per-subject safety risk score calculation

Run:
    uvicorn app:app --reload
"""

from pathlib import Path
from typing import Optional

import pandas as pd
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel, Field

# --- Data loading -------------------------------------------------------------

DATA_PATH = Path(__file__).parent / "adae.csv"


def load_adae() -> pd.DataFrame:
    """Load the ADAE dataset from CSV. Called once at startup."""
    df = pd.read_csv(DATA_PATH, low_memory=False)
    # Ensure key columns are string type for consistent filtering
    for col in ("USUBJID", "AESEV", "ACTARM"):
        if col in df.columns:
            df[col] = df[col].astype(str)
    return df


# --- Pydantic models ----------------------------------------------------------


class AEQueryRequest(BaseModel):
    """Request body for POST /ae-query. All fields are optional filters."""

    severity: Optional[list[str]] = Field(
        default=None,
        description='List of severity levels to include (e.g., ["MILD", "MODERATE"])',
        examples=[["MILD", "MODERATE"]],
    )
    treatment_arm: Optional[str] = Field(
        default=None,
        description="Treatment arm to filter on (e.g., 'Placebo')",
        examples=["Placebo"],
    )


class AEQueryResponse(BaseModel):
    """Response for POST /ae-query."""

    count: int = Field(description="Number of matching AE records")
    subject_ids: list[str] = Field(description="Unique subject identifiers in the cohort")


class RiskScoreResponse(BaseModel):
    """Response for GET /subject-risk/{subject_id}."""

    subject_id: str
    risk_score: int
    risk_category: str


# --- Severity scoring ---------------------------------------------------------

SEVERITY_WEIGHTS: dict[str, int] = {
    "MILD": 1,
    "MODERATE": 3,
    "SEVERE": 5,
}


def calculate_risk_score(severity_values: pd.Series) -> int:
    """Sum weighted severity points for a subject's AE records."""
    return int(severity_values.map(SEVERITY_WEIGHTS).fillna(0).sum())


def classify_risk(score: int) -> str:
    """Assign risk category based on total score."""
    if score < 5:
        return "Low"
    if score < 15:
        return "Medium"
    return "High"


# --- FastAPI app --------------------------------------------------------------

app = FastAPI(
    title="Clinical Trial Data API",
    description="Serves ADAE data, performs cohort filtering, and calculates patient risk scores.",
    version="1.0.0",
)

# Load data once at startup
adae_df: pd.DataFrame = load_adae()


@app.get("/", summary="Welcome message")
def root() -> dict:
    """Returns a JSON welcome message confirming the API is running."""
    return {"message": "Clinical Trial Data API is running"}


@app.post(
    "/ae-query",
    response_model=AEQueryResponse,
    summary="Dynamic AE cohort filtering",
)
def ae_query(filters: AEQueryRequest) -> AEQueryResponse:
    """
    Filter adverse events by severity and/or treatment arm.

    - If `severity` is provided, only AEs with matching AESEV values are included.
    - If `treatment_arm` is provided, only AEs from that ACTARM are included.
    - Omitted or null filters are ignored (all records pass for that dimension).

    Returns the count of matching records and a list of unique subject IDs.
    """
    mask = pd.Series(True, index=adae_df.index)

    if filters.severity is not None:
        severity_upper = [s.upper() for s in filters.severity]
        mask &= adae_df["AESEV"].isin(severity_upper)

    if filters.treatment_arm is not None:
        mask &= adae_df["ACTARM"].str.upper() == filters.treatment_arm.upper()

    filtered = adae_df.loc[mask]
    subject_ids = sorted(filtered["USUBJID"].unique().tolist())

    return AEQueryResponse(count=len(filtered), subject_ids=subject_ids)


@app.get(
    "/subject-risk/{subject_id}",
    response_model=RiskScoreResponse,
    summary="Calculate patient safety risk score",
)
def subject_risk(subject_id: str) -> RiskScoreResponse:
    """
    Calculate a weighted Safety Risk Score for a specific subject.

    Scoring: MILD=1, MODERATE=3, SEVERE=5 per AE record.
    Categories: Low (<5), Medium (5-14), High (>=15).

    Raises 404 if the subject_id is not found in the dataset.
    """
    subject_aes = adae_df[adae_df["USUBJID"] == subject_id]

    if subject_aes.empty:
        raise HTTPException(
            status_code=404,
            detail=f"Subject '{subject_id}' not found in the dataset.",
        )

    score = calculate_risk_score(subject_aes["AESEV"])
    category = classify_risk(score)

    return RiskScoreResponse(
        subject_id=subject_id,
        risk_score=score,
        risk_category=category,
    )
