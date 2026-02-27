# Question 5: Clinical Data API (FastAPI)

RESTful API serving clinical trial adverse event data with dynamic cohort filtering and per-subject safety risk scoring.

## Setup

```bash
cd question_5_api
python3 -m venv .venv
.venv/bin/pip install -r requirements.txt
```

## Run the API

```bash
cd question_5_api
.venv/bin/uvicorn app:app --reload
```

The API will be available at `http://localhost:8000`. Interactive docs at `http://localhost:8000/docs`.

## Endpoints

### `GET /`
Welcome message.
```bash
curl http://localhost:8000/
# {"message": "Clinical Trial Data API is running"}
```

### `POST /ae-query`
Dynamic AE cohort filtering by severity and/or treatment arm.
```bash
curl -X POST http://localhost:8000/ae-query \
  -H "Content-Type: application/json" \
  -d '{"severity": ["MILD", "MODERATE"], "treatment_arm": "Placebo"}'
```

### `GET /subject-risk/{subject_id}`
Calculate weighted safety risk score for a subject.
```bash
curl http://localhost:8000/subject-risk/01-701-1015
# {"subject_id": "01-701-1015", "risk_score": 3, "risk_category": "Low"}
```

Returns 404 if the subject is not found.

## Run Tests

```bash
cd question_5_api
.venv/bin/python3 -m pytest test_app.py -v
```

## Data

`adae.csv` is exported from `pharmaverseadam::adae` (1191 rows, 107 columns).
