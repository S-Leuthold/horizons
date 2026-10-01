# Test fixtures

- `ensemble_fit.rds`: a small fitted `horizons_fit` used by the ensemble, uncertainty, configure and class tests. It was built from real, anonymized MIR spectra predicting bulk carbon, so the members carry real signal (R² of about 0.25 to 0.40) and the ensemble tests can check that the meta-learner learned something. It has three members (cubist and rf) spanning the log and none transforms, so the back-transform tests have something to exercise. It needs rebuilding when the object model changes; the build script uses data that is not shipped.

Keep fixtures small (under 1 MB). Prefer synthetic data built inside the test; use a stored fixture only when the test needs real signal.
