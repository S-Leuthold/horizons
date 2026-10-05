# Test fixtures

- `ensemble_fit.rds`: a small fitted `horizons_fit` used by the ensemble, uncertainty, configure and class tests. It was built from real, anonymized MIR spectra predicting bulk carbon, so the members carry real signal (R² of about 0.25 to 0.40) and the ensemble tests can check that the meta-learner learned something. It has three members (cubist and rf) spanning the log and none transforms, so the back-transform tests have something to exercise. It needs rebuilding when the object model changes; the build script uses data that is not shipped.

- `opus/`: three Bruker OPUS files from a mid-infrared plate reader, used by the `spectra()` tests. `S01-1.0` and `S01-1.1` are two scans of one soil sample and `S02-1.0` is a scan of another, so the extension-stripped ids repeat the way Bruker's own naming does. The spectral blocks are unchanged. Identifying metadata (sample and operator names, paths, dates, UUIDs, the instrument serial number, the command history, and stale copies of rewritten blocks) has been overwritten in place, so the files parse as normal OPUS files.

Keep fixtures small (under 1 MB). Prefer synthetic data built inside the test; use a stored fixture only when the test needs real signal.
