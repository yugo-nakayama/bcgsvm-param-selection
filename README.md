# bcgsvm-param-selection

Reproducible research compendium for HDLSS SVM parameter selection.

## Quickstart (recommended)
1. Restore packages:
   - `renv::restore()`
2. Run the pipeline:
   - `targets::tar_make()`
3. Build manuscript:
   - `quarto render`

## Repository structure
- `analysis/paper/`: manuscript (Quarto)
- `R/`: core functions (kernel, estimators, loss)
- `pipeline/`: targets pipeline
- `data/`: data instructions (raw data not committed)
- `outputs/`: generated artifacts

## Notes
- Raw datasets are not included. See `data/README.md`.
