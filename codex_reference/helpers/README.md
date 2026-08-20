# Helpers

`omicsignature_compat.R` is the only location where the reference workflows
should handle changing OmicSignature function names or API behavior.

Study-specific Rmd files should call these helpers rather than duplicating
package-compatibility logic.
