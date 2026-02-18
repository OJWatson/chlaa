.PHONY: regen check-sync check-no-binaries render-vignettes-src

regen:
	Rscript tools/regenerate_model.R

check-sync:
	Rscript tools/check_model_sync.R

check-no-binaries:
	bash tools/check_no_binaries.sh

render-vignettes-src:
	Rscript tools/render_vignettes_src.R
