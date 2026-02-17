.PHONY: regen check-sync check-no-binaries

regen:
	Rscript tools/regenerate_model.R

check-sync:
	Rscript tools/check_model_sync.R

check-no-binaries:
	bash tools/check_no_binaries.sh
