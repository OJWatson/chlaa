.PHONY: regen check-sync

regen:
	Rscript tools/regenerate_model.R

check-sync:
	Rscript tools/check_model_sync.R
