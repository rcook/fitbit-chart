.PHONY: default
default:
	script/build.rb update-lambda --verbose

.PHONY: assets
assets:
	script/build.rb update-assets --verbose
