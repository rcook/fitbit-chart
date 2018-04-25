.PHONY: lambda
lambda:
	script/deploy.rb update-lambda --verbose

.PHONY: assets
assets:
	script/deploy.rb update-assets --verbose
