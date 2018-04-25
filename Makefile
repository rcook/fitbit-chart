.PHONY: lambda
lambda:
	script/deploy.rb --verbose --simulate lambda

.PHONY: assets
assets:
	script/deploy.rb --verbose --simulate assets
