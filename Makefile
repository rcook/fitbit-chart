.PHONY: default
default:

.PHONY: assets
assets:
	@script/deploy.rb --verbose --simulate assets

.PHONY: assets-no-simulate
assets-no-simulate:
	@script/deploy.rb --verbose --no-simulate assets

.PHONY: lambda
lambda:
	@script/deploy.rb --verbose --simulate lambda

.PHONY: lambda-no-simulate
lambda-no-simulate:
	@script/deploy.rb --verbose --no-simulate lambda

.PHONY: parameters
parameters:
	@script/deploy.rb --verbose --simulate parameters

.PHONY: parameters-no-simulate
parameters-no-simulate:
	@script/deploy.rb --verbose --no-simulate parameters
