#!/usr/bin/env ruby
# -*-ruby-*-
# vi:syntax=ruby
require_relative 'lib/lambda'
require_relative 'lib/manifest'
require_relative 'lib/sts'

def make_package(manifest, repo_dir)
  lambda_package = manifest.lambda_package
  function_name = lambda_package.function_name
  runtime = lambda_package.runtime
  zip_path = File.expand_path('.package-work/output/fitbit-chart-lambda.zip', repo_dir)

  account_id = STS.get_account_id

  values = {
    account_id: account_id
  }

  role = lambda_package.role % values
  handler = lambda_package.handler
  Lambda.create_function function_name, zip_path, runtime, role, handler
end

def main
  this_dir = File.expand_path('..', __FILE__)
  repo_dir = File.dirname(this_dir)
  manifest = Manifest.new(File.expand_path('manifest.yaml', repo_dir))
  make_package manifest, repo_dir
end
main
