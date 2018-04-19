#!/usr/bin/env ruby
# -*-ruby-*-
# vi:syntax=ruby
require_relative 'lib/manifest'
require_relative 'lib/temp'
require_relative 'tasks/deploy_assets_task'
require_relative 'tasks/build_package_task'
require_relative 'tasks/update_function_task'

def update_assets(manifest, repo_dir)
  DeployAssetsTask.new.run manifest, repo_dir
end

def update_lambda(manifest, repo_dir)
  Temp.with_temp do |package_path|
    BuildPackageTask.new.run manifest, repo_dir, package_path
    UpdateFunctionTask.new.run manifest, repo_dir, package_path
  end
end

def main(argv)
  this_dir = File.expand_path('..', __FILE__)
  repo_dir = File.dirname(this_dir)
  manifest = Manifest.new(File.expand_path('manifest.yaml', repo_dir))
  case argv
  when []
    STDERR.puts 'Specify command update-assets or update-lambda'
    exit 1
  when ['update-assets']
    update_assets manifest, repo_dir
  when ['update-lambda']
    update_lambda manifest, repo_dir
  else
    STDERR.puts "Unsupported command: #{argv.first}"
    exit 1
  end
end
main ARGV.clone
