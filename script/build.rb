#!/usr/bin/env ruby
# -*-ruby-*-
# vi:syntax=ruby
require_relative 'lib/manifest'
require_relative 'lib/temp'
require_relative 'tasks/deploy_assets_task'
require_relative 'tasks/build_package_task'
require_relative 'tasks/update_function_task'
require_relative 'tasks/update_parameters_task'

def update_assets(manifest, repo_dir, options = {})
  DeployAssetsTask.new(options).run manifest, repo_dir
end

def update_lambda(manifest, repo_dir, options = {})
=begin
  Temp.with_temp('.zip') do |package_path|
    [BuildPackageTask, UpdateFunctionTask].each do |cls|
      cls.new(options).run manifest, repo_dir, package_path
    end
  end
=end
  UpdateParametersTask.new(options).run manifest, repo_dir
end

def main(argv)
  this_dir = File.expand_path('..', __FILE__)
  repo_dir = File.dirname(this_dir)
  manifest = Manifest.new(File.expand_path('manifest.yaml', repo_dir))

  verbose_long = argv.delete('--verbose')
  verbose_short = argv.delete('-v')
  is_verbose = verbose_long || verbose_short

  case argv
  when []
    STDERR.puts 'Specify command update-assets or update-lambda'
    exit 1
  when ['update-assets']
    update_assets manifest, repo_dir, verbose: is_verbose
  when ['update-lambda']
    update_lambda manifest, repo_dir, verbose: is_verbose
  else
    STDERR.puts "Unsupported command: #{argv.first}"
    exit 1
  end
end
main ARGV.clone
