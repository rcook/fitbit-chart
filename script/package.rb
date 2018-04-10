#!/usr/bin/env ruby
# -*-ruby-*-
# vi:syntax=ruby
require 'fileutils'
require 'set'
require_relative 'lib/manifest'
require_relative 'lib/shell'

def get_target_path(repo_dir, target_name)
  Dir.chdir(repo_dir) do
    Shell.check_capture('stack', 'exec', '--', 'which', target_name).chomp
  end
end

def get_all_dependencies(path)
  output = Shell.check_capture('ldd', path)
  output
    .split(/\n+/)
    .map(&:strip)
    .map(&:split)
    .select { |l| l.size == 4 && l[1] == '=>' }
    .map { |l| [l[0], l[2]] }
end

def make_package(manifest, repo_dir)
  lambda_package = manifest.data.fetch('lambda-package')

  target_name = lambda_package.fetch('target-name')
  target_path = get_target_path(repo_dir, target_name)
  excluded_dependencies = Set.new(lambda_package.fetch('excluded-dependencies'))
  extra_files_dir = lambda_package.fetch('extra-files-dir')
  extra_files = lambda_package
    .fetch('extra-files')
    .map { |p| File.expand_path(File.join(extra_files_dir, p), manifest.dir) }

  dependencies = get_all_dependencies(target_path).reject { |f, _| excluded_dependencies.include?(f) }

  work_dir = File.expand_path('.package-work', repo_dir)
  input_dir = File.expand_path('input', work_dir)
  output_dir = File.expand_path('output', work_dir)

  FileUtils.remove_dir work_dir if Dir.exists?(work_dir)
  FileUtils.mkpath work_dir
  FileUtils.mkpath input_dir
  FileUtils.mkpath output_dir

  dependencies.each do |f, p|
    FileUtils.cp p, input_dir
  end

  FileUtils.cp target_path, input_dir
  extra_files.each do |p|
    FileUtils.cp p, input_dir
  end

  FileUtils.chmod_R 0777, input_dir

  output_path = File.expand_path("#{target_name}.zip", output_dir)
  Shell.check_run("zip -j #{output_path} #{input_dir}/*")
  FileUtils.cp output_path, repo_dir
end

def main
  this_dir = File.expand_path('..', __FILE__)
  repo_dir = File.dirname(this_dir)
  manifest = Manifest.new(File.expand_path('manifest.yaml', repo_dir))
  make_package manifest, repo_dir
end
main
