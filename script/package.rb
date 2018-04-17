#!/usr/bin/env ruby
# -*-ruby-*-
# vi:syntax=ruby
require 'fileutils'
require 'set'
require_relative 'lib/manifest'
require_relative 'lib/shell'

def get_all_dependencies(path)
  Shell.check_capture('stack', 'exec', '--', 'ldd', path)
    .split(/\n+/)
    .map(&:strip)
    .map(&:split)
    .select { |l| l.size == 4 && l[1] == '=>' }
    .map { |l| [l[0], l[2]] }
end

def make_package(manifest, repo_dir)
  lambda_package = manifest.lambda_package
  target_name = lambda_package.target_name
  local_install_root = Shell.check_capture('stack', 'path', '--local-install-root').chomp
  target_dir = File.expand_path('bin', local_install_root)
  target_path = File.expand_path(lambda_package.target_name, target_dir)
  excluded_dependencies = Set.new(lambda_package.excluded_dependencies)
  extra_files_dir = lambda_package.extra_files_dir
  extra_files = lambda_package
    .extra_files
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
    dep_path = File.expand_path(f, target_dir)
    Shell.check_capture('stack', 'exec', '--', 'cp', p, dep_path)
    FileUtils.cp dep_path, input_dir
  end

  FileUtils.cp target_path, input_dir

  values = {
    target_name: target_name
  }

  extra_files.each do |p|
    source_text = File.read(p)
    output_text = source_text % values
    output_path = File.expand_path(File.basename(p), input_dir)
    File.write(output_path, output_text)
  end

  FileUtils.chmod_R 0777, input_dir

  output_path = File.expand_path("#{target_name}.zip", output_dir)
  Shell.check_capture("zip -j #{output_path} #{input_dir}/*")
  FileUtils.cp output_path, repo_dir
end

def main
  this_dir = File.expand_path('..', __FILE__)
  repo_dir = File.dirname(this_dir)
  manifest = Manifest.new(File.expand_path('manifest.yaml', repo_dir))
  make_package manifest, repo_dir
end
main
