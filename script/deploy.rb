#!/usr/bin/env ruby
# -*-ruby-*-
# vi:syntax=ruby
require 'optparse'
require_relative 'lib/manifest'
require_relative 'lib/temp'
require_relative 'tasks/deploy_assets_task'
require_relative 'tasks/build_package_task'
require_relative 'tasks/update_function_task'
require_relative 'tasks/update_parameters_task'

def deploy_lambda(manifest, repo_dir, options = {})
  if options[:simulate]
    puts manifest.inspect
    puts repo_dir.inspect
    puts options.inspect
  else
    Temp.with_temp_file('.zip') do |package_path|
      [BuildPackageTask, UpdateFunctionTask].each do |cls|
          cls.new(options).run manifest, repo_dir, package_path
      end
    end
    UpdateParametersTask.new(options).run manifest, repo_dir
  end
end

def deploy_assets(manifest, repo_dir, options = {})
  if options[:simulate]
    puts manifest.inspect
    puts repo_dir.inspect
    puts options.inspect
  else
    DeployAssetsTask.new(options).run manifest, repo_dir
  end
end

def main(argv)
  program = File.basename(__FILE__)
  options = {
    verbose: false,
    simulate: true
  }

  subcommands = {
    'lambda' => {
      description: 'Update AWS Lambda function',
      parser: OptionParser.new do |opts|
        opts.banner = "Usage: #{program} [options] lambda [options]"
      end,
      action: proc { |manifest, repo_dir| deploy_lambda manifest, repo_dir, options }
    },
    'assets' => {
      description: 'Update AWS S3 assets',
      parser: OptionParser.new do |opts|
        opts.banner = "Usage: #{program} [options] assets [options]"
      end,
      action: proc { |manifest, repo_dir| deploy_assets manifest, repo_dir, options }
    }
  }

  global = OptionParser.new do |opts|
    opts.banner = "Usage: #{program} [options] [subcommand [options]]"
    opts.on '-v', '--[no-]verbose', 'Run verbosely' do |v|
      options[:verbose] = v
    end
    opts.on '-s', '--[no-]simulate', 'Run simulation' do |v|
      options[:simulate] = v
    end
    opts.separator ''
    subcommand_text = "Subcommands:\n"
    subcommands.each do |k, v|
      subcommand_text << "  #{k.ljust(20)} #{v[:description]}\n"
    end
    opts.separator subcommand_text
  end

  global.order! argv
  subcommand_name = argv.shift
  if subcommand_name.nil?
    STDERR.puts "Specify subcommand\n\n"
    STDERR.puts global.help
    exit 1
  end

  subcommand = subcommands[subcommand_name]
  if subcommand.nil?
    STDERR.puts "Unsupported subcommand \"#{subcommand_name}\"\n\n"
    STDERR.puts global.help
    exit 1
  end

  subcommand[:parser].order! argv

  this_dir = File.expand_path('..', __FILE__)
  repo_dir = File.dirname(this_dir)
  manifest = Manifest.new(File.expand_path('manifest.yaml', repo_dir))
  subcommand[:action].call manifest, repo_dir
end
main ARGV.clone
