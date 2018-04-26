#!/usr/bin/env ruby
# -*-ruby-*-
# vi:syntax=ruby
require 'optparse'
require_relative 'lib/interpreter'
require_relative 'lib/temp'
require_relative 'lib/tracer'

def main(argv)
  program = File.basename(__FILE__)
  options = {
    verbose: false,
    simulate: true
  }

  subcommands = [
    [:assets, 'Update AWS S3 assets'],
    [:lambda, 'Update AWS Lambda function'],
    [:parameters, 'Update AWS SSM parameters']
  ].inject({}) do |h, (sym, description)|
    h[sym.to_s] = {
      description: description,
      parser: OptionParser.new do |opts|
        opts.banner = "Usage: #{program} [options] #{sym} [options]"
      end,
      action: lambda do |manifest_path, repo_dir|
        Interpreter.run_command manifest_path, repo_dir, sym.to_s, options
      end
    }
    h
  end

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
  manifest_path = File.expand_path('manifest.yaml', repo_dir)
  subcommand[:action].call manifest_path, repo_dir
end
main ARGV.clone
