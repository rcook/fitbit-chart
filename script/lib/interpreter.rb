require 'yaml'
require_relative 'iam'
require_relative 'lambda'
require_relative 'package'
require_relative 's3'
require_relative 'ssm'
require_relative 'sts'
require_relative 'temp'

TASKS = {
  'command::parse-yaml' => lambda do |ctx, args|
    command = args.fetch('command')
    output = Temp.with_temp_file do |output_path|
      status = system "#{command} | tee #{output_path}"
      raise 'Failed' unless status
      File.read(output_path)
    end

    yaml = output
      .split(/\n/)
      .map { |line| line.start_with?('> ') ? line[2..-1] : nil }
      .compact
      .join("\n")

    YAML.load(yaml).each do |k, v|
      ctx.add_result k, v
    end
  end,
  'iam::create-role' => lambda do |ctx, args|
    role_name = args.fetch('role-name')
    policy_document = args.fetch('policy-document')
    role_policy = args.fetch('role-policy')
    IAM.create_role role_name, policy_document, role_policy
  end,
  'lambda::create-function' => lambda do |ctx, args|
    function_name = args.fetch('function-name')
    package_path = args.fetch('package-path')
    runtime = args.fetch('runtime')
    role_arn = args.fetch('role-arn')
    handler = args.fetch('handler')
    Lambda.create_function function_name, package_path, runtime, role_arn, handler
  end,
  'package::build' => lambda do |ctx, args|
    target_name = args.fetch('target-name')
    excluded_dependencies = args.fetch('excluded-dependencies')
    extra_files = args.fetch('extra-files')
    Package.build ctx.repo_dir, target_name, excluded_dependencies, extra_files
    puts 'DONE'
    exit 1
  end,
  's3::create-bucket' => lambda do |ctx, args|
    bucket_name = args.fetch('bucket-name')
    S3.create_bucket bucket_name
  end,
  's3::put-bucket-policy' => lambda do |ctx, args|
    bucket_name = args.fetch('bucket-name')
    bucket_policy = args.fetch('bucket-policy')
    S3.put_bucket_policy bucket_name, bucket_policy
  end,
  's3::put-object' => lambda do |ctx, args|
    bucket_name = args.fetch('bucket-name')
    key = args.fetch('key')
    local_path = ctx.resolve_path(args.fetch('local-path'))
    content_type = args.fetch('content-type')
    S3.put_object bucket_name, key, local_path, content_type
  end,
  's3::website' => lambda do |ctx, args|
    bucket_name = args.fetch('bucket-name')
    index_document = ctx.resolve_path(args.fetch('index-document'))
    error_document = ctx.resolve_path(args.fetch('error-document'))
    S3.website bucket_name, index_document, error_document
  end,
  'ssm::put-parameter' => lambda do |ctx, args|
    name = args.fetch('name')
    value = args.fetch('value')
    type = args.fetch('type').tr('-', '_').to_sym
    SSM.put_parameter name, value, type
  end,
  'sts::get-account-id' => lambda do |ctx, args|
    STS.get_account_id
  end
}.freeze

def indent(line, s = nil)
  prefix = s.nil? ? '  ' : s.ljust(2)
  prefix + line
end

module Parser
  def self.remove_key(hash, key)
    h = hash.dup
    h.delete key
    h
  end

  def self.parse_step(obj)
    return parse_step_list([]) if obj.nil?
    return parse_step_list(obj) if obj.is_a?(Array)
    return parse_check(obj) if obj.include?('check')
    if obj.include?('task')
      name = obj['task']
      args = remove_key(obj, 'task')
      result_key = obj.delete('_')
      return TaskStep.new(name, args, result_key)
    end
    return TaskStep.new(obj, {}, nil) if obj.is_a?(String)
    raise "Unsupported node #{obj.inspect}"
  end

  def self.parse_step_list(array)
    commands = array.map { |obj| parse_step(obj) }
    StepList.new commands
  end

  def self.parse_check(obj)
    command = parse_step(obj.fetch('check'))
    success = parse_step(obj.fetch('success'))
    failure = parse_step(obj.fetch('failure'))
    CheckStep.new command, success, failure
  end
end

class Step
end

class StepList < Step
  def initialize(steps)
    @steps = steps
  end

  def dump
    lines = []
    @steps.each do |step|
      step.dump.each_with_index do |line, i|
        if i == 0
          lines << indent(line, '-')
        else
          lines << indent(line)
        end
      end
    end
    lines
  end

  def run(ctx)
    @steps.each do |step|
      result = step.run(ctx)
      return false unless result
    end
    true
  end
end

class TaskStep < Step
  VALUE_SIZE = 80

  def initialize(name, args, result_key)
    @name = name
    @args = args
    @result_key = result_key
  end

  def dump
    lines = []
    lines << @name
    @args.each do |k, v|
      value = v.to_s
      value = value[0..VALUE_SIZE - 1] + '...' if value.size > VALUE_SIZE
      lines << "#{k}: #{value}"
    end
    lines
  end

  def run(ctx)
    task = TASKS[@name]
    raise "Unknown task #{@name}" unless task
    args = ctx.evaluate(@args)
    output = ctx.trace(@name) { task.call(ctx, args) }
    ctx.add_result(@result_key, output) unless @result_key.nil?
    true
  end
end

class CheckStep < Step
  def initialize(step, success, failure)
    @step = step
    @success = success
    @failure = failure
  end

  def dump
    lines = []
    lines << 'steps {'
    @step.dump.each do |line|
      lines << indent(line)
    end
    lines << '}'
    lines << 'success {'
    @success.dump.each do |line|
      lines << indent(line)
    end
    lines << '}'
    lines << 'failure {'
    @failure.dump.each do |line|
      lines << indent(line)
    end
    lines << '}'
  end

  def run(ctx)
    if @step.run(ctx)
      @success.run ctx
    else
      @failure.run ctx
    end
  end
end

class Context < Tracer
  attr_reader :repo_dir

  def initialize(manifest_path, repo_dir, props, options = {})
    super(options)
    @manifest_dir = File.dirname(manifest_path)
    @repo_dir = repo_dir
    @props = transform_keys(props) { |k| transform_key(k) }
  end

  def evaluate(obj)
    result = transform(obj) do |v|
      v.is_a?(String) ? v % @props : v
    end
  end

  def add_result(key, value)
    @props[transform_key(key)] = value
  end

  def resolve_path(p)
    File.expand_path(p, @manifest_dir)
  end

  private

  def transform_keys(obj, &block)
    if obj.is_a?(Hash)
      temp = {}
      obj.each do |k, v|
        key = block.call(k)
        temp[key] = transform_keys(v, &block)
      end
      temp
    elsif obj.is_a?(Array)
      obj.map { |item| transform_keys(item, &block) }
    else
      obj
    end
  end

  def transform(obj, &block)
    if obj.is_a?(Hash)
      temp = {}
      obj.each do |k, v|
        temp[transform(k, &block)] = transform(v, &block)
      end
      temp
    elsif obj.is_a?(Array)
      obj.map { |item| transform(item, &block) }
    else
      block.call(obj)
    end
  end

  def transform_key(k)
    k.tr('-', '_').to_sym
  end
end

module Interpreter
  def self.run_command(manifest_path, repo_dir, target_or_targets, options = {})
    obj = YAML.load(File.read(manifest_path))
    props = obj.delete('properties') or raise 'Required "properties" key not found'
    step = get_target_step(obj, target_or_targets)

    if options[:simulate]
      step.dump.each do |line|
        puts line
      end
    else
      result = step.run(Context.new(manifest_path, repo_dir, props, options))
      raise 'Step failed' unless result
    end
  end

  def self.get_target_step(obj, target_or_targets)
    if target_or_targets.is_a?(Array)
      StepList.new(target_or_targets.map { |target| get_step(obj, target) })
    else
      get_step(obj, target_or_targets)
    end
  end

  def self.get_step(obj, target)
    raise "Undefined target \"#{target}\"" unless obj.include?(target)
    target_obj = obj[target] || []
    Parser.parse_step(target_obj)
  end
end
