require 'shellwords'

class ShellError < RuntimeError
  attr_reader :command
  attr_reader :output
  attr_reader :status

  def initialize(command, output, status)
    if output.nil? || output.empty?
      message = "Command #{command} failed: status=[#{status}]"
    else
      message "Command #{command} failed: output=[#{output}] status=[#{status}]"
    end
    super(message)
    @command = command
    @output = output
    @status = status
  end
end

module Shell
  def self.build_command(*args)
    if args.is_a?(Array) && args.size == 1
      return args[0] if args[0].is_a?(String)
      return args[0].compact.shelljoin if args[0].is_a?(Array)
      raise 'Not implemented'
    end
    args.compact.shelljoin
  end

  def self.capture(*args)
    status, output, _ = capture_helper(*args)
    [status, output]
  end

  def self.check_capture(*args)
    status, output, command = capture_helper(*args)
    raise ShellError.new(command, output, status) unless status.success?
    output
  end

  def self.run(*args)
    status, _ = run_helper(*args)
    status
  end

  def self.check_run(*args)
    status, command = run_helper(*args)
    raise ShellError.new(command, nil, status) unless status.success?
  end

  def self.capture_helper(*args)
    if args.is_a?(Array) && args.size > 0 && args.last.is_a?(Hash)
      options = args.pop
    else
      options = {}
    end

    command = build_command(*args)
    puts "[COMMAND] #{command}" if options[:verbose]
    output = `#{command} 2>&1`
    status = $?
    [status, output, command]
  end

  def self.run_helper(*args)
    if args.is_a?(Array) && args.size > 0 && args.last.is_a?(Hash)
      options = args.pop
    else
      options = {}
    end

    command = build_command(*args)
    puts "[COMMAND] #{command}" if options[:verbose]
    system command
    status = $?
    [status, command]
  end
end
