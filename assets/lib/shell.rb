require 'shellwords'

module Shell
  def self.build(*args)
    if args.is_a?(Array) && args.size == 1
      return args[0] if args[0].is_a?(String)
      return args[0].compact.shelljoin if args[0].is_a?(Array)
      raise 'Not implemented'
    end
    args.compact.shelljoin
  end

  def self.capture(*args)
    if args.is_a?(Array) && args.size > 0 && args.last.is_a?(Hash)
      options = args.pop
    else
      options = {}
    end

    command = build(*args)
    puts "[COMMAND] #{command}" if options[:verbose]
    output = `#{command}`
    [output, $?]
  end

  def self.run(*args)
    if args.is_a?(Array) && args.size > 0 && args.last.is_a?(Hash)
      options = args.pop
    else
      options = {}
    end

    command = build(*args)
    puts "[COMMAND] #{command}" if options[:verbose]
    system command
    $?
  end
end
