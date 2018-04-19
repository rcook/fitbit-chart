class Task
  def initialize(options = {})
    @verbose = options[:verbose] || false
    @trace_level = 0
  end

  def trace(s, &block)
    if block_given?
      indent "[TRACE] #{s}" if @verbose
      start_time = Time.now
      @trace_level += 1
      result = yield
      @trace_level -= 1
      end_time = Time.now
      indent "(#{pretty_time_delta(start_time, end_time)})" if @verbose
      result
    else
      indent "[TRACE] #{s}" if @verbose
    end
  end

  private

  def indent(s)
    prefix = '..' * @trace_level
    puts prefix + s
  end

  def pretty_time_delta(start_time, end_time)
    delta = end_time - start_time
    if delta < 1
      "#{(delta * 1000).round(2)} ms"
    else
      "#{delta.round(2)} s"
    end
  end
end
