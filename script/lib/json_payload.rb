require_relative 'temp'

module JsonPayload
  def self.with_temp(obj)
    Temp.with_temp do |temp_path|
      File.write temp_path, JSON.pretty_generate(obj)
      yield 'file://' + temp_path
    end
  end
end
