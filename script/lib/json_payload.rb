module JsonPayload
  def self.with_temp(obj)
    f = Tempfile.new
    begin
      json_path = f.path
      f.close
      File.write json_path, JSON.pretty_generate(obj)
      yield 'file://' + json_path
    ensure
      f.close
      f.unlink
    end
  end
end
