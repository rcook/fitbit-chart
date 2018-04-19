module Temp
  def self.with_temp
    f = Tempfile.new
    begin
      temp_path = f.path
      f.close
      yield temp_path
    ensure
      f.close
      f.unlink
    end
  end
end
