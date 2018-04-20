module Temp
  def self.with_temp_file(ext = nil)
    f = ext.nil? ? Tempfile.new : Tempfile.new(['', ext])
    begin
      temp_path = f.path
      f.close
      f.unlink
      yield temp_path
    ensure
      f.close
      f.unlink
    end
  end
end
