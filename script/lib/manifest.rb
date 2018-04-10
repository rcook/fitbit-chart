require 'yaml'

class Manifest
  attr_reader :dir
  attr_reader :data

  def initialize(path)
    @path = File.expand_path(path)
    @dir = File.dirname(@path)
    @data = YAML.load(File.read(@path))
  end
end
