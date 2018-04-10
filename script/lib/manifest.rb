require 'yaml'
require_relative 'closed_struct'

class Manifest
  attr_reader :dir

  def initialize(path)
    @path = File.expand_path(path)
    @dir = File.dirname(@path)
    @data = ClosedStruct.load_yaml_file(@path)
  end

  def method_missing(name_or_sym, *args, &block)
    @data.send name_or_sym.to_s
  end
end
