require 'yaml'

class ClosedStruct
  def self.load_yaml_file(path)
    new(YAML.load(File.read(path)))
  end

  def initialize(hash)
    @hash = self.class.transform_shallow(hash)
    @cache = {}
  end

  def to_hash
    @hash
  end

  def method_missing(name_or_sym, *args, &block)
    name = name_or_sym.to_s

    return @cache[name] if @cache.include?(name)
    raise NoMethodError.new("Undefined method #{name}") unless @hash.include?(name)

    value = @hash[name]
    s = value.is_a?(Hash) ? ClosedStruct.new(value) : value
    @cache[name] = s
    s
  end

  def self.transform_shallow(obj)
    temp = {}
    obj.each do |k, v|
      new_key = k.downcase == k ? k.gsub('-', '_') : k
      temp[new_key] = v
    end
    temp
  end
end
