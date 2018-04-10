require 'yaml'

class ClosedStruct
  def self.load_yaml_file(path)
    obj = transform_obj(YAML.load(File.read(path)))
    new obj
  end

  def self.transform_obj(obj)
    if obj.is_a?(Hash)
      temp = {}
      obj.each do |k, v|
        new_key = k.downcase == k ? k.gsub('-', '_') : k
        temp[new_key] = transform_obj(v)
      end
      temp
    elsif obj.is_a?(Array)
      obj.map { |item| transform_obj(item) }
    else
      obj
    end
  end

  def self.wrap_obj(value)
    if value.is_a?(Hash)
      ClosedStruct.new(value)
    elsif value.is_a?(Array)
      value.map { |item| wrap_obj(item) }
    else
      value
    end
  end

  def initialize(hash)
    raise ArgumentError.new('Object must be hash') unless hash.is_a?(Hash)
    @hash = hash
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
    obj = self.class.wrap_obj(value)
    @cache[name] = obj
    obj
  end
end
