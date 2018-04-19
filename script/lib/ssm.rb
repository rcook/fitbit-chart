require_relative 'shell'

module SSM
  PARAMETER_TYPES = {
    string: 'String',
    string_list: 'StringList',
    secure_string: 'SecureString'
  }.freeze

  def self.put_parameter(parameter_name, parameter_value, parameter_type)
    Shell.check_run(
      'aws',
      'ssm',
      'put-parameter',
      '--name', parameter_name,
      '--value', parameter_value,
      '--type', PARAMETER_TYPES[parameter_type],
      '--overwrite')
  end
end
